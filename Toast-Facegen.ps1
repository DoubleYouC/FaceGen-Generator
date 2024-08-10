# Ensure necessary assemblies are loaded
Add-Type -AssemblyName System.Windows.Forms

# Determine the directory of the current script
$scriptDir = Split-Path -Path $PSCommandPath -Parent

# Define the path to the JSON file in the same directory as the script
$jsonFilePath = Join-Path -Path $scriptDir -ChildPath "config.json"

# Variable to store location data
$locationData = $null

$script:regkey = 'HKLM:\Software\Wow6432Node\Bethesda Softworks\Fallout4'
try {
    $script:fo4 = Get-ItemPropertyValue -Path "$script:regkey" -Name 'installed path' -ErrorAction Stop
} catch {
    $openFileDialogFo4 = New-Object System.Windows.Forms.OpenFileDialog
    $openFileDialogFo4.Title = "Select Fallout4.exe"
    $openFileDialogFo4.Filter = "Fallout 4|Fallout4.exe"
    $openFileDialogFo4.InitialDirectory = $scriptDir
    if ($openFileDialogFo4.ShowDialog() -eq [System.Windows.Forms.DialogResult]::OK) {
        $script:fo4 = Split-Path -Path $openFileDialogFo4.FileName -Parent
    } else {
        Write-Host "`nERROR: Fallout 4 directory not found.`n"
        pause
        exit
    }
}
$script:data = Join-Path $fo4 "data"
$script:CK = Join-Path $fo4 "Creationkit.exe"
$script:Archive2 = Join-Path $script:fo4 "tools\archive2\archive2.exe"
$script:facegenpatch = Join-Path $script:data "Facegenpatch.esp"
$script:ElrichDir = Join-Path $script:fo4 "tools\elric"
$script:Elrich = Join-Path $script:fo4 "tools\elric\elrich.exe"
$script:Texconv = Join-Path $script:fo4 "tools\elric\texconv.exe"
$script:xEditPath = $scriptDir #set here as default for the file open dialog
$script:xEditExecutable = $null
$script:meshes = Join-Path $scriptDir "Temp\Meshes"
$script:textures = Join-Path $script:data -ChildPath "Textures"
$script:tempfolder = Join-Path $scriptDir -ChildPath "Temp"
$script:tempFaceGenMeshes = Join-Path $script:tempfolder "Meshes\Actors\Character\FaceGenData\FaceGeom"
$script:tempFaceGenTextures = Join-Path $script:tempfolder "Textures\Actors\Character\FaceCustomization"

$script:FacegenMeshes = Join-Path $script:data "Meshes\Actors\Character\FaceGenData\FaceGeom"
$script:FacegenTextures = Join-Path $script:data "Textures\Actors\Character\FaceCustomization"

$meshesarchive = Join-Path $script:data "FaceGenPatch - Main.ba2"
$texturesarchive = Join-Path $script:data "FaceGenPatch - Textures.ba2"

if ($args -contains "-zip") {
    Write-Output "-zip has been passed. All files will be compressed into a .zip file."
}

if ($args -contains "-clean") {
    # Automatically delete loose files without prompting
    Write-Output "-clean has been passed. All loose files will be automatically deleted."
}

# Function to get or select the xEdit/FO4Edit path and executable
function GetOrSelectxEditPath {
    # Check if the JSON file exists and read the stored path and executable
    if (Test-Path $jsonFilePath) {
        $locationData = Get-Content -Path $jsonFilePath | ConvertFrom-Json

        # Make sure we are getting the correct properties as strings
        $script:xEditPath = [string]$locationData.xEditDirectory
        $script:xEditExecutable = [string]$locationData.xEditExecutable
        Write-Host "Found stored path to xEdit/FO4Edit: $script:xEditPath"
        Write-Host "Found stored executable: $script:xEditExecutable"
        $fullPath = Join-Path -Path $script:xEditPath -ChildPath $script:xEditExecutable

        # Verify if the executable exists
        if (Test-Path -Path $fullPath) {
            return $fullPath
        } else {
            $script:xEditPath = $scriptDir #set here as default for the file open dialog
            $script:xEditExecutable = $null
            Write-Host "`nERROR: The stored executable does not exist. Please select a new one.`n"
        }
    }
    # Fetch xEdit location from registry HKCR\FO4Script\DefaultIcon
    try {
        $xEditRegKey = Get-ItemProperty -Path 'Registry::HKEY_CLASSES_ROOT\FO4Script\DefaultIcon' -ErrorAction Stop
        $script:xEditPath = [string] $xEditRegKey."(default)"
    } catch {
        Write-Host "`nCouldn't detect xEdit Location from the registry.`n"
    }


    # If JSON file doesn't exist or executable is not found, prompt the user to select it
    $openFileDialog = New-Object System.Windows.Forms.OpenFileDialog
    $openFileDialog.Title = "Select FO4Edit or xEdit executable"
    $openFileDialog.Filter = "FO4Edit or xEdit|fo4edit.exe;fo4edit64.exe;xedit.exe;xedit64.exe"
    $openFileDialog.InitialDirectory = $script:xEditPath
    if ($openFileDialog.ShowDialog() -eq [System.Windows.Forms.DialogResult]::OK) {
        $selectedExe = $openFileDialog.FileName
        $script:xEditPath = Split-Path -Path $selectedExe -Parent
        $script:xEditExecutable = [System.IO.Path]::GetFileName($selectedExe)

        return $selectedExe
    } else {
        Write-Host "`nERROR: FO4Edit or xEdit executable not selected.`n"
        pause
        exit
    }
}

function SaveSettings {
    # Save the settings
    $locationData = @{
        xEditDirectory = $script:xEditPath
        xEditExecutable = $script:xEditExecutable
        scriptDirectory = $scriptDir
        Fallout4Directory = $script:fo4
    }
    $locationData | ConvertTo-Json -Compress | Set-Content -Path $jsonFilePath -Force
    Write-Host "`nSaved settings successfully.`n"
}

function CheckForFacegenPatch {

    while ($true) {
        if (Test-Path -Path $script:facegenpatch) {
            Write-Host "facegenpatch.esp found."
            break
        } else {
            Write-Host "Waiting for facegenpatch.esp..."
            Start-Sleep -Seconds 10
        }
    }
}

$steamapiPath = Join-Path $script:fo4 "steam_api64.dll"
$steamapiBackupPath = Join-Path $script:fo4 "steam_api64_downgradeBackup.dll"
$steamapiTempPath = Join-Path $script:fo4 "steam_api64.dll.tempbackup"
$script:steamapiWasReplaced = $false
function HandleSteamApiMismatch {
    # In case the user has latest Steam Creation Kit but downgraded game executable.

    $steamapiHash = Get-FileHash -Path $steamapiPath
    $creationKitHash = Get-FileHash -Path $script:CK
    if ($steamapiHash.Hash -eq `
    "81321A5CB72AE3F81243FD0B0D8928A063CA09129AB0878573BD36A28422EC4C" `
    -and $creationKitHash.Hash -eq `
    "051462F19DBE6A88CC0432297F88F6B00BBE11C5A0BE0FCEA934DCCE985B31A2") {
        Write-Host "Creation Kit is NG but has OG steam_api64.dll"
        if (Test-Path -Path $steamapiBackupPath) {
            Write-Host "NG steam_api64.dll backup found."
            Copy-Item -Path $steamapiPath -Destination $steamapiTempPath
            Copy-Item -Path $steamapiBackupPath -Destination $steamapiPath
            $script:steamapiWasReplaced = $true
        }
    }
}

# Define the full registry key path
$regkeydir = "Registry::HKEY_CLASSES_ROOT\modl\shell\open\command"

# Retrieve the '(Default)' value from the registry key
$registryValue = Get-ItemProperty -Path $regkeydir -Name "(Default)" -ErrorAction SilentlyContinue

# Check if the '(Default)' value was retrieved successfully
if ($registryValue -and $registryValue."(Default)") {
    # Extract the folder path from the '(Default)' value
    $mo2dir = Split-Path -Path $registryValue."(Default)" -Parent

    # Output the folder path
    Write-Host "MO2 Install Path: $mo2dir"
} else {
    Write-Host "Could not find MO2 installation path."
}

if (-not ('WindowHelper' -as [Type])) {
    Add-Type -TypeDefinition @"
    using System;
    using System.Runtime.InteropServices;
    using System.Text;

    public struct INPUT
    {
        public uint Type;
        public KEYBDINPUT Data;
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct KEYBDINPUT
    {
        public ushort wVk;
        public ushort wScan;
        public uint dwFlags;
        public uint time;
        public IntPtr dwExtraInfo;
    }

    public class WindowHelper {
        [DllImport("user32.dll", CharSet = CharSet.Auto, SetLastError = true)]
        public static extern int GetWindowText(IntPtr hWnd, StringBuilder lpString, int nMaxCount);

        [DllImport("user32.dll")]
        public static extern bool SetForegroundWindow(IntPtr hWnd);

        [DllImport("user32.dll")]
        public static extern uint SendInput(uint nInputs, INPUT[] pInputs, int cbSize);
    }
"@
}

# Main script execution
try {
    $fo4EditExe = GetOrSelectxEditPath
    SaveSettings

    $script = Split-Path -Path $fo4EditExe -Parent

    $pas = Join-Path -Path $script -ChildPath "Edit Scripts\FaceGen Generator.pas"
    #$facegenfilterscript = Join-Path -Path $scriptDir -ChildPath "Elric\FaceGen.cs"

    # Debug output to confirm the $pas path
    Write-Host "Path to .pas file: $pas"

    # Check if the .pas file exists before trying to run the process
    if (!(Test-Path -Path $pas)) {
        Write-Host "ERROR: The .pas file does not exist at the specified path: $pas"
        exit
    }
    $esp = [System.IO.Path]::GetFileName($script:facegenpatch)
    $script:elrichdir = Split-Path -Path $script:Elrich -Parent
    # Start the process with the valid executable path
    if (!(Test-Path -Path $script:facegenpatch)) {
        #Write-Host "Argument is $scriptArgument"
        $xEditProcess = Start-Process -FilePath $fo4EditExe -ArgumentList "-script:`"$pas`" -autoexit -autoload -nobuildrefs -FO4 -D:`"$script:data`" -vefsdir:`"$scriptDir`"" -PassThru
        CheckForFacegenPatch


        # Start-Sleep -Seconds 5

        # if ($KeysToSend) {
        #     Keypress -KeysToSend $KeysToSend
        # }

        # Start-Sleep -Seconds 15

        # while ($true) {
        #     $title = New-Object System.Text.StringBuilder 256
        #     [WindowHelper]::GetWindowText($xEditProcess.MainWindowHandle, $title, $title.Capacity) | Out-Null
        #     $title = $title.ToString()

        #     if ($title -match "FO4Script") {
        #         Write-Output "Title change detected (indicates script is complete) Closing..."
        #         Start-Sleep 1
        #         $xEditProcess.CloseMainWindow()
        #         $xEditProcess.WaitForExit()
        #         break
        #     }

        #     Start-Sleep -Milliseconds 1000
        # }
    }

    HandleSteamApiMismatch
    Start-Process -FilePath $script:CK -WorkingDirectory $script:fo4 -ArgumentList "-ExportFaceGenData:$esp W32" -Wait
    if ($script:steamapiWasReplaced) {
        Copy-Item $steamapiTempPath -Destination $steamapiPath
    }

    #If temp path already exists, wipe it out.
    if (Test-Path -Path "$script:tempfolder") {
        Remove-Item -LiteralPath "$script:tempfolder" -Recurse -Force
        Write-Host "`"$script:tempfolder`" was deleted automatically"
    }
    #Copy meshes to temp folder
    Copy-Item "$script:FacegenMeshes" -Destination "$script:tempFaceGenMeshes" -Recurse
    #Run Elric only on the meshes.
    Start-Process -FilePath $script:Elrich -WorkingDirectory $ElrichDir `
    -ArgumentList "`"$script:elrichdir\Settings\PCMeshes.esf`" -ElricOptions.ConvertTarget=`"$script:tempfolder`" -ElricOptions.OutputDirectory=`"$script:tempfolder`" -ElricOptions.CloseWhenFinished=True" `
    -Wait

    #Create meshes archive
    Start-Process -FilePath $script:Archive2 -ArgumentList "`"$script:meshes`" -r=`"$script:tempfolder`" -c=`"$meshesarchive`" -f=General -includeFilters=(?i)meshes\\actors\\character\\facegendata\\facegeom\\" -Wait

    #####################################################################################################
    # This Code was to convert textures, but perchik is now handling that inside the Creation Kit itself.
    #####################################################################################################
    # Write-Host "Converting textures..."
    # $files = Get-ChildItem "$script:FacegenTextures" -Recurse -Filter *_d.tga
    # foreach ($f in $files){
    #     $texturehere = $f.FullName
    #     Set-Location "$script:FacegenTextures"
    #     $relativepath = Get-Item $f.Directory | Resolve-Path -Relative
    #     $relativepath = $relativepath.ToString().Substring(2)
    #     $outputpath = Join-Path $script:tempFaceGenTextures $relativepath
    #     if (!(Test-Path -Path $outputpath)){
    #         New-Item -ItemType "directory" -Path $outputpath
    #     }
    #     Write-Host $texturehere
    #     Start-Process -FilePath $script:Texconv -ArgumentList "-w 1024 -h 1024 -f BC7_UNORM -ft DDS -m 1 -o `"$outputpath`" `"$texturehere`"" -Wait -WindowStyle hidden
    # }
    # $files = Get-ChildItem "$script:FacegenTextures" -Recurse -Filter *_msn.tga
    # foreach ($f in $files){
    #     $texturehere = $f.FullName
    #     Set-Location "$script:FacegenTextures"
    #     $relativepath = Get-Item $f.Directory | Resolve-Path -Relative
    #     $relativepath = $relativepath.ToString().Substring(2)
    #     $outputpath = Join-Path $script:tempFaceGenTextures $relativepath
    #     if (!(Test-Path -Path $outputpath)){
    #         New-Item -ItemType "directory" -Path $outputpath
    #     }
    #     Write-Host $texturehere
    #     Start-Process -FilePath $script:Texconv -ArgumentList "-w 1024 -h 1024 -f BC1_UNORM -ft DDS -m 1 -o `"$outputpath`" `"$texturehere`"" -Wait -WindowStyle hidden
    # }
    # $files = Get-ChildItem "$script:FacegenTextures" -Recurse -Filter *_s.tga
    # foreach ($f in $files){
    #     $texturehere = $f.FullName
    #     Set-Location "$script:FacegenTextures"
    #     $relativepath = Get-Item $f.Directory | Resolve-Path -Relative
    #     $relativepath = $relativepath.ToString().Substring(2)
    #     $outputpath = Join-Path $script:tempFaceGenTextures $relativepath
    #     if (!(Test-Path -Path $outputpath)){
    #         New-Item -ItemType "directory" -Path $outputpath
    #     }
    #     Write-Host $texturehere
    #     Start-Process -FilePath $script:Texconv -ArgumentList "-w 512 -h 512 -f BC5_UNORM -ft DDS -m 1 -o `"$outputpath`" `"$texturehere`"" -Wait -WindowStyle hidden
    # }
    #####################################################################################################
    #####################################################################################################

    #Create textures archive
    Start-Process -FilePath $script:Archive2 -ArgumentList "`"$script:textures`" -r=`"$script:data`" -c=`"$texturesarchive`" -f=DDS -includeFilters=(?i)textures\\actors\\character\\facecustomization\\" -Wait

    # Check if the -clean argument was passed
    if ($args -contains "-clean") {
        # Automatically delete loose files without prompting
        Write-Output "-clean has been passed. All loose files will be automatically deleted."
        Remove-Item -LiteralPath "$script:FacegenMeshes" -Recurse -Force
        Write-Host "`"$script:FacegenMeshes`" was deleted automatically by passing '-clean'"

        Remove-Item -LiteralPath "$script:FacegenTextures" -Recurse -Force
        Write-Host "`"$script:FacegenTextures`" was deleted automatically by passing '-clean'"

        Remove-Item -LiteralPath "$script:tempfolder" -Recurse -Force
        Write-Host "`"$script:tempfolder`" was deleted automatically by passing '-clean'"
    } else {
        # Existing code for user confirmation
        $wshell = New-Object -ComObject Wscript.Shell
        $decision = $wshell.Popup("Delete loose files at `"$script:FacegenMeshes`" ?",0,"Delete loose facegen meshes?",0x4 + 0x20)
        if ($decision -eq 6) {
            Remove-Item -LiteralPath "$script:FacegenMeshes" -Recurse
            Write-Host "`"$script:FacegenMeshes`" was deleted."
        } else {
            Write-Host "`"$script:FacegenMeshes`" was NOT deleted."
        }

        $decision = $wshell.Popup("Delete loose files at `"$script:FacegenTextures`" ?",0,"Delete loose facegen textures?",0x4 + 0x20)
        if ($decision -eq 6) {
            Remove-Item -LiteralPath "$script:FacegenTextures" -Recurse
            Write-Host "`"$script:FacegenTextures`" was deleted."
        } else {
            Write-Host "`"$script:FacegenTextures`" was NOT deleted."
        }

        $decision = $wshell.Popup("Delete temporary files at `"$script:tempfolder`" ?",0,"Delete temporary files?",0x4 + 0x20)
        if ($decision -eq 6) {
            Remove-Item -LiteralPath "$script:tempfolder" -Recurse
            Write-Host "`"$script:tempfolder`" was deleted."
        } else {
            Write-Host "`"$script:tempfolder`" was NOT deleted."
        }
    }

    #Quick Auto Clean
    Start-Process -FilePath $fo4EditExe -ArgumentList "-FO4 -IKnowWhatImDoing -QuickAutoClean -autoload -autoexit -D:`"$script:data`" `"$script:facegenpatch`"" -Wait

    # Check if the -zip argument was passed
    if ($args -contains "-zip") {
        $date = Get-Date -Format "dd-MM-yyyy"
        Write-Output "-zip has been passed. All files will be compressed into a .zip file."
        # Zip the generated files
        Compress-Archive -Path "$script:facegenpatch", "$script:meshesarchive", "$script:texturesarchive" -DestinationPath "$script:data\Facegen-$date.zip"
        Write-Host "All generated files were compressed into GeneratedFiles.zip."
    }

} catch {
    Write-Host "`nAn unexpected error occurred.`n"
    Write-Error $_
    pause
    Write-Output "Done"
    exit
}