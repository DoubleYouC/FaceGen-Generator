# Ensure necessary assemblies are loaded
Add-Type -AssemblyName System.Windows.Forms

# Determine the directory of the current script
$scriptDir = Split-Path -Path $PSCommandPath -Parent 

# Define the path to the JSON file in the same directory as the script
$jsonFilePath = Join-Path -Path $scriptDir -ChildPath "xEditLocation.json"

# Variable to store location data
$locationData = $null

# Function to get or select the xEdit/FO4Edit path and executable
function GetOrSelectxEditPath {
    # Check if the JSON file exists and read the stored path and executable
    if (Test-Path $jsonFilePath) {
        $locationData = Get-Content -Path $jsonFilePath | ConvertFrom-Json

        # Make sure we are getting the correct properties as strings
        $path = [string]$locationData.Directory
        $executable = [string]$locationData.Executable
        Write-Host "Found stored path to xEdit/FO4Edit: $path"
        Write-Host "Found stored executable: $executable"
        $fullPath = Join-Path -Path $path -ChildPath $executable

        # Verify if the executable exists
        if (Test-Path -Path $fullPath) {
            return $fullPath
        } else {
            Write-Host "`nERROR: The stored executable does not exist. Please select a new one.`n"
        }
    }

    # If JSON file doesn't exist or executable is not found, prompt the user to select it
    $openFileDialog = New-Object System.Windows.Forms.OpenFileDialog
    $openFileDialog.Title = "Select FO4Edit or xEdit executable"
    $openFileDialog.Filter = "FO4Edit or xEdit|fo4edit.exe;xedit.exe"
    $openFileDialog.InitialDirectory = $scriptDir
    if ($openFileDialog.ShowDialog() -eq [System.Windows.Forms.DialogResult]::OK) {
        $selectedExe = $openFileDialog.FileName
        $selectedPath = Split-Path -Path $selectedExe -Parent
        $selectedExecutable = [System.IO.Path]::GetFileName($selectedExe)

        # Save the path and executable to the JSON file
        $locationData = @{
            Directory = $selectedPath
            Executable = $selectedExecutable
        }
        $locationData | ConvertTo-Json -Compress | Set-Content -Path $jsonFilePath -Force
        Write-Host "`nPath and executable saved successfully.`n"

        return $selectedExe
    } else {
        Write-Host "`nERROR: FO4Edit or xEdit executable not selected.`n"
        pause
        exit
    }
}

# Main script execution
try {
    $fo4EditExe = GetOrSelectxEditPath

    # Get the directory of the FO4Edit executable
    $script = Split-Path -Path $fo4EditExe -Parent

    # Use Join-Path to construct the path to the .pas file
    $pas = Join-Path -Path $script -ChildPath "Edit Scripts\FaceGen Generator.pas"

    # Debug output to confirm the constructed path
    Write-Host "Path to .pas file: $pas"

    # Check if the .pas file exists before trying to run the process
    if (-not (Test-Path -Path $pas)) {
        Write-Host "ERROR: The .pas file does not exist at the specified path: $pas"
        exit
    }

    # Start the process with the valid executable path
    Start-Process -FilePath $fo4EditExe -ArgumentList "-FO4 -autoload -script:`"$pas`"" -Wait

} catch {
    Write-Host "`nAn unexpected error occurred.`n"
    pause
    Write-Output "Done"
    exit
}

