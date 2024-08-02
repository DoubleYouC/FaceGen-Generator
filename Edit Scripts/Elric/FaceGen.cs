var eprocess = FilterScript.Result.Process;
var filePath = asFile.ToLower();
var filename = Path.GetFileName(filePath);

if(aeType == FileConverter.ConvertType.Mesh)
{
	if(!filePath.Contains("meshes\\actors\\character\\facegendata\\facegeom"))
		eprocess = FilterScript.Result.Skip; //skip anything that isn't facegen
	if (filePath.Contains(".nif"))
    {
 		if(File.Exists(filePath.Replace(".nif", ".egm")) || filePath.Contains("facegendata"))
		{
			aMeshConverter.LogMessage(aLoggerID, InfoType.Info, "Turning off mesh optimization for Facegen file");
			aMeshConverter.OptimizeMeshes = false;
		}
		if(filename.Contains("head") || filePath.Contains("facegendata"))
		{
			aMeshConverter.ProcessAsHead = true;
		}
        if(filename.Contains("_facebones") && File.Exists(filePath.Replace("_facebones", "")))
            eprocess = FilterScript.Result.Skip; // these are exported with the non-facebones nif
    }
    else if(filePath.Contains("SwitchNodeChildren\\"))
        eprocess = FilterScript.Result.Skip; // these are combined into the switch node parent nif
}
else if(aeType == FileConverter.ConvertType.Texture)
{
	if(!filePath.Contains("textures\\actors\\character\\facecustomization"))
		eprocess = FilterScript.Result.Skip; //skip anything that isn't facegen
	if(filePath.Contains(".dds") && File.Exists(filePath.Replace(".dds", ".tga").Replace("ddstextures", "tgatextures")))
    {
        aTextureConverter.LogMessage(aLoggerID, InfoType.Info, "TGA override: DDS not processed.");
        eprocess = FilterScript.Result.Skip;
    }
	if(filePath.Contains("_d.tga"))
    {
        aTextureConverter.LogMessage(aLoggerID, InfoType.Info, "Compressing to DDS file.");
        aTextureConverter.GenerateMipMaps = false;
		//extureConverter.ForcedFormat = Bethesda.Tools.ElricInterop.TextureConverter.ForcedTextureFormat.BC7;
		aTextureConverter.QuarteringThreshold = "1024";//diff
		aTextureConverter.ForceBelowThreshold = true;
    }
    if(filePath.Contains("_msn.tga"))
    {
        aTextureConverter.LogMessage(aLoggerID, InfoType.Info, "Compressing to DDS file.");
        aTextureConverter.GenerateMipMaps = false;
		aTextureConverter.ForcedFormat = Bethesda.Tools.ElricInterop.TextureConverter.ForcedTextureFormat.BC1;//norm
		aTextureConverter.QuarteringThreshold = "1024";//norm
		aTextureConverter.ForceBelowThreshold = true;
    }
	if(filePath.Contains("_s.tga"))
    {
        aTextureConverter.LogMessage(aLoggerID, InfoType.Info, "Compressing to DDS file.");
        aTextureConverter.GenerateMipMaps = false;
		aTextureConverter.ForcedFormat = Bethesda.Tools.ElricInterop.TextureConverter.ForcedTextureFormat.BC5;//spec
		aTextureConverter.QuarteringThreshold = "512";//specx
		aTextureConverter.ForceBelowThreshold = true;
    }
}

return eprocess;