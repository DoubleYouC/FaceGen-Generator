var eprocess = FilterScript.Result.Process;
var filePath = asFile.ToLower();
var filename = Path.GetFileName(filePath);

if(aeType == FileConverter.ConvertType.Mesh)
{
    if(filePath.Contains("bak\\meshes"))
		eprocess = FilterScript.Result.Skip;
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
	if(filePath.Contains("bak\\textures"))
		eprocess = FilterScript.Result.Skip;
	if(!filePath.Contains("textures\\actors\\character\\facecustomization"))
		eprocess = FilterScript.Result.Skip; //skip anything that isn't facegen
	if(filePath.Contains("_d.tga"))
    {
        aTextureConverter.LogMessage(aLoggerID, InfoType.Info, "Setting compression method for diffuse facegen texture.");
        aTextureConverter.GenerateMipMaps = true;
		aTextureConverter.ForcedFormat = Bethesda.Tools.ElricInterop.TextureConverter.ForcedTextureFormat.BC1;//diff
		aTextureConverter.QuarteringThreshold = "1024";//diff
		aTextureConverter.ForceBelowThreshold = true;
    }
    if(filePath.Contains("_msn.tga"))
    {
        aTextureConverter.LogMessage(aLoggerID, InfoType.Info, "Setting compression method for normal facegen texture.");
        aTextureConverter.GenerateMipMaps = true;
		aTextureConverter.ForcedFormat = Bethesda.Tools.ElricInterop.TextureConverter.ForcedTextureFormat.BC1;//norm
		aTextureConverter.QuarteringThreshold = "1024";//norm
		aTextureConverter.ForceBelowThreshold = true;
    }
	if(filePath.Contains("_s.tga"))
    {
        aTextureConverter.LogMessage(aLoggerID, InfoType.Info, "Setting compression method for specular facegen texture.");
        aTextureConverter.GenerateMipMaps = true;
		aTextureConverter.ForcedFormat = Bethesda.Tools.ElricInterop.TextureConverter.ForcedTextureFormat.BC5;//spec
		aTextureConverter.QuarteringThreshold = "512";//spec
		aTextureConverter.ForceBelowThreshold = true;
    }
}

return eprocess;