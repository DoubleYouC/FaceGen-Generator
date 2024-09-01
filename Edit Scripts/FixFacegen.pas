{
    FaceGen Fix for bad BSClothExtraData
}
unit FaceGenFix;

// ----------------------------------------------------
//Create variables that will need to be used accross multiple functions/procedures.
// ----------------------------------------------------

var
    joFaces, joConfig: TJsonObject;
    sVEFSDir: string;

// ----------------------------------------------------
// Main functions and procedures go up immediately below.
// ----------------------------------------------------

function Initialize: integer;
{
    This function is called at the beginning.
}
var
    i: integer;
begin
    joFaces := TJsonObject.Create;
    joConfig := TJsonObject.Create;
    CheckLaunchArguments;
    CopyFaces;
    FixFaces;
    Result := 0;
end;

function Finalize: integer;
{
    This function is called at the end.
}
begin
    joFaces.Free;
    joConfig.SaveToFile(sVEFSDir + '\done.json', False, TEncoding.UTF8, True);
    joConfig.Free;
    Result := 0;
end;

procedure CopyFaces;
var
    i: integer;
    key, folder: string;
    nif: TwbNifFile;
begin
    for i := 0 to Pred(joFaces.A['NPCsToCopy'].Count) do begin
        key := joFaces.A['NPCsToCopy'].S[i];
        if not ResourceExists(key) then begin
            AddMessage('Warning: Missing expected facegen mesh file.' + #9 + key);
            continue;
        end;
        PatchBSClothExtraData(key);
        // AddMessage('Copying ' + key + ' to ' + sVEFSDir + '\Temp\' + key);
        // nif := TwbNifFile.Create;
        // try
        //     nif.LoadFromResource(key);
        //     folder := ExtractFilePath(key);
        //     EnsureDirectoryExists(sVEFSDir + '\Temp\' + folder);
        //     nif.SaveToFile(sVEFSDir + '\Temp\' + key);
        // finally
        //     nif.free;
        // end;
    end;
end;

procedure FixFaces;
var
    i: integer;
    model: string;
begin
    for i := 0 to Pred(joFaces.O['NPCsToPatch'].Count) do begin
        model := joFaces.O['NPCsToPatch'].Names[i];
        if not ResourceExists(model) then begin
            AddMessage('Warning: Missing expected facegen mesh file.' + #9 + model);
            continue;
        end;
        PatchBSClothExtraData(model);
    end;
end;

function CheckLaunchArguments: Boolean;
var
    i: integer;
    launchOption: string;
begin
    Result := False;
    if paramcount > 0 then begin
        for i:=1 to paramcount do begin
            launchOption := LowerCase(paramstr(i));
            AddMessage('Option: ' + launchOption);
            if Pos('-vefsdir:', launchOption) > 0 then begin
                sVEFSDir := TrimRightChars(launchOption, 9);
                AddMessage('VEFS Dir: ' + sVEFSDir);
                joConfig.LoadFromFile(sVEFSDir + '\config.json');
                joFaces.LoadFromFile(sVEFSDir + '\Faces.json');
                continue;
            end;
        end;
    end;
end;

procedure PatchBSClothExtraData(model: string);
var
    j, k, clothtype: integer;
    nif, clothnif: TwbNifFile;
    block, cloth: TwbNifBlock;
    folder, headpartModel, editorid: string;
begin
    nif := TwbNifFile.Create;
    clothnif := TwbNifFile.Create;
    clothtype := DetectClothType(model);
    try
        if clothtype > 0 then begin
            headpartModel := ClothNifModel(clothtype);
            clothnif.LoadFromResource(headpartModel);
            for k := 0 to Pred(clothnif.BlocksCount) do begin
                cloth := clothnif.Blocks[k];
                if cloth.BlockType = 'BSClothExtraData' then break;
            end;
        end;

        nif.LoadFromResource(model);
        for j := Pred(nif.BlocksCount) downto 0 do begin
            block := nif.Blocks[j];

            if block.BlockType = 'BSClothExtraData' then begin
                editorid := nif.Blocks[j-1].EditValues['Name'];
                if clothtype > 0 then begin
                    block.Assign(cloth);
                end
                else begin
                    AddMessage('Removing BSClothExtraData from ' + #9 + editorid + #9 + ' on ' + #9 + model);
                    nif.Delete(j);
                end;
            end;
        end;
        folder := ExtractFilePath(model);
        EnsureDirectoryExists(sVEFSDir + '\Temp\' + folder);
        nif.SaveToFile(sVEFSDir + '\Temp\' + model);
        AddMessage('Updated: ' + sVEFSDir + '\Temp\' + model);
    finally
        nif.free;
        clothnif.free;
    end;
end;

function DetectClothType(model: string): integer;
{
    Searches for bones and returns the correct BSClothExtraData type in integer form.

    #0
    Does not have valid cloth

    #1
    Hair_C_Cloth00
    Hair_C_Cloth01
    Hair_C_Cloth02
    Hair_L_Cloth00
    Hair_L_Cloth01
    Hair_L_Cloth02
    Hair_R_Cloth00
    Hair_R_Cloth01
    Hair_R_Cloth02
    meshes\\actors\\character\\characterassets\\hair\\female\\femalehair21.nif

    #2
    Ponytail_C_Cloth01
    Ponytail_C_Cloth02
    Ponytail_C_Cloth03
    Ponytail_C_Cloth04
    meshes\\actors\\character\\characterassets\\hair\\female\\femalehair30.nif

    #3
    SideTail_BN_B_004
    SideTail_BN_B_003
    SideTail_BN_B_002
    SideTail_BN_B_001
    SideTail_BN_A_004
    SideTail_BN_A_003
    SideTail_BN_A_002
    SideTail_BN_A_001
    meshes\\actors\\character\\characterassets\\hair\\female\\femalehair32.nif
}
var
    nif: TwbNifFile;
    cloth1, cloth2, cloth3, blockName: string;
    j: integer;
    block: TwbNifBlock;
begin
    Result := 0;
    cloth1 := 'Hair_C_Cloth00,Hair_C_Cloth01,Hair_C_Cloth02,Hair_L_Cloth00,Hair_L_Cloth01,Hair_L_Cloth02,Hair_R_Cloth00,Hair_R_Cloth01,Hair_R_Cloth02';
    cloth2 := 'Ponytail_C_Cloth01,Ponytail_C_Cloth02,Ponytail_C_Cloth03,Ponytail_C_Cloth04';
    cloth3 := 'SideTail_BN_B_004,SideTail_BN_B_003,SideTail_BN_B_002,SideTail_BN_B_001,SideTail_BN_A_004,SideTail_BN_A_003,SideTail_BN_A_002,SideTail_BN_A_001';
    nif := TwbNifFile.Create;
    try
        nif.LoadFromResource(model);
        for j := 0 to Pred(nif.BlocksCount) do begin
            block := nif.Blocks[j];
            if block.BlockType <> 'NiNode' then continue;
            blockName := block.EditValues['Name'];
            if Pos(blockName, cloth1) > 0 then begin
                Result := 1;
                break;
            end;
            if Pos(blockName, cloth2) > 0 then begin
                Result := 2;
                break;
            end;
            if Pos(blockName, cloth3) > 0 then begin
                Result := 3;
                break;
            end;
        end;
    finally
        nif.free;
    end;
end;

function ClothNifModel(i: integer): string;
begin
    if i = 1 then Result := 'meshes\actors\character\characterassets\hair\female\femalehair21.nif';
    if i = 2 then Result := 'meshes\actors\character\characterassets\hair\female\femalehair30.nif';
    if i = 3 then Result := 'meshes\actors\character\characterassets\hair\female\femalehair32.nif';
end;

function TrimRightChars(s: string; chars: integer): string;
{
    Returns right string - chars
}
begin
    Result := RightStr(s, Length(s) - chars);
end;

procedure EnsureDirectoryExists(f: string);
{
    Create directories if they do not exist.
}
begin
    if not DirectoryExists(f) then
        if not ForceDirectories(f) then
            raise Exception.Create('Can not create destination directory ' + f);
end;

end.