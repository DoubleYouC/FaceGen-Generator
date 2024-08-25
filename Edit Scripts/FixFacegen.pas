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
        AddMessage('Copying ' + key + ' to ' + sVEFSDir + '\Temp\' + key);
        nif := TwbNifFile.Create;
        try
            nif.LoadFromResource(key);
            folder := ExtractFilePath(key);
            EnsureDirectoryExists(sVEFSDir + '\Temp\' + folder);
            nif.SaveToFile(sVEFSDir + '\Temp\' + key);
        finally
            nif.free;
        end;
    end;
end;

procedure FixFaces;
var
    i: integer;
    model, headpart, headpartModel: string;
begin
    for i := 0 to Pred(joFaces.O['NPCsToPatch'].Count) do begin
        model := joFaces.O['NPCsToPatch'].Names[i];
        headpart := joFaces.O['NPCsToPatch'].O[model].S['headpart'];
        headpartModel := joFaces.O['Headparts with Cloth Data'].O[headpart].S['model'];
        PatchBSClothExtraData(headpartModel, model);
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

procedure PatchBSClothExtraData(fromNif, toNif: string);
var
    j: integer;
    nif, clothnif: TwbNifFile;
    block, cloth: TwbNifBlock;
    folder: string;
begin
    nif := TwbNifFile.Create;
    clothnif := TwbNifFile.Create;
    try
        clothnif.LoadFromResource(fromNif);
        for j := 0 to Pred(clothnif.BlocksCount) do begin
            cloth := clothnif.Blocks[j];

            if cloth.BlockType = 'BSClothExtraData' then break;
        end;

        nif.LoadFromResource(toNif);
        for j := 0 to Pred(nif.BlocksCount) do begin
            block := nif.Blocks[j];

            if block.BlockType = 'BSClothExtraData' then begin
                block.Assign(cloth);
                folder := ExtractFilePath(toNif);
                EnsureDirectoryExists(sVEFSDir + '\Temp\' + folder);
                nif.SaveToFile(sVEFSDir + '\Temp\' + toNif);
                AddMessage('Updated: ' + sVEFSDir + '\Temp\' + toNif);
                break;
            end;
        end;
    finally
        nif.free;
        clothnif.free;
    end;
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