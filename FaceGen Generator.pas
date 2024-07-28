{
    FaceGen Generator
}
unit FaceGen;

// ----------------------------------------------------
//Create variables that will need to be used accross multiple functions/procedures.
// ----------------------------------------------------

var
    iPluginFile: IInterface;
    bOnlyMissing, bSteamAppIDTxtExists: Boolean;
    sResolution, sDiffuse, sNormal, sSpecular, sCKFixesINI: string;
    tlRace, tlNpc, tlTxst, tlHdpt: TList;
    slModels, slTextures, slMaterials, slAssets, slPluginFiles: TStringList;

const
    sPatchName = 'FaceGenPatch.esp';

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
    CreateObjects;

    if not RequirementsCheck then begin
        Result := 1;
        Exit;
    end;

    if not MainMenuForm then begin
        Result := 1;
        Exit;
    end;

    CollectRecords;
    CollectAssets;

    TextureInfo;

    Result := 0;
end;

function Finalize: integer;
{
    This function is called at the end.
}
begin
    tlRace.Free;
    tlNpc.Free;
    tlTxst.Free;
    tlHdpt.Free;

    slModels.Free;
    slTextures.Free;
    slMaterials.Free;
    slAssets.Free;
    slPluginFiles.Free;
    Result := 0;
end;

procedure CreateObjects;
begin
    tlRace := TList.Create;
    tlNpc := TList.Create;
    tlTxst := TList.Create;
    tlHdpt := TList.Create;

    slModels := TStringList.Create;
    slModels.Sorted := True;
    slModels.Duplicates := dupIgnore;

    slTextures := TStringList.Create;
    slTextures.Sorted := True;
    slTextures.Duplicates := dupIgnore;

    slMaterials := TStringList.Create;
    slMaterials.Sorted := True;
    slMaterials.Duplicates := dupIgnore;

    slAssets := TStringList.Create;
    slAssets.Sorted := True;
    slAssets.Duplicates := dupIgnore;

    slPluginFiles := TStringList.Create;
end;

// ----------------------------------------------------
// UI functions and procedures go below.
// ----------------------------------------------------

function MainMenuForm: Boolean;
{
    Main menu form.
}
var
    frm: TForm;
    btnStart, btnCancel: TButton;
    pnl: TPanel;
    gbOptions: TGroupBox;
    cbTextureSize, cbDiffuseFormat, cbNormalFormat, cbSpecularFormat: TComboBox;
    rbOnlyMissing, rbAll: TRadioButton;
    slResolutions, slTextureFormats: TStringList;
begin
    frm := TForm.Create(nil);
    try
        slResolutions := TStringList.Create;
        slResolutions.Add('1024');
        slResolutions.Add('2048');

        slTextureFormats := TStringList.Create;
        slTextureFormats.Add('BC1');
        slTextureFormats.Add('BC5');
        slTextureFormats.Add('BC7');

        frm.Caption := 'FaceGen Generator';
        frm.Width := 600;
        frm.Height := 200;
        frm.Position := poMainFormCenter;
        frm.BorderStyle := bsDialog;
        frm.KeyPreview := True;
        frm.OnClose := frmOptionsFormClose;
        frm.OnKeyDown := FormKeyDown;

        gbOptions := TGroupBox.Create(frm);
        gbOptions.Parent := frm;
        gbOptions.Left := 6;
        gbOptions.Top := 12;
        gbOptions.Width := frm.Width - 30;
        gbOptions.Caption := 'Options';
        gbOptions.Height := 134;

        rbOnlyMissing := TRadioButton.Create(gbOptions);
        rbOnlyMissing.Parent := gbOptions;
        rbOnlyMissing.Left := 16;
        rbOnlyMissing.Top := 30;
        rbOnlyMissing.Width := 100;
        rbOnlyMissing.Caption := 'Missing Only';
        rbOnlyMissing.Checked := True;

        rbAll := TRadioButton.Create(gbOptions);
        rbAll.Parent := gbOptions;
        rbAll.Left := rbOnlyMissing.Left + rbOnlyMissing.Width + 20;
        rbAll.Top := rbOnlyMissing.Top;
        rbAll.Width := 80;
        rbAll.Caption := 'All';

        cbTextureSize := TComboBox.Create(gbOptions);
        cbTextureSize.Parent := gbOptions;
        cbTextureSize.Left := 78;
        cbTextureSize.Top := 60;
        cbTextureSize.Width := 50;
        cbTextureSize.Style := csDropDownList;
        cbTextureSize.Items.Assign(slResolutions);
        cbTextureSize.ItemIndex := 0;
        cbTextureSize.Hint := 'Sets the texture resolution.';
        cbTextureSize.ShowHint := True;
        CreateLabel(frm, 24, cbTextureSize.Top + 15, 'Resolution');

        cbDiffuseFormat := TComboBox.Create(gbOptions);
        cbDiffuseFormat.Parent := gbOptions;
        cbDiffuseFormat.Left := cbTextureSize.Left + cbTextureSize.Width + 52;
        cbDiffuseFormat.Top := cbTextureSize.Top;
        cbDiffuseFormat.Width := 50;
        cbDiffuseFormat.Style := csDropDownList;
        cbDiffuseFormat.Items.Assign(slTextureFormats);
        cbDiffuseFormat.ItemIndex := 0;
        cbDiffuseFormat.Hint := 'Sets the diffuse texture format.';
        cbDiffuseFormat.ShowHint := True;
        CreateLabel(frm, cbTextureSize.Left + cbTextureSize.Width + 16, cbDiffuseFormat.Top + 15, 'Diffuse');

        cbNormalFormat := TComboBox.Create(gbOptions);
        cbNormalFormat.Parent := gbOptions;
        cbNormalFormat.Left := cbDiffuseFormat.Left + cbDiffuseFormat.Width + 54;
        cbNormalFormat.Top := cbTextureSize.Top;
        cbNormalFormat.Width := 50;
        cbNormalFormat.Style := csDropDownList;
        cbNormalFormat.Items.Assign(slTextureFormats);
        cbNormalFormat.ItemIndex := 0;
        cbNormalFormat.Hint := 'Sets the normal texture format.';
        cbNormalFormat.ShowHint := True;
        CreateLabel(frm, cbDiffuseFormat.Left + cbDiffuseFormat.Width + 16, cbNormalFormat.Top + 15, 'Normal');

        cbSpecularFormat := TComboBox.Create(gbOptions);
        cbSpecularFormat.Parent := gbOptions;
        cbSpecularFormat.Left := cbNormalFormat.Left + cbNormalFormat.Width + 58;
        cbSpecularFormat.Top := cbTextureSize.Top;
        cbSpecularFormat.Width := 50;
        cbSpecularFormat.Style := csDropDownList;
        cbSpecularFormat.Items.Assign(slTextureFormats);
        cbSpecularFormat.ItemIndex := 0;
        cbSpecularFormat.Hint := 'Sets the specular texture format.';
        cbSpecularFormat.ShowHint := True;
        CreateLabel(frm, cbNormalFormat.Left + cbNormalFormat.Width + 16, cbSpecularFormat.Top + 15, 'Specular');

        btnStart := TButton.Create(gbOptions);
        btnStart.Parent := gbOptions;
        btnStart.Caption := 'Start';
        btnStart.ModalResult := mrOk;
        btnStart.Top := 102;

        btnCancel := TButton.Create(gbOptions);
        btnCancel.Parent := gbOptions;
        btnCancel.Caption := 'Cancel';
        btnCancel.ModalResult := mrCancel;
        btnCancel.Top := btnStart.Top;

        btnStart.Left := gbOptions.Width - btnStart.Width - btnCancel.Width - 16;
        btnCancel.Left := btnStart.Left + btnStart.Width + 8;

        pnl := TPanel.Create(gbOptions);
        pnl.Parent := gbOptions;
        pnl.Left := 8;
        pnl.Top := btnStart.Top - 12;
        pnl.Width := gbOptions.Width - 16;
        pnl.Height := 2;

        frm.ActiveControl := btnStart;

        if frm.ShowModal <> mrOk then begin
            Result := False;
            Exit;
        end
        else Result := True;

        sResolution := slResolutions[cbTextureSize.ItemIndex];
        sDiffuse := slTextureFormats[cbDiffuseFormat.ItemIndex];
        sNormal := slTextureFormats[cbNormalFormat.ItemIndex];
        sSpecular := slTextureFormats[cbSpecularFormat.ItemIndex];
        bOnlyMissing := rbOnlyMissing.Checked;

        if bOnlyMissing then AddMessage('Mode: Only Missing') else AddMessage('Mode: All');
        AddMessage('Resolution: ' + sResolution);
        AddMessage('Diffuse format: ' + sDiffuse);
        AddMessage('Normal format: ' + sNormal);
        AddMessage('Specular format: ' + sSpecular);

    finally
        frm.Free;
        slTextureFormats.Free;
        slResolutions.Free;
    end;
end;

procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
{
    Cancel if Escape key is pressed.
}
begin
    if Key = VK_ESCAPE then TForm(Sender).ModalResult := mrCancel;
end;

procedure frmOptionsFormClose(Sender: TObject; var Action: TCloseAction);
{
    Close form handler.
}
begin
    if TForm(Sender).ModalResult <> mrOk then Exit
end;

function CreateLabel(aParent: TControl; x, y: Integer; aCaption: string): TLabel;
{
    Create a label.
}
begin
    Result := TLabel.Create(aParent);
    Result.Parent := aParent;
    Result.Left := x;
    Result.Top := y;
    Result.Caption := aCaption;
end;

// ----------------------------------------------------
// Utility Functions and Procedures go below.
// ----------------------------------------------------

function RequirementsCheck: Boolean;
{
    Check for required files.
}
var
    iCKFixesInstalled, iCKPlatformExtendedInstalled: integer;
begin
    //Check for Creation Kit
    if not FileExists(GamePath() + 'CreationKit.exe') then begin
        MessageDlg('Please install the Creation Kit before continuing.', mtError, [mbOk], 0);
        Result := False;
        Exit;
    end;

    //Check for Creation Kit Fixes
    iCKFixesInstalled := 0;
    iCKPlatformExtendedInstalled := 0;
    if FileExists(GamePath() + 'fallout4_test.ini') then begin
        iCKFixesInstalled := 1;
        sCKFixesINI := GamePath() + 'fallout4_test.ini';
    end
    //Check for Creation Kit Platform Extended
    else if FileExists(GamePath() + 'CreationKitPlatformExtended.ini') then begin
        iCKPlatformExtendedInstalled := 1;
        sCKFixesINI := GamePath() + 'CreationKitPlatformExtended.ini';
    end;
    if iCKFixesInstalled + iCKPlatformExtendedInstalled = 0 then begin
        MessageDlg('Please install Creation Kit Platform Extended or Creation Kit Fixes by perchik71 before continuing.', mtError, [mbOk], 0);
        Result := False;
        Exit;
    end;

    //Check for the steam_appid.txt file
    if FileExists(GamePath() + 'steam_appid.txt') then bSteamAppIDTxtExists := True;

    Result := True;
end;

// ----------------------------------------------------
// Record processing Functions and Procedures go below.
// ----------------------------------------------------

procedure CollectAssets;
var
    slArchivesToAdd, slArchivedFiles: TStringList;
    slContainers: TwbFastStringList;
    i, j, idx: integer;
    f, archive, masterFile: string;
begin
    slContainers := TwbFastStringList.Create;
    slArchivesToAdd := TStringList.Create;

    ResourceContainerList(slContainers);

    for i := 0 to Pred(slContainers.Count) do begin
        archive := TrimRightChars(slContainers[i], Length(wbDataPath));
        if ContainsText(archive, ' - Animations.ba2') then continue;
        if ContainsText(archive, ' - Interface.ba2') then continue;
        if ContainsText(archive, ' - MeshesExtra.ba2') then continue;
        if ContainsText(archive, ' - Nvflex.ba2') then continue;
        if ContainsText(archive, ' - Shaders.ba2') then continue;
        if ContainsText(archive, ' - Sounds.ba2') then continue;
        if ContainsText(archive, ' - Startup.ba2') then continue;
        if ContainsText(archive, ' - Textures1.ba2') then continue;
        if ContainsText(archive, ' - Textures2.ba2') then continue;
        if ContainsText(archive, ' - Textures3.ba2') then continue;
        if ContainsText(archive, ' - Textures4.ba2') then continue;
        if ContainsText(archive, ' - Textures5.ba2') then continue;
        if ContainsText(archive, ' - Textures6.ba2') then continue;
        if ContainsText(archive, ' - Textures7.ba2') then continue;
        if ContainsText(archive, ' - Textures8.ba2') then continue;
        if ContainsText(archive, ' - Textures9.ba2') then continue;
        if ContainsText(archive, ' - Voices.ba2') then continue;
        if ContainsText(archive, ' - Voices_cn.ba2') then continue;
        if ContainsText(archive, ' - Voices_de.ba2') then continue;
        if ContainsText(archive, ' - Voices_en.ba2') then continue;
        if ContainsText(archive, ' - Voices_es.ba2') then continue;
        if ContainsText(archive, ' - Voices_esmx.ba2') then continue;
        if ContainsText(archive, ' - Voices_fr.ba2') then continue;
        if ContainsText(archive, ' - Voices_it.ba2') then continue;
        if ContainsText(archive, ' - Voices_ja.ba2') then continue;
        if ContainsText(archive, ' - Voices_pl.ba2') then continue;
        if ContainsText(archive, ' - Voices_ptbr.ba2') then continue;
        if ContainsText(archive, ' - Voices_ru.ba2') then continue;
        if archive = '' then continue;
        if ContainsText(archive, ' - Main.ba2') or ContainsText(archive, ' - Textures.ba2') then begin
            slArchivedFiles := TStringList.Create;
            ResourceList(slContainers[i], slArchivedFiles);
            for j := 0 to Pred(slArchivedFiles.Count) do begin
                f := LowerCase(slArchivedFiles[j]);
                idx := slAssets.IndexOf(f);
                if idx > -1 then begin
                    slArchivesToAdd.Add(archive);
                    masterFile := GetMasterFromArchive(archive);
                    if masterFile <> '' then begin
                        AddMasterIfMissing(iPluginFile, masterFile);
                        SortMasters(iPluginFile);
                    end;
                    break;
                end;
            end;

            slArchivedFiles.Free;
        end;
    end;


    slArchivesToAdd.Free;
    slContainers.Free;
end;

procedure CollectRecords;
{
    Collects records.
}
var
    i, j, k, l, m, idx: integer;
    filename, recordId, masterFile, material, model, relativeFormid, texture, tri: string;
    e, r, npc, eModt, eTextures, eMaterials, eParts, eMaleTints, eTintGroup, eOptions, eOption: IInterface;
    g: IwbGroupRecord;
    isPlayerChild, MQ101PlayerSpouseMale: IwbMainRecord;
    f, fallout4esm: IwbFile;
    slRace, slNpc, slTxst, slHdpt: TStringList;
begin
    slRace := TStringList.Create;
    slNpc := TStringList.Create;
    slHdpt := TStringList.Create;
    slTxst := TStringList.Create;

    for i := 0 to Pred(FileCount) do begin
        f := FileByIndex(i);
        filename := GetFileName(f);
        slPluginFiles.Add(filename);

        if SameText(filename, 'Fallout4.esm') then begin
            fallout4esm := f;
            isPlayerChild := RecordByFormID(fallout4esm, $001916C4, False); //these are handled special for facegen
            MQ101PlayerSpouseMale := RecordByFormID(fallout4esm, $000A7D34, False); //handled special for facegen
        end
        else if SameText(filename, sPatchName) then begin
            iPluginFile := f;
            //Clear out any previous edits to the file.

            if HasGroup(iPluginFile, 'NPC_') then begin
                RemoveNode(GroupBySignature(iPluginFile, 'NPC_'));
            end;
            CleanMasters(iPluginFile);
            AddMasterIfMissing(iPluginFile, 'Fallout4.esm');
        end;
    end;

    if not Assigned(iPluginFile) then begin
        iPluginFile := AddNewFileName(sPatchName, True);
        AddMasterIfMissing(iPluginFile, 'Fallout4.esm');
    end;

    for i := 0 to Pred(FileCount) do begin
        f := FileByIndex(i);

        //RACE
        g := GroupBySignature(f, 'RACE');
        for j := 0 to Pred(ElementCount(g)) do begin
            r := WinningOverride(ElementByIndex(g, j));
            recordId := GetFileName(r) + #9 + ShortName(r);
            idx := slRace.IndexOf(recordId);
            if idx > -1 then continue;
            if GetElementEditValues(r, 'DATA\Flags\FaceGen Head') <> '1' then continue;

            if ElementExists(r, 'Male Tint Layers') then begin
                eMaleTints := ElementByPath(r, 'Male Tint Layers');
                for k := 0 to Pred(ElementCount(eMaleTints)) do begin
                    eTintGroup := ElementByIndex(eMaleTints, k);
                    eOptions := ElementByName(eTintGroup, 'Options');
                    for l := 0 to Pred(ElementCount(eOptions)) do begin
                        eOption := ElementByIndex(eOptions, l);
                        eTextures := ElementByName(eOption, 'Textures');
                        for m := 0 to Pred(ElementCount(eTextures)) do begin
                            e := ElementByIndex(eTextures, m);
                            AddTexture(recordId, GetEditValue(e));
                        end;
                    end;
                end;
            end;

            if ElementExists(r, 'Female Tint Layers') then begin
                eMaleTints := ElementByPath(r, 'Male Tint Layers');
                for k := 0 to Pred(ElementCount(eMaleTints)) do begin
                    eTintGroup := ElementByIndex(eMaleTints, k);
                    eOptions := ElementByName(eTintGroup, 'Options');
                    for l := 0 to Pred(ElementCount(eOptions)) do begin
                        eOption := ElementByIndex(eOptions, l);
                        eTextures := ElementByName(eOption, 'Textures');
                        for m := 0 to Pred(ElementCount(eTextures)) do begin
                            e := ElementByIndex(eTextures, m);
                            AddTexture(recordId, GetEditValue(e));
                        end;
                    end;
                end;
            end;

            slRace.Add(recordId);
            tlRace.Add(r);
            //AddMessage(recordID);
        end;

        //NPC_
        g := GroupBySignature(f, 'NPC_');
        for j := 0 to Pred(ElementCount(g)) do begin
            r := WinningOverride(ElementByIndex(g, j));
            recordId := GetFileName(r) + #9 + ShortName(r);
            idx := slNpc.IndexOf(recordId);
            if idx > -1 then continue;
            idx := tlRace.IndexOf(LinksTo(ElementByPath(r, 'RNAM')));
            if idx = -1 then continue;
            if GetElementEditValues(r, 'ACBS\Use Template Actors\Traits') = '1' then continue;
            if GetElementEditValues(r, 'ACBS\Flags\Is CharGen Face Preset') = '1' then continue;
            if KeywordExists(r, isPlayerChild) then continue;
            if GetLoadOrderFormID(r) = GetLoadOrderFormID(MQ101PlayerSpouseMale) then continue;
            if bOnlyMissing then begin
                masterFile := GetFileName(MasterOrSelf(r));
                relativeFormid := '00' + TrimRightChars(IntToHex(GetLoadOrderFormID(r), 8), 2);
                if FaceGenExists(relativeFormid, masterFile) then continue;
            end;
            slNpc.Add(recordId);
            tlNpc.Add(r);
            AddRequiredElementMasters(r, iPluginFile, False, True);
            SortMasters(iPluginFile);
            npc := wbCopyElementToFile(r, iPluginFile, False, True);


            //AddMessage(recordID);
        end;

        //Hdpt
        g := GroupBySignature(f, 'HDPT');
        for j := 0 to Pred(ElementCount(g)) do begin
            r := WinningOverride(ElementByIndex(g, j));
            recordId := GetFileName(r) + #9 + ShortName(r);
            idx := slHdpt.IndexOf(recordId);
            if idx > -1 then continue;
            if ElementExists(r, 'Model\MODL') then begin
                model := wbNormalizeResourceName(GetElementEditValues(r, 'Model\MODL'), resMesh);
                slModels.Add(model);
                slAssets.Add(model);
            end;
            if ElementExists(r, 'Model\MODT') then begin
                eTextures := ElementByPath(r, 'Model\MODT\Textures');
                for k := 0 to Pred(ElementCount(eTextures)) do begin
                    e := ElementByIndex(eTextures, k);
                    AddTexture(recordId, GetSummary(e));
                end;
                eMaterials := ElementByPath(r, 'Model\MODT\Materials');
                for k := 0 to Pred(ElementCount(eMaterials)) do begin
                    e := ElementByIndex(eMaterials, k);
                    material := wbNormalizeResourceName(GetSummary(e), resMaterial);
                    slMaterials.Add(material);
                    slAssets.Add(material);
                    AddMaterialTextures(material);
                end;
            end;
            if ElementExists(r, 'Parts') then begin
                eParts := ElementByPath(r, 'Parts');
                for k := 0 to Pred(ElementCount(eParts)) do begin
                    e := ElementbyIndex(eParts, k);
                    tri := wbNormalizeResourceName(GetElementEditValues(e, 'NAM1'), resMesh);
                    slModels.Add(tri);
                    slAssets.Add(tri);
                end;
            end;
            slHdpt.Add(recordId);
            tlHdpt.Add(r);
            //AddMessage(recordID);
        end;

        //TXST
        g := GroupBySignature(f, 'TXST');
        for j := 0 to Pred(ElementCount(g)) do begin
            r := WinningOverride(ElementByIndex(g, j));
            recordId := GetFileName(r) + #9 + ShortName(r);
            idx := slTxst.IndexOf(recordId);
            if idx > -1 then continue;
            if GetElementEditValues(r, 'DNAM - Flags\Facegen Textures') <> '1' then continue;

            if ElementExists(r, 'Textures (RGB/A)\TX00') then begin
                AddTexture(recordId, GetElementEditValues(r, 'Textures (RGB/A)\TX00'));
            end;
            if ElementExists(r, 'Textures (RGB/A)\TX01') then begin
                AddTexture(recordId, GetElementEditValues(r, 'Textures (RGB/A)\TX01'));
            end;
            if ElementExists(r, 'Textures (RGB/A)\TX03') then begin
                AddTexture(recordId, GetElementEditValues(r, 'Textures (RGB/A)\TX03'));
            end;
            if ElementExists(r, 'Textures (RGB/A)\TX04') then begin
                AddTexture(recordId, GetElementEditValues(r, 'Textures (RGB/A)\TX04'));
            end;
            if ElementExists(r, 'Textures (RGB/A)\TX05') then begin
                AddTexture(recordId, GetElementEditValues(r, 'Textures (RGB/A)\TX05'));
            end;
            if ElementExists(r, 'Textures (RGB/A)\TX02') then begin
                AddTexture(recordId, GetElementEditValues(r, 'Textures (RGB/A)\TX02'));
            end;
            if ElementExists(r, 'Textures (RGB/A)\TX06') then begin
                AddTexture(recordId, GetElementEditValues(r, 'Textures (RGB/A)\TX06'));
            end;
            if ElementExists(r, 'Textures (RGB/A)\TX07') then begin
                AddTexture(recordId, GetElementEditValues(r, 'Textures (RGB/A)\TX07'));
            end;

            if ElementExists(r, 'MNAM') then begin
                material := wbNormalizeResourceName(GetElementEditValues(r, 'MNAM'), resMaterial);
                slMaterials.Add(material);
                slAssets.Add(material);
                AddMaterialTextures(material);
            end;

            slTxst.Add(recordId);
            tlTxst.Add(r);
            //AddMessage(recordID);
        end;
    end;
    //ListStringsInStringList(slAssets);
    ListStringsInStringList(slNPC);

    slRace.Free;
    slNpc.Free;
    slHdpt.Free;
    slTxst.Free;
end;

procedure TextureInfo;
var
    i: integer;
    f: string;
begin
    AddMessage('=======================================================================================');
    AddMessage('Textures:');
    for i := 0 to Pred(slTextures.Count) do begin
        f := slTextures[i];
        if f = '' then continue;
        AddMessage(f + #9 + GetTextureInfo(f));
    end;
end;


// ----------------------------------------------------
// Generic Functions and Procedures go below.
// ----------------------------------------------------

procedure AddTexture(id, texture: string);
{
    Add texture to texture string lists if present, and warn if missing for the id.
}
begin
    if SameText(texture, '') then Exit;
    texture := wbNormalizeResourceName(texture, resTexture);
    if not ResourceExists(texture) then begin
        AddMessage('Warning:' + id + ' defines missing texture ' + texture);
        Exit;
    end;
    slTextures.Add(texture);
    slAssets.Add(texture);
end;

function GetTextureInfo(f: string): string;
{
    Get resolution of texture in h x w format
}
var
    dds: TwbDDSFile;
    height, width, mipmaps: integer;
    cubemap: string;
begin
    dds := TwbDDSFile.Create;
    try
        try
            dds.LoadFromResource(f);
            if dds.EditValues['Magic'] <> 'DDS ' then
                raise Exception.Create('Not a valid DDS file');
        except
            on E: Exception do begin
                AddMessage('Error reading: ' + f + ' <' + E.Message + '>');
            end;
        end;
        height := dds.NativeValues['HEADER\dwHeight'];
        width := dds.NativeValues['HEADER\dwWidth'];
        mipmaps := dds.NativeValues['HEADER\dwMipMapCount'];
        if dds.NativeValues['HEADER\dwCaps2'] > 0 then
            Result := IntToStr(height) + ' x ' + IntToStr(width) + #9 + 'Mips: ' + IntToStr(mipmaps) + #9 + 'Cubemap: true'
        else
            Result := IntToStr(height) + ' x ' + IntToStr(width) + #9 + 'Mips: ' + IntToStr(mipmaps);
    finally
        dds.Free;
    end;
end;

function GetMasterFromArchive(a: string): string;
{
    Find the plugin that is loading the archive.
}
var
    f, filename: string;
    i: integer;
begin
    Result := '';
    if ContainsText(a, ' - Main.ba2') then f := LowerCase(TrimLeftChars(a, Length(' - Main.ba2')))
    else if ContainsText(a, ' - Textures.ba2') then f := LowerCase(TrimLeftChars(a, Length(' - Textures.ba2')));

    for i := 0 to Pred(FileCount) do begin
        filename := GetFileName(FileByIndex(i));
        if LowerCase(TrimLeftChars(filename, 4)) = f then begin
            Result := filename;
            break;
        end;
    end;
end;

procedure AddMaterialTextures(f: string);
{
    Add textures from material.
}
var
    i: integer;
    tp, texture: string;
    bgsm: TwbBGSMFile;
    bgem: TwbBGEMFile;
    el: TdfElement;
begin
    if RightStr(f, 4) = 'bgsm' then begin
        bgsm := TwbBGSMFile.Create;
        try
            bgsm.LoadFromResource(f);
            el := bgsm.Elements['Textures'];
            for i := 0 to Pred(el.Count) do begin
                tp := el[i].EditValue;
                if Length(tp) < 4 then continue;
                AddTexture(f, tp);
            end;
        finally
            bgsm.Free;
        end;
    end
    else if RightStr(f, 4) = 'bgem' then begin
        bgem := TwbBGEMFile.Create;
        try
            bgem.LoadFromResource(f);

            el := bgem.Elements['Base Texture'];
            AddTexture(f, el);

            el := bgem.Elements['Grayscale Texture'];
            AddTexture(f, el);

            el := bgem.Elements['Envmap Texture'];
            AddTexture(f, el);

            el := bgem.Elements['Normal Texture'];
            AddTexture(f, el);

            el := bgem.Elements['Envmap Mask Texture'];
            AddTexture(f, el);

            el := bgem.Elements['Glow Texture'];
            AddTexture(f, el);
        finally
            bgem.Free;
        end;
    end;
end;

function KeywordExists(r: IInterface; keyword: IwbMainRecord): boolean;
{
    Checks if the r has keyword.
}
var
    i: integer;
    keywords: IInterface;
begin
    Result := False;
    keywords := ElementByPath(r, 'KWDA');
    for i := 0 to Pred(ElementCount(keywords)) do begin
        if GetLoadOrderFormID(LinksTo(ElementByIndex(keywords, i))) = GetLoadOrderFormID(keyword) then begin
            Result := True;
            break;
        end;
    end;
end;

function FaceGenExists(relativeFormid, masterFile: string): Boolean;
begin
    Result := False;
    if not ResourceExists('Meshes\Actors\Character\FaceGenData\FaceGeom\' + masterFile + '\' + relativeFormid + '.nif') then Exit;
    if not ResourceExists('Textures\Actors\Character\FaceCustomization\' + masterFile + '\' + relativeFormid + '_d.dds') then Exit;
    if not ResourceExists('Textures\Actors\Character\FaceCustomization\' + masterFile + '\' + relativeFormid + '_msn.dds') then Exit;
    if not ResourceExists('Textures\Actors\Character\FaceCustomization\' + masterFile + '\' + relativeFormid + '_s.dds') then Exit;
    Result := True;
end;

function GamePath: string;
begin
    Result := TrimLeftChars(wbDataPath, 5);
end;

procedure ListStringsInStringList(sl: TStringList);
{
    Given a TStringList, add a message for all items in the list.
}
var
    i: integer;
begin
    AddMessage('=======================================================================================');
    for i := 0 to Pred(sl.Count) do AddMessage(sl[i]);
    AddMessage('=======================================================================================');
end;

function TrimRightChars(s: string; chars: integer): string;
{
    Returns right string - chars
}
begin
    Result := RightStr(s, Length(s) - chars);
end;

function TrimLeftChars(s: string; chars: integer): string;
{
    Returns left string - chars
}
begin
    Result := LeftStr(s, Length(s) - chars);
end;

end.