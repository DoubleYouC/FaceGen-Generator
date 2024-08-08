{
    FaceGen Generator
}
unit FaceGen;

// ----------------------------------------------------
//Create variables that will need to be used accross multiple functions/procedures.
// ----------------------------------------------------

var
    iPluginFile: IInterface;
    bBatchMode, bQuickFaceFix, bOnlyMissing, bAll, bSteamAppIDTxtExists: Boolean;
    sCKFixesINI, sVEFSDir, sPicVefs: string;
    tlRace, tlNpc, tlTxst, tlHdpt: TList;
    slModels, slTextures, slMaterials, slAssets, slPluginFiles: TStringList;
    rbFaceGenPreset, rbOnlyMissing, rbAll: TRadioButton;

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

    bBatchMode := CheckLaunchArguments;

    if not bBatchMode then begin

        if not MainMenuForm then begin
            Result := 1;
            Exit;
        end;

    end;

    if not bQuickFaceFix then begin
        if not RequirementsCheck then begin
            Result := 1;
            Exit;
        end;
    end;

    CollectRecords;
    ProcessRecords;
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
    uiScale: integer;
    picVefs: TPicture;
    fImage: TImage;
begin
    frm := TForm.Create(nil);
    try
        frm.Caption := 'Vault-Tec Enhanced FaceGen System';
        frm.Width := 600;
        frm.Height := 400;
        frm.Position := poMainFormCenter;
        frm.BorderStyle := bsDialog;
        frm.KeyPreview := True;
        frm.OnClose := frmOptionsFormClose;
        frm.OnKeyDown := FormKeyDown;

        picVefs := TPicture.Create;
        picVefs.LoadFromFile(sPicVefs);

        fImage := TImage.Create(frm);
		fImage.Picture := picVefs;
		fImage.Parent := frm;
        fImage.Width := 576;
		fImage.Height := 203;
		fImage.Left := 6;
		fImage.Top := 12;
        fImage.Stretch := True;

        gbOptions := TGroupBox.Create(frm);
        gbOptions.Parent := frm;
        gbOptions.Top := fImage.Top + fImage.Height + 24;
        gbOptions.Width := frm.Width - 24;
        gbOptions.Left := 6;
        gbOptions.Caption := 'Options';
        gbOptions.Height := 114;

        rbFaceGenPreset := TRadioButton.Create(gbOptions);
        rbFaceGenPreset.Name := 'rbFaceGenPreset';
        rbFaceGenPreset.Parent := gbOptions;
        rbFaceGenPreset.Left := 16;
        rbFaceGenPreset.Top := 30;
        rbFaceGenPreset.Width := 100;
        rbFaceGenPreset.Caption := 'Quick Face Fix';
        rbFaceGenPreset.Hint := 'Copies only NPCs to patch for whom facegen is missing,'
             + #13#10 + 'which will otherwise produce a missing head,'
             + #13#10 + 'and sets the "Is CharGen Face Preset" flag.'
             + #13#10 + 'The game will generate the face, which may cause stutter.';
        rbFaceGenPreset.ShowHint := True;

        rbOnlyMissing := TRadioButton.Create(gbOptions);
        rbOnlyMissing.Name := 'rbOnlyMissing';
        rbOnlyMissing.Parent := gbOptions;
        rbOnlyMissing.Left := rbFaceGenPreset.Left + rbFaceGenPreset.Width + 20;;
        rbOnlyMissing.Top := rbFaceGenPreset.Top;
        rbOnlyMissing.Width := 150;
        rbOnlyMissing.Caption := 'Generate Missing Faces';
        rbOnlyMissing.Hint := 'Copies only NPCs to patch for whom facegen is missing,'
             + #13#10 + 'which will otherwise produce a missing head.'
             + #13#10 + 'This will be used to generate FaceGen via the Creation Kit.';
        rbOnlyMissing.ShowHint := True;
        rbOnlyMissing.Checked := True;

        rbAll := TRadioButton.Create(gbOptions);
        rbAll.Name := 'rbAll';
        rbAll.Parent := gbOptions;
        rbAll.Left := rbOnlyMissing.Left + rbOnlyMissing.Width + 20;
        rbAll.Top := rbFaceGenPreset.Top;
        rbAll.Width := 150;
        rbAll.Hint := 'Copies all NPCs that use FaceGen to patch'
             + #13#10 + 'to allow regenerating all faces in the game.'
             + #13#10 + 'This will be used to generate FaceGen via the Creation Kit.';
        rbAll.Caption := 'Generate All Faces';
        rbAll.ShowHint := True;

        btnStart := TButton.Create(gbOptions);
        btnStart.Parent := gbOptions;
        btnStart.Caption := 'Start';
        btnStart.ModalResult := mrOk;
        btnStart.Top := rbFaceGenPreset.Top + 50;

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
        uiScale := Screen.PixelsPerInch * 100 / 96;
        frm.ScaleBy(uiScale, 100);
        frm.Font.Size := 8;

        if frm.ShowModal <> mrOk then begin
            Result := False;
            Exit;
        end
        else Result := True;

        bOnlyMissing := rbOnlyMissing.Checked;
        bQuickFaceFix := rbFaceGenPreset.Checked;
        bAll := rbAll.Checked;

        if bOnlyMissing then AddMessage('Mode: Only Missing')
        else if bQuickFaceFix then AddMessage('Mode: Quick Face Fix')
        else if bAll then AddMessage('Mode: All');

    finally
        frm.Free;
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
begin
    //Check for Creation Kit
    if not FileExists(GamePath() + 'CreationKit.exe') then begin
        MessageDlg('Please install the Creation Kit before continuing.', mtError, [mbOk], 0);
        Result := False;
        Exit;
    end;

    //Check for Creation Kit Platform Extended
    if FileExists(GamePath() + 'CreationKitPlatformExtended.ini') then begin
        sCKFixesINI := GamePath() + 'CreationKitPlatformExtended.ini';
    end
    else begin
        MessageDlg('Please install Creation Kit Platform Extended or Creation Kit Fixes by perchik71 before continuing.', mtError, [mbOk], 0);
        Result := False;
        Exit;
    end;

    //Check for the steam_appid.txt file
    if FileExists(GamePath() + 'steam_appid.txt') then bSteamAppIDTxtExists := True;

    Result := True;
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
            if Pos('-bbatchmode', launchOption) > 0 then begin
                Result := True;
                continue;
            end;
            if Pos('-vefsdir:', launchOption) > 0 then begin
                sVEFSDir := TrimRightChars(launchOption, 9);
                AddMessage('VEFS Dir: ' + sVEFSDir);
                sPicVefs := sVEFSDir + '\Images\vefs.png';
                continue;
            end;
            if Pos('-bOnlyMissing:', launchOption) > 0 then begin
                bAll := False;
                bOnlyMissing := True;
                bQuickFaceFix := False;
                continue;
            end;
            if Pos('-bQuickFaceFix:', launchOption) > 0 then begin
                bAll := False;
                bOnlyMissing := False;
                bQuickFaceFix := True;
                continue;
            end;
            if Pos('-bAll:', launchOption) > 0 then begin
                bAll := True;
                bOnlyMissing := False;
                bQuickFaceFix := False;
                continue;
            end;
        end;
    end;
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
    editorId, filename, recordId, masterFile, material, model, newEditorId, relativeFormid, texture, tri: string;
    e, r, npc, headpart, eModt, eTextures, eMaterials, eParts, eMaleTints, eTintGroup, eOptions, eOption: IInterface;
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

            if bOnlyMissing or bQuickFaceFix then begin
                masterFile := GetFileName(MasterOrSelf(r));
                relativeFormid := '00' + TrimRightChars(IntToHex(FixedFormID(r), 8), 2);
                if FaceGenExists(relativeFormid, masterFile) then continue;
            end;
            slNpc.Add(recordId);
            tlNpc.Add(r);
            AddRequiredElementMasters(r, iPluginFile, False, True);
            SortMasters(iPluginFile);
            npc := wbCopyElementToFile(r, iPluginFile, False, True);

            if not bQuickFaceFix then continue;
            SetElementEditValues(npc, 'ACBS\Flags\Is CharGen Face Preset', '1');


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
            editorId := GetElementEditValues(r, 'EDID');
            newEditorId := StringReplace(editorid, ' ', '', [rfReplaceAll, rfIgnoreCase]);
            newEditorId := StringReplace(newEditorId, '+', '', [rfReplaceAll, rfIgnoreCase]);
            newEditorId := StringReplace(newEditorId, '=', '', [rfReplaceAll, rfIgnoreCase]);
            if SameText(editorId, newEditorId) then continue;
            AddRequiredElementMasters(r, iPluginFile, False, True);
            SortMasters(iPluginFile);
            headpart := wbCopyElementToFile(r, iPluginFile, False, True);
            SetElementEditValues(headpart, 'EDID', newEditorId);

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

procedure ProcessRecords;
{
    Process records
}
var
    i, k, l, m, textureCount: integer;
    r, e, eTints, eTintGroup, eOptions, eOption, eTextures: IInterface;
    //r, npc, headpart, eModt, eTextures, eMaterials, eParts, eMaleTints, eTintGroup, eOptions, eOption: IInterface;
    bTint: Boolean;
    recordId: string;
begin
    //Race
    for i:=0 to Pred(tlRace) do begin
        r := ObjectToElement(tlRace[i]);
        recordId := GetFileName(r) + #9 + ShortName(r);

        //Male Head Parts (sorted)
        //  Head Part
        //    HEAD > Links To HDPT record

        //Male Race Presets
        //  RPRM - Preset NPC#0 > Links to NPC_ preset

        //Male Face Details (sorted)
        //  FTSM - Texture Set > Links to TXST facegen head textures

        //DFTM - Male Default Face Texture > Links to TXST facegen head default texture

        //Male Tints
        if ElementExists(r, 'Male Tint Layers') then begin
            eTints := ElementByPath(r, 'Male Tint Layers');
            for k := 0 to Pred(ElementCount(eTints)) do begin
                //Group # 0
                eTintGroup := ElementByIndex(eTints, k);
                //  Options
                eOptions := ElementByName(eTintGroup, 'Options');
                for l := 0 to Pred(ElementCount(eOptions)) do begin
                    //Option #0
                    eOption := ElementByIndex(eOptions, l);
                    //  Textures
                    eTextures := ElementByName(eOption, 'Textures');
                    textureCount := ElementCount(eTextures);
                    if textureCount = 1 then bTint := 1 else bTint := 0;
                    for m := 0 to Pred(textureCount) do begin
                        //TIET - Texture #0
                        e := ElementByIndex(eTextures, m);
                        AddTexture(recordId, GetEditValue(e));
                    end;
                end;
            end;
        end;

        //Female Head Parts (sorted)
        //  Head Part
        //    HEAD > Links To HDPT record

        //Female Race Presets
        //  RPRF - Preset NPC#0 > Links to NPC_ preset

        //Female Face Details (sorted)
        //  FTSF - Texture Set > Links to TXST facegen head textures

        //DFTF - Female Default Face Texture > Links to TXST facegen head default texture

        //Female Tints
        if ElementExists(r, 'Female Tint Layers') then begin
            eTints := ElementByPath(r, 'Female Tint Layers');
            for k := 0 to Pred(ElementCount(eTints)) do begin
                //Group # 0
                eTintGroup := ElementByIndex(eTints, k);
                //  Options
                eOptions := ElementByName(eTintGroup, 'Options');
                for l := 0 to Pred(ElementCount(eOptions)) do begin
                    //Option #0
                    eOption := ElementByIndex(eOptions, l);
                    //  Textures
                    eTextures := ElementByName(eOption, 'Textures');
                    textureCount := ElementCount(eTextures);
                    if textureCount = 1 then bTint := 1 else bTint := 0;
                    for m := 0 to Pred(textureCount) do begin
                        //TIET - Texture #0
                        e := ElementByIndex(eTextures, m);
                        AddTexture(recordId, GetEditValue(e));
                    end;
                end;
            end;
        end;

    end;
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