{
    Remove HasDistantLOD flag from STAT records.
}
unit HasDistantLOD;

var
    iPluginFile, iBeforeGeneration, formLists : IInterface;
    sFolipPluginFileName, sFolipBeforeGeneration : string;
    slStats : TStringList;
    tlStats : TList;
    bLightPlugin : Boolean;

const
    sFolipFileName = 'FOLIP - New LODs.esp';

function Initialize: integer;
{
    This function is called at the beginning.
}
var
    i, j, idx: integer;
    f, g, n, r, p, iFolip: IInterface;
    editorid, recordId, filename: string;
begin
    slStats := TStringList.Create;
    tlStats := TList.Create;
    sFolipBeforeGeneration := 'FOLIP - Before Generation';
    sFolipPluginFileName := 'FOLIP - After Generation';
    bLightPlugin := True;

    if wbSimpleRecords then begin
        MessageDlg('Simple records must be unchecked in xEdit options', mtInformation, [mbOk], 0);
        Result := 1;
        Exit;
    end;

    if not MainMenuForm then begin
        Result := 1;
        Exit;
    end;

    for i := 0 to Pred(FileCount) do begin
        f := FileByIndex(i);
        filename := GetFileName(f);

        if SameText(filename, sFolipFileName) then
            iFolip := f
        else if SameText(filename, sFolipPluginFileName + '.esp') then begin
            iPluginFile := f;

            //Clear out any previous edits to the file.
            if HasGroup(iPluginFile, 'STAT') then begin
                RemoveNode(GroupBySignature(iPluginFile, 'STAT'));
            end;
            if HasGroup(iPluginFile, 'CELL') then begin
                RemoveNode(GroupBySignature(iPluginFile, 'CELL'));
            end;
            if HasGroup(iPluginFile, 'WRLD') then begin
                RemoveNode(GroupBySignature(iPluginFile, 'WRLD'));
            end;
        end
        else if SameText(filename, sFolipBeforeGeneration + '.esp') then begin
            iBeforeGeneration := f;
            formLists := GroupBySignature(iBeforeGeneration, 'FLST');
        end;
    end;

    if not Assigned(iFolip) then begin
        MessageDlg('Please enable ' + sFolipFileName + ' before continuing.', mtError, [mbOk], 0);
        Result := 0;
        Exit;
    end;
    if not Assigned(iPluginFile) then begin
        iPluginFile := AddNewFileName(sFolipPluginFileName + '.esp', bLightPlugin);
        AddMasterIfMissing(iPluginFile, 'Fallout4.esm');
    end;

    for i := 0 to Pred(FileCount) do begin
        f := FileByIndex(i);
        //STAT
        g := GroupBySignature(f, 'STAT');
        for j := 0 to Pred(ElementCount(g)) do begin
            r := WinningOverride(ElementByIndex(g, j));
            recordId := GetFileName(r) + #9 + ShortName(r);
            idx := slStats.IndexOf(recordId);
            if idx > -1 then continue;
            if GetElementEditValues(r, 'Record Header\Record Flags\Has Distant LOD') <> '1' then continue;
            editorid := GetElementEditValues(r, 'EDID');
            if ContainsText(editorid, 'FOLIP_') then continue;
            slStats.Add(recordId);
            tlStats.Add(r);
        end;
    end;

    for i := 0 to Pred(tlStats.Count) do begin
        r := ObjectToElement(tlStats[i]);
        p := RefMastersDeterminePlugin(r);
        n := wbCopyElementToFile(r, p, False, True);
        SetElementNativeValues(n, 'Record Header\Record Flags\Has Distant LOD', 0);
    end;

    SpecificRecordEdits;

    MessageDlg('Patch generated successfully!' + #13#10#13#10 + 'Do not forget to save the plugin.', mtInformation, [mbOk], 0);
    Result := 0;
end;

procedure SpecificRecordEdits;
{
    Carries out specific record changes.
}
var
    i: integer;
    f, r, rCell, rWrld, p, n: IInterface;
    editorid: string;
    tlOverrides, tlParents, tlDecals: TList;
begin
    //Fetch formlists
    tlOverrides := TList.Create;
    tlParents := TList.Create;
    tlDecals := TList.Create;
    for i := 0 to Pred(ElementCount(formLists)) do begin
        f := WinningOverride(ElementByIndex(formLists, i));
        AddMessage(ShortName(f));
        editorid := GetElementEditValues(f, 'EDID');
        AddMessage(editorid);
        if editorid = 'FOLIP_Overrides' then AddFormlistToTList(f, tlOverrides)
        else if editorid = 'FOLIP_Parents' then AddFormlistToTList(f, tlParents)
        else if editorid = 'FOLIP_Decals' then AddFormlistToTList(f, tlDecals);
    end;

    //Add Enable Parents to plugin
    for i := 0 to Pred(tlParents.Count) do begin
        r := ObjectToElement(tlParents[i]);
        rCell := WinningOverride(LinksTo(ElementByIndex(r, 0)));
        rWrld := WinningOverride(LinksTo(ElementByIndex(rCell, 0)));
        p := RefMastersDeterminePlugin(rCell);
        p := RefMastersDeterminePlugin(rWrld);
        p := RefMastersDeterminePlugin(r);
        n := wbCopyElementToFile(rWrld, p, False, True);
        n := wbCopyElementToFile(rCell, p, False, True);
        n := wbCopyElementToFile(r, p, False, True);
    end;
    tlParents.Free;

    //Add Decals to plugin
    for i := 0 to Pred(tlDecals.Count) do begin
        r := ObjectToElement(tlDecals[i]);
        rCell := WinningOverride(LinksTo(ElementByIndex(r, 0)));
        rWrld := WinningOverride(LinksTo(ElementByIndex(rCell, 0)));
        p := RefMastersDeterminePlugin(rCell);
        p := RefMastersDeterminePlugin(rWrld);
        n := wbCopyElementToFile(rWrld, p, False, True);
        n := wbCopyElementToFile(rCell, p, False, True);
        n := wbCopyElementToFile(r, p, True, True);
    end;
    tlDecals.Free;

    //Remove Visible when distant from Overrides.
    for i := 0 to Pred(tlOverrides.Count) do begin
        r := ObjectToElement(tlOverrides[i]);
        editorid := GetElementEditValues(LinksTo(ElementByPath(r, 'NAME')),'EDID');
        AddMessage(editorid);
        if ContainsText(editorid, 'FOLIP_') then continue; //Skip FOLIP Fake Statics
        rCell := WinningOverride(LinksTo(ElementByIndex(r, 0)));
        rWrld := WinningOverride(LinksTo(ElementByIndex(rCell, 0)));
        p := RefMastersDeterminePlugin(rCell);
        p := RefMastersDeterminePlugin(rWrld);
        p := RefMastersDeterminePlugin(r);
        n := wbCopyElementToFile(rWrld, p, False, True);
        n := wbCopyElementToFile(rCell, p, False, True);
        n := wbCopyElementToFile(r, p, False, True);
        SetIsVisibleWhenDistant(n, False);
    end;
    tlOverrides.Free;
end;

procedure AddFormlistToTList(flst: IInterface; var list: TList);
{
    Adds all formids in a formlist to the input TList.
}
var
    formids, r: IInterface;
    i: integer;
begin
    formids := ElementByName(flst, 'FormIDs');
    for i := 0 to Pred(ElementCount(formids)) do begin
        r := LinksTo(ElementByIndex(formids, i));
        list.Add(r);
    end;
end;

function Finalize: integer;
{
    This function is called at the end.
}
begin
    slStats.Free;
    tlStats.Free;

    Result := 0;
end;

function MainMenuForm: Boolean;
{
    Main menu form.
}
var
    frm: TForm;
    btnStart, btnCancel: TButton;
    edPluginName, edBeforeGen: TEdit;
    pnl: TPanel;
    picFolip: TPicture;
    fImage: TImage;
    gbOptions: TGroupBox;
    chkLightPlugin: TCheckBox;
begin
    frm := TForm.Create(nil);
    try
        frm.Caption := 'FOLIP HasDistantLOD Flag Remover';
        frm.Width := 600;
        frm.Height := 510;
        frm.Position := poMainFormCenter;
        frm.BorderStyle := bsDialog;
        frm.KeyPreview := True;
        frm.OnClose := frmOptionsFormClose;
        frm.OnKeyDown := FormKeyDown;

        picFolip := TPicture.Create;
        picFolip.LoadFromFile(ScriptsPath() + 'FOLIP\FOLIP.jpg');

        fImage := TImage.Create(frm);
		fImage.Picture := picFolip;
		fImage.Parent := frm;
        fImage.Width := 576;
		fImage.Height := 278;
		fImage.Left := 8;
		fImage.Top := 12;

        gbOptions := TGroupBox.Create(frm);
        gbOptions.Parent := frm;
        gbOptions.Left := 10;
        gbOptions.Top := fImage.Top + fImage.Height + 24;
        gbOptions.Width := frm.Width - 30;
        gbOptions.Caption := 'FOLIP - After Generation';
        gbOptions.Height := 134;

        edBeforeGen := TEdit.Create(gbOptions);
        edBeforeGen.Parent := gbOptions;
        edBeforeGen.Name := 'edBeforeGen';
        edBeforeGen.Left := 160;
        edBeforeGen.Top := 30;
        edBeforeGen.Width := 180;
        edBeforeGen.Hint := 'Specify the Before Generation plugin name you previously used.';
        edBeforeGen.ShowHint := True;
        CreateLabel(gbOptions, 16, edBeforeGen.Top + 4, 'Before Generation Plugin:');

        edPluginName := TEdit.Create(gbOptions);
        edPluginName.Parent := gbOptions;
        edPluginName.Name := 'edPluginName';
        edPluginName.Left := edBeforeGen.Left;
        edPluginName.Top := edBeforeGen.Top + 30;
        edPluginName.Width := 180;
        edPluginName.Hint := 'Sets the output plugin name for the patch.';
        edPluginName.ShowHint := True;
        CreateLabel(gbOptions, 73, edPluginName.Top + 4, 'Output Plugin:');

        chkLightPlugin := TCheckBox.Create(gbOptions);
        chkLightPlugin.Parent := gbOptions;
        chkLightPlugin.Left := edPluginName.Left + edPluginName.Width + 16;
        chkLightPlugin.Top := edPluginName.Top + 2;
        chkLightPlugin.Width := 120;
        chkLightPlugin.Caption := 'Flag as ESL';
        chkLightPlugin.Hint := 'Flags the output plugin as ESL.';
        chkLightPlugin.ShowHint := True;

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

        edPluginName.Text := sFolipPluginFileName;
        edBeforeGen.Text := sFolipBeforeGeneration;
        chkLightPlugin.Checked := bLightPlugin;

        frm.ActiveControl := btnStart;

        if frm.ShowModal <> mrOk then begin
            Result := False;
            Exit;
        end
        else Result := True;

        sFolipPluginFileName := edPluginName.Text;
        sFolipBeforeGeneration := edBeforeGen.Text;
        bLightPlugin := chkLightPlugin.Checked;

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

function RefMastersDeterminePlugin(r: IInterface): IInterface;
{
    Adds masters required by the reference to the plugin and returns the plugin.
}
begin
    AddRequiredElementMasters(r, iPluginFile, False, True);
    SortMasters(iPluginFile);
    Result := iPluginFile;
end;

end.