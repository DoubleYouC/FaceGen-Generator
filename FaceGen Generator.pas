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

    if not RequirementsCheck then begin
        Result := 1;
        Exit;
    end;

    if not MainMenuForm then begin
        Result := 1;
        Exit;
    end;


    Result := 0;
end;

function Finalize: integer;
{
    This function is called at the end.
}
begin
    Result := 0;
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
        cbNormalFormat.ItemIndex := 1;
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
        cbSpecularFormat.ItemIndex := 1;
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


// ----------------------------------------------------
// Generic Functions and Procedures go below.
// ----------------------------------------------------

function GamePath: string;
begin
    Result := TrimLeftChars(wbDataPath, 5);
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