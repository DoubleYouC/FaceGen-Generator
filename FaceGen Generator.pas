{
    FaceGen Generator
}
unit FaceGen;

var
    iPluginFile: IInterface;
    bLightPlugin: Boolean;

const
    sPatchName = 'FaceGen Generator.esp';

function Initialize: integer;
{
    This function is called at the beginning.
}
var
    i: integer;
begin

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

function MainMenuForm: Boolean;
{
    Main menu form.
}
var
    frm: TForm;
    btnStart, btnCancel: TButton;
    pnl: TPanel;
    gbOptions: TGroupBox;
    chkLightPlugin: TCheckBox;
begin
    frm := TForm.Create(nil);
    try
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

        chkLightPlugin := TCheckBox.Create(gbOptions);
        chkLightPlugin.Parent := gbOptions;
        chkLightPlugin.Left := 16;
        chkLightPlugin.Top := 30;
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

        chkLightPlugin.Checked := bLightPlugin;

        frm.ActiveControl := btnStart;

        if frm.ShowModal <> mrOk then begin
            Result := False;
            Exit;
        end
        else Result := True;

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

end.