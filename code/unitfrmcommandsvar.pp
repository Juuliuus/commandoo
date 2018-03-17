unit unitfrmCommandsVar;

{$mode objfpc}{$H+}
{
    Commandoo Program: Helper application for Linux commands / CLI
    Copyright (C) 2017 Julius Heinrich Ludwig Schön / Ronald Michael Spicer
    created by Julius Schön / R. Spicer
    Foto.TimePirate.org / TimePirate.org / PaganToday.TimePirate.org
    Julius@TimePirate.org

        This program is free software: you can redistribute it and/or modify
        it under the terms of the GNU General Public License as published by
        the Free Software Foundation, version 3 of the License.

        This program is distributed in the hope that it will be useful,
        but WITHOUT ANY WARRANTY; without even the implied warranty of
        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
        GNU General Public License for more details.

        You should have received a copy of the GNU General Public License
        along with this program.  If not, see <http://www.gnu.org/licenses/>.

}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, HintFrame
  , lcltype {THis is needed for key up keyboard constants}
  , unitsharedobj, Buttons
  , StdCtrls, ExtCtrls
  , unitGlobForm
  ;

type

  { TfrmCommandsVar }

  TfrmCommandsVar = class(TForm)
    btnCancel : TBitBtn;
    btnApply : TButton;
    btnFileName : TButton;
    btnDone : TButton;
    btnFolderName : TButton;
    edtInput : TEdit;
    edtCmdLine : TEdit;
    FrameHint1 : TFrameHint;
    memInfo : TMemo;
    Timer1 : TTimer;
    procedure btnFileNameClick( Sender : TObject );
    procedure btnFolderNameClick( Sender : TObject );
    procedure btnOKClick(Sender : TObject);
    procedure btnCancelClick(Sender : TObject);
    procedure btnApplyClick( Sender : TObject );
    procedure btnDoneClick( Sender : TObject );
    procedure edtInputKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure FormActivate(Sender : TObject);
    procedure FormClose(Sender : TObject; var CloseAction : TCloseAction);
    procedure FormCreate(Sender : TObject);
    procedure FormKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure FormShow(Sender : TObject);
    procedure Timer1Timer( Sender : TObject );
  private
    { private declarations }
    //FCanClose : boolean;
    FHasShown : boolean;
    FIsInitialized : boolean;
    fTheVariable : string;
    fIsDone : boolean;
    fLastFile : string;
    fLastFolder : string;
    procedure HandleFormSettings( const TheType : TSettingsDirective );
    procedure NextVariable;
  public
    { public declarations }
  end;

var
  frmCommandsVar : TfrmCommandsVar;

resourcestring
  cmsgufcvDoneVariables = 'Done with Variable replacement.';


implementation


{$R *.lfm}

uses unitcommands
     , usingleInput
     , unitGlob
     //, ufrmMsgDlg
     ;

{ TfrmCommandsVar }

resourcestring
  cmsgCVChooseFolder = 'FOLDER';
  cmsgCVChooseFile = 'FILE';
  cmsgCVChooseFileFolder =
    'Only active for file name variables. '
    + LineEnding + LineEnding
    + 'This button opens a dialog that lets you '
    + LineEnding
    + 'choose a ==%s==  on your system. '
    + LineEnding + LineEnding
    + '<end> '
    ;


procedure TfrmCommandsVar.FormShow(Sender : TObject);
begin

  if not FHasShown then
  begin
    HandleFormSettings( sdLoad );

    btnFileName.Hint := format( cmsgCVChooseFileFolder, [ cmsgCVChooseFile ] );
    btnFolderName.Hint := format( cmsgCVChooseFileFolder, [ cmsgCVChooseFolder ] );


    FHasShown := true;
  end;

end;

procedure TfrmCommandsVar.Timer1Timer( Sender : TObject );
begin
  NextVariable;
  Timer1.Enabled := false;
end;

procedure TfrmCommandsVar.HandleFormSettings( const TheType : TSettingsDirective );
var
  i : Integer;
  theValue : string;
begin
  theValue := '';
  i := -1;
  case TheType of
    sdSave :
      begin
        FormSettings.AddSetting( inttoStr( Height ) );
        FormSettings.AddSetting( inttoStr( Width ) );
        FormSettings.AddSetting( fLastFile );
        FormSettings.AddSetting( fLastFolder );

        FormSettings.SaveFormSettings( Self.Name );
      end;
    sdLoad :
      While FormSettings.ReadFormSettings( Self.Name, i, theValue ) do
      case i of
        0 : Height := strtoint( theValue );
        1 : Width := strtoint( theValue );
        2 : fLastFile := theValue;
        3 : fLastFolder := theValue;
      end;
  end;

end;

procedure TfrmCommandsVar.FormClose(Sender : TObject; var CloseAction : TCloseAction);
begin
  HandleFormSettings( sdSave );
end;

procedure TfrmCommandsVar.btnOKClick(Sender : TObject);
begin
  //FCanClose := true;
  Modalresult := mrOK;
end;

procedure TfrmCommandsVar.btnFileNameClick( Sender : TObject );
begin

  if not DoSingleInput( csiChooseAFile, fLastFile, simFile, self, false ) then
  begin
    edtInput.SetFocus;
    exit;
  end;

  edtInput.Text := fLastFile;
  edtInput.SetFocus;
  //btnApply.Click;//makes it automatic but not good, can't read whats going on

end;

procedure TfrmCommandsVar.btnFolderNameClick( Sender : TObject );
begin
  if not DoSingleInput( csiChooseAFolder, fLastFolder, simDir, self, false ) then
  begin
    edtInput.SetFocus;
    exit;
  end;

  edtInput.Text := fLastFolder;
  edtInput.SetFocus;
end;

procedure TfrmCommandsVar.btnCancelClick(Sender : TObject);
begin
  //FCanClose := true;
  Modalresult := mrCancel;
end;

procedure TfrmCommandsVar.btnApplyClick( Sender : TObject );
begin

  edtInput.Text := trim( edtInput.Text );
  if pos( ' ', edtInput.Text ) > 0 then
//I chose QuotedStr here over doublequotedstring. Good decision? I don't know. These will be loaded as params
//to a process and the process is gonna have to sort it out. You lose either way depending on whether the string
//contains matched/unmatched " and '  What a pain.
    edtInput.Text := Quotedstr( edtInput.Text );
  edtCmdLine.Text := stringreplace( edtCmdLine.Text, fTheVariable, edtInput.Text, [] );
  edtInput.Text := '';
  edtInput.SetFocus;
  NextVariable;

end;

procedure TfrmCommandsVar.btnDoneClick( Sender : TObject );
begin
  if fIsDone then
    btnOKClick( self );
end;

procedure TfrmCommandsVar.edtInputKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  if Key = vk_Return then
    btnApply.Click;
end;

procedure TfrmCommandsVar.NextVariable;
var
  Idx : Integer;
begin

  Idx := CmdObjHelper.GetVariableAndPosition( edtCmdLine.Text, fTheVariable );
  if Idx = 0 then
  begin
    fIsDone := true;
    memInfo.Text := CmdObjHelper.GetVariableHelp( fTheVariable, cmsgufcvDoneVariables );
    btnDone.Enabled := true;
    exit;
  end;

  edtCmdLine.SelStart := Idx - 1;
  edtCmdLine.SelLength := cucoVariableSize;

  btnFileName.Enabled := false;
  case CmdObjHelper.GetSignificantVariableFlag( fTheVariable ) of
    'f','F' : btnFileName.Enabled := true;
  end;
  btnFolderName.Enabled := btnFileName.Enabled;

  memInfo.Text := CmdObjHelper.GetVariableHelp( fTheVariable );

end;


procedure TfrmCommandsVar.FormActivate(Sender : TObject);
begin
  if not FIsInitialized then
  begin
    FIsInitialized := true;
  end;

end;

procedure TfrmCommandsVar.FormCreate(Sender : TObject);
begin
  ApplyChangeFont( Self );
  FHasShown := false;
  FIsInitialized := false;
  fIsDone := false;
  fLastFile := '';
  fLastFolder := '';
  btnDone.Caption := cbtn_Done;
end;

procedure TfrmCommandsVar.FormKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  if ( Key = vk_Return ) and fIsDone then
    btnOkClick( self );
end;

end.
{
procedure TfrmCommandsVar.FormCloseQuery(Sender : TObject; var CanClose : boolean);
begin
  CanClose := FCanClose;

  if not FCanClose then
  begin
    if MsgDlgMessage( 'Closing the Form', 'You need to say whether all is OK or whether to cancel.', 'uniquedelvalue1' ) then
      MsgDlg( mdpInformation, false, self );
  end;
end;
 }
