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
    btnApply : TBitBtn;
    btnCancel : TBitBtn;
    btnDone : TBitBtn;
    btnFileName : TBitBtn;
    btnFolderName : TBitBtn;
    edtInput : TEdit;
    edtCmdLine : TEdit;
    FrameHint1 : TFrameHint;
    Label1 : TLabel;
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
    procedure SetInputFocus;
    procedure NextVariable;
  public
    { public declarations }
  end;

var
  frmCommandsVar : TfrmCommandsVar;

resourcestring
  cmsgufcvDoneVariables = 'Done with Variable replacement. Edit if needed.'
    + LineEnding + LineEnding
    + 'Invoking "%s" sends it to processing.'
    + LineEnding
    ;


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

const
  cArrowKeyedtInput = 500000;
  cArrowKeymemInfo  = 500001;


procedure TfrmCommandsVar.FormShow(Sender : TObject);
begin

  if not FHasShown then
  begin
    HandleFormSettings( sdLoad );

    btnFileName.Hint := format( cmsgCVChooseFileFolder, [ cmsgCVChooseFile ] );
    btnFolderName.Hint := format( cmsgCVChooseFileFolder, [ cmsgCVChooseFolder ] );

    FrameHint1.cbHints.Caption := ccbHintsEnglishOverride;

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

procedure TfrmCommandsVar.SetInputFocus;
begin
  if edtInput.Canfocus then
    edtInput.SetFocus;
end;

procedure TfrmCommandsVar.btnFileNameClick( Sender : TObject );
begin

  if not DoSingleInput( csiChooseAFile, fLastFile, simFile, self, false ) then
  begin
    SetInputFocus;
    exit;
  end;

  edtInput.Text := fLastFile;
  SetInputFocus;
  //btnApply.Click;//makes it automatic but not good, can't read whats going on

end;

procedure TfrmCommandsVar.btnFolderNameClick( Sender : TObject );
begin
  if not DoSingleInput( csiChooseAFolder, fLastFolder, simDir, self, false ) then
  begin
    SetInputFocus;
    exit;
  end;

  edtInput.Text := fLastFolder;
  SetInputFocus;
end;

procedure TfrmCommandsVar.btnCancelClick(Sender : TObject);
begin
  //FCanClose := true;
  Modalresult := mrCancel;
end;

procedure TfrmCommandsVar.btnApplyClick( Sender : TObject );
var
  editStr : string;
begin

  if fIsDone then
  begin
    editStr := edtCmdLine.Text;
    if DoSingleInput( ccapGenericEdit, editStr, simEdit, self, false ) then
      edtCmdLine.Text := editStr;
    exit;
  end;

  edtInput.Text := trim( edtInput.Text );
  //I chose QuotedStr here over doublequotedstring. Good decision? I don't know. These will be loaded as params
  //to a process and the process is gonna have to sort it out. You lose either way depending on whether the string
  //contains matched/unmatched " and '  What a pain.
  if pos( ' ', edtInput.Text ) > 0 then
    edtInput.Text := Quotedstr( edtInput.Text );
  edtCmdLine.Text := stringreplace( edtCmdLine.Text, fTheVariable, edtInput.Text, [] );
  edtInput.Text := '';
  SetInputFocus;
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
    memInfo.Text := CmdObjHelper.GetVariableHelp( fTheVariable, format( cmsgufcvDoneVariables, [ trim( copy( btnDone.Caption, 3, maxint ) ) ] ) );
    btnDone.Enabled := true;
    edtInput.Visible := false;
    btnFileName.Visible := false;
    btnFolderName.Visible := false;
    btnApply.Caption := copy( btnApply.Caption, 1, 4 ) + ccapGenericEdit;
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
  font.size := cDefaultFontSize;
  FormAutoAdjustLayout( self );
  ApplyChangeFont( Self );
  FHasShown := false;
  FIsInitialized := false;
  fIsDone := false;
  fLastFile := '';
  fLastFolder := '';
  btnDone.Caption := cbtn_Done;
  edtInput.Tag := cArrowKeyedtInput;
  memInfo.Tag := cArrowKeymemInfo;
end;

procedure TfrmCommandsVar.FormKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  //edtInput.Tag := cArrowKeyedtInput;
  //memInfo.Tag := cArrowKeymemInfo;
  if Shift = [ ssShift, ssCtrl ] then
  case key of
    VK_LEFT, VK_RIGHT :
      if assigned( self.ActiveControl ) then
      begin
        case self.ActiveControl.Tag of
          cArrowKeyedtInput : TryFocus( memInfo );
          //cArrowKeymemInfo : TryFocus( edtInput );
          else TryFocus( edtInput );
        end;
        Key := VK_UNKNOWN;
      end;
  end;
end;

end.
{

//if ( Key = vk_Return ) and fIsDone then
//  btnOkClick( self );

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
