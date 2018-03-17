unit unitfrmcommandsinput;

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

  { TfrmCommandsInput }

  TfrmCommandsInput = class(TForm)
    btnCancel : TBitBtn;
    btnDone : TButton;
    cbWordwrap : TCheckBox;
    FrameHint1 : TFrameHint;
    lblProcessInputMsg : TLabel;
    memInput : TMemo;
    procedure btnCancelClick(Sender : TObject);
    procedure btnDoneClick( Sender : TObject );
    procedure cbWordwrapChange( Sender : TObject );
    procedure FormActivate(Sender : TObject);
    procedure FormClose(Sender : TObject; var CloseAction : TCloseAction);
    procedure FormCreate(Sender : TObject);
    procedure FormShow(Sender : TObject);
  private
    { private declarations }
    //FCanClose : boolean;
    FHasShown : boolean;
    FIsInitialized : boolean;
    procedure HandleFormSettings( const TheType : TSettingsDirective );
  public
    { public declarations }
  end;

var
  frmCommandsInput : TfrmCommandsInput;

resourcestring
  ccapufciInputEmpty = 'Input empty';
  cmsgufciInputEmpty = 'There is no input. Is that what you want?';


implementation


{$R *.lfm}

uses ufrmMsgDlg
    , unitGlob
    ;

{ TfrmCommandsInput }

procedure TfrmCommandsInput.FormShow(Sender : TObject);
begin

  if not FHasShown then
  begin
    HandleFormSettings( sdLoad );
    FHasShown := true;
  end;

end;

procedure TfrmCommandsInput.HandleFormSettings( const TheType : TSettingsDirective );
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

        FormSettings.SaveFormSettings( Self.Name );
      end;
    sdLoad :
      While FormSettings.ReadFormSettings( Self.Name, i, theValue ) do
      case i of
        0 : Height := strtoint( theValue );
        1 : Width := strtoint( theValue );
      end;
  end;

end;

procedure TfrmCommandsInput.FormClose(Sender : TObject; var CloseAction : TCloseAction);
begin
  HandleFormSettings( sdSave );
end;

procedure TfrmCommandsInput.btnCancelClick(Sender : TObject);
begin
  //FCanClose := true;
  Modalresult := mrCancel;
end;

procedure TfrmCommandsInput.btnDoneClick( Sender : TObject );
begin
  //FCanClose := true;
  memInput.Text := trim( memInput.Text );
  if memInput.Text = '' then
   if MsgDlgMessage( ccapufciInputEmpty, cmsgufciInputEmpty ) then
    if MsgDlg( mdpConfirmation, true, self ) = mrNo then
      exit;

  Modalresult := mrOK;
end;

procedure TfrmCommandsInput.cbWordwrapChange( Sender : TObject );
begin
  memInput.WordWrap := cbWordWrap.Checked;
end;


procedure TfrmCommandsInput.FormActivate(Sender : TObject);
begin
  if not FIsInitialized then
  begin
    FIsInitialized := true;
  end;

end;

procedure TfrmCommandsInput.FormCreate(Sender : TObject);
begin
  ApplyChangeFont( Self );
  FHasShown := false;
  FIsInitialized := false;
  btnDone.Caption := cbtn_Done;
end;

end.
{
procedure TfrmCommandsInput.FormCloseQuery(Sender : TObject; var CanClose : boolean);
begin
  CanClose := FCanClose;

  if not FCanClose then
  begin
    if MsgDlgMessage( 'Closing the Form', 'You need to say whether all is OK or whether to cancel.', 'uniquedelvalue1' ) then
      MsgDlg( mdpInformation, false, self );
  end;
end;
 }
