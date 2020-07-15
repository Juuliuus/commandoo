unit ufrmSuperUserFile;

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

May 2020:
This form was originally designed to select and store the name of the GUI root interfaces,
gksu, gksudo, kdesudo, kdesu.
But this is all deprecated now where one should use either pkexec or admin://

Since they really don't want us running kdesudo and so on, I've ripped out the
ability to do so.

This form is now re-purposed (hence the odd-ish name) to hold the sudo string
the user wants to have used when the Command line is copied and pasted:

ie: <ROOT> kate
will be pasted as:  'sudo kate' or 'su -c "kate"', etc.
}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, HintFrame
  , lcltype {THis is needed for key up keyboard constants}
  , unitsharedobj, Buttons
  , unitGlobForm, StdCtrls, ExtCtrls
  ;

type

  { TfrmSuperUserFile }

  TfrmSuperUserFile = class(TForm)
    btnCancel : TBitBtn;
    bntOK : TBitBtn;
    btnCustom : TBitBtn;
    btnTemplate : TBitBtn;
    btnTest : TBitBtn;
    edtCustom : TEdit;
    FrameHint1 : TFrameHint;
    lblCurr : TLabel;
    lblCurrFile : TLabel;
    rgTemplates : TRadioGroup;
    procedure bntOKClick(Sender : TObject);
    procedure btnCancelClick(Sender : TObject);
    procedure btnCustomClick( Sender : TObject );
    procedure btnTemplateClick( Sender : TObject );
    procedure btnTestClick( Sender : TObject );
    procedure edtCustomKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure FormActivate(Sender : TObject);
    procedure FormClose(Sender : TObject; var CloseAction : TCloseAction);
    procedure FormCloseQuery(Sender : TObject; var CanClose : boolean);
    procedure FormCreate(Sender : TObject);
    procedure FormShow(Sender : TObject);
    procedure rgTemplatesClick( Sender : TObject );
  private
    { private declarations }
    FCanClose : boolean;
    FHasShown : boolean;
    FIsInitialized : boolean;

    procedure HandleFormSettings( const TheType : TSettingsDirective );
  public
    { public declarations }
  end;

var
  frmSuperUserFile : TfrmSuperUserFile;



implementation

{$R *.lfm}

uses ufrmMsgDlg
     , strconst_en
     , strconst_prog
     , unitcommands
     , linuxtrix
     , unitglob
     ;


{ TfrmSuperUserFile }
procedure TfrmSuperUserFile.FormShow(Sender : TObject);
var
  Idx : integer;
begin

  if not FHasShown then
  begin
    FCanClose := false;
    HandleFormSettings( sdLoad );

    FrameHint1.cbHints.Caption := ccbHintsEnglishOverride;

    rgTemplates.Items.Add( '&X  ' + cRootFileSudo );
    rgTemplates.Items.Add( '&Y  ' + cRootFileSu );
    rgTemplates.Items.Add( '&Z  ' + cRootFileSuSession );

    Idx := -1;
    if pos( cRootFileSudo, lblCurrfile.Caption ) > 0 then
      Idx := 0
    else if pos( cRootFileSu, lblCurrfile.Caption ) > 0 then
      Idx := 1
    else if pos( cRootFileSuSession, lblCurrfile.Caption ) > 0 then
      Idx := 2;

    if Idx = -1 then
    begin
      Idx := 0;
      edtCustom.Text := lblCurrfile.Caption;
    end;

    rgTemplates.ItemIndex := Idx;
    if rgTemplates.CanFocus then
      rgTemplates.SetFocus;

    FHasShown := true;

  end;

end;

procedure TfrmSuperUserFile.rgTemplatesClick( Sender : TObject );
begin
  btnTemplate.Click;
end;

procedure TfrmSuperUserFile.HandleFormSettings( const TheType : TSettingsDirective );
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

procedure TfrmSuperUserFile.FormClose(Sender : TObject; var CloseAction : TCloseAction);
begin
  HandleFormSettings( sdSave );
end;

procedure TfrmSuperUserFile.bntOKClick(Sender : TObject);
begin
  FCanClose := true;
  Modalresult := mrOK;
end;

procedure TfrmSuperUserFile.btnCancelClick(Sender : TObject);
begin
  FCanClose := true;
  Modalresult := mrCancel;
end;

procedure TfrmSuperUserFile.btnCustomClick( Sender : TObject );
var
  Idx, tempIdx : integer;
  str, tempstr : string;
begin

  if trim( edtCustom.Text ) = '' then
  begin
    Showmessage( cInputCantBeBlank );
    exit;
  end;

  str := trim( edtCustom.Text );
  str := stringreplace( str, '%S', '%s', [ rfreplaceall ] );//, rfignorecase
  Idx := pos( '%s', str );

  tempstr := copy( str, Idx + 2, maxint );
  tempIdx := pos( '%s', tempstr );

  if ( Idx = 0 ) or ( tempIdx > 0 ) then
  begin
    MsgDlgMessage( ccapRootFileInfo, cmsgRootFileInfo );
    MsgDlgInfo( self );
    exit;
  end;

  lblCurrFile.caption := str;

end;

procedure TfrmSuperUserFile.btnTemplateClick( Sender : TObject );
begin
  lblCurrFile.Caption := copy( rgTemplates.Items[ rgTemplates.ItemIndex ], 5, Maxint );
end;

procedure TfrmSuperUserFile.btnTestClick( Sender : TObject );
begin
  MsgDlgMessage( ccapRootFileTest, format( lblCurrFile.Caption, [ 'myCLine --help'] ) );
  MsgDlgInfo( self );
end;

procedure TfrmSuperUserFile.edtCustomKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  if Key = vk_Return then
    btnCustom.Click;
end;

procedure TfrmSuperUserFile.FormActivate(Sender : TObject);
begin
  if not FIsInitialized then
  begin
    FIsInitialized := true;
  end;

end;

procedure TfrmSuperUserFile.FormCloseQuery(Sender : TObject; var CanClose : boolean);
begin
  CanClose := FCanClose;
end;

procedure TfrmSuperUserFile.FormCreate(Sender : TObject);
begin
  font.size := cDefaultFontSize;
  ApplyChangeFont( Self );
  FHasShown := false;
  FIsInitialized := false;
end;

end.

