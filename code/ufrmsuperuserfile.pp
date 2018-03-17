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

}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, HintFrame
  , lcltype {THis is needed for key up keyboard constants}
  , unitsharedobj, Buttons
  , unitGlobForm, StdCtrls
  ;

type

  { TfrmSuperUserFile }

  TfrmSuperUserFile = class(TForm)
    btnCancel : TBitBtn;
    bntOK : TBitBtn;
    btnsufile : TButton;
    btnTurnOff : TButton;
    btnsuinfo : TButton;
    edtsufile : TEdit;
    edtsuparam1 : TEdit;
    edtsuparam2 : TEdit;
    FrameHint1 : TFrameHint;
    lblCurr : TLabel;
    lbFiles : TListBox;
    lblCurrFile : TLabel;
    procedure bntOKClick(Sender : TObject);
    procedure btnCancelClick(Sender : TObject);
    procedure btnsufileClick( Sender : TObject );
    procedure btnsuinfoClick( Sender : TObject );
    procedure btnTurnOffClick( Sender : TObject );
    procedure edtsuparam1DblClick( Sender : TObject );
    procedure edtsuparam2DblClick( Sender : TObject );
    procedure FormActivate(Sender : TObject);
    procedure FormClose(Sender : TObject; var CloseAction : TCloseAction);
    procedure FormCloseQuery(Sender : TObject; var CanClose : boolean);
    procedure FormCreate(Sender : TObject);
    procedure FormShow(Sender : TObject);
    procedure lbFilesClick( Sender : TObject );
  private
    { private declarations }
    FCanClose : boolean;
    FHasShown : boolean;
    FIsInitialized : boolean;

    fgksudoPath : string;
    fgksuPath : string;
    fkdesudoPath : string;
    fkdesuPath : string;

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
     , unitcommands
     , linuxtrix
     , uSingleInput
     ;

resourcestring
  cmsgsufBadPath = '//xx BAD xx/';
  cmsgBadSUFile = 'File "%s" does not exist. Can not save it.';
  ccapChooseSUFile = 'Choose ROOT authentication file';
  ccapTypeSUFileParam = 'Type in flag / parameter, if any';
  ccapTypeSUFileParam1 = 'First flag / parameter, if any';
  ccapTypeSUFileParam2 = 'Second flag / parameter, if any';
  cmsgRawSudo = 'Not allowed! "su" and "sudo" are not secure to use through a GUI.';
  ccapTurnOffRoot = 'Disallow ROOT permissions?';
  cmsgTurnOffRoot = 'If you do this you will not be able to run command lines as ROOT. Is this what you want?';
  csufHintParams =
    'These edits hold the param flags for the ROOT file. These '
    + LineEnding
    + 'are correct and shouldn''t in general be changed. '
    + LineEnding + LineEnding
    + 'However, someday in future maybe things change, so you can '
    + LineEnding
    + 'edit them by double clicking them. You should know what '
    + LineEnding
    + 'you are doing, any changes you make are at your own risk. '
    + LineEnding + LineEnding
    + 'At the time of writing only kdesu requires params. "-t" forces '
    + LineEnding
    + 'it to return the called process''s output, and "-c" tells it to '
    + LineEnding
    + 'run a command. "-c" MUST BE last.'
    + LineEnding + LineEnding
    + '<end>'
    + LineEnding + LineEnding
    ;


{ TfrmSuperUserFile }

procedure TfrmSuperUserFile.FormShow(Sender : TObject);
var
  aPath : String;
begin

  if not FHasShown then
  begin
    FCanClose := false;
    HandleFormSettings( sdLoad );

//PolicyKit ??!?? when? how?
    aPath := SystemFileLocation( 'kdesudo' );
    if aPath = '' then
      fkdesudoPath := cmsgsufBadPath
    else fkdesudoPath := ExtractFilePath( aPath );

    aPath := SystemFileLocation( 'gksudo' );
    if aPath = '' then
      fgksudoPath := cmsgsufBadPath
    else fgksudoPath := ExtractFilePath( aPath );

    aPath := SystemFileLocation( 'gksu' );
    if aPath = '' then
      fgksuPath := cmsgsufBadPath
    else fgksuPath := ExtractFilePath( aPath );

    fkdesuPath := '/usr/lib/kde4/libexec/';//not in $PATH, you just need to know where it is!!
    if not fileexists( fkdesuPath + 'kdesu' ) then
      fkdesuPath := cmsgsufBadPath;

    lbFiles.Items.Add( 'gksudo' );
    lbFiles.Items.Add( 'gksu' );
    lbFiles.Items.Add( 'kdesudo' );
    lbFiles.Items.Add( 'kdesu' );
//other varieties as of xmas 2016
    //lbFiles.Items.Add( 'ktsuss' ); //??!??
    //lbFiles.Items.Add( 'beesu' ); //redhat

    lbFiles.ItemIndex := lbFiles.Items.IndexOf( ExtractFileName( lblCurrFile.Caption ) );
    lbFilesClick( Self );

    edtsuparam1.Hint := csufHintParams;
    edtsuparam2.Hint := csufHintParams;

    FHasShown := true;

  end;

end;

procedure TfrmSuperUserFile.lbFilesClick( Sender : TObject );
var
  FNName : String;
begin

  if lbFiles.ItemIndex < 0 then
    exit;

  FNName := lbFiles.Items[ lbFiles.ItemIndex ];
  edtsuparam1.Text := '';
  edtsuparam2.Text := '';

//if, in future, you want to add support to forget passwords the settings are below
//kdesudo  kdesu: "-s" forgets password
//gksudo gksu:  none!?! --help gives no flag for forgetting, maybe it forgets always. But why?

  case lbFiles.ItemIndex of
    0 : edtsuFile.Text := fgksudoPath + FNName;
    1 : edtsuFile.Text := fgksuPath + FNName;
    2 : edtsuFile.Text := fkdesudoPath + FNName;
    3 :
      begin
        edtsuFile.Text := fkdesuPath + FNName;
        edtsuparam1.Text := '-t';
        edtsuparam2.Text := '-c';
      end;
  end;

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
  if not fileexists( edtsuFile.Text ) then
  begin
    Showmessage( format( cmsgBadSUFile, [ edtsuFile.Text ] ) );
    exit;
  end;
  FCanClose := true;
  Modalresult := mrOK;
end;

procedure TfrmSuperUserFile.btnCancelClick(Sender : TObject);
begin
  FCanClose := true;
  Modalresult := mrCancel;
end;

procedure TfrmSuperUserFile.btnsufileClick( Sender : TObject );
var
  FNName, str : String;
  fstr : RawByteString;
begin

  FNName := '';
  if not DoSingleInput( ccapChooseSUFile, FNName, simFile, self, false ) then
    exit;

  if not fileexists( FNName ) then
  begin
    Showmessage( format( cmsgBadSUFile, [ FNName ] ) );
    exit;
  end;

  fstr := ExtractFileName( FNName );
  if ( fstr = 'su' ) or ( fstr = 'sudo' ) then
  begin
    Showmessage( cmsgRawSudo );
    exit;
  end;

  edtsufile.Text := FNName;
  edtsuparam1.Text := '';
  edtsuparam2.Text := '';

  str := '';
  if DoSingleInput( ccapTypeSUFileParam1, str, simEdit, self, false ) then
  begin
    edtsuparam1.Text := str;
    str := '';
    DoSingleInput( ccapTypeSUFileParam2, str, simEdit, self, false );
    edtsuparam2.Text := str;
  end;

end;

procedure TfrmSuperUserFile.btnsuinfoClick( Sender : TObject );
begin
  MsgDlgMessage( ccapSUFileInfo, cmsgSUFileInfo );
  MsgDlgInfo( self );
end;

procedure TfrmSuperUserFile.btnTurnOffClick( Sender : TObject );
begin
  MsgDlgMessage( ccapTurnOffRoot, cmsgTurnOffRoot );
  if MsgDlgConfirmation( self ) = mrNo then
    exit;

  edtsuFile.Text := cSuperUserFileOff;
  edtsuparam1.Text := '';
  edtsuparam2.Text := '';
  fCanClose := true;
  ModalResult := mrOK;

end;

procedure TfrmSuperUserFile.edtsuparam1DblClick( Sender : TObject );
var
  str : TCaption;
begin
  str := edtsuparam1.Text;
  if not DoSingleInput( ccapTypeSUFileParam, str, simEdit, self, false ) then
    exit;
  edtsuparam1.Text := str;
end;

procedure TfrmSuperUserFile.edtsuparam2DblClick( Sender : TObject );
var
  str : TCaption;
begin
  str := edtsuparam2.Text;
  if not DoSingleInput( ccapTypeSUFileParam, str, simEdit, self, false ) then
    exit;
  edtsuparam2.Text := str;
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
  ApplyChangeFont( Self );
  FHasShown := false;
  FIsInitialized := false;
end;

end.

