unit ufrmOptions;

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
  Classes, SysUtils, LazFileUtils,
  Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons
  , lcltype {THis is needed for key up keyboard constants}
  , HintFrame
  , unitsharedobj
  , unitGlobForm
  , Spin, ComCtrls
  ;

type

  { TfrmOptions }

  TfrmOptions = class(TForm)
    Bevel1 : TBevel;
    btnAddLang : TBitBtn;
    btnAddLangOK : TBitBtn;
    btnBaseFolder : TBitBtn;
    btnBaseFolderReset : TBitBtn;
    btnDone : TBitBtn;
    btnResetDlgs : TBitBtn;
    btnRootFile : TBitBtn;
    btnSavePath : TBitBtn;
    btnSavePathReset : TBitBtn;
    btnSqlLib : TBitBtn;
    btnSqlLibReset : TBitBtn;
    cbAllowESCOutput : TCheckBox;
    cbAllowMultipleOpens : TCheckBox;
    cbCareful : TCheckBox;
    cbCaution : TCheckBox;
    cbDanger : TCheckBox;
    cbHarmless : TCheckBox;
    cbLanguage : TComboBox;
    cbLargerFont : TCheckBox;
    cbManRefreshFavorites : TCheckBox;
    cbMaxOutput : TComboBox;
    cbSqlDB : TCheckBox;
    cbTextDB : TCheckBox;
    cbUnspecified : TCheckBox;
    cbMissingSqlMsg : TCheckBox;
    DirectoryDialog : TSelectDirectoryDialog;
    FrameHint1 : TFrameHint;
    lblBaseFolder : TLabel;
    lblDisplayMax : TLabel;
    lblMaxOutputWait : TLabel;
    lblLanguage : TLabel;
    lblMaxOutput : TLabel;
    lblRootFile : TLabel;
    lblSavePath : TLabel;
    lblSqlLib : TLabel;
    lblThreatLevel : TLabel;
    ShapeLangOK : TShape;
    ShapeLangOK1 : TShape;
    ShapeLangOK2 : TShape;
    ShapeLangOK3 : TShape;
    speDisplayMax : TSpinEdit;
    speMaxOutputWait : TSpinEdit;
    tmrLangOK : TTimer;
    procedure btnAddLangClick(Sender : TObject);
    procedure btnAddLangOKClick( Sender : TObject );
    procedure btnBaseFolderClick( Sender : TObject );
    procedure btnBaseFolderResetClick( Sender : TObject );
    procedure btnRootFileClick( Sender : TObject );
    procedure btnSqlLibClick( Sender : TObject );
    procedure btnSqlLibResetClick( Sender : TObject );
    procedure btnSavePathClick(Sender : TObject);
    procedure btnResetDlgsClick(Sender : TObject);
    procedure btnSavePathResetClick( Sender : TObject );
    procedure cbLanguageChange(Sender : TObject);
    procedure cbLargerFontChange( Sender : TObject );
    procedure FormClose(Sender : TObject; var CloseAction : TCloseAction);
    procedure FormCreate( Sender : TObject );
    procedure FormShow(Sender : TObject);
    procedure lblSavePathDblClick( Sender : TObject );
    procedure speDisplayMaxKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure tmrLangOKTimer( Sender : TObject );
  private
    fRoot_File : string;
    fHasShown : boolean;
    procedure HandleFormSettings(TheType : TSettingsDirective);
    procedure UpdateShared;
    { private declarations }
  public
    { public declarations }
    VerifyLangIndex : integer;
    property Root_File : string read fRoot_File write fRoot_File;
  end;

var
  frmOptions : TfrmOptions;

implementation

uses ufrmMsgDlg, unitLanguages
  , juusgen
  , uSingleInput
  , linuxtrix
  , ufrmMain
  , ufrmSuperUserFile
  , unitDBConstants
  , unitGlob
  , strconst_en
  , strconst_prog
  ;

resourcestring

  cOptLabelHints =
    'If this line is too long, Dbl-click it to see the '
    + LineEnding
    + 'full text.'
    + LineEnding + LineEnding
    + '<end>'
    + LineEnding;

  cAddLangHint =
    'Add new language packs via this button. '
    + LineEnding
    + 'Unfortunately at this time there is only '
    + LineEnding
    + 'English and Wookie (haha) and Pyrate! '
    + LineEnding + LineEnding
    + 'If you are so kind as to provide a translation '
    + LineEnding
    + 'for your native language, then that can be '
    + LineEnding
    + 'there too. '
    + LineEnding + LineEnding
    + 'To use you simply navigate to the new language .po '
    + LineEnding
    + 'file and select it. It will be added into the '
    + LineEnding
    + 'system automatically. '
    + LineEnding + LineEnding
    + '<end> ';

  clblThreatLevelHint =
    'Each Command Line has an associated '
    + LineEnding
    + '"Threat Level". Checking the checkboxes '
    + LineEnding
    + 'below will cause a confirmation dialog to '
    + LineEnding
    + 'be displayed for that threat level. '
    + LineEnding + LineEnding
    + 'This is a safety measure which you can '
    + LineEnding
    + 'choose to use or not. '
    + LineEnding + LineEnding
    + '<end>';

  cTextOrSqlDBHint =
    'You have the option to use either, or both, '
    + LineEnding
    + 'types of Database: SQL based (sqlite) or '
    + LineEnding
    + 'simple TEXT based. '
    + LineEnding + LineEnding
    + 'NOTE: if you choose to use both be aware '
    + LineEnding
    + 'they ARE SEPARATE Databases. '
    + LineEnding + LineEnding
    + 'When these are checked then the choice for '
    + LineEnding
    + 'the use of them will be present in the '
    + LineEnding
    + 'Switch Profiles Window. '
    + LineEnding + LineEnding
    + 'If you prefer one over the other, or don''t '
    + LineEnding
    + 'want either, uncheck these and they will '
    + LineEnding
    + 'no longer be choices in the Switch DB '
    + LineEnding
    + 'window. '
    + LineEnding + LineEnding
    + 'Most likely on initial install you downloaded '
    + LineEnding
    + 'one or the other, or both, pre-filled in databases, '
    + LineEnding
    + 'SQL / Text. Unchecking the other '
    + LineEnding
    + 'here then cleans up your Switch window. '
    + LineEnding + LineEnding
    + '<end>'
    + LineEnding
    ;
  ccapOptSqlite_soname = 'Unusual sqlite 3 library name...';
  cmsgOptSqlite_soname =
    'The sqlite library name is usually "%s" with a numeric character after it, like "%s".'
    + LineEnding + LineEnding
    + 'You have chosen "%s". Choosing the wrong library could cause the program to crash when accessing '
    + 'sql DB''s. Please, be sure you know what you are doing! Are you sure you want use this library?'
    + LineEnding
    ;


  cmsgOptDefSavePathIsDefault = 'The shown Save Path is already the default.';
  ccapOptDefSavePath = 'Reset Path to Default?';
  cmsgOptDefWritePathStr = 'Base';
  cmsgOptDefSavePathStr = 'Saving';
  cmsgOptDefSavePath = 'Are you sure you want to reset the %s path to Default "%s"?';
  ctitOptDefBasePath = 'Select new config path';
  ccapOptDefBasePath = 'Reset base config path?';
  cmsgOptDefBasePath = 'Are you sure you want to reset the base config path to "%s"?';
  cmsgOptFileSavePathInvalid = 'Folder "%s" does not exist.';
  ccapOptFileSavePath = 'File saving path';
  cmsgOptSqliteActiveAlready = 'sqlite is active, no need to change it.';
  cmsgOptSqliteLibFile = 'sqlite Library file';
  ccapOptResetShowNos = 'Reset Show No More?';
  cmsgOptResetShowNos = 'Are you sure you want to re-enable Optional Messages / Information?';


{$R *.lfm}

{ TfrmOptions }

procedure TfrmOptions.btnResetDlgsClick(Sender : TObject);
begin
  MsgDlgMessage( ccapOptResetShowNos, cmsgOptResetShowNos  );
  if MsgDlgConfirmation( self ) = mrNo then
    exit;
  MsgDlgParams.ResetDoNotShowList;
end;

procedure TfrmOptions.btnSavePathResetClick( Sender : TObject );
var
  Str : String;
begin

  Str := TfrmMain( Owner ).WritingToPath;
  if Str = lblSavePath.Caption then
  begin
    MyShowMessage( cmsgOptDefSavePathIsDefault, self );
    exit;
  end;

  MsgDlgMessage( ccapOptDefSavePath, format( cmsgOptDefSavePath, [ cmsgOptDefSavePathStr, Str ] ) );
  if MsgDlgConfirmation( self ) = mrNo then
    exit;
  lblSavePath.Caption := Str;

end;

procedure TfrmOptions.btnSavePathClick(Sender : TObject);
var
  Str : String;
begin
  Str := lblSavePath.Caption;
  if not DoSingleInput( ccapOptFileSavePath, Str, simDir, self, false ) then
    exit;

  if Str <> '' then
  begin
    if not DirectoryExists( str ) then
    begin
      MsgDlgMessage( ccapError, format( cmsgOptFileSavePathInvalid, [ Str ] ) );
      MsgDlgInfo( self );
      exit;
    end;

    lblSavePath.Caption := IncludeTrailingPathDelimiter( Str );
  end;
end;

procedure TfrmOptions.btnAddLangClick(Sender : TObject);
var
  idx, CancelIdx : Integer;
begin

  CancelIdx := cbLanguage.ItemIndex;

  Idx := Languages.InstallNewLanguage( cbLanguage.Items );

  if Idx < 0 then
  begin
    cbLanguage.ItemIndex := CancelIdx;
    exit;
  end
  else cbLanguage.ItemIndex := Idx;

  cbLanguageChange( cbLanguage );

end;

procedure TfrmOptions.btnAddLangOKClick( Sender : TObject );
var
  IdxMO : Integer;
  LangId : String;
begin
  //changing languages is messing with the "translated" combobox items
    IdxMO := cbMaxOutput.ItemIndex;

    try

      LangId := GetMainLanguageID( cbLanguage.Text );

      if Languages.Under18( LangID ) then
      begin
        cbLanguage.ItemIndex := VerifyLangIndex;
        exit;
      end;

      if not Languages.UpdateProgramLang( LangId ) then
        exit;

      UpdateShared;
      TfrmMain( Owner ).UpdateSharedHintsAndCaptions;
      VerifyLangIndex := cbLanguage.ItemIndex;

    finally
      btnAddLangOK.Visible := false;
      shapeLangOK.Visible := false;
      tmrLangOK.Enabled := false;
      cbMaxOutput.ItemIndex := IdxMO;
    end;
end;

procedure TfrmOptions.btnBaseFolderClick( Sender : TObject );
begin
  if TfrmMain( Owner ).SuperUser then
  begin
    showmessage( 'Not allowed' );
    exit;
  end;
  DirectoryDialog.InitialDir := lblBaseFolder.Caption;
  DirectoryDialog.Title := ctitOptDefBasePath;
  if DirectoryDialog.Execute then
  begin
    MsgDlgMessage( ccapOptDefBasePath, format( cmsgOptDefBasePath, [ DirectoryDialog.FileName ] ) );
    if MsgDlgConfirmation( self ) = mrNo then
      exit;
    lblBaseFolder.Caption := IncludeTrailingPathDelimiter( DirectoryDialog.FileName );
  end;

end;

procedure TfrmOptions.btnBaseFolderResetClick( Sender : TObject );
var
  str : string;
begin
  if TfrmMain( Owner ).SuperUser then
  begin
    showmessage( 'Not allowed' );
    exit;
  end;

  str := GetAppConfigDir( False );
  if str = lblBaseFolder.Caption then
    exit;
  MsgDlgMessage( ccapOptDefSavePath, format( cmsgOptDefSavePath, [ cmsgOptDefWritePathStr, Str ] ) );
  if MsgDlgConfirmation( self ) = mrNo then
    exit;

  lblBaseFolder.Caption := str;
end;

procedure TfrmOptions.btnRootFileClick( Sender : TObject );
begin
  with TfrmSuperUserFile.Create( self ) do
  try
    lblCurrFile.Caption := fRoot_File;

    ShowModal;

    if ModalResult = mrOK then
    begin
      fRoot_File := lblCurrFile.Caption;
      lblRootFile.Caption := fRoot_File;
    end;

  finally
    free;
  end;
end;

procedure TfrmOptions.btnSqlLibClick( Sender : TObject );
var
  Str, Comp : string;
begin
  if TInfoServer.SqliteIsActive then
  begin
    Showmessage( cmsgOptSqliteActiveAlready );
    exit;
  end;

  Str := lblSqlLib.Caption;
  if not DoSingleInput( cmsgOptSqliteLibFile, Str, simFile, self, false ) then
    exit;

  if Str <> '' then
  begin
    if CheckFileNotFound( Str ) then
      exit;
    Comp := extractfilename( str );
//    Comp := 'libsqlite3.so'; //testing

    if ( pos( cSqlite_soname, Comp ) <> 1 ) or ( Comp <> cSqlite_soname + '.0' ) then
    begin
      MsgDlgMessage( ccapOptSqlite_soname, format( cmsgOptSqlite_soname, [ cSqlite_soname, cSqlite_soname + cSqlite_sonameExt, Str ] ) );
      if MsgDlgConfirmation( self ) = mrNo then
        exit;
    end;
    lblSqlLib.Caption := Str;
  end;

end;

procedure TfrmOptions.btnSqlLibResetClick( Sender : TObject );
var
  str, Msg : string;
begin
  if TInfoServer.SqliteIsActive then
  begin
    Showmessage( cmsgOptSqliteActiveAlready );
    exit;
  end;
  str := '';
  Msg := '';
  if not TInfoServer.SqliteInstalled( str, Msg ) then
  begin
    MsgDlgMessage( ccapOptSqliteSearch, cmsgOptSqliteSearch );
    if MsgDlgConfirmation( self ) = mrNo then
      exit;
    MsgDlgMessage( ccapOptSqliteSearch, TfrmMain( Owner ).RunCmdLineExternal( cSqliteLocateCommand ) );
    MsgDlgInfo( self );
  end;

  lblSqlLib.Caption := str;

end;

procedure TfrmOptions.cbLanguageChange(Sender : TObject);
begin
  tmrLangOK.Enabled := true;
  btnAddLangOK.Visible := true;
  shapeLangOK.Visible := true;
end;

procedure TfrmOptions.cbLargerFontChange( Sender : TObject );
begin
  if not fHasShown then
    exit;
  ApplyChangeFont( Self, true );
end;

procedure TfrmOptions.FormClose(Sender : TObject; var CloseAction : TCloseAction);
begin
  HandleFormSettings( sdSave );
end;

procedure TfrmOptions.FormCreate( Sender : TObject );
begin
  font.size := cDefaultFontSize;
  ApplyChangeFont( Self );
  UpdateShared;
  fHasShown := false;
  lblSqlLib.Hint := cOptLabelHints;
  lblBaseFolder.Hint := cOptLabelHints;
  lblSavePath.Hint := cOptLabelHints;
end;

procedure TfrmOptions.UpdateShared;
begin
  lblThreatLevel.Hint := clblThreatLevelHint;//translation also update properly
  cbUnspecified.Hint := lblThreatLevel.Hint;
  cbHarmless.Hint := lblThreatLevel.Hint;
  cbCareful.Hint := lblThreatLevel.Hint;
  cbCaution.Hint := lblThreatLevel.Hint;
  cbDanger.Hint := lblThreatLevel.Hint;

  cbSqlDB.Hint := cTextOrSqlDBHint;
  cbTextDB.Hint := cbSqlDB.Hint;

  btnAddLang.Hint := cAddLangHint;

  cbHarmless.Caption := '&C  ' + cThreatLevelHarmless;
  cbCareful.Caption := '&D  ' + cThreatLevelCareful;
  cbCaution.Caption := '&E  ' + cThreatLevelCaution;
  cbDanger.Caption := '&F  ' + cThreatLevelDanger;
  btnAddLang.Caption := '&Q  ' + ccapGenericAdd;

end;

procedure TfrmOptions.FormShow(Sender : TObject);
begin
  if not fHasShown then
  begin
    HandleFormSettings( sdLoad );
    btnDone.caption := cbtn_Done;
    FrameHint1.cbHints.Caption := ccbHintsEnglishOverride;
    fHasShown := true;
  end;
end;

procedure TfrmOptions.lblSavePathDblClick( Sender : TObject );
begin
  MsgDlgMessage( ccapOverflow, trim( TLabel( Sender ).caption ) );
  MsgDlgInfo( self );
end;

procedure TfrmOptions.speDisplayMaxKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  if key = vk_return then
    key := vk_unknown;
end;

procedure TfrmOptions.tmrLangOKTimer( Sender : TObject );
begin
  shapeLangOK.Visible := not shapeLangOK.Visible;
end;

procedure TfrmOptions.HandleFormSettings(TheType : TSettingsDirective);
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


end.

