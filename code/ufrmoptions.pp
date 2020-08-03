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
    btnUpdate : TBitBtn;
    btnRootFile : TBitBtn;
    btnSavePath : TBitBtn;
    btnSavePathReset : TBitBtn;
    btnSqlLib : TBitBtn;
    btnSqlLibReset : TBitBtn;
    cbAllowPkexec : TCheckBox;
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
    procedure btnUpdateClick( Sender : TObject );
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
{$IFDEF platAppImage}
    procedure AppImageUpdate;
{$ENDIF}
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

{$IFDEF platAppImage}
  coptcapCheckForUpgrade = 'Need to retrieve update Version';
  coptmsgCheckForUpgrade =
    'The website needs to be checked for any upgrade Version. Should commandoo check, or do you want to do it?'
    + LineEnding + LineEnding
    + 'YES = commandoo will check'
    + LineEnding
    + 'NO = you will check'
    + LineEnding
    ;
  coptcapCheckForUpgradeHowTo = 'Check for Update Instructions';
  coptmsgCheckForUpgradeHowTo =
    'The upgrade Version SEQUENCE number is stored in a file on the website, you need compare your '
    + 'current program SEQUENCE number (also found in ABOUT) to that number.'
    + LineEnding + LineEnding
    + 'Your current PROGRAM SEQUENCE number is:   %d'
    + LineEnding + LineEnding
    + 'The default location to check is (if be you have newer information use that site):'
    + LineEnding;
  coptmsgCheckForUpgradeDetails =
    LineEnding + LineEnding
    + 'You can check using your browser which will either display the number or download the file '
    + 'which you can then open and inspect. Or, using the terminal (or an entry in commandoo), use the '
    + 'following Command Line:'
    + LineEnding
    + '%s'
    + LineEnding + LineEnding
    + 'Similarly you can check what the update entails at this link:'
    + LineEnding
    + '%s'
    + LineEnding + LineEnding
    + '...where "#" is the number you find for the upgrade Version SEQUENCE number.'
    + LineEnding + LineEnding
    ;
  coptmsgCheckForUpgradeHowTo2 =
    'If the number from the website is greater than your current number then an upgrade is available. '
    +'In that case...'
    + LineEnding + LineEnding
    ;
  coptmsgCheckForUpgradeHowToFinal =
    'Go to:'
    + LineEnding
    + '%s'
    + LineEnding + LineEnding
    + 'The download is there (marked by the NEW sequence number) as well as instructions.'
    + LineEnding + LineEnding;

  coptmsgCheckForUpgradeUpgradeNotes =
    'The upgrade notes are:'
    + LineEnding
    + '-------------------------'
    + LineEnding;
  coptcapCheckForUpgradeWget = 'Required file "wget" missing';
  coptmsgCheckForUpgradeWget =
    'To check for upgrade commandoo needs "wget", but it could not be found. Install it and re-try, or '
    + 'use answer NO answer on first option and use the instructions.';
  coptcapCheckForUpgradeBadFile = 'The link''s contents were bad.';
  coptmsgCheckForUpgradeBadFile = '%s  It should have been a single number, check the link you used!';
  coptcapCheckForUpgradeUpgradeAvailable = 'Upgrade Available';
  coptmsgCheckForUpgradeLatest = 'You have the latest version of commandoo.';
{$ENDIF}

  cOptLabelHints =
    'If this line is too long, Dbl-click it to see the '
    + LineEnding
    + 'full text.'
    + LineEnding + LineEnding
    + '<end>'
    + LineEnding;

  cAddLangHint =
    'Add new language packs via this button. '
    + LineEnding + LineEnding
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
    + '(Wookie is for fun but unintelligable! If you try it, '
    + LineEnding
    + 'take note of how to change the language back to something '
    + LineEnding
    + 'readable!)'
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

{$IFDEF platAppImage}
const
  cWgetCommandLineUpgradeCheck = 'wget -q -O - %s';
{$ENDIF}


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

{$IFDEF platAppImage}
procedure TfrmOptions.AppImageUpdate;
var
  DefSitefolder, DefVerFile, WgetCmd, ReadmeFile : string;
  FetchedVer : integer;
  UpdateInfo : string;
begin
  DefSitefolder := cWebSiteBase + cWebSiteDownloads;
  DefVerFile := format( DefSitefolder, [ Upgrade_GetUpgradeVersionFileName ] );
  WgetCmd := format( cWgetCommandLineUpgradeCheck, [ DefVerFile ] );
  ReadMeFile := format( DefSitefolder, [ Upgrade_GetUpgradeInfoFileName( '#' ) ] );

  MsgDlgMessage( coptcapCheckForUpgrade, coptmsgCheckForUpgrade );
  if MsgDlgConfirmation( self ) = mrNo then
  begin
    MsgDlgMessage( coptcapCheckForUpgradeHowTo,
                   format( coptmsgCheckForUpgradeHowTo, [ c_PROG_VersionUpgradeCount ] )
                   + DefVerFile
                   + format( coptmsgCheckForUpgradeDetails, [ WgetCmd, ReadMeFile ] )
                   + coptmsgCheckForUpgradeHowTo2
                   + format( coptmsgCheckForUpgradeHowToFinal, [ cWebSiteBase + cWebSiteCommando ] )
                 );
    MsgDlgInfo( self );
    exit;
  end;

  if not SystemFileFound( 'wget' ) then
  begin
    MsgDlgMessage( coptcapCheckForUpgradeWget, coptmsgCheckForUpgradeWget );
    MsgDlgInfo( self );
    exit;
  end;

  UpdateInfo := '';
  FetchedVer := TfrmMain( Owner ).WgetUpgradeVer( DefVerFile, UpdateInfo );

  case FetchedVer of
    0  : exit; //canceled
    -1 :
      begin
        MsgDlgMessage( coptcapCheckForUpgradeBadFile, format( coptmsgCheckForUpgradeBadFile, [ coptcapCheckForUpgradeBadFile ] ) );
        MsgDlgInfo( self );
        exit;
      end;
  end;

  if FetchedVer > c_PROG_VersionUpgradeCount then
  begin
    MsgDlgMessage( coptcapCheckForUpgradeUpgradeAvailable, coptcapCheckForUpgradeUpgradeAvailable
                                                           + LineEnding + LineEnding
                                                           + format( coptmsgCheckForUpgradeHowToFinal, [ cWebSiteBase + cWebSiteCommando ] )
                                                           + coptmsgCheckForUpgradeUpgradeNotes
                                                           + UpdateInfo
                                                           )
                                                           ;
    MsgDlgInfo( self );
  end
  else if FetchedVer = c_PROG_VersionUpgradeCount then
  begin
    MsgDlgMessage( '', coptmsgCheckForUpgradeLatest );
    MsgDlgInfo( self );
  end
  else begin
    MsgDlgMessage( '', 'Your version is newer!! The developer has forgotten to put upgrade files on the specified URL!' );
    MsgDlgInfo( self );
  end;

end;
{$ENDIF}

procedure TfrmOptions.btnUpdateClick( Sender : TObject );
begin
{$IFDEF platAppImage}
  AppImageUpdate;
{$ENDIF}
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
//Since I want translations immediately the label captions get translated back to the defatul "..."
//This causes the program to think the the user changed settings and makes for some serious errors.
//these are problems only (?) on the Options form. As one can see this was also a problem for the
//cbMaxOutput combobox, I had forgotten about this.
  TripleDotProtectorSavePath : string;
  TripleDotProtectorBaseFolder : string;
  TripleDotProtectorSqlLib : string;
  TripleDotProtectorRootFile : string;
begin
  //changing languages is messing with the "translated" combobox items
    IdxMO := cbMaxOutput.ItemIndex;
    TripleDotProtectorSavePath := lblSavePath.Caption;
    TripleDotProtectorBaseFolder := lblBaseFolder.Caption;
    TripleDotProtectorSqlLib := lblSqlLib.Caption;
    TripleDotProtectorRootFile := lblRootFile.Caption;

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
      lblSavePath.Caption   := TripleDotProtectorSavePath;
      lblBaseFolder.Caption := TripleDotProtectorBaseFolder;
      lblSqlLib.Caption     := TripleDotProtectorSqlLib;
      lblRootFile.Caption   := TripleDotProtectorRootFile;
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

  str := TfrmMain( Owner ).GetDefaultWritingToPath;
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
  FormAutoAdjustLayout( self );
  ApplyChangeFont( Self );
  UpdateShared;
  fHasShown := false;
  lblSqlLib.Hint := cOptLabelHints;
  lblBaseFolder.Hint := cOptLabelHints;
  lblSavePath.Hint := cOptLabelHints;
{$IFnDEF platAppImage}
  btnUpdate.Visible := false;
{$ENDIF}
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
    cbAllowPkexec.Hint := cPkexecHintOptions + cPkexecHint;
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

