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
    btnAddLangOK : TBitBtn;
    btnDone : TBitBtn;
    btnAddLang : TBitBtn;
    btnResetDlgs : TBitBtn;
    btnRootFile : TBitBtn;
    btnSqlLib : TBitBtn;
    btnSqlLibReset : TBitBtn;
    btnSavePath : TBitBtn;
    btnSavePathReset : TBitBtn;
    cbAllowMultipleOpens : TCheckBox;
    cbLanguage : TComboBox;
    cbMaxOutput : TComboBox;
    cbManRefreshFavorites : TCheckBox;
    cbAllowESCOutput : TCheckBox;
    cbUnspecified : TCheckBox;
    cbHarmless : TCheckBox;
    cbCareful : TCheckBox;
    cbCaution : TCheckBox;
    cbDanger : TCheckBox;
    cbSqlDB : TCheckBox;
    cbTextDB : TCheckBox;
    cbLargerFont : TCheckBox;
    edtRootFile : TEdit;
    edtSavePath : TEdit;
    edtSqlLib : TEdit;
    FrameHint1 : TFrameHint;
    lblDisplayMax : TLabel;
    lblTerminalCap : TLabel;
    lblRootFileCap : TLabel;
    lblMaxOutput : TLabel;
    lblSqlLibCap : TLabel;
    lblThreatLevel : TLabel;
    lblLanguage : TLabel;
    pnlAdvOpt : TPanel;
    ShapeLangOK : TShape;
    speDisplayMax : TSpinEdit;
    tmrLangOK : TTimer;
    procedure btnAddLangClick(Sender : TObject);
    procedure btnAddLangOKClick( Sender : TObject );
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
  ;

resourcestring

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

  cmsgOptDefSavePathIsDefault = 'The shown Save Path is already the default.';
  ccapOptDefSavePath = 'Reset Path to config default?';
  cmsgOptDefSavePath = 'Are you sure you want to reset the Saving Path to Default "%s"?';
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
//this was the old code, devised so that if ROOT is running this session, permanent changes are not made to settings.
//here for historical reasons in case I need to consider that again.
//  MsgDlgParams.ResetDoNotShowList( fSuperUser );
end;

procedure TfrmOptions.btnSavePathResetClick( Sender : TObject );
var
  Str : String;
begin

  Str := TfrmMain( Owner ).WritingToPath;
  if Str = edtSavePath.Text then
  begin
    MyShowMessage( cmsgOptDefSavePathIsDefault, self );
    exit;
  end;
  MsgDlgMessage( ccapOptDefSavePath, format( cmsgOptDefSavePath, [ Str ] ) );
  if MsgDlgConfirmation( self ) = mrNo then
    exit;
  edtSavePath.Text := Str;

end;

procedure TfrmOptions.btnSavePathClick(Sender : TObject);
var
  Str : String;
begin
  Str := edtSavePath.Text;
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

    edtSavePath.Text := IncludeTrailingPathDelimiter( Str );
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

      Languages.UpdateProgramLang( LangId );

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

procedure TfrmOptions.btnRootFileClick( Sender : TObject );
begin
  with TfrmSuperUserFile.Create( self ) do
  try
    lblCurrFile.Caption := fRoot_File;

    ShowModal;

    if ModalResult = mrOK then
    begin
      fRoot_File := lblCurrFile.Caption;
      edtRootFile.Text := fRoot_File;
    end;

  finally
    free;
  end;
end;

procedure TfrmOptions.btnSqlLibClick( Sender : TObject );
var
  Str : TCaption;
begin
  if TInfoServer.SqliteIsActive then
  begin
    Showmessage( cmsgOptSqliteActiveAlready );
    exit;
  end;

  Str := edtSqlLib.Text;
  if not DoSingleInput( cmsgOptSqliteLibFile, Str, simFile, self, false ) then
    exit;

  if Str <> '' then
  begin
    if CheckFileNotFound( Str ) then
      exit;
    edtSqlLib.Text := Str;
  end;

end;

procedure TfrmOptions.btnSqlLibResetClick( Sender : TObject );
begin
  if TInfoServer.SqliteIsActive then
  begin
    Showmessage( cmsgOptSqliteActiveAlready );
    exit;
  end;

  edtSqlLib.Text := '';

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
  btnAddLang.Caption := '&Q  ' + cbtn_Add;

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

