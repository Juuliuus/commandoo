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
    btnResetDlgs : TButton;
    btnTerminal : TButton;
    btnRootFile : TButton;
    btnSqlLib : TButton;
    btnTerminalReset : TButton;
    btnSqlLibReset : TButton;
    Button2 : TButton;
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
    edtRootFile : TEdit;
    edtTerminal : TEdit;
    edtSqlLib : TEdit;
    FrameHint1 : TFrameHint;
    lblDisplayMax : TLabel;
    lblChangeFont : TLabel;
    lblTerminalCap : TLabel;
    lblRootFileCap : TLabel;
    lblMaxOutput : TLabel;
    lblSqlLibCap : TLabel;
    lblThreatLevel : TLabel;
    lblLanguage : TLabel;
    pnlAdvOpt : TPanel;
    ShapeLangOK : TShape;
    speDisplayMax : TSpinEdit;
    speChangeFont : TSpinEdit;
    StatusBar1 : TStatusBar;
    tmrLangOK : TTimer;
    ToolBar1 : TToolBar;
    procedure btnAddLangClick(Sender : TObject);
    procedure btnAddLangOKClick( Sender : TObject );
    procedure btnRootFileClick( Sender : TObject );
    procedure btnSqlLibClick( Sender : TObject );
    procedure btnSqlLibResetClick( Sender : TObject );
    procedure btnTerminalClick(Sender : TObject);
    procedure btnResetDlgsClick(Sender : TObject);
    procedure btnTerminalResetClick( Sender : TObject );
    procedure cbLanguageChange(Sender : TObject);
    procedure FormClose(Sender : TObject; var CloseAction : TCloseAction);
    procedure FormCreate( Sender : TObject );
    procedure FormShow(Sender : TObject);
    procedure speChangeFontChange( Sender : TObject );
    procedure speDisplayMaxKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure tmrLangOKTimer( Sender : TObject );
  private
    fSUFile : string;
    fSUParam1 : string;
    fSUParam2 : string;
    procedure HandleFormSettings(TheType : TSettingsDirective);
    procedure SetSUFile( AValue : string );
    procedure SetSUParam1( AValue : string );
    procedure SetSUParam2( AValue : string );
    procedure UpdateShared;
    { private declarations }
  public
    { public declarations }
    VerifyLangIndex : integer;
    property SUFile : string read fSUFile write SetSUFile;
    property SUParam1 : string read fSUParam1 write SetSUParam1;
    property SUParam2 : string read fSUParam2 write SetSUParam2;
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
  //, ufrmColorTest
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

  ccapOptDefTerminal = 'Reset to default Terminal?';
  cmsgOptDefTerminal = 'Are you sure you want to reset the Terminal Program to Default "%s"?';
  ccapOptTerminalProgram = 'Terminal Program';
  cmsgOptSqliteActiveAlready = 'sqlite is active, no need to change it.';
  cmsgOptSqliteLibFile = 'sqlite Library file';
  ccapOptResetShowNos = 'Reset Show No More?';
  cmsgOptResetShowNos = 'Are you sure you want to re-enable Optional Messages / Information?';

//const
//  cmsgOptEngPeculiar = 'You have chosen English. Due to peculiarities of translation '
//                       + 'files you need to restart the program to load English in the Main Window.';

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

procedure TfrmOptions.btnTerminalResetClick( Sender : TObject );
var
  Str : String;
begin

  Str := 'xterm';
  MsgDlgMessage( ccapOptDefTerminal,
                  format( cmsgOptDefTerminal, [ Str ] )
               );
  if MsgDlgConfirmation( self ) = mrNo then
    exit;
  edtTerminal.Text := Str;

end;

procedure TfrmOptions.btnTerminalClick(Sender : TObject);
var
  Str : String;
begin
  Str := edtTerminal.Text;
  if not DoSingleInput( ccapOptTerminalProgram, Str, simFile, self, false ) then
    exit;

  if Str <> '' then
  begin
    if CheckFileNotFound( Str ) then
      exit;
    edtTerminal.Text := Str;
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
    lblCurrFile.Caption := SUFile;

    ShowModal;

    if ModalResult = mrOK then
    begin
      SUFile := edtsufile.Text;
      SUParam1 := edtsuparam1.Text;
      SUParam2 := edtsuparam2.Text;
      edtRootFile.Text := SUFile + ' ' + SUParam1 + ' ' + SUParam2;
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

procedure TfrmOptions.FormClose(Sender : TObject; var CloseAction : TCloseAction);
begin
  HandleFormSettings( sdSave );
end;

procedure TfrmOptions.FormCreate( Sender : TObject );
begin
  ApplyChangeFont( Self );
  UpdateShared;
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
  HandleFormSettings( sdLoad );
end;


procedure TfrmOptions.speChangeFontChange( Sender : TObject );
begin
  //10 / (128/96)
  //my chosen ref: 10, lazarus changes it to the above=7.5=7 font size when screen is 128
  //str := 'SIppiX: ' + Inttostr( Graphics.ScreenInfo.PixelsPerInchX );
  //Showmessage( str );
  //str := 'SIppiY: ' + Inttostr( Graphics.ScreenInfo.PixelsPerInchY );
  //Showmessage( str );
  //str := 'ScreenPPI: ' + Inttostr( Screen.PixelsPerInch );
  //Showmessage( str );

  globFontOffset := speChangeFont.Value;
  ApplyChangeFont( Self, true );

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

procedure TfrmOptions.SetSUFile( AValue : string );
begin
  if fSUFile = AValue then Exit;
  fSUFile := AValue;
end;

procedure TfrmOptions.SetSUParam1( AValue : string );
begin
  if fSUParam1 = AValue then Exit;
  fSUParam1 := AValue;
end;

procedure TfrmOptions.SetSUParam2( AValue : string );
begin
  if fSUParam2 = AValue then Exit;
  fSUParam2 := AValue;
end;

end.

