unit ufrmAbout;

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
  Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls, Menus;

type

  { TfrmAbout }

  TfrmAbout = class (TForm)
    btnSaveToFile : TBitBtn;
    btnOK : TBitBtn;
    btnToggle : TButton;
    Image1 : TImage;
    lblTopic : TLabel;
    memOutput : TMemo;
    procedure btnSaveToFileClick( Sender : TObject );
    procedure btnOKClick(Sender : TObject);
    procedure btnToggleClick( Sender : TObject );
    procedure FormCreate( Sender : TObject );
    procedure FormShow( Sender : TObject );
  private
    procedure GetCurrentDBInfo;
    procedure GetCustomDBInfo;
    procedure GetDefaultDBInfo;

    function GetIsCurrent( const SettingsStr : string ) : string;
    procedure ShowAbout;
    procedure ShowIntro;
    function GetNamebtnToggle : string;
    { private declarations }
  public
    { public declarations }
  end;

var
  frmAbout : TfrmAbout;

implementation

uses strconst_prog
     , strconst_en
     , unitDBUtils
     , unitDBConstants
     , ufrmMain
     , juusgen
     , unitglobform
     , uSingleInput
     , ufrmMsgdlg
     ;

{$R *.lfm}

resourcestring
  cmsgAboutCustomDatabaseProfiles1 = '%s based%s';
  cmsgAboutCustomDatabaseProfiles2 = '   Path: "%s"';
  cmsgAboutCurrentDB = 'Current Database Profile: %s ( %s based ): ';
  cmsgAboutDefaultBased = '%s based : %s';

  cmstFilePlural = 'Files';
  cmstFileSingular = 'File';

  cmsgAboutInUse = '%s present';
  cmsgAboutInUseNot = '%s not present';

  cmsgAboutDefaultProfileName = 'Profile Name: %s';
  cmsgAboutDefaultProfileHeader = 'Default Database Profiles:';
  cmsgAboutCustomProfileHeader = 'Custom Database Profile(s):';
  cmsgCurrentDB = '          <== ( CURRENT )';

  ccapAboutToggle = '&A  Toggle: About  =>  Introduction  =>  Tips  =>  FreshStart  =>  upgrade  =>  ibus';


const
  constSeparator = '-----------------------------------';
var
  Frm : TfrmMain;

{ TfrmAbout }

procedure TfrmAbout.btnOKClick(Sender : TObject);
begin
  Close;
end;

procedure TfrmAbout.btnSaveToFileClick( Sender : TObject );
var
  aFile : string;
begin
  //aFile := Frm.SavingToPath + format( cSaveToFileTemplate, [ GetNamebtnToggle ] );
  aFile := Frm.SavingToPath + format( cSaveToFileTemplate, [ lblTopic.Caption ] );


  if DoSingleInput( csiChooseAFile, aFile, simFile, self, false, true ) then
  begin
    if fileexists( aFile ) then
      if MsgdlgMessage( ccapSaveFileExists, format( cmsgSaveFileExists, [ aFile ] ) ) then
        if MsgDlgAttentionConfirm( self ) = mrNo then
          exit;

    memOutput.Lines.SaveToFile( aFile );
    btnSaveToFile.enabled := false;
  end;

end;

procedure TfrmAbout.ShowIntro;
begin
  memOutput.Text := format(cmsgIntro, [cGNU_GPL, cmsgOwnRisk ] );
  memOutput.SelStart := 0;
end;

function TfrmAbout.GetNamebtnToggle : string;
begin
  result := 'info';
  case btnToggle.Tag mod 6 of
    0 : result := cSaveToFileAbout;
    1 : result := cSaveToFileIntro;
    2 : result := cSaveToFileTips;
    3 : result := cSaveToFileFreshStart;
    4 : result := cSaveToFileUpgrade;
    5 : result := cSaveToFileIbus;
  end;
end;

procedure TfrmAbout.btnToggleClick( Sender : TObject );
begin
  btnToggle.Tag := btnToggle.Tag + 1;
  case btnToggle.Tag mod 6 of
    0 : ShowAbout;
    1 : ShowIntro;
    2 : memOutput.Text := format( cmsgTips, [ cmsgFormHotKeys ] );
    3 : memOutput.Text := cmsgFirstLocalRunAbout + format( cmsgFirstLocalRun, [ cmsgFormHotKeys ] );
    4 : memOutput.Text := cmsgCommandooUpgrade;
    5 : memOutput.Text := cmsgIbus;
  end;
  lblTopic.Caption := GetNamebtnToggle;
  btnSaveToFile.enabled := true;
end;

procedure TfrmAbout.FormCreate( Sender : TObject );
begin
  font.size := cDefaultFontSize;
  ApplyChangeFont( Self );
end;

function TfrmAbout.GetIsCurrent( const SettingsStr : string ) : string;
var
  ProName : String;
  ProUseDB : Boolean;
begin
  ProName := ExtractProfileName( SettingsStr );
  ProUseDB := pos( trim( cDefaultDBProfileIsDBStr ), SettingsStr ) > 0;
  result := strif( ( ProName = Frm.ProfileName ) and ( ProUseDb = Frm.UseDB ), cmsgCurrentDB );
end;

procedure TfrmAbout.GetCurrentDBInfo;
begin
  memOutput.Lines.Add( format( cmsgAboutCurrentDB,
                               [ Frm.ProfileName,
                                 strif( Frm.UseDB,
                                        trim( cDefaultDBProfileIsDBStr ),
                                        trim( cDefaultDBProfileIsDBStrNot )
                                       )
                                ]
                             )
                      );

  memOutput.Lines.Add( constSeparator );
  memOutput.Lines.Add( GetDBNameList( Frm.ProfileName, Frm.ProfilePath, not Frm.UseDB ) );
  memOutput.Lines.Add( '' );
  memOutput.Lines.Add( '' );

end;

procedure TfrmAbout.GetDefaultDBInfo;
var
  PathStr , FakeIniEntry: String;
begin
  memOutput.Lines.Add( cmsgAboutDefaultProfileHeader );
  memOutput.Lines.Add( constSeparator );

  memOutput.Lines.Add( format( cmsgAboutDefaultProfileName, [ cDefaultDBProfileName ] ) );

//text based
  FakeIniEntry := cDefaultDBProfileName + cDefaultDBProfileIsDBStrNot;
  memOutput.Lines.Add( format( cmsgAboutDefaultBased,
                                [ trim( cDefaultDBProfileIsDBStrNot ),
                                  strif( IniDBFilesExist( Frm.WritingToPath, cDefaultDBProfileName, false ),
                                         format( cmsgAboutInUse, [ cmstFilePlural ] ),
                                         format( cmsgAboutInUseNot, [ cmstFilePlural ] )
                                       )
                                ]
                             )
                       + GetIsCurrent( FakeIniEntry ) );
//sql based
  PathStr := GenerateDBFilePath( Frm.WritingToPath, cDefaultDBProfileName, cSqlDBExtension );
  FakeIniEntry := cDefaultDBProfileName + cDefaultDBProfileIsDBStr;
  memOutput.Lines.Add( format( cmsgAboutDefaultBased,
                               [
                                 trim( cDefaultDBProfileIsDBStr ),
                                 strif( Fileexists( PathStr ),
                                        format( cmsgAboutInUse, [ cmstFileSingular ] ),
                                        format( cmsgAboutInUseNot, [ cmstFileSingular ] )
                                      )
                               ]
                             )
                       + GetIsCurrent( FakeIniEntry ) );

  memOutput.Lines.Add( '' );
  memOutput.Lines.Add( '' );

end;

procedure TfrmAbout.GetCustomDBInfo;
var
  SL : TStringList;
  i : Integer;
  PathStr : String;
begin
  SL := TStringList.Create;
  try
    GetRawProfileList( Frm.IFS, SL );
    if SL.Count > 0 then
    begin
      SL.Sort;
      memOutput.Lines.Add( cmsgAboutCustomProfileHeader );
      memOutput.Lines.Add( constSeparator );
      for i := 0 to SL.Count - 1 do
      begin
        PathStr := Frm.IFS.ReadString( cSectTabDBProfiles, SL[ i ], '<patherror>' );
        PathStr := strif( PathStr = constDefaultPathValue, Frm.WritingToPath, PathStr );
        memOutput.Lines.Add( format( cmsgAboutCustomDatabaseProfiles1, [ SL[ i ], GetIsCurrent( SL[ i ] ) ] ) );
        memOutput.Lines.Add( format( cmsgAboutCustomDatabaseProfiles2, [ PathStr ] ) );
        memOutput.Lines.Add( '' );
      end;
      memOutput.Lines.Add( '' );
    end;

  finally
    SL.free;
  end;
end;


procedure TfrmAbout.ShowAbout;
begin

  memOutput.Lines.Clear;
  memOutput.Lines.Add( 'About:' );
  memOutput.Lines.Add( '======' );
  memOutput.Lines.Add( '' );

  memOutput.Lines.Add( format( cAboutLine, [ cHandwrittenVersion + cVersionDate ] ) + Frm.GetWidgetString );
  memOutput.Lines.Add( '' );
  memOutput.Lines.Add( format( cAboutBDLine, [ c_DB_HandwrittenVersion + c_DB_VersionDate ] ) );
  memOutput.Lines.Add( '' );
{$IFDEF platAppImage}
//anyway to get the location of the AppImage?
  memOutput.Lines.Add( 'AppImage Version (https://appimage.org/): commandoo[XXX].AppImage' );
  memOutput.Lines.Add( 'Extracted to and running in:' );
{$ENDIF}
  memOutput.Lines.Add( format( cAboutInstalled, [ extractfilePath( Application.Exename ) ] ) );

  memOutput.Lines.Add( '' );
  memOutput.Lines.Add( format( cAboutLanguage, [ Frm.GetPODirectory ] ) );
  memOutput.Lines.Add( '' );
  memOutput.Lines.Add( format( cAboutFormSettings,
                               [ Frm.WritingToPath + cReferenceProgramName + cSectTabFormSettingsExtension ]
                              )
                     );

  memOutput.Lines.Add( '' );
  memOutput.Lines.Add( '' );

  GetCurrentDBInfo;
  GetDefaultDBInfo;
  GetCustomDBInfo;

  memOutput.Lines.Add( cEmail );
  memOutput.Lines.Add( '' );
  memOutput.Lines.Add( cAboutGitHub );
  memOutput.Lines.Add( '' );
  memOutput.Lines.Add( cContribute );
  memOutput.Lines.Add( '' );

  memOutput.Lines.Add( '' );
  memOutput.Lines.Add( cGNU_GPL );
  memOutput.Lines.Add( '' );
  memOutput.Lines.Add( cmsgOwnRisk );


  memOutput.SelStart := 0;
end;


procedure TfrmAbout.FormShow( Sender : TObject );
begin
  Frm := TfrmMain( Owner );
  btnToggle.Caption := ccapAboutToggle;
  lblTopic.Caption := GetNamebtnToggle;
  ShowAbout;
end;


end.

