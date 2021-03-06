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
    memToggle : TMemo;
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
    function GetNamebtnToggle( Idx : integer ) : string;
    procedure ShowTogglePath;
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

  ccapAboutToggle = '&A  Toggle Display...';
  //ccapAboutToggleThru = 'Toggles Thru: ';


const
  constSeparator = '-----------------------------------';
{$IFDEF platAppImage}
  cDokumentCount = 8;//7;
{$ELSE}
  cDokumentCount = 7;//6;
{$ENDIF}

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

procedure TfrmAbout.ShowTogglePath;
var
  i : integer;
  Str : string;
begin
  str := '';
  for i := 0 to cDokumentCount - 2 do
    Str := Str + GetNamebtnToggle( i ) + '  ==>  ';
  Str := Str + GetNamebtnToggle( cDokumentCount - 1 );
  memToggle.Lines.Text := Str;
end;

function TfrmAbout.GetNamebtnToggle( Idx : integer ) : string;
begin
  result := 'info';
  case Idx of
    0 : result := cSaveToFileAbout;
    1 : result := cSaveToFileWhatsNew;
    2 : result := cSaveToFileIntro;
    3 : result := cSaveToFileTips;
    4 : result := cSaveToFileFreshStart;
    5 : result := cSaveToFileUpgrade;
    6 : result := cSaveToFileIbus;
{$IFDEF platAppImage}
    7 : result := cSaveToFileUSBDrive;
{$ENDIF}
  end;
end;

procedure TfrmAbout.btnToggleClick( Sender : TObject );
var
  Idx : integer;
begin
  btnToggle.Tag := btnToggle.Tag + 1;
  Idx := btnToggle.Tag mod cDokumentCount;
  case Idx of
    0 : ShowAbout;
    1 : memOutput.Text := cmsgWhatsNew;
    2 : ShowIntro;
    3 : memOutput.Text := format( cmsgTips, [ cmsgFormHotKeys ] );
    4 : memOutput.Text := cmsgFirstLocalRunAbout + format( cmsgFirstLocalRun, [ cmsgFormHotKeys ] );
    5 : memOutput.Text := cmsgCommandooUpgrade;
    6 : memOutput.Text := cmsgIbus;
{$IFDEF platAppImage}
    7 : memOutput.Text := format( cmsgCommandooThumbDrives, [ Frm.AppImagePath ] );
{$ENDIF}
  end;
  lblTopic.Caption := GetNamebtnToggle( Idx );
  btnSaveToFile.enabled := true;
end;

procedure TfrmAbout.FormCreate( Sender : TObject );
begin
  font.size := cDefaultFontSize;
  FormAutoAdjustLayout( self );
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

  memOutput.Lines.Add( 'GUID: ' + Frm.ProfileGUID );
  memOutput.Lines.Add( constSeparator );
  memOutput.Lines.Add( GetDBNameList( Frm.ProfileName, Frm.ProfilePath, not Frm.UseDB ) );
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

  memOutput.Lines.Add( format( cAboutLine, [ cHandwrittenVersion
                                            + '.'
                                            + inttostr( c_PROG_VersionUpgradeCount )
                                            + cVersionDate
                                            ]
                      )
                      + Frm.GetWidgetString );
  memOutput.Lines.Add( format( cStandardProgramSequence, [ c_PROG_VersionUpgradeCount ] ) );
  memOutput.Lines.Add( '' );
  memOutput.Lines.Add( format( cAboutBDLine, [ c_DB_HandwrittenVersion + '.' + inttostr( c_DB_VersionUpgradeCount ) + c_DB_VersionDate ] ) );
  memOutput.Lines.Add( format( cStandardDBSequence, [ c_DB_VersionUpgradeCount ] ) );
  memOutput.Lines.Add( '' );
  memOutput.Lines.Add( 'System reported path: ' + Application.ExeName );
  memOutput.Lines.Add( '' );
  GetCurrentDBInfo;
  memOutput.Lines.Add( 'commandoo WebSite:     ' + cWebSiteBase + cWebSiteCommando );
  memOutput.Lines.Add( '' );
{$IFDEF platAppImage}
  memOutput.Lines.Add( 'Using commandoo AppImage Version (https://appimage.org/) installed in:' );
  memOutput.Lines.Add(  Frm.AppImagePath );
  memOutput.Lines.Add( '' );
  memOutput.Lines.Add( format( cAboutInstalled, [ Frm.AppImageRunningPath ] ) );
{$ELSE}
  memOutput.Lines.Add( format( cAboutInstalled, [ extractfilePath( Application.Exename ) ] ) );
{$ENDIF}

  memOutput.Lines.Add( '' );
  memOutput.Lines.Add( format( cAboutFormSettings,
                               [ Frm.WritingToPath + cReferenceProgramName + cSectTabFormSettingsExtension ]
                              )
                     );
  memOutput.Lines.Add( format( cAboutLanguage, [ Frm.GetPODirectory ] ) );
  memOutput.Lines.Add( format( cAboutSearches, [ Frm.GetSearchesDirectory ] ) );
  memOutput.Lines.Add( '' );
  memOutput.Lines.Add( '' );

  GetDefaultDBInfo;
  GetCustomDBInfo;

  memOutput.Lines.Add( cEmail );
  memOutput.Lines.Add( 'commandoo WebSite:     ' + cWebSiteBase + cWebSiteCommando );

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
  lblTopic.Caption := cSaveToFileAbout;//GetNamebtnToggle;
  ShowTogglePath;
  ShowAbout;
end;


end.

