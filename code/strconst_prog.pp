unit strconst_prog;

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
  Classes, SysUtils;


const
  cprogEscapeReplacement = '!';
  cSqliteLocateCommand = 'locate -i -e libsqlite3.so';
  cConfigPathFileName = 'config_path__DoNotDelete';
  cConfigPathSection = 'Working_Config_Path';
  cConfigPathWritingPath = 'WritingPath';
  cRootMode = '<< ROOT MODE >> ';
  ccapProgram = 'COMMANDOO : GUI for Linux Commands / CLI';

  cAttnBar = LineEnding + '==========' + LineEnding;

  cSaveToFileTemplate = 'commandoo_%s.txt';

  cSectTabNoShows = 'NoShows';

  cRootFileSudo = 'sudo %s';
  cRootFileSu = 'su -c "%s"';
  cRootFileSuSession = 'su --session-command "%s"';

  cSectTabFormSettings = 'FormSettings';
  cFormSettingsProgramLangCol = 'ProgramLang';
  cFormSettingsMaxInOutPutCol = 'MaxInOutPut';
  cFormSettingsOutPutDisplayMax = 'DisplayMax';
  cFormSettingsMaxOutputWait = 'MaxOutputWait';
  //cFormSettingsSystemColorsIdx = 'SystemColorsIdx';
  cFormSettingsLastQuickRun = 'LastQuickRun';
  cFormSettingsManRefreshFav = 'ManRefreshFav';
  //cFormSettingsLastNoteBookTab = 'LastNoteBookTab';
  cFormSettingsWarnUnspecified = 'WarnUnspecified';
  cFormSettingsWarnHarmless = 'WarnHarmless';
  cFormSettingsWarnCareful = 'WarnCareful';
  cFormSettingsWarnCaution = 'WarnCaution';
  cFormSettingsWarnDanger = 'WarnDanger';
  cFormSettingsAllowMultipleOpens = 'AllowMultipleOpens';
  cFormSettingsAllowPkexec = 'AllowPkexec';
  cFormSettingsLargerFont = 'LargerFont';
  cFormSettingsAllowSqlDB = 'AllowSqlDB';
  cFormSettingsDoShowSqlMissing = 'DoShowSqlMissing';
  cFormSettingsAllowTextDB = 'AllowTextDB';
  cFormSettingsRootFile = 'RootFile';
  cFormSettingsSavingPath = 'SavingPath';

  cSectTabFormSettingsExtension = '.settings';

  cSectTabSupportedLanguages = 'SupportedLanguages';

  cSectTabCurrProfile = 'CurrentProfile';
  cCurrProfileName = 'CPName';
  cCurrProfileIsDB = 'CPIsDB';
  cCurrProfilePath = 'CPPath';

  cSectTabCurrSqliteLibrary = 'SQLite';
  cCurrSqliteLibraryPath = 'SqlLibPath';


  cDefaultDBProfileName = 'DB';
  cDefaultDBProfileIsDB = false;
  cDefaultDBProfileIsDBStrNot = '   < file >';
  cDefaultDBProfileIsDBStr = '   < sql >';

  cSectTabDBProfiles = 'DBProfiles';
  cLanguageFolderName = 'languages';
  cSearchFolderName = 'searches';
  cReferenceProgramName = 'commandoo';

  cconstCommandLabel = 'Command';
  cconstCommandLineLabel = 'Command Line';
  cWebSiteBase = 'https://timepirate.org/';
  cWebSiteCommando = 'downloads.html';
  cWebSiteDownloads = 'downloads/%s';

//beginning of a blacklist for programs that do not take --help or -V, etc commands.
//This is bad because when adding a CL entry the help is supposed to be shown but the program runs
//instead as a child process, locking commandoo until the child is halted.
//I ran accress speedcrunch, there may be more! Who knows. For now this simple thing will do
//but if the list that I'm aware of grows then will need a structure of some sort.
  cHelpVer_BLACKLIST = '::speedcrunch::';

//=================================== IMPORTANT
//  changes to versions need to have code for them in
//TfrmMain.CheckUpdates_PROG
//TfrmMain.CheckUpdates_DB
//see also c_DB_VersionUpgradeCount
//===================================
//increment this number for any Program settings cleanup / change, ONLY needs to be done if there is a change
//that needs to be programatically handled.
  //c_PROG_VersionUpgradeCount = 3;
  //c_PROG_VersionUpgradeCount = 4; //29.06.2020 Allow Escape chars taken out
  c_PROG_VersionUpgradeCount = 5; //05.08.2020 languages will be updated
//=====  PROGRAM   ==============================

//==> NOTE: only change MAJOR VERSION when it is INCOMPATIBLE (ie. DB schema change) with previous MAJOR VERSION

  cHandwrittenVersion = '2.0.1';//'1.0.1';//all done. ?
  cSectTab_PROG_VersionCount = '_PROG_VersionCount';
  cStandardProgramSequence = 'PROGRAM SEQUENCE #:    %d';
  cVersionDate = ' (August 2020)';//ReleaseCandidate';
  cUpgradeDir = 'upgrades';
{$IFDEF platAppImage}
  cUpgradeFileName = 'commandoo-x86_64';
  cUpgradeExtUpgradeVersion = '.ver.txt'; //No number
  cUpgradeExtUpgradeInfo = '.readme.txt';
  cUpgradeExtAppImage = '.AppImage';
  cUpgradeExtGPGSignature = '.sig';
  cUpgradeExtZsync = '.zsync';
  //commandoo-x86_64.4.(appimage).zsync [FUTURE have them get appimageupdate]
  function Upgrade_GetUpgradeVersionFileName : string;
  function Upgrade_GetUpgradeInfoFileName( const Sequence : string ) : string;
  function Upgrade_GetAppImageFileName( const Sequence : string ) : string;
  function Upgrade_GetGPGSignatureFileName( const Sequence : string ) : string;
  function Upgrade_GetZsyncFileName( const Sequence : string ) : string;
{$ENDIF}



implementation

{$IFDEF platAppImage}
  function Upgrade_GetFileName : string;
  begin
    result := cUpgradeFileName;
  end;

  function Upgrade_GetUpgradeVersionFileName : string;
  begin
    result := Upgrade_GetFileName + cUpgradeExtAppImage + cUpgradeExtUpgradeVersion;
  end;

  function Upgrade_GetUpgradeInfoFileName( const Sequence : string ) : string;
  begin
    result := Upgrade_GetAppImageFileName( Sequence ) + cUpgradeExtUpgradeInfo;
  end;

  function Upgrade_GetAppImageFileName( const Sequence : string ) : string;
  begin
    result := Upgrade_GetFileName + '_' + Sequence + cUpgradeExtAppImage;
  end;

  function Upgrade_GetGPGSignatureFileName( const Sequence : string ) : string;
  begin
    result := Upgrade_GetAppImageFileName( Sequence ) + cUpgradeExtGPGSignature;
    //result := Upgrade_GetFileName + Sequence + cUpgradeExtAppImage + cUpgradeExtGPGSignature;
  end;

  function Upgrade_GetZsyncFileName( const Sequence : string ) : string;
  begin
    result := Upgrade_GetAppImageFileName( Sequence ) + cUpgradeExtZsync;
    //result := Upgrade_GetFileName + Sequence + cUpgradeExtAppImage + cUpgradeExtZsync;
  end;

{$ENDIF}

end.


