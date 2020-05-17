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

  cAttnBar = LineEnding + '==========' + LineEnding;

  cSectTabNoShows = 'NoShows';

  cSectTabSuperUserFile = 'SUFile';
  cSuperUserFile_File = 'SuperUserFile';
  cSuperUserFile_Param1 = 'Param1';
  cSuperUserFile_Param2 = 'Param2';


  cSectTabFormSettings = 'FormSettings';
  cFormSettingsProgramLangCol = 'ProgramLang';
  cFormSettingsMaxInOutPutCol = 'MaxInOutPut';
  cFormSettingsOutPutDisplayMax = 'DisplayMax';
  cFormSettingsFontOffset = 'FontOffset';
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
  cFormSettingsAllowESCOutput = 'AllowESC';
  cFormSettingsAllowSqlDB = 'AllowSqlDB';
  cFormSettingsAllowTextDB = 'AllowTextDB';

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

//not used, because not secure. Maybe later but user must know pword is passed as cleartext
//cSudoPath = '/usr/bin/sudo';


//=================================== IMPORTANT
//  changes to versions need to have code for them in
//TfrmMain.CheckUpdates_PROG
//TfrmMain.CheckUpdates_DB
//see also c_DB_VersionUpgradeCount
//===================================


//increment this number for any Program settings cleanup / change, ONLY needs to be done if there is a change
  c_PROG_VersionUpgradeCount = 3;//2;//
//=====  PROGRAM   ==============================
  cHandwrittenVersion = '2.0.0';//'1.0.1';//all done. ?
  cSectTab_PROG_VersionCount = '_PROG_VersionCount';
  cVersionDate = ' (June 2020)';// ReleaseCandidate';

implementation

end.


