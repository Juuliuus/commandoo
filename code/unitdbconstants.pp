unit unitDBConstants;
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

{$mode objfpc}{$H+}

interface

//uses
//  Classes, SysUtils;

const

  //===================================
  //  changes to versions need to have code for them in
  //TfrmMain.CheckUpdates_PROG
  //TfrmMain.CheckUpdates_DB
  //see also c_PROG_VersionUpgradeCount
  //===================================

  //increment this number for any database cleanup / change, ONLY needs to be done if there is a change
    //c_DB_VersionUpgradeCount = 2;//oct 26 2016
    //c_DB_VersionUpgradeCount = 3;//Jan 25 2017  Make ThreatLevel intuitive remove 0th useless element
    c_DB_VersionUpgradeCount = 4;//April 28 2017  ObjId removed
  //=====  DATABASE  ==============================
    //c_DB_HandwrittenVersion = '1.0.2'; //lineiniidx goes from db
    //c_DB_HandwrittenVersion = '1.0.3'; //'<<F>>' fiasco, quotes shouldn't have been there.
    //c_DB_HandwrittenVersion = '1.0.4'; //Make ThreatLevel intuitive remove 0th useless elemnet
    c_DB_HandwrittenVersion = '2.0.0'; //integration of sql / text, objid goes away, sortorder added to cmdlines
    c_DB_VersionDate = ' (April 2017)';


type

  TCondition = ( coNone, coRegEx, coEqual, coNotEqual, coMatchQualified, coMatchQualifiedNot,
                 coContains, coContainsNot, coLT, coLTE, coGT, coGTE, coIsEmpty );
  TSearchOperatorType = ( sotNone, sotNOT, sotAND, sotOR, sotXOR, sotOpen, sotClose );
  TSearchItemType = ( sitOperator, sitString, sitBoolean, sitInteger, sitDouble );


  //TThreatLevel = ( tlDefaultInvalid, tlNotSpecified, tlHarmless, tlCareful, tlCaution, tlDanger );
  TThreatLevel = ( tlNotSpecified, tlHarmless, tlCareful, tlCaution, tlDanger );
  //TSuperUserMode = ( sumDefaultInvalid, sumUnknown, sumNotRequired, sumAmbivalent, sumRequired );
  //TCommandType = ( ctDefaultInvalid, ctUnknown, ctNativeLinux, ctInstalled, ctCustom );

  TCmdFilesDefined = ( cfdCmd, cfdCmdLine, cfdENDFORCOUNTING_DoNotUse );
  TCmdObjType = ( cotNone, cotBase, cotCmd, cotCmdLine );

  TsqlIndexType = ( sqlINone, sqlIPK, sqliPKAlias, sqlIFK, sqlIFauxFK, sqlIIndex, sqlIUniqueIndex );

  TControlFieldID = ( cofidVersionCount, cofidProfileGUID, cofidENDFORCOUNTING_DoNotUse );


  TCmdFieldID = ( fidInValid,  //for saved searches when fields are changed or someone hand edits the saved search file
               fidRowID, //shared
               fidCmdFK, //cmd line
               fidSuperUserMode, fidNotes, fidIsFavorite, fidDetachProcess, fidThreatLevel, //shared
               fidCommandName, fidKeywords, fidHelpCommand, fidVersionCommand, fidLocationPath, //Cmd
               fidSO, fidEntry, fidFriendlyName, fidWantsInput, fidUseShell, fidTerminalOnly, fidAlert,//CmdLine
               fidENDFORCOUNTING_DoNotUse );

resourcestring

//Keep in mind that this and other structures that can vary during program run
//needs to be tied in to translation system, since they are outside of .po system in that
//they are translated at start up. If the language is changed they need to be updated.
//With this in mind see how this is done and use the methods as appropriate.
//see procedure TfrmMain.UpdateSharedHintsAndCaptions; as a start

  cCmdColInValidCaption = '<invalid>';
  cCmdColInValidCaptionAbbrev = '<INVAL>';

//columns shared by both Cmd and CmdLine
  cCmdColSuperUserModeCaption = 'As SuperUser';
  cCmdColSuperUserModeCaptionAbbrev = 'Super';
  cCmdColNotesCaption = 'Notes';
  cCmdColNotesCaptionAbbrev = 'Notes';
  cCmdColIsFavoriteCaption = 'Is Favorite';
  cCmdColIsFavoriteCaptionAbbrev = 'Fav';
  cCmdColDetachProcessCaption = 'Child Proc';//'Detach Process';
  cCmdColDetachProcessCaptionAbbrev = 'ChProc';//'Detach Process';
  cCmdColThreatLevelCaption = 'ThreatLevel';
  cCmdColThreatLevelCaptionAbbrev = 'ThLev';

//columns specific to Cmd
  cCmdColCommandNameCaption = 'Command Name';
  cCmdColCommandNameCaptionAbbrev = 'Cmd';
  cCmdColKeywordsCaption = 'Keywords';
  cCmdColKeywordsCaptionAbbrev = 'KeyWd';
  cCmdColHelpCommandCaption = 'Help';
  cCmdColHelpCommandCaptionAbbrev = 'Help';
  cCmdColVersionCommandCaption = 'Version';
  cCmdColVersionCommandCaptionAbbrev = 'Vers';
  cCmdColLocationPathCaption = 'Folder';
  cCmdColLocationPathCaptionAbbrev = 'Fldr';

//columns specific to CmdLine
  cCmdColEntryCaption = 'CmdLine';//'Entry';
  cCmdColEntryCaptionAbbrev = 'CLine';
  cCmdColFriendlyNameCaption = 'Friendly Name';
  cCmdColFriendlyNameCaptionAbbrev = 'Fr_Name';
  cCmdColWantsInputCaption = 'Wants Input';
  cCmdColWantsInputCaptionAbbrev = 'Input';
  cCmdColUseShellCaption = 'Use Shell';
  cCmdColUseShellCaptionAbbrev = 'Shell';
  cCmdColTerminalOnlyCaption = 'Term. Only';
  cCmdColTerminalOnlyCaptionAbbrev = 'Term';
  cCmdColAlertCaption = 'Alert on use';
  cCmdColAlertCaptionAbbrev = 'Alert';

//KeyWords List
  cKeyWordsCaption_Singular = 'Keyword';
  cKeyWordsCaption_Plural = 'Keywords';

//ThreatLevel constants
  cThreatLevelHarmless = 'Harmless';
  cThreatLevelCareful = 'Careful';
  cThreatLevelCaution = 'Caution';
  cThreatLevelDanger = 'Danger';
  cThreatLevelNotSpecified = '<Not_Set!>';


const
  cIniSectionMaster_Misc_List = '-LoadMe_Misc_List_%s-';
  cIniSectionMaster_CmdObj_List = '-LoadMe_List_CmdObj-';

  cIniSectionCmdLineObj = 'CmdLineObj';

  cIniFileNameCommands = 'Cmd';
  cIniFileNameCommandLines = 'CmdLine';
  cIniFileNameMisc = 'Misc';
  cDBTabCommands = 'tblCmd';
  cDBTabCmdLines = 'tblCmdLine';
  cDBTabKeyWords = 'tblKeyWords';
  cDBTabControl = 'tblControl';

  cCmdColInValid = '<invalid>';
//Command Column Names
//columns that both Cmd and CmdLine use/need
  cCmdColRowID = 'rowid';
  cCmdColCmdFK = 'CmdFK';//cmdline only
  cCmdColSuperUserMode = 'SuperUserMode';
  cCmdColNotes = 'Notes';
  cCmdColIsFavorite = 'IsFavorite';
  cCmdColDetachProcess = 'DetachProcess';
  cCmdColThreatLevel = 'ThreatLevel';

//columns specific to Cmd
  cCmdColCommandName = 'CommandName';
  cCmdColKeywords = 'Keywords';
  cCmdColHelpCommand = 'HelpCommand';
  cCmdColVersionCommand = 'VersionCommand';
  cCmdColLocationPath = 'LocationPath';
  //cCmdColLineIniIdx = 'LineIniIdx';
  //cCmdColCommandType = 'CommandType';

//columns specific to CmdLine
  cCmdColEntry = 'Entry';
  cCmdColFriendlyName = 'FriendlyName';
  cCmdColWantsInput = 'WantsInput';
  cCmdColUseShell = 'UseShell';
  cCmdColTerminalOnly = 'TerminalOnly';
  cCmdColAlert = 'TerminalAlert';

//column for KeyWords
//by making the columnnames match then the List field for KeyWords can be matched up with the appropriate master list
  cKeyWordKeyWord = cCmdColKeywords;//'KeyWords';

//column for Control
  cSectTab_DB_VersionCount = '_DB_VersionCount';
  cSectTab_DB_ProfileGUID = '_DB_ProfileGUID';


//if fields are added or deleted then saved searches won't know what field to use
//this allows lookup of the field to get back it's current index
//NEVER RE-USE THESE ID's, if a field is later removed that ID CAN NOT be re-used or wormhole to Black Hole will open
  cCmdColMissingUniqueID = 0;
  cCmdColINTERNALUniqueID = 1;//read but not written
  cCmdColINTERNAL_CmdLine_FK_UniqueID = 2;//read and written, specifies CmdLine Foreign Key to Commands
  cCmdColSuperUserModeUniqueID = 829800545;
  cCmdColNotesUniqueID = 686766142;
  cCmdColIsFavoriteUniqueID = 54495245;
  cCmdColDetachProcessUniqueID = 403489349;
  cCmdColThreatLevelUniqueID = 559326324;
  cCmdColCommandNameUniqueID = 238227321;
  cCmdColKeywordsUniqueID = 379104383;
  cCmdColHelpCommandUniqueID = 172576032;
  cCmdColVersionCommandUniqueID = 841318704;
  cCmdColLocationPathUniqueID = 504812749;
  cCmdColSOUniqueID = 182726924;
  cCmdColEntryUniqueID = 101507315;
  cCmdColFriendlyNameUniqueID = 567383862;
  cCmdColWantsInputUniqueID = 462247456;
  cCmdColUseShellUniqueID = 258295869;
  cCmdColTerminalOnlyUniqueID = 35133422;
  cCmdColAlertUniqueID = 247906182;
//spares
// 201283457 226229809 722842100;

  csqlExistenceCheckerFieldName = Ord( fidCommandName );
  csqlExistenceCheckerTableName = cDBTabCommands;



implementation

end.

