unit unitDBStructure;

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
{$MODESWITCH ADVANCEDRECORDS}   //turn on to have record functions and properties

interface

uses
  Classes, SysUtils
  , unitFields
  , unitDBConstants
  , controls
  ;


type
//Controls all database super-structure
  TDataLocation = ( dlCmd, dlCmdLine, dlKeyWord, dlControl, dlForCountingDNU );

const
//Controls assignment of the two database types
  aryTextData: array[ 0..2 ] of TDataLocation = ( dlCmd, dlCmdLine, dlKeyWord );
  arySqlData: array[ 0..3 ] of TDataLocation = ( dlCmd, dlCmdLine, dlKeyWord, dlControl );

type

  TDBSql_ControlStruct = record
      TableName : string;
      FieldType : TufFieldType;
      ControlFieldID : TControlFieldID;
      ColumnName : string;
      IndexType : TsqlIndexType;
      Null : boolean;
  end;

  TDBListKeyWordStructure = record
    private
      SectTabName : string;
    public
      DataLocation : TDataLocation;
      LinkedToDataLocation : TDataLocation;
      LinkedToFieldID : TCmdFieldID;
      FieldType : TufFieldType;
      DisplayCaptionSingular : string;
      DisplayCaptionPlural : string;
      ColumnName : string;
      IndexType : TsqlIndexType;
      Null : boolean;
      function GetSectTabName : string;
      procedure UpdateDisplayCaption;
  end;


  TDBCmdStructure = record
    FieldType : TufFieldType;
    FieldID : TCmdFieldID;
    CmdObjType : TCmdObjType;
    ColumnName : string;
    DisplayCaption : string;
    DisplayAbbrev : string;
    Searchable : boolean;
    UniqueID : integer;
    IndexType : TsqlIndexType;
    Null : boolean;
    CaptionList : TList;
    //procedure RegisterCaption( Controls : array of TObject );
    //StrDef : string;
    (* VARIABLE PART *)
    //case FieldType: TufFieldType of
    //  ftUnk : ( Unknown : boolean );
    //  ftString_Key, ftString, ftList : ( DefaultStr : char );
    //  ftBoolean : ( DefaultBool : boolean );
    //  ftInteger : ( DefaultInt : integer );
    //  ftEnum : ( DefaultEnum : integer );
  end;


const

//=Define= Search Columnnames used to determine search results display name
//In the case of cmd's just the commandname is displayed, but cmdline's can either be CL or FriendlyName
  arySearchResultDisplayCol_Cmd : array[ 0..1 ] of string = ( cCmdColCommandName, '' );
  arySearchResultDisplayCol_CmdLine : array[ 0..1 ] of string = ( cCmdColEntry, cCmdColFriendlyName );



//These "constant" records are used to provide information to various parts of this system: Display labels for
//languages, sql db creation, searching routines. It defines the structure of the cmd and cmdline objects.
//==========================
//it is indexed by TCmdFieldID enum. Therefore, the definition of each record ==MUST BE== in the same order here
//as it is in TCmdFieldID declaration.
//==========================
// And yes, this system would NOT work if we needed to distinguish between Cmd.Notes and CmdLine.Notes. It would need
//a re-design. But in this case I don't differentiate, for example in searches, between the two, just that it is a
//"Note" type field.

  CmdStruct : array[ 0..Ord( fidENDFORCOUNTING_DoNotUse ) - 1 ] of TDBCmdStructure = (
      //fidInValid
      (
        FieldType : ftString;
        FieldID : fidInValid;
        CmdObjType : cotNone;//cotBase;
        ColumnName : cCmdColInValid;
        DisplayCaption : cCmdColInValidCaption;
        DisplayAbbrev : cCmdColInValidCaptionAbbrev;
        Searchable : false;
        UniqueID : cCmdColMissingUniqueID;
        IndexType : sqlINone;
        Null : true;
        CaptionList : nil;
      ),
      //fidRowID
      (
        FieldType : ftInteger;
        FieldID : fidRowID;
        CmdObjType : cotBase;
        ColumnName : cCmdColRowID;
        DisplayCaption : cCmdColRowID;//literal, internal use only
        DisplayAbbrev : cCmdColRowID;
        Searchable : false;
        UniqueID : cCmdColINTERNALUniqueID;
        IndexType : sqlINone;
        Null : false;
        CaptionList : nil;
      ),
      //fidCmdFK
      (
        FieldType : ftInteger;
        FieldID : fidCmdFK;
        CmdObjType : cotCmdLine;
        ColumnName : cCmdColCmdFK;
        DisplayCaption : 'CmdFK';//literal, internal use only
        DisplayAbbrev : 'CmdFK';
        Searchable : false;
        UniqueID : cCmdColINTERNAL_CmdLine_FK_UniqueID;
        IndexType : sqlIIndex;
        Null : false;
        CaptionList : nil;
      ),
      //fidSuperUserMode
      (
        FieldType : ftBoolean;
        FieldID : fidSuperUserMode;
        CmdObjType : cotBase;
        ColumnName : cCmdColSuperUserMode;
        DisplayCaption : cCmdColSuperUserModeCaption;
        DisplayAbbrev : cCmdColSuperUserModeCaptionAbbrev;
        Searchable : true;
        UniqueID : cCmdColSuperUserModeUniqueID;
        IndexType : sqlINone;
        Null : true;
        CaptionList : nil;
      ),
      //fidNotes
      (
        //FieldType : ftString;
        FieldType : ftText;
        FieldID : fidNotes;
        CmdObjType : cotBase;
        ColumnName : cCmdColNotes;
        DisplayCaption : cCmdColNotesCaption;
        DisplayAbbrev : cCmdColNotesCaptionAbbrev;
        Searchable : true;
        UniqueID : cCmdColNotesUniqueID;
        IndexType : sqlINone;
        Null : true;
        CaptionList : nil;
      ),
      //fidIsFavorite
      (
        FieldType : ftBoolean;
        FieldID : fidIsFavorite;
        CmdObjType : cotBase;
        ColumnName : cCmdColIsFavorite;
        DisplayCaption : cCmdColIsFavoriteCaption;
        DisplayAbbrev : cCmdColIsFavoriteCaptionAbbrev;
        Searchable : true;
        UniqueID : cCmdColIsFavoriteUniqueID;
        IndexType : sqlINone;
        Null : true;
        CaptionList : nil;
      ),
      //fidDetachProcess
      (
        FieldType : ftBoolean;
        FieldID : fidDetachProcess;
        CmdObjType : cotBase;
        ColumnName : cCmdColDetachProcess;
        DisplayCaption : cCmdColDetachProcessCaption;
        DisplayAbbrev : cCmdColDetachProcessCaptionAbbrev;
        Searchable : true;
        UniqueID : cCmdColDetachProcessUniqueID;
        IndexType : sqlINone;
        Null : true;
        CaptionList : nil;
      ),
      //fidThreatLevel
// for each ftEnum a return list must be written in ReturnEnumerationConstants
      (
        FieldType : ftEnum;
        FieldID : fidThreatLevel;
        CmdObjType : cotBase;
        ColumnName : cCmdColThreatLevel;
        DisplayCaption : cCmdColThreatLevelCaption;
        DisplayAbbrev : cCmdColThreatLevelCaptionAbbrev;
        Searchable : true;
        UniqueID : cCmdColThreatLevelUniqueID;
        IndexType : sqlINone;
        Null : true;
        CaptionList : nil;
      ),
      //fidCommandName
      (
        FieldType : ftString_Key;//IMPORTANT!!!! And used only one time
        FieldID : fidCommandName;
        CmdObjType : cotCmd;
        ColumnName : cCmdColCommandName;
        DisplayCaption : cCmdColCommandNameCaption;
        DisplayAbbrev : cCmdColCommandNameCaptionAbbrev;
        Searchable : true;
        UniqueID : cCmdColCommandNameUniqueID;
        IndexType : sqlIUniqueIndex;
        Null : false;
        CaptionList : nil;
      ),
      //fidKeyWords
      (
        FieldType : ftList;
        FieldID : fidKeyWords;
        CmdObjType : cotCmd;
        ColumnName : cCmdColKeyWords;
        DisplayCaption : cCmdColKeyWordsCaption;
        DisplayAbbrev : cCmdColKeyWordsCaptionAbbrev;
        Searchable : true;
        UniqueID : cCmdColKeyWordsUniqueID;
        IndexType : sqlINone;
        Null : true;
        CaptionList : nil;
      ),
      //fidHelpCommand
      (
        FieldType : ftString;
        FieldID : fidHelpCommand;
        CmdObjType : cotCmd;
        ColumnName : cCmdColHelpCommand;
        DisplayCaption : cCmdColHelpCommandCaption;
        DisplayAbbrev : cCmdColHelpCommandCaptionAbbrev;
        Searchable : false;
        UniqueID : cCmdColHelpCommandUniqueID;
        IndexType : sqlINone;
        Null : true;
        CaptionList : nil;
      ),
      //fidVersionCommand
      (
        FieldType : ftString;
        FieldID : fidVersionCommand;
        CmdObjType : cotCmd;
        ColumnName : cCmdColVersionCommand;
        DisplayCaption : cCmdColVersionCommandCaption;
        DisplayAbbrev : cCmdColVersionCommandCaptionAbbrev;
        Searchable : false;
        UniqueID : cCmdColVersionCommandUniqueID;
        IndexType : sqlINone;
        Null : true;
        CaptionList : nil;
      ),
      //fidLocationPath
      (
        FieldType : ftString;
        FieldID : fidLocationPath;
        CmdObjType : cotCmd;
        ColumnName : cCmdColLocationPath;
        DisplayCaption : cCmdColLocationPathCaption;
        DisplayAbbrev : cCmdColLocationPathCaptionAbbrev;
        Searchable : true;
        UniqueID : cCmdColLocationPathUniqueID;
        IndexType : sqlIIndex;
        Null : true;
        CaptionList : nil;
      ),
      //fidSO
      (
        FieldType : ftInteger;
        FieldID : fidSO;
        CmdObjType : cotCmdLine;
        ColumnName : 'SO';
        DisplayCaption : 'SortOrder';
        DisplayAbbrev : 'SO';
        Searchable : false;
        UniqueID : cCmdColSOUniqueID;
        IndexType : sqlINone;
        Null : false;
        CaptionList : nil;
      ),
      //fidEntry
      (
        FieldType : ftString;
        FieldID : fidEntry;
        CmdObjType : cotCmdLine;
        ColumnName : cCmdColEntry;
        DisplayCaption : cCmdColEntryCaption;
        DisplayAbbrev : cCmdColEntryCaptionAbbrev;
        Searchable : true;
        UniqueID : cCmdColEntryUniqueID;
        IndexType : sqlINone;
        Null : false;
        CaptionList : nil;
      ),
      //fidFriendlyName
      (
        FieldType : ftString;
        FieldID : fidFriendlyName;
        CmdObjType : cotCmdLine;
        ColumnName : cCmdColFriendlyName;
        DisplayCaption : cCmdColFriendlyNameCaption;
        DisplayAbbrev : cCmdColFriendlyNameCaptionAbbrev;
        Searchable : true;
        UniqueID : cCmdColFriendlyNameUniqueID;
        IndexType : sqlINone;
        Null : true;
        CaptionList : nil;
      ),
      //fidWantsInput
      (
        FieldType : ftBoolean;
        FieldID : fidWantsInput;
        CmdObjType : cotCmdLine;
        ColumnName : cCmdColWantsInput;
        DisplayCaption : cCmdColWantsInputCaption;
        DisplayAbbrev : cCmdColWantsInputCaptionAbbrev;
        Searchable : true;
        UniqueID : cCmdColWantsInputUniqueID;
        IndexType : sqlINone;
        Null : true;
        CaptionList : nil;
      ),
      //fidUseShell
      (
        FieldType : ftBoolean;
        FieldID : fidUseShell;
        CmdObjType : cotCmdLine;
        ColumnName : cCmdColUseShell;
        DisplayCaption : cCmdColUseShellCaption;
        DisplayAbbrev : cCmdColUseShellCaptionAbbrev;
        Searchable : true;
        UniqueID : cCmdColUseShellUniqueID;
        IndexType : sqlINone;
        Null : true;
        CaptionList : nil;
       ),
       //fidTerminalOnly
       (
         FieldType : ftBoolean;
         FieldID : fidTerminalOnly;
         CmdObjType : cotBase;//cotCmdLine;
         ColumnName : cCmdColTerminalOnly;
         DisplayCaption : cCmdColTerminalOnlyCaption;
         DisplayAbbrev : cCmdColTerminalOnlyCaptionAbbrev;
         Searchable : true;
         UniqueID : cCmdColTerminalOnlyUniqueID;
         IndexType : sqlINone;
         Null : true;
         CaptionList : nil;
       ),
       //fidAlert
       (
         FieldType : ftBoolean;
         FieldID : fidAlert;
         CmdObjType : cotCmdLine;
         ColumnName : cCmdColAlert;
         DisplayCaption : cCmdColAlertCaption;
         DisplayAbbrev : cCmdColAlertCaptionAbbrev;
         Searchable : true;
         UniqueID : cCmdColAlertUniqueID;
         IndexType : sqlINone;
         Null : true;
         CaptionList : nil;
        )
   );

  KeyWordStruct : TDBListKeyWordStructure = (
    SectTabName : cIniSectionMaster_Misc_List;
    DataLocation : dlKeyword;
    LinkedToDataLocation : dlCmd;
    LinkedToFieldID : fidKeyWords;
    FieldType : ftString;
    DisplayCaptionSingular : cKeyWordsCaption_Singular;
    DisplayCaptionPlural : cKeyWordsCaption_Plural;
    ColumnName : cKeyWordKeyWord;
    IndexType : sqlINone;// sqlIUniqueIndex;// CASE PROBLEM??? Yes, enforce unique caseinsensitive in code.
    Null : true;
  );

  ControlStruct : array[ 0..Ord( cofidENDFORCOUNTING_DoNotUse ) - 1 ] of TDBSql_ControlStruct = (
    //cofidVersionCount
    (
      TableName : cDBTabControl;
      FieldType : ftInteger;
      ControlFieldID : cofidVersionCount;
      ColumnName : cSectTab_DB_VersionCount;//'_DB_VersionCount';
      IndexType : sqlINone;
      Null : true;
     ),
     //cofidProfileGUID
     (
       TableName : cDBTabControl;
       FieldType : ftString;
       ControlFieldID : cofidProfileGUID;
       ColumnName : cSectTab_DB_ProfileGUID;//''_DB_ProfileGUID'';
       IndexType : sqlINone;
       Null : true;
      )
    );


var

//variables (like fidKeyWords, ftList) that are designed to work with Lists ( like KeyWordStruct) need to
//have a Link available to read so that when designing Searches the appropriate list choices are shown.
//that's what this is for, look in code to see how it is used.
//if in future more lists are used then add them here, all list types are just added at the same time
//and then looked up by search form
  ListFields_Linker : TStringList;

  procedure GetSearchResultDisplayCol( const DL : TDataLocation; var Main, Alt : string );


  function Get_CommandStruct_ControlColumn( const UseDB : boolean ) : string;
  function Get_CmdLineStruct_FK_Column : string;
  function GetTableNames( const DL : TDataLocation; const DoUseDB : boolean ) : string;


  function Get_ThreatLevel_Text( const TL : TThreatLevel ) : string; overload;
  function Get_ThreatLevel_Text( const Idx : integer ) : string; overload;
  function Return_ThreatLevel_Items( Strings : TStrings; Complete : boolean = false ) : integer;
  function ReturnEnumerationConstants( const UID : integer; Strings : TStrings ) : boolean;

  procedure Clear_ListFields_Linker;
  function ListEmpty_ListFields_Linker( const ToMatch : string ) : boolean;
  function ListFields_Linker__ValidItem( const IdxStr, ItemStr : string ) : boolean;
  function ListFields_Linker__GetFirst_List_Item( const IdxStr : string ) : string;
  function ListFields_Linker__AttachList( const IdxStr : string; Strings : TStrings  ) : boolean;
  function ListFields_Linker__ListIndex( const IdxStr : string ) : integer;

//functions to get predefined constants
  function Get_Cmd_Field_FieldType( const ID : integer ) : TufFieldType; overload;
  function Get_Cmd_Field_FieldType( const ID : TCmdFieldID ) : TufFieldType; overload;

  function Get_Cmd_Field_UniqueID( const ID : integer ) : integer; overload;
  function Get_Cmd_Field_UniqueID( const ID : TCmdFieldID ) : integer; overload;


  function Get_Cmd_Field_DisplayCaption( const ID : TCmdFieldID ) : string; overload;
  function Get_Cmd_Field_DisplayCaption( const ID : integer ) : string; overload;

  function Get_Cmd_Field_ColumnName( const ID : integer ) : string; overload;
  function Get_Cmd_Field_ColumnName( const ID : TCmdFieldID ) : string; overload;

  function Get_Cmd_Field_IdxFromUniqueID( const UID : integer ) : integer; overload;
  function Get_Cmd_Field_IdxFromUniqueID( const UID : integer ) : TCmdFieldID; overload;

  procedure Register_Cmd_DisplayCaption( const ID : TCmdFieldID; Controls : array of TControl );
  procedure Update_Cmd_DisplayCaptions;

  function Bool_To_Str( const Cond: boolean ): string;
  function Str_To_Bool( const Cond: string ): boolean;

  function GetControlTableSql( const TableName : string; SL : TStrings ) : string;
  function GetKWordTableSql( const TableName : string; SL : TStrings ) : string;
  function GetCmd_CmdLineTableSql( const TableName : string; SL : TStrings; const Exclude : TCmdObjType ) : string;
  function Get_IniFile_List_Section( const DL : TDataLocation ) : string; overload;
  function Get_IniFile_List_Section( const DL : Integer ) : string; overload;

const
  dbsTrueStr = 't';
  dbsFalseStr = 'f';

resourcestring
  cdbsDisplay_True = 'true';
  cdbsDisplay_False = 'false';

implementation

uses
     juusgen
     , unitGlob
     ;


procedure GetSearchResultDisplayCol( const DL : TDataLocation; var Main, Alt : string );
begin
  case DL of
    dlCmd :
     begin
       Main := arySearchResultDisplayCol_Cmd[ 0 ];
       Alt := arySearchResultDisplayCol_Cmd[ 1 ];
     end;
   dlCmdLine :
     begin
       Main := arySearchResultDisplayCol_CmdLine[ 0 ];
       Alt := arySearchResultDisplayCol_CmdLine[ 1 ];
     end;
  end;
end;


function GetCreateTableSql( IT : TsqlIndexType; FT : TufFieldType; const TableName, ColumnName : string; IsNull : boolean; SL : TStrings ) : string;
begin

  result := '';

  case IT of
    sqlIPK : ; //"id" INT NOT NULL PRIMARY KEY AUTOINCREMENT UNIQUE,
    sqliPKAlias : ;//"id" INTEGER PRIMARY KEY ASC NOT NULL,'
                   //ie, overrides builtin rowid and becomes alias
    sqlIFK : ; //'FOREIGN KEY(Parent) REFERENCES Command(Rowid)'
    sqlIFauxFK : ; //"id" INTEGER PRIMARY KEY ASC NOT NULL,'
                  //not sure if needed, would be unique indexed managed by program, but can just do that normally
    else
      result := result
                + format( ' "%s" %s%s', [ ColumnName,
                                          FieldTypeToStr_SQL( FT ),
                                          strif( not IsNull, ' NOT NULL' )
                                        ]
                       )
                + ',';
  end;

//SL will carry any additional sql needed
  case IT of
    sqlIIndex : SL.Add( format( 'CREATE INDEX ix_%s_%s ON %s( %s )', [ TableName, ColumnName, TableName, ColumnName ] ) );
    sqlIUniqueIndex : SL.Add( format( 'CREATE UNIQUE INDEX uix_%s_%s ON %s( %s )', [ TableName, ColumnName, TableName, ColumnName ] ) );
  end;

end;

function InitialCreateTableSql( const TableName : string; SL : TStrings ) : string;
begin
  SL.Clear;
  result := format( 'CREATE TABLE "%s" (', [ TableName ] );
end;

function GetControlTableSql( const TableName : string; SL : TStrings ) : string;
var
  i , l: Integer;
  CS : TDBSql_ControlStruct;
begin

  result := InitialCreateTableSql( TableName, SL );
  l := high( ControlStruct );
  for i := 0 to l do
  begin
    CS := TDBSql_ControlStruct( ControlStruct[ i ] );

    result := result + GetCreateTableSql( CS.IndexType, CS.FieldType, TableName, CS.ColumnName, CS.Null, SL );

  end;

  result := Copy( result, 1, Length( Result ) -1 ) + ')';

end;

function GetKWordTableSql( const TableName : string; SL : TStrings ) : string;
begin
  result := InitialCreateTableSql( TableName, SL );

  result := result
           + GetCreateTableSql( KeyWordStruct.IndexType, KeyWordStruct.FieldType, TableName, KeyWordStruct.ColumnName, KeyWordStruct.Null, SL );

  result := Copy( result, 1, Length( Result ) -1 ) + ')';

end;

function GetCmd_CmdLineTableSql( const TableName : string; SL : TStrings; const Exclude : TCmdObjType ) : string;
var
  i , l: Integer;
  CS : TDBCmdStructure;
begin
  result := InitialCreateTableSql( TableName, SL );
  l := high( CmdStruct );
  for i := 0 to l do
  begin
    CS := TDBCmdStructure( CmdStruct[ i ] );
    if ( CS.CmdObjType = cotNone )
        or ( CS.CmdObjType = Exclude )
        or ( CS.UniqueID = cCmdColINTERNALUniqueID ) //Internal means no DB field
        then
      continue;

    result := result + GetCreateTableSql( CS.IndexType, CS.FieldType, TableName, CS.ColumnName, CS.Null, SL );

  end;

  result := Copy( result, 1, Length( Result ) -1 ) + ')';

end;

function Get_IniFile_List_Section( const DL : Integer) : string;
begin
  result := Get_IniFile_List_Section( TDataLocation( DL ) );
end;

function Get_IniFile_List_Section( const DL : TDataLocation ) : string;
begin
  case DL of
//    dlControl : ; No lists
//    dlCmdLine : ; No Lists
    dlKeyWord : result := format( cIniSectionMaster_Misc_List, [ KeyWordStruct.ColumnName ] );
    dlCmd : result := cIniSectionMaster_CmdObj_List;
    else result := '';
  end;
end;

function Get_CommandStruct_ControlColumn( const UseDB : boolean ) : string;
var
  i , Cnt: integer;
begin
  result := 'invalid';
  Cnt := 0;
  for i := 0 to High( CmdStruct ) do
  begin
    if CmdStruct[ i ].FieldType = ftString_Key then
    begin
      if Cnt > 0 then
        continue;
      inc( Cnt );
      case UseDB of
        true : result := CmdStruct[ i ].ColumnName;
        else result := cIniSectionMaster_CmdObj_List;
      end;
    end;
  end;
  if Cnt = 0 then
    raise EErrorDevelopment.create( 'GetCommandStructControlColumn: No ftString_Key field' );
  if Cnt >= 2 then
    InternalMessage( 'GetCommandStructControlColumn: Hey Programmer! too many ftString_Key fields' );
end;

function Get_CmdLineStruct_FK_Column : string;
var
  i , Cnt: integer;
begin
  result := 'invalid';
  Cnt := 0;
  for i := 0 to High( CmdStruct ) do
  begin
    if CmdStruct[ i ].UniqueID = cCmdColINTERNAL_CmdLine_FK_UniqueID then
    begin
      if Cnt > 0 then
        continue;
      inc( Cnt );
      result := CmdStruct[ i ].ColumnName;
    end;
  end;
  if Cnt = 0 then
    raise EErrorDevelopment.create( 'Get_CmdLineStruct_FK_Column: No CmdLine FK field' );
  if Cnt >= 2 then
    InternalMessage( 'Get_CmdLineStruct_FK_Column: Hey Programmer! too many CmdLine FK fields' );
end;

function GetTableNames( const DL : TDataLocation; const DoUseDB : boolean ) : string;
begin
  result := '';
  if DouseDB then
  case DL of
    dlCmd :     result := cDBTabCommands;
    dlCmdLine : result := cDBTabCmdLines;
    dlKeyWord : result := cDBTabKeywords;
    dlControl : result := cDBTabControl;
  end
  else
  begin
    case DL of
    dlCmd :     result := cIniFileNameCommands;
    dlCmdLine : result := cIniFileNameCommandLines;
    dlKeyWord : result := cIniFileNameMisc;
    end
  end;
end;

function Bool_To_Str( const Cond: boolean ): string;
begin
  if Cond then
    Result := dbsTrueStr
  else
    Result := dbsFalseStr;
end;

function Str_To_Bool( const Cond: string ): boolean;
begin
  result := Cond = dbsTrueStr;
end;


function Get_Cmd_Field_FieldType( const ID : integer ) : TufFieldType;
begin
  result := CmdStruct[ ID ].FieldType;
end;

function Get_Cmd_Field_FieldType( const ID : TCmdFieldID ) : TufFieldType;
begin
  result := Get_Cmd_Field_FieldType( Ord( ID ) );
end;


function Get_Cmd_Field_UniqueID( const ID : integer ) : integer;
begin
  result := cCmdColMissingUniqueID;//0;

  if ( ID < Low( CmdStruct ) ) or ( ID > High( CmdStruct ) ) then
    exit;

  result := CmdStruct[ ID ].UniqueID;
end;

function Get_Cmd_Field_UniqueID( const ID : TCmdFieldID ) : integer;
begin
  result := Get_Cmd_Field_UniqueID( Ord( ID ) );
end;


function Get_Cmd_Field_ColumnName( const ID : integer ) : string;
begin
  result := CmdStruct[ ID ].ColumnName;
end;

function Get_Cmd_Field_ColumnName( const ID : TCmdFieldID ) : string;
begin
  result := Get_Cmd_Field_ColumnName( Ord( ID ) );
end;


function Get_Cmd_Field_DisplayCaption( const ID : integer ) : string;
begin
  result := CmdStruct[ ID ].DisplayCaption;
end;

function Get_Cmd_Field_DisplayCaption( const ID : TCmdFieldID ) : string;
begin
  result := Get_Cmd_Field_DisplayCaption( Ord( ID ) );
end;


procedure Register_Cmd_DisplayCaption( const ID : TCmdFieldID; Controls : array of TControl );
var
  Idx , i: Integer;
begin

  Idx := Ord( ID );

  if not assigned( CmdStruct[ Idx ].CaptionList ) then
      CmdStruct[ Idx ].CaptionList := TList.Create;

  for i := low( Controls ) to High( Controls ) do
  begin
    CmdStruct[ Idx ].CaptionList.Add( Controls[ i ] );
    Controls[ i ].Tag := CmdStruct[ Idx ].UniqueID;
  end;

end;

procedure Update_Cmd_DisplayCaptions;
var
  i , j: Integer;
  DispCap : String;
begin
//Field language support
//Since the fields Display Captions  are not part of components they need to be updated manually
//if a user changes languages.

  for i := Low( CmdStruct ) to High( CmdStruct ) do
  begin

    case CmdStruct[ i ].FieldID of
      fidInValid : DispCap := cCmdColInValidCaption;
      fidSuperUserMode : DispCap := cCmdColSuperUserModeCaption;
      fidNotes : DispCap := cCmdColNotesCaption;
      fidIsFavorite : DispCap := cCmdColIsFavoriteCaption;
      fidDetachProcess : DispCap := cCmdColDetachProcessCaption;
      fidThreatLevel : DispCap := cCmdColThreatLevelCaption;
      fidCommandName : DispCap := cCmdColCommandNameCaption;
      fidKeyWords : DispCap := cCmdColKeyWordsCaption;
      fidHelpCommand : DispCap := cCmdColHelpCommandCaption;
      fidVersionCommand : DispCap := cCmdColVersionCommandCaption;
      fidLocationPath : DispCap := cCmdColLocationPathCaption;
      fidEntry : DispCap := cCmdColEntryCaption;
      fidFriendlyName : DispCap := cCmdColFriendlyNameCaption;
      fidWantsInput : DispCap := cCmdColWantsInputCaption;
      fidUseShell : DispCap := cCmdColUseShellCaption;
      fidTerminalOnly : DispCap := cCmdColTerminalOnlyCaption;
      fidAlert : DispCap := cCmdColAlertCaption;
      else DispCap := '<internal>';
    end;


    CmdStruct[ i ].DisplayCaption := DispCap;
    if assigned( CmdStruct[ i ].CaptionList ) then
    begin
      for j := 0 to CmdStruct[ i ].CaptionList.Count - 1 do
  //Each Field structure has lables/buttons/etc from mainform assigned, this updates their caption property
        TControl( CmdStruct[ i ].CaptionList[ j ] ).Caption := CmdStruct[ i ].DisplayCaption;
    end;
  end;

end;

function Get_Cmd_Field_IdxFromUniqueID( const UID : integer) : integer;
var
  i : Integer;
begin

//This is primarily designed for saved searches so that old searches that may or may not match current
//DB struture can be found and properly indexed.

  result := cCmdColMissingUniqueID;// 0; //-1;

  for i := Low( CmdStruct ) to High( CmdStruct ) do
  begin
    if CmdStruct[ i ].Searchable then //?? if in future search is taken away from a field needs to be ignored??
      if CmdStruct[ i ].UniqueID = UID then
      begin
        result := i;
        break;
      end;
  end;

end;

function Get_Cmd_Field_IdxFromUniqueID( const UID : integer ) : TCmdFieldID;
begin
//This is primarily designed for saved searches so that old searches that may or may not match current
//DB struture can be found and properly indexed.
  result := TCmdFieldID( Get_Cmd_Field_IdxFromUniqueID( UID ) );
end;


function TDBListKeyWordStructure.GetSectTabName : string;
begin
  result := format( SectTabName, [ ColumnName ] );
end;

procedure TDBListKeyWordStructure.UpdateDisplayCaption;
begin
  DisplayCaptionSingular := cKeyWordsCaption_Singular;
  DisplayCaptionPlural := cKeyWordsCaption_Plural;
end;

function Get_ThreatLevel_Text( const TL : TThreatLevel ) : string;
begin
  case TL of
    tlHarmless : result := cThreatLevelHarmless;
    tlCareful :  result := cThreatLevelCareful;
    tlCaution :  result := cThreatLevelCaution;
    tlDanger :   result := cThreatLevelDanger;
    else result := cThreatLevelNotSpecified;
  end;
end;

function Get_ThreatLevel_Text( const Idx : integer ) : string;
begin
  result := Get_ThreatLevel_Text( TThreatLevel( Idx ) );
end;

function Return_ThreatLevel_Items( Strings : TStrings; Complete : boolean = false ) : integer;
begin

  Strings.Clear;

  if Complete then //for searches: to add the unspecified category
    Strings.Add( Get_ThreatLevel_Text( tlNotSpecified ) );

  Strings.Add( Get_ThreatLevel_Text( tlHarmless ) );
  Strings.Add( Get_ThreatLevel_Text( tlCareful ) );
  Strings.Add( Get_ThreatLevel_Text( tlCaution ) );
  Strings.Add( Get_ThreatLevel_Text( tlDanger ) );

  result := Strings.Count;

end;

function ReturnEnumerationConstants( const UID : integer; Strings : TStrings ) : boolean;
var
  Cnt : Integer;
begin
  Cnt := -1;
  case UID of
    cCmdColThreatLevelUniqueID : Cnt := Return_ThreatLevel_Items( Strings, true );
  end;
  result := Cnt <> 0;
end;


procedure Clear_ListFields_Linker;
var
  i : Integer;
begin
  for i := 0 to ListFields_Linker.Count - 1 do
    ListFields_Linker.Objects[ i ] := nil;//do not free objects, that is taken care of by program
  ListFields_Linker.Clear;
end;

function ListFields_Linker__ListIndex( const IdxStr : string ) : integer;
begin
  Result := ListFields_Linker.IndexOf( IdxStr );
end;

function ListFields_Linker__GetFirst_List_Item( const IdxStr : string ) : string;
var
  Idx : Integer;
begin
  Result := '????';
  Idx := ListFields_Linker__ListIndex( IdxStr );
  if Idx > -1 then
  begin
    if TStrings( ListFields_Linker.Objects[ Idx ] ).Count > 0 then
      result := TStrings( ListFields_Linker.Objects[ Idx ] )[ 0 ];
  end;
end;

function ListFields_Linker__AttachList( const IdxStr : string; Strings : TStrings  ) : boolean;
var
  Idx : Integer;
begin
  Result := false;
  Idx := ListFields_Linker__ListIndex( IdxStr );
  if Idx > -1 then
  begin
    Strings.Assign( TStrings( ListFields_Linker.Objects[ Idx ] ) );
    result := true;
  end;
end;

function ListFields_Linker__ValidItem( const IdxStr, ItemStr : string ) : boolean;
var
  Idx : Integer;
begin
  Idx := ListFields_Linker.IndexOf( IdxStr );
  if Idx > -1 then
    Idx := TStringList( ListFields_Linker.Objects[ Idx ] ).IndexOf( ItemStr );
  result := Idx > -1;
end;

function ListEmpty_ListFields_Linker( const ToMatch : string ) : boolean;
var
  Idx : Integer;
begin

  result := true;

  if ListFields_Linker.Count = 0 then
    exit;

  Idx := ListFields_Linker.IndexOf( ToMatch );

  If Idx < 0 then
    exit;

  if TStringList( ListFields_Linker.Objects[ Idx ] ).Count = 0 then
    exit;

  result := false;

end;

var
  i : integer;


initialization

  ListFields_Linker := TStringList.Create;


finalization

  for i := Low( CmdStruct ) to High( CmdStruct ) do
    if assigned( CmdStruct[ i ].CaptionList ) then
      CmdStruct[ i ].CaptionList.free; //do not free the objects, they are pointers to components on Main Form

  if assigned( ListFields_Linker ) then
  begin
    Clear_ListFields_Linker;
    FreeAndNil( ListFields_Linker );
  end;

end.

