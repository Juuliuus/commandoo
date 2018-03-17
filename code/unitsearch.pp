unit unitSearch;

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

uses
  Classes, SysUtils
  , unitDBStructure
  , unitDBConstants
  , unitFields
  , synregexpr //this is the Regexpression unit
  , jinifiles
  , unitSharedObj
  ;


type

  TBaseSearch = class;

  { TSearchItem }

  TSearchItem = class( TObject )
  private
    fNeedDefault : boolean;
    fAll_AndOr_Idx : integer;
    fCmdFieldID_Idx : integer;
    fCondition : TCondition;
    fFieldType : TufFieldType;
    fInValidMsg : string;
    fReUseFetchedIdx : integer;
    fSearchOperatorType : TSearchOperatorType;
    fSearchValue : string;
    fSIT : TSearchItemType;
    fsiUniqueID : integer;
    fCaseSensitive : boolean;
    fUserTag : integer;
    fValid : boolean;
    fOrigDisplayCaption : string; //so bad fields in a saved search can display what the name was
    function HandleBoolean( const Compare : boolean ) : boolean;
    function HandleDouble( const Compare : double ) : boolean;
    function HandleInteger( const Compare : integer ) : boolean;
    function HandleList : boolean;
    function HandleString : boolean;
    function HandleText : boolean;
    procedure SetAll_AndOr_Idx( AValue : integer );
    procedure SetCondition( AValue : TCondition );
    procedure SeTFieldType( AValue : TufFieldType );
    procedure SetReUseFetchedIdx( AValue : integer );
    procedure SetSearchOperatorType( AValue : TSearchOperatorType );
    procedure SetSearchValue( AValue : string );
    procedure SetSIT( AValue : TSearchItemType );
    procedure SetsiUniqueID( AValue : integer );
    procedure SetCaseSensitive( AValue : boolean );
    function GetsiColumnName : string;
    function GetsiDisplayCaption : string;
    function GetsiDisplayAbbrev : string;
    procedure SetUserTag( AValue : integer );


  protected
    fCompare_To : variant;
    BS : TBaseSearch;
    procedure Save( IFile : TJInifile; const IdxID, Idx : integer );
    procedure Load( IFile : TJInifile; const IdxID, Idx : integer );
    function Validate : boolean;


  public
    constructor Create( aBS : TBaseSearch; const aFieldType : TufFieldType ); virtual;
    destructor Destroy; override;

    function Evaluate : boolean;
    procedure SearchItem_Init( const pSVal : string; const pCond : TCondition;
                                         const pCaseSens : boolean; const UID : integer );
    procedure SearchItem_ReInit( const pCmdFieldID : TCmdFieldID; const NewFieldType : TufFieldType );
    procedure Operator_Init( pSOT : TSearchOperatorType = sotOR );
    function GetSearchOperatorCaption : string;
    function GetNewSearchValueDefault : string;
    function GetNewSearchConditionDefault : TCondition;
    function IsInvalid : boolean;
    procedure ClearInvalidMessage;
    procedure SetOldFieldName( const aName : string );
    function SearchValue_ForDisplay : string;

    property All_AndOr_Idx : integer read fAll_AndOr_Idx write SetAll_AndOr_Idx;

    property ReUseFetchedIdx : integer read fReUseFetchedIdx write SetReUseFetchedIdx;
    property Condition : TCondition read fCondition write SetCondition;
    property SIT : TSearchItemType read fSIT write SetSIT;
    property SearchOperatorType : TSearchOperatorType read fSearchOperatorType write SetSearchOperatorType;
    property FieldType : TufFieldType read fFieldType write SetFieldType;
    property siColumnName : string read GetsiColumnName;
    property siDisplayCaption : string read GetsiDisplayCaption;
    property siDisplayAbbrev : string read GetsiDisplayAbbrev;
    property siUniqueID : integer read fsiUniqueID write SetsiUniqueID;
    property CmdFieldID_Idx : integer read fCmdFieldID_Idx;

    property SearchValue : string read fSearchValue write SetSearchValue;
    property CaseSensitive : boolean read fCaseSensitive write SetCaseSensitive;
    property UserTag : integer read fUserTag write SetUserTag;

    property InValidMsg : string read fInValidMsg;
    property Valid : boolean read fValid;
    property OrigDisplayCaption : string read fOrigDisplayCaption;


  end;


//First implementation, I also have other ideas but for now this will do
//Also, due to the design I currently allow searches that are either commands or command lines but not
//both combined in search logic. I think I have a way to do that but will let that wait until after first off. release
  { TBaseSearch }

  TBaseSearch = class( TObject )
  private
    fDBServer : TInfoServer;//pointer only don't create
    fInfoServerIdx : TDataLocation;
    fIsUsed : boolean;
    fResultDisplayColAlt : string;
    fResultDisplayColMain : string;
    fSIList : TList;
    faryBoolEval : array of array of boolean;
    fUserTag : integer;

    function InvalidStructure : boolean;
    procedure PrePrep;
    procedure ClearSIList;
    function Evaluate( Idx : integer; const SizeOfList : integer; var NewPosition : integer ) : boolean;
    function InitializeSearch( const Key : string ) : boolean;
    procedure PostPrep;
    procedure SetDBServer( AValue : TInfoServer );
    procedure SetInfoServerIdx( AValue : TDataLocation );
    procedure SetIsUsed( AValue : boolean );
    procedure SetSIList( AValue : TList );
    function GetLoaded : boolean;
    procedure SetUserTag( AValue : integer );

  protected
    RegX : TRegExpr;
    procedure Save( IFile : TJiniFile; const IdxId : integer );
    function Load( IFile : TJiniFile; const IdxID : integer) : boolean;

  public

    SearchIndices : array [ 0..Ord( cfdENDFORCOUNTING_DoNotUse ) - 1 ] of integer; // iniidx's, -1 = ignore

    constructor Create( DBS : TInfoServer ); virtual;
    destructor Destroy; override;

    function GetExpression : string;

    procedure Clear;
    procedure Execute( MatchingItems : TStrings );
    procedure AnalyzeAndOr( Idx : integer; var NewPos : integer; SectionStack : integer = - 1 );
    procedure AddOperator( const Kind : TSearchOperatorType );
    function AddSearchItem( const CmdIdx : integer; const SearchVal : string;
                            const Cond : TCondition; const CaseSens : boolean ) : integer; overload;
    function AddSearchItem( const CmdIdx : TCmdFieldID; const SearchVal : string;
                            const Cond : TCondition; const CaseSens : boolean ) : integer; overload;

    property SIList : TList read fSIList write SetSIList;
    property InfoServerIdx : TDataLocation read fInfoServerIdx write SetInfoServerIdx;
    property Loaded : boolean read GetLoaded;

    property IsUsed : boolean read fIsUsed write SetIsUsed;
    property UserTag : integer read fUserTag write SetUserTag;

    property ResultDisplayColMain : string read fResultDisplayColMain;
    property ResultDisplayColAlt : string read fResultDisplayColAlt;

    property DBServer : TInfoServer read fDBServer write SetDBServer;


  end;




const
  cIdxCmd = 0; //signifies that Searches[ idx ] is for Commands
  cIdxCmdLine = 1; //signifies that Searches[ idx ] is for CommandLines

type
  { TSearchObj }

  TSearchObj = class( TObject )
  private
    fDBInfo : string;
    fFileName : string;
    fUserTag : integer;
    procedure SetDBInfo( AValue : string );
    procedure SetFileName( AValue : string);
    function GetLoaded : boolean;
    procedure SetUserTag( AValue : integer );

  protected
    function GetExpressionString( const cIdx : integer ) : string;
  public

    Searches : array[ cIdxCmd..cIdxCmdLine ] of TBaseSearch;

    constructor Create( const Info : string; DBS : TInfoServer ); virtual;
    destructor Destroy; override;
    class function CanLoad( const FN : String; var Idx : integer ) : boolean;

    function Load( const FN : String ) : boolean;
    procedure Save( const FN, Info : String );
    function GetAllExpressions : string;
    procedure Clear( NewName : string = '' );
    function InvalidStructure : string;

    property Loaded : boolean read GetLoaded;
    property DBInfo : string read fDBInfo write SetDBInfo;
    property FileName : string read fFileName write SetFileName;
    property UserTag : integer read fUserTag write SetUserTag;

  end;

  { TSearchFieldRef }

  TSearchFieldRef = class( TObject )
  private
    fColumnName : string;
    fsfrCmdFieldID : TCmdFieldID;
    fsfrCmdObjType : TCmdObjType;
    fsfrFieldType : TufFieldType;
    procedure SetColumnName( AValue : string );
    procedure SetsfrCmdFieldID( AValue : TCmdFieldID );
    procedure SetsfrCmdObjType( AValue : TCmdObjType );

  protected
  public

    constructor Create( const COT : TCmdObjType; const CFI : TCmdFieldID ); virtual;
    destructor Destroy; override;

    property sfrCmdObjType : TCmdObjType read fsfrCmdObjType write SetsfrCmdObjType;
//TCmdObjType = ( cotNone, cotBase, cotCmd, cotCmdLine );
    property sfrCmdFieldID : TCmdFieldID read fsfrCmdFieldID write SetsfrCmdFieldID;
//fidSuperUserMode, fidNotes, fidIsFavorite, fidDetachProcess, fidThreatLevel, and more
    property sfrFieldType : TufFieldType read fsfrFieldType;
//TufFieldType = ( ftUndefined, ftString_Key, ftString, ftBoolean, ftInteger, ftList, ftEnum, ftDouble, ftText );
    property ColumnName : string read fColumnName write SetColumnName;

  end;

// functions for writing/reading settings to/from saved searches
  function SearchItemTypeToStr( const SIT : TSearchItemType ) : string;
  function StrToSearchItemType( const SIT : string ) : TSearchItemType;
  function SearchOperatorTypeToStr( const SOT : TSearchOperatorType; DoFormat : shortint = 0 ) : string;
  function StrToSearchOperatorType( const SOT : string ) : TSearchOperatorType;

  function ConditionToStr( const CO : TCondition ) : string;
  function StrToCondition( const CO : string ) : TCondition;
  function ConditionToStr_Display( const CO : TCondition ) : string;

  function FieldType_To_SearchItemType( const FT : TufFieldType ) : TSearchItemType;
  function GetFieldTypeforOperator : TufFieldType;

  procedure GetAllowedSearchConditions( Strings : TStrings; const ListIdx : integer );
  function ReturnSearchCondition( const Idx, ListIdx : integer ) : TCondition;
  function ReturnSearchConditionIdx( const Cond : TCondition; const ListIdx : integer ) : integer;

  procedure Get_Cmd_Fields_Searchable( var Strings : TStringList );

resourcestring
  cusSearchCaption_List = 'List:  %s';
  cusSearchCaption_String = 'String:  %s';
  cusSearchCaption_Text = 'Text:  %s';
  cusSearchCaption_Bool = 'Boolean:  %s';
  cusSearchCaption_Enum = 'Enum:  %s';
  cusSearchCaption_Integer = 'Int:  %s';
  cusSearchCaption_Double = 'Real:  %s';
  cDefSearchFileName = 'NewSearch';
  ccapusSearchDetailProblem = 'Search Item Problem(s)';
  ccapSearchLoadError = 'Error Opening Search';
  cmsgSearchLoadError = 'Search File "%s" could not be opened. File invalid or missing.';

const
//used to specify cConditions... arrays
  cConditionsAllowed_List = 0;
  cConditionsAllowed_Number = 1;
  cConditionsAllowed_Text = 2;
  cConditionsAllowed_String = 3;
  cConditionsAllowed_Boolean = 4;

var
  gSearchInfo : TStringlist;


implementation

uses //unitSharedObj
     //,
     unitDBUtils
     , juusgen
     , unitGlob
     ;

resourcestring
  cusMatchTypeEquals = 'Equals (%s)';
  cusMatchTypeNoEquals = 'Doesn''t Equal (%s)';

  cusMatchTypeMatch = 'Matches (%s)';
  cusMatchTypeNoMatch = 'Doesn''t Match (%s)';

  cusMatchTypeContains = 'Contains (%s)';
  cusMatchTypeNoContains = 'Doesn''t Contain (%s)';
  cusMatchTypeIsEmpty = 'Item is Empty (%s)';
  cusMatchTypeRE = 'Regular Expression (%s)';

  cusMatchTypeLT = 'Less than (%s)';
  cusMatchTypeLTE = 'Less than or equal (%s)';
  cusMatchTypeGT = 'Greater than (%s)';
  cusMatchTypeGTE = 'Greater than or equal (%s)';
  cusMatchTypeNONE = 'ERROR (%s)';

  cusInValidMsgBadField = 'Field "%s" does not exist in the database.' + Lineending + LineEnding;
  cusInValidMsgBadListItem = 'List Value "%s" does not exist in this Field''s Master %s List, '
                             + 'maybe you deleted it? It has been changed to a valid value for you.'
                             + Lineending + LineEnding;
  cuscapSearchExpr = 'Search Expressions:' + LineEnding + LineEnding;
  cuscapCurrExpr = 'Current Expression: ';
  cuscapOffExpr = '( turned off, not used )';
  cuscapNoExpr = '< empty, no search items >';
  cuscapBadUniqueIdx = '%s: UniqueID invalid';



const
  AND_Idx = 0;
  OR_Idx = 1;
  cBaseSearchSection = 'Search';
  cSearchItemSection = '%dSI%d';
  cBadColumnName = 'x*x';

  cStrCondDisplayErr = '<err>';
  cStrCondDisplayRegEx = 'RE';
  cStrCondDisplayEqual = '=';
  cStrCondDisplayNotEqual = '<>';
  cStrCondDisplayContains = '*';
  cStrCondDisplayNotContains = '~*';
  cStrCondDisplayLT = '<';
  cStrCondDisplayLTE = '<=';
  cStrCondDisplayGT = '>';
  cStrCondDisplayGTE = '>=';
  cStrCondDisplayIsEmpty = 'IsEmpty';

  cusIniVal_UnkField = 'UnkField';
  cusIniVal_Name = 'Name';
  cusIniVal_DBInfo = 'DBInfo';
  cusIniVal_UserTag = 'UserTag';
  cusIniVal_IsUsed = 'IsUsed';
  cusIniVal_SICount = 'SICount';
  cusIniVal_FieldType = 'FieldType';
  cusIniVal_SOT = 'SOT';
  cusIniVal_UID = 'UID';
  cusIniVal_CaseSensitive = 'CaseSensitive';
  cusIniVal_Condition = 'Condition';
  cusIniVal_SearchValue = 'SearchValue';
  cusIniVal_OrigField = 'OrigField';

  cConditions_List : array[ 0..5 ] of TCondition = ( coMatchQualified, coMatchQualifiedNot,
                                                     coContains, coContainsNot, coIsEmpty, coRegEx );
  //another order for testing
    //cConditions_List : array[ 0..5 ] of TCondition = ( coRegEx, coIsEmpty, coContainsNot, coContains,
    //                                                    coMatchQualifiedNot, coMatchQualified );
  cConditions_Text : array[ 0..3 ] of TCondition = ( coContains, coContainsNot, coIsEmpty, coRegEx );
  cConditions_String : array[ 0..5 ] of TCondition = ( coEqual, coNotEqual,
                                                       coContains, coContainsNot, coIsEmpty, coRegEx );
  cConditions_Number : array[ 0..5 ] of TCondition = ( coEqual, coNotEqual, coLT, coLTE, coGT, coGTE );
  cConditions_Boolean : array[ 0..1 ] of TCondition = ( coEqual, coNotEqual );

var
  CurrConditionsArray : array of TCondition;
  locSearchIniFile : TJIniFile;


procedure GetConditionArray( const ListIdx : integer );
begin
  case ListIdx of
    cConditionsAllowed_List : CurrConditionsArray := cConditions_List;
    cConditionsAllowed_Number : CurrConditionsArray := cConditions_Number;
    cConditionsAllowed_Text : CurrConditionsArray := cConditions_Text;
    cConditionsAllowed_String : CurrConditionsArray := cConditions_String;
    cConditionsAllowed_Boolean : CurrConditionsArray := cConditions_Boolean;
  end;
end;

procedure GetAllowedSearchConditions( Strings : TStrings; const ListIdx : integer );
var
  i : Integer;
  FormatStr : String;
begin

  GetConditionArray( ListIdx );

//returns conditions for the search edit forms, to change order of condtions,
//change the order of condtions in cConditions_List
  for i := Low( CurrConditionsArray ) to High( CurrConditionsArray ) do
  begin
    case CurrConditionsArray[ i ] of
      coMatchQualified : FormatStr := cusMatchTypeMatch;
      coMatchQualifiedNot : FormatStr := cusMatchTypeNoMatch;
      coContains : FormatStr := cusMatchTypeContains;
      coContainsNot : FormatStr := cusMatchTypeNoContains;
      coIsEmpty : FormatStr := cusMatchTypeIsEmpty;
      coRegEx : FormatStr := cusMatchTypeRE;
      coEqual : FormatStr := cusMatchTypeEquals;
      coNotEqual : FormatStr := cusMatchTypeNoEquals;
      coLT : FormatStr := cusMatchTypeLT;
      coLTE : FormatStr := cusMatchTypeLTE;
      coGT : FormatStr := cusMatchTypeGT;
      coGTE : FormatStr := cusMatchTypeGTE;
      else FormatStr := cusMatchTypeNONE;
    end;

    Strings.Add( format( FormatStr,
                        [
                         ConditionToStr_Display( CurrConditionsArray[ i ] )
                        ]
                       )
               );

  end;
end;

function ReturnSearchCondition( const Idx, ListIdx : integer ) : TCondition;
begin
  GetConditionArray( ListIdx );
  result := CurrConditionsArray[ Idx ];
end;

function ReturnSearchConditionIdx( const Cond : TCondition; const ListIdx : integer ) : integer;
var
  i : Integer;
begin
  GetConditionArray( ListIdx );
  result := 0;//or -1?? question of failing or defaulting
  for i := Low( CurrConditionsArray ) to High( CurrConditionsArray ) do
  begin
    if CurrConditionsArray[ i ] <> Cond then
      continue;
    result := i;
    break;
  end;
end;


procedure Get_Cmd_Fields_Searchable( var Strings : TStringList );
var
  i : Integer;
begin

  if not assigned( Strings ) then
    Strings := TStringlist.Create;

  for i := Low( CmdStruct ) to High( CmdStruct ) do
  begin
    if not CmdStruct[ i ].Searchable then
      continue;
//    fSearchFields
    Strings.AddObject(
             CmdStruct[ i ].DisplayCaption,
             TSearchFieldRef.Create( CmdStruct[ i ].CmdObjType, CmdStruct[ i ].FieldID )
                                   );
  end;

  Strings.Sort;

end;


procedure GetAllowedConditions_List( Strings : TStrings );
var
  i : Integer;
  FormatStr : String;
begin
//returns conditions for the search edit forms, to change order of condtions,
//change the order of condtions in cConditions_List
  for i := Low( cConditions_List ) to High( cConditions_List ) do
  begin
    case cConditions_List[ i ] of
      coMatchQualified : FormatStr := cusMatchTypeMatch;
      coMatchQualifiedNot : FormatStr := cusMatchTypeNoMatch;
      coContains : FormatStr := cusMatchTypeContains;
      coContainsNot : FormatStr := cusMatchTypeNoContains;
      coIsEmpty : FormatStr := cusMatchTypeIsEmpty;
      coRegEx : FormatStr := cusMatchTypeRE;
    end;
    Strings.Add( format(
                         FormatStr,
                         [
                           ConditionToStr_Display( cConditions_List[ i ] )
                         ]
                       )
               );
  end;
end;

function ReturnCondition_List( Idx : integer ) : TCondition;
begin
  result := cConditions_List[ Idx ];
end;

function ReturnConditionIdx_List( Cond : TCondition ) : integer;
var
  i : Integer;
begin
  result := 0;//or -1?? question of failing or defaulting
  for i := Low( cConditions_List ) to High( cConditions_List ) do
  begin
    if cConditions_List[ i ] <> Cond then
      continue;
    result := i;
    break;
  end;
end;


{ TSearchFieldRef }

procedure TSearchFieldRef.SetsfrCmdFieldID( AValue : TCmdFieldID );
begin
  if fsfrCmdFieldID = AValue then Exit;
  fsfrCmdFieldID := AValue;
  fsfrFieldType := CmdStruct[ ord( fsfrCmdFieldID ) ].FieldType;
end;

procedure TSearchFieldRef.SetColumnName( AValue : string );
begin
  if fColumnName = AValue then Exit;
  fColumnName := AValue;
end;

procedure TSearchFieldRef.SetsfrCmdObjType( AValue : TCmdObjType );
begin
  if fsfrCmdObjType = AValue then Exit;
  fsfrCmdObjType := AValue;
end;

constructor TSearchFieldRef.Create( const COT : TCmdObjType; const CFI : TCmdFieldID );
begin
  //inherited ;
  sfrCmdFieldID := CFI;
  sfrCmdObjType := COT;
  case CFI of
    fidKeywords : fColumnName := KeyWordStruct.ColumnName;
    else fColumnName := '<error>';
  end;
end;

destructor TSearchFieldRef.Destroy;
begin
  inherited Destroy;
end;

{ TSearchObj }

procedure TSearchObj.SetDBInfo( AValue : string );
begin
  if fDBInfo = AValue then Exit;
  fDBInfo := AValue;
end;

procedure TSearchObj.SetFileName( AValue : string );
begin
  if fFileName = AValue then Exit;
  fFileName := AValue;
end;

function TSearchObj.GetLoaded : boolean;
var
  i : integer;
begin
  result := false;
  for i := Low( Searches ) to High( Searches ) do
    result := result or Searches[ i ].Loaded;
end;

procedure TSearchObj.SetUserTag( AValue : integer );
begin
  if fUserTag = AValue then Exit;
  fUserTag := AValue;
end;

function TSearchObj.GetExpressionString( const cIdx : integer ) : string;
begin
  case cIdx of
    cIdxCmd : result := cNameItem_Command + LineEnding;
    cIdxCmdLine : result := cNameItem_CommandLine + LineEnding;
    else result := 'Invalid cIdx###' + LineEnding;
  end;
end;

constructor TSearchObj.Create( const Info : string; DBS : TInfoServer );
begin
  inherited Create;

  fDBInfo := Info;
  fFileName := cDefSearchFileName;
  fUserTag := 0;

  Searches[ cIdxCmd ] := TBaseSearch.Create( DBS );
  Searches[ cIdxCmd ].InfoServerIdx := dlCmd;

  Searches[ cIdxCmdLine ] := TBaseSearch.Create( DBS );
  Searches[ cIdxCmdLine ].InfoServerIdx := dlCmdLine;

end;

destructor TSearchObj.Destroy;
var
  i : integer;
begin

  for i := Low( Searches ) to High( Searches ) do
    TObject( Searches[ i ] ).Free;

  inherited Destroy;
end;

function TSearchObj.GetAllExpressions : string;
var
  i : integer;
begin
  result := cuscapSearchExpr;
  for i := Low( Searches ) to High( Searches ) do
  begin
    result := result + GetExpressionString( i ) + Searches[ i ].GetExpression + LineEnding + LineEnding;
  end;

end;

procedure TSearchObj.Save( const FN, Info : String );
var
  i : Integer;
begin

  if fileexists( FN ) then
    deletefile( FN );

  fDBinfo := Info;

  locSearchIniFile := TJiniFile.Create( FN );
  try
    locSearchIniFile.CacheUpdates := true;
    locSearchIniFile.WriteSetIniFileWarning( false );
    locSearchIniFile.WriteString( cBaseSearchSection, cusIniVal_DBInfo, DBInfo );
    locSearchIniFile.WriteString( cBaseSearchSection, cusIniVal_Name, FileName );
    locSearchIniFile.WriteInteger( cBaseSearchSection, cusIniVal_UserTag, fUserTag );

    for i := Low( Searches ) to High( Searches ) do
      Searches[ i ].Save( locSearchIniFile, i );

    locSearchIniFile.UpdateFile;

  finally
    locSearchIniFile.Free;
    locSearchIniFile := nil;
  end;

end;

class function TSearchObj.CanLoad( const FN : String; var Idx : integer) : boolean;
begin

  result := false;

  if not fileexists( FN ) then
    exit;

  locSearchIniFile := TJiniFile.Create( FN );
  try

    if not locSearchIniFile.SectionExists( cBaseSearchSection ) then
//invalid or not one of our search ini files
      exit;

    Idx := locSearchIniFile.ReadInteger( cBaseSearchSection, cusIniVal_UserTag, 0 );

    result := true;

  finally
    FreeAndNil( locSearchIniFile );
  end;

  if not result then
    InternalMessage( format( cmsgSearchLoadError, [ FN ] ) );

end;


function TSearchObj.Load( const FN : String ) : boolean;
var
  i : Integer;
begin

  result := false;
  if not fileexists( FN ) then
    exit;

  locSearchIniFile := TJiniFile.Create( FN );
  try

    if not locSearchIniFile.SectionExists( cBaseSearchSection ) then
//invalid or not one of our search ini files
      exit;

    Clear( extractfilename( FN ) );

    fDBInfo := locSearchIniFile.ReadString( cBaseSearchSection, cusIniVal_DBInfo, '<no info>' );
    FileName := locSearchIniFile.ReadString( cBaseSearchSection, cusIniVal_Name, cDefSearchFileName );
    fUserTag := locSearchIniFile.ReadInteger( cBaseSearchSection, cusIniVal_UserTag, 0 );

    for i := Low( Searches ) to High( Searches ) do
      if not Searches[ i ].Load( locSearchIniFile, i ) then
        exit;

    result := true;

  finally
   locSearchIniFile.Free;
   locSearchIniFile := nil;
  end;

end;

function TSearchObj.InvalidStructure : string;
var
  i : integer;
  IsBad : Boolean;
begin

  result := '';
  IsBad := false;

  for i := Low( Searches ) to High( Searches ) do
  begin
    IsBad := Searches[ i ].InvalidStructure;
    if IsBad then
      break;
  end;

  if IsBad then
  begin
    result := GetAllExpressions;
    Clear;
  end;

end;


procedure TSearchObj.Clear( NewName : string = '' );
var
  i : integer;
begin

  if NewName <> '' then
    FileName := NewName;

  for i := Low( Searches ) to High( Searches ) do
    Searches[ i ].Clear;

end;


{ TBaseSearch }


procedure TBaseSearch.SetInfoServerIdx( AValue : TDataLocation );
begin
  if fInfoServerIdx = AValue then Exit;
  fInfoServerIdx := AValue;
  GetSearchResultDisplayCol( fInfoServerIdx, fResultDisplayColMain, fResultDisplayColAlt );
end;

procedure TBaseSearch.SetIsUsed( AValue : boolean );
begin
  if fIsUsed = AValue then Exit;
  fIsUsed := AValue;
end;

procedure TBaseSearch.SetSIList( AValue : TList );
begin
  if fSIList = AValue then Exit;
  fSIList := AValue;
end;

function TBaseSearch.GetLoaded : boolean;
var
  i , GoodCnt: Integer;
begin
  result := false;
  GoodCnt := 0;

  for i := 0 to SIList.Count - 1 do
  begin
    if TSearchItem( SIList[ i ] ).SIT = sitOperator then
      continue;
    if TSearchItem( SIList[ i ] ).IsInvalid then
      exit
    else inc( GoodCnt );
  end;

  if GoodCnt = 0 then
    exit; //it's empty

  result := true;

end;

procedure TBaseSearch.SetUserTag( AValue : integer );
begin
  if fUserTag = AValue then Exit;
  fUserTag := AValue;
end;


constructor TBaseSearch.Create( DBS : TInfoServer );
var
  i : integer;
begin

  fIsUsed := true;
  fUserTag := 0;
  fDBServer := DBS;

  for i := Low( SearchIndices ) to High( SearchIndices ) do
    SearchIndices[ i ] := -1;
  fSIList := TList.Create;
  RegX := TRegExpr.Create;
  fInfoServerIdx := dlForCountingDNU;//initialize to "invalid" value
end;

destructor TBaseSearch.Destroy;
begin

  SetLength( faryBoolEval, 0, 0 );
  faryBoolEval := nil;

  if assigned( RegX ) then
   FreeAndNil( RegX );

  if assigned( fSIList ) then
  begin
    ClearSIList;
    FreeAndNil( fSIList );
  end;

  inherited Destroy;

end;

procedure TBaseSearch.ClearSIList;
var
  i : Integer;
begin
  for i := 0 to fSIList.Count - 1 do
  begin
    if assigned( fSIList[ i ] ) then
      TObject( fSIList[ i ] ).Free;
  end;
  SIList.Clear;
end;

function TBaseSearch.GetExpression : string;
var
  i : Integer;
  SI : TSearchItem;
  Cond , aValue: string;
const
  CondFormat = ' %s ';
  OpFormat = '%s ';
begin

  result := '';

  for i := 0 to SIList.Count - 1 do
  begin
    SI := TSearchItem( SIList[ i ] );
    if SI.SIT = sitOperator then
    begin
      result := result + format( OpFormat, [ SearchOperatorTypeToStr( SI.SearchOperatorType, 1 ) ] );
      continue;
    end;

    Cond := format( CondFormat, [ ConditionToStr_Display( SI.Condition ) ] );

    aValue := SI.SearchValue_ForDisplay;
    if SI.SIT = sitString then
      aValue := DoubleQuotedString( aValue );
    result := result + SI.siDisplayCaption + Cond + aValue + ' ';

  end;

  if result = '' then
    result := cuscapNoExpr
  else result := cuscapCurrExpr
                 + strif( not IsUsed, cuscapOffExpr )
                 + LineEnding + LineEnding
                 + result
                 + LineEnding + LineEnding;

end;

procedure TBaseSearch.Save( IFile : TJiniFile; const IdxId : integer );
var
  i : Integer;
  Section : String;
begin

  Section := inttostr( IdxId ) + cBaseSearchSection;

  IFile.WriteBool( Section, cusIniVal_IsUsed, fIsUsed );
  IFile.WriteInteger( Section, cusIniVal_UserTag, fUserTag );

  if Loaded then
  begin

    IFile.WriteInteger( Section, cusIniVal_SICount, SIList.Count );

    for i := 0 to SIList.Count - 1 do
      TSearchItem( SIList[ i ] ).Save( IFile, IdxId, i );

  end else IFile.WriteInteger( Section, cusIniVal_SICount, 0 );


end;

procedure TBaseSearch.Clear;
begin
//No, shouldn't matter  //InfoServerIdx := should this be cleared??
  ClearSIList;
end;

function TBaseSearch.Load( IFile : TJiniFile; const IdxID : integer ) : boolean;
var
  Str , Section: String;
  SICount : LongInt;
  i : Integer;
begin

  result := false;

  Section := inttostr( IdxID ) + cBaseSearchSection;

  if not IFile.SectionExists( Section ) then
    exit;

  fUserTag := IFile.ReadInteger( Section, cusIniVal_UserTag, 0 );
  fIsUsed := IFile.ReadBool( Section, cusIniVal_IsUsed, true );

  SICount := IFile.ReadInteger( Section, cusIniVal_SICount, 0 );

  for i := 0 to SICount - 1 do
  begin

    Str := IFile.ReadString( format( cSearchItemSection, [ IdxID, i ] ), cusIniVal_FieldType, '' );
    if Str = '' then
      raise EErrorBadHandEdit.create( format( 'TBaseSearch.Load: Invalid %s.', [ cusIniVal_FieldType ] ) );
    SIList.Add( TSearchItem.Create( Self, StrToFieldType( Str ) ) );

    TSearchItem( SIList[ i ] ).Load( IFile, IdxID, i );

  end;

  result := true;

end;

function TBaseSearch.InvalidStructure : boolean;
var
  i, ParenCount , LastOne: Integer;
  SI : TSearchItem;
  SOT , LastSOT: TSearchOperatorType;
begin
//this is only to check loaded Search Files because they may have been hand edited with an accidental invalid structure
  result := false;
  ParenCount := 0;
  LastSOT := sotNone;
  LastOne := SIList.Count - 1;

  for i := 0 to LastOne do
  begin

    SI := TSearchItem( SIList[ i ] );

    SOT := SI.SearchOperatorType;

    case SOT of
      sotOpen : inc( ParenCount );
      sotClose : dec( ParenCount );
    end;

 //    sotNone, sotNOT, sotAND, sotOR, sotXOR, sotOpen, sotClose
    if i > 0 then
    begin

      if i = LastOne then
      begin
         result := SOT in [ sotNOT, sotAND, sotOR, sotXOR, sotOpen ];
         if result then
           break;
      end;

      case LastSOT of
        sotNone : result := SOT in [ sotOpen ];
        sotNOT : result := SOT in [ sotNOT, sotAND, sotOR, sotXOR, sotClose ];
        sotAND, sotOR, sotXOR : result := SOT in [ sotAND, sotOR, sotXOR, sotClose ];
        sotOpen : result := SOT in [ sotAND, sotOR, sotXOR, sotClose ];
        sotClose : result := SOT in [ sotNone, sotNOT, sotOpen ];
      end;

    end // i > 0
    else result := SOT in [ sotAND, sotOR, sotXOR, sotClose ];

    if result then
      break;

    LastSOT := SOT;

  end;

  if not result then
   result := ParenCount <> 0;

end;

function TBaseSearch.InitializeSearch( const Key : string ) : boolean;
var
  i , j, LastReUse: Integer;
  SI, tmpSI : TSearchItem;
  IsValid : Boolean;
begin


  result := false;
  LastReUse := 0;
  IsValid := false;

  for i := 0 to SIList.Count - 1 do
  begin

    SI := TSearchItem( SIList[ i ] );

    if SI.SIT = sitOperator then
      continue;

    if SI.ReUseFetchedIdx <= LastReUse then
      continue;

    if SI.siColumnName = cBadColumnName then
      raise EErrorDevelopment.create( format( cuscapBadUniqueIdx, [ 'TBaseSearch.InitializeSearch' ] ) );

    SI.fCompare_To := fDBServer.SearchRead( SI.SIT, Key, SI.siColumnName, IsValid );

//For text (ini) files there are control sections that are not data, these must not be processed, and so not IsValid
    if not IsValid then
      exit;

    for j := i + 1 to SIList.Count - 1 do
    begin
//this re-uses values we've already looked up for search items that will also use that same value
//gives a small efficiency gain, I believe
      tmpSI := TSearchItem( SIList[ j ] );
      if tmpSI.SIT = sitOperator then
        continue;

      if SI.ReUseFetchedIdx = tmpSI.ReUseFetchedIdx then
        tmpSI.fCompare_To := SI.fCompare_To;

    end;

    LastReUse := SI.ReUseFetchedIdx;

  end;

  result := true;

end;


procedure TBaseSearch.AnalyzeAndOr( Idx : integer; var NewPos : integer; SectionStack : integer = -1 );
var
  AndCount, OrCount, TotCount: Integer;
  SI : TSearchItem;
begin
//evaluates and's and or's and xor's for each parenthesis group "( )" individually and recursively. The index to
//these results is stored on the opening parenthesis "(" so that it can used in Evaluate to bail out early if
//the logical result is known early.

  AndCount := 0;
  OrCount := 0;
  TotCount := 0;

  inc( SectionStack );
  TSearchItem( SIList[ Idx ] ).All_AndOr_Idx := SectionStack;
  SetLength( faryBoolEval, SectionStack + 1, 2 );
  faryBoolEval[ SectionStack ][ AND_Idx ] := false;
  faryBoolEval[ SectionStack ][ OR_Idx ] := false;

  inc( Idx );

  While Idx < SIList.Count - 1 do
  begin

    SI := TSearchItem( SIList[ Idx ] );

    if SI.SIT = sitOperator then
    begin

      case SI.SearchOperatorType of
        sotAND :
          begin
            inc( TotCount );
            Inc( AndCount );
          end;
        sotOR :
        begin
          inc( TotCount );
          Inc( OrCount );
        end;
        sotXOR : inc( TotCount );
        sotOpen : AnalyzeAndOr( Idx, Idx, SectionStack );
        sotClose :
          begin
            NewPos := Idx;
            break;
          end;
      end;

    end;

    inc( Idx );

  end;//while

  if TotCount > 0 then
  begin
    faryBoolEval[ SectionStack ][ AND_Idx ] := AndCount = TotCount;
    faryBoolEval[ SectionStack ][ OR_Idx ] := OrCount = TotCount;
  end;

end;

procedure TBaseSearch.AddOperator( const Kind : TSearchOperatorType );
var
  Idx : Integer;
  SI : TSearchItem;
begin

  if Kind = sotNone then
    raise EErrorDevelopment.create( 'TBaseSearch.AddOperator: sotNone is invalid for operators.' );

  Idx := SIList.Add( TSearchItem.Create( Self, ftUndefined ) );
  SI := TSearchItem( SIList[ Idx ] );
  SI.SearchOperatorType := Kind;

end;

function TBaseSearch.AddSearchItem( const CmdIdx : integer; const SearchVal : string;
                                     const Cond : TCondition; const CaseSens : boolean ) : integer;
//var
//  Idx : Integer;
begin
  result := SIList.Add( TSearchItem.Create( Self, CmdStruct[ CmdIdx ].FieldType ) );

  TSearchItem( SIList[ result ] ).SearchItem_Init( SearchVal, Cond, CaseSens, CmdStruct[ CmdIdx ].UniqueID );

end;

function TBaseSearch.AddSearchItem( const CmdIdx : TCmdFieldID; const SearchVal : string;
                                     const Cond : TCondition; const CaseSens : boolean ) : integer;
begin
  result := AddSearchItem( Ord( CmdIdx ), SearchVal, Cond, CaseSens );
end;

function TBaseSearch.Evaluate( Idx : integer; const SizeOfList : integer; var NewPosition : integer ) : boolean;
var
  LogicalOper, ArrayIdx: Integer;
  SI : TSearchItem;
  DoNegate, InterimResult, DoContinue, DoRecurse , IsComplete: Boolean;
const
  IsAnd = 0;
  IsOr = 1;
  isXOR = 2;
begin

//Evaluates Parenthese'd sections of a search item recursively

  result := false;
  LogicalOper := -1;//also serves as flag for first evaluation via the case statement
  InterimResult := true;
  DoNegate := false;
  IsComplete := false;

//Opening parenthesis ("(") is always on IDX, tied to it is the index into the array for all AND or all OR
//to prevent exhaustive bool eval when it's not necessary.
  ArrayIdx := TSearchItem( SIList[ Idx ] ).All_AndOr_Idx;

  inc( Idx );//corresponds to a "(" so move to next item to begin processing

  while Idx <= SizeOfList do
  begin

    SI := TSearchItem( SIList[ Idx ] );
    DoContinue := true;
    DoRecurse := false;

    if SI.SIT = sitOperator then
    begin
      DoContinue := false;

      case SI.SearchOperatorType of
        sotNone : raise EErrorDevelopment.create( 'TBaseSearch.Evaluate: Malformed operator SearchItem' );
        sotNOT : DoNegate := true;
        sotAND : LogicalOper := IsAnd;
        sotOR : LogicalOper := IsOR;
        sotXOR : LogicalOper := IsXOR;
        sotOpen :
          begin
            DoContinue := true;
            DoRecurse := true;
          end;
        sotClose :
          begin
            NewPosition := Idx;
            break;
          end;
        else raise EErrorDevelopment.create( 'TBaseSearch.Evaluate: Undefined SearchOperator type' );
      end;// Case

    end; // is sitOperator

    if DoContinue then //MUST always continue, breaking only on ")"
    begin

      if not IsComplete then //BUT if all is evaluated no need to process further, and save cycles on chips
      begin

        if DoRecurse then
          InterimResult := Self.Evaluate( Idx, SizeOfList, Idx ) //each Parenthesis group ( sotOpen, sotClose ) is recursively evaluated
        else InterimResult := SI.Evaluate;

        if DoNegate then
        begin
          InterimResult := not InterimResult;
          DoNegate := false;
        end;

        case LogicalOper of
          IsAnd : result := result and InterimResult;
          IsOr : result := result or InterimResult;
          IsXOR : result := result xor InterimResult;
          else result := InterimResult;
        end;

        //exhaustive search routine, bails out if no need for further evaluation in this parenthese'd section
        if faryBoolEval[ ArrayIdx ][ AND_Idx ] then
          if result = false then
            IsComplete := true;
        if faryBoolEval[ ArrayIdx ][ OR_Idx ] then
          if result = true then
            IsComplete := true;

      end;//if not iscomplete

    end; //if DoContinue

    inc( Idx );

  end;//while

end;


procedure TBaseSearch.Execute( MatchingItems : TStrings );
var
  i, dummyInt : Integer;
  Key : String;
begin

  dummyInt := -1; //suppress compiler message, value not used but need a var variable for first eval call

  if SIList.Count = 0 then //no search items
    exit;

  fDBServer.GetSearchList( fResultDisplayColMain, fResultDisplayColAlt, InfoServerIdx );

  if fDBServer.SearchCount = 0 then
    exit;

  PrePrep;

  AnalyzeAndOr( 0, dummyInt );//Method to bail out early, so non-exhaustive boolean searches makes for
                              //a variable efficiency gain (most? of the time)

  for i := 0 to fDBServer.SearchCount - 1 do
  begin

    Key := fDBServer.GetSearchIdx( i );

//SearchList contains all the keys to lookup the field contents
    if not InitializeSearch( Key ) then
      continue;

//dummyInt: value of new position not needed, we're done, only used inside recursive calls
    if Self.Evaluate( 0, SIList.Count - 1, dummyInt ) then
      MatchingItems.Add( inttostr( i ) );

  end;

  PostPrep;//so control paren's not saved and some cleanup

end;

procedure TBaseSearch.PrePrep;
var
  Idx , i, j: Integer;
  SI , tmpSI: TSearchItem;
begin
  //"(" and ")" control everything in recursion, we add a temporary surrounding to searchItems so that
  //the search is self managing. Temporary in that it is not saved or displayed, and are removed when done.
  SIList.Insert( 0, TSearchItem.Create( Self, ftUndefined ) );
  TSearchItem( SIList[ 0 ] ).SearchOperatorType := sotOpen;

  Idx := SIList.Add( TSearchItem.Create( Self, ftUndefined ) );
  TSearchItem( SIList[ Idx ] ).SearchOperatorType := sotClose;

//this also indicates which fields are re-used and attaches indexes so that value does not
//have to be fetched again.
  Idx := 0;
  for i := 0 to SIList.Count - 1 do
  begin
    SI := TSearchItem( SIList[ i ] );
    if SI.SIT = sitOperator then
      continue;
    if SI.ReUseFetchedIdx > 0 then
      continue;
    inc( Idx );
    SI.ReUseFetchedIdx := Idx;
    for j := i + 1 to SIList.Count - 1 do
    begin
      tmpSI := TSearchItem( SIList[ j ] );
      if tmpSI.SIT = sitOperator then
        continue;
      if tmpSI.siColumnName = SI.siColumnName then
        tmpSI.ReUseFetchedIdx := SI.ReUseFetchedIdx;
    end;

  end;

end;

procedure TBaseSearch.PostPrep;
var
  Idx , i: Integer;
  SI : TSearchItem;
begin
//clean up temporary setup for evaluation
  Idx := SIList.Count - 1;
  if Idx < 1 then
    exit;

//remove surrounding brackets
  TSearchItem( SIList[ Idx ] ).Free;
  SIList.Delete( Idx );

  TSearchItem( SIList[ 0 ] ).Free;
  SIList.Delete( 0 );

//reset data sharing indicators
  for i := 0 to SIList.Count - 1 do
  begin
    SI := TSearchItem( SIList[ i ] );
    if SI.SIT = sitOperator then
      continue;
    SI.ReUseFetchedIdx := 0
  end;

end;

procedure TBaseSearch.SetDBServer( AValue : TInfoServer );
begin
  if fDBServer = AValue then Exit;
  fDBServer := AValue;
end;

{ TSearchItem }


procedure TSearchItem.SetCondition( AValue : TCondition );
begin

  if fCondition = AValue then Exit;
  fCondition := AValue;

end;

procedure TSearchItem.SetAll_AndOr_Idx( AValue : integer );
begin
  if fAll_AndOr_Idx = AValue then Exit;
  fAll_AndOr_Idx := AValue;
end;

procedure TSearchItem.SeTFieldType( AValue : TufFieldType );
begin
  if fFieldType = AValue then Exit;
  fFieldType := AValue;
end;

procedure TSearchItem.SetReUseFetchedIdx( AValue : integer );
begin
  if fReUseFetchedIdx = AValue then Exit;
  fReUseFetchedIdx := AValue;
end;

procedure TSearchItem.SetSearchOperatorType( AValue : TSearchOperatorType );
begin
  if fSearchOperatorType = AValue then Exit;
  fSearchOperatorType := AValue;
end;

procedure TSearchItem.SetSearchValue( AValue : string );
begin
  if fSearchValue = AValue then Exit;
  fSearchValue := AValue;
end;


procedure TSearchItem.SetSIT( AValue : TSearchItemType );
begin
  if fSIT = AValue then Exit;
  fSIT := AValue;
end;

procedure TSearchItem.SetsiUniqueID( AValue : integer );
begin

  if aValue < 0 then
    raise EErrorDevelopment.create( format( cuscapBadUniqueIdx, [ 'TSearchItem.SetsiUniqueID' ] ) );

  if fsiUniqueID = AValue then Exit;

  fsiUniqueID := AValue;

  fCmdFieldID_idx := Get_Cmd_Field_IdxFromUniqueID( fsiUniqueID );

  fValid := fCmdFieldID_idx > cCmdColMissingUniqueID; // > 0

  if fValid then
    fOrigDisplayCaption := format( '%s : %s', [ CmdStruct[ fCmdFieldID_idx ].DisplayCaption,
                                                CmdStruct[ fCmdFieldID_idx ].ColumnName
                                               ] );

end;

procedure TSearchItem.SetCaseSensitive( AValue : boolean );
begin
  if fCaseSensitive = AValue then Exit;
  fCaseSensitive := AValue;
end;

procedure TSearchItem.ClearInvalidMessage;
begin
  if fValid then //i.e., it is a valid field, invalid fields will always warn
    fInvalidMsg := '';
end;


function TSearchItem.IsInvalid : boolean;
begin

//Currently operators are not checked for validity because they should always be valid.
//but I check anyway just in case an operator is checked by accident
  if SIT = sitOperator then
  begin
    result := false;
    exit;
  end;

  result := not fValid
            or ( fInValidMsg <> '' )
            //or ( fCmdFieldID_idx < 0 )
            //or ( fCmdFieldID_idx >= ord( fidENDFORCOUNTING_DoNotUse ) )
            ;

end;

procedure TSearchItem.SetOldFieldName( const aName : string );
begin
  fOrigDisplayCaption := strif( aName <> '', aName, cusIniVal_UnkField );
end;

function TSearchItem.SearchValue_ForDisplay : string;
begin

  if fSIT = sitBoolean then
  begin
    case SearchValue of
      dbsFalseStr : result := cdbsDisplay_False;//'f';
      else result := cdbsDisplay_True;//'t';
    end;
  end
  else
  begin
    if FieldType = ftEnum then
      result := Get_ThreatLevel_Text( strtoint( SearchValue ) )
    else result := SearchValue;
  end;

end;

function TSearchItem.Validate : boolean;
begin

  result := true;

//these errors "should never" happen through the program. The checks are for loaded saved searches,
//where it was hand edited and is invalid or Fields have been Deleted or data used is out of date and/or sync

    case fFieldType of
      ftList : //They are using a KeyWord that no longer exists in the Master KeyWord List and the condition is (NO)MATCH
        begin
          if ( SearchValue <> '' ) and ( Condition in [ coMatchQualified, coMatchQualifiedNot ] ) then
          begin
            //if not ListFields_Linker__ValidItem( siDisplayCaption, SearchValue ) then
            if not ListFields_Linker__ValidItem( siColumnName, SearchValue ) then
            begin
              fInValidMsg := fInValidMsg + format( cusInValidMsgBadListItem, [ SearchValue, siDisplayCaption ] );
              result := false;
            end;
          end;
        end;
//do we have more "invalid" stuff? Write it!
      ftString_Key, ftString,  ftText : ;
      ftBoolean : ;
      ftInteger : ;
      ftEnum : // hmmm this needs to be extended to more generic function
        //if ( SearchValue <> '' ) then
        //begin
        //  Idx := strtoint( SearchValue );
        //  if ( Idx < Low( TThreat)
        //end
      ;
      ftDouble : ;
    end;

end;

function TSearchItem.GetsiColumnName : string;
begin
  case fCmdFieldID_idx of
    -1 : result := cBadColumnName;
    else result := CmdStruct[ fCmdFieldID_idx ].ColumnName;
  end;
end;

function TSearchItem.GetsiDisplayCaption : string;
begin
  case fCmdFieldID_idx of
    -1 : result := cBadColumnName;
     0 : result := fOrigDisplayCaption;
    else result := CmdStruct[ fCmdFieldID_idx ].DisplayCaption;
  end;
end;

function TSearchItem.GetsiDisplayAbbrev : string;
begin
  case fCmdFieldID_idx of
    -1 : result := cBadColumnName;
     0 : result := fOrigDisplayCaption;
    else result := CmdStruct[ fCmdFieldID_idx ].DisplayAbbrev;
  end;
end;

procedure TSearchItem.SetUserTag( AValue : integer );
begin
  if fUserTag = AValue then Exit;
  fUserTag := AValue;
end;

procedure TSearchItem.Save( IFile : TJInifile; const IdxID, Idx : integer );
var
  Sect : String;
begin

  Sect := format( cSearchItemSection, [ IdxID, Idx ] );

  IFile.WriteString( Sect, cusIniVal_FieldType, FieldTypeToStr( FieldType ) );
  IFile.WriteString( Sect, cusIniVal_SOT, SearchOperatorTypeToStr( SearchOperatorType ) );
  IFile.WriteInteger( Sect, cusIniVal_UserTag, fUserTag );
  if SIT = sitOperator then
    exit;//operators don't need the others


  IFile.WriteInteger( Sect, cusIniVal_UID, siUniqueID );
//--->> Dont save these, they are looked up at runtime vis siUniqueid
  //IFile.WriteString( Sect, 'ColName', siColumnName );
  //IFile.WriteString( Sect, 'Caption', siDisplayCaption );
//=========
  IFile.WriteBool( Sect, cusIniVal_CaseSensitive, CaseSensitive );

  IFile.WriteString( Sect, cusIniVal_Condition, ConditionToStr( Condition ) );

  IFile.WriteString( Sect,
                     cusIniVal_SearchValue,
                     stringreplace( SearchValue, LineEnding, csoNonPrintingDelimiter, [ rfreplaceall ] ) );

  IFile.WriteString( Sect, cusIniVal_OrigField, fOrigDisplayCaption );


end;

procedure TSearchItem.Load( IFile : TJInifile; const IdxID, Idx : integer );
var
  Sect , Str: String;
begin

  Sect := format( cSearchItemSection, [ IdxID, Idx ] );

//==>> Read in BaseSearch.Load and sent to SearchItem.Create, don't re-do
  //Str := IFile.ReadString( Sect, cusIniVal_FieldType, '' );
  //if Str <> '' then
  //  FieldType := StrToFieldType( Str );
//=========
  fUserTag := IFile.ReadInteger( Sect, cusIniVal_UserTag, fUserTag );

  Str := IFile.ReadString( Sect, cusIniVal_SOT, '' );
  if Str <> '' then
    SearchOperatorType := StrToSearchOperatorType( Str );

  if SIT = sitOperator then
    exit;//operators don't need the others

  siUniqueID := IFile.ReadInteger( Sect, cusIniVal_UID, -1 );

  CaseSensitive := IFile.ReadBool( Sect, cusIniVal_CaseSensitive, false );

  Str := IFile.ReadString( Sect, cusIniVal_Condition, '' );
  if Str <> '' then
    Condition := StrToCondition( Str );

  SearchValue := stringreplace( IFile.ReadString( Sect, cusIniVal_SearchValue, '*<err>' ),
                                csoNonPrintingDelimiter,
                                LineEnding,
                                [ rfreplaceall ]
                               );

  fOrigDisplayCaption := IFile.ReadString( Sect, cusIniVal_OrigField, cusIniVal_UnkField );

end;


procedure TSearchItem.SearchItem_Init( const pSVal : string; const pCond : TCondition;
                                     const pCaseSens : boolean; const UID : integer );
begin
  SearchValue := pSVal;
  Condition := pCond;
  CaseSensitive := pCaseSens;
  siUniqueID := UID;

  if not fValid then
    fInValidMsg := fInValidMsg + format( cusInValidMsgBadField, [ fOrigDisplayCaption ] );

  if fNeedDefault then
  begin
    SearchValue := GetNewSearchValueDefault;
    fNeedDefault := false;
  end;

  Validate;

end;

procedure TSearchItem.SearchItem_ReInit( const pCmdFieldID : TCmdFieldID; const NewFieldType : TufFieldType );
var
  CurrFieldType : TufFieldType;

  function ConditionIsPresent( const Ary : array of TCondition ) : boolean;
  var
    i : Integer;
  begin
    result := false;
    for i := Low( Ary ) to High( Ary ) do
    begin
      if Condition = Ary[ i ] then
      begin
        result := true;
        break;
      end;
    end;
  end;

begin


  CurrFieldType := FieldType;

  FieldType := NewFieldType;//ftList, ftString, etc;
  fSIT := FieldType_To_SearchItemType( FieldType );

//=============
//analyze values to see if user typed values can still be used, if not then use defaults.
//=============

//determine valid Condition
//TCondition = ( coNone, coRegEx, coEqual, coNotEqual, coMatchQualified, coMatchQualifiedNot,
//               coContains, coContainsNot, coLT, coLTE, coGT, coGTE, coIsEmpty );
  case CurrFieldType of

    ftString_Key, ftString :
      case NewFieldType of
        ftString_Key, ftString : ;
        ftList : if not ConditionIsPresent( cConditions_List ) then
                   Condition := GetNewSearchConditionDefault;
        ftText : if not ConditionIsPresent( cConditions_Text ) then
                   Condition := GetNewSearchConditionDefault;
        ftBoolean : if not ConditionIsPresent( cConditions_Boolean ) then
                   Condition := GetNewSearchConditionDefault;
        ftInteger, ftEnum, ftDouble : if not ConditionIsPresent( cConditions_Number ) then
                   Condition := GetNewSearchConditionDefault;
      end;

    ftList :  //currently don't share any others with numbers or bool
      case NewFieldType of
        ftList : ;
        ftString_Key, ftString : if not ConditionIsPresent( cConditions_String ) then
                   Condition := GetNewSearchConditionDefault;
        ftText : if not ConditionIsPresent( cConditions_Text ) then
                   Condition := GetNewSearchConditionDefault;
        else Condition := GetNewSearchConditionDefault;
      end;

    ftText :  //currently don't share any others with numbers or bool
      case NewFieldType of
        ftText : ;
        ftString_Key, ftString : if not ConditionIsPresent( cConditions_String ) then
                   Condition := GetNewSearchConditionDefault;
        ftList : if not ConditionIsPresent( cConditions_List ) then
                   Condition := GetNewSearchConditionDefault;
        else Condition := GetNewSearchConditionDefault;
      end;

    ftBoolean :
      case NewFieldType of
        ftBoolean, ftString_Key, ftString, ftInteger, ftEnum, ftDouble : ;
        else Condition := GetNewSearchConditionDefault;
      end;

    ftInteger, ftEnum, ftDouble :
      case NewFieldType of
        ftInteger, ftEnum, ftDouble : ;
        ftString_Key, ftString : if not ConditionIsPresent( cConditions_String ) then
                   Condition := GetNewSearchConditionDefault;
        ftBoolean : if not ConditionIsPresent( cConditions_Boolean ) then
                   Condition := GetNewSearchConditionDefault;
        else Condition := GetNewSearchConditionDefault;
      end;

  end; //case CurrFieldType


//determine valid SearchValue
//TufFieldType = ( ftUndefined, ftString_Key, ftString, ftBoolean, ftInteger, ftList, ftEnum, ftDouble, ftText );
  case CurrFieldType of

    ftString_Key, ftString :
      case NewFieldType of
        ftList :
          begin
            if Condition in [ coMatchQualified, coMatchQualifiedNot ] then
              fNeedDefault := true;
          end;
        ftBoolean, ftInteger, ftDouble, ftEnum : fNeedDefault := true;
      end;

    ftList :
      case NewFieldType of
        ftList, ftString_Key, ftString, ftText  : ;
        else fNeedDefault := true;
      end;

    ftText :
    begin
      SearchValue := stringreplace( SearchValue, LineEnding, '', [ rfreplaceall ] );
      case NewFieldType of
        ftList :
            if Condition in [ coMatchQualified, coMatchQualifiedNot ] then
              fNeedDefault := true;
        ftString_Key, ftString : ;
        else fNeedDefault := true;
      end;
    end;

    ftBoolean :
      case NewFieldType of
        ftBoolean : ;
        else fNeedDefault := true;
      end;

    ftInteger, ftEnum :
      case NewFieldType of
        ftInteger, ftEnum : ;
        else fNeedDefault := true;
      end;

    ftDouble :
      case NewFieldType of
        ftDouble : ;
        else fNeedDefault := true;
      end;

  end; //case CurrFieldType



//determine valid CaseSensitivie
  case CurrFieldType of

    ftString_Key, ftString, ftList, ftText :
      case NewFieldType of
        ftString_Key, ftString, ftList, ftText : ;
        else CaseSensitive := false;
      end;

    else CaseSensitive := false;

  end;


  SearchItem_Init( SearchValue,
                   Condition,
                   CaseSensitive,
                   Get_Cmd_Field_UniqueID( pCmdFieldID )
                  );
end;


procedure TSearchItem.Operator_Init( pSOT : TSearchOperatorType = sotOR );
begin
  SearchOperatorType := pSOT;
end;

function TSearchItem.GetSearchOperatorCaption : string;
begin
  result := SearchOperatorTypeToStr( SearchOperatorType, 1 );
end;

function TSearchItem.GetNewSearchValueDefault : string;
begin
  case FieldType of
    ftString_Key, ftString, ftText : result := '???';
    //ftList : result := ListFields_Linker__GetFirst_List_Item( siDisplayCaption );
    ftList : result := ListFields_Linker__GetFirst_List_Item( siColumnName );
    ftBoolean : result := dbsTrueStr;//'Troo';
    ftInteger, ftEnum : result := '0';
    ftDouble : result := '0.0';
    else result := '<unkown>';
  end;
end;

function TSearchItem.GetNewSearchConditionDefault : TCondition;
begin
  case FieldType of
    ftList : result := coMatchQualified;
    ftText : result := coContains;
    else result := coEqual;
  end;
end;


constructor TSearchItem.Create( aBS : TBaseSearch; const aFieldType : TufFieldType );
begin

  inherited Create;

  BS := aBS;

  FieldType := aFieldType;//ftList, ftString, etc;

  fSIT := FieldType_To_SearchItemType( FieldType );

  fValid := false;
  fCondition := coEqual;
  fsiUniqueID := -1;
  fCmdFieldID_Idx := -1;
  fReUseFetchedIdx := 0;
  fCompare_To := 0;
  fSearchValue := '';
  fCaseSensitive := false;
  fSearchOperatorType := sotNone;
  fAll_AndOr_Idx := -1;
  fUserTag := 0;
  //No!! ==>  fOrigDisplayCaption := ''; Do Not set here!
  fInValidMsg := '';
  fNeedDefault := false;
end;

destructor TSearchItem.Destroy;
begin
  inherited Destroy;
end;

function TSearchItem.HandleString : boolean;
begin

  case Condition of
    coRegEx :
      begin
        BS.RegX.Expression := SearchValue;//'\bsu(do)?\s';
        Result := TCOFieldString.ReqExpr_Handler( BS.RegX, fCompare_To );
      end;
    coEqual : Result := TCOFieldString.Equals_Handler( SearchValue, fCompare_To, true, CaseSensitive );
    coNotEqual : Result := TCOFieldString.Equals_Handler( SearchValue, fCompare_To, false, CaseSensitive );
    //coMatchQualified : ; handled with Lists
    //coMatchQualifiedNot : ; handled with Lists
    coContains : Result := TCOFieldString.Contains_Handler( SearchValue, fCompare_To, true, CaseSensitive );
    coContainsNot : Result := TCOFieldString.Contains_Handler( SearchValue, fCompare_To, false, CaseSensitive );
    coIsEmpty : result := TCOFieldString.IsEmpty( fCompare_To );
    else result := false;
  end;

end;

function TSearchItem.HandleText : boolean;
begin

  case Condition of
    //coRegEx : ; handled with String
    //coEqual : ; handled with String and doesn't apply to Lists
    //coNotEqual : ; handled with String and doesn't apply to Lists
    //coMatchQualified : handled with String and doesn't apply to Lists
    //coMatchQualifiedNot : handled with String and doesn't apply to Lists
    //coIsEmpty : ; handled with String//result := TCOFieldDerivedList.IsEmpty( fCompare_To );
    coContains : result := TCOFieldString.Contains_Handler_Text( SearchValue, fCompare_To, true, CaseSensitive );
    coContainsNot : result := TCOFieldString.Contains_Handler_Text( SearchValue, fCompare_To, false, CaseSensitive );
    else result := HandleString;
  end;

end;

function TSearchItem.HandleList : boolean;
begin

  case Condition of
    //coRegEx : ; handled with String
    //coEqual : ; handled with String and doesn't apply to Lists
    //coNotEqual : ; handled with String and doesn't apply to Lists
    coMatchQualified : result := TCOFieldDerivedList.Match( SearchValue, fCompare_To, true );
    coMatchQualifiedNot : result := TCOFieldDerivedList.Match( SearchValue, fCompare_To, false );
    //coIsEmpty : ; handled with String//result := TCOFieldDerivedList.IsEmpty( fCompare_To );
    //coContains : ; handled with String
    //coContainsNot : ; handled with String
    else result := HandleString;
  end;

end;

function TSearchItem.HandleBoolean( const Compare : boolean ) : boolean;
begin

  case Condition of
    coEqual : result := Str_To_Bool( SearchValue ) = Compare;
    coNotEqual : result := Str_To_Bool( SearchValue ) <> Compare;
    else result := false;
  end;

end;

function TSearchItem.HandleInteger( const Compare : integer ) : boolean;
var
  i : integer;//LongInt;
begin

  i := strtoint( SearchValue );

  case Condition of
    coEqual : result := Compare = i;
    coNotEqual : result := Compare <> i;
    coLT : result := Compare < i;
    coLTE : result := Compare <= i;
    coGT : result := Compare > i;
    coGTE : result := Compare >= i;
    else result := false;
  end;

end;

function TSearchItem.HandleDouble( const Compare : double ) : boolean;
var
  d : Double;
begin

  d := StrToFloat( SearchValue );

  case Condition of
    coEqual : result := Compare = d;
    coNotEqual : result := Compare <> d;
    coLT : result := Compare < d;
    coLTE : result := Compare <= d;
    coGT : result := Compare > d;
    coGTE : result := Compare >= d;
    else result := false;
  end;

end;


function TSearchItem.Evaluate : boolean;
begin

  result := false;

  case FieldType of
    ftString_Key, ftString : result := HandleString;
    ftText : result := HandleText;
    ftList : result := HandleList;
//sending compare_to as param automically casts the variant
    ftBoolean : result := HandleBoolean( fCompare_To );
    ftInteger, ftEnum : result := HandleInteger( fCompare_To );
    ftDouble : result := HandleDouble( fCompare_To );
    else raise EErrorDevelopment.create( 'TSearchItem.Evaluate: invalid field type' );
  end;

end;

function SearchItemTypeToStr( const SIT : TSearchItemType ) : string;
begin
  case SIT of
    sitOperator : result := 'Oper';
    sitString : result := 'Str';
    sitBoolean : result := 'Bool';
    sitInteger : result := 'Int';
    sitDouble : result := 'Real';
    else result := 'Invalid';
  end;
end;

function StrToSearchItemType( const SIT : string ) : TSearchItemType;
begin
  case uppercase( SIT ) of
    'OPER' : result := sitOperator;
    'STR' : result := sitString;
    'BOOL' : result := sitBoolean;
    'INT' : result := sitInteger;
    'REAL' : result := sitDouble;
    //else raise EErrorDevelopment.create( format( 'StrToSearchItemType: "%s" is invalid.', [ SIT ] ) );
  end;
end;

function SearchOperatorTypeToStr( const SOT : TSearchOperatorType; DoFormat : shortint ) : string;
begin
  case SOT of
    sotNone : result := 'None';
    sotNOT : result := 'Not';
    sotAND : result := 'And';
    sotOR : result := 'Or';
    sotXOR : result := 'XOr';
    sotOpen : result := '(';//'Open';
    sotClose : result := ')';//'Close';
    else result := 'Invalid';
  end;
  case DoFormat of
    -1 : result := lowercase( result );
    1 : result := uppercase( result );
  end;
end;

function StrToSearchOperatorType( const SOT : string ) : TSearchOperatorType;
begin
  case uppercase( SOT ) of
    'NONE' : result := sotNone;
    'NOT' : result := sotNOT;
    'AND' : result := sotAND;
    'OR' : result := sotOR;
    'XOR' : result := sotXOR;
    'OPEN', '(' : result := sotOpen;
    'CLOSE', ')' : result := sotClose;
    //else raise EErrorDevelopment.create( format( 'StrToSearchOperatorType: "%s" is invalid.', [ SOT ] ) );
  end;

end;

function ConditionToStr( const CO : TCondition ) : string;
begin
//used in saving to file, don't change
  case CO of
    coNone : result := 'NONE';
    coRegEx : result := 'RE';
    coEqual : result := '=';
    coNotEqual : result := '<>';
    coMatchQualified : result := 'M=';
    coMatchQualifiedNot : result := 'M<>';
    coContains : result := '*';
    coContainsNot : result := '~*';
    coLT : result := 'LT';
    coLTE : result := 'LTE';
    coGT : result := 'GT';
    coGTE : result := 'GTE';
    coIsEmpty : result := '%';
    else result := 'INVALID';
  end;
end;

function StrToCondition( const CO : string ) : TCondition;
begin
//used in reading a file, don't change
  case uppercase( CO ) of
    'NONE' : result := coNone;
    'RE' : result := coRegEx;
    '=' : result := coEqual;
    '<>' : result := coNotEqual;
    'M=' : result := coMatchQualified;
    'M<>' : result := coMatchQualifiedNot;
    '*' : result := coContains;
    '~*' : result := coContainsNot;
    'LT' : result := coLT;
    'LTE' : result := coLTE;
    'GT' : result := coGT;
    'GTE' : result := coGTE;
    '%' : result := coIsEmpty;
    //else raise EErrorDevelopment.create( format( 'StrToCondition: "%s" is invalid.', [ CO ] ) );
  end;
end;

function ConditionToStr_Display( const CO : TCondition ) : string;
begin
//used in saving to file, don't change
  case CO of
    coRegEx : result := cStrCondDisplayRegEx;
    coEqual, coMatchQualified : result := cStrCondDisplayEqual;
    coNotEqual, coMatchQualifiedNot : result := cStrCondDisplayNotEqual;
    coContains : result := cStrCondDisplayContains;
    coContainsNot : result := cStrCondDisplayNotContains;
    coLT : result := cStrCondDisplayLT;
    coLTE : result := cStrCondDisplayLTE;
    coGT : result := cStrCondDisplayGT;
    coGTE : result := cStrCondDisplayGTE;
    coIsEmpty : result := cStrCondDisplayIsEmpty;
    else result := cStrCondDisplayErr;
  end;
end;

function FieldType_To_SearchItemType( const FT : TufFieldType ) : TSearchItemType;
begin
  case FT of
    ftUndefined : result := sitOperator;
    ftString_Key, ftString, ftList, ftText : result := sitString;
    ftBoolean : result := sitBoolean;
    ftInteger,  ftEnum : result := sitInteger;
    ftDouble : result := sitDouble;
    else raise EErrorDevelopment.create( 'FieldType_To_SearchItemType: invalid FieldType' );
  end;
end;

function GetFieldTypeforOperator : TufFieldType;
begin
  result := ftUndefined;
end;


end.

