unit unitcommands;

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
  Classes
  , SysUtils
  , process
  , AsyncProcess
  , linuxtrix
  , regexpr //this is the Regexpression unit
  , unitfields
  , unitDBStructure
  , unitDBConstants
  , unitsharedobj
  , unitSearch
  , unitGlob
  //, StdCtrls
  ;



const

  ciniSubSeparator = '//';
  ciniSub2Str = '%s' + ciniSubSeparator + '%s';//'%s//%s';
  ciniSub3Str = ciniSub2Str + ciniSubSeparator + '%s';//'%s//%s//%s';
  ciniDerivedListBookends = #27#27;//'|';


  cReservedDel = '<DEL>  ';
  cReservedUpdate = '<UPDATE>  ';
  cReservedAdd = '<ADD>  ';
  cReservedBlankCommand = '<EMPTY>';

  cReservedSuperUser = '<ROOT> ';


type

  TCmdObjHelper = class;

  { TBaseCmdObj }

  TBaseCmdObj = class(TObject)
  private
    //runtime
    fDoDelete: boolean;
    fIniSection: string;
    fIsNew: boolean;
    fSaveFieldNameSL: TStringList;
    fSaveFieldValueSL: TStringList;
    fTextDBKeyField : TCOFieldString_KeyField;//pointer only do not create

    function SaveDB : integer;
    procedure SaveINI;
    procedure SetDoDelete(AValue: boolean);
    procedure SetIniSection(AValue: string); virtual; abstract;
    procedure SetIsNew(AValue: boolean);

    //DB

  private
    fFieldRowID: TCOFieldInteger;
    procedure SetRowID( AValue: integer );
    function GetRowID: integer;

  private
    fFieldSuperUserMode: TCOFieldBoolean;
    procedure SetSuperUserMode(AValue: boolean);
    function GetSuperUserMode: boolean;

  private
    fFieldDetachProcess: TCOFieldBoolean;
    procedure SetDetachProcess(AValue: boolean);
    function GetDetachProcess: boolean;

  private
    fFieldNotes: TCOFieldString;
    procedure SetNotes(AValue: string);
    function GetNotes: string;

  private
    fFieldThreatLevel: TCOFieldEnum; //ENUM Holds the ORD (integer) value of TThreatLevel
    procedure SetThreatLevel(AValue: TThreatLevel);
    function GetThreatLevel: TThreatLevel;

  private
    fFieldTerminalOnly: TCOFieldBoolean;
    procedure SetTerminalOnly(AValue: boolean);
    function GetTerminalOnly: boolean;

  private
    fDBServer : TInfoServer;
    fFieldIsFavorite: TCOFieldBoolean;
    procedure SetDBServer( AValue : TInfoServer );
    procedure SetIsFavorite(AValue: boolean);
    function GetIsFavorite: boolean;

    procedure SetSaveFieldNameSL( AValue : TStringList );
    procedure SetSaveFieldValueSL( AValue : TStringList );

  protected
    fDataLocation : TDataLocation;
    fFields: TList;
    fCmdObjHelper : TCmdObjHelper;
    function CreateField( var Obj : TObject; const DBS : TDBCmdStructure) : string;

  public
    constructor Create( DBS : TInfoServer ); virtual;
    destructor Destroy; override;

    function Save : boolean; virtual;
    function DoUpdate: boolean; virtual;
    procedure ResetUpdate; virtual;
    function GetDisplayString: string; virtual;
    function BaseMerge( FromCmdObj : TBaseCmdObj; const MergeSource : string; var Info : string ) : boolean;

    //DB
    property IsFavorite: boolean read GetIsFavorite write SetIsFavorite;//boolean;
    property Notes: string read GetNotes write SetNotes;

    property DetachProcess: boolean read GetDetachProcess write SetDetachProcess;
    property SuperUserMode: boolean read GetSuperUserMode write SetSuperUserMode;
    property TerminalOnly: boolean read GetTerminalOnly write SetTerminalOnly;
    //enums
    property ThreatLevel: TThreatLevel read getThreatLevel write SetThreatLevel;

    //runtime
    property RowID: integer read GetRowID write SetRowID;//read but not written, sqlite rowid methods
    property IsNew: boolean read fIsNew write SetIsNew;
    property DoDelete: boolean read fDoDelete write SetDoDelete;
    property DataLocation: TDataLocation read fDataLocation;
    property IniSection: string read fIniSection write SetIniSection;
    property SaveFieldNameSL : TStringList read fSaveFieldNameSL write SetSaveFieldNameSL;
    property SaveFieldValueSL : TStringList read fSaveFieldValueSL write SetSaveFieldValueSL;
    property DBServer : TInfoServer read fDBServer write SetDBServer;

  end;

  { TCmdLineObj }

  TCmdLineObj = class(TBaseCmdObj)

  private

    fFieldCmdFK: TCOFieldInteger;
    procedure SetCmdFK( AValue: integer );
    function GetCmdFK: integer;

  private

    fFieldSO: TCOFieldInteger;
    procedure SetSO( AValue: integer );
    function GetSO: integer;

  private
    fFieldEntry: TCOFieldString;
    procedure SetEntry(AValue: string);
    function GetEntry: string;

  private
    fFieldFriendlyName: TCOFieldString;
    procedure SetFriendlyName(AValue: string);
    function GetFriendlyName: string;

  private
    fFieldUseShell: TCOFieldBoolean;
    procedure SetUseShell(AValue: boolean);
    function GetUseShell: boolean;

  private
    fFieldWantsInput: TCOFieldBoolean;
    procedure SetWantsInput(AValue: boolean);
    function GetWantsInput: boolean;

  private
    fFieldAlert: TCOFieldBoolean;
    procedure SetAlert(AValue: boolean);
    function GetAlert: boolean;

    procedure LoadDB( Idx : integer );
    procedure LoadINI(Key: string);

    procedure SetIniSection(AValue: string); override;

  protected
    //Relation or foreign key into parent command object for ini files
    fRelationSection: string;
  public
    constructor Create( DBS: TInfoServer; theIniSection: string; iSO : integer ); reintroduce;
    destructor Destroy; override;

    function Save( const OrderIdx : integer ) : boolean; reintroduce;
    procedure Load( const Key : string; const Idx : integer );
    procedure Copy( aCmdLineObj : TCmdLineObj );
    function Merge( FromCmdObj : TCmdLineObj; const MergeSource : string ) : boolean;
    procedure Assign( aCmdLineObj: TCmdLineObj );
    function DoUpdate: boolean; override;
    procedure MarkFieldsForUpdate;
    procedure ResetUpdate; override;
    procedure SetRelationSection(aSectionName: string);
    function GetDisplayString: string; override;
    function sqlGetCommandName : string;
    //DB

    property CmdFK: integer read GetCmdFK write SetCmdFK;
    property SO: integer read GetSO write SetSO;
    property Entry: string read GetEntry write SetEntry;

    property FriendlyName: string read GetFriendlyName write SetFriendlyName;

    property WantsInput: boolean read GetWantsInput write SetWantsInput;

    property UseShell: boolean read GetUseShell write SetUseShell;

    property Alert: boolean read GetAlert write SetAlert;

    //runtime

  end;

  { TCmdObj }

  TCmdObj = class(TBaseCmdObj)
  private
    fTextDBSectionID : string;
    fCmdLines: TStringList;
    fLoadedCommandName: string;

    fFieldCommandName: TCOFieldString_KeyField;
    procedure SetCommandName(AValue: string);
    function GetCommandName: string;

  private
    fFieldKeywords: TCOFieldDerivedList;
    procedure SetKeywords(AValue: string);
    function GetKeywords: string;

  private
    fFieldHelpCommand: TCOFieldString;
    procedure SetHelpCommand(AValue: string);
    function GetHelpCommand: string;

  private
    fFieldLocationPath: TCOFieldString;
    procedure SetLocationPath(AValue: string);
    function GetLocationPath: string;

  private
    fFieldVersionCommand: TCOFieldString;
    procedure SetVersionCommand(AValue: string);
    function GetVersionCommand: string;

    procedure ClearCmdLinesOutofIniFile( const TheName: string );
    function DoClearCmdLines: boolean;
    procedure FreeCmdLines;
    procedure LoadDB( const Key, Col : string; const Idx : integer );
    procedure LoadINI( const Key: string );
    procedure PrepCommandLinesForReWrite;
    procedure SaveCmdLineINI;
    procedure SaveCommandLines;
    function IsSystemPath : boolean;
    function MergeKeyWords( FromCmdObj : TCmdObj ) : boolean;
    function Merge_CmdLines( FromCmdObj : TCmdObj; const MergeSource : string ) : boolean;
    function Compare_CmdLines( FromCmdObj : TCmdObj; var Identical : boolean ) : string;


    procedure SetIniSection(AValue: string); override;
    procedure SetCmdLines(AValue: TStringList);

  protected
    fCmdLineDataLocation : TDataLocation;
    procedure SaveDB;
    procedure SaveIni;
  public
    constructor Create( DBS : TInfoServer ); override;
    destructor Destroy; override;

    class function MergeCommands( SourceDB, DestDB : TInfoServer; const MergeSource : string ) : boolean;
    class function MergeCommand( DestDB : TInfoServer; CO : TCmdObj; const MergeSource : string ) : string;
    class function ConvertCommands( SourceDB, DestDB : TInfoServer ) : boolean;
    class function CompareCommands( SourceDB, DestDB : TInfoServer; Strings : TStrings;
                                    const frName, toName : string ) : boolean;

    function Save : boolean; override;
    procedure Load( const Key, Col : string; const Idx : integer );
    function RemoveData : boolean;
    procedure Copy(aCmdObj : TCmdObj; const NewName : string );
    function Merge( FromCmdObj : TCmdObj; const MergeSource : string) : boolean;
    function Compare( FromCmdObj : TCmdObj; var Identical : boolean ) : string;
    //procedure Assign( aCmdObj : TCmdObj );
    function AsText : string;
    function AddCommandLine( const anEntry: string ): integer;
    function ReorderCmdLine( const idx, aFactor: integer ): integer;
    function GetDisplayString: string; override;
    procedure CmdLineDelete( const Idx: integer; const DeleteState: boolean );
    procedure UpdateCmdLinesDisplay( aCmdLineObj: TCmdLineObj );
    function GetLoadedName: string;

    function GetLiteralPath : string;

    function DoUpdate: boolean; override;
    procedure ResetUpdate; override;
    procedure ResetDBServer( DBS : TInfoServer; ForceSave : boolean = false );
    //DB
    property CommandName: string read GetCommandName write SetCommandName;
    property Keywords: string read GetKeywords write SetKeywords;//TStringlist;
    property HelpCommand: string read GetHelpCommand write SetHelpCommand;//string;
    property VersionCommand: string read GetVersionCommand write SetVersionCommand;//string;
    property LocationPath: string read GetLocationPath write SetLocationPath;//string;

    //runtime
    property CmdLines: TStringList read fCmdLines write SetCmdLines;

  end;


  TDBListType = ( dbltKeyWord );
  { TCmdListObj }

  TCmdListObj = class(TObject)
  private
    fDBServer : TInfoServer;// pointer only
    fColumnName : string;
    fIniSection: string;
    fSelfDataLocation: TDataLocation;
    fTargetDataLocation : TDataLocation;
    fTargetFieldID : TCmdFieldID;
    fIsDirty: boolean;
    fListItemsSL: TStringList;
    fChangesSL: TStringList;
    fInternalIdx: integer;
    function Find( const anEntry: string ): boolean;
    procedure SetColumnName( AValue : string );
    procedure SetDBServer( AValue : TInfoServer );
    procedure SetTargetDataLocation( AValue : TDataLocation );
    procedure DBUpdateTarget( sqlSL : TStrings );
    function ApplyChangesRawDBData( const OrigEntry : string; var EntryResult : string ) : boolean;
    function ApplyChangesObjectData( const OrigEntry : string; var EntryResult : string ) : boolean;

  protected
    procedure Load;
    procedure Save( sqlSL : TStrings );

  public
    constructor Create( DL, LinkTo : TDataLocation; LinkToFID : TCmdFieldID;
                                                const anIniSection, aColumname : string; DBS : TInfoServer );
    destructor Destroy; override;

    class function CreateListStructure( var CLO : TCmdListObj;
                             const ListID : TDBListType; DBS : TInfoServer; DoReg : boolean = true) : boolean;
    class function MergeLists( const ListID : TDBListType; SourceDB, DestDB : TInfoServer ) : boolean;
    class function MergeListsOne( const ListID : TDBListType; DestDB : TInfoServer; CO : TCmdObj ) : boolean;
    class function CompareLists( const ListID : TDBListType; SourceDB, DestDB : TInfoServer;
                                 Strings : TStrings; const frName, toName : string ) : boolean;
    class function ConvertLists( const ListID : TDBListType; SourceDB, DestDB : TInfoServer ) : boolean;
    class function IsInListAndEmpty( const ListIdx : string ) : boolean;
    class procedure ClearListStructures( theLists : array of TCmdListObj );

    procedure Save_Direct;
    function Update( const OldEntry, NewEntry: string ): integer;
    function Delete( Entries : string ) : integer;
    function Add( const anEntry : string ) : integer;
    function Consolidate( const OldEntry, NewEntry: string ): integer;
    procedure Revert;
    function ApplyUpdateList( const LT : TDBListType; CmdSL : TStrings; ShowingCmd : Integer = - 1 ) : boolean;
    function Get_Search_List_Text( const ToSearch : string; TCS : integer ) : string;
    function NeedsDBUpdate: boolean;
    property ListItemsSL: TStringList read fListItemsSL;
    property ChangesSL: TStringList read fChangesSL;
    property ColumnName : string read fColumnName write SetColumnName;
    property TargetDataLocation : TDataLocation read fTargetDataLocation write SetTargetDataLocation;
    property DBServer : TInfoServer read fDBServer write SetDBServer;
    //runtime

  end;


  { TCmdObjHelper }

  TCmdObjHelper = class(TObject)
  private
    fCmd_ControlColumn_sql : string;
    fCmdLine_ControlColumn_sql : string;
    fDBServer : TInfoServer;//Pointer only
    ftheRegX : TRegExpr;
    procedure GetCmdDisplayObjList( Source, Dest : TStringList; IsCmd : boolean = true );
    procedure GetCmdDisplayObjList_Cmd( Source, Dest : TStringList; DL : TDataLocation );
    procedure GetCmdDisplayObjList_CmdLine( Source, Dest : TStringList; DL : TDataLocation );
    function GetCmdDisplayObj_CmdLineEntry( const Main, Alt : string ) : string;
    procedure ProcessCommandEntry( aString : string; strings : TStrings );
    procedure SetDBServer( AValue : TInfoServer );

    function VariablesAsClipboardText( anEntry : string ) : string;

    function GetCmd_ControlColumn_sql : string;
    function GetCmdLine_ControlColumn_sql : string;

  protected

  public
    constructor Create;
    destructor Destroy; override;
    procedure GetCmdList( anSL : TStrings );
    function HasRealPath( const PathStr : string ) : boolean;
    function GetNormalizedPath( const PathAlias, CommandName : string ) : string;
    function CanRunCommand( ExecuteStr : string ) : boolean;
    function RunCommand( Params : TStrings; UseShell : boolean; ShStr : string = '' ) : string;
    function RunCommandDetached( var aProc : TAsyncProcess; Params : Tstrings) : string;// overload;
    procedure ProcessFileNamePath(const aComName: string; var aComFile: string; var aComPath: string);
    function RouteCommand( aString : string; const DoDetach : boolean; var aProcess : TAsyncProcess ) : string;

    function CleanCommandsEntry( aString : string) : string;
    function InitializeAndNormalizeCommandsEntry( aString : string; var IsPiped : boolean; WantsInput : boolean) : string;

    function ThreatLevelToListIdx( const TL : TThreatLevel ) : integer;
    function ListIdxToThreatLevel( const Idx : Integer ) : TThreatLevel;
    function GetThreatLevelText( const TL : TThreatLevel ) : string;
    function Search( aSearch : TSearchObj; Results : TStrings) : integer;
    function GetCmdLine_DisplayObjects( Source, Dest : TStringList ) : integer;
    procedure GetCmdDisplayObjList_CmdLine_External( CO : TCmdObj; Dest : TStringList; Friendly : boolean = true );

    procedure CreateCmdObj( var aCO : TCmdObj );
    procedure LoadCmdObj( var aCO : TCmdObj; const Key : string );
    procedure LoadCmdLineObj( var aCLO : TCmdLineObj; const CommandName, Section : string;
                              iSO : integer = cFieldIntegerTypeDefault );


    function ClipBoardVersion( const Str, RootFile : string ) : string;
    function GetVariableAndPosition( const aString : string; var VarName : string ) : integer;
    function GetSignificantVariableFlag( const aString : string ) : string;
    function GetVariableHelp( const VariableName : string; DefaultResult : string = '' ) : string;
    function GetVariableReplacements( var aString : string) : boolean;
    function GetProcessInput : boolean;

    property Cmd_ControlColumn_sql : string read GetCmd_ControlColumn_sql;
    property CmdLine_ControlColumn_sql : string read GetCmdLine_ControlColumn_sql;
    property DBServer : TInfoServer read fDBServer write SetDBServer;

  end;

  { TCmdSL }

  TCmdSL = class(TObject)
  private
    fSL : TStringlist;
    procedure SetSL( AValue : TStringlist );

  protected

  public
    constructor Create;
    destructor Destroy; override;
    property SL: TStringlist read fSL write SetSL;

  end;

  { TCmdDisplayObj }

  TCmdDisplayObj = class(TObject)
  private
    fCommandName : string;
    fDetailIdx : integer;
    fEntry : string;
    fDL : TDataLocation;
    fIsCommandLine : boolean;
    fSectTab : string;
    procedure SetCommandName( AValue : string );
    procedure SetDetailIdx( AValue : integer );
    procedure SetEntry( AValue : string );
    procedure SetDL( AValue : TDataLocation );
    procedure SetIsCommandLine( AValue : boolean );
    procedure SetSectTab( AValue : string );

  protected

  public
    constructor Create;
    destructor Destroy; override;

    property Entry : string read fEntry write SetEntry;
    property CommandName : string read fCommandName write SetCommandName;
    property SectTab : string read fSectTab write SetSectTab;
    property DL : TDataLocation read fDL write SetDL;
    property IsCommandLine : boolean read fIsCommandLine write SetIsCommandLine;
    property DetailIdx : integer read fDetailIdx write SetDetailIdx;

  end;

  { TCmdRec }

  TCmdRec = class( TObject )
  private
    fCmd : TCmdObj;
    fCLIndex : integer;
    procedure SetCLIndex( AValue : integer );
    procedure SetCmd( AValue : TCmdObj );
  public
    constructor Create;
    destructor Destroy; override;
    property Cmd : TCmdObj read fCmd write SetCmd;
    property CLIndex : integer read fCLIndex write SetCLIndex;
  end;


var
  CmdObjHelper : TCmdObjHelper;

const
  cucoVariableString = '<<S>>';
  cucoVariableInteger = '<<I>>';
  cucoVariableNumber = '<<N>>';
  cucoVariableFile = '<<F>>';
  cucoVariableRegExpr = '<<[nNsSiIfF]>>';
  cucoVariableSize = length( cucoVariableString );
  cucoSignificantVariableStart = 3;
  cucoSignificantVariableLength = 1;
  //cucoDoSep = '//SearchSEPARATOR//';


resourcestring
  cucoVariableStringClipText = '||TEXT_HERE||';
  cucoVariableIntegerClipText = '||INTEGER_HERE||';
  cucoVariableNumberClipText = '||REAL_NUMBER_HERE||';
  cucoVariableFileClipText = '||FILE_OR_FOLDER_HERE||';
  cucoMerge_String = 'FROM String %s was "%s"' + LineEnding;
  cucoMerge_Boolean = 'FROM boolean %s was different' + LineEnding;
  cucoMerge_Text = '%s: Text merged from %s'
                   + LineEnding + LineEnding;
  cucoMerge_NewObj = '%s: merged from %s'
                   + LineEnding + LineEnding;
  cucoMerge_Text_Separator = LineEnding + '== Original Text ==' + LineEnding;
  cucoMerge_Inconsistency = '%s: Merge Inconsistencies: '
                            + LineEnding
                            + '%s'
                            + LineEnding;


  cmsgcleBadCommand = 'Bad command!';
  cmsgucoVariableString =
      'Put in a string (i.e., AlphaNumeric)'
      + LineEnding + LineEnding
      + 'This basically means anything can go in here, what it should '
      + 'be depends entirely on what the command flag wants and the '
      + 'result you are looking for. '
     ;
  cmsgucoVariableInteger =
      'Put in an integer (i.e., a number like 1, 2, 3...)'
      + LineEnding + LineEnding
      + 'The variable indicates that an integer should go here, but sometimes '
      + 'more is allowed. Oftentimes this can be a size, and you can simply '
      + 'type the size directly like: 1024000000 '
      + LineEnding + LineEnding
      + 'But in cases where it is a file size for example, suffixes are allowed. '
      + 'For example: 1024M is equivalent to the above size. '
      + LineEnding + LineEnding
      + 'This will ALWAYS depend on the command and the flag, so do your research '
      + 'but, for your reference, here are common allowed suffixes for file sizes: '
      + LineEnding + LineEnding
      + 'kB = 1024   K = 1000 ( 2K = 2000 bytes ) '
      + LineEnding
      + 'mB = 1024000   M = 1000000 ( 2mB = 2.048 Megabytes ) '
      + LineEnding
      + 'mG = 1024000000   G = 1000000000 ( 3G = 3 Gigabytes ) '
      + LineEnding
      + 'mix and match. 4096M = 4 Gigabytes '
      + LineEnding
     ;
  cmsgucoVariableNumber =
      'Put in a float value (i.e., 1.34, 2.8, 3.14159...) '
     ;
  cmsgucoVariableFilename =
      'File and Folder names are automatically quoted for you, as is necessary for file/folder names '
      + 'that have spaces in them. Therefore, there is no need to put quotes around the <<F>> variable. '
      + 'In fact that will cause failure. So make sure the command line has simply <<F>> ==NOT== ''<<F>>'' '
      + LineEnding + LineEnding
      + 'Put in a file or folder (depending on what the flag '
      + 'wants) like: '
      + LineEnding + LineEnding
      + '/home/Suzy/myfile.txt '
      + LineEnding
      + '~/myfile.txt '
      + LineEnding
      + '~/ '
      + LineEnding
      + '/bin '
      + LineEnding
      + 'And so on and so forth.. '
      + LineEnding + LineEnding
     ;

  cmsgucoVariableInvalid = 'Invalid variable "%s".';
  cmsgucoVariableInvalidHelp =
      LineEnding + LineEnding
      + 'Invalid variable "%s". Valid are: '
      + LineEnding + LineEnding
      + cucoVariableString + ' (string)' + LineEnding
      + cucoVariableInteger + ' (integer)' + LineEnding
      + cucoVariableNumber + ' (real number)' + LineEnding
      + cucoVariableFile + ' (file name)' + LineEnding
       ;
  cmsgcomCommandDisallowed = '"%s" command disallowed because it could hang the program. Run in Terminal.';
  cmsgcomCommandDisallowedDetached = '"%s" command can not be run as a detached child process because it could hang the program.';
  cmsgucoDisallowPipe_Detach = 'Can not run piped commands as detached processes.';
  cmsgucoDisallowPkexec_detached = 'Can not run pkexec commands as detached processes.';
  cmsgucoDisallowShell_Detach = 'Can not run commands designated to go through Shell as detached processes.';
  cmsgucoNoCmdOrParams = 'No command / parameters';
  cmsgucoMerge_Added = '"%s" added to Destination';
  cmsgucoMerge_Existing = '"%s" merged to existing in Destination';
  cmsgucoError_MergeConvSaving = 'problem saving command. %s halted.';
  cmsgucoError_Merge = 'Merge';
  cmsgucoError_Converting = 'Converting';
  cmsgInDestNot = '>> Not in Destination [%s]: ' + LineEnding;
  cmsgInDest = '>> Present in Destination [%s]: ' + LineEnding;
  cmsgCompareSummary = '========= Summary of %s: ============';
  cmsgDBStatistics = '%s has %d Commands and %d Command Lines';
  cmsgPathsDiffer = 'Path is different: "%s" vs. "%s"';
  cmsgCLIs = '  present:';
  cmsgCLIsNot = '  not present:';
  cmsgCLDest = '  present in destination but not in <source:';
  cmsgCLsNone = 'none';
  ccapCompareListSpecific = '"%s" %s List:';
  ccapCompareList = '%s Comparison:';
  cmsgCompareListCnt = 'Count = %d';
  ccapCompareIdentical = LineEnding + '  >> Identical';



implementation

uses
   juusgen
  , unitfrmCommandsVar
  , unitfrmCommandsInput
  , controls
  , unitDBUtils
  ;

{ TCmdDisplayObj }


procedure TCmdDisplayObj.SetCommandName( AValue : string );
begin
  if fCommandName = AValue then Exit;
  fCommandName := AValue;
end;

procedure TCmdDisplayObj.SetDetailIdx( AValue : integer );
begin
  if fDetailIdx = AValue then Exit;
  fDetailIdx := AValue;
end;


procedure TCmdDisplayObj.SetEntry( AValue : string );
begin
  if fEntry = AValue then Exit;
  fEntry := AValue;
end;

procedure TCmdDisplayObj.SetDL( AValue : TDataLocation );
begin
  if fDL = AValue then Exit;
  fDL := AValue;
end;

procedure TCmdDisplayObj.SetIsCommandLine( AValue : boolean );
begin
  if fIsCommandLine = AValue then Exit;
  fIsCommandLine := AValue;
end;

procedure TCmdDisplayObj.SetSectTab( AValue : string );
begin
  if fSectTab = AValue then Exit;
  fSectTab := AValue;
end;


constructor TCmdDisplayObj.Create;
begin
  fSectTab := '';
  fDetailIdx := -1;
end;

destructor TCmdDisplayObj.Destroy;
begin
  inherited Destroy;
end;



{ TCmdSL }

procedure TCmdSL.SetSL( AValue : TStringlist );
begin
  if fSL = AValue then Exit;
  fSL := AValue;
end;

constructor TCmdSL.Create;
begin
  fSL := TStringList.Create;
end;

destructor TCmdSL.Destroy;
var
  i : Integer;
begin
  if assigned( fSL ) then
  begin
  for i := 0 to fSL.Count - 1 do
   begin
     if assigned( fSL.Objects[ i ] ) then
     begin
       fSL.Objects[ i ].Free;
       fSL.Objects[ i ] := nil;
     end;
   end;
   FreeAndNil( fSL );
  end;
  inherited Destroy;
end;

{ TCmdObjHelper }

constructor TCmdObjHelper.Create;
begin
  fCmd_ControlColumn_sql := '';
  fCmdLine_ControlColumn_sql := '';

  ftheRegX := TRegExpr.Create;
  RefreshEnvironmentPath;
end;

destructor TCmdObjHelper.Destroy;
begin
  if assigned( ftheRegX ) then
    FreeAndNil( ftheRegX );

  inherited Destroy;
end;


procedure TCmdObjHelper.GetCmdList( anSL : TStrings );
begin

{$IFNDEF Release}
  if DontRun( not assigned( fDBServer ),
              'TCmdObjHelper.GetCmdList: Programmer! fDBServer not assigned, set before using CmdObjHelper'
             ) then
    exit;
{$ENDIF}

  fDBServer.SetControlSlot( dlCmd );

  fDBServer.LoadList( strif( fDBServer.UseDB, Cmd_ControlColumn_sql, cIniSectionMaster_CmdObj_List ), anSL );

end;

procedure TCmdObjHelper.ProcessCommandEntry( aString : string; strings : TStrings );
begin
  CommandToList( aString,  Strings );
end;

procedure TCmdObjHelper.SetDBServer( AValue : TInfoServer );
begin
  if not assigned( AValue ) then
    raise EErrorDevelopment.create( 'TCmdObjHelper.SetDBServer: Infoserver is nil.' );
  if fDBServer = AValue then Exit;
  fDBServer := AValue;
end;


function TCmdObjHelper.CleanCommandsEntry( aString : string ) : string;

  procedure CheckReservedWordAtPosOne(ToChk: string);
  begin
    if pos(ToChk, aString) = 1 then
      aString := stringreplace(aString, ToChk, '', []);
  end;

begin
//What's Returned is the properly formatted command, Indicators snipped off
  if aString = '' then
    exit;
  CheckReservedWordAtPosOne(cReservedDel);
  CheckReservedWordAtPosOne(cReservedUpdate);
  CheckReservedWordAtPosOne(cReservedAdd);

  Result := trim( aString );

end;


function TCmdObjHelper.GetVariableHelp( const VariableName: string; DefaultResult : string = '' ) : string;
begin

  if DefaultResult = '' then
    DefaultResult := format( cmsgucoVariableInvalid, [ VariableName ] );

  case GetSignificantVariableFlag( VariableName ) of
    's','S' : result := cmsgucoVariableString;
    'i','I' : result := cmsgucoVariableInteger;
    'f','F' : result := cmsgucoVariableFilename;
    'n','N' : result := cmsgucoVariableNumber;
    else result := DefaultResult;

  end;
end;

function TCmdObjHelper.GetSignificantVariableFlag( const aString : string ) : string;
begin
  result := copy( aString, cucoSignificantVariableStart, cucoSignificantVariableLength );
end;


function TCmdObjHelper.GetVariableAndPosition( const aString : string; var VarName : string ) : integer;
begin
  result := 0;
  VarName := '';
  if ftheRegX.Exec( aString ) then
   result := ftheRegX.MatchPos[ 0 ];

  if result > 0 then
    VarName := copy( aString, result, cucoVariableSize );

//other variations on regexpr
  //  While theRegX.Exec( substr ) do
  //  begin
  //    i := theRegX.MatchPos[ 0 ];
  //    case copy( substr, i + 2, 1 ) of
  //      's' :  substr := stringreplace( substr, copy( substr, theRegX.MatchPos[ 0 ], 5 ), 'is', [] );
  //      'f' :  substr := stringreplace( substr, copy( substr, theRegX.MatchPos[ 0 ], 5 ), '2.2', [] );
  //    end;
  ////      if i > 0 then
  //
  //  end;

  //example looping through whole thing
  //    if theRegX.Exec( substr ) then
  //    begin
  //      i := theRegX.MatchPos[ 0 ];
  //      Showmessage( copy( substr, i, 5 ) );
  //      while theRegX.ExecNext do
  //      begin
  //        i := theRegX.MatchPos[ 0 ];
  //        Showmessage( copy( substr, i, 5 ) );
  //      end;
  //    end;


end;


function TCmdObjHelper.GetProcessInput : boolean;
begin

  result := true;

  with TfrmCommandsInput.Create( nil ) do
  try

    ShowModal;
    if ModalResult = mrCancel then
    begin
      result := false;
      exit;
    end;
    globltInputForProcess := memInput.Text

  finally
    free;
  end;

end;


function TCmdObjHelper.GetVariableReplacements( var aString : string) : boolean;
begin

  result := true;

  ftheRegX.Expression := cucoVariableRegExpr;

  if not ftheRegX.Exec( aString ) then
    exit;

  with TfrmCommandsVar.Create( nil ) do
  try

    edtCmdLine.Text := aString;
    ShowModal;
    if ModalResult = mrCancel then
    begin
      result := false;
      exit;
    end;
    aString := trim( edtCmdLine.Text );

  finally
    free;
  end;

end;


function TCmdObjHelper.InitializeAndNormalizeCommandsEntry( aString : string; var IsPiped : boolean; WantsInput : boolean) : string;

const
  cLinuxHomeSymbol = '~/';
  Delimiters = [ ' ', '"', '''' ];

  procedure ExpandHomeSymbol;
  var
    theHome : string;
    Idx : SizeInt;
  begin
    ////need to expand to full path,  so "~" expanded.
    Idx := pos( cLinuxHomeSymbol, aString );
    if Idx > 0 then
      theHome := GetEnvironmentVariable( 'HOME' );
    while Idx > 0 do
    begin

      if ( Idx = 1 ) or ( aString[ Idx - 1 ] in Delimiters ) then
        aString := stringreplace( aString, cLinuxHomeSymbol, IncludeTrailingPathDelimiter( theHome ), [] );

      Idx := pos( cLinuxHomeSymbol, aString );

    end;
  end;

begin

  globltInputForProcess := '';
  result := '';

  if not GetVariableReplacements( aString ) then
    exit;

  if WantsInput and not GetProcessInput then
    exit;

  aString := CleanCommandsEntry( aString );

  ExpandHomeSymbol;

  IsPiped := ShouldGoToShell( aString );

  Result := trim( aString );

end;

function TCmdObjHelper.ThreatLevelToListIdx( const TL : TThreatLevel ) : integer;
begin
  result := Ord( TL ) - 1;
end;

function TCmdObjHelper.ListIdxToThreatLevel( const Idx : Integer ) : TThreatLevel;
begin
  Result := TThreatLevel( Idx + 1 );
end;

function TCmdObjHelper.GetThreatLevelText( const TL : TThreatLevel ) : string;
begin
  result := Get_ThreatLevel_Text( TL );
end;

procedure TCmdObjHelper.GetCmdDisplayObjList_Cmd( Source, Dest : TStringList; DL : TDataLocation );
var
  CDO : TCmdDisplayObj;
  i, ItemIdx : Integer;
begin


  for i := 0 to Source.Count - 1 do
  begin

    ItemIdx := strtoint( Source[ i ] );
    CDO := TCmdDisplayObj.Create;
    CDO.DL := DL;

    CDO.SectTab := fDBServer.GetSearchIdx( ItemIdx );
    //29 rowid =OR=
    //xxx ini section

    CDO.CommandName := fDBServer.GetSearchDisplayMain( ItemIdx );

//entry for list to display, command name here
    CDO.Entry := CDO.CommandName;


    CDO.IsCommandLine := false;
    Dest.AddObject( CDO.Entry, CDO );
    CDO := nil;

  end;

  Dest.Sort;

end;

procedure TCmdObjHelper.GetCmdDisplayObjList_CmdLine( Source, Dest : TStringList; DL : TDataLocation );
var
  CDO : TCmdDisplayObj;
  i, ItemIdx : Integer;
  Idx : SizeInt;
begin
//This routine fills in the CmdDisplayObj, the standin for objects in generated lists:
// == FROM A SEARCH == which uses a special InfoServer construct.
  for i := 0 to Source.Count - 1 do
  begin
    CDO := TCmdDisplayObj.Create;
    CDO.DL := DL;

    ItemIdx := strtoint( Source[ i ] );
    CDO.SectTab := fDBServer.GetSearchIdx( ItemIdx );//Source[ i ];
    //29 rowid =OR=
    //xxx//xxx//29  ini section

    Idx := Pos( ciniSubSeparator, CDO.SectTab );
    CDO.CommandName := copy( CDO.SectTab, 1, Idx - 1 );

    CDO.IsCommandLine := true;

//use a friendly name if it exists
    CDO.Entry := GetCmdDisplayObj_CmdLineEntry( fDBServer.GetSearchDisplayMain( ItemIdx ),
                                                fDBServer.GetSearchDisplayAlt( ItemIdx ) );
//entry for list to display, commandline itself or friendly name here
    Dest.AddObject( CDO.Entry, CDO );
    CDO := nil;

  end;

  Dest.Sort;

end;

procedure TCmdObjHelper.GetCmdDisplayObjList_CmdLine_External( CO : TCmdObj; Dest : TStringList; Friendly : boolean = true );
var
  CDO : TCmdDisplayObj;
  CLO : TCmdLineObj;
  i : Integer;
  Idx : SizeInt;
begin
//This routine fills in the CmdDisplayObj, the standin for objects in generated lists:
// == FROM AN OBJECT == request usually generated in onclick events

  for i := 0 to CO.CmdLines.Count - 1 do
  begin

    CLO := TCmdLineObj( CO.CmdLines.Objects[ i ] );
    if not assigned( CLO ) then
      continue;

    CDO := TCmdDisplayObj.Create;
    CDO.DL := CLO.DataLocation;
    CDO.IsCommandLine := true;

    if fDBServer.UseDB then
    begin
      CDO.SectTab := inttostr( CLO.RowID );
      //29 rowid
      CDO.CommandName := CO.CommandName;
    end
    else
    begin
      CDO.SectTab := CLO.IniSection;
      //xxx//xxx//29 ini section
      Idx := Pos( ciniSubSeparator, CDO.SectTab );
      CDO.CommandName := copy( CDO.SectTab, 1, Idx - 1 );
    end;

//use a friendly name if it exists
    if Friendly then
      CDO.Entry := GetCmdDisplayObj_CmdLineEntry( CLO.Entry, CLO.FriendlyName )
    else CDO.Entry := CLO.Entry;
//entry for list to display, commandline itself or friendly name here
    Dest.AddObject( CDO.Entry, CDO );
    CDO := nil;

  end;

  Dest.Sort;

end;

procedure TCmdObjHelper.CreateCmdObj( var aCO : TCmdObj );
begin
  aCO := TCmdObj.Create( fDBServer );
  aCO.fCmdObjHelper := self;
end;

function TCmdObjHelper.GetCmdDisplayObj_CmdLineEntry( const Main, Alt : string ) : string;
begin
  result :=  strif( Alt = '', Main, '  ' + Alt + '           --[ ' + Main + ' ]' );
end;

procedure TCmdObjHelper.GetCmdDisplayObjList( Source, Dest : TStringList; IsCmd : boolean = true );
begin
  if IsCmd then
    GetCmdDisplayObjList_Cmd( Source, Dest, dlCmd )
  else GetCmdDisplayObjList_CmdLine( Source, Dest, dlCmdLine );
end;


function TCmdObjHelper.GetCmdLine_DisplayObjects( Source, Dest : TStringList) : integer;
begin

  result := 0;

//  fDBServer.SetControlSlot( dlCmdLine );

  if fDBServer.UseDB then
    GetCmdDisplayObjList( Source, Dest, true )
  else GetCmdDisplayObjList( Source, Dest, false );

  result := Dest.Count;

end;


function TCmdObjHelper.Search( aSearch : TSearchObj; Results : TStrings ) : integer;
var
  SL : TStringList;
  CmdSL, CmdLineSL: TStringList;
  InsertIdx , i: Integer;
begin

{$IFNDEF Release}
  if DontRun( not assigned( fDBServer ),
              'TCmdObjHelper.Search: Programmer!! fDBServer not assigned, set before using CmdObjHelper'
             ) then
    exit;
{$ENDIF}

    result := -1;
    InsertIdx := -1;

    SL := TStringlist.Create;
    CmdSL := TStringlist.Create;
    CmdLineSL := TStringlist.Create;

    try

      for i := Low( aSearch.Searches ) to High( aSearch.Searches ) do
      begin

        fDBServer.OpenSearch;

        aSearch.Searches[ i ].Execute( SL );

//non generic, assumes only two searches comes through: Cmd search and Cmdline search
//would need to expand and design change if this ever changes, which is unlikely unless a third detail table
//of some sort was added.
        case i of
          0 :
            begin
              if ( Length( aSearch.Searches ) > 1 ) and ( SL.Count > 0 ) then
//sends back the position to separate Cmd's from CmdLine's for the listbox
                InsertIdx := SL.Count;
              GetCmdDisplayObjList( SL, CmdSL, true );
            end;
          else GetCmdDisplayObjList( SL, CmdLineSL, false );
        end;

        SL.Clear;

      end;

      if ( InsertIdx > -1 ) and ( CmdSL.Count + CmdLineSL.Count > InsertIdx ) then
        result := InsertIdx;

      for i := 0 to CmdSL.Count - 1 do
      begin
        Results.AddObject( CmdSL[ i ], CmdSL.Objects[ i ] );
        CmdSL.Objects[ i ] := nil;
      end;
      for i := 0 to CmdLineSL.Count - 1 do
      begin
        Results.AddObject( CmdLineSL[ i ], CmdLineSL.Objects[ i ] );
        CmdLineSL.Objects[ i ] := nil;
      end;

    finally
      fDBServer.CloseSearch;
      if assigned( CmdSL ) then
        FreeAndNil( CmdSL );
      if assigned( CmdLineSL ) then
        FreeAndNil( CmdLineSL );
      if assigned( SL ) then
        FreeAndNil( SL );
    end;

end;


procedure TCmdObjHelper.LoadCmdObj( var aCO : TCmdObj; const Key : string );
begin
  try
    aCO := TCmdObj.Create( fDBServer );
    aCO.fCmdObjHelper := self;
    aCO.Load( Key, Cmd_ControlColumn_sql, strtointdef( Key, cFieldIntegerTypeDefault ) );
  except
    on e:exception do
    begin
      if assigned( aCO ) then
        FreeAndNil( aCO );
      EErrorRaise( e, cEErrorCmdObj, 'TCmdObjHelper.LoadCmdObj: ' );
    end;
  end;
end;

procedure TCmdObjHelper.LoadCmdLineObj( var aCLO : TCmdLineObj; const CommandName, Section : string;
                                        iSO : integer = cFieldIntegerTypeDefault );
begin
//CmdLineObj is a detail table
//this supports loading of a single CLO (ie, search result) and Loading bundle of CLO's belonging to a Cmd.
  try
    aCLO := TCmdLineObj.Create( fDBServer, CommandName, iSO );
    aCLO.Load( strif( fDBserver.UseDB, CmdLine_ControlColumn_sql, Section ), strtointdef( Section, cFieldIntegerTypeDefault ) );
  except
    on e:exception do
    begin
      if assigned( aCLO ) then
        FreeAndNil( aCLO );
      EErrorRaise( e,
                   cEErrorCmdObj,
                   'TCmdObjHelper.LoadCmdLineObj: '
                   + format( 'TCmdObjHelper.LoadCmdLineObj error: "%s" "%s"  ', [ CommandName, Section ] )
                  );
    end;
  end;

end;

function TCmdObjHelper.ClipBoardVersion( const Str, RootFile : string ) : string;
begin
  result := CmdObjHelper.VariablesAsClipboardText( Str );

  if pos( cReservedSuperUser, Str ) = 1 then
  begin
    result := trim( stringreplace( result, cReservedSuperUser, '', [ rfIgnoreCase ] ) );
    result := StripPkexec( result );
    result := format( RootFile, [ result ] );
  end;

end;


function TCmdObjHelper.VariablesAsClipboardText( anEntry : string ) : string;
begin
  anEntry := stringreplace( anEntry, cucoVariableString, cucoVariableStringClipText, [ rfIgnoreCase, rfReplaceAll ] );
  anEntry := stringreplace( anEntry, cucoVariableInteger, cucoVariableIntegerClipText, [ rfIgnoreCase, rfReplaceAll ] );
  anEntry := stringreplace( anEntry, cucoVariableNumber, cucoVariableNumberClipText, [ rfIgnoreCase, rfReplaceAll ] );
  result := stringreplace( anEntry, cucoVariableFile, cucoVariableFileClipText, [ rfIgnoreCase, rfReplaceAll ] );
end;

function TCmdObjHelper.GetCmd_ControlColumn_sql : string;
begin
  if fCmd_ControlColumn_sql = '' then
    fCmd_ControlColumn_sql := Get_CommandStruct_ControlColumn( true );
  result := fCmd_ControlColumn_sql;
end;

function TCmdObjHelper.GetCmdLine_ControlColumn_sql : string;
begin
  if fCmdLine_ControlColumn_sql = '' then
    fCmdLine_ControlColumn_sql := Get_CmdLineStruct_FK_Column;
  result := fCmdLine_ControlColumn_sql;
end;

function TCmdObjHelper.RouteCommand( aString : string; const DoDetach : boolean; var aProcess : TAsyncProcess ) : string;
var
  CmdSL : TCmdSL;
  DoThroughShell , IsPiped: Boolean;

  function GoodToRun : boolean;
  begin
    result := false;

    if DoDetach and IsPkexec( aString ) then //assigned( CmdSL.SL.Objects[ 0 ] ) then
      raise EErrorDisallowed.Create( cmsgucoDisallowPkexec_detached );

//if strings.objects is assigned it is a pipe'ing command line, else it is a simple command
    if DoDetach and IsPiped then //assigned( CmdSL.SL.Objects[ 0 ] ) then
      raise EErrorDisallowed.Create( cmsgucoDisallowPipe_Detach );

    if DoDetach and DoThroughShell then
      raise EErrorDisallowed.Create( cmsgucoDisallowShell_Detach );

    result := true;
  end;

begin

  if aString = '' then
    exit;

  // echo 'pants are cool' | grep 'moo' | sed 's/o/x/' | awk '{ print $1 }'
  // echo ${PIPESTATUS[@]}
  //0 1 0 0

// what if > or < or >> or <<? or 2>&1  Then the command should be sent through shell, it may, or may not work.
//To do that write a #27 as first character of aString (the command string to run)
  DoThroughShell := pos( csoNonPrintingDelimiter, aString ) = 1;
  if DoThroughShell then
    aString := copy( aString, 2, MAXINT );//length( aString ) );

  IsPiped := ShouldGoToShell( aString );

// !!! fairly important to use TCmdSL! Unless you like memory leaks. The stringlistc can be normal Stringlist or,
  //  if a piped command, a stringlist of stringlists. This object takes care of that and frees the memory.
  // Not so true anymore, since it was only used for my local piped CL's routines which are currently dead,
  // but keeping it for now, it doesn't hurt anything
  CmdSL := TCmdSL.Create;

  try

    if not GoodToRun then
      exit;

    if DoThroughShell or IsPiped then
    begin

//strip anything that calls for elevated priveleges
      if pos( cReservedSuperUser, aString ) = 1 then
        aString := stringreplace( aString, cReservedSuperUser, '', [rfIgnorecase] );

      aString := StripPkexec( aString );

      DoThroughShell := true;//force pipes through shell too.

    end
    else ProcessCommandEntry( aString, CmdSL.SL );

    if DoDetach then
        result := RunCommandDetached( aProcess, CmdSL.SL )
    else result := RunCommand( CmdSL.SL, DoThroughShell, aString );

  finally
    CmdSL.Free;
  end;

end;

procedure TCmdObjHelper.ProcessFileNamePath(const aComName: string; var aComFile: string; var aComPath: string);
begin

  aComFile := ExtractFileName( aComName );
  aComPath := ExtractFilePath( aComName );

  if not FileExists( aComName ) then
  begin
    aComPath := ExtractFilePath( SystemFileLocation( aComFile ) );
    if ( aComPath = '' ) then
      aComPath := cmsgcleBadPath //'$BAD_PATH'
    else aComPath := cCommandInPathStr; //'$PATH'
  end
//if user types in the /bin/ etc. PATH CHECKING ROUTINE HERE
  else if Path_in_EnvironPath( aComPath ) then
       aComPath := cCommandInPathStr; //'$PATH'
end;

function TCmdObjHelper.HasRealPath( const PathStr : string ) : boolean;
begin
  result :=  not ( ( PathStr = cCommandInPathStr ) or ( PathStr = cmsgcleBadPath ) );
end;

function TCmdObjHelper.GetNormalizedPath( const PathAlias, CommandName : string ) : string;
var
  thePath : String;
begin
//returns a path the program can use, not the constants
  thePath := StrIf( HasRealPath( PathAlias ), PathAlias );
//I store the path with final slash but...this makes it bulletproof.
  result := strif( thePath <> '', IncludeTrailingPathDelimiter( thePath ) ) + CommandName;
end;

function TCmdObjHelper.CanRunCommand( ExecuteStr : string ) : boolean;
begin
//Running file HAS TO BE CHECKED again, TProcess makes ==VERY== nasty if it can't execute it.
  result := false;

  if not fileexists( ExecuteStr ) then
  begin
    ExecuteStr := extractfileName( ExecuteStr );
    if not SystemFileFound( ExecuteStr ) then
      exit;
  end;

  result := true;

end;


function TCmdObjHelper.RunCommand( Params : TStrings; UseShell : boolean; ShStr : string = '' ) : string;
var
  ResultErr, CheckStr: String;
  Idx : integer;

  function DisAllowedCommand( Cmd : string ) : boolean;
  begin
    Cmd := extractfilename( Cmd );
    result := ( Cmd = 'time' ) or ( Cmd = 'timeout' );
  end;

begin

  result := cmsgcleBadCommand;

  if UseShell then
  begin

    ResultErr := '';

    Idx := pos( ' ', ShStr );
    if Idx > 0 then
      CheckStr := copy( ShStr, 1, Idx - 1 )
    else CheckStr := ShStr;

    if NotAllowedInShell( trim( CheckStr ), ResultErr ) then
    begin
      result := ResultErr;
      exit;
    end;

    result := RunThroughShell( ShStr );

    exit;
  end;

  if Params.Count = 0 then
  begin
    result := cmsgucoNoCmdOrParams;
    exit;
  end;

  if DisallowedCommand( Params[ 0 ] ) then
  begin
    result := format( cmsgcomCommandDisallowed, [ Params[ 0 ] ] );
    exit;
  end;

  if not CanRunCommand( Params[ 0 ] ) then
    exit;

  result := ProcString( Params );

end;


function TCmdObjHelper.RunCommandDetached( var aProc : TAsyncProcess; Params : Tstrings ) : string;

  function DetachedNotAllowed( const Cmd : string ) : boolean;
  begin
    result := ( Cmd = 'bash' )
              or ( Cmd = 'sh' )
              or ( Cmd = 'dash' )
              or ( Cmd = 'ash' )
              or ( Cmd = 'tcsh' )
              or ( Cmd = 'csh' )
              or ( Cmd = 'zsh' )
              or ( Cmd = 'ksh' )
              or ( Cmd = 'pdksh' )
              or ( Cmd = 'Scsh' )
              //or....????!!!!!
              ;
  end;

begin

    result := cmsgucoNoCmdOrParams;

    if Params.Count = 0 then
      exit;

    result := cmsgcleBadCommand;

    if not CanRunCommand( Params[ 0 ] ) then
      exit;

    result := format( cmsgcomCommandDisallowedDetached, [ Params[ 0 ] ] );

    if DetachedNotAllowed( extractFileName( Params[ 0 ] ) ) then
      exit;

    aProc := TAsyncProcess.Create(globLinuxProcessOwner);//( nil );
    result := ProcDetached( aProc, Params );

end;


{ TCmdListObj }

procedure TCmdListObj.Revert;
begin
  fListItemsSL.Clear;
  fChangesSL.Clear;
  fIsDirty := False;
  fInternalIdx := -1;
  Load;
end;


procedure TCmdListObj.DBUpdateTarget( sqlSL : TStrings );
var
  SL: TStringList;
  i: integer;
  Str, NewStr , ID, TableName: string;
begin

//update the list's target data to synchronize with changes made

  fDBServer.SetControlSlot( fTargetDataLocation );

  NewStr := '';

  if fDBServer.UseDB then
  begin

    if not assigned( sqlSL ) then
      raise EErrorDevelopment.create( 'TCmdListObj.DBUpdateTarget: sqlSL not assigned' );

    try

      TableName := fDBServer.OpenQuery( fTargetDataLocation, fColumnName, '' );
      while not fDBServer.QueryEOF do
      begin
        Str := fDBServer.QueryVal( fColumnName );
        if ApplyChangesRawDBData( Str, NewStr ) then
        begin
          ID := fDBServer.QueryVal( csoSqlite_rowid );
          sqlSL.Add( GetUpdate_Sql( TableName,
                                    format( '%s = %s', [ fColumnName, QuotedStr( NewStr ) ] ),
                                    format( '%s=', [ csoSqlite_rowid ] ) + ID
                                  )
                    );
        end;
        fDBServer.QueryNext;
      end;

    finally
      fDBServer.CloseQuery;
    end;

  end
  else
  begin

//someday maybe figure out if you can also somehow make text db also "transaction" aware. Meaning, all or nothing "commit"
    SL := TStringList.Create;
    try
      fDBServer.GetSectionList( SL );
      for i := 0 to SL.Count - 1 do
      begin
        Str := fDBServer.ReadString(SL[i], fColumnName, cFieldStringTypeDefault);
        if (Str <> '') and (pos(csoNonPrintingDelimiter, Str) > 0) then
        begin

          if ApplyChangesRawDBData( Str, NewStr ) then
            fDBServer.WriteString(SL[i], fColumnName, NewStr);

        end;
      end;
      fDBServer.SaveIniFile;

    finally
      SL.Free;
    end;
  end;

end;

function TCmdListObj.NeedsDBUpdate: boolean;
begin
  Result := FChangesSL.Count > 0;
end;


function TCmdListObj.ApplyChangesRawDBData( const OrigEntry : string; var EntryResult : string ) : boolean;
var
  OrigStr, ReplaceStr: string;
  i: integer;
begin

  EntryResult := OrigEntry;

  for i := 0 to fChangesSL.Count - 1 do
  begin

    ReplaceStr := fChangesSL[i];

    OrigStr := csoNonPrintingDelimiter
               + snip( ReplaceStr, csoNonPrintingDelimiter, True)
               + csoNonPrintingDelimiter;

    if ReplaceStr = '' then
      ReplaceStr := csoNonPrintingDelimiter
    else
    begin
      ReplaceStr := csoNonPrintingDelimiter
                    + ReplaceStr
                    + csoNonPrintingDelimiter;
      if pos( ReplaceStr, EntryResult ) > 0 then
//it is a consolidate where the target name already exists
        ReplaceStr := csoNonPrintingDelimiter
    end;

    EntryResult := StringReplace( EntryResult, OrigStr, ReplaceStr, [rfignorecase, rfreplaceall] );

  end;
  if EntryResult = csoNonPrintingDelimiter then
    EntryResult := '';
//comparestr is case sensitive
  Result := CompareStr( OrigEntry, EntryResult ) <> 0;

end;


function TCmdListObj.ApplyChangesObjectData( const OrigEntry : string; var EntryResult : string ) : boolean;
var
  OrigStr, ReplaceStr: string;
  i, Idx: integer;
  SL: TStringList;
begin

  Result := false;

  SL := TStringList.Create;
  try
    SL.CaseSensitive := true;
    SL.Text := OrigEntry;

    for i := 0 to fChangesSL.Count - 1 do
    begin

      ReplaceStr := fChangesSL[i];

      OrigStr := snip( ReplaceStr, csoNonPrintingDelimiter, True );

      Idx := SL.IndexOf( ReplaceStr );
      if Idx > -1 then
//for consolidation where the target word is already in the list
        ReplaceStr := '';

      Idx := SL.IndexOf( OrigStr );

      if Idx > -1 then
      begin

        if ReplaceStr = '' then
          SL.Delete(Idx)
        else
          SL[Idx] := ReplaceStr;

        result := true;

      end;

    end;

    EntryResult := SL.Text;

  finally
    SL.Free;
  end;

end;


function TCmdListObj.ApplyUpdateList( const LT : TDBListType; CmdSL : TStrings; ShowingCmd : Integer ) : boolean;
var
  i: integer;
  aCmdObj: TCmdObj;
  NewStr: string;
  WasInUpdateMode : boolean;
  SL : TStringList;

  procedure ApplyChanges;
  begin

    case LT of
      dbltKeyWord :
        if ApplyChangesObjectData( aCmdObj.Keywords, NewStr ) then
          aCmdObj.Keywords := NewStr;
    end;

  end;

  procedure ProcessCurrentlyDisplayedObject;
  begin
//frmMain CmdObj is always associated with lbCommands.ItemIndex, here ShowingCmd
    WasInUpdateMode := aCmdObj.DoUpdate;

    case LT of

      dbltKeyWord :
//need to check here 'cause if Keywords is changed DoUpdate is true
        if ApplyChangesObjectData( aCmdObj.Keywords, NewStr ) then
        begin
          aCmdObj.Keywords := NewStr;
          if not WasInUpdateMode then
            aCmdObj.ResetUpdate;
        end;

    end;

  end;

begin

  result := NeedsDBUpdate;

  SL := TStringList.Create;
  try

    try

      if NeedsDBUpdate then
        DBUpdateTarget( SL  );

      Save( SL ); //it's self aware-ish and only updates as necessary

    except
      on e:exception do
        EErrorRaise( e, cEErrorDatabaseProblem, 'TCmdListObj.ApplyUpdateList: ' );
    end;

    if NeedsDBUpdate then
    begin

      for i := 0 to CmdSL.Count - 1 do
      begin
        //process current list of displayed and EditState commands
        aCmdObj := TCmdRec( CmdSL.Objects[ i ] ).Cmd;

        if not Assigned(aCmdObj) then
          continue;

        if i = ShowingCmd then
        begin
  //the command being viewed is a special case
          ProcessCurrentlyDisplayedObject;
          continue;
        end;

        if aCmdObj.DoUpdate then
          ApplyChanges;

      end;

    end;

  finally
    SL.free;
    fChangesSL.Clear;
  end;

end;

function TCmdListObj.Get_Search_List_Text( const ToSearch : string; TCS : integer ) : string;
var
  SL : TStringList;
  SO : TSearchObj;
begin
  SL := TStringlist.Create;
  try
    SO := TSearchObj.Create( 'InternalUse', fDBServer );
    SO.UserTag := TCS;
    SO.Searches[ cIdxCmdLine ].IsUsed := false;

    SO.Searches[ cIdxCmd ].AddSearchItem( fTargetFieldID, ToSearch, coMatchQualified, false );

    CmdObjHelper.Search( SO, SL );

    result := SL.Text;

  finally
    FreeStringListWithObjects( SL );
    FreeAndNil( SL );

    if assigned( SO ) then
      FreeAndNil( SO );
  end;

end;

constructor TCmdListObj.Create( DL, LinkTo : TDataLocation; LinkToFID : TCmdFieldID;
                                            const anIniSection, aColumname : string; DBS : TInfoServer );
begin
  inherited Create;

  fListItemsSL := TStringList.Create;
  fListItemsSL.CaseSensitive := False;
  fChangesSL := TStringList.Create;
  fDBServer := DBS;

  fIniSection := anIniSection;
  fSelfDataLocation := DL;
  fTargetDataLocation := LinkTo;
  fTargetFieldID := LinkToFID;
  fColumnName := aColumname;

  Revert; //i.e. load up the data

end;

destructor TCmdListObj.Destroy;
begin

  if assigned(fListItemsSL) then
    FreeAndNil(fListItemsSL);

  if assigned(fChangesSL) then
    FreeAndNil(fChangesSL);

  inherited Destroy;
end;

class function TCmdListObj.CreateListStructure( var CLO : TCmdListObj;
                                      const ListID : TDBListType; DBS : TInfoServer; DoReg : boolean = true ) : boolean;
begin
  result := false;
  case ListID of
    dbltKeyWord :
      begin
        CLO := TCmdListObj.Create( KeyWordStruct.DataLocation ,
                                   KeyWordStruct.LinkedToDataLocation,
                                   KeyWordStruct.LinkedToFieldID,
                                   KeyWordStruct.GetSectTabName,
                                   KeyWordStruct.ColumnName,
                                   DBS
                                 );
        result := true;
      end;
  end;

  if DoReg and result then
  begin
  //if in future more lists are used then add them in similar fashion, List fields then automatically
  //look up the appropriate list in the Search Forms
//internalmessage( CLO.Columnname + LineEnding + CLO.ListItemsSL.Text );
    ListFields_Linker.AddObject( CLO.ColumnName, CLO.ListItemsSL );
  end;

end;

class function TCmdListObj.MergeLists( const ListID : TDBListType; SourceDB, DestDB : TInfoServer) : boolean;
var
  FromKeyW, ToKeyW : TCmdListObj;
  i , Idx, OrigCount: Integer;

begin

  result := true;
  try

    FromKeyW := nil;
    ToKeyW := nil;

    try

      TCmdListObj.CreateListStructure( FromKeyW, ListID, SourceDB, false );
      TCmdListObj.CreateListStructure( ToKeyW, ListID, DestDB, false );

      OrigCount := ToKeyW.ListItemsSL.Count;
      for i := 0 to FromKeyW.ListItemsSL.count - 1 do
      begin
        Idx := ToKeyW.ListItemsSL.IndexOf( FromKeyW.ListItemsSL[ i ] );
        if Idx < 0 then
          ToKeyW.ListItemsSL.Add( FromKeyW.ListItemsSL[ i ] );
      end;
      if OrigCount < ToKeyW.ListItemsSL.Count then
      begin
        ToKeyW.ListItemsSL.Sort;
        ToKeyW.Save_Direct;
      end;

    finally //Lists

      if assigned( ToKeyW ) then
        FreeAndNil( ToKeyW );

      if assigned( FromKeyW ) then
        FreeAndNil( FromKeyW );
    end;

  except
    result := false;
  end;

end;

class function TCmdListObj.MergeListsOne( const ListID : TDBListType; DestDB : TInfoServer; CO : TCmdObj ) : boolean;
var
  ToKeyW : TCmdListObj;
  i, Idx, OrigCount: Integer;
  SL : TStringList;

begin

  result := true;
  try

    ToKeyW := nil;

    try
      SL := TStringlist.Create;
      SL.Text := CO.Keywords;

      TCmdListObj.CreateListStructure( ToKeyW, ListID, DestDB, false );

      OrigCount := ToKeyW.ListItemsSL.Count;
      for i := 0 to SL.Count - 1 do
      begin
        Idx := ToKeyW.ListItemsSL.IndexOf( SL[ i ] );
        if Idx < 0 then
          ToKeyW.ListItemsSL.Add( SL[ i ] );
      end;
      if OrigCount < ToKeyW.ListItemsSL.Count then
      begin
        ToKeyW.ListItemsSL.Sort;
        ToKeyW.Save_Direct;
      end;

    finally //Lists
      if assigned( SL ) then
        FreeAndNil( SL );
      if assigned( ToKeyW ) then
        FreeAndNil( ToKeyW );
    end;

  except
    result := false;
  end;

end;

class function TCmdListObj.CompareLists( const ListID : TDBListType; SourceDB, DestDB : TInfoServer;
                                         Strings : TStrings; const frName, toName : string ) : boolean;
var
  FromKeyW, ToKeyW : TCmdListObj;
  //i , Idx, OrigCount: Integer;
  i : integer;
begin

  result := false;

  FromKeyW := nil;
  ToKeyW := nil;

  try

    TCmdListObj.CreateListStructure( FromKeyW, ListID, SourceDB, false );
    TCmdListObj.CreateListStructure( ToKeyW, ListID, DestDB, false );

    Strings.Add( '' );
    Strings.Add( '' );
    Strings.Add( '========== ' + format( ccapCompareList, [ FromKeyW.ColumnName ] ) );
    Strings.Add( '' );
    Strings.Add( format( ccapCompareListSpecific, [ frName, FromKeyW.ColumnName ] ) );
    Strings.Add( '---------------' );
    Strings.Add( format( cmsgCompareListCnt, [ FromKeyW.ListItemsSL.Count ] ) );

    FromKeyW.ListItemsSL.Sort;
    for i := 0 to FromKeyW.ListItemsSL.count - 1 do
      Strings.Add( FromKeyW.ListItemsSL[ i ] );

    Strings.Add( '' );
    Strings.Add( format( ccapCompareListSpecific, [ toName, ToKeyW.ColumnName ] ) );
    Strings.Add( '---------------' );
    Strings.Add( format( cmsgCompareListCnt, [ ToKeyW.ListItemsSL.Count ] ) );

    ToKeyW.ListItemsSL.Sort;
    for i := 0 to ToKeyW.ListItemsSL.count - 1 do
      Strings.Add( ToKeyW.ListItemsSL[ i ] );

    Strings.Add( '' );

  finally //Lists

    if assigned( ToKeyW ) then
      FreeAndNil( ToKeyW );

    if assigned( FromKeyW ) then
      FreeAndNil( FromKeyW );
  end;

  result := true;


end;

class function TCmdListObj.ConvertLists( const ListID : TDBListType; SourceDB, DestDB : TInfoServer ) : boolean;
var
  KeyW : TCmdListObj;
begin
  result := false;
  KeyW := nil;
  try
    try
      TCmdListObj.CreateListStructure( KeyW, ListID, SourceDB, false );
      KeyW.DBServer := DestDB;
      KeyW.Save_Direct;
    finally
      if assigned( KeyW ) then
        FreeAndNil( KeyW );
    end;
    result := true;
  except
    on e:exception do
      EErrorRaise( e, cEErrorDatabaseProblem, 'TCmdListObj.ConvertLists: ' );
  end;

end;

class function TCmdListObj.IsInListAndEmpty( const ListIdx : string ) : boolean;
begin
  result := ListEmpty_ListFields_Linker( ListIdx );
end;

class procedure TCmdListObj.ClearListStructures( theLists : array of TCmdListObj );
var
  i : Integer;
begin

  Clear_ListFields_Linker;

  for i := Low( theLists ) to High( theLists ) do
  begin
    if assigned( theLists[ i ] ) and ( theLists[ i ] is TCmdListObj ) then
      FreeAndNil( theLists[ i ] );
  end;

end;

procedure TCmdListObj.Save( sqlSL : TStrings );
begin

  fDBServer.SetControlSlot( fSelfDataLocation );

  try
    fDBServer.SaveList( fIsDirty, strif( fDBServer.UseDB, ColumnName, fIniSection ), fListItemsSL, sqlSL );
  finally
    fIsDirty := False;
  end;

end;

procedure TCmdListObj.Save_Direct;
var
  SL : TStringList;
begin
  SL := TStringList.Create;
  try
    fDBServer.SetControlSlot( fSelfDataLocation );
    fDBServer.SaveList( true, strif( fDBServer.UseDB, ColumnName, fIniSection ), fListItemsSL, SL );
  finally
    SL.free;
  end;
end;

procedure TCmdListObj.Load;
begin
  fDBServer.SetControlSlot( fSelfDataLocation );
  fDBServer.LoadList( strif( fDBServer.UseDB, ColumnName, fIniSection ), fListItemsSL );
  fIsDirty := False;
end;

function TCmdListObj.Consolidate( const OldEntry, NewEntry : string ) : integer;
var
  OldEntryIdx: integer;
begin
  Result := -1;
  if not find(OldEntry) then
    exit;

  OldEntryIdx := fInternalIdx;

  if not find(NewEntry) then
    exit;

  fListItemsSL.Delete(OldEntryIdx);

  fChangesSL.Add( OldEntry + csoNonPrintingDelimiter + NewEntry );

//redundant?? It was checked above. If nothing inbetween resets fInternalIdx then this can go
  find( NewEntry );

  Result := fInternalIdx;

  fIsDirty := True;
end;


function TCmdListObj.Update( const OldEntry, NewEntry : string ) : integer;
begin

  Result := -1;
  if not find(OldEntry) then
    exit;

  fChangesSL.Add(OldEntry + csoNonPrintingDelimiter + NewEntry);

  fListItemsSL[fInternalIdx] := NewEntry;
  fListItemsSL.Sort;

  find(NewEntry);
  Result := fInternalIdx;

  fIsDirty := True;

end;

function TCmdListObj.Find( const anEntry : string ) : boolean;
begin
  fInternalIdx := fListItemsSL.IndexOf( anEntry );
  Result := fInternalIdx > -1;
end;

procedure TCmdListObj.SetColumnName( AValue : string );
begin
  if fColumnName = AValue then Exit;
  fColumnName := AValue;
end;

procedure TCmdListObj.SetDBServer( AValue : TInfoServer );
begin
  if not assigned( AValue ) then
    raise EErrorDevelopment.create( 'TCmdListObj.SetDBServer: Infoserver is nil.' );
  if fDBServer = AValue then Exit;
  fDBServer := AValue;
end;

procedure TCmdListObj.SetTargetDataLocation( AValue : TDataLocation );
begin
  if fTargetDataLocation = AValue then Exit;
  fTargetDataLocation := AValue;
end;

function TCmdListObj.Delete( Entries : string ) : integer;
var
  ThisEntry: string;
begin
  Result := -1;

  if Entries = '' then
    exit;

  repeat
    ThisEntry := Snip(Entries, csoNonPrintingDelimiter);

    if find(ThisEntry) then
    begin
      fChangesSL.Add(fListItemsSL[fInternalIdx]);
      fListItemsSL.Delete(fInternalIdx);
      fIsDirty := True;
    end;

  until Entries = '';

end;

function TCmdListObj.Add( const anEntry: string ): integer;
var
  Idx: integer;
begin
  if find(anEntry) then
  begin
    Result := fInternalIdx;
    exit;
  end;

  //The user has the ability to make a variety of changes at one time.
  //One that is very ambivalent is the case of deleting an item and then
  //later adding it back. This could mean they really want to delete that item
  //from the DB and then add the item back to start "fresh". But I believe
  //it would be more along the lines of an accidental deletion and the add is to put it
  //back quickly so no changes are made to DB due to this "accident".
  //Since I think this is the more "natural" conclusion people will come to
  //I simply undo the delete. Deletions are in the list as single entries,
  //renames are double entries separated by #27
  Idx := fChangesSL.IndexOf(anEntry);
  while Idx > -1 do
  begin
    fChangesSL.Delete(Idx);
    Idx := fChangesSL.IndexOf(anEntry);
  end;

  fListItemsSL.Add(anEntry);
  fListItemsSL.Sort;

  find(anEntry);
  Result := fInternalIdx;

  fIsDirty := True;
end;


{ TCmdLineObj }


procedure TCmdLineObj.SetUseShell(AValue: boolean);
begin
  if fFieldUseShell.Value = AValue then
    Exit;
  fFieldUseShell.Value := AValue;
end;

function TCmdLineObj.GetUseShell: boolean;
begin
  Result := fFieldUseShell.Value;
end;

procedure TCmdLineObj.SetWantsInput(AValue: boolean);
begin
  if fFieldWantsInput.Value = AValue then
    Exit;
  fFieldWantsInput.Value := AValue;
end;

procedure TCmdLineObj.SetAlert(AValue: boolean);
begin
  if fFieldAlert.Value = AValue then
    Exit;
  fFieldAlert.Value := AValue;
end;

function TCmdLineObj.GetWantsInput: boolean;
begin
  Result := fFieldWantsInput.Value;
end;


function TCmdLineObj.GetAlert: boolean;
begin
  Result := fFieldAlert.Value;
end;


procedure TCmdLineObj.SetFriendlyName(AValue: string);
begin
  if fFieldFriendlyName.Value = AValue then
    Exit;
  fFieldFriendlyName.Value := AValue;
end;

function TCmdLineObj.GetFriendlyName: string;
begin
  Result := fFieldFriendlyName.Value;
end;

procedure TCmdLineObj.SetCmdFK( AValue: integer );
begin
  if fFieldCmdFK.Value = AValue then
    Exit;
  fFieldCmdFK.Value := AValue;
end;


function TCmdLineObj.GetCmdFK: integer;
begin
  Result := fFieldCmdFK.Value;
end;

procedure TCmdLineObj.SetSO( AValue: integer );
begin
  if fFieldSO.Value = AValue then
    Exit;
  fFieldSO.Value := AValue;
end;


function TCmdLineObj.GetSO: integer;
begin
  Result := fFieldSO.Value;
end;

procedure TCmdLineObj.SetEntry(AValue: string);
begin
  if fFieldEntry.Value = AValue then
    Exit;
  fFieldEntry.Value := AValue;
end;


function TCmdLineObj.GetEntry: string;
begin
  Result := fFieldEntry.Value;
end;

procedure TCmdLineObj.SetIniSection(AValue: string);
begin
  if fIniSection = AValue then
    exit;
  fIniSection := AValue;
end;

constructor TCmdLineObj.Create( DBS : TInfoServer; theIniSection : string; iSO : integer );
var
  i : Integer;
begin

  fDataLocation := dlCmdLine;

  inherited Create( DBS );

  SetRelationSection( theIniSection );

  for i := Low( CmdStruct ) to High( CmdStruct ) do
  begin
    if CmdStruct[ i ].CmdObjType = cotCmdLine then
    begin
      case CmdStruct[ i ].FieldID of
        fidCmdFK :        CreateField( TObject( fFieldCmdFK ), CmdStruct[ i ] );
        fidSO :           CreateField( TObject( fFieldSO ), CmdStruct[ i ] );
        fidEntry :        CreateField( TObject( fFieldEntry ), CmdStruct[ i ] );
        fidFriendlyName : CreateField( TObject( fFieldFriendlyName ), CmdStruct[ i ] );
        fidWantsInput :   CreateField( TObject( fFieldWantsInput ), CmdStruct[ i ] );
        fidUseShell :     CreateField( TObject( fFieldUseShell ), CmdStruct[ i ] );
        fidAlert :        CreateField( TObject( fFieldAlert ), CmdStruct[ i ] );
      end;
    end;
  end;

  fFieldSO.Value := iSO;

end;

destructor TCmdLineObj.Destroy;
begin

  if assigned(fFieldCmdFK) then
    FreeAndNil(fFieldCmdFK);
  if assigned( fFieldSO ) then
    FreeAndNil( fFieldSO );
  if assigned(fFieldUseShell) then
    FreeAndNil(fFieldUseShell);
  if assigned(fFieldWantsInput) then
    FreeAndNil(fFieldWantsInput);
  if assigned(fFieldEntry) then
    FreeAndNil(fFieldEntry);
  if assigned(fFieldFriendlyName) then
    FreeAndNil(fFieldFriendlyName);
  if assigned(fFieldAlert) then
    FreeAndNil(fFieldAlert);

  inherited Destroy;

end;

function TCmdLineObj.Save( const OrderIdx : integer ) : boolean;
var
  strIdx: string;
begin

  fDBServer.SetControlSlot( fDataLocation );

  if not fDBServer.UseDB then
  begin
    strIdx := IntToStr( OrderIdx );
    fDBServer.WriteString(fRelationSection, strIdx, strIdx);
    fIniSection := format(ciniSub2Str, [fRelationSection, strIdx]);
  end;

  result := inherited Save;

end;


procedure TCmdLineObj.LoadDB( Idx : integer );
var
  i : Integer;
begin

  if not fDBServer.QueryIsOpen then
  begin
    //CLO's are loaded by owning CMD (which starts a Query to be read from) or by individual calls in Search Results.
    //The method to load by individual calls is very inefficient (makes a select query for EACH field). Upon thinking
    //seems to me that if there is no open query then it is alright to open one and get the efficiency of simply
    //reading the values. So I'm gonna try this and if all is good the other method can be removed.
    //The "flag" used by the fields to determine their method of load was if Idx < 0 a query is open, if not the
    //inefficient multiple queries are used (the call in the else clause) which uses Idx to determine what to do.

    try
     fDBServer.OpenQuery( fDataLocation, '*', 'rowid=' + inttostr( Idx ) );

      if fDBServer.QueryEOF then
        exit;

      for i := 0 to fFields.Count - 1 do
        TCOField( fFields[ i ] ).LoadSQLValue( cFieldIntegerTypeDefault );//flag to use query values, =\'s -1 at this time

    finally
      fDBServer.CloseQuery;
    end;

  end
  else
    for i := 0 to fFields.Count - 1 do
      TCOField( fFields[ i ] ).LoadSQLValue( Idx );
end;

procedure TCmdLineObj.LoadINI(Key: string);
var
  i: integer;
begin
  fIniSection := Key;
  for i := 0 to fFields.Count - 1 do
    TCOField( fFields[ i ] ).LoadIniValue;
end;

procedure TCmdLineObj.Load( const Key : string; const Idx : integer );
begin
  fDBServer.SetControlSlot( fDataLocation );
  if fDBServer.UseDB then
    LoadDB( Idx )
  else LoadINI( Key );
end;

procedure TCmdLineObj.Copy( aCmdLineObj : TCmdLineObj );
begin
  Assign( aCmdLineObj );
  IsNew := true;
end;

function TCmdLineObj.Merge( FromCmdObj : TCmdLineObj; const MergeSource : string ) : boolean;
var
  ExtraInfo : String;
begin

  ExtraInfo := '';

  //property CmdFK: not merged
  //property SO: not merged
  //property Entry: not merged

  if FriendlyName <> FromCmdObj.FriendlyName then
    ExtraInfo := ExtraInfo + format( cucoMerge_Boolean, [ Get_Cmd_Field_DisplayCaption( fidFriendlyName ) ] );

  if WantsInput <> FromCmdObj.WantsInput then
    ExtraInfo := ExtraInfo + format( cucoMerge_Boolean, [ Get_Cmd_Field_DisplayCaption( fidWantsInput ) ] );

  if UseShell <> FromCmdObj.UseShell then
    ExtraInfo := ExtraInfo + format( cucoMerge_Boolean, [ Get_Cmd_Field_DisplayCaption( fidUseShell ) ] );

  if Alert <> FromCmdObj.Alert then
    ExtraInfo := ExtraInfo + format( cucoMerge_Boolean, [ Get_Cmd_Field_DisplayCaption( fidAlert ) ] );

  result := BaseMerge( FromCmdObj, MergeSource, ExtraInfo ) or ( ExtraInfo <> '' );

  if ExtraInfo <> '' then
    Notes := format( cucoMerge_Inconsistency, [ DateToStr( now ), ExtraInfo ] ) + Notes;

end;

procedure TCmdLineObj.Assign(aCmdLineObj: TCmdLineObj);
begin

  Entry := aCmdLineObj.Entry;
  FriendlyName := aCmdLineObj.FriendlyName;
  WantsInput := aCmdLineObj.WantsInput;
  TerminalOnly := aCmdLineObj.TerminalOnly;
  Alert := aCmdLineObj.Alert;
  UseShell := aCmdLineObj.UseShell;
  IsFavorite := aCmdLineObj.IsFavorite;
  Notes := aCmdLineObj.Notes;
  DetachProcess := aCmdLineObj.DetachProcess;
  SuperUserMode := aCmdLineObj.SuperUserMode;
  ThreatLevel := aCmdLineObj.ThreatLevel;
//RowID, CmdFK, SO are not set here, automatically assigned
  IsNew := aCmdLineObj.IsNew;
  fDataLocation := aCmdLineObj.fDataLocation;
//autohandled  IniSection := aCmdLineObj.IniSection;

end;

function TCmdLineObj.DoUpdate: boolean;
begin
  Result := inherited DoUpdate;
end;

procedure TCmdLineObj.MarkFieldsForUpdate;
var
  i: integer;
begin
//currently, mostly because of text db requirements, all old cl's are deleted and then re-written out
//DoUpdate forces the field to give out its columnname and values.
//But now with sql IsNew is needed (doesn't affect text db) so that the new rowid is returned.
//later maybe I fix sql routines to be pinpoint precise and reuse rows in sql table.
  IsNew := true;
  for i := 0 to fFields.Count - 1 do
    TCOField(fFields[i]).DoUpdate := True;
end;

procedure TCmdLineObj.ResetUpdate;
begin
  inherited ResetUpdate;
end;

procedure TCmdLineObj.SetRelationSection(aSectionName: string);
begin
  fRelationSection := format(ciniSub2Str, [aSectionName, cIniSectionCmdLineObj]);
end;

function TCmdLineObj.GetDisplayString: string;
begin
  Result := inherited GetDisplayString + Entry;
end;

function TCmdLineObj.sqlGetCommandName : string;
begin
//fix for goto commandline in revamped search routines. Need the CL's command name
  fDBServer.SetControlSlot( dlCmd );//==> NOT <== cmdLine, using foreign key here on its Master Command
  result := fDBServer.FetchFromForeignKey( Get_Cmd_Field_ColumnName( fidCommandName ), inttoStr( CmdFK ) );
end;


{ TCmdObj }

procedure TCmdObj.SetCommandName(AValue: string);
begin
  if fFieldCommandName.Value = AValue then
    Exit;
  fFieldCommandName.Value := AValue;
end;

function TCmdObj.GetCommandName: string;
begin
  Result := fFieldCommandName.Value;
end;

procedure TCmdObj.SetKeywords(AValue: string);
begin
  if fFieldKeywords.Value = AValue then
    Exit;
  fFieldKeywords.Value := AValue;
end;


procedure TCmdObj.SetCmdLines(AValue: TStringList);
begin
  if fCmdLines = AValue then
    Exit;
  fCmdLines := AValue;
end;


procedure TCmdObj.SetHelpCommand(AValue: string);
begin
  if fFieldHelpCommand.Value = AValue then
    Exit;
  fFieldHelpCommand.Value := AValue;
end;


function TCmdObj.GetKeywords: string;
begin
  Result := fFieldKeywords.Value;
end;


function TCmdObj.GetHelpCommand: string;
begin
  Result := fFieldHelpCommand.Value;
end;

procedure TCmdObj.SetLocationPath(AValue: string);
begin
  if fFieldLocationPath.Value = AValue then
    Exit;
  fFieldLocationPath.Value := AValue;
end;


function TCmdObj.GetLocationPath: string;
begin
  Result := fFieldLocationPath.Value;
end;

function TCmdObj.GetVersionCommand: string;
begin
  Result := fFieldVersionCommand.Value;
end;


procedure TCmdObj.SetVersionCommand(AValue: string);
begin
  if fFieldVersionCommand.Value = AValue then
    Exit;
  fFieldVersionCommand.Value := AValue;
end;


procedure TCmdObj.SetIniSection(AValue: string);
begin
  if fIniSection = AValue then
    exit;
  fIniSection := AValue;
end;


constructor TCmdObj.Create( DBS : TInfoServer );
var
  i : Integer;
  s : String;
begin

  fDataLocation := dlCmd;
  fCmdLineDataLocation := dlCmdLine;

  inherited Create( DBS );

  fCmdLines := TStringList.Create;
  fLoadedCommandName := '';
  fTextDBSectionID := '<BadSect>';
  s := '';

  for i := Low( CmdStruct ) to High( CmdStruct ) do
  begin
    if CmdStruct[ i ].CmdObjType = cotCmd then
    begin

      case CmdStruct[ i ].FieldID of
        fidCommandName :    s := CreateField( TObject( fFieldCommandName ), CmdStruct[ i ] );
        fidKeywords :       s := CreateField( TObject( fFieldKeywords ), CmdStruct[ i ] );
        fidHelpCommand :    s := CreateField( TObject( fFieldHelpCommand ), CmdStruct[ i ] );
        fidVersionCommand : s := CreateField( TObject( fFieldVersionCommand ), CmdStruct[ i ] );
        fidLocationPath :   s := CreateField( TObject( fFieldLocationPath ), CmdStruct[ i ] );
      end;

      if s <> '' then
        fTextDBSectionID := s;//registering the control keys for Text DB's

    end;
  end;

end;

destructor TCmdObj.Destroy;
begin

  if assigned(fFieldCommandName) then
    FreeAndNil(fFieldCommandName);

  if assigned(fFieldKeywords) then
    FreeAndNil(fFieldKeywords);

  if assigned(fFieldHelpCommand) then
    FreeAndNil(fFieldHelpCommand);

  if assigned(fFieldVersionCommand) then
    FreeAndNil(fFieldVersionCommand);

  if assigned(fFieldLocationPath) then
    FreeAndNil(fFieldLocationPath);


  if assigned(fCmdLines) then
  begin
    FreeCmdLines;
    CmdLines.Free;
  end;

  inherited Destroy;

end;

class function TCmdObj.MergeCommands( SourceDB, DestDB : TInfoServer; const MergeSource : string ) : boolean;
var
  FromCmdObj, ToCmdObj : TCmdObj;
  FromCmdObjHelper, ToCmdObjHelper : TCmdObjHelper;
  Failure : Boolean;
  FromList, ToList : TStringList;
  i , Idx : Integer;
  MySlightSpeedup: Integer;

  procedure InitPointers;
  begin
//since I use one "finally" I want to be sure that initially no one is assigned just in case
    ToCmdObj := nil;
    FromCmdObj := nil;
    FromCmdObjHelper := nil;
    ToCmdObjHelper := nil;
    FromList := nil;
    ToList := nil;
  end;

  procedure CmdObjHelperInit( var aCmdObjHelper : TCmdObjHelper; anIS : TInfoServer; theList : TStringList );
  begin
    aCmdObjHelper := TCmdObjHelper.Create;
    aCmdObjHelper.DBServer := anIS;
    aCmdObjHelper.GetCmdList( theList );
  end;

begin
  result := true;

  try

    InitPointers;

    FromList := TStringlist.Create;
    FromList.CaseSensitive := true;

    ToList := TStringlist.Create;
    ToList.CaseSensitive := true;

    CmdObjHelperInit( FromCmdObjHelper, SourceDB, FromList );
    CmdObjHelperInit( ToCmdObjHelper, DestDB, ToList );

//need to figure out what is with sqlite with mergecommands and mergecommand
//Everything is correct here but without the special fix call (immediate commit) the transaction
//gets confused and drops insertions. This does not happen in save or convert for example (as far as I've tried)
//So this is a total hack...that works. This is only an issue when merging TO a sql DB.
    DestDB.Initiate_Save;

    try
//Man this problem is REALLY bugging me, the slowdown doesn't happen in IDE but does in release
//for merging TO sql files, everything else is fine and this code IS CORRECT, that is what is really buggin me.
//Anyway, this makes merging to sql's as fast as possible, worst case is simply terrible so slowwwww.
//but in many cases that would be more normal it is an improvement...until I find out, if ever, what this
//ridiculous fucking problem is.  Tested and it, finally, with reasonable-ish speed, makes a true good complete
//merge in cases of TO sql db's. Fuck me. What a fucking pain.
MySlightSpeedup := 0;

      for i := 0 to FromList.Count - 1 do
      begin
        Failure := true;
        try
          FromCmdObjHelper.LoadCmdObj( FromCmdObj, FromList[ i ] );
          Idx := ToList.IndexOf( FromCmdObj.CommandName );

          if Idx < 0 then //not present in Destination
          begin
            //if MySlightSpeedup = 2 then
            //  DestDB.FIX_MERGE;
            MySlightSpeedup := 1;

            FromCmdObj.ResetDBServer( DestDB, true );
            FromCmdObj.Notes := format( cucoMerge_NewObj, [ DateToStr( now ), MergeSource ] )
                                + FromCmdObj.Notes;
            Failure := not FromCmdObj.Save;
            //DestDB.FIX_MERGE;
          end
          else
          begin //present in Destination
            if MySlightSpeedup = 1 then
              DestDB.FIX_MERGE;
            MySlightSpeedup := 2;

            ToCmdObjHelper.LoadCmdObj( ToCmdObj, ToList[ Idx ] );

            if ToCmdObj.Merge( FromCmdObj, MergeSource ) then
            begin
              Failure := not ToCmdObj.Save;
              DestDB.FIX_MERGE;
            end
            else Failure := false;

          end;

        finally
          if assigned( FromCmdObj ) then
            FreeAndNil( FromCmdObj );
          if assigned( ToCmdObj ) then
            FreeAndNil( ToCmdObj );
          if Failure then
            raise EErrorDatabaseProblem.create( 'TCmdObj.MergeCommands: problem saving command, conversion halted.' );
        end;
      end;

      DestDB.Finalize_Save( 1 );
      //DestDB.Finalize_Save( 0 );//commits are already done my FIX_MERGE

    except
      on e:exception do
      begin
        DestDB.Finalize_Save( -1 );
        EErrorRaise( e, cEErrorDatabaseProblem, 'TCmdObj.MergeCommands: ' );
      end;
    end;

  finally

    if assigned( FromList ) then
      FreeAndNil( FromList );

    if assigned( ToList ) then
      FreeAndNil( ToList );

    if assigned( ToCmdObjHelper ) then
      FreeAndNil( ToCmdObjHelper );

    if assigned( FromCmdObjHelper ) then
      FreeAndNil( FromCmdObjHelper );
  end;

end;

class function TCmdObj.CompareCommands( SourceDB, DestDB : TInfoServer; Strings : TStrings;
                                        const frName, toName : string) : boolean;
var
  FromCmdObj, ToCmdObj : TCmdObj;
  FromCmdObjHelper, ToCmdObjHelper : TCmdObjHelper;
  FromList, ToList : TStringList;
  i, j, Idx, CmdLineCnt : Integer;
  //i, j, Idx, FromCmdCnt, FromCmdLineCnt, ToCmdCnt, ToCmdLineCnt : Integer;
  FromSummary, InDest, InDestNot, InterimResult : string;
  IsIdentical : boolean;

  procedure InitPointers;
  begin
//since I use one "finally" I want to be sure that initially no one is assigned just in case
    ToCmdObj := nil;
    FromCmdObj := nil;
    FromCmdObjHelper := nil;
    ToCmdObjHelper := nil;
    FromList := nil;
    ToList := nil;

    CmdLineCnt := 0;

  end;

  procedure CmdObjHelperInit( var aCmdObjHelper : TCmdObjHelper; anIS : TInfoServer; theList : TStringList );
  begin
    aCmdObjHelper := TCmdObjHelper.Create;
    aCmdObjHelper.DBServer := anIS;
    aCmdObjHelper.GetCmdList( theList );
  end;

const
  NamePath = '%s      ( %s )';
begin

  result := false;

  try

    InitPointers;

    FromList := TStringlist.Create;
    FromList.CaseSensitive := true;

    ToList := TStringlist.Create;
    ToList.CaseSensitive := true;

    CmdObjHelperInit( FromCmdObjHelper, SourceDB, FromList );
    FromList.Sort;
    CmdObjHelperInit( ToCmdObjHelper, DestDB, ToList );
    ToList.Sort;

    try

      InDest := '';
      InDestNot := '';
      FromSummary := '';
      InterimResult := '';

      for i := 0 to FromList.Count - 1 do
      begin
        try
          FromCmdObjHelper.LoadCmdObj( FromCmdObj, FromList[ i ] );
          inc( CmdLineCnt, FromCmdObj.CmdLines.Count );

          FromSummary := FromSummary + format( NamePath, [ FromCmdObj.CommandName, FromCmdObj.GetLiteralPath ] );
          for j := 0 to FromCmdObj.CmdLines.Count - 1 do
            FromSummary := FromSummary + LineEnding + '  ' + TCmdLineObj( FromCmdObj.CmdLines.Objects[ j ] ).Entry;
          FromSummary := FromSummary + LineEnding;// + LineEnding;

          Idx := ToList.IndexOf( FromCmdObj.CommandName );

          if Idx < 0 then //not present in Destination
          begin
            InDestNot := InDestNot + format( cmsgInDestNot, [ toName ] ) + FromCmdObj.CommandName + LineEnding;
          end
          else
          begin //present in Destination

            ToCmdObjHelper.LoadCmdObj( ToCmdObj, ToList[ Idx ] );
            InDest := InDest + format( cmsgInDest, [ toName ] )
                      + format( NamePath, [ ToCmdObj.CommandName, ToCmdObj.GetLiteralPath ] )
                      + LineEnding;

            IsIdentical := false;
            InterimResult := ToCmdObj.Compare( FromCmdObj, IsIdentical );
            if IsIdentical then
              InterimResult := format( cCLInfo, [ cNameItem_CommandLinePlur, ccapCompareIdentical ] ) + LineEnding + LineEnding;

            InDest := InDest + InterimResult;

          end;

        finally
          if assigned( FromCmdObj ) then
            FreeAndNil( FromCmdObj );
          if assigned( ToCmdObj ) then
            FreeAndNil( ToCmdObj );
        end;
      end;

      if FromSummary <> '' then
        FromSummary := format( cmsgCompareSummary, [ frName ] ) + LineEnding + LineEnding + FromSummary;

      Strings.Text := Strings.Text + InDest + InDestNot + FromSummary;

      Strings.Add( '' );
      Strings.Add( format( cmsgDBStatistics, [ frName, FromList.Count, CmdLineCnt ] ) );
      Strings.Add( '' );
      Strings.Add( '' );
      Strings.Add( '' );
      Strings.Add( '' );
      Strings.Add( format( cmsgCompareSummary, [ toName ] ) );
      Strings.Add( '' );
      Strings.Add( '' );

      CmdLineCnt := 0;

      for i := 0 to ToList.Count - 1 do
      begin
        try

          ToCmdObjHelper.LoadCmdObj( ToCmdObj, ToList[ i ] );
          inc( CmdLineCnt, ToCmdObj.CmdLines.Count );

          Strings.Add( format( NamePath, [ ToCmdObj.CommandName, ToCmdObj.GetLiteralPath ] ) );


          for j := 0 to ToCmdObj.CmdLines.Count - 1 do
            Strings.Add( '  ' + TCmdLineObj( ToCmdObj.CmdLines.Objects[ j ] ).Entry );

        finally
          if assigned( ToCmdObj ) then
            FreeAndNil( ToCmdObj );
        end;
      end;

      Strings.Add( '' );
      Strings.Add( format( cmsgDBStatistics, [ toName, ToList.Count, CmdLineCnt ] ) );
      Strings.Add( '' );
      Strings.Add( '' );

    except
      on e:exception do
        EErrorRaise( e, cEErrorDatabaseProblem, 'TCmdObj.CompareCommands: ' );
    end;

  finally

    if assigned( FromList ) then
      FreeAndNil( FromList );

    if assigned( ToList ) then
      FreeAndNil( ToList );

    if assigned( ToCmdObjHelper ) then
      FreeAndNil( ToCmdObjHelper );

    if assigned( FromCmdObjHelper ) then
      FreeAndNil( FromCmdObjHelper );
  end;

  result := true;

end;

class function TCmdObj.MergeCommand( DestDB : TInfoServer; CO : TCmdObj; const MergeSource : string ) : string;
var
  ToCmdObj : TCmdObj;
  ToCmdObjHelper : TCmdObjHelper;
  Failure : Boolean;
  ToList : TStringList;
  Idx: Integer;

  procedure InitPointers;
  begin
//since I use one "finally" I want to be sure that initially no one is assigned just in case
    ToCmdObj := nil;
    ToCmdObjHelper := nil;
    ToList := nil;
  end;

  procedure CmdObjHelperInit( var aCmdObjHelper : TCmdObjHelper; anIS : TInfoServer; theList : TStringList );
  begin
    aCmdObjHelper := TCmdObjHelper.Create;
    aCmdObjHelper.DBServer := anIS;
    aCmdObjHelper.GetCmdList( theList );
  end;

begin

  result := '<unknown>';

  try

    InitPointers;

    ToList := TStringlist.Create;
    ToList.CaseSensitive := true;

    CmdObjHelperInit( ToCmdObjHelper, DestDB, ToList );

    Failure := false;
    try

      DestDB.Initiate_Save;

      try

        Idx := ToList.IndexOf( CO.CommandName );
        if Idx < 0 then //not present in Destination
        begin

          ToCmdObjHelper.CreateCmdObj( ToCmdObj );
          ToCmdObj.Copy( CO, CO.CommandName );
          TOCmdObj.Notes := format( cucoMerge_NewObj, [ DateToStr( now ), MergeSource ] )
                              + ToCmdObj.Notes;
          Failure := not ToCmdObj.Save;
          //DestDB.FIX_MERGE;
          result := strif( not Failure, format( cmsgucoMerge_Added, [ ToCmdObj.CommandName ] ) )
                    + ': ' + MergeSource;
        end
        else
        begin //present in Destination

          ToCmdObjHelper.LoadCmdObj( ToCmdObj, CO.CommandName );

          if ToCmdObj.Merge( CO, MergeSource ) then
          begin
            Failure := not ToCmdObj.Save;
            //DestDB.FIX_MERGE;
           end;
          result := strif( not Failure, format( cmsgucoMerge_Existing, [ ToCmdObj.CommandName ] ) );

        end;

        DestDB.Finalize_Save( 1 );
        //DestDB.Finalize_Save( 0 );//commits are already done my FIX_MERGE
      except
        on e:exception do
        begin
          DestDB.Finalize_Save( -1 );
          EErrorRaise( e, cEErrorDatabaseProblem, 'TCmdObj.MergeCommand: ' );
        end;
      end;

    finally
      if assigned( ToCmdObj ) then
        FreeAndNil( ToCmdObj );
      if Failure then
        raise EErrorDatabaseProblem.create( 'TCmdObj.MergeCommand: '
                                            + format( cmsgucoError_MergeConvSaving, [ cmsgucoError_Merge ] )
                                           );
    end;


  finally

    if assigned( ToList ) then
      FreeAndNil( ToList );

    if assigned( ToCmdObjHelper ) then
      FreeAndNil( ToCmdObjHelper );

  end;

end;

class function TCmdObj.ConvertCommands( SourceDB, DestDB : TInfoServer) : boolean;
var
  aCmdObjHelper : TCmdObjHelper;
  i : Integer;
  SL : TStringList;
  Failure : Boolean;
  aCmdObj : TCmdObj;
begin
  result := false;
  aCmdObjHelper := nil;
  aCmdObj := nil;
  try
    SL := TStringList.Create;
    try
      aCmdObjHelper := TCmdObjHelper.Create;
      aCmdObjHelper.DBServer := SourceDB;
      aCmdObjHelper.GetCmdList( SL );

      DestDB.Initiate_Save;

      for i := 0 to SL.Count - 1 do
      begin
        Failure := true;
        try
          aCmdObjHelper.LoadCmdObj( aCmdObj, SL[ i ] );
          aCmdObj.ResetDBServer( DestDB, true );
          Failure := not aCmdObj.Save;
        finally
          if assigned( aCmdObj ) then
            FreeAndNil( aCmdObj );
          if Failure then
            raise EErrorDatabaseProblem.create( format( cmsgucoError_MergeConvSaving, [ cmsgucoError_Converting ] ) );
        end;
      end;

//      raise exception.create( 'Ouch!' );
      DestDB.Finalize_Save( 1 );

    finally
      if assigned( SL ) then
        FreeAndNil( SL );
      if assigned( aCmdObjHelper ) then
        FreeAndNil( aCmdObjHelper );
    end;

    result := true;

  except

    on e:exception do
    begin
      DestDB.Finalize_Save( -1 );
      EErrorRaise( e, cEErrorDatabaseProblem, 'TCmdObj.ConvertCommands: ' );
    end;

  end;

end;


procedure TCmdObj.FreeCmdLines;
var
  i: integer;
begin
  for i := 0 to CmdLines.Count - 1 do
  begin
    if assigned( CmdLines.Objects[ i ] ) then
    begin
      CmdLines.Objects[ i ].free;
      CmdLines.Objects[ i ] := nil;
    end;
  end;
  CmdLines.Clear;
end;

function TCmdObj.DoClearCmdLines: boolean;
var
  i: integer;
begin
  //due to misery, uh, I mean feature of INI files if any CommandLine is changed
  //then all CmdLines must be re-written after being cleared out of ini file.
  //for now I'm using this with sql too, could do individual updates but then need an ORDER field for sort order

  Result := False;
  for i := 0 to CmdLines.Count - 1 do
  begin
    if TCmdLineObj(CmdLines.Objects[i]).DoUpdate then
    begin
      Result := True;
      break;
    end;
  end;

end;

procedure TCmdObj.ClearCmdLinesOutofIniFile( const TheName : string );
var
  TheSection, TheSubSection: string;
  Idx: integer;
begin

  if TheName = '' then
    exit;

  TheSection := format( ciniSub2Str, [ TheName, cIniSectionCmdLineObj ] );
  fDBServer.EraseSection( TheSection );

  Idx := 0;
  TheSubSection := format( ciniSub2Str, [ TheSection, IntToStr( Idx ) ] );
  while fDBServer.SectionExists( TheSubSection ) do
  begin
    fDBServer.EraseSection( TheSubSection );
    Inc( Idx );
    TheSubSection := format( ciniSub2Str, [TheSection, IntToStr( Idx ) ] );
  end;

end;


procedure TCmdObj.SaveDB;
begin

//===> be aware that the Infoserver's .Initiate_Save; MUST be called around CmdObj saves.

  fDBServer.SetControlSlot( fDataLocation );
  fDBServer.NeedsCommit := true;

//sqlite doesn't "do" nested transactions, so the transaction is manually handled over all obj saves
  try

    inherited Save;

    if DoClearCmdLines then
    begin

      fDBServer.SetControlSlot( fCmdLineDataLocation );//Detail Index
      fDBServer.DeleteItems_ManTran_sql( fCmdObjHelper.CmdLine_ControlColumn_sql + '=' + inttostr( RowID ) );

      PrepCommandLinesForReWrite;
      SaveCommandLines;

    end;

  except
    on e:exception do
      EErrorRaise( e, cEErrorDatabaseProblem, 'TCmdObj.SaveDB: ' );
  end;

end;

procedure TCmdObj.PrepCommandLinesForReWrite;
var
  i: integer;
  CmdLineObj: TCmdLineObj;
begin
  for i := 0 to CmdLines.Count - 1 do
  begin
//Need to set the Command Lines to update so they will be saved with the new data
    CmdLineObj := TCmdLineObj( CmdLines.Objects[ i ] );
    CmdLineObj.MarkFieldsForUpdate;
    CmdLineObj.SetRelationSection( IniSection );
  end;
end;

procedure TCmdObj.SaveCommandLines;
var
  i: integer;
  CmdLineObj: TCmdLineObj;
begin
//reverse order remove dodeletes
  for i := CmdLines.Count - 1 downto 0 do
  begin
    CmdLineObj := TCmdLineObj( CmdLines.Objects[ i ] );
    if CmdLineObj.DoDelete then
    begin
      FreeAndNil( CmdLineObj );
      CmdLines.Delete( i );
    end;
  end;

//straight order so lines are in exact matching order
  for i := 0 to CmdLines.Count - 1 do
  begin
    CmdLineObj := TCmdLineObj( CmdLines.Objects[ i ] );
    CmdLineObj.SO := i;
    CmdLineObj.CmdFK := RowID;
    CmdLineObj.Save( i );
    CmdLines[ i ] := CmdLineObj.Entry;
  end;
end;

procedure TCmdObj.SaveCmdLineINI;
begin

  PrepCommandLinesForReWrite;

  fDBServer.SetControlSlot( fCmdLineDataLocation );//Detail Index

  //If command is renamed need to clear out old, now invalid, sections
  //Favs/Lines must be re-written when any one of them change so that order is kept
  //accurate. Could maybe come up with an indexing scheme to handle it all but
  //that, I think, would be much more complex.
  //Belt and Suspenders, call both of these to ensure all traces of old gone
  ClearCmdLinesOutofIniFile(fLoadedCommandName);
  ClearCmdLinesOutofIniFile(IniSection);

  SaveCommandLines;

end;

procedure TCmdObj.SaveIni;
var
  NeedCleanup: boolean;
begin

  fDBServer.SetControlSlot( fDataLocation );

  NeedCleanUp := (fLoadedCommandName <> '') and (fLoadedCommandName <> CommandName);

  if NeedCleanUp then
    fFieldCommandName.CleanUpChangedKeyField(fLoadedCommandName);

  inherited Save;

  if DoClearCmdLines or NeedCleanup then
    SaveCmdLineINI;

end;

function TCmdObj.Save : boolean;
var
  i : Integer;
begin

  result := false;
  try

    if fIsNew then
    begin
//for newly added objects force fields to give back their data which is dependant on field's DoUpdate property
      for i := 0 to fFields.Count - 1 do
        TCOField( fFields[ i ] ).DoUpdate := True;
    end;

    if not DoUpdate then
      exit;

    if fDBServer.UseDB then
      SaveDB
    else SaveINI;

    fLoadedCommandName := CommandName;

    fIsNew := false;
    result := true;

  except

    on e:exception do
      EErrorRaise( e, cEErrorDatabaseProblem, 'TCmdObj.Save: ' );
 //don't use commandname, if there is a serous assignment problem it will raise another error
//raise EErrorDatabaseProblem.Create( format( 'TCmdObj.Save: command "%s" -- ', [ CommandName ] ) + e.message );
  end;

end;


procedure TCmdObj.LoadDB( const Key, Col : string; const Idx : integer );
var
  i, SortOrder : integer;
  aLine: TCmdLineObj;
begin

  try

    if Idx > 0 then
      fDBServer.OpenQuery( fDataLocation, '*', 'rowid=' + Key ) //searches return indexes, internal uses string
//Key here is a linux command, hopefully no one ever makes linux commands that contain " or ' because that would
//be a problem. This is why a string should never be a KEY, but supporting text db vs sql db required it at the
//time of text db development. It could be in the future maybe fixed so this would NEVER be a problem, but not right now.
    else fDBServer.OpenQuery( fDataLocation, '*', Col + '=' + QuotedStr( Key ) );

    if fDBServer.QueryEOF then
      exit;

    for i := 0 to fFields.Count - 1 do
      TCOField( fFields[ i ] ).LoadSQLValue( cFieldIntegerTypeDefault );//flag to use query values

  finally
    fDBServer.CloseQuery;
  end;

//load cmdlines

  FreeCmdLines;
  SortOrder := -1;
  aLine := nil;

  try

    fDBServer.OpenQuery( fCmdLineDataLocation, '*', fCmdObjHelper.CmdLine_ControlColumn_sql + '=' + inttostr( Self.RowID ) );
    while not fDBServer.QueryEof do
    begin
      inc( SortOrder );
      fCmdObjHelper.LoadCmdLineObj(
           aLine,
           CommandName,
           inttostr( cFieldIntegerTypeDefault ),//flag, don't pay attention to rowid, they are in an open query
           SortOrder
                                  );

      CmdLines.AddObject( ALine.Entry, ALine );

      fDBServer.QueryNext;

    end;

  finally
    fDBServer.CloseQuery;
  end;

end;

procedure TCmdObj.LoadINI( const Key : string );
var
  i: integer;
  SL: TStringList;
  CmdLineList: string;
  aLine: TCmdLineObj;
begin

  fDBServer.SetControlSlot( fDataLocation );

  aLine := nil;
  for i := 0 to fFields.Count - 1 do
    TCOField( fFields[ i ] ).LoadIniValue;

  fDBServer.SetControlSlot( fCmdLineDataLocation );

  FreeCmdLines;

  SL := TStringList.Create;
  try
    CmdLineList := format( ciniSub2Str, [ Key, cIniSectionCmdLineObj ] );
    fDBServer.LoadList( CmdLineList, SL );
    for i := 0 to SL.Count - 1 do
    begin
      fCmdObjHelper.LoadCmdLineObj( aLine,
                                   CommandName,
                                   format( ciniSub2Str, [ CmdLineList, SL[ i ] ] ),
                                   i
                                  );

      CmdLines.AddObject( ALine.Entry, ALine );
    end;

  finally
    SL.Free;
    ALine := nil;
  end;

end;

procedure TCmdObj.Load( const Key, Col : string; const Idx : integer );
begin

  fIniSection := Key;

  if fDBServer.UseDB then
    LoadDB( Key, Col, Idx )
  else LoadINI(Key);

  fLoadedCommandName := CommandName;

end;

function TCmdObj.RemoveData : boolean;
begin

  result := false;
  try

    if fDBServer.UseDB then
    begin

      fDBServer.NeedsCommit := true;
      fDBServer.DeleteItems_ManTran_sql(
                     GetDelete_sql(
                                    GetTableNames( fCmdLineDataLocation, true ),
                                    fCmdObjHelper.CmdLine_ControlColumn_sql + '=' + inttostr( RowID )
                                   ),
                                   false
                                       );
      fDBServer.DeleteItems_ManTran_sql(
              GetDelete_sql( GetTableNames( fDataLocation, true ), csoSqlite_rowid + '=' + inttostr( RowID ) ),
              false
                                       );
    end
    else
    begin

      fDBServer.SetControlSlot( fDataLocation );

      fDBServer.DeleteKey( fTextDBSectionID, fTextDBKeyField.Value );
      fDBServer.EraseSection( fTextDBKeyField.Value );

      fDBServer.SetControlSlot( fCmdLineDataLocation );//Detail Index

      ClearCmdLinesOutofIniFile( IniSection );

    end;
    result := true;

  except
    on e:exception do
      EErrorRaise( e, cEErrorDatabaseProblem, 'TCmdObj.RemoveData: ' );
  end;

end;

procedure TCmdObj.Copy( aCmdObj : TCmdObj; const NewName : string );
var
  i , Idx: Integer;
  NewCmdLineObj , srcCmdLineObj: TCmdLineObj;
begin

//Need Unique
  CommandName := NewName;
  LocationPath := aCmdObj.LocationPath;

//  fLoadedCommandName initialized on create
  Keywords := aCmdObj.Keywords;
  HelpCommand := aCmdObj.HelpCommand;
  VersionCommand := aCmdObj.VersionCommand;
  ThreatLevel := aCmdObj.ThreatLevel;
//base
  IsFavorite := aCmdObj.IsFavorite;
  Notes := aCmdObj.Notes;
  DetachProcess := aCmdObj.DetachProcess;
  SuperUserMode := aCmdObj.SuperUserMode;
  TerminalOnly := aCmdObj.TerminalOnly;
//RowID is not set here, automatically assigned
  IsNew := true;
  fDataLocation := aCmdObj.fDataLocation;
//autohandled  IniSection := aCmdObj.IniSection;

  for i := 0 to aCmdObj.CmdLines.Count - 1 do
  begin
    srcCmdLineObj := TCmdLineObj( aCmdObj.CmdLines.Objects[ i ] );

    Idx := Self.AddCommandLine( srcCmdLineObj.Entry );
    NewCmdLineObj := TCmdLineObj( Self.CmdLines.Objects[ Idx ] );
    NewCmdLineObj.Assign( srcCmdLineObj );
    NewCmdLineObj.IsNew := true;

  end;

end;

function TCmdObj.MergeKeyWords( FromCmdObj : TCmdObj ) : boolean;
var
  FromKeyList , ToKeyList: TStringList;
  i , Idx: Integer;
begin

  result := false;
  FromKeyList := nil;
  ToKeyList := nil;
  try
    FromKeyList := TStringList.Create;
    ToKeyList := TStringList.Create;

    FromKeyList.Text := FromCmdObj.KeyWords;
    ToKeyList.Text := Keywords;
//comparetext is case insensitive
    if CompareText( FromKeyList.Text, ToKeyList.Text ) <> 0 then
    begin
      result := true;
      for i := 0 to FromKeyList.Count - 1 do
      begin
        Idx := ToKeyList.IndexOf( FromKeyList[ i ] );
        if Idx < 0 then
          ToKeyList.Add( FromKeyList[ i ] );
      end;
      KeyWords := ToKeyList.Text;
    end;

  finally
    if assigned( ToKeyList ) then
      FreeAndNil( ToKeyList );

    if assigned( FromKeyList ) then
      FreeAndNil( FromKeyList );
  end;

end;


function TCmdObj.Merge_CmdLines( FromCmdObj : TCmdObj; const MergeSource : string ) : boolean;
var
  CLOList : TStringList;
  i , Idx, CLOIdx : integer;
//  i , Idx : integer;
  FromCLO , ToCLO: TCmdLineObj;
begin

  result := false;

  CLOList := TStringlist.Create;
  try
    CLOList.CaseSensitive := true;

    for i := 0 to CmdLines.Count - 1 do
      CLOList.Add( TCmdLineObj( CmdLines.Objects[ i ] ).Entry  );

    for i := 0 to FromCmdObj.CmdLines.Count - 1 do
    begin
      FromCLO := TCmdLineObj( FromCmdObj.CmdLines.Objects[ i ] );//pointer only

      Idx := CLOList.IndexOf( FromCLO.Entry );
      if Idx < 0 then
      begin
        CLOIdx := AddCommandLine( FromCLO.Entry );
        ToCLO := TCmdLineObj( CmdLines.Objects[ CLOIdx ] );
        ToCLO.Assign( FromCLO );
        ToCLO.IsNew := true;
        ToCLO.Notes := format( cucoMerge_NewObj, [ DateToStr( now ), MergeSource ] )
                      + ToCLO.Notes;
        result := true;
      end
      else
      begin
        ToCLO := TCmdLineObj( CmdLines.Objects[ Idx ] );//pointer only
        result := result or ToCLO.Merge( FromCLO, MergeSource );
      end;
    end;

  finally
    if assigned( CLOList ) then
      FreeAndNil( CLOList );
  end;


end;

function TCmdObj.Compare_CmdLines( FromCmdObj : TCmdObj; var Identical : boolean ) : string;
var
  CLOList : TStringList;
  i , Idx : integer; //, CLOIdx: Integer;
  FromCLO : TCmdLineObj;//, ToCLO: TCmdLineObj;
  InList, InListNot, InDest : string;
begin

  result := '';
  InList := '';
  InListNot := '';
  InDest := '';

  CLOList := TStringlist.Create;
  try
    CLOList.CaseSensitive := true;

    for i := 0 to CmdLines.Count - 1 do
      CLOList.Add( TCmdLineObj( CmdLines.Objects[ i ] ).Entry  );

    for i := 0 to FromCmdObj.CmdLines.Count - 1 do
    begin
      FromCLO := TCmdLineObj( FromCmdObj.CmdLines.Objects[ i ] );//pointer only

      Idx := CLOList.IndexOf( FromCLO.Entry );
      if Idx < 0 then
      begin
        InListNot := InListNot + '    ' + FromCLO.Entry + LineEnding;
      end
      else
      begin
        InList := InList + '    ' + CLOList[ Idx ] + LineEnding;
      end;
    end;

    for i := 0 to CmdLines.Count - 1 do
    begin
      FromCLO := TCmdLineObj( CmdLines.Objects[ i ] );//pointer only

      Idx :=  FromCmdObj.CmdLines.IndexOf( FromCLO.Entry );

      if Idx < 0 then
        InDest := InDest + '    ' + FromCLO.Entry + LineEnding;

    end;

    if InListNot <> '' then
      InListNot := cmsgCLIsNot + LineEnding + InListNot;
    if InList <> '' then
      InList := cmsgCLIs + LineEnding + InList;
    if InDest <> '' then
      InDest := cmsgCLDest + LineEnding + InDest;

    Identical := ( InListNot = '' ) and ( InDest = '' );

    if ( InListNot <> '' ) or ( InList <> '' ) or ( InDest <> '' ) then
      Result := format( cCLInfo, [ cNameItem_CommandLinePlur, '' ] ) + Lineending + InList + InListNot + InDest + LineEnding
    else result := format( cCLInfo, [ cNameItem_CommandLinePlur, cmsgCLsNone ] ) + LineEnding + LineEnding;

  finally
    if assigned( CLOList ) then
      FreeAndNil( CLOList );
  end;

end;

function TCmdObj.Merge( FromCmdObj : TCmdObj; const MergeSource : string ) : boolean;
var
  ExtraInfo : String;
  InterimResult : Boolean;
begin

  result := false;
  ExtraInfo := '';

  //property CommandName: Not merged
  //property HelpCommand: Not merged
  //property VersionCommand: Not merged

  if LocationPath <> FromCmdObj.LocationPath then
  begin
    ExtraInfo := ExtraInfo + format( cucoMerge_String,
                   [ Get_Cmd_Field_DisplayCaption( fidLocationPath ), FromCmdObj.LocationPath ]
                                   );
  end;

  InterimResult := MergeKeywords( FromCmdObj );

  result := result or InterimResult;

  InterimResult := BaseMerge( FromCmdObj, MergeSource, ExtraInfo ) or ( ExtraInfo <> '' );

  result := result or InterimResult;

  if ExtraInfo <> '' then
    Notes := format( cucoMerge_Inconsistency, [ DateToStr( now ), ExtraInfo ] ) + Notes;

//Another ODD problem, the original line (remmed below; its the last line) DIDNT work!!!
//It literally WOULD NOT STEP into Merge_CmdLines, so now I use InterimResult. Fuck me.
  InterimResult := Merge_CmdLines( FromCmdObj, MergeSource );

  result := result or InterimResult;
//  result := result or Merge_CmdLines( FromCmdObj, MergeSource );

end;

function TCmdObj.Compare( FromCmdObj : TCmdObj; var Identical : boolean ) : string;
var
  FromP, ToP : String;
  CmdIdentical, CmdLineIdentical : boolean;
begin

    result := '';
    CmdIdentical := false;
    CmdLineIdentical := false;

    ToP := GetLiteralPath;
    FromP := FromCmdObj.GetLiteralPath;

    if ToP <> FromP then
      result := format( cmsgPathsDiffer, [ ToP, FromP ] ) + LineEnding
    else CmdIdentical := true;


    result := Result + Compare_CmdLines( FromCmdObj, CmdLineIdentical ) + LineEnding;

    Identical :=  CmdIdentical and CmdLineIdentical;

end;

//procedure TCmdObj.Assign( aCmdObj: TCmdObj );
//var
//  i , Idx: Integer;
//  NewCmdLineObj , srcCmdLineObj: TCmdLineObj;
//begin
////===================================
////Never found a need for an assign to this object, when necessary I use copy
////should this ever need to be done...be careful.
////===================================
//
////Need Unique
//  CommandName := NEWNAME;
//  LocationPath := aCmdObj.LocationPath;
//
////  fLoadedCommandName initialized on create
//  Keywords := aCmdObj.Keywords;
//  HelpCommand := aCmdObj.HelpCommand;
//  VersionCommand := aCmdObj.VersionCommand;
//  ThreatLevel := aCmdObj.ThreatLevel;
////base
//  IsFavorite := aCmdObj.IsFavorite;
//  Notes := aCmdObj.Notes;
//  DetachProcess := aCmdObj.DetachProcess;
//  SuperUserMode := aCmdObj.SuperUserMode;
//  TerminalOnly := aCmdObj.TerminalOnly;
//  IsNew := aCmdObj.IsNew;
//  fDataLocation := aCmdObj.fDataLocation;
////autohandled  IniSection := aCmdObj.IniSection;
//
//  for i := 0 to aCmdObj.CmdLines.Count - 1 do
//  begin
//    srcCmdLineObj := TCmdLineObj( aCmdObj.CmdLines.Objects[ i ] );
//
//    Idx := Self.AddCommandLine( srcCmdLineObj.Entry );
//    NewCmdLineObj := TCmdLineObj( Self.CmdLines.Objects[ Idx ] );
//    NewCmdLineObj.Assign( srcCmdLineObj );
//    NewCmdLineObj.IsNew := true;
//
//  end;
//
//end;


function TCmdObj.AsText : string;
var
  CmdString : string;
  aCmdLineObj : TCmdLineObj;
  i : Integer;
const
  NormalLeader = '%s : ';
  UpWrapper = '%s -------------------';
  DownWrapper = 'End %s -------------------';

begin

  result := '< TCmdObj.AsText: error >';
  CmdString := format( NormalLeader, [ Get_Cmd_Field_DisplayCaption( fidCommandName ) ] ) + CommandName + LineEnding;

  CmdString := CmdString + format( NormalLeader, [ Get_Cmd_Field_DisplayCaption( fidLocationPath ) ] ) + LocationPath + LineEnding;
  CmdString := CmdString + format( NormalLeader, [ Get_Cmd_Field_DisplayCaption( fidRowID ) ] ) + inttostr( RowID ) + LineEnding;

//  CmdString := CmdString + 'Keywords : ' + stringreplace( CmdObj.Keywords, csoNonPrintingDelimiter, ',', [rfreplaceall] ) + LineEnding;
  CmdString := CmdString + format( UpWrapper, [ Get_Cmd_Field_DisplayCaption( fidKeyWords ) ] ) + LineEnding;
  CmdString := CmdString + Keywords;
  CmdString := CmdString + format( DownWrapper, [ Get_Cmd_Field_DisplayCaption( fidKeyWords ) ] ) + LineEnding;

  CmdString := CmdString + format( NormalLeader, [ Get_Cmd_Field_DisplayCaption( fidHelpCommand ) ] ) + HelpCommand + LineEnding;

  CmdString := CmdString + format( NormalLeader, [ Get_Cmd_Field_DisplayCaption( fidVersionCommand ) ] ) + VersionCommand + LineEnding;

  CmdString := CmdString + format( NormalLeader, [ Get_Cmd_Field_DisplayCaption( fidThreatLevel ) ] ) + fCmdObjHelper.GetThreatLevelText( ThreatLevel ) + LineEnding;

//base
  CmdString := CmdString + format( NormalLeader, [ Get_Cmd_Field_DisplayCaption( fidIsFavorite ) ] ) + strif( IsFavorite, 'true', 'false' ) + LineEnding;

  CmdString := CmdString + format( NormalLeader, [ Get_Cmd_Field_DisplayCaption( fidDetachProcess ) ] ) + strif( DetachProcess, 'true', 'false' ) + LineEnding;

  CmdString := CmdString + format( NormalLeader, [ Get_Cmd_Field_DisplayCaption( fidSuperUserMode ) ] ) + strif( SuperUserMode, 'true', 'false' ) + LineEnding;

  CmdString := CmdString + format( NormalLeader, [ Get_Cmd_Field_DisplayCaption( fidTerminalOnly ) ] ) + strif( TerminalOnly, 'true', 'false' ) + LineEnding;

  CmdString := CmdString + format( UpWrapper, [ Get_Cmd_Field_DisplayCaption( fidNotes ) ] ) + LineEnding;
  CmdString := CmdString + Notes + LineEnding;
  CmdString := CmdString + format( DownWrapper, [ Get_Cmd_Field_DisplayCaption( fidNotes ) ] ) + LineEnding;

  CmdString := CmdString + LineEnding + format( UpWrapper, [ cNameItem_CommandLinePlur ] ) + LineEnding + LineEnding;

  for i := 0 to CmdLines.Count - 1 do
  begin

    aCmdLineObj := TCmdLineObj( CmdLines.Objects[ i ] );

    CmdString := CmdString + format( NormalLeader, [ Get_Cmd_Field_DisplayCaption( fidRowID ) ] ) + inttostr( aCmdLineObj.RowID ) + LineEnding;
    CmdString := CmdString + format( NormalLeader, [ Get_Cmd_Field_DisplayCaption( fidCmdFK ) ] ) + inttostr( aCmdLineObj.CmdFK ) + LineEnding;
    CmdString := CmdString + format( NormalLeader, [ Get_Cmd_Field_DisplayCaption( fidSO ) ] ) + inttostr( aCmdLineObj.SO ) + LineEnding;
    CmdString := CmdString + format( NormalLeader, [ Get_Cmd_Field_DisplayCaption( fidEntry ) ] ) + aCmdLineObj.Entry + LineEnding;

    CmdString := CmdString + format( NormalLeader, [ Get_Cmd_Field_DisplayCaption( fidFriendlyName ) ] ) + aCmdLineObj.FriendlyName + LineEnding;

    CmdString := CmdString + format( NormalLeader, [ Get_Cmd_Field_DisplayCaption( fidThreatLevel ) ] ) + fCmdObjHelper.GetThreatLevelText( aCmdLineObj.ThreatLevel ) + LineEnding;

    CmdString := CmdString + format( NormalLeader, [ Get_Cmd_Field_DisplayCaption( fidWantsInput ) ] ) + strif( aCmdLineObj.WantsInput, 'true', 'false' ) + LineEnding;

    CmdString := CmdString + format( NormalLeader, [ Get_Cmd_Field_DisplayCaption( fidTerminalOnly ) ] ) + strif( aCmdLineObj.TerminalOnly, 'true', 'false' ) + LineEnding;

    CmdString := CmdString + format( NormalLeader, [ Get_Cmd_Field_DisplayCaption( fidAlert ) ] ) + strif( aCmdLineObj.Alert, 'true', 'false' ) + LineEnding;

    CmdString := CmdString + format( NormalLeader, [ Get_Cmd_Field_DisplayCaption( fidUseShell ) ] ) + strif( aCmdLineObj.UseShell, 'true', 'false' ) + LineEnding;

    CmdString := CmdString + format( NormalLeader, [ Get_Cmd_Field_DisplayCaption( fidIsFavorite ) ] ) + strif( aCmdLineObj.IsFavorite, 'true', 'false' ) + LineEnding;

    CmdString := CmdString + format( NormalLeader, [ Get_Cmd_Field_DisplayCaption( fidDetachProcess ) ] ) + strif( aCmdLineObj.DetachProcess, 'true', 'false' ) + LineEnding;

    CmdString := CmdString + format( NormalLeader, [ Get_Cmd_Field_DisplayCaption( fidSuperUserMode ) ] ) + strif( aCmdLineObj.SuperUserMode, 'true', 'false' ) + LineEnding;


    CmdString := CmdString + format( UpWrapper, [ Get_Cmd_Field_DisplayCaption( fidNotes ) ] ) + LineEnding;
    CmdString := CmdString + aCmdLineObj.Notes + LineEnding;
    CmdString := CmdString + format( DownWrapper, [ Get_Cmd_Field_DisplayCaption( fidNotes ) ] ) + LineEnding;

    CmdString := CmdString + LineEnding + LineEnding;

  end;

  CmdString := CmdString + format( DownWrapper, [ cNameItem_CommandLinePlur ] ) + LineEnding;

  result := CmdString;

end;


function TCmdObj.AddCommandLine( const anEntry : string ) : integer;
var
  aCmdLine: TCmdLineObj;
begin

  Result := CmdLines.AddObject( anEntry, TCmdLineObj.Create( fDBServer, IniSection, CmdLines.Count ) );

  aCmdLine := TCmdLineObj(CmdLines.Objects[Result]);

  aCmdLine.Entry := anEntry;
  aCmdLIne.CmdFK := RowID;

  aCmdLine.IsNew := True;

  CmdLines[ Result ] := aCmdLine.GetDisplayString;

end;

function TCmdObj.ReorderCmdLine( const idx, aFactor : integer ) : integer;
var
  OldCmdLine, MovingCmdLine: TCmdLineObj;
begin

  Result := idx + aFactor;

  MovingCmdLine := TCmdLineObj( CmdLines.Objects[ idx ] );
  MovingCmdLine.SO := Result;
  OldCmdLine := TCmdLineObj( CmdLines.Objects[ idx + aFactor ] );
  OldCmdLine.SO := Idx;

  CmdLines.Objects[ Result ] := MovingCmdLIne;
  CmdLines[ Result ] := MovingCmdLIne.GetDisplayString;

  CmdLines.Objects[ idx ] := OldCmdLine;
  CmdLines[ idx ] := OldCmdLine.GetDisplayString;

end;

function TCmdObj.GetDisplayString: string;
begin
  Result := inherited GetDisplayString + CommandName;
end;

procedure TCmdObj.CmdLineDelete( const Idx : integer; const DeleteState : boolean );
var
  aCmdLineObj: TCmdLineObj;
begin
  aCmdLineObj := TCmdLineObj(CmdLines.Objects[Idx]);
  aCmdLineObj.DoDelete := DeleteState;
  CmdLines[Idx] := aCmdLineObj.GetDisplayString;
end;

procedure TCmdObj.UpdateCmdLinesDisplay(aCmdLineObj: TCmdLineObj);
var
  Idx: integer;
begin

  Idx := CmdLines.IndexOfObject(aCmdLineObj);
  if Idx > -1 then
    CmdLines[Idx] := aCmdLineObj.GetDisplayString;

end;

function TCmdObj.GetLoadedName: string;
begin
  if fLoadedCommandName <> '' then
    Result := fLoadedCommandName
  else
    Result := CommandName;
end;

function TCmdObj.IsSystemPath : boolean;
begin
  result := LocationPath = cCommandInPathStr;
end;

function TCmdObj.GetLiteralPath : string;
begin
  result := GetLiteralPath_Generic( LocationPath, CommandName );
end;

function TCmdObj.DoUpdate: boolean;
var
  i: integer;
begin
  Result := inherited DoUpdate;
  if not Result then
    for i := 0 to CmdLines.Count - 1 do
      if TCmdLineObj(CmdLines.Objects[i]).DoUpdate then
      begin
        Result := True;
        break;
      end;
end;

procedure TCmdObj.ResetUpdate;
var
  i : Integer;
begin
  inherited ResetUpdate;
  for i := 0 to CmdLines.Count - 1 do
    TCmdLineObj( CmdLines.Objects[ i ] ).ResetUpdate;
end;

procedure TCmdObj.ResetDBServer( DBS : TInfoServer; ForceSave : boolean = false );
var
  i : Integer;
begin
  if not assigned( DBS ) then
    raise EErrorDevelopment.create( 'TCmdObj.ResetDBServer: Infoserver is nil.' );
  DBServer := DBS;
  for i := 0 to CmdLines.Count - 1 do
    TCmdLineObj( CmdLines.Objects[ i ] ).DBServer := DBS;
  if ForceSave then
  begin
    IsNew := true;
    for i := 0 to CmdLines.Count - 1 do
      TCmdLineObj( CmdLines.Objects[ i ] ).IsNew := true;
  end;
end;

{ TBaseCmdObj }


procedure TBaseCmdObj.SetDoDelete(AValue: boolean);
begin
  if fDoDelete = AValue then
    Exit;
  fDoDelete := AValue;
end;

procedure TBaseCmdObj.SetRowID( AValue: integer );
begin
  if fFieldRowID.Value = AValue then
    Exit;
  fFieldRowID.Value := AValue;
end;

function TBaseCmdObj.GetRowID: integer;
begin
  Result := fFieldRowID.Value;
end;

procedure TBaseCmdObj.SetIsFavorite(AValue: boolean);
begin
  if fFieldIsFavorite.Value = AValue then
    Exit;
  fFieldIsFavorite.Value := AValue;
end;

procedure TBaseCmdObj.SetDBServer( AValue : TInfoServer );
begin
  if not assigned( AValue ) then
    raise EErrorDevelopment.create( 'TBaseCmdObj.SetDBServer: Infoserver is nil.' );
  if fDBServer = AValue then Exit;
  fDBServer := AValue;
end;

function TBaseCmdObj.GetIsFavorite: boolean;
begin
  Result := fFieldIsFavorite.Value;
end;

procedure TBaseCmdObj.SetThreatLevel(AValue: TThreatLevel);
var
  Orded : integer;
begin
  Orded := Ord( AValue );
  if fFieldThreatLevel.Value = Orded then
    exit;
  fFieldThreatLevel.Value := Orded;
end;

function TBaseCmdObj.GetThreatLevel: TThreatLevel;
begin
  Result := TThreatLevel( fFieldThreatLevel.Value );
end;


procedure TBaseCmdObj.SetSaveFieldNameSL( AValue : TStringList );
begin
  if fSaveFieldNameSL = AValue then Exit;
  fSaveFieldNameSL := AValue;
end;

procedure TBaseCmdObj.SetSaveFieldValueSL( AValue : TStringList );
begin
  if fSaveFieldValueSL = AValue then Exit;
  fSaveFieldValueSL := AValue;
end;


function TBaseCmdObj.CreateField( var Obj: TObject; const DBS : TDBCmdStructure ) : string;
begin
  result := '';
  case DBS.FieldType of
    //    ftUnk :
    ftString_Key:
      begin
        Obj := TCOFieldString_KeyField.Create( self, DBS.ColumnName );
        result := Get_IniFile_List_Section( fDataLocation );
        TCOFieldString_KeyField( Obj ).ControlSectionID := result;
//pointer only //registering the control field for Text DB's
        fTextDBKeyField := TCOFieldString_KeyField( Obj );//pointer only
      end;
    ftString, ftText: Obj := TCOFieldString.Create( self, DBS.ColumnName );
    ftBoolean: Obj := TCOFieldBoolean.Create( self, DBS.ColumnName );
    ftInteger: Obj := TCOFieldInteger.Create( self, DBS.ColumnName );
    ftList: Obj := TCOFieldDerivedList.Create( self, DBS.ColumnName );
    ftEnum: Obj := TCOFieldEnum.Create( self, DBS.ColumnName );
    ftDouble: Obj := TCOFieldDouble.Create( self, DBS.ColumnName );
  else
      InternalMessage('TBaseCmdObj.CreateField: Programmer!!! FieldType not in list');
  end;
  TCOField( Obj ).FieldType := DBS.FieldType;
  TCOField( Obj ).IsInternal := DBS.UniqueID = cCmdColINTERNALUniqueID;
  fFields.Add( Obj );
end;


procedure TBaseCmdObj.SetIsNew(AValue: boolean);
begin
  if fIsNew = AValue then
    Exit;
  fIsNew := AValue;
end;

procedure TBaseCmdObj.SetNotes(AValue: string);
begin
  if fFieldNotes.Value = AValue then
    Exit;
  fFieldNotes.Value := AValue;
end;


function TBaseCmdObj.GetNotes: string;
begin
  Result := fFieldNotes.Value;
end;

procedure TBaseCmdObj.SetTerminalOnly(AValue: boolean);
begin
  if fFieldTerminalOnly.Value = AValue then
    Exit;
  fFieldTerminalOnly.Value := AValue;
end;

function TBaseCmdObj.GetTerminalOnly: boolean;
begin
  Result := fFieldTerminalOnly.Value;
end;

procedure TBaseCmdObj.SetSuperUserMode(AValue: boolean);
begin
  if fFieldSuperUserMode.Value = AValue then
    Exit;
  fFieldSuperUserMode.Value := AValue;
end;

function TBaseCmdObj.GetSuperUserMode: boolean;
begin
  Result := fFieldSuperUserMode.Value;
end;


function TBaseCmdObj.GetDetachProcess: boolean;
begin
  Result := fFieldDetachProcess.Value;
end;


procedure TBaseCmdObj.SetDetachProcess(AValue: boolean);
begin
  if fFieldDetachProcess.Value = AValue then
    Exit;
  fFieldDetachProcess.Value := AValue;
end;

constructor TBaseCmdObj.Create( DBS : TInfoServer );
var
  i : Integer;
begin

  fSaveFieldNameSL := TStringList.Create;
  fSaveFieldValueSL := TStringList.Create;
  fFields := TList.Create;
  fTextDBKeyField := nil;//pointer only do not create
  fDBServer := DBS;
  fCmdObjHelper := nil;

  fDoDelete := False;
  fIsNew := False;

  for i := Low( CmdStruct ) to High( CmdStruct ) do
  begin
    if CmdStruct[ i ].CmdObjType = cotBase then
    begin
      case CmdStruct[ i ].FieldID of
        fidRowID :         CreateField( TObject( fFieldRowID ), CmdStruct[ i ] );
        fidSuperUserMode : CreateField( TObject( fFieldSuperUserMode ), CmdStruct[ i ] );
        fidTerminalOnly :  CreateField( TObject( fFieldTerminalOnly ), CmdStruct[ i ] );
        fidNotes :         CreateField( TObject( fFieldNotes ), CmdStruct[ i ] );
        fidIsFavorite :    CreateField( TObject( fFieldIsFavorite ), CmdStruct[ i ] );
        fidDetachProcess : CreateField( TObject( fFieldDetachProcess ), CmdStruct[ i ] );
        fidThreatLevel : begin
                           CreateField( TObject( fFieldThreatLevel ), CmdStruct[ i ] );
                           ThreatLevel := tlNotSpecified;//enums need specific initialization;
                         end;
      end;
    end;
  end;

  //fSuperUserMode := sumDefaultInvalid;// Set it to Invalid state so machinery can work
  //CreateField( TObject( fFieldSuperUserMode ), ftEnum, cCmdColSuperUserMode );
  //SetSuperUserMode( sumUnknown );

end;

destructor TBaseCmdObj.Destroy;
begin

  fTextDBKeyField := nil;//pointer only do not free
  fCmdObjHelper := nil;//pointer only do not free

  if assigned(fFieldRowID) then
    FreeAndNil(fFieldRowID);

  if assigned(fFieldTerminalOnly) then
    FreeAndNil(fFieldTerminalOnly);

  if assigned(fFieldSuperUserMode) then
    FreeAndNil(fFieldSuperUserMode);

  if assigned(fFieldDetachProcess) then
    FreeAndNil(fFieldDetachProcess);

  if assigned(fFieldNotes) then
    FreeAndNil(fFieldNotes);

  if assigned(fFieldIsFavorite) then
    FreeAndNil(fFieldIsFavorite);

  //ENUMS
  if assigned(fFieldThreatLevel) then
    FreeAndNil(fFieldThreatLevel);


  if assigned(fSaveFieldNameSL) then
    FreeAndNil(fSaveFieldNameSL);
  if assigned(fSaveFieldValueSL) then
    FreeAndNil(fSaveFieldValueSL);
  if assigned(fFields) then
    FreeAndNil(fFields);


  inherited Destroy;
end;

procedure TBaseCmdObj.SaveINI;
var
  i: integer;
begin
  for i := 0 to fFields.Count - 1 do
    TCOField(fFields[i]).SaveINIValue;
end;


function TBaseCmdObj.SaveDB : integer;
var
  i: integer;
  Str1 , Str2: String;
begin

//transaction managed from Tcmdobj.saveDB
  result := 0;

  for i := 0 to fFields.Count - 1 do
    TCOField(fFields[i]).GetSQLSaveFieldValuePair;

  Str1 := '';
  Str2 := '';

  If fisNew then
  begin
    BuildInsertStrings( Str1, Str2, fSaveFieldNameSL, fSaveFieldValueSL );
    result := fDBServer.InsertItem_ManTran_sql( Str1, Str2 );
  end
  else
  begin
    if fSaveFieldNameSL.Count > 0 then //If 0 then this obj has no updates, must be a sub-object where DoUpdae is true
      fDBServer.UpdateItems_ManTran_sql( BuildUpdateStrings( fSaveFieldNameSL, fSaveFieldValueSL ),
                                          'rowid=' + inttostr( RowID )
                                        );
  end;

end;


function TBaseCmdObj.Save : boolean;
var
  Idx: integer;
begin

  result := true;

  if fDBServer.UseDB then
  begin
    Idx := SaveDB;
    if fIsNew then
      RowID := Idx;
    fSaveFieldNameSL.Clear;
    fSaveFieldValueSL.Clear;
  end
  else SaveINI;

end;

function TBaseCmdObj.DoUpdate: boolean;
var
  i: integer;
begin

  Result := False;

  if IsNew or DoDelete then
  begin
    Result := True;
    exit;
  end;

  for i := 0 to fFields.Count - 1 do
  begin
    if TCOField(fFields[i]).DoUpdate then
    begin
      Result := True;
      break;
    end;
  end;

end;

procedure TBaseCmdObj.ResetUpdate;
var
  i : Integer;
begin
//in order to get all List updates in one transaction need to have this procedure because
//the showing command is changed internally and would flag falsely that the command needs to be updated
  for i := 0 to fFields.Count - 1 do
    TCOField( fFields[ i ] ).DoUpdate := false;
end;

function TBaseCmdObj.GetDisplayString: string;
begin
  if DoDelete then
    Result := cReservedDel
  else if IsNew then
    Result := cReservedAdd
  else if DoUpdate then
    Result := cReservedUpdate
  else
    Result := '';
end;

function TBaseCmdObj.BaseMerge( FromCmdObj : TBaseCmdObj; const MergeSource : string; var Info : string ) : boolean;
begin

  result := false;

  if IsFavorite <> FromCmdObj.IsFavorite then
    Info := Info + format( cucoMerge_Boolean, [ Get_Cmd_Field_DisplayCaption( fidIsFavorite ) ] );

  if DetachProcess <> FromCmdObj.DetachProcess then
    Info := Info + format( cucoMerge_Boolean, [ Get_Cmd_Field_DisplayCaption( fidDetachProcess ) ] );

  if SuperUserMode <> FromCmdObj.SuperUserMode then
    Info := Info + format( cucoMerge_Boolean, [ Get_Cmd_Field_DisplayCaption( fidSuperUserMode ) ] );

  if TerminalOnly <> FromCmdObj.TerminalOnly then
    Info := Info + format( cucoMerge_Boolean, [ Get_Cmd_Field_DisplayCaption( fidTerminalOnly ) ] );

  if ThreatLevel <> FromCmdObj.ThreatLevel then
    Info := Info + format( cucoMerge_String, [
                                  Get_Cmd_Field_DisplayCaption( fidThreatLevel ),
                                  fCmdObjHelper.GetThreatLevelText( FromCmdObj.ThreatLevel )
                                             ]
                         );

  if CompareText( Trim( FromCmdObj.Notes ), Trim( Notes ) ) <> 0 then
  begin
    Notes := format( cucoMerge_Text, [ DateToStr( now ), MergeSource ] )
                     + FromCmdObj.Notes
                     + cucoMerge_Text_Separator
                     + Notes;
    result := true;
  end;

  result := result or ( Info <> '' );

end;

{ TCmdRec }

procedure TCmdRec.SetCLIndex( AValue : integer );
begin
  if fCLIndex = AValue then Exit;
  fCLIndex := AValue;
end;

procedure TCmdRec.SetCmd( AValue : TCmdObj );
begin
  if fCmd = AValue then Exit;
  fCmd := AValue;
end;

constructor TCmdRec.Create;
begin
  inherited;
  fCmd := nil;
  fCLIndex := -1;
end;

destructor TCmdRec.Destroy;
begin
  //if assigned( fCmd ) then
  //  FreeAndNil( fCmd );
  fCmd := nil;
  inherited Destroy;
end;


initialization

  CmdObjHelper := TCmdObjHelper.Create;

finalization

  if assigned( CmdObjHelper ) then
    FreeAndNil( CmdObjHelper );

end.


