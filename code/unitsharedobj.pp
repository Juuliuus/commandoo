unit unitsharedobj;

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
  Classes, SysUtils
  , sqlite3conn, sqldb, DB, sqlite3dyn
  , JInifiles, forms
  , graphics
  , controls
  , unitDBConstants
  , unitDBStructure
  ;

type

  { TSharedObj }

  TSharedObj = class(TObject)
  private
    fIsInitialized : boolean;

    procedure SetIsInitialized(AValue : boolean);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property IsInitialized : boolean read FIsInitialized write SetIsInitialized;

  end;


  { T_IS_SearchControl }

  T_IS_SearchControl = class( TObject )
  private
    fID : TStringlist;
    fStrMain : TStringlist;
    fStrAlt : TStringlist;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Add( const ID, Main, Alt : string );
    function GetID( const Idx : integer ) : string;
    function GetMain( const Idx : integer ) : string;
    function GetAlt( const Idx : integer ) : string;
    function Count : integer;

    property ID : TStringlist read fID;
    property StrMain : TStringlist read fStrMain;
    property StrAlt : TStringlist read fStrAlt;

  end;


  { TInfoServer }

  TInfoServer = class(TSharedObj)
  private
    fCurrentControlSlotDL : TDataLocation;
    fCurrentControlSlotIdx : integer;

    fControlSlots : TStringlist;
    fNeedsCommit : boolean;
    fIniFile : TJIniFile;
    fSqlDB : TSQLite3Connection;
    fsqlQ : TSQLQuery;
    fSqlT : TSQLTransaction;
    f_IS_SC : T_IS_SearchControl;

    fUseDB : boolean;
    function CreateTables : boolean;
    procedure FetchSingleColumnList( const ColName : string; anSL : TStrings );
    function FetchSingleValue( const TableName, ColName, Key : string ) : Variant;
    function FreeControlSlots( DoSave : boolean = false ) : boolean;
    function IsValidDB( const FldName, TabName : string ) : boolean;
    procedure SetDefaultValues;
    procedure SetNeedsCommit( AValue : boolean );
    procedure Set_ControlStruct_Value_sql( const CF : TControlFieldID; const Value : string );
    procedure DoDirect_sql( aSqlList : TStrings = nil );

  public
    constructor Create; override;
    destructor Destroy; override;

    class function SqliteInstalled( sqlLib : string) : boolean;
    class function SqliteIsActive : boolean;
    class procedure Rechecking_SqliteIsActive;

    function Init( const FilePath, FileName : string; const DoUseDB : boolean ) : boolean;
    function UnInitialize( const DoSave : boolean ) : boolean;
    function SaveAll : boolean;

    procedure SetControlSlot( const DL : TDataLocation );
    procedure GetSearchList( const ColMain, ColAlt : string; const DL : TDataLocation );
    procedure OpenSearch;
    procedure CloseSearch;
    function SearchCount : integer;
    function GetSearchIdx( const Idx : integer ) : string;
    function GetSearchDisplayMain( const Idx : integer ) : string;
    function GetSearchDisplayAlt( const Idx : integer ) : string;

    procedure UpdateItems_ManTran_sql( const PairStr, Where : string );
    function InsertItem_ManTran_sql( const FldStr, ValStr : string ) : integer;
    procedure DeleteItems_ManTran_sql( const Where : string; IsWhere : boolean = true );
    procedure Initiate_Save;
    procedure FIX_MERGE;
    procedure Finalize_Save( aType : integer ); //-1 rollback 0 end 1 commit

//iteratering queries
    function OpenQuery( const DL : TDataLocation; const SelectPhrase, Where : string ) : string;
    function QueryIsOpen : boolean;
    procedure CloseQuery;
    function QueryVal( const ColName : string ) : variant;
    function QueryEof : boolean;
    //procedure QueryFirst;
    procedure QueryNext;

    function FetchFromForeignKey( const ColName, Key : string ) : string;
    function LoadList( const ColName : string; anSL : TStrings ) : boolean;
    function SaveList( const DirtyList : boolean; const ColName : string; anSL, sqlSL : TStrings ) : boolean;
    procedure EraseList( const SectTabName : string );

    procedure SaveIniFile;
    function SectionExists( const SectName : string ) : boolean;
    procedure WriteString( const SectName, KeyName, Value : string );
    function ReadString( const SectName, KeyName, DefaultVal : string ) : string;
    procedure WriteBool( const SectName, KeyName : string; const Value : boolean );
    function ReadBool( const SectName, KeyName : string; const DefaultVal : boolean ) : boolean;
    procedure WriteInteger( const SectName, KeyName : string; const Value : integer );
    procedure WriteDouble( const SectName, KeyName : string; const Value : Double );
    function ReadInteger( const SectName, KeyName : string; const DefaultVal : integer ) : integer;
    function ReadDouble( const SectName, KeyName : string; const DefaultVal : Double ) : Double;


    function SearchRead( const SIT : TSearchItemType; const Key, ColName : string; var IsValid : boolean ) : variant;

    procedure ReadSection( const SectName : string; anSL : TStrings );
    procedure EraseSection( const SectName : string );
    procedure RenameSection (const OldSection, NewSection : String );
    procedure DeleteKey( const Section, Ident: String );
    procedure GetSectionList( Strings : TStrings );


    function GetFieldValue( const ColName, Key : string ) : variant;
    function GetDBGUID : string;
    function UpdateGUID( const FName : string; const isSql : boolean ) : boolean;
    function GetDBVersionUpgradeCount : integer;
    procedure UpdateDBVersionCount( const UpgradedToVersion : integer );
    procedure RemoveField_File( const Key : string; DL : TDataLocation; IgnoreSection : string = '' );
    procedure DBUpgrade_Cleanup0002( const Key : string );
    procedure DBUpgrade_Cleanup0003( const DL : TDataLocation; const Key : string );

    property UseDB : boolean read FUseDB;
    property NeedsCommit : boolean read fNeedsCommit write SetNeedsCommit;
  end;


  { TFormSettings }

  TFormSettings = class(TObject)
  private

    fFormSettingsSL : TStringList;
    fIsInitialized : boolean;
    fSettingsList : TStringlist;
    fSettingsSectTab : string;
    fIsReading : boolean;
    fIniFile : TJiniFIle;

    procedure CheckIsReading;
    function GetListAsTextForSaving( Strings : TStrings) : string;
    procedure LoadFormSettings( const FormName : string );
    procedure SetIsInitialized( AValue : boolean );

  protected

  public
    constructor Create;
    destructor Destroy; override;
    class function GetDataSection : string;
    class procedure ClearAllFormSettings( IniFile : TJinifile );
    function Init( anIniFile : TJiniFIle) : boolean;

    procedure RemoveFormSetting( const FormName : string );
    procedure RemoveFormSettingFromDB( const FormName : string; DoUpdate : boolean = false);
    procedure SaveFormSettings( const FormName : string );
    function ReadFormSettings( const FormName : string; var Idx : integer; var Value : string ) : boolean;
    procedure AddSetting( const ASetting : string );
    property IsInitialized : boolean read fIsInitialized write SetIsInitialized;

  end;


var

  InfoServer : TInfoServer;
  FormSettings : TFormSettings;

const
  //cfcTabSectColInvalid = 'Hey Programmer! TFormColor.Init needs valid Section/Table & Column Names to save and read SystemColors preference';
//were tags that were used in old system color management which turned out to be a bad idea
  //csoIgnoreColorChange = 123321;
  //csoIgnoreColorChangeChildren = 223322;
  csoCallOnce = 'Hey Programmer! %s.Init should only be called once and, generally, only from Main Form.';
  csoObjectNotInitialized = 'Hey Programmer! %s is not initialized, %s halted';
  csoGenError = '%s: Error %s';
  csoSqlite_rowid = 'rowid';

  //crefOldSystemColorKey = 'UseSystemColors';

  cSqliteDefaultLibraryLocation64 = '/usr/lib/x86_64-linux-gnu/libsqlite3.so.0';
  cSqliteDefaultLibraryLocation32 = '/usr/lib/i386-linux-gnu/libsqlite3.so.0';
  csocapSqlNoGuidValue = '{unkGUID}';

resourcestring
  csocapSqlNoGuid = '%s: For some reason sql db has no GUID, tell the developer';



implementation

uses juusgen
     , unitDBUtils
     , linuxtrix
     , stdCtrls
     , unitGlob
     ;

var
  SqliteAllowed : boolean = false;

const

  //csoDBFileNamesEmpty = 'Hey Programmer! SavingServer.Init needs at least one filename, the first is the main file, others will be DB/etc ini files.';
  csoUseAddSetting = 'Hey Programmer! You didnt call AddSetting before calling SaveFormSettings';
  csoFontKey = 'font';
  cso_SavingServerFormSettings = 'SavingServerFormSettings';

{ T_IS_SearchControl }

constructor T_IS_SearchControl.Create;
begin
  fID := TStringlist.Create;
  fStrMain := TStringlist.Create;
  fStrAlt := TStringlist.Create;
end;

destructor T_IS_SearchControl.Destroy;
begin

  if assigned( fID ) then
    FreeAndNil( fId );
  if assigned( fStrMain ) then
    FreeAndNil( fStrMain );
  if assigned( fStrAlt ) then
    FreeAndNil( fStrAlt );
  inherited Destroy;
end;

procedure T_IS_SearchControl.Clear;
begin
  fID.Clear;
  fStrMain.Clear;
  fStrAlt.Clear;
end;

procedure T_IS_SearchControl.Add( const ID, Main, Alt : string );
begin
  if ID <> '' then
    fID.Add( ID );
  fStrMain.Add( Main );
  fStrAlt.Add( Alt );
end;

function T_IS_SearchControl.GetID( const Idx : integer ) : string;
begin
  result := fID[ Idx ];
end;

function T_IS_SearchControl.GetMain( const Idx : integer ) : string;
begin
  result := fStrMain[ Idx ];
end;

function T_IS_SearchControl.GetAlt( const Idx : integer ) : string;
begin
  result := fStrAlt[ Idx ];
end;

function T_IS_SearchControl.Count : integer;
begin
  result := fStrMain.Count;
end;


{ TInfoServer }

procedure TInfoServer.SetControlSlot( const DL : TDataLocation );
begin

  if ( DL = fCurrentControlSlotDL ) then
    exit;

  fCurrentControlSlotDL := DL;
  fCurrentControlSlotIdx := Ord( DL );

  if not fUseDB then
    FIniFile := TJIniFile( fControlSlots.Objects[ fCurrentControlSlotIdx ] );

end;

procedure TInfoServer.GetSectionList( Strings : TStrings );
begin
  fIniFIle.GetSectionList( Strings );
end;

function TInfoServer.GetFieldValue( const ColName, Key : string ) : variant;
begin
//assumes setcontrolslot was set, currently only fields call this so yes.
  result := FetchSingleValue( fControlSlots[ fCurrentControlSlotIdx ], ColName, Key );
end;

procedure TInfoServer.GetSearchList( const ColMain, ColAlt : string; const DL : TDataLocation );
var
  i : Integer;
begin

  SetControlSlot( DL );

  if UseDB then
  begin

    fsqlQ.SQL.Text := GetSelect_Sql( ColMain + strif( ColAlt <> '', ', ' + ColAlt ),
                                     fControlSlots[ fCurrentControlSlotIdx ]
                                   );
    try
      fsqlQ.Open;

      fsqlQ.First;
      while not fsqlQ.EOF do
      begin
        case fsqlQ.FieldCount of
          2 :
            f_IS_SC.Add( fsqlQ.FieldValues[ csoSqlite_rowid ],
                         fsqlQ.FieldValues[ ColMain ],
                         ''
                        );
          3 :
            f_IS_SC.Add( fsqlQ.FieldValues[ csoSqlite_rowid ],
                         fsqlQ.FieldValues[ ColMain ],
                         fsqlQ.FieldValues[ ColAlt ]
                        );
        end;

        fsqlQ.Next;
      end;

    finally
      fsqlQ.Close;
//From what I could tell, sqlQ should not be creating a tran for me, but, apparently, it is, even from a simple
//select statement. goddamit. This should ends this. Appears to happen only when open method is called on a sqlquery obj.
//so VERY important to do this otherwise...exceptions
      if fsqlT.Active then
        //fsqlT.Rollback;
        fsqlT.EndTransaction;
    end;

  end
  else
  begin
    fIniFile.GetSectionList( f_IS_SC.ID );
    for i := 0 to f_IS_SC.fID.Count - 1 do
    begin
      f_IS_SC.Add( '',
                   fIniFile.ReadString( f_IS_SC.fID[ i ], ColMain, '<error>' ),
                   fIniFile.ReadString( f_IS_SC.fID[ i ], ColAlt, '' )
                  );
    end;
  end;
end;

function TInfoServer.UpdateGUID( const FName : string; const isSql : boolean ) : boolean;
var
  IFile : TJiniFile;
begin
  result := false;
  IFile := nil;
  try
    try

      if IsSql then
        Set_ControlStruct_Value_sql( cofidProfileGUID, DoubleQuotedString( GetRawGuidString ) )
      else
      begin
        IFile := TJiniFile.Create( FName );
        IFile.CacheUpdates := true;
        SetProfileGUID( IFile,
                        cSectTab_DB_ProfileGUID,
                        cKey_ProfileGuid,
                        GetRawGuidString );
        IFile.UpdateFile;
      end;

    finally
      If assigned( IFile ) then
        FreeAndNil( IFile );
    end;

    result := true;

  except
    on e:exception do
      EErrorRaise( e, cEErrorDatabaseProblem, 'TInfoServer.UpdateGUID: ' );
  end;
end;

function TInfoServer.GetDBGUID : string;
var
  i : Integer;
begin

  if fUseDB then
    result := FetchSingleValue( ControlStruct[ Ord( cofidProfileGUID ) ].TableName,
                                ControlStruct[ Ord( cofidProfileGUID ) ].ColumnName,
                                '1' )
  else result := GetProfileGUID( TJIniFile( fControlSlots.Objects[ Ord( dlCmd ) ] ), cSectTab_DB_ProfileGUID );

  if result = '' then
  begin
    result := GetRawGuidString;
    if fUseDB then
    begin
    //sql db should never be updated, it is made at creation and stays with it. So this section "should" never be hit
    // COPY db is an exception but see UpdateGUID
      internalMessage( format( csocapSqlNoGuid, [ 'TInfoServer.GetDBGUID' ] ) );
      result := csocapSqlNoGuidValue;
    end
    else
    begin
      //this one is simply adding it to legacy inifile DB's (maybe new inifiles too, but not a problem).
      for i := 0 to High( aryTextData ) do
        SetProfileGUID( TJIniFile( fControlSlots.Objects[ Ord( aryTextData[ i ] ) ] ),
                        cSectTab_DB_ProfileGUID,
                        cKey_ProfileGuid,
                        result );
    end;
  end;

end;

function TInfoServer.GetDBVersionUpgradeCount : integer;
begin
  result := -1;
  if fUseDB then
    result := FetchSingleValue( ControlStruct[ Ord( cofidVersionCount ) ].TableName,
                                ControlStruct[ Ord( cofidVersionCount ) ].ColumnName,
                                '1' )
  else result := GetUpgradeLevel( TJIniFile( fControlSlots.Objects[ Ord( dlCmd ) ] ), cSectTab_DB_VersionCount );

end;

procedure TInfoServer.Set_ControlStruct_Value_sql( const CF : TControlFieldID; const Value : string );
begin
  fsqlT.StartTransaction;
  try
    fSqlDB.ExecuteDirect(
                GetUpdate_Sql(
                               ControlStruct[ Ord( CF ) ].TableName,
                               format( '%s = %s', [ ControlStruct[ Ord( CF ) ].ColumnName, Value ] ),
                                       csoSqlite_rowid + '=1'
                                         )
                       );
    fsqlT.Commit;
  except
    fsqlT.Rollback;
    raise;
  end;
end;

procedure TInfoServer.UpdateDBVersionCount( const UpgradedToVersion : integer );
var
  i : Integer;
begin

  if UseDB then
    Set_ControlStruct_Value_sql( cofidVersionCount, inttostr( UpgradedToVersion ) )
  else
    for i := 0 to High( aryTextData ) do
      UpdateUpgradeLevel( TJIniFile( fControlSlots.Objects[ Ord( aryTextData[ i ] ) ] ),
                          cSectTab_DB_VersionCount,
                          cKey_VersionCount,
                          UpgradedToVersion );

end;

procedure TInfoServer.RemoveField_File( const Key : string; DL : TDataLocation; IgnoreSection : string );
var
  SL : TStringList;
  i : Integer;
begin
  SL := TStringList.Create;
  try
    SetControlSlot( DL );
    FInifile.GetSectionList( SL );
    for i := 0 to SL.Count - 1 do
    begin
      if SL[ i ] = IgnoreSection then
        continue;
      FIniFile.DeleteKey( SL[ i ], Key );
    end;
    SaveIniFile;
  finally
    SL.free;
  end;
end;

procedure TInfoServer.DBUpgrade_Cleanup0002( const Key : string );
var
  SL : TStringList;
  i : Integer;
  TempStr : String;
begin
//special for oldDB updates, literally set to dlCmdLine
  SL := TStringList.Create;
  try
    SetControlSlot( dlCmdLine );
    FInifile.GetSectionList( SL );
    for i := 0 to SL.Count - 1 do
    begin
      TempStr := FIniFile.ReadString( SL[ i ], Key, '' );
      if TempStr = '' then
        continue;
      if pos( '''<<', TempStr ) > 0 then
      begin
        TempStr := stringreplace( TempStr, '''<<F>>''', '<<F>>', [ rfreplaceall, rfignorecase ] );
        FIniFile.WriteString( SL[ i ], Key, TempStr );
      end;
    end;
    SaveIniFile;
  finally
    SL.free;
  end;
end;

procedure TInfoServer.DBUpgrade_Cleanup0003( const DL : TDataLocation; const Key : string );
var
  SL : TStringList;
  i : Integer;
  TempInt: integer;
begin

  SL := TStringList.Create;
  try
    SetControlSlot( DL );
    FInifile.GetSectionList( SL );
    for i := 0 to SL.Count - 1 do
    begin
      TempInt := FIniFile.ReadInteger( SL[ i ], Key, -9999 );
      if TempInt = -9999 then
        continue;

      dec( TempInt );
      if TempInt < 0 then
        TempInt := 0;
      FIniFile.WriteInteger( SL[ i ], Key, TempInt );
    end;
    SaveIniFile;
  finally
    SL.free;
  end;
end;

function TInfoServer.IsValidDB( const FldName, TabName : string ) : boolean;
begin
  result := false;
  try
    fSqlDB.ExecuteDirect( GetSelect_Sql( FldName, TabName ) );
    fsqlT.EndTransaction;
    result := true;
  except
    fSqlDB.Close; //not one of OUR sqlite db's
  end;
end;


function TInfoServer.CreateTables : boolean;
var
  i , j: Integer;
  SqlCmd, FldStr , ValStr: String;
  SL : TStringList;
begin

  result := true;
  fsqlT.StartTransaction;
  SqlCmd := '';
  try
    SL := TStringlist.create;
    try
      for i := 0 to fControlSlots.Count - 1 do
      begin

        case TDataLocation( i ) of
          dlCmd     : SqlCmd := GetCmd_CmdLineTableSql( fControlSlots[ i ], SL, cotCmdLine );
          dlCmdLine : SqlCmd := GetCmd_CmdLineTableSql( fControlSlots[ i ], SL, cotCmd );
          dlKeyWord : SqlCmd := GetKWordTableSql( fControlSlots[ i ], SL );
          dlControl :
            begin
              SqlCmd := GetControlTableSql( fControlSlots[ i ], SL );

   //Control table needs to be initialized on create
              FldStr := ControlStruct[ Ord( cofidVersionCount ) ].ColumnName
                        + ', '
                        + ControlStruct[ Ord( cofidProfileGUID ) ].ColumnName;

              ValStr := inttostr( c_DB_VersionUpgradeCount )
                        + ', '
                        + QuotedStr( GetRawGuidString );

              SL.Add( GetInsert_Sql( fControlSlots[ i ], FldStr, ValStr ) );

            end
          else SqlCmd := 'xxInvalidxx';
        end;

        fSqlDB.ExecuteDirect( SqlCmd );

        for j := 0 to SL.Count - 1 do //indexes and anything else
          fSqlDB.ExecuteDirect( SL[ j ] );

      end;//for i

      fSqlT.Commit;

    finally
      SL.free;
    end;

  except

    on e:exception do
    begin
      fSqlT.Rollback;
      result := false;
  //no exception, system recovers on its own...   EErrorRaise( e, cEErrorDatabaseProblem, 'TInfoServer.CreateTables: ' );
      InternalMessage( e.message );
    end;

  end;

end;


function TInfoServer.Init( const FilePath, FileName : string; const DoUseDB : boolean ) : boolean;
var
  DBName, TName : string;
  IsNew : boolean;
  i, Idx : Integer;
  DL : TDataLocation;
const
  Errmsg = 'TInfoServer.Init: %s structure not defined properly';
begin
  try

    result := false;

    if DontRun( IsInitialized, format( csoCallOnce, [ 'InfoServer' ] ) ) then
      exit;


    fUseDB := DoUseDB;

//slots are made for all possiblities, then indexed by sql/text based on TDataLocation
    for i := 0 to Ord( dlForCountingDNU ) - 1 do
      fControlSlots.Add( '' );

    if fUseDB then
    begin

      fSqlDB.DatabaseName := GenerateDBFilePath( FilePath, FileName, cSqlDBExtension );

      isNew := not FileExists( fSqlDB.DatabaseName );

      try
        fSqlDB.Open;
      except
         on e: exception do
         begin
           TInfoServer.Rechecking_SqliteIsActive;
           //This exception is caught below
           raise exception.create( 'SQL DB Open: ' + e.message );
         end;
      end;

      if not fSqlDB.Connected then
      begin
//no exceptions here, need results back in main form      raise exception.create( 'Boohoo, connection didnt work' );
//this should literally never happen (unless the hard drive is full, permissions or the path is bad)
        InternalMessage( 'sqlite connection failed' );
        exit;
      end;

      for i := 0 to High( arySqlData ) do
      begin
        DL := arySqlData[ i ];
        TName := GetTableNames( DL, fUseDB );
        if TName = '' then
        begin
//No exceptions here, need result back in main form.
          InternalMessage( format( errmsg, [ 'DB arySqlData' ] ) );
          exit;
        end;
        fControlSlots[ Ord( DL ) ] := TName;
      end;

      case IsNew of
        true :
          if not CreateTables then
          begin
//file's auto created by sqlite but no good and next time tries to validate it.
            Deletefile( fSqlDB.DatabaseName );
            exit;
          end;
        false :
          if not IsValidDB( Get_Cmd_Field_ColumnName( csqlExistenceCheckerFieldName ), csqlExistenceCheckerTableName ) then
             exit;
      end;

//certainly this turned out to be necessary like CloseQuery is
//some fsqlQ can turn this on and it should be off after all creation and/or validity checking.
      if fsqlT.Active then
        fsqlT.EndTransaction;//best choice??
        //fsqlT.Active := false;
    end
    else //Not fUseDB
    begin

      FIniFile := nil;

      for i := 0 to High( aryTextData ) do
      begin
        DL := aryTextData[ i ];
        DBName := GetTableNames( DL, fUseDB );
        if DBName = '' then
        begin
//No exceptions here, need result back in main form.
          InternalMessage( format( errmsg, [ 'IniFiles aryTextData' ] ) );
          exit;
        end;

        DBName := GenerateDBFilePath(
                     FilePath,
                     format( '%s' + DBName, [ FileName ] ),
                     cIniDBExtension );

        isNew := not fileexists( DBName );

        Idx := Ord( DL );
        fControlSlots[ Idx ] := DBName;
        fControlSlots.Objects[ Idx ] := TJIniFile.Create( DBName );

        with TJIniFile( fControlSlots.Objects[ Idx ] ) do
        begin
          CacheUpdates := true;
          if isNew then
          begin
            WriteSetIniFileWarning( false );
            UpdateUpgradeLevel( TJIniFile( fControlSlots.Objects[ Idx ] ),
                              cSectTab_DB_VersionCount,
                              cKey_VersionCount,
                              c_DB_VersionUpgradeCount
                               );
          end;
        end;

      end; //for i

    end; //not fUseDB

    IsInitialized := true;
    result := true;
  except
    on e:exception do
    begin
//No exceptions here, need result back in main form.
      result := false;
      InternalMessage( format( errmsg, [ 'Error: ' ] ) + e.message );
    end;
  end;

end;


function TInfoServer.UnInitialize( const DoSave : boolean ) : boolean;
begin

  result := false;

  if not FreeControlSlots( DoSave ) then
    fControlSlots := TStringList.Create;

  fsqlQ.Close;
  if fSqlT.Active then
//??? Not sure what is best and/or proper here. Certainly it SHOULDN'T be active.
//  fSqlT.Active := false;
    fSqlT.EndTransaction;

  fSqlDB.Close( true );

  SetDefaultValues;

  result := true;

end;


procedure TInfoServer.SetDefaultValues;
begin
  fCurrentControlSlotDL := dlForCountingDNU;//needs to be "invalid" DL so it is always run through setcontrolslot
  fCurrentControlSlotIdx := -1;
  fNeedsCommit := false;
//must use private fIsInitialized. The SETTER, as a safety measure, won't let it be uninitialized
  fIsInitialized := false;
end;

procedure TInfoServer.SetNeedsCommit( AValue : boolean );
begin
  if fNeedsCommit = AValue then Exit;
  fNeedsCommit := AValue;
end;

constructor TInfoServer.Create;
begin
  inherited;

  SetDefaultValues;

  fControlSlots := TStringList.Create;

  f_IS_SC := T_IS_SearchControl.Create;

  fSqlT := TSQLTransaction.Create( nil );
  fSqlT.Options := [];
  fSqlT.Name := 'SqlT';

  fSqlDB := TSQLite3Connection.Create( nil );
  fSqlDB.LogEvents := [];
  fSqlDB.Name := 'SqlDB';
  fSqlDB.Transaction := fSqlT;

  fSqlQ := TSQLQuery.Create( nil );
  fSqlQ.Options := [];
  fSqlQ.Name := 'SqlQ';
  fSqlQ.DataBase := fSqlDB;
  fSqlQ.Transaction := fSqlT;

end;

destructor TInfoServer.Destroy;
begin

  if FreeControlSlots then
    FreeAndNil( fControlSlots );

  if assigned( f_IS_SC ) then
    FreeAndNil( f_IS_SC );

  if assigned( fSqlT ) then
  begin
    fSqlT.Active := false;
    FreeAndNil( fSqlT );
  end;

  if assigned( fSqlQ ) then
  begin
    fSqlQ.Close;
    FreeAndNil( fSqlQ );
  end;

  if assigned( fSqlDB ) then
  begin
    fSqlDB.Close;
    FreeAndNil( fSqlDB );
  end;

  inherited Destroy;

end;

class function TInfoServer.SqliteIsActive : boolean;
begin
  result := SqliteAllowed;
end;

class procedure TInfoServer.Rechecking_SqliteIsActive;
begin
  SqliteAllowed := false;
end;

class function TInfoServer.SqliteInstalled( sqlLib : string ) : boolean;
begin
  //cSqliteDefaultLibraryLocation64 = '/usr/lib/x86_64-linux-gnu/libsqlite3.so.0';
  //cSqliteDefaultLibraryLocation32 = '/usr/lib/i386-linux-gnu/libsqlite3.so.0';
  result := false;

  if SqliteAllowed then
    exit; //have already found it this session.
  //needs this kind of protection because what if sqlite is working fine?!
  //then it is NOT allowed to be changed and information should not be updated

  if sqlLib = '' then
{$IFDEF Bit32}
    sqlLib := cSqliteDefaultLibraryLocation32;
{$ELSE}
    sqlLib := cSqliteDefaultLibraryLocation64;
{$ENDIF}

  if not FileExists( sqlLib ) then //+ 'xxx' ) then testing
    exit;

  sqlite3dyn.SQLiteDefaultLibrary := sqlLib;

  result := true;
  SqliteAllowed := true;

end;

function TInfoServer.FreeControlSlots( DoSave : boolean = false ) : boolean;
var
  i : Integer;
begin
  FIniFile := nil;
  result := false;
  if not assigned( fControlSlots ) then
    exit;
  result := true;

  if fUseDB then
  begin
    if Dosave and fsqlT.Active then
      fsqlT.Commit;
      //fsqlT.EndTransaction;//???
  end
  else
    for i := 0 to fControlSlots.Count - 1 do
    begin
      FIniFile := TJInifile( fControlSlots.Objects[ i ] );
      if not assigned( FIniFile ) then
        continue;
      if DoSave then
        FIniFIle.Updatefile;
//==> causes exceptions on bad files, I want to control updates, changed in jinifile.pp
//was automatically doing updates. Set it to false to prevent the problem.
      FIniFile.CacheUpdates := false;
      FIniFile := nil;
      fControlSlots.Objects[ i ].free;
      fControlSlots.Objects[ i ] := nil;
    end;

  fControlSlots.Clear;

end;

procedure TInfoServer.FetchSingleColumnList( const ColName : string; anSL : TStrings );
begin

  fsqlQ.Close;
  fsqlQ.SQL.Text := GetSelect_Sql( ColName, fControlSlots[ fCurrentControlSlotIdx ] );
  try
    fsqlQ.Open;
    while not fsqlQ.Eof do
    begin
      anSL.Add( fsqlQ.FieldValues[ ColName ] );
      fsqlQ.Next;
    end;

  finally
    fsqlQ.Close;
//even a simple select statement makes fsqlQ start a "phantom" transaction, it, uhhh, gets in the way
    fsqlT.EndTransaction;
  end;

end;

function TInfoServer.FetchSingleValue( const TableName, ColName, Key : string ) : Variant;
begin

  result := '';
  fsqlQ.Close;
  fsqlQ.SQL.Text := GetSelect_Sql( ColName, TableName, csoSqlite_rowid + '=' + Key );
  try
    fsqlQ.Open;
    if not fsqlQ.EOF then
     result := fsqlQ.FieldValues[ ColName ];
  finally
    fsqlQ.Close;
//even a simple select statement makes fsqlQ start a "phantom" transaction, it, uhhh, gets in the way
    fsqlT.EndTransaction;
  end;

end;

function TInfoServer.LoadList( const ColName : string; anSL : TStrings ) : boolean;
begin

  result := false;

  if DontRun( not IsInitialized, format( csoObjectNotInitialized, [ 'InfoServer','LoadList' ] ) ) then
    exit;

  anSL.Clear;

  if fUseDB then
    FetchSingleColumnList( ColName, anSL )
  else FIniFile.LoadList( ColName, anSL );

  result := true;

end;

procedure TInfoServer.DoDirect_sql( aSqlList : TStrings = nil );
var
  i : Integer;
begin

//proc ===> NOT <==== for insert statements

  if not assigned( aSqlList ) then
    raise EErrorDevelopment.Create( 'TInfoServer.DoDirect_sql: unassgined aSqlList.' );

  if asqlList.Count = 0 then
    exit;

  try

    fsqlT.StartTransaction;
    try

      for i := 0 to asqlList.Count - 1 do
        fsqlDB.ExecuteDirect( asqlList[ i ] );

      fsqlT.Commit;

    except
      fsqlT.Rollback;
    end;

  finally
    asqlList.Clear;
  end;

end;

procedure TInfoServer.Initiate_Save;
begin
//fix for vast slowdown of saving in release vs. IDE, appears to have been too many individual transactions
//still wondering why the slowdown, however was NOT apparent in IDE...???!!
//So here starts a manually managed transaction, so this proc needs to be balanced properly, find in code for examples.
  if not fUseDB then
    exit;
  fsqlT.StartTransaction;
  NeedsCommit := false;//commit  rollback or simply endtrans
end;

procedure TInfoServer.FIX_MERGE;
begin
//total hack to fix issue in merging TO sql DB's
  if not fUseDB then
    exit;
  fsqlT.Commit;
  fsqlT.StartTransaction;
end;

procedure TInfoServer.Finalize_Save( aType : integer );
begin
  if not fUseDB then
    exit;
  if not NeedsCommit or not fsqlT.Active then
    exit;
  case aType of
    -1 : fsqlT.Rollback;
     1 : fsqlT.Commit;
     else fsqlT.EndTransaction;
  end;
  NeedsCommit := false;
end;

function TInfoServer.SaveList( const DirtyList : boolean; const ColName : string; anSL, sqlSL : TStrings ) : boolean;
var
  TName : String;
  i : Integer;
begin

  result := false;

  if DontRun( not IsInitialized, format( csoObjectNotInitialized, [ 'InfoServer','SaveList' ] ) )
     or DontRun( not assigned( anSL ), format( csoGenError, [ self.ClassName + '.SaveList', 'anSL is nil' ] ) )
  then
    exit;

  if UseDB then
  begin

    if DirtyList then
    begin
      TName := fControlSlots[ fCurrentControlSlotIdx ];

      sqlSL.Add( GetDelete_Sql( TName, csoSqlite_rowid + '>0' ) );

      for i := 0 to anSL.Count - 1 do
        sqlSL.Add( GetInsert_Sql( TName, ColName, QuotedStr( anSL[ i ] ) ) );
    end;

    DoDirect_sql( SqlSL );

  end
  else if DirtyList then
         FIniFile.SaveList( ColName, anSL );

  result := true;

end;

procedure TInfoServer.EraseList( const SectTabName : string );
begin
  if assigned( fIniFile ) then
    fIniFile.EraseSection( SectTabName );
end;


procedure TInfoServer.SaveIniFile;
begin
  if fUseDB then
    exit;
  if assigned( fIniFile ) then
    FIniFile.UpdateFile;
end;

function TInfoServer.SectionExists( const SectName : string ) : boolean;
begin
  result := FIniFile.SectionExists( SectName );
end;

procedure TInfoServer.WriteBool( const SectName, KeyName : string; const Value : boolean );
begin
  fIniFIle.WriteBool( SectName, KeyName, Value );
end;

function TInfoServer.ReadBool( const SectName, KeyName : string; const DefaultVal : boolean ) : boolean;
begin
  result := fIniFIle.Readbool( SectName, KeyName, DefaultVal );
end;

procedure TInfoServer.WriteInteger( const SectName, KeyName : string; const Value : integer );
begin
  fIniFile.WriteInteger( SectName, KeyName, Value );
end;

procedure TInfoServer.WriteDouble( const SectName, KeyName : string; const Value : Double );
begin
  fIniFile.WriteFloat( SectName, KeyName, Value );
end;

function TInfoServer.ReadInteger( const SectName, KeyName : string; const DefaultVal : integer ) : integer;
begin
  result := fIniFIle.ReadInteger( SectName, KeyName, DefaultVal );
end;

function TInfoServer.ReadDouble( const SectName, KeyName : string; const DefaultVal : Double ) : Double;
begin
  result := fIniFIle.ReadFloat( SectName, KeyName, DefaultVal );
end;

function TInfoServer.FetchFromForeignKey( const ColName, Key : string ) : string;
begin
  result := '';
  if not fUseDB then
    exit;
  result := FetchSingleValue( fControlSlots[ fCurrentControlSlotIdx ], ColName, Key );
end;

function TInfoServer.SearchRead( const SIT : TSearchItemType; const Key, ColName : string; var IsValid : boolean ) : variant;
const
  ThisFuncstr = 'TInfoServer.SearchRead: ';
begin

  if fUseDB then
  begin
    try
      IsValid := true;
      result := FetchSingleValue( fControlSlots[ fCurrentControlSlotIdx ], ColName, Key );
    except
      on e:exception do
      begin
        IsValid := false;
//from db unit   ESQLDatabaseError = class(EDatabaseError)
        if ( e is ESQLDatabaseError ) then
          EErrorRaise( e, cEErrorDO_NOT_RAISE, ThisFuncstr )
        else EErrorRaise( e, cEErrorDatabaseProblem, ThisFuncstr );
      end;
    end;
  end
  else
  begin
    IsValid := FIniFile.ValueExists( Key, ColName );
    case SIT of
      sitString : result := FIniFIle.ReadString( Key, ColName, '' );
      sitBoolean : result := FIniFIle.ReadBool( Key, ColName, false );
      sitInteger : result := FIniFIle.ReadInteger( Key, ColName, 0 );
      sitDouble : result := FIniFIle.ReadFloat( Key, ColName, 0.0 );
    end;
  end;

end;

procedure TInfoServer.WriteString( const SectName, KeyName, Value : string );
begin
  fIniFIle.WriteString( SectName, KeyName, Value );
end;

function TInfoServer.ReadString( const SectName, KeyName, DefaultVal : string ) : string;
begin
  result := fIniFIle.ReadString( SectName, KeyName, DefaultVal );
end;

procedure TInfoServer.ReadSection( const SectName : string; anSL : TStrings );
begin
  fIniFile.readsection( SectName, anSL );
end;

procedure TInfoServer.EraseSection( const SectName : string );
begin
  FIniFile.EraseSection( SectName );
end;

procedure TInfoServer.RenameSection(const OldSection, NewSection : String);
begin
  fIniFile.RenameSection( OldSection, NewSection );
end;

procedure TInfoServer.DeleteKey( const Section, Ident : String );
begin
  fIniFile.DeleteKey( Section, Ident );
end;

function TInfoServer.SaveAll : boolean;
var
  i : Integer;
begin

  result := false;

  try

    if fUseDB then
    begin
      if fsqlT.Active then
        fsqlT.EndTransaction;
    end
    else
    begin
      for i := 0 to fControlSlots.Count - 1 do
      begin
        if assigned( fControlSlots.Objects[ i ] ) then
          TJIniFile( fControlSlots.Objects[ i ] ).UpdateFile;
      end;
    end;
    result := true;

  except
    on e:exception do
      EErrorRaise( e, cEErrorDatabaseProblem, 'TInfoServer.SaveAll: ' );
  end;

end;

procedure TInfoServer.OpenSearch;
begin
  f_IS_SC.Clear;
end;

procedure TInfoServer.CloseSearch;
begin
  f_IS_SC.Clear;
end;

function TInfoServer.SearchCount : integer;
begin
  result := f_IS_SC.Count;
end;

function TInfoServer.GetSearchIdx( const Idx : integer ) : string;
begin
  result := f_IS_SC.GetID( Idx );
end;

function TInfoServer.GetSearchDisplayMain( const Idx : integer ) : string;
begin
  result := f_IS_SC.GetMain( Idx );
end;

function TInfoServer.GetSearchDisplayAlt( const Idx : integer ) : string;
begin
  result := f_IS_SC.GetAlt( Idx );
end;

procedure TInfoServer.UpdateItems_ManTran_sql( const PairStr, Where : string );
begin
//===================
//  manually managed transactions....careful!
//===================
  fsqlDB.ExecuteDirect( GetUpdate_Sql( fControlSlots[ fCurrentControlSlotIdx ], PairStr, Where ) );
end;

function TInfoServer.InsertItem_ManTran_sql( const FldStr, ValStr : string ) : integer;
begin
  result := 0;
//=============================
//  manually managed transactions....careful!
//=============================

  fsqlDB.ExecuteDirect( GetInsert_Sql( fControlSlots[ fCurrentControlSlotIdx ], FldStr, ValStr ) );
  result := fsqlDB.GetInsertID;

end;

procedure TInfoServer.DeleteItems_ManTran_sql( const Where : string; IsWhere : boolean = true );
begin
  if not IsWhere then
     fsqlDB.ExecuteDirect( Where )
  else fsqlDB.ExecuteDirect( GetDelete_Sql( fControlSlots[ fCurrentControlSlotIdx ], Where ) );
end;

function TInfoServer.OpenQuery( const DL : TDataLocation; const SelectPhrase, Where : string ) : string;
begin
  //returns tablename
  SetControlSlot( DL );
  result := fControlSlots[ fCurrentControlSlotIdx ];
  fsqlQ.Close;
  fsqlQ.SQL.Text := GetSelect_Sql( SelectPhrase, result, Where );
  fsqlQ.Open;
  //fsqlQ.First;
end;

function TInfoServer.QueryIsOpen : boolean;
begin
  result := fsqlQ.Active;
end;

procedure TInfoServer.CloseQuery;
begin
  fsqlQ.Close;
//From what I could tell, sqlQ should not be creating a tran for me, but, apparently, it is, even from a simple
//select statement. goddamit. This should ends this. Appears to happen only when open method is called on a sqlquery obj.
  fsqlT.EndTransaction;
end;

function TInfoServer.QueryVal( const ColName : string ) : variant;
begin
  ////if fsqlQ.Active then //would rather know that there is a problem
    result := fsqlQ.FieldValues[ ColName ];
end;

function TInfoServer.QueryEof : boolean;
begin
  result := fsqlQ.EOF;
end;

//procedure TInfoServer.QueryFirst;
//begin
//  if fsqlQ.Active then
//    fsqlQ.First;
//end;

procedure TInfoServer.QueryNext;
begin
  if fsqlQ.Active and not fsqlQ.EOF then
    fsqlQ.Next;
end;

{ TFormSettings }

constructor TFormSettings.Create;
begin
//  inherited;
  FFormSettingsSL := TStringList.Create;
  FSettingsList := TStringList.Create;
  FSettingsSectTab := GetDataSection;//cso_SavingServerFormSettings;
  fIsReading := false;
  fIsInitialized := false;
end;

destructor TFormSettings.Destroy;
var
  i : Integer;
begin

  if Assigned( FFormSettingsSL ) then
  begin

    for i := 0 to FFormSettingsSL.Count - 1 do
      if assigned( FFormSettingsSL.Objects[ i ] ) then
      begin
        FFormSettingsSL.Objects[ i ].Free;
        FFormSettingsSL.Objects[ i ] := nil;
      end;

    FFormSettingsSL.Clear;
    FreeAndNil( FFormSettingsSL );

  end;

  if Assigned( fSettingsList ) then
    FreeAndNil( fSettingsList );

  inherited Destroy;

end;

class function TFormSettings.GetDataSection : string;
begin
  result := cso_SavingServerFormSettings;
end;

class procedure TFormSettings.ClearAllFormSettings( IniFile : TJinifile );
begin
  IniFile.EraseSection( GetDataSection );
end;


function TFormSettings.Init( anIniFile : TJiniFIle ) : boolean;
begin

  result := false;

  if DontRun( IsInitialized, format( csoCallOnce, [ 'FormSettings' ] ) )
  then
    exit;

  fIniFile := anIniFile;

  IsInitialized := true;
  result := true;

end;


procedure TFormSettings.CheckIsReading;
begin

  if fIsReading then
  begin
//apparently there was a read that wasn't completed via a WHILE loop. Ditch it.
//this only needs to be done on Additions and Saving
    fSettingsList.Clear;
    fIsReading := false;
  end;

end;

procedure TFormSettings.LoadFormSettings( const FormName : string );
var
  idx : Integer;
  Key : string;
begin

  if DontRun( not IsInitialized, format( csoObjectNotInitialized, [ 'FormSettings', 'LoadFormSettings' ] ) ) then
    exit;

  Key := uppercase( FormName );

  FSettingsList.Clear;

  idx := FFormSettingsSL.IndexOf( Key );
  if idx > -1 then
  begin
    FSettingsList.Assign( TStringlist( FFormSettingsSL.Objects[ idx ] ) );
  end else
  begin

    with fIniFile do
    begin
      if ValueExists( FSettingsSectTab, Key ) then
      begin
          idx := FFormSettingsSL.AddObject( Key, TStringlist.Create );
          ReturnCommaStrAsTStrings( readstring( FSettingsSectTab, Key, '' ),
                                    TStringlist( FFormSettingsSL.Objects[ idx ] ),
                                    csoNonPrintingDelimiter );


          FSettingsList.Assign( TStringlist( FFormSettingsSL.Objects[ idx ] ) );

      end; //ValueExists
    end; //with FIniFile
  end;// Idx > -1

  FIsReading := true;

end;

procedure TFormSettings.SetIsInitialized( AValue : boolean );
begin
  if fIsInitialized = AValue then
    Exit;
  fIsInitialized := AValue;
end;

function TFormSettings.ReadFormSettings( const FormName : string; var Idx : integer; var Value : string ) : boolean;
begin

  Result := false;

  if DontRun( not IsInitialized, format( csoObjectNotInitialized, [ 'FormSettings', 'ReadFormSettings' ] ) ) then
    exit;

  if not FIsReading then
  begin
    LoadFormSettings( FormName );
    Idx := -1;
  end;

  if FSettingsList.Count > 0 then
  begin
//by this method the caller can read the entire list in a "while" statement
    Value := FSettingsList[ 0 ];
    FSettingsList.Delete( 0 );
    Inc( Idx );
    result := true;
  end
  else FIsReading := false;

end;

procedure TFormSettings.RemoveFormSettingFromDB( const FormName : string; DoUpdate : boolean );
begin
  fIniFile.DeleteKey( FSettingsSectTab, FormName );
  if DoUpdate then
    fIniFile.UpdateFile;
end;

procedure TFormSettings.RemoveFormSetting( const FormName : string );
var
  idx : Integer;
  Key : string;
begin
  Key := uppercase( FormName );

  idx := FFormSettingsSL.IndexOf( Key );

  if idx > -1 then
  begin
//Immediate delete from ini
    RemoveFormSettingFromDB( Key );
    FFormSettingsSL.Objects[ idx ].Free;
    FFormSettingsSL.Objects[ idx ] := nil;
    FFormSettingsSL.Delete( idx );
  end;

end;

function TFormSettings.GetListAsTextForSaving( Strings : TStrings ) : string;
var
  i : Integer;
begin
  result := '';

  for i := 0 to Strings.Count - 1 do
    result := result + Strings[ i ] + csoNonPrintingDelimiter;

end;


procedure TFormSettings.SaveFormSettings( const FormName : string );
var
  idx : Integer;
  Key , SaveStr: string;
begin

  if not IsInitialized then
    exit;

  CheckIsReading;

  try

    if DontRun( FSettingsList.Count = 0, csoUseAddSetting ) then
      exit;

    Key := uppercase( FormName );

    idx := FFormSettingsSL.IndexOf( Key );
    if idx > -1 then
      TStringlist( FFormSettingsSL.Objects[ idx ] ).Assign( FSettingsList )
    else
    begin

      idx := FFormSettingsSL.AddObject( Key, TStringlist.Create );
      TStringlist( FFormSettingsSL.Objects[ idx ] ).Assign( FSettingsList );

    end;

//Immediate save to ini
    SaveStr := GetListAsTextForSaving( FSettingsList );
    if SaveStr <> '' then
      FIniFile.WriteString( FSettingsSectTab, Key, SaveStr );

  finally
    FSettingsList.Clear;
  end;

end;

procedure TFormSettings.AddSetting( const ASetting : string );
begin
  CheckIsReading;
  FSettingsList.Add( ASetting );
end;


{ TSharedObj }

constructor TSharedObj.Create;
begin
//  inherited;
  fIsInitialized := false;
end;

destructor TSharedObj.Destroy;
begin
  inherited Destroy;
end;

procedure TSharedObj.SetIsInitialized(AValue : boolean);
begin
  if not FIsInitialized and ( AValue = true ) then
    FIsInitialized := True;
end;


initialization

  InfoServer := TInfoServer.Create;
  FormSettings := TFormSettings.Create;


finalization
  if assigned( InfoServer ) then
    FreeAndNil( InfoServer );
  if assigned( FormSettings ) then
    FreeAndNil( FormSettings );

end.



