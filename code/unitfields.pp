unit unitfields;

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
  , synregexpr //this is the Regexpression unit
  ;

type
  //TFieldNotifyEvent = procedure(Sender: TField) of object;
  //property OnChange: TFieldNotifyEvent read FOnChange write FOnChange;

//TFieldType was conflicting with DB.TFieldType
  TufFieldType = ( ftUndefined, ftString_Key, ftString, ftBoolean, ftInteger, ftList, ftEnum, ftDouble, ftText );

  { TCOField }

  TCOField = class(TObject)
  private

    fFieldType: TufFieldType;
    fIsInternal : boolean;
    fIsKeyField: boolean;

    fBaseCmdObj: TObject;
    fColumnName: string;
    fDoUpdate: boolean;
    procedure SetColumnName(AValue: string);
    procedure SetDoUpdate(AValue: boolean);
    procedure SeTFieldType( AValue : TufFieldType );
    procedure SetIsInternal( AValue : boolean );

  protected
    function GetSQLSaveFieldValuePairInternal( const aValue : string ) : boolean;
    function SaveINIValueBase( theValue: variant ): boolean;
    function LoadIniValueBase: variant;
  public
    constructor Create( aOwner : TObject; const aColumnName : string ); virtual;
    destructor Destroy; override;
    procedure LoadIniValue; virtual; abstract;
    function SaveINIValue: boolean; virtual; abstract;
    function GetSQLSaveFieldValuePair: boolean; virtual; abstract;
    procedure LoadSQLValue( const Idx : integer ); virtual; abstract;

    //runtime
    property DoUpdate: boolean read fDoUpdate write SetDoUpdate;
    property ColumnName: string read fColumnName write SetColumnName;
    property FieldType : TufFieldType read fFieldType write SeTFieldType;
    property IsInternal : boolean read fIsInternal write SetIsInternal;

  end;

  { TCOFieldString }

  TCOFieldString = class(TCOField)
  private
    fValue: string;
    procedure SetValue(AValue: string);
    function EncodeString: string; virtual;
    function DecodeString( const InString : string ) : string; virtual;

  protected
  public
    constructor Create( aOwner: TObject; const aColumnName: string ); override;
    destructor Destroy; override;
    class function Contains_Handler( const SearchFor, SearchIn : string;
                                     const IsPresent, CaseSensitive : boolean) : boolean;
    class function Contains_Handler_Text( const SearchFor : string; SearchIn : string;
                                          const IsPresent, CaseSensitive : boolean) : boolean;
    class function Equals_Handler( const SearchFor : string; SearchIn : string;
                                   const IsEqual, CaseSensitive : boolean) : boolean;
    class function ReqExpr_Handler( RegEx : TRegExpr; const SearchIn : string ) : boolean;
    class function IsEmpty( const SearchIn : string ) : boolean;


    procedure LoadIniValue; override;
    function SaveINIValue: boolean; override;
    function GetSQLSaveFieldValuePair: boolean; override;
    procedure LoadSQLValue( const Idx : integer ); override;

    property Value: string read FValue write SetValue;

  end;


  { TCOFieldDerivedList }

  TCOFieldDerivedList = class(TCOFieldString)
  private
    function EncodeString: string; override;
    function DecodeString( const InString: string ): string; override;
  protected
  public
    constructor Create( aOwner: TObject; const aColumnName: string ); override;
    destructor Destroy; override;
    class function Match( const SearchFor, SearchIn : string; const IsPresent : boolean ) : boolean;
  end;

  { TCOFieldString_KeyField }

  TCOFieldString_KeyField = class(TCOFieldString)
  private
    fControlSectionID : string;
    procedure SetControlSectionID( AValue : string );
  protected
  public
    constructor Create( aOwner: TObject; const aColumnName: string ); override;
    destructor Destroy; override;

    function SaveINIValue: boolean; override;
    procedure CleanUpChangedKeyField( const anOldName : string );
    property ControlSectionID : string read fControlSectionID write SetControlSectionID;

  end;


  { TCOFieldBoolean }

  TCOFieldBoolean = class(TCOField)
  private
    fValue: boolean;
    procedure SetValue(AValue: boolean);
  protected
  public
    constructor Create(aOwner: TObject; const aColumnName: string ); override;
    destructor Destroy; override;
    procedure LoadIniValue; override;
    function SaveINIValue: boolean; override;
    function GetSQLSaveFieldValuePair: boolean; override;
    procedure LoadSQLValue( const Idx : integer ); override;

    property Value: boolean read fValue write SetValue;

  end;

  { TCOFieldInteger }

  TCOFieldInteger = class(TCOField)
  private
    fValue: integer;
    procedure SetValue(AValue: integer);
  protected
  public
    constructor Create( aOwner: TObject; const aColumnName: string ); override;
    destructor Destroy; override;
    procedure LoadIniValue; override;
    function SaveINIValue: boolean; override;
    function GetSQLSaveFieldValuePair: boolean; override;
    procedure LoadSQLValue( const Idx : integer ); override;

    property Value: integer read fValue write SetValue;

  end;


  { TCOFieldEnum }

  TCOFieldEnum = class(TCOField)
  private
    fValue: integer;
    procedure SetValue(AValue: integer);
  protected
  public
    constructor Create( aOwner: TObject; const aColumnName: string ); override;
    destructor Destroy; override;
    procedure LoadIniValue; override;
    function SaveINIValue: boolean; override;
    function GetSQLSaveFieldValuePair: boolean; override;
    procedure LoadSQLValue( const Idx : integer ); override;

    property Value: integer read fValue write SetValue;

  end;

  { TCOFieldDouble }

  TCOFieldDouble = class(TCOField)
  private
    fValue : Double;
    procedure SetValue( AValue : Double );
  protected
  public
    constructor Create( aOwner: TObject; const aColumnName: string ); override;
    destructor Destroy; override;
    procedure LoadIniValue; override;
    function SaveINIValue: boolean; override;
    function GetSQLSaveFieldValuePair: boolean; override;
    procedure LoadSQLValue( const Idx : integer ); override;

    property Value: Double read fValue write SetValue;

  end;


  function FieldTypeToStr_SQL( const FT : TufFieldType ) : string;
  function FieldTypeToStr( const FT : TufFieldType ) : string;
  function StrToFieldType( const FT : string ) : TufFieldType;

const

  cFieldStringTypeDefault = '';
  cFieldBooleanTypeDefault = False;
  cFieldIntegerTypeDefault = -1;
  cFieldEnumTypeDefault = 0;
  cFieldDoubleTypeDefault = 0.0;


implementation

uses
  unitsharedobj
  , juusgen
  , unitcommands
  , unitDBUtils
  , unitGlob
  ;


{ TCOFieldDouble }

procedure TCOFieldDouble.SetValue( AValue : Double );
begin
  if fValue = AValue then Exit;
  fValue := AValue;
  DoUpdate := True;
end;

constructor TCOFieldDouble.Create( aOwner : TObject; const aColumnName : string );
begin
  inherited Create(aOwner, aColumnName);
  fValue := cFieldDoubleTypeDefault;
end;

destructor TCOFieldDouble.Destroy;
begin
  inherited Destroy;
end;

procedure TCOFieldDouble.LoadIniValue;
begin
  fValue := LoadIniValueBase;
end;

function TCOFieldDouble.SaveINIValue : boolean;
begin
  Result := SaveINIValueBase( fValue );
end;

function TCOFieldDouble.GetSQLSaveFieldValuePair : boolean;
begin
  result := GetSQLSaveFieldValuePairInternal( FloatToStr( fValue ) );
end;

procedure TCOFieldDouble.LoadSQLValue( const Idx : integer );
begin
  if Idx < 1 then //internal query from Cmd obj.
    fValue := TBaseCmdObj( fBaseCmdObj ).DBServer.QueryVal( ColumnName )
//external query from search result, inefficient but in theory it's just a single obj load, not a list
  else fValue :=  TBaseCmdObj( fBaseCmdObj ).DBServer.GetFieldValue( ColumnName, inttostr( Idx ) );
  DoUpdate := false;
end;


{ TCOFieldDerivedList }

function TCOFieldDerivedList.EncodeString: string;
begin
  Result := ciniDerivedListBookends + inherited EncodeString + ciniDerivedListBookends;
end;

function TCOFieldDerivedList.DecodeString( const InString : string ) : string;
begin
  Result := inherited DecodeString(
    stringreplace(InString, ciniDerivedListBookends, '', [rfreplaceall]));
end;

constructor TCOFieldDerivedList.Create( aOwner : TObject; const aColumnName : string );
begin
  inherited Create(aOwner, aColumnName);
end;

destructor TCOFieldDerivedList.Destroy;
begin
  inherited Destroy;
end;

class function TCOFieldDerivedList.Match( const SearchFor, SearchIn : string; const IsPresent : boolean ) : boolean;
var
  Idx : SizeInt;
begin

  if SearchFor = '' then
  begin
    if Trim( SearchIn ) = '' then
      Idx := 1
    else Idx := 0;
  end
  else Idx := pos( csoNonPrintingDelimiter + UpperCase( SearchFor ) + csoNonPrintingDelimiter, UpperCase( SearchIn ) );

  case Idx of
    0 : Result := not IsPresent;
    else result := IsPresent;
  end;

end;


{ TCOFieldString }

procedure TCOFieldString.SetValue(AValue: string);
begin
  if fValue = AValue then
    Exit;
  fValue := AValue;
  if fIsKeyField then
    TBaseCmdObj( fBaseCmdObj ).IniSection := trim(fValue);
  fDoUpdate := True;
end;

function TCOFieldString.EncodeString: string;
begin
  Result := stringreplace(fValue, Lineending, csoNonPrintingDelimiter, [rfreplaceall]);
end;

function TCOFieldString.DecodeString( const InString: string ): string;
begin
  Result := stringreplace(InString, csoNonPrintingDelimiter, Lineending, [rfreplaceall]);
end;

function TCOFieldString.GetSQLSaveFieldValuePair: boolean;
begin
  result := GetSQLSaveFieldValuePairInternal( QuotedStr( EncodeString ) );
end;

procedure TCOFieldString.LoadSQLValue( const Idx : integer );
begin
  if Idx < 1 then //internal query from Cmd obj.
    fValue := DecodeString( TBaseCmdObj( fBaseCmdObj ).DBServer.QueryVal( ColumnName ) )
  //external query from search result, inefficient but in theory it's just a single obj load, not a list
  else fValue := DecodeString( TBaseCmdObj( fBaseCmdObj ).DBServer.GetFieldValue( ColumnName, inttostr( Idx ) ) );
  DoUpdate := false;
end;

function TCOFieldString.SaveINIValue: boolean;
begin
  Result := SaveINIValueBase(EncodeString);
end;

procedure TCOFieldString.LoadIniValue;
begin
  fValue := DecodeString(LoadIniValueBase);
end;

constructor TCOFieldString.Create( aOwner : TObject; const aColumnName : string );
begin
  inherited Create(aOwner, aColumnName);
  fValue := cFieldStringTypeDefault;
end;

destructor TCOFieldString.Destroy;
begin
  inherited Destroy;
end;

class function TCOFieldString.Contains_Handler( const SearchFor, SearchIn : string;
                                                const IsPresent, CaseSensitive : boolean ) : boolean;
var
  Idx : SizeInt;
begin

  //No need for this in "contains:
  //SearchIn := stringreplace( SearchIn, csoNonPrintingDelimiter, LineEnding, [ rfreplaceall ] );

  if CaseSensitive then
    Idx := pos( SearchFor, SearchIn )
  else Idx := pos( UpperCase( SearchFor ), UpperCase( SearchIn ) );

  case Idx of
    0 : Result := not IsPresent;
    else result := IsPresent;
  end;

end;

class function TCOFieldString.Contains_Handler_Text( const SearchFor : string; SearchIn : string;
                                                     const IsPresent, CaseSensitive : boolean) : boolean;
var
  Idx : SizeInt;
begin

  SearchIn := stringreplace( SearchIn, csoNonPrintingDelimiter, LineEnding, [ rfreplaceall ] );

  if CaseSensitive then
    Idx := pos( SearchFor, SearchIn )
  else Idx := pos( UpperCase( SearchFor ), UpperCase( SearchIn ) );

  case Idx of
    0 : Result := not IsPresent;
    else result := IsPresent;
  end;

end;

class function TCOFieldString.Equals_Handler( const SearchFor : string; SearchIn : string;
                                              const IsEqual, CaseSensitive : boolean) : boolean;
var
  Idx : SizeInt;
begin

  SearchIn := stringreplace( SearchIn, csoNonPrintingDelimiter, LineEnding, [ rfreplaceall ] );

  if CaseSensitive then
    Idx := CompareStr( SearchFor, SearchIn )
  else Idx := CompareText( SearchFor, SearchIn );

  case Idx of
    0 : Result := IsEqual;
    else result := not IsEqual;
  end;

end;

class function TCOFieldString.ReqExpr_Handler( RegEx : TRegExpr; const SearchIn : string ) : boolean;
begin
  Result := RegEx.Exec( stringreplace( SearchIn, csoNonPrintingDelimiter, LineEnding, [ rfreplaceall ] ) );
end;

class function TCOFieldString.IsEmpty( const SearchIn : string ) : boolean;
begin
  result := trim( SearchIn ) = '';
end;


{ TCOFieldInteger }

procedure TCOFieldInteger.SetValue(AValue: integer);
begin
  if fValue = AValue then
    Exit;
  fValue := AValue;
  DoUpdate := True;
end;

function TCOFieldInteger.GetSQLSaveFieldValuePair: boolean;
begin
  result := GetSQLSaveFieldValuePairInternal( IntToStr(fValue) );
end;

procedure TCOFieldInteger.LoadSQLValue( const Idx : integer );
begin
  if Idx < 1 then //internal query from Cmd obj.
    fValue := TBaseCmdObj( fBaseCmdObj ).DBServer.QueryVal( ColumnName )
//external query from search result, inefficient but in theory it's just a single obj load, not a list
  else fValue :=  TBaseCmdObj( fBaseCmdObj ).DBServer.GetFieldValue( ColumnName, inttostr( Idx ) );
  DoUpdate := false;
end;

function TCOFieldInteger.SaveINIValue: boolean;
begin
  Result := SaveINIValueBase(fValue);
end;

procedure TCOFieldInteger.LoadIniValue;
begin
  fValue := LoadIniValueBase;
end;

constructor TCOFieldInteger.Create( aOwner : TObject; const aColumnName : string );
begin
  inherited Create(aOwner, aColumnName);
  fValue := cFieldIntegerTypeDefault;
end;

destructor TCOFieldInteger.Destroy;
begin
  inherited Destroy;
end;


{ TCOFieldEnum }

procedure TCOFieldEnum.SetValue(AValue: integer);
begin
  if fValue = AValue then
    Exit;
  fValue := AValue;
  DoUpdate := True;
end;


function TCOFieldEnum.GetSQLSaveFieldValuePair: boolean;
begin
  result := GetSQLSaveFieldValuePairInternal( IntToStr(fValue) );
end;

procedure TCOFieldEnum.LoadSQLValue( const Idx : integer );
begin
  if Idx < 1 then //internal query from Cmd obj.
    fValue := TBaseCmdObj( fBaseCmdObj ).DBServer.QueryVal( ColumnName )
//external query from search result, inefficient but in theory it's just a single obj load, not a list
  else fValue :=  TBaseCmdObj( fBaseCmdObj ).DBServer.GetFieldValue( ColumnName, inttostr( Idx ) );
  DoUpdate := false;
end;

function TCOFieldEnum.SaveINIValue: boolean;
begin
  Result := SaveINIValueBase(fValue);
end;

procedure TCOFieldEnum.LoadIniValue;
begin
  fValue := LoadIniValueBase;
end;

constructor TCOFieldEnum.Create( aOwner : TObject; const aColumnName : string );
begin
  inherited Create(aOwner, aColumnName);
  fValue := cFieldEnumTypeDefault;
end;

destructor TCOFieldEnum.Destroy;
begin
  inherited Destroy;
end;


{ TCOFieldBoolean }

procedure TCOFieldBoolean.SetValue(AValue: boolean);
begin
  if fValue = AValue then
    Exit;
  fValue := AValue;
  DoUpdate := True;
end;

function TCOFieldBoolean.GetSQLSaveFieldValuePair: boolean;
begin
  result := GetSQLSaveFieldValuePairInternal( IntToStr( BoolToByte( fValue ) ) );
end;

procedure TCOFieldBoolean.LoadSQLValue( const Idx : integer );
begin
  //sqlite stores boolean as 0/1
  if Idx < 1 then //internal query from Cmd obj.
    fValue := TBaseCmdObj( fBaseCmdObj ).DBServer.QueryVal( ColumnName ) <> 0
//external query from search result, inefficient but in theory it's just a single obj load, not a list
  else fValue :=  TBaseCmdObj( fBaseCmdObj ).DBServer.GetFieldValue( ColumnName, inttostr( Idx ) ) <> 0;
  DoUpdate := false;
end;

function TCOFieldBoolean.SaveINIValue: boolean;
begin
  Result := SaveINIValueBase(fValue);
end;


procedure TCOFieldBoolean.LoadIniValue;
begin
  fValue := LoadIniValueBase;
end;

constructor TCOFieldBoolean.Create( aOwner : TObject; const aColumnName : string );
begin
  inherited Create(aOwner, aColumnName);
  fValue := cFieldBooleanTypeDefault;
end;

destructor TCOFieldBoolean.Destroy;
begin
  inherited Destroy;
end;

{ TCOFieldString_KeyField }

procedure TCOFieldString_KeyField.SetControlSectionID( AValue : string );
begin
  if fControlSectionID = AValue then Exit;
  fControlSectionID := AValue;
end;

constructor TCOFieldString_KeyField.Create( aOwner : TObject; const aColumnName : string );
begin
  inherited Create(aOwner, aColumnName);
  fIsKeyField := True;
end;

destructor TCOFieldString_KeyField.Destroy;
begin
  inherited Destroy;
end;

function TCOFieldString_KeyField.SaveINIValue : boolean;
begin
  TBaseCmdObj( fBaseCmdObj ).DBServer.WriteString( fControlSectionID, fValue, fValue );
  Result := SaveINIValueBase( fValue );
end;

procedure TCOFieldString_KeyField.CleanUpChangedKeyField( const anOldName: string );
begin
  if TBaseCmdObj( fBaseCmdObj ).DBServer.SectionExists(anOldName) then
  begin
    TBaseCmdObj( fBaseCmdObj ).DBServer.RenameSection(anOldName, fValue);
    TBaseCmdObj( fBaseCmdObj ).DBServer.DeleteKey( fControlSectionID, anOldName );
  end;

end;

{ TCOField }

procedure TCOField.SetColumnName(AValue: string);
begin
  if FColumnName = AValue then
    Exit;
  fColumnName := AValue;
end;

procedure TCOField.SetDoUpdate(AValue: boolean);
begin
  if FDoUpdate = AValue then
    Exit;
  fDoUpdate := AValue;
end;

procedure TCOField.SeTFieldType( AValue : TufFieldType );
begin
  if fFieldType = AValue then Exit;
  fFieldType := AValue;
end;

procedure TCOField.SetIsInternal( AValue : boolean );
begin
  if fIsInternal = AValue then Exit;
  fIsInternal := AValue;
end;

function TCOField.GetSQLSaveFieldValuePairInternal( const aValue : string ) : boolean;
begin
  Result := False;
  if not DoUpdate or IsInternal then
    exit;

  TBaseCmdObj( fBaseCmdObj ).SaveFieldNameSL.Add( fColumnName );
  TBaseCmdObj( fBaseCmdObj ).SaveFieldValueSL.Add( aValue );

  Result := True;

end;

function TCOField.SaveINIValueBase(theValue: variant): boolean;
begin

  Result := False;

  if not DoUpdate then
    exit;

  case fFieldType of
    ftString_Key, ftString, ftList, ftText :
      TBaseCmdObj( fBaseCmdObj ).DBServer.Writestring( TBaseCmdObj( fBaseCmdObj ).IniSection, ColumnName, theValue );
    ftBoolean:
      TBaseCmdObj( fBaseCmdObj ).DBServer.WriteBool( TBaseCmdObj( fBaseCmdObj ).IniSection, ColumnName, theValue );
    ftInteger, ftEnum:
      TBaseCmdObj( fBaseCmdObj ).DBServer.WriteInteger( TBaseCmdObj( fBaseCmdObj ).IniSection, ColumnName, theValue );
    ftDouble :
      TBaseCmdObj( fBaseCmdObj ).DBServer.WriteDouble( TBaseCmdObj( fBaseCmdObj ).IniSection, ColumnName, theValue );
    else
      raise EErrorCOField.Create('Unknown FieldType in TCOField.SaveINIValueBase');
  end;

  DoUpdate := false;
  Result := True;

end;

function TCOField.LoadIniValueBase: variant;
begin

  case fFieldType of
    ftString_Key, ftString, ftList, ftText :
      Result := TBaseCmdObj( fBaseCmdObj ).DBServer.ReadString( TBaseCmdObj( fBaseCmdObj ).IniSection, ColumnName, cFieldStringTypeDefault );
    ftBoolean:
      Result := TBaseCmdObj( fBaseCmdObj ).DBServer.ReadBool( TBaseCmdObj( fBaseCmdObj ).IniSection, ColumnName, cFieldBooleanTypeDefault );
    ftInteger, ftEnum:
      Result := TBaseCmdObj( fBaseCmdObj ).DBServer.ReadInteger( TBaseCmdObj( fBaseCmdObj ).IniSection, ColumnName, cFieldIntegerTypeDefault );
    ftDouble :
      Result := TBaseCmdObj( fBaseCmdObj ).DBServer.ReadDouble( TBaseCmdObj( fBaseCmdObj ).IniSection, ColumnName, cFieldDoubleTypeDefault );
    else
      raise EErrorCOField.Create('Unknown FieldType in TCOField.LoadINIValueBase');
  end;
  DoUpdate := false;

end;

constructor TCOField.Create( aOwner : TObject; const aColumnName : string );
begin
  fBaseCmdObj := aOwner;
  fColumnName := aColumnName;
  fDoUpdate := False;
  fFieldType := ftUndefined;
  fIsInternal := true;
end;

destructor TCOField.Destroy;
begin
  inherited Destroy;
end;

function FieldTypeToStr_SQL( const FT : TufFieldType ) : string;
begin
  case FT of
    ftString_Key, ftString, ftList, ftText : result := 'TEXT';
    ftBoolean : result := 'NUMERIC';//if date comes later also numeric
    ftInteger, ftEnum : result := 'INTEGER';
    ftDouble : result := 'REAL';
    else result := 'Invalid';
  end;
end;

function FieldTypeToStr( const FT : TufFieldType ) : string;
begin
  case FT of
    ftUndefined : result := 'UD';
    ftString_Key : result := 'StrKey';
    ftString : result := 'Str';
    ftBoolean : result := 'Bool';
    ftInteger : result := 'Int';
    ftList : result := 'List';
    ftEnum : result := 'Enum';
    ftDouble : result := 'Real';
    ftText : result := 'Text';
    else result := 'Invalid';
  end;
end;

function StrToFieldType( const FT : string ) : TufFieldType;
begin
  case uppercase( FT ) of
    'UD' : result := ftUndefined;
    'STRKEY' : result := ftString_Key;
    'STR' : result := ftString;
    'BOOL' : result := ftBoolean;
    'INT' : result := ftInteger;
    'LIST' : result := ftList;
    'ENUM' : result := ftEnum;
    'REAL' : result := ftDouble;
    'TEXT' : result := ftText;
    else raise EErrorDevelopment.create( format( 'StrToFieldType: "%s" is invalid.', [ FT ] ) );
  end;
end;


end.
