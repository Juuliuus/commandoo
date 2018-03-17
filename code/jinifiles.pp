{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 Erik WachtMeester.

    File which provides TIniFile and friends.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{* Original disclaimer:
 * FCL inifiles.pp rewrite by Erik Wachtmeester (erikw@hotelconcepts.com)
 *
 * Proposed replacement for inifiles.pp v 1.8
 *
 * This version is Borland Delphi 5 compatible, implementing the classes
 * TCustomIniFile, TIniFile and TMemIniFile, with all the public
 * properties and methods that Delphi 5 implements.
 *
 * (inifiles.pp v 1.8 only implements TIniFile with some properties and
 *  methods missing, and some functionality added)
 *
 * In order to stay compatible with v 1.8, I added:
 * - TIniFile can be created and loaded from, and saved to a stream.
 * - ReadSectionRaw method (although it doesn't add empty lines to the
 *   TStrings recipient like v 1.8, since empty lines aren't stored in
 *   the SectionList object structure)
 * - ReadInteger supports '0x' type hex formats
 * - Comment support (this isn't standard in ini files)
 * - EscapeLineFeeds creation parameter
 *
 * Since the SectionList object structure is very different from the
 * way Delphi 5 accesses ini files (Delphi mostly uses Windows calls
 * like GetPrivateProfileString, etc.) it's completely platform
 * independant, and probably faster.
 * The only drawback is memory consumption: all sections, keys and
 * values are kept in memory. But same goes for inifiles.pp v 1.8
 * (the FFileBuffer member) and for Delphi's TMemIniFile.
 * Anyway, Windows restricts ini files to 64K max, so this shouldn't be
 * too much of a problem.
 *
 *
 * Adapted by Julius Heinrich Ludwig Schön / Ronald Michael Spicer
 * Foto.TimePirate.org / TimePirate.org / PaganToday.TimePirate.org
 * Julius@TimePirate.org
 * May 2016 extending the class a bit and making
 * for linux (see note above about windows)
 * frst addtion ability to rename a section.
 *}

unit JiniFiles;

{$mode objfpc}
{$H+}

interface

uses classes, sysutils, contnrs;

type
  { THashedStringList }

  THashedStringList = class(TStringList)
  private
    FValueHash: TFPHashList;
    FNameHash: TFPHashList;
    FValueHashValid: Boolean;
    FNameHashValid: Boolean;
    procedure UpdateValueHash;
    procedure UpdateNameHash;
  protected
    procedure Changed; override;
  public
    destructor Destroy; override;
    function IndexOf(const S: String): Integer; override;
    function IndexOfName(const Name: String): Integer; override;
  end;

  TJiniFileKey = class
  Private
    FIdent: string;
    FValue: string;
  public
    constructor Create(const AIdent, AValue: string);
    property Ident: string read FIdent write FIdent;
    property Value: string read FValue write FValue;
  end;

  TJiniFileKeyList = class(TList)
  private
    function GetItem(Index: integer): TJiniFileKey;
    function KeyByName(const AName: string; CaseSensitive : Boolean): TJiniFileKey;
  public
    destructor Destroy; override;
    procedure Clear; override;
    property Items[Index: integer]: TJiniFileKey read GetItem; default;
  end;

  { TJiniFileSection }

  TJiniFileSection = class
  private
    FName: string;
    FKeyList: TJiniFileKeyList;
    procedure SetName( AName : string );
  public
    Function Empty : Boolean;
    constructor Create(const AName: string);
    destructor Destroy; override;
    property Name: string read FName write SetName;
    property KeyList: TJiniFileKeyList read FKeyList;
  end;

  TJiniFileSectionList = class(TList)
  private
    function GetItem(Index: integer): TJiniFileSection;
    function SectionByName(const AName: string; CaseSensitive : Boolean): TJiniFileSection;
  public
    destructor Destroy; override;
    procedure Clear;override;
    property Items[Index: integer]: TJiniFileSection read GetItem; default;
  end;

  { TCustomIniFile }

  TCustomIniFile = class
  Private
    FFileName: string;
    FSectionList: TJiniFileSectionList;
    FEscapeLineFeeds: boolean;
    FCaseSensitive : Boolean;
    FStripQuotes : Boolean;
  public
    constructor Create(const AFileName: string; AEscapeLineFeeds : Boolean = False); virtual;
    destructor Destroy; override;
    function SectionExists(const Section: string): Boolean; virtual;
    function ReadString(const Section, Ident, Default: string): string; virtual; abstract;
    procedure WriteString(const Section, Ident, Value: String); virtual; abstract;
    function ReadInteger(const Section, Ident: string; Default: Longint): Longint; virtual;
    procedure WriteInteger(const Section, Ident: string; Value: Longint); virtual;
    function ReadInt64(const Section, Ident: string; Default: Int64): Longint; virtual;
    procedure WriteInt64(const Section, Ident: string; Value: Int64); virtual;
    function ReadBool(const Section, Ident: string; Default: Boolean): Boolean; virtual;
    procedure WriteBool(const Section, Ident: string; Value: Boolean); virtual;
    function ReadDate(const Section, Ident: string; Default: TDateTime): TDateTime; virtual;
    function ReadDateTime(const Section, Ident: string; Default: TDateTime): TDateTime; virtual;
    function ReadFloat(const Section, Ident: string; Default: Double): Double; virtual;
    function ReadTime(const Section, Ident: string; Default: TDateTime): TDateTime; virtual;
    function ReadBinaryStream(const Section, Name: string; Value: TStream): Integer; virtual;
    procedure WriteDate(const Section, Ident: string; Value: TDateTime); virtual;
    procedure WriteDateTime(const Section, Ident: string; Value: TDateTime); virtual;
    procedure WriteFloat(const Section, Ident: string; Value: Double); virtual;
    procedure WriteTime(const Section, Ident: string; Value: TDateTime); virtual;
    procedure WriteBinaryStream(const Section, Name: string; Value: TStream); virtual;
    procedure ReadSection(const Section: string; Strings: TStrings); virtual; abstract;
    procedure ReadSections(Strings: TStrings); virtual; abstract;
    procedure ReadSectionValues(const Section: string; Strings: TStrings); virtual; abstract;
    procedure EraseSection(const Section: string); virtual; abstract;
    procedure DeleteKey(const Section, Ident: String); virtual; abstract;
    procedure UpdateFile; virtual; abstract;
    function ValueExists(const Section, Ident: string): Boolean; virtual;
    property FileName: string read FFileName;
    property EscapeLineFeeds: boolean read FEscapeLineFeeds;
    Property CaseSensitive : Boolean Read FCaseSensitive Write FCaseSensitive;
    Property StripQuotes : Boolean Read FStripQuotes Write FStripQuotes;
  end;

  { TJiniFile }

  TJiniFile = class(TCustomIniFile)
  Private
    fDoTrimValues : boolean;
    FStream: TStream;
    FCacheUpdates: Boolean;
    FDirty : Boolean;
    procedure FillSectionList(AStrings: TStrings);
    Procedure DeleteSection(ASection : TJiniFileSection);
    Procedure MaybeDeleteSection(ASection : TJiniFileSection);
    procedure SetCacheUpdates(const AValue: Boolean);
  protected
    procedure MaybeUpdateFile;
    property Dirty : Boolean Read FDirty;
  public
    constructor Create(const AFileName : string; AEscapeLineFeeds : Boolean = False); override;
    constructor Create(AStream : TStream; AEscapeLineFeeds : Boolean = False);
    destructor Destroy; override;
    function ReadString(const Section, Ident, Default: string): string; override;
    procedure WriteString(const Section, Ident, Value: String); override;
    procedure ReadSection(const Section: string; Strings: TStrings); override;
    procedure ReadSectionRaw(const Section: string; Strings: TStrings);
    procedure ReadSections(Strings: TStrings); override;
    procedure ReadSectionValues(const Section: string; Strings: TStrings); override;
    procedure EraseSection(const Section: string); override;
    procedure RenameSection (const OldSection, NewSection : String );
    procedure DeleteKey(const Section, Ident: String); override;
    procedure UpdateFile; override;
    procedure GetSectionList( Strings : TStrings );
    procedure ShowSectionList(SearchTerm : string; Strings : TStrings);
    function SaveList( SectTabName : string; anSL : TStrings ) : boolean;
    function LoadList( const SectTabName : string; anSL : TStrings ) : boolean;
    procedure WriteSetIniFileWarning( SaveNow : boolean = true );
    property Stream: TStream read FStream;
    property CacheUpdates : Boolean read FCacheUpdates write SetCacheUpdates;
  end;

  TMemIniFile = class(TJiniFile)
  public
    constructor Create(const AFileName: string; AEscapeLineFeeds : Boolean = False); override;
    procedure Clear;
    procedure GetStrings(List: TStrings);
    procedure Rename(const AFileName: string; Reload: Boolean);
    procedure SetStrings(List: TStrings);
  end;

implementation

Resourcestring
  SErrCouldNotCreatePath = 'Could not create directory "%s"';


const
   Brackets  : array[0..1] of Char = ('[', ']');
   Separator : Char = '=';
   Comment   : Char = ';';
   LF_Escape : Char = '\';
   jIniFileWarning1 = 'This is an automatically generated and maintained Data file.';
   jIniFileWarning2 =  '   --- Edit this file by hand AT YOUR OWN RISK! ---';
   jIniFileWarningLine = '=================================';
   jIniFileWarning = 'WARNING';
   jIniFileWarningKey = 'README%s';

function CharToBool(AChar: char): boolean;
begin
  Result := (Achar = '1');
end;

function BoolToChar(ABool: boolean): char;
begin
  if ABool then
    Result := '1'
  else
    Result := '0';
end;

function IsComment(const AString: string): boolean;
begin
  Result := False;
  if AString > '' then
    Result := (Copy(AString, 1, 1) = Comment);
end;

{ THashedStringList }

destructor THashedStringList.Destroy;
begin
  FreeAndNil(FValueHash);
  FreeAndNil(FNameHash);
  inherited Destroy;
end;

function THashedStringList.IndexOf(const S: String): Integer;
var
  I: Integer;
begin
  if not FValueHashValid then
    UpdateValueHash;

  I := FValueHash.FindIndexOf(S);
  if I >= 0 then
//    Result := Integer(FValueHash[I])-1
    Result := PtrUInt(FValueHash[I])-1
  else
    Result := -1;
end;

function THashedStringList.IndexOfName(const Name: String): Integer;
var
  I: Integer;
begin
  if not FNameHashValid then
    UpdateNameHash;

  I := FNameHash.FindIndexOf(Name);
  if I >= 0 then
//    Result := Integer(FNameHash[I])-1
    Result := PtrUInt(FNameHash[I])-1
  else
    Result := -1;
end;

procedure THashedStringList.Changed;
begin
  FValueHashValid := False;
  FNameHashValid := False;
  inherited Changed;
end;

procedure THashedStringList.UpdateValueHash;
var
  I: Integer;
begin
  if not Assigned(FValueHash) then
    FValueHash := TFPHashList.Create
  else
    FValueHash.Clear;
  for I := 0 to Count - 1 do
    FValueHash.Add(Strings[I], Pointer(I+1));
  FValueHashValid := True;
end;

procedure THashedStringList.UpdateNameHash;
var
  I: Integer;
begin
  if not Assigned(FNameHash) then
    FNameHash := TFPHashList.Create
  else
    FNameHash.Clear;
  for I := 0 to Count - 1 do
    FNameHash.Add(Names[I], Pointer(I+1));
  FNameHashValid := True;
end;

{ TJiniFileKey }

constructor TJiniFileKey.Create(const AIdent, AValue: string);
begin
  FIdent := AIdent;
  FValue := AValue;
end;

{ TJiniFileKeyList }

function TJiniFileKeyList.GetItem(Index: integer): TJiniFileKey;
begin
  Result := nil;
  if (Index >= 0) and (Index < Count) then
    Result := TJiniFileKey(inherited Items[Index]);
end;

function TJiniFileKeyList.KeyByName(const AName: string; CaseSensitive : Boolean): TJiniFileKey;
var
  i: integer;
begin
  Result := nil;
  if (AName > '') and not IsComment(AName) then
    If CaseSensitive then
      begin
      for i := 0 to Count-1 do
        if Items[i].Ident=AName then
          begin
          Result := Items[i];
          Break;
          end;
      end
    else
      for i := 0 to Count-1 do
        if CompareText(Items[i].Ident, AName) = 0 then begin
          Result := Items[i];
          Break;
        end;
end;

destructor TJiniFileKeyList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJiniFileKeyList.Clear;
var
  i: integer;
begin
  for i := Count-1 downto 0 do
    Items[i].Free;
  inherited Clear;
end;

procedure TJiniFileSection.SetName (AName : string );
begin
  if FName = AName then exit;
  FName := AName;
end;

function TJiniFileSection.Empty : Boolean;

Var
  I : Integer;

begin
  Result:=True;
  I:=0;
  While Result and (I<KeyList.Count)  do
    begin
    result:=IsComment(KeyList[i].Ident);
    Inc(i);
    end;
end;


{ TJiniFileSection }

constructor TJiniFileSection.Create(const AName: string);
begin
  FName := AName;
  FKeyList := TJiniFileKeyList.Create;
end;

destructor TJiniFileSection.Destroy;
begin
  FKeyList.Free;
end;

{ TJiniFileSectionList }

function TJiniFileSectionList.GetItem(Index: integer): TJiniFileSection;
begin
  Result := nil;
  if (Index >= 0) and (Index < Count) then
    Result := TJiniFileSection(inherited Items[Index]);
end;

function TJiniFileSectionList.SectionByName(const AName: string; CaseSensitive : Boolean): TJiniFileSection;
var
  i: integer;
begin
  Result := nil;
  if (AName > '') and not IsComment(AName) then
    If CaseSensitive then
      begin
      for i:=0 to Count-1 do
        if (Items[i].Name=AName) then
          begin
          Result := Items[i];
          Break;
          end;
      end
    else
      for i := 0 to Count-1 do
        if CompareText(Items[i].Name, AName) = 0 then
          begin
          Result := Items[i];
          Break;
          end;
end;

destructor TJiniFileSectionList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJiniFileSectionList.Clear;
var
  i: integer;
begin
  for i := Count-1 downto 0 do
  begin
    Items[i].Free;
    //Items[ i ] := nil;
  end;
  inherited Clear;
end;

{ TCustomIniFile }

constructor TCustomIniFile.Create(const AFileName: string; AEscapeLineFeeds : Boolean = False);
begin
  FFileName := AFileName;
  FSectionList := TJiniFileSectionList.Create;
  FEscapeLineFeeds := AEscapeLineFeeds;
end;

destructor TCustomIniFile.Destroy;
begin
  FSectionList.Free;
  inherited Destroy;
end;

function TCustomIniFile.SectionExists(const Section: string): Boolean;

Var
  S : TJiniFileSection;

begin
  S:=FSectionList.SectionByName(Section,CaseSensitive);
  Result:=Assigned(S) and Not S.Empty;
end;

function TCustomIniFile.ReadInteger(const Section, Ident: string; Default: Longint): Longint;
begin
  // StrToInfDef() supports hex numbers prefixed with '0x' via val()
  Result := StrToIntDef(ReadString(Section, Ident, ''), Default);
end;

procedure TCustomIniFile.WriteInteger(const Section, Ident: string; Value: Longint);
begin
  WriteString(Section, Ident, IntToStr(Value));
end;

function TCustomIniFile.ReadInt64(const Section, Ident: string; Default: Int64
  ): Longint;
begin
  Result := StrToInt64Def(ReadString(Section, Ident, ''), Default);
end;

procedure TCustomIniFile.WriteInt64(const Section, Ident: string; Value: Int64);
begin
  WriteString(Section, Ident, IntToStr(Value));
end;

function TCustomIniFile.ReadBool(const Section, Ident: string; Default: Boolean): Boolean;
var
  s: string;
begin
  Result := Default;
  s := ReadString(Section, Ident, '');
  if s > '' then
    Result := CharToBool(s[1]);
end;

procedure TCustomIniFile.WriteBool(const Section, Ident: string; Value: Boolean);
begin
  WriteString(Section, Ident, BoolToChar(Value));
end;

function TCustomIniFile.ReadDate(const Section, Ident: string; Default: TDateTime): TDateTime;

begin
  Result := StrToDateDef(ReadString(Section, Ident, ''),Default);
end;

function TCustomIniFile.ReadDateTime(const Section, Ident: string; Default: TDateTime): TDateTime;

begin
  Result := StrToDateTimeDef(ReadString(Section, Ident, ''),Default);
end;

function TCustomIniFile.ReadFloat(const Section, Ident: string; Default: Double): Double;

begin
  Result:=StrToFloatDef(ReadString(Section, Ident, ''),Default);
end;

function TCustomIniFile.ReadTime(const Section, Ident: string; Default: TDateTime): TDateTime;

begin
  Result := StrToTimeDef(ReadString(Section, Ident, ''),Default);
end;

procedure TCustomIniFile.WriteDate(const Section, Ident: string; Value: TDateTime);
begin
  WriteString(Section, Ident, DateToStr(Value));
end;

procedure TCustomIniFile.WriteDateTime(const Section, Ident: string; Value: TDateTime);
begin
  WriteString(Section, Ident, DateTimeToStr(Value));
end;

procedure TCustomIniFile.WriteFloat(const Section, Ident: string; Value: Double);
begin
  WriteString(Section, Ident, FloatToStr(Value));
end;

procedure TCustomIniFile.WriteTime(const Section, Ident: string; Value: TDateTime);
begin
  WriteString(Section, Ident, TimeToStr(Value));
end;

function TCustomIniFile.ValueExists(const Section, Ident: string): Boolean;
var
  oSection: TJiniFileSection;
begin
  Result := False;
  oSection := FSectionList.SectionByName(Section,CaseSensitive);
  if oSection <> nil then
    Result := (oSection.KeyList.KeyByName(Ident,CaseSensitive) <> nil);
end;

function TCustomIniFile.ReadBinaryStream(const Section, Name: string; Value: TStream): Integer;

Var
  //M : TMemoryStream;
  S : String;
  PB,PR : PByte;
  PC : PChar;
  H : String[3];
  i,{l2,} code : Integer;


begin
  S:=ReadString(Section,Name,'');
  Setlength(H,3);
  H[1]:='$';
  Result:=Length(S) div 2;
  If Result>0 then
    begin
    GetMem(PR,Result);
    Try
      PC:=PChar(S);
      PB:=PR;
      For I:=1 to Result do
        begin
        H[2]:=PC[0];
        H[3]:=PC[1];
        Val(H,PB^,code);
        Inc(PC,2);
        Inc(PB);
        end;
      Value.WriteBuffer(PR^,Result);
    finally
      FreeMem(PR);
    end;
    end;
end;

procedure TCustomInifile.WriteBinaryStream(const Section, Name: string; Value: TStream);


Var
  M : TMemoryStream;
  S : String;
  PB : PByte;
  PC : PChar;
  H : String[2];
  i : Integer;

begin
  M:=TMemoryStream.Create;
  Try
    M.CopyFrom(Value,0);
    SetLength(S,M.Size*2);
    If (length(S)>0) then
      begin
      PB:=M.Memory;
      PC:=PChar(S);
      For I:=1 to Length(S) div 2 do
        begin
        H:=HexStr(PB^,2);
        PC[0]:=H[1];
        PC[1]:=H[2];
        Inc(PC,2);
        Inc(PB);
        end;
      end;
    WriteString(Section,Name,S);
  Finally
    M.Free;
  end;
end;

{ TJiniFile }

constructor TJiniFile.Create(const AFileName: string; AEscapeLineFeeds : Boolean = False);
var
  slLines: TStringList;
begin
  //Can't set property because SectionList is filled here, don't want to redo the
  //create chain, so, since I don't use EscapeLineFeeds, I'll use default false here
  fDoTrimValues := AEscapeLineFeeds;
  If Not (self is TMemIniFile) then
    StripQuotes:=True;
  inherited Create(AFileName,AEscapeLineFeeds);
  FStream := nil;
  slLines := TStringList.Create;
  try
    if FileExists(FFileName) then
      begin
      // read the ini file values
      slLines.LoadFromFile(FFileName);
      FillSectionList(slLines);
      end;
    slLines.Clear;
  finally
    slLines.Free;
//trying to remove mem leaks
    slLines := nil;
  end;
end;

constructor TJiniFile.Create(AStream: TStream; AEscapeLineFeeds : Boolean = False);
var
  slLines: TStringList;
begin
  fDoTrimValues := AEscapeLineFeeds;
  inherited Create('',AEscapeLineFeeds);
  FStream := AStream;
  slLines := TStringList.Create;
  try
    // read the ini file values
    slLines.LoadFromStream(FStream);
    FillSectionList(slLines);
  finally
    slLines.Free;
    slLines := nil;
  end;
end;

destructor TJiniFile.Destroy;
begin
  If FDirty and FCacheUpdates then
    try
      UpdateFile;
    except
      // Eat exception. Compatible to D7 behaviour, see comments to bug 19046
    end;
  inherited destroy;
end;

procedure TJiniFile.FillSectionList(AStrings: TStrings);
var
  i,j: integer;
  sLine, sIdent, sValue: string;
  oSection: TJiniFileSection;

  procedure RemoveBackslashes;
  var
    i,l: integer;
    s: string;
    //bAppendNextLine, bAppended: boolean;
  begin
    AStrings.BeginUpdate;
    try
      For I:=AStrings.Count-2 downto 0 do
        begin
        S:=AStrings[i];
        L:=Length(S);
        If (I<AStrings.Count-1) and (L>0) and (S[L]=LF_Escape) then
          begin
          S:=Copy(S,1,L-1)+AStrings[I+1];
          AStrings.Delete(I+1);
          AStrings[i]:=S;
          end;
        end;
    finally
      AStrings.EndUpdate;
    end;
  end;

begin
  oSection := nil;
  FSectionList.Clear;
  if FEscapeLineFeeds then
    RemoveBackslashes;
  for i := 0 to AStrings.Count-1 do
  begin
  //Julius had to change this as it affected how my values are read.
  //sLine := Trim(AStrings[i]);
    sLine := AStrings[i];
    if fDoTrimValues then
      sLine := trim( sLine );
    if sLine > '' then
      begin
      if IsComment(sLine) and (oSection = nil) then begin
        // comment at the beginning of the ini file
        oSection := TJiniFileSection.Create(sLine);
        FSectionList.Add(oSection);
        continue;
      end;
      if (Copy(sLine, 1, 1) = Brackets[0]) and (Copy(sLine, length(sLine), 1) = Brackets[1]) then begin
        // regular section
        oSection := TJiniFileSection.Create(Copy(sLine, 2, Length(sLine) - 2));
        FSectionList.Add(oSection);
      end else if oSection <> nil then begin
        if IsComment(sLine) then begin
          // comment within a section
          sIdent := sLine;
          sValue := '';
        end else begin
          // regular key
          j:=Pos(Separator, sLine);
          if j=0 then
           begin
             sIdent:='';
             sValue:=sLine
           end
          else
           begin
             sIdent:=Copy(sLine, 1,  j - 1);
             sValue:=Copy(sLine, j + 1, Length(sLine) - j);
             if fDoTrimValues then
             begin
               sIdent:= trim( sIdent );
               sValue:= trim( sValue );
             end;

             //sIdent:=Trim(Copy(sLine, 1,  j - 1));
             //sValue:=Trim(Copy(sLine, j + 1, Length(sLine) - j));
           end;
        end;
        oSection.KeyList.Add(TJiniFileKey.Create(sIdent, sValue));
      end;
      end;
  end;
//trying to fix memory leaks.
  oSection := nil;
end;

function TJiniFile.ReadString(const Section, Ident, Default: string): string;
var
  oSection: TJiniFileSection;
  oKey: TJiniFileKey;
  J: integer;
begin
  Result := Default;
  oSection := FSectionList.SectionByName(Section,CaseSensitive);
  if oSection <> nil then begin
    oKey := oSection.KeyList.KeyByName(Ident,CaseSensitive);
    if oKey <> nil then
      If StripQuotes then
      begin
        J:=Length(oKey.Value);
        // Joost, 2-jan-2007: The check (J>1) is there for the case that
        // the value consist of a single double-quote character. (see
        // mantis bug 6555)
        If (J>1) and ((oKey.Value[1] in ['"','''']) and (oKey.Value[J]=oKey.Value[1])) then
           Result:=Copy(oKey.Value,2,J-2)
        else
           Result:=oKey.Value;
      end
      else Result:=oKey.Value;
    end;
  end;

procedure TJiniFile.SetCacheUpdates(const AValue: Boolean);
begin
//causes exceptions on bad files, I want to control updates, changed in jinifile.pp
//was automatically doing updates.
  //if FCacheUpdates and not AValue and FDirty then
  //  UpdateFile;
  FCacheUpdates := AValue;
end;


procedure TJiniFile.WriteString( const Section, Ident, Value: String );
var
  oSection: TJiniFileSection;
  oKey: TJiniFileKey;
begin
  if (Section > '') and (Ident > '') then
    begin
    // update or add key
    oSection := FSectionList.SectionByName(Section,CaseSensitive);
    if (oSection = nil) then
      begin
      oSection := TJiniFileSection.Create(Section);
      FSectionList.Add(oSection);
      end;
    with oSection.KeyList do
      begin
      oKey := KeyByName(Ident,CaseSensitive);
      if oKey <> nil then
        oKey.Value := Value
      else
        oSection.KeyList.Add(TJiniFileKey.Create(Ident, Value));
      end;
    end;
  MaybeUpdateFile;
end;

procedure TJiniFile.ReadSection(const Section: string; Strings: TStrings);
var
  oSection: TJiniFileSection;
  i: integer;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    oSection := FSectionList.SectionByName(Section,CaseSensitive);
    if oSection <> nil then with oSection.KeyList do
      for i := 0 to Count-1 do
        if not IsComment(Items[i].Ident) then
          Strings.Add(Items[i].Ident);
  finally
    Strings.EndUpdate;
  end;
end;

procedure TJiniFile.ReadSectionRaw(const Section: string; Strings: TStrings);
var
  oSection: TJiniFileSection;
  i: integer;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    oSection := FSectionList.SectionByName(Section,CaseSensitive);
    if oSection <> nil then with oSection.KeyList do
      for i := 0 to Count-1 do
        if not IsComment(Items[i].Ident) then
         begin
           if Items[i].Ident<>'' then
            Strings.Add(Items[i].Ident + Separator +Items[i].Value)
           else
            Strings.Add(Items[i].Value);
         end;
  finally
    Strings.EndUpdate;
  end;
end;

procedure TJiniFile.ReadSections(Strings: TStrings);
var
  i: integer;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    for i := 0 to FSectionList.Count-1 do
      if not IsComment(FSectionList[i].Name) then
        Strings.Add(FSectionList[i].Name);
  finally
    Strings.EndUpdate;
  end;
end;

procedure TJiniFile.ReadSectionValues(const Section: string; Strings: TStrings);
var
  oSection: TJiniFileSection;
  s: string;
  i,J: integer;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    oSection := FSectionList.SectionByName(Section,CaseSensitive);
    if oSection <> nil then with oSection.KeyList do
      for i := 0 to Count-1 do begin
        s := Items[i].Value;
      If StripQuotes then
        begin
          J:=Length(s);
          // Joost, 2-jan-2007: The check (J>1) is there for the case that
          // the value consist of a single double-quote character. (see
          // mantis bug 6555)
          If (J>1) and ((s[1] in ['"','''']) and (s[J]=s[1])) then
             s:=Copy(s,2,J-2);
        end;
        if Items[i].Ident<>'' then
          s:=Items[i].Ident+Separator+s;
        Strings.Add(s);
      end;
  finally
    Strings.EndUpdate;
  end;
end;

procedure TJiniFile.DeleteSection(ASection : TJiniFileSection);

begin
  FSectionList.Delete(FSectionList.IndexOf(ASection));
  ASection.Free;
end;

procedure TJiniFile.MaybeDeleteSection(ASection : TJiniFileSection);

begin
  If Asection.Empty then
    DeleteSection(ASection);
end;

procedure TJiniFile.EraseSection(const Section: string);
var
  oSection: TJiniFileSection;
begin
  oSection := FSectionList.SectionByName(Section,CaseSensitive);
  if oSection <> nil then begin
    { It is needed so UpdateFile doesn't find a defunct section }
    { and cause the program to crash }
    DeleteSection(OSection);
    MaybeUpdateFile;
  end;
end;


procedure TJiniFile.RenameSection(const OldSection, NewSection : String);
var
 oSection: TJIniFileSection;
// oKey: TIniFileKey;
begin
  oSection := FSectionList.SectionByName(OldSection,CaseSensitive);
  oSection.SetName( NewSection );
  //if oSection <> nil then
  //  begin
  //  oKey := oSection.KeyList.KeyByName(Ident,CaseSensitive);
  //  if oKey <> nil then
  //    begin
  //    oSection.KeyList.Delete(oSection.KeyList.IndexOf(oKey));
  //    oKey.Free;
  //    MaybeUpdateFile;
  //    end;
  //  end;
end;


procedure TJiniFile.DeleteKey(const Section, Ident: String);
var
 oSection: TJiniFileSection;
 oKey: TJiniFileKey;
begin
  oSection := FSectionList.SectionByName(Section,CaseSensitive);
  if oSection <> nil then
    begin
    oKey := oSection.KeyList.KeyByName(Ident,CaseSensitive);
    if oKey <> nil then
      begin
      oSection.KeyList.Delete(oSection.KeyList.IndexOf(oKey));
      oKey.Free;
      MaybeUpdateFile;
      end;
    end;
end;

procedure TJiniFile.UpdateFile;
var
  slLines: TStringList;
  i, j: integer;
  D : String;

begin
  slLines := TStringList.Create;
  try
    for i := 0 to FSectionList.Count-1 do
      with FSectionList[i] do begin
        if IsComment(Name) then
          // comment
          slLines.Add(Name)
        else
          // regular section
          slLines.Add(Brackets[0] + Name + Brackets[1]);
        for j := 0 to KeyList.Count-1 do
          if IsComment(KeyList[j].Ident) then
            // comment
            slLines.Add(KeyList[j].Ident)
          else
            // regular key
            slLines.Add(KeyList[j].Ident + Separator + KeyList[j].Value);
        if (i < FSectionList.Count-1) and not IsComment(Name) then
          slLines.Add('');
      end;
    if FFileName > '' then
      begin
      D:=ExtractFilePath(FFileName);
      If D <> '' Then
        if not ForceDirectories(D) then
          Raise EInoutError.CreateFmt(SErrCouldNotCreatePath,[D]);
      slLines.SaveToFile(FFileName);
      end
    else if FStream <> nil then
      slLines.SaveToStream(FStream);
    FillSectionList(slLines);
    FDirty := false;
  finally
    slLines.Free;
    slLines := nil;
  end;
end;

procedure TJiniFile.GetSectionList(Strings : TStrings);
var
  i : Integer;
begin
  for i := 0 to FSectionList.Count - 1 do
    Strings.Add( FSectionList[ i ].Name );
//  Strings.Assign( FSectionList.Items );
end;


function TJiniFile.LoadList( const SectTabName : string; anSL : TStrings) : boolean ;
var
  SL : TStringList;
  i : Integer;
begin

  result := false;

  anSL.Clear;
  anSL.BeginUpdate;

  SL := TStringList.Create;
  try
    Readsection( SectTabName, SL );
    for i := 0 to SL.Count - 1 do
      AnSL.Add( Readstring ( SectTabName, SL[ i ], '' ) );

  finally
    SL.Free;
  end;

  anSL.EndUpdate;
  result := true;

end ;


function TJiniFile.SaveList( SectTabName : string; anSL : TStrings ) : boolean;
var
  i : Integer;
begin

  result := false;

  if anSL.Count > 0 then
  begin

    EraseSection( SectTabName );

    for i := 0 to anSL.Count - 1 do
      writestring ( SectTabName, inttostr( i ), anSL[ i ] );

    //if DoCommit then
      UpdateFile;

  end;

  result := true;

end ;

procedure TJiniFile.WriteSetIniFileWarning( SaveNow : boolean = true );
begin
  WriteString( jIniFileWarning, format( jIniFileWarningKey, [ '1' ] ), jIniFileWarningLine );
  WriteString( jIniFileWarning, format( jIniFileWarningKey, [ '2' ] ), '' );
  WriteString( jIniFileWarning, format( jIniFileWarningKey, [ '3' ] ), jIniFileWarning1 );
  WriteString( jIniFileWarning, format( jIniFileWarningKey, [ '4' ] ), jIniFileWarning2 );
  WriteString( jIniFileWarning, format( jIniFileWarningKey, [ '5' ] ), '' );
  WriteString( jIniFileWarning, format( jIniFileWarningKey, [ '6' ] ), jIniFileWarningLine );

  if SaveNow then
    UpdateFile;
end;


procedure TJiniFile.ShowSectionList(SearchTerm : string; Strings : TStrings);
var
  i, j : Integer;
  Key : TJiniFileKey;
begin
//  Strings.Clear;
//  SearchTerm := uppercase( SearchTerm );
  Strings.BeginUpdate;
  for i := 0 to FSectionList.Count - 1 do
  begin

    for j := 0 to FSectionList[ i ].KeyList.Count - 1 do
    begin
      Key := TJiniFileKey( FSectionList[ i ].KeyList[ j ] );
      if Key.Ident + Separator + Key.Value = SearchTerm then
      begin
        Strings.Add( FSectionList[ i ].Name
                     + ': '
                     + Key.Ident
                     + '  '
                     + Key.Value );
        if pos( '//', FSectionList[ i ].Name ) > 1 then
        begin
          Strings.add( ReadString( FSectionList[ i ].Name, 'Entry', '<error>' ) );
        end;
      end;

    end;

//    FSectionList[ i ].KeyList[ j ].Ident + Separator + FSectionList[ i ].KeyList[ j ].Value;


  //  if ValueExists( TJiniFileSection( FSectionList[ i ] ).Name, 'Notes' ) then
  //  begin
  //  Str := uppercase( readstring( TJiniFileSection( FSectionList[ i ] ).Name, 'Notes', '' ) );
  ////    if pos( uppercase( readstring( TJiniFileSection( FSectionList[ i ] ).Name, 'Notes', '' ) ), SearchTerm ) > 0 then
  //    if pos( SearchTerm, uppercase( readstring( TJiniFileSection( FSectionList[ i ] ).Name, 'Notes', '' ) ) ) > 0 then
  //      Strings.Add( TJiniFileSection( FSectionList[ i ] ).Name );
  //
  //  end;






    //Str := TJiniFileSection( FSectionList[ i ] ).Name;
    //Strings.Add( Str );
    //TJiniFileSection = class
    //private
    //  FName: string;
    //  FKeyList: TJiniFileKeyList;
    //  procedure SetName( AName : string );
    //public
    //  Function Empty : Boolean;
    //  constructor Create(const AName: string);
    //  destructor Destroy; override;
    //  property Name: string read FName write SetName;
    //  property KeyList: TJiniFileKeyList read FKeyList;
    //end;
    //
    //TJiniFileSectionList = class(TList)
    //private
    //  function GetItem(Index: integer): TJiniFileSection;

  end;
  Strings.EndUpdate;
end;

procedure TJiniFile.MaybeUpdateFile;
begin
  If FCacheUpdates then
    FDirty:=True
  else
    UpdateFile;
end;

{ TMemIniFile }

constructor TMemIniFile.Create(const AFileName: string; AEscapeLineFeeds : Boolean = False);

begin
  Inherited;
  FCacheUpdates:=True;
end;

procedure TMemIniFile.Clear;
begin
  FSectionList.Clear;
end;

procedure TMemIniFile.GetStrings(List: TStrings);
var
  i, j: integer;
  oSection: TJiniFileSection;
begin
  List.BeginUpdate;
  try
    for i := 0 to FSectionList.Count-1 do begin
      oSection := FSectionList[i];
      with oSection do begin
        if IsComment(Name) then
          List.Add(Name)
        else
          List.Add(Brackets[0] + Name + Brackets[1]);
        for j := 0 to KeyList.Count-1 do begin
          if IsComment(KeyList[j].Ident) then
            List.Add(KeyList[j].Ident)
          else
            List.Add(KeyList[j].Ident + Separator + KeyList[j].Value);
        end;
      end;
      if i < FSectionList.Count-1 then
        List.Add('');
    end;
  finally
    List.EndUpdate;
  end;
end;

procedure TMemIniFile.Rename(const AFileName: string; Reload: Boolean);
var
  slLines: TStringList;
begin
  FFileName := AFileName;
  FStream := nil;
  if Reload then begin
    slLines := TStringList.Create;
    try
      slLines.LoadFromFile(FFileName);
      FillSectionList(slLines);
    finally
      slLines.Free;
      slLines := nil;
    end;
  end;
end;

procedure TMemIniFile.SetStrings(List: TStrings);
begin
  FillSectionList(List);
end;

end.


