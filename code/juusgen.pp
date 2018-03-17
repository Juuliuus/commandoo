unit juusgen;

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
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls, Dialogs,
  Forms, Menus, clipbrd, ExtCtrls, LazFileUtils
  ;

const
  FMT_TIME_DEFAULT = 'hh:mm:ss';
  FMT_DATE_DEFAULT = 'ccccmmdd';

function StrIf( const Cond: boolean; const Msg: string; BadMsg: string = '' ): string;
function IntIf( const Cond: boolean; const TrueInt, FalseInt : integer ): integer;
function BoolToByte( const Cond: boolean ): byte;
function IntToBool( const anInt: integer ): boolean;
function DoubleQuotedString( const Str: string; DoTrim: boolean = False ): string;
//Send in delimited string, SL is filled with the items.
procedure ReturnCommaStrAsTStrings( const CommaStr: string; SL: TStrings;
                     const Delim: char; KeepDelimiter: boolean = False);
procedure FreeStringListWithObjects( SL: TStrings );
function GetMainLanguageID( Longform: string ): string;
function GetMainLanguageIdx( const LangID: string; aList: TStrings ): integer;
function DontRun( const Cond: boolean; const msg: string; RaiseExcept: boolean = False ): boolean;
procedure InternalMessage( const Msg: string );
function IsReservedPhrase( const theWord, ReservedPhrase, AMsg: string ): boolean;
function IsReservedWord( const theWord, ReservedWord, AMsg : string ): boolean;
function Snip(var SrcStr: string; const Delimiter: char; ReturnUnDelimited: boolean = False ): string;
function GetPreciseControlCoords( aControl : TControl; LeftOffset : integer = 0; TopOffset : integer = 0 ) : TPoint;
function GetValidItemIndex( const OldIndex, ItemCount : integer ) : integer;
function ConvertSizeStrToInt( const SizeStr : string ) : integer;
////Given a version gives back an integer with each section padded to 2 digits. NumSections is how many blocks
////2.9.1 => 20901
function StringVersionToInteger( StrVer : string; const NumSections : integer ) : integer;
function CheckFileNotFound( const FName : string ) : boolean;
procedure NilObjects( Strings : TStrings; DoClear : boolean = true );

const
  megaStr = ' MB';
  GigaStr = ' GB';
  TeraStr = ' TB';

var
//I need to control where showmessage shows up. Showmessage is used when there is no
//UI showing yet or possibly not showing for some reason. I want it to match where the
//program is, ie. monitor 1 or 2 or whatever. This is my solution for now
  ShowMessagePtX : integer = -1;
  ShowMessagePtY : integer = -1;

implementation

function IntIf( const Cond: boolean; const TrueInt, FalseInt : integer ): integer;
begin
  if Cond then
    Result := TrueInt
  else
    Result := FalseInt;
end;

procedure NilObjects( Strings : TStrings; DoClear : boolean = true );
var
  i : Integer;
begin
//leave original objects, just remove references
  for i := 0 to Strings.Count - 1 do
    Strings.Objects[ i ] := nil;
  if DoClear then
    Strings.Clear;
end;


function CheckFileNotFound( const FName : string ) : boolean;
begin
  result := false;
  if not fileexists( FName ) then
  begin
    Result := true;
    MessageDlg( 'File Invalid', format( '%s can not be found.', [ FName ] ), mtInformation, [mbOK], 0 );
  end;
end;

function ConvertSizeStrToInt( const SizeStr : string ) : integer;
var
  theBase : LongInt;
  TheFactor : Integer;
  Idx : SizeInt;
begin
  result := -1;

  Idx := pos( ' ', SizeStr );
  if Idx = 0 then
    exit;

  if length( SizeStr ) >= Idx + 1 then
  begin

    theBase := strtoint( copy( SizeStr, 1, Idx - 1 ) );

    case copy( SizeStr, Idx + 1, 1 ) of //expecting only one letter e.g. '500 M'
       'K','k' : TheFactor := 1000;     //maybe expand later for more complex possibilities
       'M','m' : TheFactor := 1000000;
       'G','g' : TheFactor := 1000000000;
       else exit;
    end;

    result := TheBase * TheFactor;

  end;

end;

function GetValidItemIndex( const OldIndex, ItemCount : integer ) : integer;
begin
  if ItemCount = 0 then
  begin
    result := -1;
    exit;
  end;
  result := OldIndex;
  if ( result < 0 ) and ( ItemCount > 0 ) then
    result := 0;
  if result > ItemCount - 1 then
    result := ItemCount - 1;
end;


function GetPreciseControlCoords( aControl : TControl; LeftOffset : integer = 0; TopOffset : integer = 0 ) : TPoint;
begin
  Result.x := aControl.Left + LeftOffset;
  Result.y := aControl.Top + TopOffset;
//  Pt := ScreenToClient( Pt );
  Result := AControl.Parent.ClientToScreen( Result );
end;

function Snip(var SrcStr: string; const Delimiter: char; ReturnUnDelimited: boolean = False ): string;
var
  Idx: SizeInt;
begin
  Result := '';
  Idx := pos(delimiter, SrcStr);
  if Idx > 0 then
  begin
    Result := copy(SrcStr, 1, Idx - 1);
    //wheee! I'm lazy!
    SrcStr := copy(SrcStr, Idx + 1, Maxint);
  end
  else if ReturnUnDelimited then
  begin
    Result := SrcStr;
    SrcStr := '';
  end;
end;


function IsReservedPhrase( const theWord, ReservedPhrase, AMsg: string ): boolean;
begin

  Result := False;

  if pos( ReservedPhrase, UpperCase( theWord ) ) > 0 then
  begin
    ShowMessage( Format( AMsg, [ ReservedPhrase ] ) );
    Result := True;
  end;

end;


function IsReservedWord( const theWord, ReservedWord, AMsg : string ): boolean;
begin

  Result := False;

  if UpperCase( theWord ) = ReservedWord then
  begin
    ShowMessage( Format( AMsg, [ ReservedWord ] ) );
    Result := True;
  end;

end;

procedure InternalMessage( const Msg: string );
begin
  if ( ShowMessagePtX <> -1 ) or ( ShowMessagePtY <> -1 ) then
    ShowMessagePos( Msg, ShowMessagePtX, ShowMessagePtY )
  else ShowMessage( Msg );
  ShowMessagePtX := -1;
  ShowMessagePtY := -1;
end;

function DontRun( const Cond: boolean; const msg: string; RaiseExcept: boolean = False ): boolean;
begin
  Result := Cond;
  if Result then
  begin
    if RaiseExcept then
      raise Exception.Create(msg)
    else ShowMessage(Msg);
  end;
end;


procedure FreeStringListWithObjects( SL: TStrings );
var
  i: integer;
begin

  if Assigned(SL) then
  begin
    for i := 0 to SL.Count - 1 do
    begin
      if Assigned( SL.Objects[ i ] ) then
      begin
        SL.Objects[ i ].free;
        SL.Objects[ i ] := nil;
      end;
    end;
    SL.Clear;
  end;

end;


function BoolToByte( const Cond: boolean ): byte;
begin
  if Cond then
    Result := 1
  else
    Result := 0;
end;

function IntToBool( const anInt: integer ): boolean;
begin
  Result := anInt <> 0;
end;

function DoubleQuotedString( const Str: string; DoTrim: boolean = False ): string;
begin
  Result := '"' + strif(DoTrim, trim(Str), Str) + '"';
end;

//Send in delimited string, SL is filled with the items.
procedure ReturnCommaStrAsTStrings( const CommaStr: string; SL: TStrings;
     const Delim: char; KeepDelimiter: boolean = False);
var
  BuildStr: string;
  Idx, iFrom, iLen: integer;
begin

  if not Assigned(SL) then
    Exit;

  SL.Clear;

  iFrom := 1;
  iLen := 0;
  BuildStr := Trim(CommaStr);
  for Idx := 1 to Length(BuildStr) do
  begin
    Inc(iLen);
    if (BuildStr[Idx] = Delim) then
    begin
      SL.Add(Trim(Copy(BuildStr, iFrom, IntIf(KeepDelimiter, Ilen, iLen - 1))));
      iFrom := IntIf(KeepDelimiter, Idx, Idx + 1);
      //       SL.Add( Trim( Copy( BuildStr, iFrom, iLen - 1 ) ) );
      //       iFrom := Idx + 1;
      iLen := 0;
    end;
    if (Idx = Length(BuildStr)) then
    begin
      SL.Add(Trim(Copy(BuildStr, iFrom, IntIf(KeepDelimiter, Ilen + 1, iLen))));
      iFrom := IntIf(KeepDelimiter, Idx, Idx + 1);
      //       SL.Add( Trim( Copy( BuildStr, iFrom, iLen ) ) );
      //       iFrom := Idx + 1;
      iLen := 0;
    end;
  end;
end;


function StrIf( const Cond: boolean; const Msg: string; BadMsg: string = '' ): string;
begin
  Result := '';
  if Cond then
    Result := Msg
  else
    Result := BadMsg;
end;


function GetMainLanguageID( Longform: string ): string;
var
  Idx: SizeInt;
begin
  Result := 'en';
  Idx := pos('(', Longform);
  if Idx = 0 then
    exit;
  Longform := copy(Longform, Idx + 1, length(Longform));
  Idx := pos(')', Longform);
  if Idx = 0 then
    exit;
  Result := copy(Longform, 1, Idx - 1);

end;

function GetMainLanguageIdx( const LangID: string; aList: TStrings ): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to aList.Count - 1 do
  begin
    if pos('(' + LangID + ')', aList[i]) > 0 then
    begin
      Result := i;
      break;
    end;
  end;
  if Result = -1 then
    Result := 0;
end;


//Given a version gives back an integer with each section padded to 2 digits. NumSections is how many blocks
//12.6.11 => 120611
function StringVersionToInteger( StrVer : string; const NumSections : integer ) : integer;
var
  ConvertedStr , NumberStr: String;
  Idx : SizeInt;
  i : Integer;

  procedure PadNumberStr( ToPad : string );
  begin
    if length( ToPad ) = 1 then
      ToPad := '0' + ToPad;
    ConvertedStr := ConvertedStr + ToPad;
  end;

  procedure GetNumberStr;
  begin
    Idx := pos( '.', StrVer );
    if Idx > 0 then
    begin
      NumberStr := copy( StrVer, 1, Idx - 1 );
      StrVer := copy( StrVer, Idx + 1, length( StrVer ) );
      PadNumberStr( NumberStr );
    end
    else PadNumberStr( StrVer );

  end;

begin
  //StrVer format is #.#.# up to (in far, far future) ##.##.## , want two digits for each position for all these formats
  ConvertedStr := '0';
  for i := 1 to NumSections do
    GetNumberStr;
  result := strtoint( ConvertedStr );
end;

//TListSortCompare = function (Item1, Item2: Pointer): Integer;
//
//List.Sort(@CompareByPositionPtr);
//
//function CompareByPosition(A, B: TMyObject): Integer; inline;
//begin
//  if A.Position < B.Position then
//    Result := -1
//  else if A.Position > B.Position then
//    Result := 1
//  else
//    Result := 0;
//end;
//
//function CompareByPositionPtr(A, B: Pointer): Integer;
//begin
//  Result := CompareByPosition(TMyObject(A), TMyObject(B));
//end;

Procedure QuickSort(FList: PPointerList; L, R : Longint;
                     Compare: TListSortCompare);
var
  I, J : Longint;
  P, Q : Pointer;
begin
//stolen from lazarus code, need to re-work this for strings...someday
 repeat
   I := L;
   J := R;
   P := FList^[ (L + R) div 2 ];
   repeat
     while Compare(P, FList^[i]) > 0 do
       I := I + 1;
     while Compare(P, FList^[J]) < 0 do
       J := J - 1;
     If I <= J then
     begin
       Q := FList^[I];
       Flist^[I] := FList^[J];
       FList^[J] := Q;
       I := I + 1;
       J := J - 1;
     end;
   until I > J;
   // sort the smaller range recursively
   // sort the bigger range via the loop
   // Reasons: memory usage is O(log(n)) instead of O(n) and loop is faster than recursion
   if J - L < R - I then
   begin
     if L < J then
       QuickSort(FList, L, J, Compare);
     L := I;
   end
   else
   begin
     if I < R then
       QuickSort(FList, I, R, Compare);
     R := J;
   end;
 until L >= R;
end;


end.
{
was written specifically to read rsync sizes but has maybe some usefulness in a more generic way.
function GetJobSize( SizeStr : string ) : real;
var
  idx : SizeInt;
  factor : real;
begin
  factor := 0.000001;
  idx := pos( ':', SizeStr );
  SizeStr := Trim( copy( SizeStr, idx + 1, length( SizeStr ) ) );
  idx := pos( ' ', SizeStr );
  SizeStr := Uppercase( copy( SizeStr, 1, idx - 1 ) );
  if pos( 'K', SizeStr ) > 0 then
  begin
    SizeStr := stringreplace( SizeStr, 'K', '', [ ] );
    Factor := 0.001;
  end;
  if pos( 'M', SizeStr ) > 0 then
  begin
    SizeStr := stringreplace( SizeStr, 'M', '', [ ] );
    Factor := 1;
  end;
  if pos( 'G', SizeStr ) > 0 then
  begin
    SizeStr := stringreplace( SizeStr, 'G', '', [ ] );
    Factor := 1000;
  end;
  if pos( 'T', SizeStr ) > 0 then
  begin
    SizeStr := stringreplace( SizeStr, 'T', '', [ ] );
    Factor := 1000000;
  end;
  result := strtofloat( trim( SizeStr ) ) * factor;
end;


}
