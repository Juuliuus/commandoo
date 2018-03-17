unit unitDBUtils;

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
  , JIniFiles
  , sqlDB
  , unitDBStructure
  ;

const
  csoNonPrintingDelimiter = #27;//'|';


//Update level specific procs
  procedure Update_DB_Version_0001( const Key : string );
  procedure Update_DB_Version_0002( const Key : string );
  procedure Update_DB_Version_0003( const Key : string );
  procedure Update_DB_Version_0004( const Key : string );
  procedure Update_PROG_Version_0001( IFile : TJiniFile; const FormSettEntry : string );
  procedure Update_PROG_Version_0002( IFile : TJiniFile );

//Text
  procedure UpdateUpgradeLevel( IFile : TJinifile; const aSection, aKey : string;
                                   const aValue : integer; const DoSave : boolean = true );
  function GetUpgradeLevel( IFile : TJinifile; const aSection : string ) : integer;
  function GetProfileGUID( IFile : TJinifile; const aSection : string ) : string;
  procedure SetProfileGUID( IFile : TJinifile; const aSection, aKey, aValue : string );


//Generic
  function DBMismatch( const intVerData, IntVerProg : integer; ShowMsg : boolean = true ) : boolean;
  function GenerateDBFilePath( const aPath, aFileName, anExtension : string ) : string;
  function GetIniDBNameTemplate( const DL : TDataLocation ) : string;
  function GetIniDBFileFromToList( Strings : TStrings; const OldPath, NewPath, OldProfileName, NewProfileName : string;
                                   SeparatorChar : string = csoNonPrintingDelimiter ) : boolean;
  function IniDBFilesExist( const Currpath, CurrName : string; MustBeCorrect : boolean = true ) : boolean;
  function GetIniDBFiles( Strings : TStrings; const Currpath, CurrName : string; MustBeCorrect : boolean = true ) : integer;
  function PathDisplayToPathValue( const Path : string ) : string; overload;
  function PathDisplayToPathValue( const Path, DefaultPath : string ) : string; overload;
  function PathValueToPathDisplay( const Path : string ) : string;
  function GetDBNameList( const ProfileName : string; Path : string = ''; IsTextDB : boolean = true ) : string;
  function ExtractProfileName( const ProfileEntry : string ) : string;

  procedure GetRawProfileList( IFile : TJinifile; Strings : TStrings );


  function GetSelect_Sql( const ColNames, TableName : string; Where : string = '' ) : string;
  function GetDelete_Sql( const TableName, Where : string ) : string;
  function GetInsert_Sql( const TableName, FldStr, ValStr : string ) : string;
  function GetUpdate_Sql( const TableName, ColValPairs, Where : string ) : string;
  Procedure BuildInsertStrings( var TheFlds, TheVals : string; sFld, sVal : TStrings );
  function BuildUpdateStrings( sFld, sVal : TStrings ) : string;
  function NumberOfInifiles : integer;

resourcestring
  cmsgDBUtilOldProgNewDB = 'Program expects DB UpgradeCount "%d". The loading DB UpgradeCount is "%d". '
                            +'The Database is from a newer version of the program. Loading is disabled as a safety measure. '
                            + 'Use a newer version of the program, or load an older database. ';
  cmsgDBUtilWrongNumIni = 'path "%s" with profile name "%s" does not have correct number (%d) of text datafiles';


const
  cKey_VersionCount = 'LastVersionUpgrade';
  cKey_ProfileGuid = 'GUID';
  cSqlDBExtension = '.sqlite';
  cIniDBExtension = '.data';
  constDefaultPathValue = 'DefPath';

resourcestring
  constDefaultPathDisplay = 'Default';


implementation

uses juusgen
     , unitsharedobj
     , strconst_prog
     , unitGlob
     ;

const
  cBadBuildStrings = '%s: BadBuildStrings!';


function GetSelect_Sql( const ColNames, TableName : string; Where : string = '' ) : string;
begin
//sqlite has this thing: select * DOES NOT select rowid. Then query values for rowid don't exist. Bummer.
//put it here and simplify all SELECT calls
  result := format( 'SELECT %s, %s from %s ', [ csoSqlite_rowid, Colnames, TableName ] ) + strif( Where <> '', 'Where ' + Where );
end;

function GetDelete_Sql( const TableName, Where : string ) : string;
begin
  result := format( 'DELETE FROM %s WHERE %s', [ TableName, Where ] );
end;

function GetInsert_Sql( const TableName, FldStr, ValStr : string ) : string;
begin
  result := format( 'INSERT INTO %s ( %s ) VALUES ( %s )', [ TableName, FldStr, ValStr ] );
end;

function GetUpdate_Sql( const TableName, ColValPairs, Where : string ) : string;
begin
  result := format( 'UPDATE %s SET %s WHERE %s',
                    [ TableName,
                      ColValPairs,
                      Where
                    ]
                  );
end;

function BadBuildStrings( sFld, sVal : TStrings ) : boolean;
begin

  result := true;

  if not assigned( sFld ) or not assigned( sVal ) then
    exit;

  if ( sFld.Count <> sVal.Count ) or ( sFld.Count = 0 ) then
    exit;

  result := false;

end;

Procedure BuildInsertStrings( var TheFlds, TheVals : string; sFld, sVal : TStrings );
var
  i : Integer;
begin

  if BadBuildStrings( sFld, sVal ) then
    raise EErrorDatabaseProblem.create( format( cBadBuildStrings, [ 'BuildInsertStrings' ] ) );

  for i := 0 to sFld.Count - 2 do
  begin
    TheFlds := TheFlds + sFld[ i ] + ', ';
    TheVals := TheVals + sVal[ i ] + ', ';
  end;

  TheFlds := TheFlds + sFld[ sFld.Count - 1 ];
  TheVals := TheVals + sVal[ sFld.Count - 1 ];

end;

function NumberOfInifiles : integer;
begin
  result := High( aryTextData );
end;

function BuildUpdateStrings( sFld, sVal : TStrings ) : string;
var
  i : Integer;
begin

  result := '';

  if BadBuildStrings( sFld, sVal ) then
    raise EErrorDatabaseProblem.create( format( cBadBuildStrings, [ 'BuildUpdateStrings' ] ) );

  for i := 0 to sFld.Count - 2 do
  begin
    result := result + sFld[ i ] + '=' + sVal[ i ] + ', ';
  end;

  result := result + sFld[ sFld.Count - 1 ] + '=' + sVal[ sFld.Count - 1 ];

end;

function DBMismatch( const intVerData, IntVerProg : integer; ShowMsg : boolean = true ) : boolean;
begin
  result := false;
  if intVerData > intVerProg  then //older program using Newer DBase, or whatever other comparision in future
  begin
    result := true;
    if ShowMsg then
      InternalMessage( format( cmsgDBUtilOldProgNewDB, [ IntVerProg, intVerData ] ) );
  end;
end;

function GetProfileGUID( IFile : TJinifile; const aSection : string ) : string;
begin
  result := IFile.ReadString( aSection, cKey_ProfileGuid, '' );
end;

procedure SetProfileGUID( IFile : TJinifile; const aSection, aKey, aValue : string );
begin
  IFile.WriteString( aSection, aKey, aValue );
  IFIle.UpdateFile;
end;

function GetUpgradeLevel( IFile : TJinifile; const aSection : string ) : integer;
begin
  result := IFile.ReadInteger( aSection, cKey_VersionCount, 0 );
end;

procedure UpdateUpgradeLevel( IFile : TJinifile; const aSection, aKey : string;
                                 const aValue : integer; const DoSave : boolean = true );
begin
  IFile.WriteInteger( aSection, aKey, aValue );
  if DoSave then
    IFIle.UpdateFile;
end;

procedure Update_DB_Version_0001( const Key : string );
begin
//got rid of LineIniIdx field in Cmd file
  //no need to check useDB, this change was before sqlite
  //in case db struct changes later the ini section needs to be what it was at that time: '-LoadMe_List_CmdObj-'
  InfoServer.RemoveField_File( Key, dlCmd, '-LoadMe_List_CmdObj-' );
end;

procedure Update_DB_Version_0002( const Key : string );
begin
//found I had released data with quotes around <<F>> variable ( '<<F>>' ). Wrong. Fix just in case.
//no need to check useDB, this change was before sqlite
//chg was in CmdLine
  InfoServer.DBUpgrade_Cleanup0002( Key );
end;

procedure Update_DB_Version_0003( const Key : string );
begin
//tlDefaultInvalid, 0th element was in TThreatLevel. It was never used, and, never a good idea.
//getting rid of it makes all the Threatlevel routines more intuitive.
//this needs to be done for both Cmd and CmdLine
  InfoServer.DBUpgrade_Cleanup0003( dlCmd, Key );
  InfoServer.DBUpgrade_Cleanup0003( dlCmdLine, Key );
end;

procedure Update_DB_Version_0004( const Key : string );
begin
//Old field 'ObjID' removed during sql / text db integration
//this needs to be done for both Cmd and CmdLine, but only ini files
  InfoServer.RemoveField_File( Key, dlCmd, '-LoadMe_List_CmdObj-' );
  InfoServer.RemoveField_File( Key, dlCmdLine );
end;

procedure Update_PROG_Version_0001( IFile : TJiniFile; const FormSettEntry : string );
begin
//clean up of some junk that happened as Upgrade system was being developed
  IFile.EraseSection( '_DB_Version' );
  IFile.EraseSection( '_Program_Version' );
  IFile.DeleteKey( FormSettings.GetDataSection, FormSettEntry );

end;

procedure Update_PROG_Version_0002( IFile : TJiniFile );
begin
//When changes are made to min form sizes =AND= the user has resized the forms later to their liking, then
//The user can get wacky looking forms. I only noticed this now and so made a PROG update for it, but this is
//likely to be true relatively often? So maybe this should later just be a general function called
//for EVERY NEW VERSION update?
  FormSettings.ClearAllFormSettings( IFile );
  //IFile.EraseSection( FormSettings.GetDataSection );
end;

function GenerateDBFilePath( const aPath, aFileName, anExtension : string ) : string;
begin
  result := IncludeTrailingPathDelimiter( aPath ) + aFileName + anExtension;
end;


function GetIniDBNameTemplate( const DL : TDataLocation ) : string;
begin
  result := '%s' + GetTableNames( DL, false );
end;

function GetIniDBFileFromToList( Strings : TStrings; const OldPath, NewPath, OldProfileName, NewProfileName : string;
                                 SeparatorChar : string = csoNonPrintingDelimiter ) : boolean;
var
  i : Integer;
  FromF , ToF: String;
begin
  result := true;
  Strings.Clear;

  for i := 0 to High( aryTextData ) do
  begin
    FromF := GenerateDBFilePath( OldPath, format( GetIniDBNameTemplate( aryTextData[ i ] ), [ OldProfileName ] ), cIniDBExtension );
    if not FileExists( FromF ) then
    begin
      result := false;
      exit;
    end;
    ToF := GenerateDBFilePath( NewPath, format( GetIniDBNameTemplate( aryTextData[ i ] ), [ NewProfileName ] ), cIniDBExtension );
    Strings.Add( FromF + SeparatorChar + ToF );
  end;
end;

function IniDBFilesExist( const Currpath, CurrName : string; MustBeCorrect : boolean = true ) : boolean;
var
  i , Count, aryCount: Integer;
  Check : String;
  Exists : Boolean;
begin
  result := true;
  Count := 0;
  aryCount := High( aryTextData );

  for i := 0 to aryCount do
  begin
    Check := GenerateDBFilePath( CurrPath, format( GetIniDBNameTemplate( aryTextData[ i ] ), [ CurrName ] ), cIniDBExtension );
    Exists := FileExists( Check );
    if Exists then
      inc( Count );
    result := result and Exists;
  end;

  if MustBeCorrect and result and ( Count < aryCount ) then
  begin
    result := false;
    raise EErrorDatabaseProblem.create( 'IniDBFilesExist: ' + format( cmsgDBUtilWrongNumIni, [ CurrPath, CurrName, Count ] ) );
  end;

end;

function GetIniDBFiles( Strings : TStrings; const Currpath, CurrName : string; MustBeCorrect : boolean = true ) : integer;
var
  i , Count: Integer;
  Check : String;
begin
  result := 0;
  Strings.Clear;

  Count := High( aryTextData );

  for i := 0 to Count  do
  begin
    Check := GenerateDBFilePath( CurrPath, format( GetIniDBNameTemplate( aryTextData[ i ] ), [ CurrName ] ), cIniDBExtension );
    if FileExists( Check ) then
      Strings.Add( Check );
  end;

  result := Strings.Count;

  if MustBeCorrect and ( Strings.Count < Count ) then
    raise EErrorDatabaseProblem.create( 'GetIniDBFiles: ' + format( cmsgDBUtilWrongNumIni, [ CurrPath, CurrName, Result ] ) );

end;

function GetDBNameList( const ProfileName : string; Path : string = ''; IsTextDB : boolean = true ) : string;
var
  i , Count: Integer;
begin
  Result := '';
  if not IsTextDB then
  begin
    Result := Path + ProfileName + cSqlDBExtension;
    exit;
  end;

  Count := High( aryTextData );

  for i := 0 to Count - 1 do
  begin
    Result := Result
              + Path
              + format( GetIniDBNameTemplate( aryTextData[ i ] ), [ ProfileName ] )
              + cIniDBExtension
              + LineEnding;
  end;

  Result := Result
            + Path
            + format( GetIniDBNameTemplate( aryTextData[ Count ] ), [ ProfileName ] )
            + cIniDBExtension;

end;

function ExtractProfileName( const ProfileEntry : string ) : string;
begin
  result := stringreplace( ProfileEntry, cDefaultDBProfileIsDBStrNot, '', [ rfReplaceAll, rfIgnoreCase ] );
  result := stringreplace( result, cDefaultDBProfileIsDBStr, '', [ rfReplaceAll, rfIgnoreCase ] );
end;

function PathDisplayToPathValue( const Path : string ) : string;
begin
  result := strif( Path = constDefaultPathDisplay, constDefaultPathValue, Path );
end;

function PathDisplayToPathValue( const Path, DefaultPath : string ) : string; overload;
begin
  result := strif( ( Path = DefaultPath ) or ( Path = constDefaultPathDisplay ),
                     constDefaultPathValue, Path );
end;


function PathValueToPathDisplay( const Path : string ) : string;
begin
  result := strif( Path = constDefaultPathValue, constDefaultPathDisplay, Path )
end;

procedure GetRawProfileList( IFile : TJinifile; Strings : TStrings );
begin
  Ifile.ReadSection( cSectTabDBProfiles, Strings );
end;

end.

