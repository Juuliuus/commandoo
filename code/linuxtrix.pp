unit linuxtrix;

{$mode objfpc}{$H+}
{
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
  Classes, SysUtils, process, AsyncProcess, Dialogs;//, StdCtrls;

function SystemFileLocation( const FName : string ) : string;
function SystemFileFound( const FName : string; ShowError : boolean = false ) : boolean;
function DeterminePathSeparatorUsed( const PathEnvirString : string ) : char;
function GetLinuxEnvPathString( var PathEnvirString : string; AddEndingLinuxSlash : boolean = false  ) : char;
//procedure GetMountedDrives( Strings : TStrings );
function GetRawGuidString( RaiseExcept : boolean = false ) : string;

function IsLinuxBuiltin( const FName : string ) : boolean;
procedure FillProcDefaults( aProc : TProcess; doPipeStdErr : boolean = false );
procedure HandleProcParams( var aProc : TProcess; var Params : Tstrings );


function GetBuiltInOutput( const Command : string; Params : Tstrings ) : string; // overload;
function BuiltInNotAllowedInShell( const CmdName : string; var ResultStr : string ) : boolean;

//=========do not change!!!! Central to the program for quick finds of environment data
function QuickProc( const theExec, theParams : string; ThirdParam : string = '' ) : string;
//==================================================
function SimpleProc( const CmdLine : string; const DoStdOut, DoStdErr, DoFormat : boolean ) : string;
function RunThroughShell( const aString : string ) : string;


//does this work?
//OutputLines.LoadFromStream(hprocess.Output);

//reference if sudo ever used, which it will not be...not secure
//hProcess.Executable := '/bin/sh';
//// Now we add all the parameters on the command line:
//hprocess.Parameters.Add('-c');
//// Here we pipe the password to the sudo command which then executes fdisk -l:
//hprocess.Parameters.add('echo ' + sPass  + ' | sudo -S fdisk -l');

function ProcString( Params : TStrings ) : string;
//function ProcInteger( ParamsAndResult : TStrings; SudoPW : string = '' ) : integer;
function ProcInteger( ParamsAndResult : TStrings ) : integer;
function ProcDetached( var aProc : TProcess; const fullCommand : string ) : string; overload;
function ProcDetached( aProc : TAsyncProcess; Params : Tstrings ) : string; overload;
//procedure ReleaseProcess( var aProc : TProcess; ECode : integer = 999 );
function GetProcessOutputToStream( aProc : TProcess; const IsOutput : boolean;
                           var MemS : TMemoryStream; DoExecute : boolean = true ) : integer;
function GetProcessOutput( aProc : TProcess; var TheInput : string;
                           const DoStdOut, DoStdErr : boolean; DoFormat : boolean = true ) : string;

procedure StringfromMemStream( var Str : string; MS : TMemoryStream );
procedure WriteStringToMemoryStream( SourceString : string; MemoryStream : TMemorystream );
procedure WriteMemoryStreamToString( var SourceString : string; MemoryStream : TMemorystream );
//function ParsePiping( PipeStr : string; Strings : TStrings ) : integer;


resourcestring
  cltmsgWhich = '`which` command could not find system file `%s`';
  cltMissingBash = 'Cannot retrieve output from builtin "%s" on a system that does not support bash';
  cltDetachedProcess = 'running as detached process';
  cltProcessExitCode = 'Process Exit Code: %d';
  cltProcessExitStatus = 'System Status Code: %d';
  cltProcessInput = 'Input';
  cltProcessOutput = 'Output';
  cltNoOutput = '< no output >';
  cltNoError = '< no error >';

  cltProcessResultUndefined = '< Process failed to run properly >';
  cltProcessResultCanceled = '< process canceled >';
  cltProcessResultTooMuch = '< %s exceeded internal maximum of %d characters, use a terminal or change maximium >';
  cltProcessResultTimeOut = '< timeout waiting for process output >';
  cltProcessResultUnknown = '< process finished with unknown code: %d >';
  cltProcessRefusesInput = '< The Process would not allow input. >';
  cltProcessNoneStr = '< none >';
  cltProcessThroughShell = '==RUNNING THROUGH SHELL==';
  cltProcess4thDimension = 'Due to possibility of opening a 4th dimensional rift it is not allowed to run "%s"';
  cltProcessNoParams = 'No Params';


var
  globltHasBASH : boolean = false;
  globltDoCancelProcess : boolean;
  globltProcessMaxOutput : integer = 500000;
  //globltOutputMemo : TMemo;

  globltInputForProcess : string = '';

const
  cProcessResultUndefined = -2;
  cProcessResultCanceled = -3;
  cProcessResultTooMuch = -4;
  cProcessResultTimeOut = -5;
  cProcessResultOK = 0;
  cLinuxPipeFlag = ' | ';
  cLimitInfinityMaxCnt = 50000;
  cltBadGuid = '{}';
  cltProcessStdErrStr = 'stderr';
  cltProcessStdOutStr = 'stdout';


implementation

uses juusgen
     , Forms
     , pipes
     , unitGlob
     ;

var
  dummystring : string = ''; //special use don't change

const
  CheckAdjust = 17;

function InterpretExitCodes( aProc : TProcess ) : string;
begin
//learned that exitstatus is only meaningful when process is terminated and reflects exitcode
//only when the process was not interrupted by the system.
  result := '________________'
            + LineEnding
            + format( cltProcessExitCode, [ aProc.ExitCode ])
            + LineEnding
            + format( cltProcessExitStatus, [ aProc.ExitStatus ])
            + LineEnding
            ;
end;


function GetRawGuidString( RaiseExcept : boolean = false ) : string;
var
  Idx : Integer;
  GStr : TGuid;
begin
  result := '';
  Idx := CreateGUID( GStr );
  if Idx <> 0 then
  begin
    if RaiseExcept then
      raise EErrorSystem.Create( 'GUID Generation failure in GetRawGuidString' )
    else result := cltBadGuid;
  end else result := GuidToString( GStr );
end;

function GetLinuxEnvPathString( var PathEnvirString : string; AddEndingLinuxSlash : boolean = false ) : char;
begin
//todo are env var's  in locale?? Or always english??
  PathEnvirString := GetEnvironmentVariable( 'PATH' );
  result := DeterminePathSeparatorUsed( PathEnvirString );
  if AddEndingLinuxSlash then
    PathEnvirString := stringreplace( PathEnvirString, result, '/'+ result, [rfReplaceAll] );
  PathEnvirString := result + PathEnvirString + result;
end;

function DeterminePathSeparatorUsed( const PathEnvirString : string ) : char;
const
  PossibleLinuxEnvPathSeperators = ':;,-. ?';
begin
  for result in PossibleLinuxEnvPathSeperators do
    if pos( result, PathEnvirString ) > 0 then
      break;
end;


procedure FillProcDefaults( aProc : TProcess; doPipeStdErr : boolean = false );
var
  I : Integer;
begin
  if not assigned( aProc ) then
    exit;

//According to inet help: On Unix, setting this variable has no effect.
  aProc.InheritHandles := False;//setting it anyway!

// Copy default environment variables including DISPLAY variable for GUI application to work
  for I := 0 to GetEnvironmentVariableCount - 1 do
    aProc.Environment.Add( GetEnvironmentString( I ) );

  aProc.Options := [ poUsePipes ];
  if doPipeStdErr then
    aProc.Options := aProc.Options + [ poStderrToOutPut ]

end;

procedure FillAsyncProcDefaults( aProc : TAsyncProcess );
var
  I : Integer;
begin
  if not assigned( aProc ) then
    exit;

//According to inet help: On Unix, setting this variable has no effect.
  aProc.InheritHandles := False;//setting it anyway!

// Copy default environment variables including DISPLAY variable for GUI application to work
  for I := 0 to GetEnvironmentVariableCount - 1 do
    aProc.Environment.Add( GetEnvironmentString( I ) );

//for Detached Processes turns out DON'T USE PIPES!!!!!!
//While everything appeared to be ok I finally had a problem with Blender
//where it would render a video out to only frame 1227 and then hang.
//Turn off pipes, use asyncProc intead (though now I'm not sure that Process wouuldn't also work as it did
//before as long as pipes were off) and Blender can render all day long.

  //NO NO NO !!! aProc.Options := [ poUsePipes ];
  //NO NO NO !!! if doPipeStdErr then
  //NO NO NO !!!   aProc.Options := aProc.Options + [ poStderrToOutPut ]

end;


function DoProcessInputSpecial( aProc : TProcess; var TheInput : string ) : string;
var
  byteWritten : Integer;
  WriteCnt : Cardinal;
  MemIn : TMemorystream;
begin
//this is a special input for process used only for the ability to "inject" the initial/only command
//with desired typed in info/filename, based on global globltInputForProcess var

  result := '';

  if not assigned( aProc ) then
    raise EErrorDevelopment.create( 'DoProcessInput Special: aProc not assigned' );

  if not assigned( aProc.Input ) then
  begin
    result := LineEnding + cltProcessRefusesInput + LineEnding;
    exit;
  end;

  try

    if TheInput = '' then
      exit;

    byteWritten := 0;
    WriteCnt := aProc.PipeBufferSize;//100;

    MemIn := TMemoryStream.Create;

    try

      WriteStringToMemoryStream( TheInput, MemIn );

      while aProc.Running do
      begin

//        Application.HandleMessage; //giving an access violation at full speed. Don't know what up.
        //if cancel flags then
        //  break;

        if byteWritten < MemIn.Size then
        begin

          if MemIn.Size - byteWritten < WriteCnt then
            WriteCnt := MemIn.Size - byteWritten;

          aProc.Input.Write( ( MemIn.Memory + byteWritten )^, WriteCnt );
          inc( byteWritten, WriteCnt );

          //safety limit, can be set externally
          if byteWritten > globltProcessMaxOutput then
          begin
            result := LineEnding + format( cltProcessResultTooMuch, [ cltProcessInput, globltProcessMaxOutput ] ) + LineEnding ;
            break;
          end;

        end
        else break;

      end;//while

    finally
      MemIn.Free;
    end;

  finally
    //Process that are to be only output must be ensured to have the input closed
    //like cksum w/o params, waits, FOREVER, for input
    //now it is guaranteed to be closed
    aProc.CloseInput;
    TheInput := '';
  end;

end;

function GetFullProcessInfo( aProc : TProcess; var MemErr : TMemoryStream; PipeInfo : string = '' ) : string;
var
  i : Integer;
  ErrStr : string;
begin
  ErrStr := '';
  result := aProc.Executable;
  for i := 0 to aProc.Parameters.Count - 1 do
    result := result + ' ' + aProc.Parameters[ i ];

  StringfromMemStream( ErrStr, MemErr );
  MemErr.Clear;
  if ErrStr = '' then
    ErrStr := format( '%s: %s', [ cltProcessStdErrStr, cltProcessNoneStr ] )
  else ErrStr := format( '%s: %s', [ cltProcessStdErrStr, ErrStr ] );

  result := result
            + strif( PipeInfo <> '', PipeInfo + LineEnding, LineEnding )
            + InterpretExitCodes( aProc )
            + LineEnding
            + ErrStr
            + LineEnding + LineEnding + LineEnding;

end;



function GetProcessOutputToStream( aProc : TProcess; const IsOutput : boolean;
                           var MemS : TMemoryStream; DoExecute : boolean = true ) : integer;
var
  NumByt: LongInt;
  BytRead: LongInt;
  SStream : TStringStream;
  theOutput : TInputPipeStream;
  EmergencyCordCnt , CheckCancel: Integer;
begin

  result := cProcessResultUndefined;

  if not assigned( aProc ) then
    raise EErrorDevelopment.create( 'GetProcessOutputToStream: aProc not assigned' );

  globltDoCancelProcess := false;

  if DoExecute then
    aProc.Execute;

  if IsOutput then
    theOutput := aProc.Output
  else theOutput := aProc.Stderr;

  if not assigned( theOutput ) then
    exit;

  EmergencyCordCnt := 0;
  SStream := TStringStream.Create( '' );
  try

    result := cProcessResultOK;
    BytRead := 0;
    CheckCancel := 0;

    while aProc.Active or ( theOutput.NumBytesAvailable > 0 ) do
    begin

      inc( CheckCancel );
      if CheckCancel mod CheckAdjust = 0 then
      begin
        Application.Handlemessage;
        if globltDoCancelProcess then
        begin
          result := cProcessResultCanceled;
          break;
        end;
      end;

      if theOutput.NumBytesAvailable = 0 then
      begin
        sleep( 10 );//===>> this turned out to be super important
 //temporary (?) emergency cord: just in case for now
        inc( EmergencyCordCnt );
        if EmergencyCordCnt > 200 then
        begin
          result := cProcessResultTimeOut;
          break;
        end;
        continue;
      end;

      // make sure we have room
      MemS.SetSize( BytRead + theOutput.NumBytesAvailable );

      // try reading it
      NumByt := theOutput.Read( (MemS.Memory + BytRead)^, theOutput.NumBytesAvailable );
      if NumByt > 0 then// All read() calls will block, except the final one.
      begin

        //if assigned( globltOutputMemo ) then
        //begin
        //was designed for "live" updates, found it to be a pain, maybe later? For reference
        //
        //  SStream.size := NumByt;//0;
        //  SStream.WriteBuffer( ( MemS.Memory + BytRead )^, NumByt );
        //  globltOutputMemo.SetFocus;
        //  globltOutputMemo.Lines.Text := globltOutputMemo.Lines.Text + SStream.DataString;
        //  globltOutputMemo.SelStart := length( globltOutputMemo.Lines.Text );
        //
        //end;

        Inc( BytRead, NumByt );

      end;

//safety limit, can be set externally
      if MemS.Size > globltProcessMaxOutput then
      begin
        result := cProcessResultTooMuch;
        break;
      end;

    end; //while loop

    MemS.SetSize(BytRead);

  finally
    SStream.Free;
  end;

end;



function GetProcessOutput( aProc : TProcess; var TheInput : string;
                           const DoStdOut, DoStdErr : boolean; DoFormat : boolean = true ) : string;
var
  MemS: TMemoryStream;
  ProcStatus : Integer;
  InfoString, ErrStr, OutStr: String;

  function GetProcStatus : string;
  var
    aMsg : string;
  begin
    result := '';
    if ProcStatus < 0 then
    begin
      case ProcStatus of
        cProcessResultUndefined : aMsg := cltProcessResultUndefined;
        cProcessResultCanceled : aMsg := cltProcessResultCanceled;
        cProcessResultTooMuch : aMsg := format( cltProcessResultTooMuch, [ cltProcessOutput, globltProcessMaxOutput ] );
        cProcessResultTimeOut : aMsg := cltProcessResultTimeOut;
        else aMsg := format( cltProcessResultUnknown, [ ProcStatus ] );
      end;
      result := result + LineEnding + LineEnding + aMsg + LineEnding;
    end;

  end;

  function DoProcessIt( IsOutput : boolean ) : string;
  begin
    result := '';
    MemS.Clear;
    ProcStatus := GetProcessOutputToStream( aProc, IsOutput, MemS, false );

    StringfromMemStream( result, MemS );

    if DoFormat then
    begin
      result := LineEnding + strif( IsOutput, cltProcessStdOutStr, cltProcessStdErrStr ) + ':'
                + LineEnding + '-----' + LineEnding + LineEnding
                + strif( result <> '', result, strif( IsOutput, cltNoOutput, cltNoError ) )
                + LineEnding + getProcStatus;
    end;
  end;

begin

  result := '';

  if not assigned( aProc ) then
    raise EErrorDevelopment.create( 'GetProcessOutput: aProc not assigned' );

  OutStr := '';
  ErrStr := '';

  MemS := TMemoryStream.Create;
  try

    aProc.Execute;

    InfoString := DoProcessInputSpecial( aProc, TheInput );

    if DoStdOut then
    begin
      OutStr := DoProcessIt( true );
      OutStr := OutStr + InfoString;//add any info from INPUT routing if any
    end;

    if DoStdErr and ( ProcStatus <> cProcessResultCanceled ) then
      ErrStr := DoProcessIt( false );

    result := OutStr + ErrStr;

  finally
    if assigned( MemS ) then
      MemS.Free;
  end;

end;



procedure HandleProcParams( var aProc : TProcess; var Params : Tstrings );
begin
  aProc.Executable := Params[ 0 ];
  Params.Delete( 0 );
  aProc.Parameters.Assign( Params );
end;

procedure HandleAsyncProcParams( var aProc : TAsyncProcess; var Params : Tstrings );
begin
  aProc.Executable := Params[ 0 ];
  Params.Delete( 0 );
  aProc.Parameters.Assign( Params );
end;

function ProcDetached( var aProc : TProcess; const fullCommand : string ) : string;
var
  SL : TStringList;
begin

  result := '';

  if not assigned( aProc ) then
    raise EErrorDevelopment.create( 'ProcDetached: aProc not assigned' );

  FillProcDefaults( aProc );

  SL := TStringList.Create;
  try
    CommandToList( fullCommand,  SL );
    HandleProcParams( aProc, TStrings( SL ) );
  finally
    SL.free;
  end;

  aProc.ShowWindow := swoShow;

  aProc.Execute;

  DoProcessInputSpecial( aProc, globltInputForProcess );

end;



function ProcDetached( aProc : TAsyncProcess; Params : Tstrings ) : string;
begin

  result := '';

  if not assigned( aProc ) then
    raise EErrorDevelopment.create( 'ProcDetached: aProc not assigned' );

  FillAsyncProcDefaults( aProc );

  HandleAsyncProcParams( aProc, Params );

  aProc.Execute;

//this combination now caught and disallowed.
//since it was discovered that using pipes with Detached Processes had odd problems and pipes
//have now been turned off, then it is not possible to send input into a detached process
  //DoProcessInputSpecial( aProc, globltInputForProcess );

end;

//procedure ReleaseProcess( var aProc : TProcess; ECode : integer = 999 );
//begin
//
//  if assigned( aProc ) then
//  begin
////terminate blows up if it's not running, what can I trust? Running? Active?
////    if aProc.running then  //this does work for detached processes so may be useful in future
//    aProc.Terminate( ECode );
//    freeandnil( aProc );
//  end;
//
//end;


function RunThroughShell( const aString : string ) : string;
var
  tmpResult: String;
  SL : TStringList;
begin

  result := '';

  SL := TStringList.Create;
  try

    if not globltHasBASH then
      SL.Add( 'sh' )
    else SL.Add( 'bash' ); //OUCH!!! This fixed a SERIOUS problem where subsequent runs gave SIGTTIN errors
    SL.Add( '-c' );
    SL.add( aString );

    tmpResult := ProcString( SL );

    result := cltProcessThroughShell
              + LineEnding
              + 'Command := ' + aString + LineEnding
              + '=======' + LineEnding;

    if trim( tmpResult ) = '' then
      tmpResult := cltNoOutput;

    result := result + tmpResult + LineEnding;

  finally
    SL.free;
  end;

end;


function SimpleProc( const CmdLine : string; const DoStdOut, DoStdErr, DoFormat : boolean ) : string;
var
  aProc: TProcess;
  SL : TStringList;
begin
//  QuickProc( 'sh -c "type alias"' ) for example or "$PWD | grep A"
//  QuickProc( 'sh -c "cat /home/juus/test.me | grep CW"' ) <---- worked

  Result := '';

  aProc := TProcess.Create(nil);
  try

    FillProcDefaults( aProc );
    SL := TStringList.Create;
    try
      CommandToList( CmdLine,  SL );
      HandleProcParams( aProc, TStrings( SL ) );
    finally
      SL.free;
    end;

    Result := GetProcessOutput( aProc, DummyString, DoStdOut, DoStdErr, DoFormat );

  finally
    aProc.Terminate( 0 );
    FreeandNil( aProc );
  end;

end;


function QuickProc( const theExec, theParams : string; ThirdParam : string = '' ) : string;
var
  aProc: TProcess;
begin
//=========do not change!!!! Central to the program for quick finds of environment data

  Result := '';

    aProc := TProcess.Create(nil);
    try
      FillProcDefaults( aProc );
      aProc.Executable := theExec;

      aProc.Parameters.Add( theParams );

      if ThirdParam <> '' then
        aProc.Parameters.Add( ThirdParam );

      Result := Trim( GetProcessOutput( aProc, DummyString, true, false, false ) );

    finally
      aProc.Terminate( 0 );
      FreeandNil( aProc );
    end;

end;


function IsLinuxBuiltin( const FName : string ) : boolean;
//var
//  ChkStr : String;
begin
  if not globltHasBASH then
    result := pos ( ' builtin', SimpleProc( format( 'sh -c "type %s"', [ FName ] ), true, false, false ) ) > 0
//NO!!==>  else result := pos ( ' builtin', SimpleProc( format( 'bash -i -c "type %s"', [ FName ] ), true, false, false ) ) > 0;
  else result := pos ( ' builtin', SimpleProc( format( 'bash -c "type %s"', [ FName ] ), true, false, false ) ) > 0;

  //later>> ok I changed languages and the type return messgae is still in
  //English so I'm going to use above rather than below code...
  //
  //this is sooooo FUNKY, but linux is not giving much help here
  //a positive check is "builtin' in string, much simpler but...
  //..does that vary from linux to linux, what about other locale's?
  //so this is the best I've got right now. Todo: make bulletproof.

//  ChkStr := QuickProc( format( 'sh -c "type %s"', [ FName ] ) );
//  result := ( pos( ':', ChkStr ) = 0 ) and ( pos( '/', ChkStr ) = 0 );

end;


function BuiltInNotAllowedInShell( const CmdName : string; var ResultStr : string ) : boolean;
const
  Cmd = 'command';
  Exc = 'exec';
begin
//Disallowed Builtins
  result := ( CmdName = Cmd ) or ( CmdName = Exc );
  if result then
    ResultStr := format( cltProcess4thDimension, [ Cmd + ', ' + Exc ] )
                 + LineEnding + LineEnding
  else ResultStr := '';
end;

function GetBuiltInOutput( const Command : string; Params : Tstrings ) : string;
var
  ParamStr, BuiltInName : String;
  i : Integer;
begin

  if not globltHasBASH then
  begin
    result := cltMissingBash;
    exit;
  end;

  BuiltInName := Params[ 0 ];

  if Command <> 'help' then
    if BuiltInNotAllowedInShell( BuiltInName, result ) then
      exit;

  Params.Delete( 0 );
  ParamStr := '';
  for i := 0 to Params.Count - 1 do
  begin
    ParamStr := ParamStr + ' ' + Params[ i ];
  end;

//but here the -i param is REQUIRED to get BUILTIN output
  result := SimpleProc( format( 'bash -i -c "%s %s %s"', [ Command, BuiltInName, ParamStr  ] ), true, true, true );

end;

function SystemFileLocation( const FName : string ) : string;
begin
  result := QuickProc( 'which', FName );//after superuser bug, trimming takes place in QuickProc

//DEBIAN fix for where sbin's not in $PATH, even sudo'er
  if ExtractFilePath( result ) = '' then
  begin
    result := '/sbin/' + FName;
    if FileExists( result ) then
      exit;
    result := '/usr/sbin/' + FName;
    if FileExists( result ) then
      exit;
    result := '/usr/local/sbin/' + FName;
    if FileExists( result ) then
      exit;
    result := '';
  end;
end;


function SystemFileFound( const FName : string; ShowError : boolean = false ) : boolean;
begin
  result := false;
  try
    result := SystemFileLocation( FName ) <> '';
    if not result and ShowError then
      Showmessage( format( cltmsgWhich, [ FName ] ) );
  except
    result := false;
    if ShowError then
      Showmessage( format( cltmsgWhich, [ FName ] ) );
  end;
end;

procedure StringfromMemStream( var Str : string; MS : TMemoryStream );
begin
//see also WriteMemoryStreamToString
  MS.Seek(0, soFromBeginning);
  SetString( Str, pAnsiChar( MS.Memory ), MS.Size );
end;

procedure WriteStringToMemoryStream( SourceString : string; MemoryStream : TMemorystream );
begin
  if not assigned( MemoryStream ) or ( SourceString = '' ) then
    exit;
  //from some delphi code on 'net, works great.
        // Write the string to the stream. We want to write from SourceString's
        // data, starting at a pointer to SourceString (this returns a pointer to
        // the first character), dereferenced (this gives the actual character).
  MemoryStream.WriteBuffer( Pointer( SourceString )^, Length( SourceString ) );

end;

procedure WriteMemoryStreamToString( var SourceString : string; MemoryStream : TMemorystream );
begin
  if not assigned( MemoryStream ) then
    exit;
  //from some delphi code on 'net, works great.
        // Set the length, so we have space to read into
  SetLength( SourceString, MemoryStream.Size );
  MemoryStream.ReadBuffer( Pointer( SourceString )^, MemoryStream.Size );

end;


function ProcString( Params : TStrings ) : string;
var
  aProc : TProcess;
begin
  Result := cltProcessNoParams;

  if Params.Count = 0 then
    exit;

  aProc := TProcess.Create(nil);
  try

    FillProcDefaults( aProc );
    HandleProcParams( aProc, Params );

    Result := GetProcessOutput( aProc, globltInputForProcess, true, true );
    if trim( result ) = '' then
      result := cltNoOutput + LineEnding;

  finally
    aProc.Terminate( 0 );
    Result := Result + InterpretExitCodes( aProc );
    FreeandNil( aProc );
  end;

end;

//function ProcInteger( ParamsAndResult : TStrings; SudoPW : string = '' ) : integer;
function ProcInteger( ParamsAndResult : TStrings ) : integer;
var
  aProc: TProcess;
  //I : Integer;
  //SudoPassword : string;
begin

  Result := -1;

  if ParamsAndResult.Count = 0 then
    exit;

  aProc := TProcess.Create(nil);
  try
    FillProcDefaults( aProc );

    //=================> pkexec

//    Call a secure program that does it, e.g. gksudo ... or xterm -e sudo ...

//////I leave this here as history, maybe it will come in useful some other day...too convoluted to use right now
//////aProc.Executable := '/bin/sh'; /bin/bash??!!!
//////aProc.Parameters.Add( '-c' );
//////
//////rsyncStr := ProgRSYNCPath + ' ';
//////
//////for i := 0 to CmdParams.Count - 1 do
//////begin
//////  rsyncStr := rsyncStr + CmdParams[ i ] + ' ';
//////
//////end;
//////aProc.Parameters.Add( rsyncStr );

//the below are used with sudo if you want to allow that later, password is cleartext though!!!
//better are the kdesudo or gksudo, but sudo can be implemented if needed / wanted
    //if fSuperUser then
    //   aProc.Executable := cSuperUserPath
    //else

    //      if fSuperUser then
    //      begin
    //        if fSuperUser_o then
    ////the below are used with sudo if you want to allow that later, password is cleartext though!!!
    ////better are the kdesudo or gksudo, but sudo can be implemented if needed / wanted
    //        //aProc.Parameters.add( '-S' );//sudo takes password from stdin
    //      end;


    HandleProcParams( aProc, ParamsAndResult );

    ParamsAndResult.Text := GetProcessOutput( aProc, globltInputForProcess, true, true );


//the below are used with sudo if you want to allow that later, password is cleartext though!!!
//better are the kdesudo or gksudo, but sudo can be implemented if needed / wanted
//would need to build a password box....
    //if fSuperUser then
    //begin
    //  SudoPassword := 'mypasswordincleartext!!!' + LineEnding;
    //  aProc.Input.Write(SudoPassword[1], Length(SudoPassword));
    //  SudoPassword := 'password'; //hope this will scramble memory
    //  SudoPassword := ''; // and make the program a bit safer from snooping?!?
    //end;

    Result := aProc.ExitCode;

  finally
    aProc.Terminate( 0 );
    aProc.Free;
  end;

end;

//this works grreat, just not using it in this project
//
//procedure GetMountedDrives( Strings : TStrings );
//var
//  i : Integer;
//  idx : Integer;
//  str : String;
//  SL : TStringlist;
//const
//  cLinuxSlash = '/';//there is a laz variable that has this, can't remember it right now
//begin
//  //This grabs all currently mounted drives using df command, filtered out system drives
//  //Strings should be the list of drives that have been historically mounted on this computer
//  //=IF= you are keeping track of that. Otherwise will fill Strings with current drives.
////=======================
////moved from another program, not fully tested yet
//
//  if not assigned( Strings ) then
//    exit;
//
//  if not SystemFileFound( 'df', true ) then
//    exit;
//
//  SL := TStringlist.Create;
//  try
//    SL.Text := QuickProc( 'df', '-aTh' );
//
//    for i := 0 to SL.Count - 1 do
//    begin
//      if pos( '/dev/', SL[ i ] ) = 1 then
//      begin
//        idx := pos( ' ', SL[ i ] );
//
//        if idx > 1 then
//          str := copy( Strings[ i ], idx, length( Strings[ i ] ) )
//        else exit;
//
//        idx := pos( cLinuxSlash, str );
//
//        if idx > 0 then
//          str := copy( str, idx, length( str ) )
//        else exit;
//
//        if length( str ) > 0 then
//        begin
//
//          if str = cLinuxSlash then
//            str := 'Root Drive';
//
//          //This builds list of drives that have HAVE EVER BEEN MOUNTED
//
//          Idx := Strings.IndexOf( str );
//
//          if Idx = -1 then
//            Strings.Add( str );
//
//        end;
//
//      end;
//    end;
//
//  finally
//    SL.Free;
//  end;
//
//end;


end.
{

//reference
  //Function  DiskFree(drive:
  //Function  DiskSize(drive:
  //Function GetCurrentDir :
  //Function SetCurrentDir
  //Function CreateDir
  //Function RemoveDir
  //Function ForceDirectories(Const

}
