unit unitglob;

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
  Classes, SysUtils;

const
  cEErrorDO_NOT_RAISE    = 0;

  cEErrorCmdObj          = 1;
  cEErrorCOField         = 2;
  cEErrorListObj         = 3;
  cEErrorDevelopment     = 4;
  cEErrorBadHandEdit     = 5;
  cEErrorDatabaseProblem = 6;
  cEErrorSystem          = 7;
  cEErrorSettings        = 8;
  cEErrorDisallowed      = 9;

  cNameItem_Command = 'Command';
  cNameItem_CommandLine = 'Command Line';
  cNameItem_CommandPlur = 'Commands';
  cNameItem_CommandLinePlur = 'Command Lines';
  cCLInfo = '--%s: %s';

//button captions, yes, BUT should NOT be translated.
  cbtn_SearchNOT = '± NOT';
  ccbHintsEnglishOverride = '&H  Show hints on mouse-over';
  cCommandInPathStr = '$PATH';
  //cLinuxBuiltInStr = '$BUILTIN';
  cmsgcleBadPath = '$BAD_PATH';
  Dots = '...';
  cDoubleS = '%s %s';
  ccapGenericUnderlinedOK = '&OK';
  ccapGenericOK = 'OK';

resourcestring
  cmsgNoCommandSelected = 'No item is selected.';
  //ccapGenericEdit = 'Edit %s';
  //ccapGenericAdd = 'Add %s';
  //ccapGenericDelete = 'Delete %s';
  //ccapGenericImport = 'Import %s';
  cmsgGenericDelete = 'Are you sure you want to delete the selected items?';
  ccapGenericChoose = 'Choose %s';
  ccapGenericFile = 'File';
  ccapGenericFolder = 'Folder';
  ccapGenericProfile = 'Profile';

  ccapGenericImport = 'Import';
  ccapGenericSelect = 'Select';
  ccapGenericAdd = 'Add';
  ccapGenericNew = 'New';
  ccapGenericEdit = 'Edit';
  ccapGenericCopy = 'Copy';
  ccapGenericDelete = 'Delete';
  ccapGenericClose = 'Close';
  ccapGenericCancel = 'Cancel';
//Convert
//Compare To
//Merge To


//Re-Used button captions to keep the translating less miserable
//I've decided to use numbers/letters for shortcuts as they are language independant
  cObjlblRun = '&R  Run';
  cObjlblGo = '&G  Go';
  cObjlblRunTest = '&R  Test';
  cbtn_Copy = 'Copy';
  cbtn_Done = '&O  Done';
  crg_MatchType = 'Match Type';
  crg_Value = 'Value';
  ccapFindLBHint = 'Begin typing...';
  ccapFindTextHint = 'Type then <enter>...';

type
  //raise EErrorDatabaseProblem.Create( 'EXCEPTION STRESS TEST' );
  EErrorSELF_RAISE = class(Exception);
  EErrorUnknown = class(Exception);
  EErrorCmdObj = class(Exception);
  EErrorCOField = class(Exception);
  EErrorListObj = class(Exception);
  EErrorDevelopment = class( Exception );//"should" be caught in development
  EErrorBadHandEdit = class( Exception );//things where hand editing cause problems and/or development
  EErrorDatabaseProblem = class( Exception );//file structure, sql statements
  EErrorSystem = class( Exception );//problems with calls to system
  EErrorSettings = class( Exception );//problems user settings
  EErrorDisallowed = class( Exception );//things that are not allowed

procedure EErrorRaise( e : exception; const cEError : integer; const Msg : string );



var
  globFatalException : boolean = false;

implementation

procedure EErrorRaise( e : exception; const cEError : integer; const Msg : string );
begin
  if (e is EAccessViolation ) then
    GlobFatalException := true;
  case cEError of
    cEErrorCmdObj          :
      raise EErrorCmdObj.Create( Msg + e.message );
    cEErrorCOField         :
      raise EErrorCOField.Create( Msg + e.message );
    cEErrorListObj         :
      raise EErrorListObj.Create( Msg + e.message );
    cEErrorDevelopment     :
      raise EErrorDevelopment.Create( Msg + e.message );
    cEErrorBadHandEdit     :
      raise EErrorBadHandEdit.Create( Msg + e.message );
    cEErrorDatabaseProblem :
      raise EErrorDatabaseProblem.Create( Msg + e.message );
    cEErrorSystem          :
      raise EErrorSystem.Create( Msg + e.message );
    cEErrorSettings        :
      raise EErrorSettings.Create( Msg + e.message );
    cEErrorDisallowed      :
      raise EErrorDisallowed.Create( Msg + e.message );
    cEErrorDO_NOT_RAISE    :
      begin
        if GlobFatalException then
          raise EErrorSELF_RAISE.Create( Msg + e.message );
      end;
    else
      raise EErrorUnknown.Create( Msg + e.message );
  end;
end;

end.

