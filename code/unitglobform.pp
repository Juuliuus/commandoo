unit unitGlobForm;

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
  Classes, SysUtils, Dialogs, StdCtrls, Forms, Controls, graphics, ActnList;

type

  //{ TListBoxHelper2 }  //Helper class example if I ever need or want it
  //
  //TListBoxHelper = class helper for TListbox
  //  procedure DoClick;
  //end;

  TSettingsDirective = (sdSave, sdLoad );

function StandardOutputHeader( const Cmd : string ) : string;
function CanJumpToCommand( const CmdStr, CmdLineStr : string; LBCmd, LBCmdLIne : TListbox ) : boolean;
function TryToFindEditedCommand( const CmdSearch : string; LBCmd : TListbox ) : integer;
procedure ApplyChangeFont( AForm : TForm; AlwaysChange : boolean = false );


resourcestring
  cmsgcleExecuting = 'running: ';
  cmsggfCommandNotFound = 'Command "%s" not found, it must be in an edited state and can not be found until it is saved.';

var
  globFontsLarge : boolean = false;

const
  cDefaultFontSize = 9;

implementation

uses unitcommands, unitGlob;

procedure ApplyChangeFont( AForm : TForm; AlwaysChange : boolean = false );
var
  i , NewSize : Integer;

  procedure SetAControl( aControl : TControl );
  var
    j : integer;
  begin

    aControl.Font.Size := aControl.Font.Size + NewSize;

    if aControl is TWinControl then
    with TWinControl( aControl ) do
    begin
      for j := 0 to ControlCount - 1 do
        SetAControl( Controls[ j ] );
    end;

  end;

begin

  if not assigned( aForm ) then
    raise EErrorDevelopment.Create( 'ApplyChangeFont: incoming objects are nil' );

  aForm.AutoAdjustLayout( lapAutoAdjustForDPI, aForm.DesignTimePPI, Screen.PixelsPerInch, aForm.Width, ScaleX( aForm.Width, aForm.DesignTimePPI ) );

//form creates don't need to change if false, but Options setting and result always need it
  if not globFontsLarge and not AlwaysChange then
    exit;

  if aForm.Font.Size = cDefaultFontSize then
    NewSize := 1
  else NewSize := -1;

  aForm.Font.Size := aForm.Font.Size + NewSize;

  for i := 0 to aForm.Controlcount - 1 do
    SetAControl( aForm.Controls[ i ] );

end;

function StandardOutputHeader( const Cmd : string ) : string;
begin
  result :=
    trim( cmsgcleExecuting + Cmd )
    + LineEnding
    + '______________________';
end;

procedure JumpToCommand( anIdx : integer; CmdLineStr : string; LBCmd, LBCmdLIne : TListbox );
begin
  LBCmd.ItemIndex := anIdx;
  LBCmd.Click;
  if not assigned( LBCmdLine ) then
    exit;
  anIdx := LBCmdLine.Items.IndexOf( CmdLineStr );
  if anIdx > -1 then
  begin
    LBCmdLine.ItemIndex := anIdx;
    LBCmdLine.Click;
  end;

end;

function TryToFindEditedCommand( const CmdSearch : string; LBCmd : TListbox ) : integer;
var
  i : Integer;
  aCmdObj : TCmdObj;
begin
  result := -1;
  for i := LBCmd.Items.Count - 1 downto 0 do
  begin
    aCmdObj := TCmdObj( LBCmd.Items.Objects[ i ] );
    if not Assigned(aCmdObj) then
      continue;
    if aCmdObj.DoUpdate then
    begin
 //find entries like "<UPDATE> gimp'
      if pos(' ' + CmdSearch, LBCmd.Items[ i ] ) > 0 then
      begin
        result := i;
        break;
      end;
    end;
  end;
end;


function CanJumpToCommand( const CmdStr, CmdLineStr : string; LBCmd, LBCmdLIne : TListbox ) : boolean;
var
  Idx : Integer;
begin
  result := false;
  if not assigned( LBCmd ) then
    exit;

  if CmdStr <> '' then
  begin

    Idx := LBCmd.Items.IndexOf( CmdStr );

    if Idx = -1 then
    begin
      Idx := TryToFindEditedCommand( CmdStr, LBCmd );
      if Idx = -1 then
      begin
        MessageDlg( format( cmsggfCommandNotFound, [ CmdStr ] ), mtInformation, [mbOK], 0);
        exit;
      end;
    end;

    JumpToCommand( Idx, CmdLineStr, LBCmd, LBCmdLIne );
    result := true;

  end;

end;

//{ TListBoxHelper2 }
//
//procedure TListBoxHelper.DoClick;
//begin
//  Self.Click;
//end;

end.

