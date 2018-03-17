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

procedure StandardOutputHeader( var Str : String; const Cmd : string ); //overload;
function CanJumpToCommand( const CmdStr, CmdLineStr : string; LBCmd, LBCmdLIne : TListbox ) : boolean;
function TryToFindEditedCommand( const CmdSearch : string; LBCmd : TListbox ) : integer;
procedure ApplyChangeFont( AForm : TForm; AlwaysChange : boolean = false );


resourcestring
  cmsgcleExecuting = 'running: ';
  cmsggfCommandNotFound = 'Command "%s" not found, it must be in an edited state and can not be found until it is saved.';

var
  globRefFontSize : integer = 0;
  globFontOffset : integer = 377;

implementation

uses unitcommands, unitGlob;

const
  cDefaultFontSize = 10;
  cDesignPPI = 96;


Procedure SetRefFontSize;
begin
  globRefFontSize := trunc( cDefaultFontSize / ( Screen.PixelsPerInch / cDesignPPI )  );
end;

procedure ApplyChangeFont( AForm : TForm; AlwaysChange : boolean = false );
var
  i , NewSize : Integer;

  procedure SetAControl( aControl : TControl );
  var
    j : integer;
  begin

    with aControl do
    begin

      if ( aControl is TListbox ) then
        aControl.Font.Size := NewSize + 1
      else if ( aControl is TMemo ) then
        aControl.Font.Size := NewSize + 2
      else if aControl.Font.Size >= globRefFontSize + 3 then //BIG letter labels
        aControl.Font.Size := NewSize + 3
      else aControl.Font.Size := NewSize;

      if aControl is TWinControl then
      with TWinControl( aControl ) do
      begin
        for j := 0 to ControlCount - 1 do
          SetAControl( Controls[ j ] );
      end;

    end;

  end;

begin
//when kubuntu reset my pixperinch, the forms looked terrible.
//Lazarus high dpi (below) wanted to use the below...but...
//NO! don't want this, scaling REALLY messes up the form. It works properly, but then I guess it was my
//choice of how it is setup.
//TODO Anyway, check it out with laz 1.8 when I'm ready to switch from 1.6.
//Til then I will make a setup option to increase/decrease font size but WILL NOT Scale the buttons.
    ////Self.AutoAdjustLayout( lapAutoAdjustForDPI, 96, 128, Self.Width, ScaleX( Self.Width, 96 ) );
    //Self.AutoAdjustLayout( lapAutoAdjustForDPI, 96, Screen.PixelsPerInch, Self.Width, ScaleX( Self.Width, 96 ) );
    { High DPI in Lazarus 1.8 and above
      To handle High DPI using new features in 1.8, follow these steps:
          On Windows: enable DPI awarness in Project Options -> Application. Decide if you want to support per monitor DPI awarness or not.
          Enable LCL scaling for your application DPI awarness in Project Options -> Application -> "Use LCL scaling (Hi-DPI).
          Set TForm.Scaled=True for all your forms (it is the default value). All WYSIWYG should work automatically. Also the designer scales the forms accordingly.
          If you create controls run-time, scale all coordinates, sizes etc that have to be DPI-aware with TControl.Scale96ToForm() or ScaleDesignToForm() (depending on your choice of default PPI) or prepare your container (e.g. panel with controls) as it was with 96 PPI and then call TControl.AutoAdjustLayout(lapAutoAdjustForDPI, 96, ParentFormOfTheContainer.PixelsPerInch, 0, 0);
          If some of your components don't scale their inner sizes, override DoAutoAdjustLayout and scale the sizes (see TToolBar) - it has to be done for all controls. If a LCL control misses DoAutoAdjustLayout please report to mantis and provide a patch if you can.
      Lazarus IDE high DPI   The Lazarus IDE is DPI-aware itself.
      High DPI in older Lazarus
      Is not as nice as 1.8 but it works, call on each form OnCreate event:
      Self.AutoAdjustLayout(lapAutoAdjustForDPI, 96, Screen.PixelsPerInch, Self.Width, ScaleX(Self.Width, 96));
      }

  if not assigned( aForm ) then
    raise EErrorDevelopment.Create( 'ApplyChangeFont: incoming objects are nil' );

  if globFontOffset > 2 then
    globFontOffset := 0;

  if globRefFontSize = 0 then
    SetRefFontSize;

//form creates don't need to change if 0, but Options setting and result always need it
  if ( globFontOffset = 0 ) and not AlwaysChange then
    exit;

  NewSize := globRefFontSize + globFontOffset;

  aForm.Font.Size := NewSize;

  for i := 0 to aForm.Controlcount - 1 do
    SetAControl( aForm.Controls[ i ] );

end;

procedure StandardOutputHeader( var Str : String; const Cmd : string );
begin
  Str :=
    trim( cmsgcleExecuting + Cmd )
    + LineEnding
    + '______________________'
    + LineEnding;
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

