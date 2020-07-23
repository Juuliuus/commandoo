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
  Classes, SysUtils, Dialogs, StdCtrls, Forms, Controls, graphics, ActnList, Menus, Buttons;

type

  //{ TListBoxHelper2 }  //Helper class example if I ever need or want it
  //
  //TListBoxHelper = class helper for TListbox
  //  procedure DoClick;
  //end;

  TSettingsDirective = (sdSave, sdLoad );

function StandardOutputHeader( const Cmd : string ) : string;
procedure FormAutoAdjustLayout( AForm : TForm );
procedure ApplyChangeFont( AForm : TForm; AlwaysChange : boolean = false );
procedure SetCapMenuButton( M : TMenuItem; B : TBitBtn; const Cap, Extra : string );
procedure TryFocus( aControl : TWinControl );


resourcestring
  cmsgcleExecuting = 'running: ';
  cmsggfCommandNotFound = 'Command "%s" not found, it must be in an edited state and can not be found until it is saved.';

var
  globFontsLarge : boolean = false;

const
  cDefaultFontSize = 9;

implementation

uses unitGlob;

procedure TryFocus( aControl : TWinControl );
begin
  if aControl.Canfocus then
    aControl.Setfocus;
end;

procedure SetCapMenuButton( M : TMenuItem; B : TBitBtn; const Cap, Extra : string );
const
  HotK = '%s  ';
begin
  M.Caption := Cap;
  if Extra <> '' then
    B.Caption := format( HotK, [ Extra ] ) + M.Caption
  else B.Caption := M.Caption;
end;

procedure FormAutoAdjustLayout( AForm : TForm );
begin
  {$IFNDEF Release}
    if not assigned( aForm ) then
      raise EErrorDevelopment.Create( 'FormAutoAdjustLayout: incoming object is nil' );
  {$ENDIF}
  aForm.AutoAdjustLayout( lapAutoAdjustForDPI, aForm.DesignTimePPI, Screen.PixelsPerInch, aForm.Width, ScaleX( aForm.Width, aForm.DesignTimePPI ) );
end;

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

{$IFNDEF Release}
  if not assigned( aForm ) then
    raise EErrorDevelopment.Create( 'ApplyChangeFont: incoming object is nil' );
{$ENDIF}

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


//{ TListBoxHelper2 }
//
//procedure TListBoxHelper.DoClick;
//begin
//  Self.Click;
//end;

end.

