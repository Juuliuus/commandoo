unit ufrmFind;

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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, StrUtils, LCLType;

type

  { TfrmFind }

  TfrmFind = class( TForm)
    edtFind : TEdit;
    lbFindResult : TListBox;
    procedure edtFindChange( Sender : TObject );
    procedure edtFindKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure FormCreate( Sender : TObject );
    procedure FormDeactivate( Sender : TObject );
    procedure FormKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure lbFindResultDblClick( Sender : TObject );
    procedure lbFindResultKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
  private
    fItemToFind : string;
    fLB : TListbox;
    function GotItemToFind : boolean;
    procedure SetItemToFind( const AValue : string );
    procedure SetLB( AValue : TListbox );
    { private declarations }
  public
    { public declarations }
    property LB : TListbox read fLB write SetLB;
    property ItemToFind  : string read fItemToFind write SetItemToFind;
  end;

var
  frmFind : TfrmFind;

implementation

{$R *.lfm}

{ TfrmFind }

procedure TfrmFind.edtFindChange( Sender : TObject );
var
  i : Integer;
begin
  lbFindResult.Items.Clear;
  lbFindResult.Items.BeginUpdate;
  for i := 0 to LB.Items.Count - 1 do
  begin
    if AnsiContainsText( LB.Items[ i ], edtFind.Text ) then
     lbFindResult.Items.Add( LB.Items[ i ] );
  end;
  lbFindResult.Items.EndUpdate;
end;

procedure TfrmFind.edtFindKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  if Key = vk_return then
    if lbFindResult.Count = 1 then
    begin
      ItemToFind := lbFindResult.Items[ 0 ];
      Close;
    end;
  if ( Key = vk_down ) and ( lbFindResult.Items.Count > 0 ) then
  begin
    lbFindResult.ItemIndex := 0;
    lbFindResult.SetFocus;
  end;
end;

procedure TfrmFind.FormCreate( Sender : TObject );
begin
  fItemToFind := '';
end;

procedure TfrmFind.FormDeactivate( Sender : TObject );
begin
  Close;
end;

procedure TfrmFind.FormKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  if Key = vk_escape then
    Close;
end;

function TfrmFind.GotItemToFind : boolean;
begin
  result := false;
  if ( lbFindResult.Items.Count = 0 ) or ( lbFindResult.ItemIndex < 0 ) then
    exit;
  ItemToFind := lbFindResult.Items[ lbFindResult.ItemIndex ];
  result := true;

end;

procedure TfrmFind.lbFindResultDblClick( Sender : TObject );
begin
  if GotItemToFind then
    Close;
end;

procedure TfrmFind.lbFindResultKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  if key = vk_return then
    if GotItemToFind then
      Close;
  if ( Key = vk_up ) and ( lbFindResult.ItemIndex <= 0 ) then
  begin
    edtFind.Setfocus;
  end;
end;

procedure TfrmFind.SetLB( AValue : TListbox );
begin
  if fLB = AValue then Exit;
  fLB := AValue;
end;

procedure TfrmFind.SetItemToFind( const AValue : string );
begin
  if fItemToFind = AValue then Exit;
  fItemToFind := AValue;
end;

end.

