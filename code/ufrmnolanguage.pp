unit ufrmnolanguage;

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
  Classes, SysUtils, LazFileUtils,
  Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons
  , unitglobform
  ;

type

  { TfrmNoLanguage }

  TfrmNoLanguage = class (TForm)
    btnOK : TBitBtn;
    cbLanguage : TComboBox;
    lblProgDesc : TLabel;
    Label2 : TLabel;
    procedure btnOKClick(Sender : TObject);
    procedure FormCloseQuery(Sender : TObject; var CanClose : boolean);
    procedure FormCreate(Sender : TObject);
  private
    { private declarations }
    fCanClose : boolean;
  public
    { public declarations }
  end;

var
  frmNoLanguage : TfrmNoLanguage;

implementation

{$R *.lfm}

{ TfrmNoLanguage }

uses strconst_en;

procedure TfrmNoLanguage.btnOKClick(Sender : TObject);
begin
  fCanClose := true;
end;

procedure TfrmNoLanguage.FormCloseQuery(Sender : TObject; var CanClose : boolean);
begin
  CanClose := fCanClose;
end;

procedure TfrmNoLanguage.FormCreate(Sender : TObject);
begin
  ApplyChangeFont( Self );
  fCanClose := false;
  lblProgDesc.Caption := ccapProgram;
end;

end.

