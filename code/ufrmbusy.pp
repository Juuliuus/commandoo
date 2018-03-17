unit ufrmBusy;

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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons;

type

  TProcessType = ( ptInfo, ptProcess, ptPipe );
  { TfrmBusy }

  TfrmBusy = class(TForm)
    btnbusyCancel : TBitBtn;
    lblBusyHint : TLabel;
    lblMsg : TLabel;
    pnlStuff : TPanel;
    Timer1 : TTimer;
    procedure btnbusyCancelClick( Sender : TObject );
    procedure btnCancelPipeClick( Sender : TObject );
    procedure FormCloseQuery( Sender : TObject; var CanClose : boolean );
    procedure FormShow( Sender : TObject );
    procedure Timer1Timer( Sender : TObject );
  private
    { private declarations }
  public
    { public declarations }
    ShowPoint : TPoint;
    procedure Shutdown;
    procedure ProcessMode;
    procedure UpdateCaptions;

  end;

var
  frmBusy : TfrmBusy;

implementation

uses unitsharedobj, linuxtrix;
{$R *.lfm}

resourcestring
  ccapBusyCaption = 'Working...';
  ccapBusyCancel = '&W Cancel';
  ccapBusyHint = 'Program will block until finished.';
{ TfrmBusy }

procedure TfrmBusy.btnbusyCancelClick( Sender : TObject );
begin
  globltDoCancelProcess := true;
end;

procedure TfrmBusy.btnCancelPipeClick( Sender : TObject );
begin
  globltDoCancelProcess := true;
end;

procedure TfrmBusy.FormCloseQuery( Sender : TObject; var CanClose : boolean );
begin
  CanClose := false;
end;

procedure TfrmBusy.FormShow( Sender : TObject );
begin
  Top := ShowPoint.Y;
  Left := ShowPoint.X;
end;

procedure TfrmBusy.Timer1Timer( Sender : TObject );
begin

  Timer1.Enabled := false;
  Screen.Cursor := crHourglass;
  Show;
  Invalidate;
  Application.Processmessages;

end;

procedure TfrmBusy.Shutdown;
begin
  Timer1.Enabled := false;
  btnbusyCancel.Visible := false;
  lblBusyHint.Visible := false;
  if showing then
    Sleep( 750 );
  hide;
end;

procedure TfrmBusy.ProcessMode;
begin
  lblBusyHint.Visible := true;
  btnbusyCancel.Visible := true;
  Timer1.Enabled := true;
end;

procedure TfrmBusy.UpdateCaptions;
begin
  caption := ccapBusyCaption;
  btnBusyCancel.caption := ccapBusyCancel;
  lblBusyHint.Caption := ccapBusyHint;
end;

end.

