unit ufrm_search_operator;

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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls
  , lcltype {THis is needed for key up keyboard constants}
  , unitSearch
  , ufrmBoolExpr
  ;

type

  { Tfrm_Search_Operator }

  Tfrm_Search_Operator = class( TForm)
    lblSeOpType : TLabel;
    rgSeOpOperator : TRadioGroup;
    procedure FormKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure rgSeOpOperatorSelectionChanged( Sender : TObject );
  private
    { private declarations }
    fSeOpSI : TSearchItem;
    fBE : TfrmBoolExpr;
    fIsLoading : boolean;

    procedure RegisterBoolExpr( BE : TfrmBoolExpr );
    procedure SetSeOpSI( AValue : TSearchItem );
  public
    { public declarations }
    procedure Link_SI_BoolExpr( anSI : TSearchItem; BE : TfrmBoolExpr );
    property SeOpSI : TSearchItem read fSeOpSI write SetSeOpSI;

  end;

var
  frm_Search_Operator : Tfrm_Search_Operator;

implementation

uses ufrmSearch
  , unitDBConstants
  , unitGlob
  ;

{$R *.lfm}

{ Tfrm_Search_Operator }

procedure Tfrm_Search_Operator.rgSeOpOperatorSelectionChanged( Sender : TObject );
begin
  if not assigned( fBE ) or fIsLoading then
    exit;
  case rgSeOpOperator.ItemIndex of
    0 : SeOpSI.SearchOperatorType := sotOR;
    1 : SeOpSI.SearchOperatorType := sotAND;
    2 : SeOpSI.SearchOperatorType := sotXOR;
  end;
  TfrmSearch( Parent ).UpdateCaption( fBE );
end;

procedure Tfrm_Search_Operator.FormKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  //pressing return in memo sent the key to parent form which clicked the OK button. ugh.
    if key = vk_return then
      Key := VK_UNKNOWN;
end;

procedure Tfrm_Search_Operator.RegisterBoolExpr( BE : TfrmBoolExpr );
begin
  fBE := BE;
end;

procedure Tfrm_Search_Operator.SetSeOpSI( AValue : TSearchItem );
var
  LabelType : TSearchOperatorType;
begin
  LabelType := AValue.SearchOperatorType;
  case LabelType of
    sotAND, sotOR, sotXOR : ;
    else raise EErrorDevelopment.Create( 'Tfrm_Search_Operator.SetSeOpSI: Invalid Operator Type.' );
  end;
  //no, don't do if fSeOpSI = AValue then Exit;

  fIsLoading := true;
  fSeOpSI := AValue;
  case LabelType of
    sotOR : rgSeOpOperator.ItemIndex := 0;
    sotAND : rgSeOpOperator.ItemIndex := 1;
    sotXOR : rgSeOpOperator.ItemIndex := 2;
  end;

  fIsLoading := false;

  if not self.Showing then
    Show;

end;

procedure Tfrm_Search_Operator.Link_SI_BoolExpr( anSI : TSearchItem; BE : TfrmBoolExpr );
begin
  RegisterBoolExpr( BE );
  SeOpSI := anSI;//BoolExpr must be nil because setting this fires the click event
end;

end.

