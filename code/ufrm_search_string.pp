unit ufrm_search_string;

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
  , ufrmBoolExpr, Menus
  ;

type

  { Tfrm_Search_String }

  Tfrm_Search_String = class( TForm)
    cbMatchCase : TCheckBox;
    lblFreeText : TLabel;
    lblSeType : TLabel;
    memSearchValue : TMemo;
    rgSeList : TRadioGroup;
    procedure cbMatchCaseChange( Sender : TObject );
    procedure FormCreate( Sender : TObject );
    procedure FormHide( Sender : TObject );
    procedure FormKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure memSearchValueChange( Sender : TObject );
    procedure rgSeListSelectionChanged( Sender : TObject );
  private
    { private declarations }
    fSe_SI : TSearchItem;
    fBE : TfrmBoolExpr;
    fIsLoading : boolean;

//enabling vars
    fIsEmptyIdx : integer;
    fIsRegX : integer;

    procedure EnableControls( const Idx : integer );
    procedure RegisterBoolExpr( BE : TfrmBoolExpr );
    procedure SetSe_SI( AValue : TSearchItem );
    procedure UpdateSearchValue;
  public
    { public declarations }
    procedure Link_SI_BoolExpr( anSI : TSearchItem; BE : TfrmBoolExpr );
    property Se_SI : TSearchItem read fSe_SI write SetSe_SI;

  end;

var
  frm_Search_String : Tfrm_Search_String;

implementation

uses ufrmSearch
    , ufrmMsgDlg
    , unitDBConstants
    , unitGlob
    ;

{$R *.lfm}

{ Tfrm_Search_String }

procedure Tfrm_Search_String.rgSeListSelectionChanged( Sender : TObject );
begin

  EnableControls( rgSeList.ItemIndex );

  if not assigned( fBE ) or fIsLoading or ( rgSeList.Itemindex < 0 ) then
    exit;

  Se_SI.Condition := ReturnSearchCondition( rgSeList.ItemIndex, cConditionsAllowed_String );
//example for later  Se_SI.Condition := ReturnSearchCondition( rgSeList.ItemIndex, cConditionsAllowed_Text );

  TfrmSearch( Parent ).UpdateCaption( fBE );

end;

procedure Tfrm_Search_String.FormCreate( Sender : TObject );
begin

  fIsLoading := false;

  GetAllowedSearchConditions( rgSeList.Items, cConditionsAllowed_String );

//grab indexes that are needed for enabling / disabling
  fIsEmptyIdx := ReturnSearchConditionIdx( coIsEmpty, cConditionsAllowed_String  );
  fIsRegX := ReturnSearchConditionIdx( coRegEx, cConditionsAllowed_String  );
  rgSeList.Caption := crg_MatchType;

end;

procedure Tfrm_Search_String.FormHide( Sender : TObject );
begin
  rgSeList.ItemIndex := -1;
end;

procedure Tfrm_Search_String.FormKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  //pressing return in memo sent the key to parent form which clicked the OK button. ugh.
    if key = vk_return then
      Key := VK_UNKNOWN;
end;

procedure Tfrm_Search_String.cbMatchCaseChange( Sender : TObject );
begin
  if fIsLoading then
    exit;
  Se_SI.CaseSensitive := cbMatchCase.Checked;
  TfrmSearch( Parent ).UpdateCaption( fBE );
end;

procedure Tfrm_Search_String.UpdateSearchValue;
begin
//callers of this maybe need to check fIsLoading, but not always
  Se_SI.SearchValue := memSearchValue.Text;
  TfrmSearch( Parent ).UpdateCaption( fBE );
end;

procedure Tfrm_Search_String.memSearchValueChange( Sender : TObject );
begin
  if fIsLoading then
    exit;
  UpdateSearchValue;
end;

procedure Tfrm_Search_String.EnableControls( const Idx : integer );
begin

  if Idx < 0 then
    exit;

  cbMatchCase.Enabled := not ( Idx in [ fIsEmptyIdx, fIsRegX ] );

  memSearchValue.Enabled := ( Idx <> fIsEmptyIdx );

  lblFreeText.Caption := rgSeList.Items[ Idx ];

end;

procedure Tfrm_Search_String.RegisterBoolExpr( BE : TfrmBoolExpr );
begin
  fBE := BE;
end;

procedure Tfrm_Search_String.SetSe_SI( AValue : TSearchItem );
var
  LabelType : TSearchOperatorType;
begin

  if AValue.SIT <> sitString then
    raise EErrorDevelopment.Create( 'Tfrm_Search_String.SetSe_SI: String Type expected.' );

  LabelType := AValue.SearchOperatorType;
  if LabelType <> sotNone then
    raise EErrorDevelopment.Create( 'Tfrm_Search_String.SetSe_SI: Invalid Operator Type.' );

  fIsLoading := true;

  //No, don't do ==> if fSe_SI = AValue then Exit;

  fSe_SI := AValue;

  lblSEType.caption := format( cusSearchCaption_String, [ fSe_SI.siDisplayCaption ] );

  cbMatchCase.Checked := fSe_SI.CaseSensitive;

  memSearchValue.Text := fSe_SI.SearchValue;

  rgSeList.ItemIndex := ReturnSearchConditionIdx( fSe_SI.Condition, cConditionsAllowed_String );

  fIsLoading := false;

  if not self.Showing then
    Show;

end;

procedure Tfrm_Search_String.Link_SI_BoolExpr( anSI : TSearchItem; BE : TfrmBoolExpr );
begin
  RegisterBoolExpr( BE );
  Se_SI := anSI;
  if anSI.InValidMsg <> '' then
  begin
    if assigned( gSearchInfo ) then
    begin
      gSearchInfo.Text :=  gSearchInfo.Text + anSI.InValidMsg + LineEnding + LineEnding;
    end
    else
    begin
      MsgDlgMessage( ccapusSearchDetailProblem, anSI.InValidMsg );
      MsgDlgInfo( TForm( Parent ) );
    end;
    anSI.ClearInvalidMessage;
  end;
end;

end.

