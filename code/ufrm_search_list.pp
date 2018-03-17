unit ufrm_search_list;

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

  { Tfrm_Search_List }

  Tfrm_Search_List = class( TForm)
    cbMatchCase : TCheckBox;
    lblListChoice : TLabel;
    lblFreeText : TLabel;
    lblKeywords : TLabel;
    lblSeType : TLabel;
    lbList : TListBox;
    memSearchValue : TMemo;
    mniCopy : TMenuItem;
    popList : TPopupMenu;
    rgSeList : TRadioGroup;
    procedure cbMatchCaseChange( Sender : TObject );
    procedure FormCreate( Sender : TObject );
    procedure FormHide( Sender : TObject );
    procedure FormKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure lbListClick( Sender : TObject );
    procedure memSearchValueChange( Sender : TObject );
    procedure mniCopyClick( Sender : TObject );
    procedure rgSeListSelectionChanged( Sender : TObject );
  private
    { private declarations }
    fSe_SI : TSearchItem;
    fBE : TfrmBoolExpr;
    fIsLoading : boolean;

//enabling vars
    fMatchIdx : integer;
    fMatchNotIdx : integer;
    fIsEmptyIdx : integer;
    fContainsIdx : integer;
    fContainsNotIdx : integer;

    procedure EnableControls( const Idx : integer );
    procedure ForceIfNecessaryToValidItem;
    procedure RegisterBoolExpr( BE : TfrmBoolExpr );
    procedure SetSe_SI( AValue : TSearchItem );
    procedure UpdateSearchValue;
  public
    { public declarations }
    procedure Link_SI_BoolExpr( anSI : TSearchItem; BE : TfrmBoolExpr );

    property Se_SI : TSearchItem read fSe_SI write SetSe_SI;

  end;

var
  frm_Search_List : Tfrm_Search_List;

implementation

uses ufrmSearch
    , ufrmMsgDlg
    , unitDBConstants
    , unitGlob
    ;

{$R *.lfm}

{ Tfrm_Search_List }

procedure Tfrm_Search_List.rgSeListSelectionChanged( Sender : TObject );
begin

  EnableControls( rgSeList.ItemIndex );

  if not assigned( fBE ) or fIsLoading or ( rgSeList.Itemindex < 0 ) then
    exit;

  Se_SI.Condition := ReturnSearchCondition( rgSeList.ItemIndex, cConditionsAllowed_List );

  TfrmSearch( Parent ).UpdateCaption( fBE );

end;

procedure Tfrm_Search_List.FormCreate( Sender : TObject );
begin

  fIsLoading := false;

  GetAllowedSearchConditions( rgSeList.Items, cConditionsAllowed_List );

//grab indexes that are needed for enabling / disabling
  fMatchIdx := ReturnSearchConditionIdx( coMatchQualified, cConditionsAllowed_List );
  fMatchNotIdx := ReturnSearchConditionIdx( coMatchQualifiedNot, cConditionsAllowed_List  );
  fIsEmptyIdx := ReturnSearchConditionIdx( coIsEmpty, cConditionsAllowed_List  );
  fContainsIdx := ReturnSearchConditionIdx( coContains, cConditionsAllowed_List  );
  fContainsNotIdx := ReturnSearchConditionIdx( coContainsNot, cConditionsAllowed_List  );

  rgSeList.Caption := crg_MatchType;

end;

procedure Tfrm_Search_List.FormHide( Sender : TObject );
begin
  rgSeList.ItemIndex := -1;
end;

procedure Tfrm_Search_List.FormKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  //pressing return in memo sent the key to parent form which clicked the OK button. ugh.
    if key = vk_return then
      Key := VK_UNKNOWN;
end;

procedure Tfrm_Search_List.cbMatchCaseChange( Sender : TObject );
begin
  if fIsLoading then
    exit;
  Se_SI.CaseSensitive := cbMatchCase.Checked;
  TfrmSearch( Parent ).UpdateCaption( fBE );
end;

procedure Tfrm_Search_List.lbListClick( Sender : TObject );
begin
  mniCopyClick( lbList );
end;

procedure Tfrm_Search_List.UpdateSearchValue;
begin
//callers of this maybe need to check fIsLoading, but not always
  Se_SI.SearchValue := memSearchValue.Text;
  TfrmSearch( Parent ).UpdateCaption( fBE );
end;

procedure Tfrm_Search_List.memSearchValueChange( Sender : TObject );
begin
  if fIsLoading then
    exit;
  UpdateSearchValue;
end;

procedure Tfrm_Search_List.mniCopyClick( Sender : TObject );
begin
  if ( lbList.Items.Count = 0 ) or ( lbList.ItemIndex < 0 ) then
    exit;
  memSearchValue.Text := lbList.Items[ lbList.ItemIndex ];
end;


procedure Tfrm_Search_List.EnableControls( const Idx : integer );
var
  Are_Matchers : Boolean;
begin

  if Idx < 0 then
    exit;

  cbMatchCase.Enabled := Idx in [ FContainsIdx, FContainsNotIdx ];

  Are_Matchers := Idx in [ fMatchIdx, fMatchNotIdx ];

  lbList.Enabled := ( Idx <> fIsEmptyIdx );

  memSearchValue.Enabled := ( Idx <> fIsEmptyIdx ) and not Are_Matchers;//lbList.Enabled;

  case Are_Matchers of
    true :
      begin
        ForceIfNecessaryToValidItem;
        lblListChoice.Visible := true;
      end;
   else
     begin
       lblListChoice.Visible := false;
       lbList.Enabled := false;
     end;
  end;
  lblFreeText.Caption := rgSeList.Items[ Idx ];

end;

procedure Tfrm_Search_List.ForceIfNecessaryToValidItem;
var
  Idx : Integer;
begin
  Idx := lbList.Items.IndexOf( trim( memSearchValue.Text ) );
  if Idx < 0 then
  begin
    lbList.ItemIndex := 0; //should be ok, search isn't opened if list is empty.
    memSearchValue.Text := lbList.Items[ 0 ];
    UpdateSearchValue;
  end
  else lbList.ItemIndex := Idx;
end;


procedure Tfrm_Search_List.RegisterBoolExpr( BE : TfrmBoolExpr );
begin
  fBE := BE;
end;

procedure Tfrm_Search_List.SetSe_SI( AValue : TSearchItem );
var
  LabelType : TSearchOperatorType;
begin

  if AValue.SIT <> sitString then
    raise EErrorDevelopment.Create( 'Tfrm_Search_List.SetSe_SI: String Type expected.' );

  LabelType := AValue.SearchOperatorType;
  if LabelType <> sotNone then
    raise EErrorDevelopment.Create( 'Tfrm_Search_List.SetSe_SI: Invalid Operator Type.' );

  fIsLoading := true;

  //No, don't do ==> if fSe_SI = AValue then Exit;

  fSe_SI := AValue;

  lblSEType.caption := format( cusSearchCaption_List, [ fSe_SI.siDisplayCaption ] );

  cbMatchCase.Checked := fSe_SI.CaseSensitive;

  memSearchValue.Text := fSe_SI.SearchValue;

  rgSeList.ItemIndex := ReturnSearchConditionIdx( fSe_SI.Condition, cConditionsAllowed_List );
//example for later  //rgSeList.ItemIndex := ReturnSearchConditionIdx( fSe_SI.Condition, cConditionsAllowed_Text );

  fIsLoading := false;

  if not self.Showing then
    Show;

end;

procedure Tfrm_Search_List.Link_SI_BoolExpr( anSI : TSearchItem; BE : TfrmBoolExpr );
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

