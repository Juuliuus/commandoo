unit ufrmBoolExpr;

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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs
  , lcltype {THis is needed for key up keyboard constants}
  , unitsharedobj, Buttons, unitGlobForm, StdCtrls, ExtCtrls{, ComCtrls}
  , ActnList, Menus
  , unitDBConstants
  , unitDBStructure
  , unitSearch
  , unitFields
  ;

type


  { TfrmBoolExpr }

  TfrmBoolExpr = class(TForm)
    actCmdSI_L_Add : TAction;
    actCmdSI_R_Add : TAction;
    actCmd_NOT_Toggle : TAction;
    actCmd_Delete : TAction;
    actCmd_Exchange : TAction;
    actCmd_Paren_Add : TAction;
    actCmd_Paren_L_Move : TAction;
    actCmd_Paren_R_Move : TAction;
    actCmdSelectLeft : TAction;
    actCmdSelectRight : TAction;
    ActionList1 : TActionList;
    Bevel1 : TBevel;
    btnCmdAdd_SI_L : TButton;
    btnCmdCopy : TButton;
    btnCmdSelectLeft : TButton;
    btnCmdSelectRight : TButton;
    btnCmdSI_R_Add : TButton;
    btnCmd_Delete : TButton;
    btnCmd_Exchange : TButton;
    btnCmd_NOT_Toggle : TButton;
    btnCmd_Paren_Add : TButton;
    btnCmd_Paren_L_Move : TButton;
    btnCmd_Paren_R_Move : TButton;
    cbCmdCompactView : TCheckBox;
    cbCmdUse : TCheckBox;
    flowCmd : TFlowPanel;
    lblExchangeNotice : TLabel;
    MenuItem1 : TMenuItem;
    MenuItem2 : TMenuItem;
    MenuItem3 : TMenuItem;
    MenuItem4 : TMenuItem;
    mnibeCmdCompact : TMenuItem;
    mnibeCmdCopy : TMenuItem;
    MenuItem6 : TMenuItem;
    mnifseCmdRoot : TMenuItem;
    mniCmdSelectLeft : TMenuItem;
    mniCmdSelectRight : TMenuItem;
    mniCmd_Paren_L_Move : TMenuItem;
    mniCmd_Paren_R_Move : TMenuItem;
    mniCmd_Paren_Add : TMenuItem;
    mniCmdSI_L_Add : TMenuItem;
    mniCmdSI_R_Add : TMenuItem;
    mniCmd_NOT_Toggle : TMenuItem;
    mniCmd_Delete : TMenuItem;
    mniCmd_Exchange : TMenuItem;
    pnlDoCopy : TPanel;
    pnlCmd : TPanel;
    popCmd : TPopupMenu;
    scbCmd : TScrollBox;
    shpPRight : TShape;
    shpPLeft : TShape;
    Tmr : TTimer;
    procedure actCmdSelectLeftExecute( Sender : TObject );
    procedure actCmdSelectRightExecute( Sender : TObject );
    procedure actCmdSI_L_AddExecute( Sender : TObject );
    procedure actCmdSI_R_AddExecute( Sender : TObject );
    procedure actCmd_DeleteExecute( Sender : TObject );
    procedure actCmd_ExchangeExecute( Sender : TObject );
    procedure actCmd_NOT_ToggleExecute( Sender : TObject );
    procedure actCmd_Paren_AddExecute( Sender : TObject );
    procedure actCmd_Paren_L_MoveExecute( Sender : TObject );
    procedure actCmd_Paren_R_MoveExecute( Sender : TObject );
    procedure btnCmdCopyClick( Sender : TObject );
    procedure cbCmdCompactViewChange( Sender : TObject );
    procedure cbCmdUseChange( Sender : TObject );
    procedure FormClose(Sender : TObject; var CloseAction : TCloseAction);
    procedure FormCreate(Sender : TObject);
    procedure FormKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure FormShow(Sender : TObject);
    procedure mnibeCmdCompactClick( Sender : TObject );
    procedure TmrStartTimer( Sender : TObject );
    procedure TmrStopTimer( Sender : TObject );
    procedure TmrTimer( Sender : TObject );
  private
    fDefaultCmdFieldID : TCmdFieldID;
    { private declarations }
    fEditMode : integer;
    FHasShown : boolean;
    fCmdPointer : TControl; //do not create, used internally
    fLastCmdPointer : TControl; //do not create, used internally
    fListSearchItems : TList;
    fSelectSIMode : boolean;
    fSelectSIIdx : integer;
    fValidFields : TStringlist;

    function AddToEnd( const Idx : integer; anSI : TSearchItem ) : boolean;
    procedure Add_OP_SI( anSI : TSearchItem; pFID : TCmdFieldID; const Idx : integer );
    procedure Add_SI_OP( anSI : TSearchItem; pFID : TCmdFieldID; const Idx : integer );

    procedure Add_ToEnd( anSI : TSearchItem; pFID : TCmdFieldID );
    function CheckGlobCopySI : boolean;
    procedure ClearEmptyParens( Idx : integer );
    procedure ClearParensColor( Idx : integer = - 1 );
    //procedure DebugExpression;
    procedure DeleteItems( const Idx, DelCount : integer );
    procedure DeleteParens( const OrigIdx, MatchIdx : integer );
    procedure DetermineParenHighlighting( aControl : TControl );
    procedure ExchangeSIs( aControl : TControl );
    procedure GetEnclosingParens( aControl : TControl );
    function GetInactiveColor( aControl : TControl) : TColor;
    function GetParenSibling_Left( aControl : TControl) : integer;
    function GetParen_Right( aControl : TControl) : integer;
    function Get_Non_OP_ToLeft( Idx : integer ) : integer;
    function Get_Non_OP_ToRight( const Idx : integer ) : integer;
    function HadNone( anSI : TSearchItem ) : boolean;
    procedure HighlightMatchingParenToLeft( aControl : TControl );
    procedure HighlightMatchingParenToRight( aControl : TControl );
    function IsEmptyParens( const Idx : integer ) : boolean;
    function IsLast_SI( const Idx : integer ) : boolean;
    function Is_OpenParen_Bookended_By_NOTs( const Idx : integer ) : boolean;
    function Is_NOT_Item( const Idx : integer ) : boolean;
    function IsSingle_SI : integer;
    function NextSI_IsCloseParen( const Idx : integer ) : boolean;
    function Paren_Move_Invalid( const Cond : boolean; shp : TShape ) : boolean;
    procedure ProcessCmdActions;
    procedure ReFocus( Idx : integer );

    procedure AddToFlowPanel( const Flow : TFlowPanel; const LB : TLabel; const Idx : integer );
    procedure Add_Control( LB : TLabel; var SI : TSearchItem; const Idx : integer );
    procedure Add_OP( anSI : TSearchItem; pSOT : TSearchOperatorType; const Idx : integer );
    function aCTRL( const Idx : integer ) : TControl;
    procedure Delete_Control( const Idx : integer ); overload;
    procedure Delete_Control( const aControl : TControl ); overload;
    //procedure HandleFormSettings(TheType : TSettingsDirective);
    function Add_SI( anSI : TSearchItem; pFID : TCmdFieldID; const Idx : integer) : smallint;
    procedure MakeDisplayLabel( LB : TLabel; SI : TSearchItem );
    procedure MasterClick( Sender : TObject );
    function SetCmdPointer( aControl : TControl) : boolean;
    procedure SetCursors( Cur : TCursor; const Blink : boolean; myIdx : integer = - 1 );
    procedure SetDefaultCmdFieldID( AValue : TCmdFieldID );
    procedure SetEditMode( const AValue : integer );
    procedure SetValidFields( AValue : TStringlist );
    procedure UpDateLabelCaption_SI( LB : TLabel; SI : TSearchItem );
  public
    { public declarations }

    procedure UpdateCurrCaption;
    procedure FocusOn;
    procedure FocusOff;
    procedure TransferSearchCriteria( destBS : TBaseSearch );
    function ReadSearchCriteria( fromBS : TBaseSearch) : boolean;
    function HasItems : boolean;
    function ReInit( pCmdFieldID : TCmdFieldID; pFieldType : TufFieldType) : boolean;
    procedure ReSelect;
    procedure CopyMode( const DoEnable : boolean );


    property EditMode : integer read fEditMode write SetEditMode;
    property CmdPointer : TControl read fCmdPointer;
    property ListSearchItems : TList read fListSearchItems;
    property DefaultCmdFieldID : TCmdFieldID read fDefaultCmdFieldID write SetDefaultCmdFieldID;
    property ValidFields : TStringlist read fValidFields write SetValidFields;

  end;

var
  frmBoolExpr : TfrmBoolExpr;

resourcestring
  cfseUseCmd_Caption = 'Use COMMAND search';
  cfseUseCmdLine_Caption = 'Use COMMAND LINE search';
  cfseMenuRoot = 'Boolean builder';

implementation

{$R *.lfm}

uses ufrmSearch
     , juusgen
     , unitGlob
     , ufrmMsgDlg
     , strconst_en
     ;

{.$DEFINE Numbered_SI_Items}
resourcestring

  cfseExpressionHint =
    'This is one of the expressions you have created. '
    + LineEnding + LineEnding
    + 'Dbl Click it to edit it, or use one of the buttons '
    + LineEnding
    + 'above to manipulate it.'
    + LineEnding + LineEnding
    + '<end>'
    + LineEnding + LineEnding
    ;

  cfseOperatorHint =
    'This is an operator or organizing item. '
    + LineEnding + LineEnding
    + 'With Operators (or, and, xor) you can Dbl Click it '
    + LineEnding
    + 'to edit it. '
    + LineEnding + LineEnding
    + 'You can always use one of the buttons above to '
    + LineEnding
    + 'manipulate it.'
    + LineEnding + LineEnding
    + '<end>'
    + LineEnding + LineEnding
    ;

  cfseMainHint =
    'Contains the current expressions for this '
    + LineEnding
    + 'portion of the search. '
    + LineEnding + LineEnding
    + 'Select any item to enable the appropriate '
    + LineEnding
    + 'buttons and then perform the action you '
    + LineEnding
    + 'want. '
    + LineEnding + LineEnding
    + 'Dbl Click expressions or operators to edit '
    + LineEnding
    + 'them. '
    + LineEnding + LineEnding
    + '<end>'
    + LineEnding + LineEnding
    ;

  cfseMoveParenHint =
    'Use these to move parentheses around. '
    + LineEnding + LineEnding
    + 'When adding parentheses only the selected '
    + LineEnding
    + 'item gets them. You can then select either '
    + LineEnding
    + 'of the parentheses and use these to move and  '
    + LineEnding
    + 'logically group as you like. '
    + LineEnding + LineEnding
    + 'The move is smart and will not allow logical '
    + LineEnding
    + 'grouping errors. '
    + LineEnding + LineEnding
    + '<end>'
    + LineEnding + LineEnding
    ;

  cmsgBEInvalidField = '"%s" is not a valid field for "%s".';
  cmsgBEClipBoardFull = 'Search item clipboard is already full, <esc> or add to an expression.';
  cmsgBENoItem = 'No Search Item to copy!';
  cmsgBEInvalidItem = 'cant copy, invalid';

{$IFDEF Numbered_SI_Items}
var
  SI_CREATE_COUNT : integer = 0;
{$ENDIF}

const
  DefNewSIString = '  %s  %s  "%s"  ';
  DefNewSI = '  %s  %s  %s  ';
  DefNewOP = '  %s  ';

  cColNonSelectedSI = $262626;//121212;//clBlack;
  cColExhangeColor = $256109;//#096125
  cColInvalidSI = $091593; // #931509 dull red
  cColNonSelectedOP = clBlack;
  cColNonSelectedParen = clWhite;
  cColMatchingParen = clAqua;//899318 ;//#189389

  cColSelected = $AA5E2E;//dark blueish //#2E5EAA

  LT_UNK = 0;
  LT_SI = 1;
  LT_OP = 2;
  LT_Not = 3;
  LT_O = 4;
  LT_C = 5;


{ TfrmBoolExpr }

procedure TfrmBoolExpr.FormShow(Sender : TObject);
begin

  if not FHasShown then
  begin
//Don't do
    //HandleFormSettings( sdLoad );

    flowCmd.AutoSize := true;

    ProcessCmdActions;

    FHasShown := true;
  end;

end;

procedure TfrmBoolExpr.mnibeCmdCompactClick( Sender : TObject );
begin
  cbCmdCompactView.Checked := not cbCmdCompactView.Checked;
end;

procedure TfrmBoolExpr.TmrStartTimer( Sender : TObject );
begin
  lblExchangeNotice.Visible := true;
end;

procedure TfrmBoolExpr.TmrStopTimer( Sender : TObject );
begin
  lblExchangeNotice.Visible := false;
end;

procedure TfrmBoolExpr.TmrTimer( Sender : TObject );
begin
  lblExchangeNotice.Visible := not lblExchangeNotice.Visible;
end;

function TfrmBoolExpr.GetInactiveColor( aControl : TControl ) : TColor;
begin

  case aControl.HelpContext of
    LT_SI :
      begin
        if TSearchItem( fListSearchItems[ aControl.Tag ] ).IsInvalid then
          result := cColInvalidSI
        else result := cColNonSelectedSI;
      end;
    LT_UNK : result := clOlive;//Uhhhgly! So, it is easy to see!
    else result := cColNonSelectedOP;// = 0;
  end;

end;

procedure TfrmBoolExpr.ProcessCmdActions;
var
  LabelType : THelpContext;
  Cnt : Integer;
  AllOff : Boolean;
begin

  AllOff := fSelectSIMode;//in future maybe other conditions too

  shpPLeft.Visible := false;
  shpPRight.Visible := false;

  if assigned( fCmdPointer ) then
    LabelType := fCmdPointer.HelpContext
  else LabelType := LT_UNK;

  Cnt := flowCmd.ControlList.Count;

  actCmdSelectLeft.Enabled := ( Cnt > 1 );// or AllOff;
  actCmdSelectRight.Enabled := ( Cnt > 1 );// or AllOff;

  actCmdSI_L_Add.Enabled := ( ( Cnt = 0 ) or ( LabelType in [ LT_SI, LT_O, LT_C, LT_Not ]  ) ) and not AllOff;
  actCmdSI_R_Add.Enabled := ( ( Cnt = 0 ) or ( LabelType in [ LT_SI, LT_O, LT_C ] ) ) and not AllOff;

  actCmd_NOT_Toggle.Enabled := ( ( LabelType in [ LT_SI, LT_O ] ) and ( Cnt > 0 ) ) and not AllOff;

  actCmd_Delete.Enabled := ( ( LabelType in [ LT_SI, LT_O, LT_C, LT_Not ] ) and ( Cnt > 0 ) ) and not AllOff;

  actCmd_Exchange.Enabled := ( LabelType = LT_SI ) and ( ( Cnt > 0 ) and ( IsSingle_SI = -1 ) ) and not AllOff;
  actCmd_Paren_Add.Enabled := ( LabelType = LT_SI ) and not AllOff;

  actCmd_Paren_L_Move.Enabled := ( LabelType in [ LT_O, LT_C ] ) and not AllOff;
  actCmd_Paren_R_Move.Enabled := actCmd_Paren_L_Move.Enabled;

end;


//procedure TfrmBoolExpr.HandleFormSettings(TheType : TSettingsDirective);
//var
//  i : Integer;
//  theValue : string;
//begin
//  theValue := '';
//  i := -1;
//  case TheType of
//    sdSave :
//      begin
//        FormSettings.AddSetting( inttoStr( Height ) );
//        FormSettings.AddSetting( inttoStr( Width ) );
//
//        FormSettings.SaveFormSettings( Self.Name );
//      end;
//    sdLoad :
//      While FormSettings.ReadFormSettings( Self.Name, i, theValue ) do
//      case i of
//        0 : Height := strtoint( theValue );
//        1 : Width := strtoint( theValue );
//      end;
//  end;
//
//end;

procedure TfrmBoolExpr.FormClose(Sender : TObject; var CloseAction : TCloseAction);
var
  i : Integer;
begin

  fLastCmdPointer := nil;
  fCmdPointer := nil;

  if assigned( fValidFields ) then
  begin
    for i := 0 to fValidFields.Count - 1 do
      fValidFields.Objects[ i ] := nil;
    FreeAndNil( fValidFields );
  end;

  //HandleFormSettings( sdSave );

  if assigned( fListSearchItems ) then
  begin
    for i := 0 to fListSearchItems.Count - 1 do
    begin
      if not assigned( fListSearchItems[ i ] ) then
        continue;
      TObject( fListSearchItems[ i ] ).Free;
    end;
    fListSearchItems.Free;
  end;

end;

function TfrmBoolExpr.HadNone( anSI : TSearchItem ) : boolean;
begin
  result := false;
  if flowCmd.ControlList.Count = 0 then
  begin
    Add_SI( anSI, DefaultCmdFieldID, MAXINT );
    result := true;
  end;
end;


function TfrmBoolExpr.AddToEnd( const Idx : integer; anSI : TSearchItem ) : boolean;
begin
  result := false;
  if Idx = flowCmd.ControlList.Count - 1 then
  begin
    Add_ToEnd( anSI, DefaultCmdFieldID );
    result := true;
  end;
end;

procedure TfrmBoolExpr.actCmdSI_L_AddExecute( Sender : TObject );
var
  Idx , NotFactor: Integer;
begin
  try

    {$IFDEF Numbered_SI_Items}
    inc( SI_CREATE_COUNT );
    {$ENDIF}

    if not CheckGlobCopySI then
      exit;

    if HadNone( GlobCopySI ) then //its ok for it to be nil
      exit;

    NotFactor := 0;

    Idx := flowCmd.GetControlIndex( fCmdPointer );

    if Is_NOT_Item( Idx ) then //adjusts insert point if the item is "NOT"'ed
      inc( NotFactor );

    case flowCmd.ControlList[ Idx ].Control.HelpContext of
      LT_C : Add_OP_SI( GlobCopySI, DefaultCmdFieldID, Idx - NotFactor ); //")"
      else Add_SI_OP( GlobCopySI, DefaultCmdFieldID, Idx - NotFactor );
    end;

    GetEnclosingParens( fCmdPointer );

  finally
    TfrmSearch( Parent ).IsCopying( false );
    GlobCopySI := nil; //done with it, remove reference
  end;

end;

procedure TfrmBoolExpr.actCmdSelectLeftExecute( Sender : TObject );
var
  Idx : Integer;
begin
  Idx := flowCmd.GetControlIndex( fCmdPointer );
  if flowCmd.ControlList[ Idx ].Index = 0 then
    exit;
  MasterClick( flowCmd.ControlList[ Idx - 1 ].Control  );
end;

procedure TfrmBoolExpr.actCmdSelectRightExecute( Sender : TObject );
var
  Idx : Integer;
begin
  Idx := flowCmd.GetControlIndex( fCmdPointer );
  if flowCmd.ControlList[ Idx ].Index = flowCmd.ControlList.Count - 1 then
    exit;
  MasterClick( flowCmd.ControlList[ Idx + 1 ].Control  );
end;

function TfrmBoolExpr.CheckGlobCopySI : boolean;
var
  Idx : integer;
begin
  result := true;
  if assigned( GlobCopySI ) then
  begin
    Idx := ValidFields.IndexOf( GlobCopySI.siDisplayCaption );
    if Idx < 0 then
    begin
      showmessage(
              format( cmsgBEInvalidField, [
                         GlobCopySI.siDisplayCaption,
                         strif( HelpContext = 0, cNameItem_CommandPlur, cNameItem_CommandLinePlur )
                                        ]
                     )
                 );
      GlobCopySI := nil;
      result := false;
    end;
  end;
end;

procedure TfrmBoolExpr.actCmdSI_R_AddExecute( Sender : TObject );
var
  Idx : Integer;
begin

  try

    {$IFDEF Numbered_SI_Items}
    inc( SI_CREATE_COUNT );
    {$ENDIF}

    if not CheckGlobCopySI then
      exit;

    if HadNone( GlobCopySI ) then //its ok for it to be nil
      exit;

    Idx := flowCmd.GetControlIndex( fCmdPointer );

    if not AddToEnd( Idx, GlobCopySI ) then //its ok if GlobCopySI is nil
    case flowCmd.ControlList[ Idx ].Control.HelpContext of
      LT_O : Add_SI_OP( nil, DefaultCmdFieldID, Idx + 1 ); // "("
      else Add_OP_SI( GlobCopySI, DefaultCmdFieldID, Idx + 1 );
    end;

    GetEnclosingParens( fCmdPointer );

  finally
    TfrmSearch( Parent ).IsCopying( false );
    GlobCopySI := nil; //done with it, remove reference
  end;

end;

function TfrmBoolExpr.IsSingle_SI : integer;
var
  Idx, Count, i : Integer;
begin
  result := -1;
  Idx := -1;
  Count := 0;
  for i := 0 to flowCmd.ControlList.Count - 1 do
  begin
    if flowCmd.ControlList[ i ].Control.HelpContext = LT_SI then
    begin
      inc( Count );
      if Count > 1 then
        break //not single
      else Idx := i;//point to 1st one's position
    end;
  end;
  if Count = 1 then
    result := Idx;
end;

function TfrmBoolExpr.IsLast_SI( const Idx : integer ) : boolean;
var
  i : Integer;
begin
  result := false;
  for i := flowCmd.ControlList.Count - 1 downto 0 do
  begin
    if flowCmd.ControlList[ i ].Control.HelpContext = LT_SI then
    begin
      if i = Idx then
        result := true;
      break;
    end;
  end;
end;

function TfrmBoolExpr.Is_NOT_Item( const Idx : integer ) : boolean;
begin
  result := false;
  if Idx = 0 then
    exit;
  if flowCmd.ControlList[ Idx - 1 ].Control.HelpContext = LT_Not then
    result := true;
end;

function TfrmBoolExpr.Is_OpenParen_Bookended_By_NOTs( const Idx : integer ) : boolean;
begin
  result := false;
  if Idx = 0 then
    exit;
  result := ( flowCmd.ControlList[ Idx - 1 ].Control.HelpContext = LT_Not )
            and ( flowCmd.ControlList[ Idx + 1 ].Control.HelpContext = LT_Not );
end;

function TfrmBoolExpr.Get_Non_OP_ToLeft( Idx : integer ) : integer;
var
  i : Integer;
begin
  result := IsSingle_SI;
  if result > -1 then
    exit;

  if Idx < 0 then
    Idx := flowCmd.ControlList.Count - 1;

  for i := Idx downto 0 do
  begin
    if flowCmd.ControlList[ i ].Control.HelpContext = LT_OP then
      continue;
    result := i;
    break;
  end;
end;

function TfrmBoolExpr.Get_Non_OP_ToRight( const Idx : integer ) : integer;
var
  i : Integer;
begin
  result := IsSingle_SI;
  if result > -1 then
    exit;
  for i := Idx to flowCmd.ControlList.Count - 1 do
  begin
    if flowCmd.ControlList[ i ].Control.HelpContext = LT_OP then
      continue;
    result := i;
    break;
  end;
end;

function TfrmBoolExpr.NextSI_IsCloseParen( const Idx : integer ) : boolean;
begin
  result := false;
  if Idx < flowCmd.ControlList.Count - 1 then
  begin
    if flowCmd.ControlList[ Idx + 1 ].Control.HelpContext = LT_C then //")"
      result := true;
  end;
end;

procedure TfrmBoolExpr.actCmd_DeleteExecute( Sender : TObject );
var
  Idx , DelCount, NextSIIdx, NotFactor: integer;
  LastB4Paren : Boolean;
begin

  if not assigned( fCmdPointer ) then //necessary? button enabling probably prevents this
    exit;

  Idx := flowCmd.GetControlIndex( fCmdPointer );
  DelCount := 0;
  NotFactor := 0;
  LastB4Paren := false;

  case fCmdPointer.HelpContext of

    LT_SI :
      begin

        if NextSI_IsCloseParen( Idx ) then //if next item is ")", it is technically last and must delete to Left
          LastB4Paren := true;

        if Is_NOT_Item( Idx ) then
//adjusts delcount if the item is "NOT"'ed
          inc( NotFactor );

        if LastB4Paren or IsLast_SI( Idx ) then
        begin

          if IsSingle_SI < 0 then //more than one true search item remains
          begin
            NextSIIdx := Get_Non_OP_ToLeft( Idx - 1 - NotFactor );
            DelCount := Idx - NextSIIdx;
            Idx := NextSIIdx + 1;
          end
          else
          begin //only one searchItem remains
            Idx := Idx - NotFactor;
            DelCount := 1 + NotFactor;
          end;

        end
        else
        begin
          DelCount := Get_Non_OP_ToRight( Idx + 1 ) - Idx;
          Dec( Idx, NotFactor );
          Inc( DelCount, NotFactor );
        end;

        DeleteItems( Idx, DelCount );
        ClearEmptyParens( Idx );

      end;

    LT_Not :
      begin
        DeleteItems( Idx, 1 );
        SetCmdPointer( flowCmd.ControlList[ Idx ].Control );
      end;

    LT_O : //"("
      begin
        NextSIIdx := GetParen_Right( flowCmd.ControlList[ Idx ].Control );
        DeleteParens( Idx, NextSIIdx );
      end;

    LT_C : //")"
      begin
        NextSIIdx := GetParenSibling_Left( flowCmd.ControlList[ Idx ].Control );
        DeleteParens( NextSIIdx, Idx );
      end;

    else raise EErrorDevelopment.create( 'TfrmBoolExpr.actCmd_DeleteExecute: Object may not be deleted, OPERATOR (OP)sent in!' );

  end; //case

  ReFocus( Idx );

end;

procedure TfrmBoolExpr.ReFocus( Idx : integer );
begin
  if Idx > flowCmd.ControlList.Count - 1 then
    Idx := Get_Non_OP_ToLeft( -1 )
  else Idx := Get_Non_OP_ToRight( Idx );

  if Idx < 0 then
    SetCmdPointer( nil )
  else
    SetCmdPointer( flowCmd.ControlList[ Idx ].Control );

end;


procedure TfrmBoolExpr.DeleteParens( const OrigIdx, MatchIdx : integer );
var
  Is_NOT_ed : Boolean;
begin

  Delete_Control( flowCmd.ControlList[ MatchIdx ].Control );

//need to check BEFORE we delete things!
  Is_NOT_ed := Is_NOT_Item( OrigIdx );

  Delete_Control( flowCmd.ControlList[ OrigIdx ].Control );

  if Is_NOT_ed then
    Delete_Control( flowCmd.ControlList[ OrigIdx - 1 ].Control );

end;

procedure TfrmBoolExpr.ClearEmptyParens( Idx : integer );
var
  DelCount : Integer;
  IsLast : Boolean;
begin
//if a delete result in something like:  NOT ( (  ) ), the whole "empty" structure goes too

  while IsEmptyParens( Idx ) do //  "(" and ")" with nothing in between
  begin
    DelCount := 2;//at least these two : "(" and ")" : must be deleted

    IsLast := ( Idx = flowCmd.ControlList.Count - 1 )//must check while Idx points to ")"
              or NextSI_IsCloseParen( Idx );//a ")" to the right is a logical "last"

    if not IsLast and ( flowCmd.ControlList[ Idx + 1 ].Control.HelpContext = LT_OP ) then
 //if following the ) is an or/xor/and, it goes too
      inc( DelCount );

    Idx := Idx - 1;//now set idx to the "("
    if Is_NOT_Item( Idx ) then
    begin //if the "(" has a NOT in front, get rid of that too
      dec( Idx );
      inc( DelCount );
    end;

    if IsLast and ( Idx > 0 ) and ( flowCmd.ControlList[ Idx - 1 ].Control.HelpContext = LT_OP ) then
    begin //was no or/xor/and following the ")", so is there an or/xor/and ==BEFORE== the "("
      dec( Idx );
      inc( DelCount );
    end;

    DeleteItems( Idx, DelCount );

  end;

end;

procedure TfrmBoolExpr.DeleteItems( const Idx, DelCount : integer );
var
  i : Integer;
begin
  for i := 1 to DelCount do
    Delete_Control( flowCmd.ControlList[ Idx ].Control );
end;

function TfrmBoolExpr.IsEmptyParens( const Idx : integer ) : boolean;
var
  LabelType : THelpContext;
begin

  result := false;
  if Idx > flowCmd.ControlList.Count - 1 then
    exit;

  LabelType := flowCmd.ControlList[ Idx ].Control.HelpContext;
  if LabelType = LT_C then
    if flowCmd.ControlList[ Idx - 1 ].Control.HelpContext = LT_O then
      result := true;

end;


procedure TfrmBoolExpr.SetCursors( Cur : TCursor; const Blink : boolean; myIdx : integer );
var
  i : Integer;
begin

  Tmr.Enabled := Blink;
  ProcessCmdActions;

  scbCmd.Cursor := Cur;

  for i := 0 to flowCmd.ControlList.Count - 1 do
  begin
    if i = myIdx then
      Continue;
    if flowCmd.ControlList[ i ].Control.HelpContext = LT_SI then
    begin
      flowCmd.ControlList[ i ].Control.Cursor := Cur;
      if cur <> crDefault then
        flowCmd.ControlList[ i ].Control.Color := cColExhangeColor
      else flowCmd.ControlList[ i ].Control.Color := cColNonSelectedSI;

    end;
  end;
end;

procedure TfrmBoolExpr.SetDefaultCmdFieldID( AValue : TCmdFieldID );
begin
  if fDefaultCmdFieldID = AValue then Exit;
  fDefaultCmdFieldID := AValue;
end;

procedure TfrmBoolExpr.actCmd_ExchangeExecute( Sender : TObject );
begin
  fSelectSIMode := true;
  fSelectSIIdx := flowCmd.GetControlIndex( fCmdPointer );
  SetCursors( crHandPoint, true, fSelectSIIdx );
end;

function TfrmBoolExpr.aCTRL( const Idx : integer ) : TControl;
begin
  result := flowCmd.ControlList[ Idx ].Control;
end;


procedure TfrmBoolExpr.actCmd_NOT_ToggleExecute( Sender : TObject );
var
  Idx : integer;
  DeleteIt : Boolean;
begin

  {$IFDEF Numbered_SI_Items}
  inc( SI_CREATE_COUNT );
  {$ENDIF}

  DeleteIt := false;

  Idx := flowCmd.GetControlIndex( fCmdPointer );

  if Idx > 0 then
    DeleteIt := aCTRL( Idx - 1 ).HelpContext = LT_Not;

  if DeleteIt then
    Delete_Control( Idx - 1 )
  else Add_OP( nil, sotNOT, Idx );

end;

procedure TfrmBoolExpr.actCmd_Paren_AddExecute( Sender : TObject );
var
  Idx : Integer;
begin
  {$IFDEF Numbered_SI_Items}
  inc( SI_CREATE_COUNT );
  {$ENDIF}
  Idx := flowCmd.GetControlIndex( fCmdPointer );
  Add_OP( nil, sotClose, Idx + 1 );
  Add_OP( nil, sotOpen, Idx );
  GetEnclosingParens( fCmdPointer );
end;

function TfrmBoolExpr.Paren_Move_Invalid( const Cond : boolean; shp : TShape ) : boolean;
begin
  result := false;
  if Cond then
  begin
    shp.Visible := true;
    result := true;
  end;
end;

procedure TfrmBoolExpr.actCmd_Paren_L_MoveExecute( Sender : TObject );
var
  i , Idx , NewIdx, ParenCheck, ParenSum: Integer;
  LabelType : THelpContext;
begin

  Idx := flowCmd.GetControlIndex( fCmdPointer );

  shpPRight.Visible := false;

  case flowCmd.ControlList[ Idx ].Control.HelpContext of

    LT_C : //==> ")"
      begin

        NewIdx := -1;
//start at postion to the right ignoring the immediately preceding, then search for valid landing position
        for i := Idx - 2 downto 0 do
        begin
          if flowCmd.ControlList[ i ].Control.HelpContext = LT_SI then
          begin
            NewIdx := i;
            break;
          end;
        end;
        //shpPLeft.Visible := false;
        //shpPRight.Visible := false;

        if Paren_Move_Invalid( NewIdx < 0, shpPLeft ) then
          exit;

        ParenCheck := MAXINT;
        ParenSum := 0;
//routine checks that move is in this paren "phrase"
//eg. ( item or item) or ( item or item ), can't allow this result: ( item or item )) or ( item or item   , etc.
        for i := 0 to Idx - 1 do
        begin
          case flowCmd.ControlList[ i ].Control.HelpContext of
            LT_O :
              begin
                inc( ParenSum );
                if ParenSum = 1 then
                  ParenCheck := i;
              end;
            LT_C :
              begin
                dec( ParenSum );
                if ParenSum = 0 then
                  ParenCheck := MAXINT;
              end;
          end;

        end;

        if Paren_Move_Invalid( NewIdx <= ParenCheck, shpPLeft ) then
          exit;

        flowCmd.ControlList[ Idx ].Index := NewIdx + 1;
        HighlightMatchingParenToLeft( fCmdPointer );

      end;

    LT_O : //==> "("
      begin

        ParenCheck := 0;
        NewIdx := -1;

//Open Paren can be in this position:  NOT ( NOT searchitem  which results in:  NOT NOT ( searchitem
//this is not good so in this case, rare, a NOT is sacrificed
        if Is_OpenParen_Bookended_By_NOTs( Idx ) then
          inc( ParenCheck );

//start to the next position left of the ( (or the NOT if one) and search for valid landing site
        for i := Idx - 1 - ParenCheck downto 0 do
        begin
          LabelType := flowCmd.ControlList[ i ].Control.HelpContext;
          if ( LabelType = LT_OP ) or ( LabelType = LT_C ) then
            continue;
          NewIdx := i;
          break;
        end;

        if Paren_Move_Invalid( NewIdx = -1, shpPLeft ) then
          exit;

        flowCmd.ControlList[ Idx ].Index := NewIdx;
        if ParenCheck > 0 then
          DeleteItems( Idx, 1 );

        HighlightMatchingParenToRight( fCmdPointer );

      end;

  end;

end;

procedure TfrmBoolExpr.actCmd_Paren_R_MoveExecute( Sender : TObject );
var
  i , Idx , NewIdx, ParenCheck, ParenSum, NOT_MustGo: Integer;
  LabelType : THelpContext;
begin

  Idx := flowCmd.GetControlIndex( fCmdPointer );
  shpPLeft.Visible := false;

  case flowCmd.ControlList[ Idx ].Control.HelpContext of

    LT_O : //==> "("
      begin

//Open Paren can be in this position:  NOT ( NOT searchitem  which results in:  NOT NOT ( searchitem
//this is not good so in this case, rare, a "NOT" is sacrificed
        NOT_MustGo := 0;
        NewIdx := -1;

        if Is_OpenParen_Bookended_By_NOTs( Idx ) then
          inc( NOT_MustGo );

        NewIdx := -1;
//start to the right of "(" ignoring immediate following item and a NOT if there is one
        for i := Idx + 2 + NOT_MustGo to flowCmd.ControlList.Count - 1 do
        begin
          LabelType := flowCmd.ControlList[ i ].Control.HelpContext;
          if ( LabelType = LT_OP ) or ( LabelType = LT_C ) then
            continue;
          NewIdx := i;
          break;
        end;

        if Paren_Move_Invalid( NewIdx = -1, shpPRight ) then
          exit;

        ParenCheck := -1;
        ParenSum := 0;
//routine checks that move is in this paren "phrase"
//eg. ( item or item) or ( item or item ), can't allow this result: item or item ) or (( item or item )    , etc.
        for i := flowCmd.ControlList.Count - 1 downto Idx + 1 do
        begin
          case flowCmd.ControlList[ i ].Control.HelpContext of
            LT_C :
              begin
                inc( ParenSum );
                if ParenSum = 1 then
                  ParenCheck := i;
              end;
            LT_O :
              begin
                dec( ParenSum );
                if ParenSum = 0 then
                  ParenCheck := -1;
              end;
          end;

        end;

        if Paren_Move_Invalid( NewIdx >= ParenCheck, shpPRight ) then
          exit;

        flowCmd.ControlList[ Idx ].Index := NewIdx - 1;
        if NOT_MustGo > 0 then
          DeleteItems( Idx, 1 );

        HighlightMatchingParenToRight( fCmdPointer );

      end;

    LT_C : //==> ")"
      begin

        NewIdx := -1;

//start to the next position right of the ")" and get next valid landing point
//yes, I tried to generalize this but it appears that " in " won't work with open array of integer param, sadly.
        for i := Idx + 1 to flowCmd.ControlList.Count - 1 do
        begin
          LabelType := flowCmd.ControlList[ i ].Control.HelpContext;
          if LabelType in [ LT_OP, LT_O, LT_Not ] then
            continue;
          NewIdx := i;
          break;
        end;

        if Paren_Move_Invalid( NewIdx = -1, shpPRight ) then
          exit;

        flowCmd.ControlList[ Idx ].Index := NewIdx;

        HighlightMatchingParenToLeft( fCmdPointer );

      end;

  end;

end;

procedure TfrmBoolExpr.btnCmdCopyClick( Sender : TObject );
var
  Idx : Integer;
begin

  if assigned( GlobCopySI ) then
  begin
    showmessage( cmsgBEClipBoardFull );
    exit;
  end;

  if fListSearchItems.Count = 0 then
  begin
    Showmessage( cmsgBENoItem );
    exit;
  end;

  Idx := flowCmd.GetControlIndex( fCmdPointer );
  if Idx < 0 then
    exit;

  GlobCopySI := TSearchItem( fListSearchItems[ flowCmd.ControlList[ Idx ].Control.Tag ] );

  if not GlobCopySI.Valid then
  begin
    GlobCopySI := nil;
    showmessage( cmsgBEInvalidItem );
    exit;
  end;

  TfrmSearch( Parent ).IsCopying( true );

end;

//procedure TfrmBoolExpr.DebugExpression;
//var
//  Str : String;
//  i : Integer;
//begin
//  Str := '';
//  for i := 0 to flowCmd.ControlList.Count - 1 do
//  begin
//    Str := Str + inttostr( i ) + ' : ' + flowCmd.ControlList[ i ].Control.Caption + LineEnding;
//  end;
//  MsgDlgMessage('Here', Str );
//  MsgDlgInfo( self );
//
//end;

procedure TfrmBoolExpr.cbCmdCompactViewChange( Sender : TObject );
var
  i : Integer;
  Cntrl : TControl;
  SI : TSearchItem;
begin
  for i := 0 to flowCmd.ControlList.Count - 1 do
  begin
    Cntrl := flowCmd.ControlList[ i ].Control;
    SI := TSearchItem( fListSearchItems[ Cntrl.Tag ] );
    if SI.SearchOperatorType <> sotNone then //it's or/xor/and or NOT or "(" or ")"
      continue;
    UpDateLabelCaption_SI( TLabel( Cntrl ), SI );
  end;
end;

procedure TfrmBoolExpr.cbCmdUseChange( Sender : TObject );
var
  i, aTag : Integer;
begin
  for i := 0 to ControlCount - 1 do
  begin
    aTag := TControl( Controls[ i ] ).Tag;
    case aTag of
      1 : TControl( Controls[ i ] ).Visible := cbCmdUse.Checked;
      2 : TControl( Controls[ i ] ).Visible := false;
    end;
  end;
end;

procedure TfrmBoolExpr.ExchangeSIs( aControl : TControl );
var
  Idx : integer;
begin

  Idx := flowCmd.GetControlIndex( aControl );
  if ( flowCmd.ControlList[ Idx ].Control.HelpContext <> LT_SI )
     or ( Idx = fSelectSIIdx ) then
    exit;

  flowCmd.ControlList[ fSelectSIIdx ].Index := Idx;

  flowCmd.ControlList[ IntIf( fSelectSIIdx > Idx, Idx + 1, Idx - 1 ) ].Index := fSelectSIIdx;

  SetCmdPointer( flowCmd.ControlList[ Idx ].Control );

  fSelectSIMode := false;

  SetCursors( crDefault, false, Idx );

end;


procedure TfrmBoolExpr.MasterClick( Sender : TObject );
var
  Idx: integer;
begin

  if fSelectSIMode then
  begin
    ExchangeSIs( TControl( Sender ) );
    exit;
  end;

  Idx := flowCmd.GetControlIndex( TControl( Sender ) );

  SetCmdPointer( aCTRL( Idx ) );

end;

function TfrmBoolExpr.SetCmdPointer( aControl : TControl ) : boolean;
var
  SI : TSearchItem;
begin

  result := true;
  if assigned( fCmdPointer ) then
    fCmdPointer.Color := GetInactiveColor( fCmdPointer );// cColNonSelectedSI;//clBlack;

  fCmdPointer := aControl;
  ProcessCmdActions;
  TfrmSearch( Parent ).SetBEFocus( HelpContext );

  if not assigned( fCmdPointer ) then
  begin
    result := false;
    exit;
  end;

  fCmdPointer.Color := cColSelected;

  if fCmdPointer <> fLastCmdPointer then
    TfrmSearch( Parent ).HideEditor;

  fLastCmdPointer := fCmdPointer;

  DetermineParenHighlighting( fCmdPointer );

  SI := TSearchItem( fListSearchItems[ fCmdPointer.Tag ] );

  case fCmdPointer.HelpContext of
                                       //use SI.Valid or  not SI.IsInvalid??
    LT_SI : TfrmSearch( Parent ).SetFieldVisibility( true, SI.Valid, Self, SI.siDisplayCaption );
    else TfrmSearch( Parent ).SetFieldVisibility( false, true, Self );
  end;

  case fCmdPointer.HelpContext of
    LT_OP, LT_SI :
        TfrmSearch( Parent ).Link_SI_BoolExpr( SI, Self );
  end;

end;


procedure TfrmBoolExpr.DetermineParenHighlighting( aControl : TControl );
begin
  case aControl.HelpContext of

    LT_O : //find next Paren to the right
      begin
        aControl.Font.Color := cColNonSelectedParen;
        HighlightMatchingParenToRight( aControl );
      end;

    LT_C : //find mate to left
      begin
        aControl.Font.Color := cColNonSelectedParen;
        HighlightMatchingParenToLeft( aControl );
      end;

    else GetEnclosingParens( aControl );

  end;
end;


procedure TfrmBoolExpr.HighlightMatchingParenToRight( aControl : TControl );
begin
  aCTRL( GetParen_Right( aControl ) ).Font.Color := cColMatchingParen;
end;

procedure TfrmBoolExpr.HighlightMatchingParenToLeft( aControl : TControl );
begin
  aCTRL( GetParenSibling_Left( aControl ) ).Font.Color := cColMatchingParen;
end;

procedure TfrmBoolExpr.GetEnclosingParens( aControl : TControl );
var
  Idx : Integer;
begin
  Idx := GetParenSibling_Left( aControl );
  if Idx < 0 then
    exit;
  aCTRL( Idx ).Font.Color := cColMatchingParen;
  Idx := GetParen_Right( aCTRL( Idx ) );
  aCTRL( Idx ).Font.Color := cColMatchingParen;
end;

procedure TfrmBoolExpr.ClearParensColor( Idx : integer = - 1 );
var
  i : Integer;
  CTRL : TControl;
begin
  for i := 0 to flowCmd.ControlList.Count - 1 do
  begin
    if i = Idx then
      continue;
    CTRL := aCTRL( i );
    if CTRL.HelpContext in [ LT_O, LT_C ] then
    begin
      CTRL.Color := cColNonSelectedOP;
      CTRL.Font.Color := cColNonSelectedParen;
    end;

  end;
end;

function TfrmBoolExpr.GetParenSibling_Left( aControl : TControl ) : integer;
var
  ChildParens, Idx, i: Integer;
  LabelType : THelpContext;
begin
  result := -1;
  ChildParens := 0;
  Idx := flowCmd.GetControlIndex( aControl );
  ClearParensColor( Idx );

  for i := Idx - 1 downto 0 do
  begin
    LabelType := aCTRL( i ).HelpContext;
    if LabelType < LT_O then
      continue;
    if ( ChildParens = 0 ) and  ( LabelType = LT_O ) then
    begin
      result := i;
      break;
    end;
    if LabelType = LT_C then
      inc( ChildParens );
    if LabelType = LT_O then
      dec( ChildParens );
  end;
end;

function TfrmBoolExpr.GetParen_Right( aControl : TControl ) : integer;
var
  ChildParens , Idx, i: Integer;
  LabelType : THelpContext;
begin
  result := -1;
  ChildParens := 0;
  Idx := flowCmd.GetControlIndex( aControl );
  ClearParensColor( Idx );

  for i := Idx + 1 to flowCmd.ControlList.Count - 1 do
  begin
    LabelType := aCTRL( i ).HelpContext;
    if LabelType < LT_O then
      continue;
    if ( ChildParens = 0 ) and  ( LabelType = LT_C ) then
    begin
      result := i;
      break;
    end;
    if LabelType = LT_O then
      inc( ChildParens );
    if LabelType = LT_C then
      dec( ChildParens );
  end;
end;

procedure TfrmBoolExpr.SetEditMode( const AValue : integer );
begin
  if fEditMode = AValue then Exit;
  fEditMode := AValue;
end;

procedure TfrmBoolExpr.SetValidFields( AValue : TStringlist );
begin
  if fValidFields = AValue then Exit;
  fValidFields := AValue;
end;


procedure TfrmBoolExpr.UpDateLabelCaption_SI( LB : TLabel; SI : TSearchItem );
var
  ValStr , FormatStr, Cond: String;
begin

  if ( SI.FieldType in [ ftString_Key, ftString, ftText, ftList ] ) and
     not ( SI.Condition in [ coRegEx, coIsEmpty ] )
  then
    FormatStr := DefNewSIString
  else FormatStr := DefNewSI;

  Cond := ConditionToStr_Display( SI.Condition );
  ValStr := SI.SearchValue_ForDisplay;
  //ValStr := SI.SearchValue;
  if SI.Condition = coIsEmpty then
    ValStr := '';

  if cbCmdCompactView.Checked then
  begin
    if length( ValStr ) > 15 then
      ValStr := copy( ValStr, 1, 15 ) + '...';
    {$IFDEF Numbered_SI_Items}
    LB.Caption := format( FormatStr, [ SI.siDisplayAbbrev, Cond, ValStr + inttostr( SI_CREATE_COUNT ) ] );
    {$ELSE}
    LB.Caption := format( FormatStr, [ SI.siDisplayAbbrev, Cond, ValStr ] );
    {$ENDIF}
  end
  else
  {$IFDEF Numbered_SI_Items}
  LB.Caption := format( FormatStr, [ SI.siDisplayCaption, Cond, ValStr + inttostr( SI_CREATE_COUNT ) ] );
  {$ELSE}
  LB.Caption := format( FormatStr, [ SI.siDisplayCaption, Cond, ValStr ] );
  {$ENDIF}
end;

procedure TfrmBoolExpr.UpdateCurrCaption;
begin

  case fCmdPointer.HelpContext of
    LT_OP :
      fCmdPointer.Caption := format( DefNewOP,
           [ TSearchItem( fListSearchItems[ fCmdPointer.Tag ] ).GetSearchOperatorCaption ]
                                );
    LT_SI : UpDateLabelCaption_SI( TLabel( fCmdPointer ), TSearchItem( fListSearchItems[ fCmdPointer.Tag ] ) );
  end;

end;


procedure TfrmBoolExpr.FocusOn;
begin
  pnlCmd.Color := $E58218;// light blue
  scbCmd.Color := clBlack;
//  flowCmd.Color := scbCmd.Color;
end;

procedure TfrmBoolExpr.FocusOff;
begin
  fLastCmdPointer := nil;
  pnlCmd.Color := $303030; // DarkGray;
  scbCmd.Color := $303030;//clGray;
end;

procedure TfrmBoolExpr.TransferSearchCriteria( destBS : TBaseSearch );
var
  i : Integer;
  Cntrl : TControl;
  SI : TSearchItem;
begin

  DestBS.IsUsed := cbCmdUse.Checked;

  for i := 0 to flowCmd.ControlList.Count - 1 do
  begin
    Cntrl := flowCmd.ControlList[ i ].Control;
    SI := TSearchItem( fListSearchItems[ Cntrl.Tag ] );
    if SI.SearchOperatorType <> sotNone then
      DestBS.AddOperator( SI.SearchOperatorType )
    else
      DestBS.AddSearchItem( SI.CmdFieldID_Idx, SI.SearchValue, SI.Condition, SI.CaseSensitive );
  end;

end;

function TfrmBoolExpr.ReadSearchCriteria( fromBS : TBaseSearch ) : boolean;
var
  i : Integer;
  SI : TSearchItem;
begin
  gSearchInfo := TStringlist.Create;
  try
    result := true;
    for i := 0 to fromBS.SIList.Count - 1 do
    begin
      SI := TSearchItem( fromBS.SIList[ i ] );
      if SI.SearchOperatorType <> sotNone then
        Add_OP( SI, SI.SearchOperatorType, MAXINT )
      else
        if Add_SI( SI, TCmdFieldID( SI.CmdFieldID_Idx ), MAXINT ) <> 0 then
          result := false;
    end;
    if gSearchInfo.Text <> '' then
    begin
      MsgDlgMessage( ccapusSearchDetailProblem, gSearchInfo.Text );
      MsgDlgInfo( TForm( Parent ) );
    end;
  finally
    gSearchInfo.free;
  end;
end;

function TfrmBoolExpr.HasItems : boolean;
begin
  result := cbCmdUse.Checked
            and ( flowCmd.ControlList.Count > 0 );
end;

function TfrmBoolExpr.ReInit( pCmdFieldID : TCmdFieldID; pFieldType : TufFieldType) : boolean;
var
  SI : TSearchItem;
begin
  result := true;
  SI := TSearchItem( fListSearchItems[ fCmdPointer.Tag ] );
  SI.SearchItem_ReInit( pCmdFieldID, pFieldType );
  DefaultCmdFieldID := pCmdFieldID;
end;

procedure TfrmBoolExpr.ReSelect;
begin
  MasterClick( fCmdPointer );
end;


procedure TfrmBoolExpr.MakeDisplayLabel( LB : TLabel; SI : TSearchItem );
begin

  if not assigned( LB ) then
    raise EErrorDevelopment.create( 'TfrmBoolExpr.MakeDisplayLabel: LB unassigned, expected valid TLabel' );

  LB.Tag := fListSearchItems.Add( SI );

  LB.Transparent := false;
  LB.Color := cColNonSelectedOP;
  LB.Font.Color := clCream;
  LB.OnClick := @MasterClick;
//===> DO NOT set parent here <====  bad, bad mojo
  //LB.Parent := flowCmd;

  if SI.SearchOperatorType =  sotNone then // so it's not an operator it's a searchItem
  begin
    SI := TSearchItem( fListSearchItems[ LB.Tag ] );
    UpDateLabelCaption_SI( LB, SI );
    SetCmdPointer( LB );
    exit;
  end;

  if LB.HelpContext in [ LT_O, LT_C ] then
  begin
    LB.Font.Style := [ fsBold ];
    LB.Font.Color := cColNonSelectedParen;
  end;

  {$IFDEF Numbered_SI_Items}
  LB.Caption := format( DefNewOP, [ SI.GetSearchOperatorCaption + inttostr( SI_CREATE_COUNT ) ] );
  {$ELSE}
  LB.Caption := format( DefNewOP, [ SI.GetSearchOperatorCaption ] );
  {$ENDIF}

end;

procedure TfrmBoolExpr.AddToFlowPanel( const Flow : TFlowPanel; const LB : TLabel; const Idx : integer );
var
  FPC : TFlowPanelControl;//TCollectionItem;
begin

  FPC := TFlowPanelControl( Flow.ControlList.Add );
  LB.Parent := Flow; //parent MUST be set here, otherwise ==> bad juju
  FPC.Control := LB;

  if Idx < MAXINT then //MAXINT is flag for a simple add, otherwise an insert
// ===> DON'T <===== set index until ==AFTER== Control is assigned, guarantee you won't like the results!
    FPC.Index := Idx;

end;

procedure TfrmBoolExpr.Delete_Control( const Idx : integer );
begin
  Delete_Control( flowCmd.ControlList[ Idx ].Control )
end;

procedure TfrmBoolExpr.Delete_Control( const aControl : TControl );
begin
  flowCmd.RemoveControl( aControl );
//don't need to free SI associated with Control, that SI is ignored from now on,
//and all temporary SI's are freed on form close / destroy
end;

procedure TfrmBoolExpr.Add_Control( LB : TLabel; var SI : TSearchItem; const Idx : integer );
begin
  MakeDisplayLabel( LB, SI );
  AddToFlowPanel( flowCmd, LB, Idx );
  ProcessCmdActions;
end;


procedure TfrmBoolExpr.Add_OP_SI( anSI : TSearchItem; pFID : TCmdFieldID; const Idx : integer );
begin
//always OR SearchItem (ie., or keyword="test" )
  Add_SI( anSI, pFID, Idx );
  Add_OP( nil, sotOR, Idx );
end;

procedure TfrmBoolExpr.Add_SI_OP( anSI : TSearchItem; pFID : TCmdFieldID; const Idx : integer );
begin
//always SearchItem OR (ie., keyword="test" or )
  Add_OP( nil, sotOR, Idx );
  Add_SI( anSI, pFID, Idx );
end;

procedure TfrmBoolExpr.Add_ToEnd( anSI : TSearchItem; pFID : TCmdFieldID );
begin
//always added to end (MAXINT=simple add); so an "OR SearchItem"
  Add_OP( nil, sotOR, MAXINT );
  Add_SI( anSI, pFID, MAXINT );
end;


function TfrmBoolExpr.Add_SI( anSI : TSearchItem; pFID : TCmdFieldID; const Idx : integer ) : smallint;
var
  LB : TLabel;
  SI : TSearchItem;
begin

  result := 0;

  if assigned( anSI ) then
  begin
    SI := TSearchItem.Create( nil, anSI.FieldType );
    if not anSI.Valid then
      SI.SetOldFieldName( anSI.OrigDisplayCaption );
    SI.SearchItem_Init( anSI.SearchValue,
                        anSI.Condition,
                        anSI.CaseSensitive,
                        Get_Cmd_Field_UniqueID( anSI.CmdFieldID_Idx ) );
    if SI.IsInvalid then
      result := 1;
  end
  else
  begin
    SI := TSearchItem.Create( nil, Get_Cmd_Field_FieldType( pFID ) );
    SI.SearchItem_Init( '', SI.GetNewSearchConditionDefault, false, Get_Cmd_Field_UniqueID( pFID ) );
    SI.SearchValue := SI.GetNewSearchValueDefault;
  end;

  LB := TLabel.Create( self );
  LB.Font.Color := clcream;
  LB.HelpContext := LT_SI;
  LB.Hint := cfseExpressionHint;
  Add_Control( LB, SI, Idx );

end;

procedure TfrmBoolExpr.Add_OP( anSI : TSearchItem; pSOT : TSearchOperatorType; const Idx : integer );
var
  LB : TLabel;
  SI : TSearchItem;
begin

  if assigned( anSI ) then
    SI := TSearchItem.Create( nil, anSI.FieldType )
  else
    SI := TSearchItem.Create( nil, GetFieldTypeforOperator );

  SI.Operator_Init( pSOT );

  LB := TLabel.Create( self );
  LB.Font.Color := clcream;
  LB.Hint := cfseOperatorHint;
  case pSOT of
    sotNOT : LB.HelpContext := LT_Not;
    sotAND, sotOR, sotXOR : LB.HelpContext := LT_OP;
    sotOpen : LB.HelpContext := LT_O;
    sotClose : LB.HelpContext := LT_C;
    else LB.HelpContext := LT_UNK;
  end;

  Add_Control( LB, SI, Idx );

end;


procedure TfrmBoolExpr.FormCreate(Sender : TObject);
begin
  font.size := cDefaultFontSize;
  ApplyChangeFont( Self );
  FHasShown := false;
  fListSearchItems := TList.Create;
  fValidFields := TStringlist.Create;
  fSelectSIMode := false;
  fLastCmdPointer := nil;
  fCmdPointer := nil;
  fDefaultCmdFieldID := fidIsFavorite;

  flowCmd.Hint := cfseMainHint;
  scbCmd.Hint := cfseMainHint;
  btnCmd_Paren_L_Move.Hint := cfseMoveParenHint;
  btnCmd_Paren_R_Move.Hint := cfseMoveParenHint;

  btnCmdCopy.Caption := cbtn_Copy;
  btnCmd_NOT_Toggle.Caption := cbtn_SearchNOT;
  mnifseCmdRoot.Caption := format( ccapPopMenuRootRoot, [ cfseMenuRoot, ccapPopMenuRootMenuStr ] );
  mnibeCmdCopy.Caption := cbtn_Copy;
  mnibeCmdCompact.Caption := cbCmdCompactView.Caption;
end;

procedure TfrmBoolExpr.FormKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
var
  PosPt : TPoint;
begin
  if Key = vk_escape then
  begin
    TfrmSearch( Parent ).IsCopying( false );
    GlobCopySI := nil;
    if fSelectSIMode then
    begin
      SetCursors( crDefault, false, fSelectSIIdx );
      fSelectSIMode := false;
      exit;
    end;
  end;

  if Shift = [ ssShift, ssCtrl ] then
    if key = VK_P then
    begin
      PosPt := GetPreciseControlCoords( cbCmdUse, 30, 30 );
      PopCmd.PopUp( PosPt.x, PosPt.y );
    end;

end;

procedure TfrmBoolExpr.CopyMode( const DoEnable : boolean );
begin
  btnCmdCopy.Enabled := not DoEnable;

  if DoEnable then
    pnlDoCopy.Color := clRed
  else
    pnlDoCopy.Color := clDefault;

end;

end.

{
247 740
}

