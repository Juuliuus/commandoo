unit ufrmListManager;

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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, HintFrame
  , lcltype {THis is needed for key up keyboard constants}
  , unitsharedobj, Buttons, StdCtrls, Menus, ActnList
  , unitcommands
  , uSingleInput
  , unitGlobForm
  ;

type

  { TfrmListManager }

  TfrmListManager = class(TForm)
    actCancel : TAction;
    actAdd : TAction;
    actEdit : TAction;
    actDelete : TAction;
    actConsolidate : TAction;
    actShowUsage : TAction;
    actRevert : TAction;
    actOK : TAction;
    ActionList1 : TActionList;
    btnConsolidate : TButton;
    btnRevert : TButton;
    btnEdit : TButton;
    btnCancel : TBitBtn;
    btnOK : TBitBtn;
    btnAdd : TButton;
    btnDelete : TButton;
    btnShowUsage : TButton;
    FrameHint1 : TFrameHint;
    gbManageList : TGroupBox;
    lblInstructions : TLabel;
    lbList : TListBox;
    MenuItem1 : TMenuItem;
    MenuItem2 : TMenuItem;
    MenuItem3 : TMenuItem;
    mniShowUsage : TMenuItem;
    mniRevert : TMenuItem;
    mniConsolidate : TMenuItem;
    mniAdd : TMenuItem;
    mniEdit : TMenuItem;
    mniDelete : TMenuItem;
    mniCancel : TMenuItem;
    mniOK : TMenuItem;
    PopupMenu1 : TPopupMenu;
    procedure actAddExecute(Sender : TObject);
    procedure actCancelExecute(Sender : TObject);
    procedure actConsolidateExecute(Sender : TObject);
    procedure actDeleteExecute(Sender : TObject);
    procedure actEditExecute(Sender : TObject);
    procedure actOKExecute(Sender : TObject);
    procedure actRevertExecute(Sender : TObject);
    procedure actShowUsageExecute( Sender : TObject );
    procedure FormActivate(Sender : TObject);
    procedure FormClose(Sender : TObject; var CloseAction : TCloseAction);
    procedure FormCloseQuery(Sender : TObject; var CanClose : boolean);
    procedure FormCreate(Sender : TObject);
    procedure FormShow(Sender : TObject);
    procedure lbListDblClick(Sender : TObject);
    procedure lbListKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
  private
    { private declarations }
    FCanClose : boolean;
    fClueTarget : string;
    fDisplayWordPlural : string;
    fDisplayWordSingular : string;
    FHasShown : boolean;
    FIsInitialized : boolean;
    fListObj : TCmdListObj;
    fListType : integer;
    fManagementMode : boolean;
    fConsolidateMode : boolean;

    //function DoSingleInput(ATitle : string; AValue : string; AFormMode : TSingleInputMode; DoStoreValue : boolean = false) : string;
    procedure HandleFormSettings( const TheType : TSettingsDirective );
    function IsDuplicate( const ToChk : string ) : boolean;
    function lbListInvalidChoice( RequireSingleSelection : boolean = true ) : boolean;
    procedure SetClueTarget( const AValue : string );
    procedure SetDisplayWordPlural( const AValue : string );
    procedure SetDisplayWordSingular( const AValue : string );
    procedure SetListObj( AValue : TCmdListObj );
    procedure SetListType( const AValue : integer );
    procedure SetManagementMode( const AValue : boolean );
    procedure UpdatelbList( const Idx : integer; Revertable : boolean = true );
  public
    { public declarations }
    property ListObj : TCmdListObj read fListObj write SetListObj;
    property DisplayWordSingular : string read fDisplayWordSingular write SetDisplayWordSingular;
    property DisplayWordPlural : string read fDisplayWordPlural write SetDisplayWordPlural;
    property ManagementMode : boolean read fManagementMode write SetManagementMode;
    property ClueTarget : string read fClueTarget write SetClueTarget;
    property ListType : integer read fListType write SetListType;
  end;

var
  frmListManager : TfrmListManager;


implementation

{$R *.lfm}

uses ufrmMsgDlg
  , juusgen
  , ufrmListManagerConsolidate
  , strconst_en
  , unitDBUtils
  , unitglob
  ;

const
  ccapListManagerDefaultInit = '!!Dev Error: No Word%s!!';

resourcestring
  //ccapListManagerCloseModalForm = 'Closing Modal Form Manually';
  //cmsgListManagerCloseModalForm = 'On a Modal form you need to indicate closing with "OK", "CANCEL", etc.';
  ccapListManagerMultipleItems = 'Multiple Items Selected';
  cmsgListManagerMultipleItems = 'For this choice only one item should be selected.';
  cmsgListManagerClue = 'Select %s for: %s';
  cmsgListManagerMangementMode = 'Manage Master %s';
  ccapListManagerNothingToConsolidate = 'Nothing to Consolidate to';
  cmsgListManagerNothingToConsolidate = 'There is only one item in the list so there is nothing to consolidate to.';
  cmsgListManagerListWarning =
    'Just so you know.'
    + LineEnding + LineEnding
    + 'Changes made to this list will be propagated through the Command database '
    + 'when you press OK / Done. '
    + LineEnding + LineEnding
    + 'For instance if you delete a %s here any Command using that %s will  '
    + 'have it removed. Or when you edit one (rename it) that new name will replace '
    + 'the old one in all Commands using it. '
    + LineEnding + LineEnding
    + 'In other words changes to the master %s list are synchronized to the database.  '
    + 'Keep this in mind while making changes so you are not surprised later that things '
    + 'have changed. '
    + LineEnding + LineEnding;

  ccapListManagerDone = '&O  Done';
  ccapListManagerConsolidateInfo = 'Consolidate %s';
  ccapListManagerListItemUsage = '"%s" is used by:';



{ TfrmListManager }

procedure TfrmListManager.FormShow(Sender : TObject);
begin

  if FHasShown then
    exit;

  FHasShown := true;

  FCanClose := false;
  HandleFormSettings( sdLoad );

  UpdatelbList( -1, false );

  if not fManagementMode then
    Caption := format( cmsgListManagerClue, [ fDisplayWordPlural, ClueTarget ] )
  else
  begin
    btnOK.Caption := ccapListManagerDone;
    btnCancel.Visible := false;
    lblInstructions.Caption := '';
    Caption := format( cmsgListManagerMangementMode, [ fDisplayWordPlural ] );
  end;

end;

procedure TfrmListManager.lbListDblClick(Sender : TObject);
begin
  if not fManagementMode then
    actOK.Execute
  else
  begin
    if actEdit.Enabled then
      actEdit.Execute
    else actConsolidate.Execute;
  end;
end;

procedure TfrmListManager.lbListKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  if Key = vk_Return then
  begin
    if actEdit.Enabled then
      actEdit.Execute
    else actConsolidate.Execute;
  end;
end;

procedure TfrmListManager.HandleFormSettings( const TheType : TSettingsDirective );
var
  i : Integer;
  theValue : string;
begin
  theValue := '';
  i := -1;
  case TheType of
    sdSave :
      begin
        FormSettings.AddSetting( inttoStr( Height ) );
        FormSettings.AddSetting( inttoStr( Width ) );

        FormSettings.SaveFormSettings( Self.Name );
      end;
    sdLoad :
      While FormSettings.ReadFormSettings( Self.Name, i, theValue ) do
      case i of
        0 : Height := strtoint( theValue );
        1 : Width := strtoint( theValue );
      end;
  end;

end;

procedure TfrmListManager.SetListObj( AValue : TCmdListObj );
begin
  if fListObj = AValue then Exit;
  fListObj := AValue;
end;

procedure TfrmListManager.SetListType( const AValue : integer );
begin
  if fListType = AValue then Exit;
  fListType := AValue;
end;

procedure TfrmListManager.SetManagementMode( const AValue : boolean );
begin
  if fManagementMode = AValue then Exit;
  fManagementMode := AValue;
end;

procedure TfrmListManager.FormClose(Sender : TObject; var CloseAction : TCloseAction);
begin
  HandleFormSettings( sdSave );
end;

procedure TfrmListManager.actOKExecute(Sender : TObject);
begin
  FCanClose := true;
  Modalresult := mrOK;
end;

procedure TfrmListManager.actRevertExecute(Sender : TObject);
begin
  ListObj.Revert;
  fConsolidateMode := false;
  UpdatelbList( -1, false );
end;

procedure TfrmListManager.actShowUsageExecute( Sender : TObject );
begin

  if lbListInvalidChoice( true ) then
    exit;

  MsgDlgMessage( format( ccapListManagerListItemUsage, [ lbList.Items[ lbList.ItemIndex ] ] ),
                 ListObj.Get_Search_List_Text( lbList.Items[ lbList.ItemIndex ], ListType )
                );
  MsgDlgInfo( self );

end;

procedure TfrmListManager.actCancelExecute(Sender : TObject);
begin
  FCanClose := true;
  Modalresult := mrCancel;
end;

procedure TfrmListManager.actConsolidateExecute(Sender : TObject);
var
  SelectedItem : String;
  Idx : Integer;
begin

  if lbListInvalidChoice then
    exit;

  if lbList.Items.Count = 1 then
  begin
    MsgDlgMessage( ccapListManagerNothingToConsolidate, cmsgListManagerNothingToConsolidate );
    MsgDlgInfo( self );
    exit;
  end;

  with TfrmListManagerConsolidate.Create( self ) do
  try

    caption := format( ccapListManagerConsolidateInfo, [ fDisplayWordSingular ] );
    lblInfo.Caption := caption + ':';
    DisplayWord := fDisplayWordSingular;
    SelectedItem := lbList.Items[ lbList.ItemIndex ];
    lblItem.Caption := DoubleQuotedString( SelectedItem );

    cbChoose.Items.Assign( lbList.Items );
    Idx := cbChoose.Items.IndexOf( SelectedItem );
//can't consolidate to itself, remove it
    cbChoose.Items.Delete( Idx );

    Showmodal;

    if ModalResult = mrOK then
    begin
      Idx := ListObj.Consolidate( SelectedItem, cbChoose.Text);
      if Idx > -1 then
      begin
        fConsolidateMode := true;
        UpdatelbList( Idx );
      end;
    end;

  finally
    free;
  end;


end;

procedure TfrmListManager.actDeleteExecute(Sender : TObject);
var
  i : integer;
  DelStr : String;
begin

  if lbListInvalidChoice( false ) then
    exit;

  if MsgDlgMessage( format( ccapGenericDelete, [ fDisplayWordPlural ] ), cmsgGenericDelete,
                    fDisplayWordSingular + 'cmsgGenericDelete' ) then
    if MsgDlgConfirmation( self ) = mrNo then
      exit;

  DelStr := '';
  for i := lbList.Items.Count - 1 downto 0 do
    if lbList.Selected[ i ] then
      DelStr := DelStr + lbList.Items[ i ] + csoNonPrintingDelimiter;

  UpdatelbList( ListObj.Delete( DelStr ) );

end;

function TfrmListManager.IsDuplicate( const ToChk : string ) : boolean;
var
  Idx : Integer;
begin
  Idx := lbList.Items.IndexOf( ToChk );

  result := ( Idx > -1 )
            and ( CompareStr( lbList.Items[ Idx ], ToChk ) = 0 );
  if result then
  begin

    MsgDlgMessage( format( ccapListManagerDuplicate, [ fDisplayWordSingular ] ),
                   format( cmsgListManagerDuplicate, [ fDisplayWordSingular, ToChk ] )
                 );
    MsgDlgInfo( self );
  end;

end;

function TfrmListManager.lbListInvalidChoice( RequireSingleSelection : boolean = true ) : boolean;
begin
  result := ( lbList.Items.Count = 0 )
            or ( lbList.ItemIndex < 0 )
            or ( lbList.SelCount = 0 );

  if result then
  begin
    MsgDlgMessage( ccapListManagerNothingSelected, cmsgListManagerNothingSelected );
    MsgDlgInfo( self );
    exit;
  end;
  if RequireSingleSelection and ( lbList.SelCount > 1 )  then
  begin
    MsgDlgMessage( ccapListManagerMultipleItems, cmsgListManagerMultipleItems );
    MsgDlgInfo( self );
    result := true;
  end;
end;

procedure TfrmListManager.SetClueTarget( const AValue : string );
begin
  if fClueTarget = AValue then Exit;
  fClueTarget := AValue;
end;

procedure TfrmListManager.SetDisplayWordPlural( const AValue : string );
begin
  if fDisplayWordPlural = AValue then Exit;
  fDisplayWordPlural := AValue;
end;

procedure TfrmListManager.SetDisplayWordSingular( const AValue : string );
begin
  if fDisplayWordSingular = AValue then Exit;
  fDisplayWordSingular := AValue;
end;


procedure TfrmListManager.actEditExecute(Sender : TObject);
var
  Str : String;
begin

  if lbListInvalidChoice then
    exit;

  Str := lbList.Items[ lbList.ItemIndex ];
  if not DoSingleInput( format( ccapGenericEdit, [ fDisplayWordSingular ] ), Str, simEdit, self )
     or IsDuplicate( Str ) then
     exit;

  UpdatelbList( ListObj.Update( lbList.Items[ lbList.ItemIndex ], Str ) );

end;


procedure TfrmListManager.UpdatelbList( const Idx : integer; Revertable : boolean = true );
begin

  lbList.Items := ListObj.ListItemsSL;
  lbList.ItemIndex := Idx;

  actAdd.Enabled := not fConsolidateMode;
  actEdit.Enabled := not fConsolidateMode and ( lbList.Items.Count > 0 );
  actDelete.Enabled := actEdit.Enabled;

  actConsolidate.Enabled := ( fConsolidateMode or not Revertable ) and ( lbList.Items.Count > 1 );

  actRevert.Enabled := Revertable or fConsolidateMode;

end;

procedure TfrmListManager.actAddExecute(Sender : TObject);
var
  Str : String = '';
begin
  if not DoSingleInput( format( ccapGenericAdd, [ fDisplayWordSingular ] ), Str, simEdit, self )
     or IsDuplicate( Str ) then
     exit;

  UpdatelbList( ListObj.Add( Str ) );

end;

procedure TfrmListManager.FormActivate(Sender : TObject);
begin
  if not FIsInitialized then
  begin

    FIsInitialized := true;

    FCanClose := false;

    if MsgDlgMessage( ccapGeneralInformation,
                      format( cmsgListManagerListWarning, [ fDisplayWordSingular, fDisplayWordSingular, fDisplayWordSingular ] ),
                      fDisplayWordSingular + 'cmsgListManagerListWarning'
                      ) then
      MsgDlgInfo( self );


  end;

end;

procedure TfrmListManager.FormCloseQuery(Sender : TObject; var CanClose : boolean);
begin
  CanClose := FCanClose;
end;

procedure TfrmListManager.FormCreate(Sender : TObject);
begin
  ApplyChangeFont( Self );
  FHasShown := false;
  FIsInitialized := false;
  fDisplayWordPlural := format( ccapListManagerDefaultInit, [ 's' ] );
  fDisplayWordSingular := format( ccapListManagerDefaultInit, [ '' ] );
  fManagementMode := false;
  fClueTarget := '!!<Dev Error>!!';
  fConsolidateMode := false;
  btnAdd.Caption := '&A  ' + cbtn_Add;
  btnEdit.Caption := '&B  ' + cbtn_Edit;
  btnDelete.Caption := '&D  ' + cbtn_Delete;
end;

end.

