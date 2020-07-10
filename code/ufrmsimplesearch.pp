unit ufrmSimpleSearch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, HintFrame
  , lcltype {THis is needed for key up keyboard constants}
  , unitsharedobj, Buttons
  , unitGlobForm, StdCtrls, ExtCtrls
  , unitSearch
  , unitDBConstants
  ;

type

  { TfrmSimpleSearch }

  TfrmSimpleSearch = class(TForm)
    Bevel1 : TBevel;
    btnAdvSearchCurrent : TBitBtn;
    btnAdvSearchLoad : TBitBtn;
    btnAdvSearchNew : TBitBtn;
    btnClear : TBitBtn;
    btnClear1 : TBitBtn;
    btnClear2 : TBitBtn;
    btnSSCancel : TBitBtn;
    btnSSOK : TBitBtn;
    btnChkAllCmdLine : TButton;
    btnChkNoneCmd : TButton;
    btnChkNoneCmdLine : TButton;
    btnChkAllCmd : TButton;
    btnSSReset : TBitBtn;
    cbMatchCase : TCheckBox;
    cbMatchCase1 : TCheckBox;
    cbMatchCase2 : TCheckBox;
    cbRE : TCheckBox;
    cbRE1 : TCheckBox;
    cbRE2 : TCheckBox;
    cgCmd : TCheckGroup;
    cgCmdLine : TCheckGroup;
    FrameHint1 : TFrameHint;
    Label1 : TLabel;
    lblDesignProf : TLabel;
    lblSeType : TLabel;
    lblSeType2 : TLabel;
    memSearchValue : TMemo;
    memSearchValue1 : TMemo;
    memSearchValue2 : TMemo;
    procedure btnSSOKClick(Sender : TObject);
    procedure btnAdvSearchCurrentClick( Sender : TObject );
    procedure btnSSCancelClick(Sender : TObject);
    procedure btnChkAllCmdClick( Sender : TObject );
    procedure btnChkAllCmdLineClick( Sender : TObject );
    procedure btnChkNoneCmdClick( Sender : TObject );
    procedure btnChkNoneCmdLineClick( Sender : TObject );
    procedure btnClearClick( Sender : TObject );
    procedure btnSSResetClick( Sender : TObject );
    procedure cbREClick( Sender : TObject );
    procedure FormActivate(Sender : TObject);
    procedure FormClose(Sender : TObject; var CloseAction : TCloseAction);
    procedure FormCloseQuery(Sender : TObject; var CanClose : boolean);
    procedure FormCreate(Sender : TObject);
    procedure FormKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure FormShow(Sender : TObject);
  private
    { private declarations }
    FCanClose : boolean;
    FHasShown : boolean;
    FIsInitialized : boolean;
    fUseAdancedSearch : integer;
    procedure CheckGroups( Sender : TCheckGroup; DoCheck : boolean );
    procedure FillSearchObj_Intern( SO : TSearchObj; CG : TCheckGroup; const Value : string; const Cond : TCondition; const MatchCase : boolean );
    procedure FocusSearchValue( aMem : TMemo );
    procedure HandleFormSettings(TheType : TSettingsDirective);
    procedure SetUseAdancedSearch( AValue : integer );
  public
    { public declarations }
    procedure Initialize( theFields : TStrings );
    procedure FillSearchObj( SO : TSearchObj );
    property UseAdancedSearch : integer read fUseAdancedSearch write SetUseAdancedSearch;
  end;

var
  frmSimpleSearch : TfrmSimpleSearch;

implementation

{$R *.lfm}

uses ufrmMsgDlg
     , unitfields
     , unitGlob
     //, strconst_prog
     ;

resourcestring
  cSSmsgNoSearchValue = 'You''ve specified no search text.';
  cSSmsgNoSearchFields = 'No search fields are checked.';
  cSSmsgBtnClears = 'Reset';
  cSScapResetAll = 'Reset all inputs';
  cSSmsgResetAll = 'Are you sure you want to reset all the inputs?';
  cSSHintReset =
    'Resets the items to the left: clears '
    + Lineending
    + 'Search Text, and unchecks the two '
    + Lineending
    + 'checkboxes. '
    + Lineending + LineEnding
    + '<end>'
    ;

  cSSHintAdditional =
    'Additional search text. Anything typed  '
    + Lineending
    + 'will also be searched for. '
    + Lineending + LineEnding
    + 'You have 3 search text areas, all are '
    + Lineending
    + '"OR"d, that is, search for "bob" '
    + Lineending
    + 'OR "Alice" OR "Carol". '
    + Lineending + LineEnding
    + '<end>'
    ;

  cSScapMatchCase = 'Match Case';
  cSSHintMatchCase =
    'If checked requires search text above '
    + Lineending
    + 'to match case exactly. '
    + Lineending + LineEnding
    + 'If unchecked case doesn''t matter and '
    + Lineending
    + 'search text "bob" will match bob, '
    + Lineending
    + 'boB, bOb, BoB, etc. '
    + Lineending + LineEnding
    + '<end>'
    ;

  cSScapRegEx = 'Regular Expression';
  cSSHintRegEx =
    'If checked this indicates that the '
    + Lineending
    + 'search text above is a '
    + Lineending
    + 'Regular Expression. '
    + Lineending + LineEnding
    + '<end>'
    ;


  cSSmsgChecks = 'Checks';
  cSSmsgUnChecks = 'Unchecks';

  cSSHintCChecks =
    '%s all the boxes in the '
    + Lineending
    + 'area below. '
    + Lineending + LineEnding
    + '<end>'
    ;

  cSSHintCheckBoxGroups =
    'Contains a checkbox list of all '
    + Lineending
    + 'text (string) fields for: '
    + Lineending + LineEnding
    + '%s. '
    + Lineending + LineEnding
    + 'Uncheck fields if you don''t '
    + Lineending
    + 'want to search them.'
    + Lineending + LineEnding
    + '<end>'
    ;

  cSSHintAdvSearchCurrent =
    'Current Search; that is, '
    + Lineending
    + 'whatever was last searched for '
    + Lineending
    + 'in this Database profile.'
    ;

  cSSHintAdvSearchNew =
    'New Search; that is, '
    + Lineending
    + 'a blank advanced search '
    + Lineending
    + 'window.'
    ;

  cSSHintAdvSearchLoad =
    'The "load a saved Search" window.'
    ;

  cSSHintAdvSearch =
    'Switches to the ADVANCED SEARCH '
    + Lineending
    + 'window, which has 3 options. '
    + Lineending
    + 'This button opens: '
    + Lineending + LineEnding
    + '%s'
    + Lineending + LineEnding
    + '<end>'
    ;

const
  cArrowKeymemSearchValue  = 500000;
  cArrowKeymemSearchValue1 = 500001;
  cArrowKeymemSearchValue2 = 500002;

{ TfrmSimpleSearch }

procedure TfrmSimpleSearch.FormShow(Sender : TObject);
begin

  if not FHasShown then
  begin
    FCanClose := false;
//    FormColors.ApplySystemColors( Self );
    HandleFormSettings( sdLoad );
    btnSSOK.caption := cObjlblGo;
    FrameHint1.cbHints.Caption := ccbHintsEnglishOverride;

    memSearchValue.Tag  := cArrowKeymemSearchValue;
    memSearchValue1.Tag := cArrowKeymemSearchValue1;
    memSearchValue2.Tag := cArrowKeymemSearchValue2;

    FHasShown := true;
  end;

end;

procedure TfrmSimpleSearch.HandleFormSettings(TheType : TSettingsDirective);
var
  i , j, idx: Integer;
  theValue , str: string;
const
  TVal = 'T';
  FVal = 'F';
begin
  theValue := '';
  i := -1;
  case TheType of
    sdSave :
      begin
        FormSettings.AddSetting( inttoStr( Height ) );//0
        FormSettings.AddSetting( inttoStr( Width ) );//1

        FormSettings.AddSetting( MemSearchValue.Text ); //2, 3, 4
        FormSettings.AddSetting( MemSearchValue1.Text ); //2, 3, 4
        FormSettings.AddSetting( MemSearchValue2.Text ); //2, 3, 4

        if cbMatchCase.Checked then //5,6,7
          FormSettings.AddSetting( TVal )
        else FormSettings.AddSetting( FVal );

        if cbMatchCase1.Checked then
          FormSettings.AddSetting( TVal )
        else FormSettings.AddSetting( FVal );

        if cbMatchCase2.Checked then
          FormSettings.AddSetting( TVal )
        else FormSettings.AddSetting( FVal );


        if cbRE.Checked then //8, 9, 10
          FormSettings.AddSetting( TVal )
        else FormSettings.AddSetting( FVal );

        if cbRE1.Checked then
          FormSettings.AddSetting( TVal )
        else FormSettings.AddSetting( FVal );

        if cbRE2.Checked then
          FormSettings.AddSetting( TVal )
        else FormSettings.AddSetting( FVal );

        str := ''; // 11
        for j := 0 to cgCmd.Items.Count - 1 do
        begin
          if cgCmd.Checked[ j ] then
            str := str + TVal
          else str := str + FVal;
        end;
        FormSettings.AddSetting( str );

        str := ''; // 12
        for j := 0 to cgCmdLine.Items.Count - 1 do
        begin
          if cgCmdLine.Checked[ j ] then
            str := str + TVal
          else str := str + FVal;
        end;
        FormSettings.AddSetting( str );

        FormSettings.SaveFormSettings( Self.Name );

      end;
    sdLoad :
      While FormSettings.ReadFormSettings( Self.Name, i, theValue ) do
      case i of
        0 : Height := strtoint( theValue );
        1 : Width := strtoint( theValue );

        2 : MemSearchValue.Text := theValue;
        3 : MemSearchValue1.Text := theValue;
        4 : MemSearchValue2.Text := theValue;

        5 : cbMatchCase.Checked := theValue = TVal;
        6 : cbMatchCase1.Checked := theValue = TVal;
        7 : cbMatchCase2.Checked := theValue = TVal;

        8 : cbRE.Checked := theValue = TVal;
        9 : cbRE1.Checked := theValue = TVal;
       10 : cbRE2.Checked := theValue = TVal;

       11 :
          begin
            idx := cgCmd.Items.Count - 1;
            if Length( TheValue ) - 1 < idx then
              idx := Length( TheValue ) - 1;

            for j := 0 to Idx do
              cgCmd.Checked[ j ] := TheValue[ j + 1 ] = TVal;
          end;
       12 :
          begin
            idx := cgCmdLine.Items.Count - 1;
            if Length( TheValue ) - 1 < idx then
              idx := Length( TheValue ) - 1;

            for j := 0 to Idx do
              cgCmdLine.Checked[ j ] := TheValue[ j + 1 ] = TVal;
          end;

      end;
  end;

end;

procedure TfrmSimpleSearch.SetUseAdancedSearch( AValue : integer );
begin
  if fUseAdancedSearch = AValue then Exit;
  fUseAdancedSearch := AValue;
end;

procedure TfrmSimpleSearch.Initialize( theFields : TStrings );
var
  SFR : TSearchFieldRef;
  i , Idx: Integer;
begin

  if theFields.Count = 0 then
    raise EErrorDevelopment.create( 'TfrmSimpleSearch.Initialize: No Fields were sent in.' );

  cgCmd.Tag := cIdxCmd;
  cgCmdLine.Tag := cIdxCmdLine;

  for i := 0 to theFields.Count - 1 do
  begin

    SFR := TSearchFieldRef( theFields.Objects[ i ] );

    if not ( SFR.sfrFieldType in [ ftString_Key, ftString, ftList, ftText ] ) then
      continue;

    case SFR.sfrCmdObjType of
      cotBase :
        begin
          Idx := cgCmd.Items.AddObject( theFields[ i ], SFR );
          cgCmd.Checked[ Idx ] := true;
          Idx := cgCmdLine.Items.AddObject( theFields[ i ], SFR );
          cgCmdLine.Checked[ Idx ] := true;
        end;
      cotCmd :
        begin
          Idx := cgCmd.Items.AddObject( theFields[ i ], SFR );
          cgCmd.Checked[ Idx ] := true;
        end;
      cotCmdLine :
        begin
          Idx := cgCmdLine.Items.AddObject( theFields[ i ], SFR );
          cgCmdLine.Checked[ Idx ] := true;
        end;
    end;
  end;

end;

procedure TfrmSimpleSearch.FillSearchObj_Intern( SO : TSearchObj; CG : TCheckGroup;
                                                 const Value : string;
                                                 const Cond : TCondition; const MatchCase : boolean );
var
  i : Integer;
begin

  for i := 0 to CG.Items.Count - 1 do
  begin
    if CG.Checked[ i ] then
    begin

      if SO.Searches[ CG.Tag ].SIList.Count > 0 then
        SO.Searches[ CG.Tag ].AddOperator( sotOR );
      SO.Searches[ CG.Tag ].AddSearchItem(
              TSearchFieldRef( CG.Items.Objects[ i ] ).sfrCmdFieldID,
              Value,
              Cond,
              MatchCase
                                                  );
    end;

  end;

end;

procedure TfrmSimpleSearch.FillSearchObj( SO : TSearchObj );

  procedure Add_SO_Items( const SearchValue : string; const IsRE : boolean; const MatchCase : boolean );
  var
    Cond : TCondition;
  begin
    if trim( SearchValue ) <> '' then //only trim here to check it's not absolutely nothing
    begin

      if IsRE then
        Cond := coRegEx
      else Cond := coContains;

      FillSearchObj_Intern( SO, cgCmd, SearchValue, Cond, MatchCase );
      FillSearchObj_Intern( SO, cgCmdLine, SearchValue, Cond, MatchCase );

    end;
  end;

begin
  Add_SO_Items( memSearchValue.Text, cbRE.Checked, cbMatchCase.Checked );
  Add_SO_Items( memSearchValue1.Text, cbRE1.Checked, cbMatchCase1.Checked );
  Add_SO_Items( memSearchValue2.Text, cbRE2.Checked, cbMatchCase2.Checked );
end;

procedure TfrmSimpleSearch.FormClose(Sender : TObject; var CloseAction : TCloseAction);
begin
  HandleFormSettings( sdSave );
end;

procedure TfrmSimpleSearch.btnSSOKClick(Sender : TObject);
var
  i : Integer;
  HaveChecked : Boolean;
begin

  if ( trim( memSearchValue.Text ) = '' )
     and ( trim( memSearchValue1.Text ) = '' )
     and ( trim( memSearchValue2.Text ) = '' ) then
  begin
    ShowMessage( cSSmsgNoSearchValue );
    exit;
  end;

  HaveChecked := false;
  for i := 0 to cgCmd.Items.Count - 1 do
  begin
    HaveChecked := cgCmd.Checked[ i ];
    if HaveChecked then
      break;
  end;

  if not HaveChecked then
  for i := 0 to cgCmdLine.Items.Count - 1 do
  begin
    HaveChecked := cgCmdLine.Checked[ i ];
    if HaveChecked then
      break;
  end;

  if not HaveChecked then
  begin
    ShowMessage( cSSmsgNoSearchFields );
    exit;
  end;

  FCanClose := true;
  Modalresult := mrOK;

end;

procedure TfrmSimpleSearch.btnAdvSearchCurrentClick( Sender : TObject );
begin
  fCanClose := true;
  fUseAdancedSearch := TButton( Sender ).Tag;
  ModalResult := mrCancel;
end;

procedure TfrmSimpleSearch.btnSSCancelClick(Sender : TObject);
begin
  FCanClose := true;
  Modalresult := mrCancel;
end;

procedure TfrmSimpleSearch.btnChkAllCmdClick( Sender : TObject );
begin
  CheckGroups( cgCmd, true );
end;

procedure TfrmSimpleSearch.btnChkAllCmdLineClick( Sender : TObject );
begin
  CheckGroups( cgCmdLine, true );
end;

procedure TfrmSimpleSearch.btnChkNoneCmdClick( Sender : TObject );
begin
  CheckGroups( cgCmd, false );
end;

procedure TfrmSimpleSearch.btnChkNoneCmdLineClick( Sender : TObject );
begin
  CheckGroups( cgCmdLine, false );
end;

procedure TfrmSimpleSearch.CheckGroups( Sender : TCheckGroup; DoCheck : boolean );
var
  i : Integer;
begin
  for i := 0 to Sender.Items.Count - 1 do
  begin
    Sender.Checked[ i ] := DoCheck;
  end;
end;

procedure TfrmSimpleSearch.btnClearClick( Sender : TObject );
begin
  Case TComponent( Sender ).Tag of
    0 :
      begin
        memSearchValue.Text := '';
        cbMatchCase.Checked := false;
        cbRE.Checked := false;
        FocusSearchValue( memSearchValue );
      end;
    1 :
      begin
        memSearchValue1.Text := '';
        cbMatchCase1.Checked := false;
        cbRE1.Checked := false;
        FocusSearchValue( memSearchValue1 );
      end;
    2 :
      begin
        memSearchValue2.Text := '';
        cbMatchCase2.Checked := false;
        cbRE2.Checked := false;
        FocusSearchValue( memSearchValue2 );
      end;
  end;
end;

procedure TfrmSimpleSearch.btnSSResetClick( Sender : TObject );
begin

  if MsgDlgMessage( cSScapResetAll, cSSmsgResetAll, 'cSSmsgResetAll') then
    if MsgDlgConfirmation( self ) = mrNo then
      exit;

  btnClear.Click;
  btnClear1.Click;
  btnClear2.Click;
  btnChkAllCmd.Click;
  btnChkAllCmdLine.Click;
  if memSearchValue.CanFocus then
    memSearchValue.SetFocus;
end;

procedure TfrmSimpleSearch.FocusSearchValue( aMem : TMemo );
begin
  if aMem.CanFocus then
    aMem.SetFocus;
end;

procedure TfrmSimpleSearch.cbREClick( Sender : TObject );
begin
  case TComponent( Sender ).Tag of
    0 : FocusSearchValue( memSearchValue );
    1 : FocusSearchValue( memSearchValue1 );
    2 : FocusSearchValue( memSearchValue2 );
  end;
end;

procedure TfrmSimpleSearch.FormActivate(Sender : TObject);

  procedure SetCBs( CB : TCheckbox; const Shortcut : string; const IsRE : boolean );
  begin
    if IsRE then
    begin
      CB.Caption := format( '&%s  ', [ ShortCut ] ) + cSScapRegEx;
      CB.Hint := cSSHintRegEx;
    end
    else
    begin
      CB.Caption := format( '&%s  ', [ ShortCut ] ) + cSScapMatchCase;
      CB.Hint := cSSHintMatchCase;
    end;
  end;

begin
  if not FIsInitialized then
  begin

    cgCmd.Caption := ' ' + cNameItem_CommandPlur + ' ';
    cgCmdLine.Caption := ' ' + cNameItem_CommandLinePlur + ' ';

//for po file translations, yes its a pain but
//really makes life easier for translators
    btnClear.Caption := '&I  ' + cSSmsgBtnClears;
    btnClear.Hint := cSSHintReset;
    btnClear1.Caption := '&J  ' + cSSmsgBtnClears;
    btnClear1.Hint := cSSHintReset;
    btnClear2.Caption := '&K  ' + cSSmsgBtnClears;
    btnClear2.Hint := cSSHintReset;

    btnChkNoneCmd.Hint := format( cSSHintCChecks, [ cSSmsgUnChecks ] );
    btnChkNoneCmdLine.Hint := format( cSSHintCChecks, [ cSSmsgUnChecks ] );;
    btnChkAllCmd.Hint := format( cSSHintCChecks, [ cSSmsgChecks ] );
    btnChkAllCmdLine.Hint := format( cSSHintCChecks, [ cSSmsgChecks ] );

    cgCmd.Hint := format( cSSHintCheckBoxGroups, [ cNameItem_CommandPlur ] );
    cgCmdLine.Hint := format( cSSHintCheckBoxGroups, [ cNameItem_CommandLinePlur ] );

    SetCBs( cbMatchCase, 'A', false );
    SetCBs( cbRE, 'B', true );
    SetCBs( cbMatchCase1, 'C', false );
    SetCBs( cbRE1, 'D', true );
    SetCBs( cbMatchCase2, 'E', false );
    SetCBs( cbRE2, 'F', true );

    memSearchValue1.Hint := cSSHintAdditional;
    memSearchValue2.Hint := cSSHintAdditional;

    btnAdvSearchCurrent.Hint := format( cSSHintAdvSearch, [ cSSHintAdvSearchCurrent ] );
    btnAdvSearchNew.Hint := format( cSSHintAdvSearch, [ cSSHintAdvSearchNew ] );
    btnAdvSearchLoad.Hint := format( cSSHintAdvSearch, [ cSSHintAdvSearchLoad ] );

    FIsInitialized := true;
  end;

end;

procedure TfrmSimpleSearch.FormCloseQuery(Sender : TObject; var CanClose : boolean);
begin
  CanClose := FCanClose;
end;

procedure TfrmSimpleSearch.FormCreate(Sender : TObject);
begin
  font.size := cDefaultFontSize;
  ApplyChangeFont( Self );
  FHasShown := false;
  FIsInitialized := false;
  fUseAdancedSearch := -1;
end;

procedure TfrmSimpleSearch.FormKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
//pressing return in memo sent the key to parent form which clicked the OK button. ugh.
//In this form however it is ok to do so. I leave the code here just in case.,
//return in memo now runs the search
  //if key = vk_return then
  //  Key := VK_UNKNOWN;
  if key = VK_ESCAPE then
  begin
    btnSSCancel.Click;
    exit;
  end;

  if Shift = [ ssShift, ssCtrl ] then
  case key of
    VK_LEFT, VK_RIGHT :
      if assigned( self.ActiveControl ) then
      begin
        case self.ActiveControl.Tag of
          cArrowKeymemSearchValue :
            case Key of
              VK_RIGHT : TryFocus( memSearchValue1 );
              VK_LEFT : TryFocus( memSearchValue2 );
            end;
          cArrowKeymemSearchValue1 :
            case Key of
              VK_RIGHT : TryFocus( memSearchValue2 );
              VK_LEFT : TryFocus( memSearchValue );
            end;
          cArrowKeymemSearchValue2 :
            case Key of
              VK_RIGHT : TryFocus( memSearchValue );
              VK_LEFT : TryFocus( memSearchValue1 );
            end;
          else TryFocus( memSearchValue );
        end;
        Key := VK_UNKNOWN;
      end;
  end;

end;

end.

