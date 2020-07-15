unit ufrmSearch;

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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, HintFrame
  , lcltype {THis is needed for key up keyboard constants}
  , unitsharedobj, Buttons, unitGlobForm, StdCtrls, ExtCtrls{, ComCtrls}
  , ActnList, Menus
  , unitDBStructure
  , unitDBConstants
  , unitSearch
  , ufrm_search_operator
  , ufrm_search_list
  , ufrm_search_string
  , ufrm_search_text
  , ufrm_search_bool
  , ufrm_search_enum
//not using right now, no int or double fields
  //, ufrm_search_integer
  //, ufrm_search_double
  , ufrmBoolExpr
  ;

type

  TSearchEditMode = ( semNormal, semLists );

  { TfrmSearch }

  TfrmSearch = class(TForm)
    Bevel1 : TBevel;
    btnCancel : TBitBtn;
    btnOK : TBitBtn;
    btnUseSimpleSearch : TBitBtn;
    cbField : TComboBox;
    edtSearchFileName : TEdit;
    edtCurrProf : TEdit;
    FrameHint1 : TFrameHint;
    lblWhichBoolExpr : TLabel;
    lblFieldCaption : TLabel;
    lblProgramerTip_SearchForm : TLabel;
    lblDesignProf : TLabel;
    lblProgramerTip_Cmd : TLabel;
    lblProgramerTip2 : TLabel;
    lblSearchFileName : TLabel;
    shpDiv : TShape;
    procedure btnOKClick(Sender : TObject);
    procedure btnCancelClick(Sender : TObject);
    procedure btnUseSimpleSearchClick( Sender : TObject );
    procedure cbFieldSelect( Sender : TObject );
    procedure FormActivate(Sender : TObject);
    procedure FormClose(Sender : TObject; var CloseAction : TCloseAction);
    procedure FormCloseQuery(Sender : TObject; var CanClose : boolean);
    procedure FormCreate(Sender : TObject);
    procedure FormDestroy( Sender : TObject );
    procedure FormKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure FormShow(Sender : TObject);
  private
    { private declarations }
    FCanClose : boolean;
    fEditMode : TSearchEditMode;
    FHasShown : boolean;
    FIsInitialized : boolean;

    BECmd : TfrmBoolExpr;
    BECmdLine : TfrmBoolExpr;
    fSeOP : Tfrm_Search_Operator;
    fSeList : Tfrm_Search_List;
    fSeString : Tfrm_Search_String;
    fSeText : Tfrm_Search_Text;
    fSeBool : Tfrm_Search_Bool;
    fSeEnum : Tfrm_Search_Enum;
    //fSeInt : Tfrm_Search_Integer;
    //fSeReal : Tfrm_Search_Double;
    fShowingForm : TForm; //do not create, pointer only
    fFocusedBoolExpr : TfrmBoolExpr; //do not create, pointer only
    fUseSimpleSearch : boolean;

    procedure HandleFormSettings( const TheType : TSettingsDirective );
    procedure CreateSubForms( SO : TSearchObj; theFields : TStrings );
    procedure SetEditMode( AValue : TSearchEditMode );
  public
    { public declarations }

    procedure UpdateCaption( BE : TfrmBoolExpr );
    procedure Link_SI_BoolExpr( anSI : TSearchItem; BE : TfrmBoolExpr );
    procedure HideEditor;
    procedure SetFieldVisibility( const State, IsValid : boolean; BE : TfrmBoolExpr; Cap : string = '' );
    procedure SetBEFocus( const BEContext : integer );
    procedure TransferSearchCriteria( SO : TSearchObj );
    function ReadSearchCriteria( SO : TSearchObj) : boolean;
    function Initialize( theFields : TStrings; SO : TSearchObj;
                           const DoLoad : boolean; const Stamp : string; const aMode : TSearchEditMode ) : boolean;
    procedure IsCopying( const CopyEnabled : boolean );


    property EditMode : TSearchEditMode read fEditMode write SetEditMode;
    property UseSimpleSearch : boolean read fUseSimpleSearch;

  end;

var
  frmSearch : TfrmSearch;

//pointer only, no create or free. Used always by "add" search items and it's ok if it is nil
  GlobCopySI : TSearchItem;

resourcestring
  cfseSearchCaption = 'Search %s';
  cfseSearchCaptionLists = 'Lists';
  cfseSearchCaptionFields = 'Fields';
  cmsgseWhichBoolExpr = 'Editing ==> %s <== Expression:';
  cmsgseWhichBoolExprC = 'COMMAND';
  cmsgseWhichBoolExprCL = 'COMMAND LINE';
  ccapGenericSubFormHint =
    'Select the "match" type you want for the '
    + 'Search and select the value you want below. '
    + LineEnding + LineEnding
    + '<end>'
    + LineEnding;

  ccapGenericTextHint =
    'The text being searched for. '
    + LineEnding + LineEnding
    + '<end>'
    + LineEnding;


  ccapGenericMatchCaseHint =
    'Determines whether the item must match '
    + 'in letter case. '
    + LineEnding + LineEnding
    + 'For example, if this is checked then the '
    + 'search item must match case exactly: '
    + LineEnding + LineEnding
    + '"abc" matches only "abc", not "AbC" or "aBC", '
    + 'etc. '
    + LineEnding + LineEnding
    + 'Unchecked then all of the above will be '
    + 'matches. '
    + LineEnding + LineEnding
    + 'This is ignored in LIST searches if the '
    + 'Match Type is "match". '
    + LineEnding + LineEnding
    + '<end>'
    + LineEnding;

implementation

{$R *.lfm}

uses ufrmMsgDlg
     , unitfields
     , unitGlob
     ;

resourcestring
  cfseCurrentProfileStamp = 'Current Profile:  %s';
  cfseDesignedProfileStamp = 'Search Designed in:  %s';

const
  //cBETop = 112;
  //cBELeft = 8;
  cBEGutter = 80;


{ TfrmSearch }

procedure TfrmSearch.CreateSubForms( SO : TSearchObj; theFields : TStrings );
var
  i , j: Integer;
  COT : TCmdObjType;
  //FT : TufFieldType;

  procedure SetBasicProps( aForm : TObject );
  begin
    with TForm( aForm ) do
    begin
      Top := lblProgramerTip_SearchForm.Top;
      Left := lblProgramerTip_SearchForm.Left;
      Parent := Self;
      Color := clDefault;//self.Color;
      Font.Color := clDefault;
    end;
  end;

begin

  for i := cIdxCmd to cIdxCmdLine do
  case i of
    cIdxCmd :
    begin
      if not assigned( BECmd ) then
      begin
          BECmd := TfrmBoolExpr.Create( Self );

          with BECmd do
          begin
            cbCmdUse.Caption := cfseUseCmd_Caption;
            for j := 0 to theFields.Count - 1 do
            begin
              COT := TSearchFieldRef( theFields.Objects[ j ] ).sfrCmdObjType;
              if ( COT = cotBase ) or ( COT = cotCmd ) then
                ValidFields.AddObject(  theFields[ j ], theFields.Objects[ j ] );
            end;

          if fEditMode = semLists then
              DefaultCmdFieldID := TSearchFieldRef( theFields.Objects[ 0 ] ).sfrCmdFieldID
            else DefaultCmdFieldID := fidCommandName;

      //ValidFields.Add( 'test' );
            Top := lblProgramerTip_Cmd.Top;// cBETop;//112;
            Left := lblProgramerTip_Cmd.Left;// cBELeft;

            cbCmdUse.Checked := SO.Searches[ i ].IsUsed;
            cbCmdUse.Enabled := fEditMode <> semLists;
            HelpContext := 0;
            Parent := Self;
          end;
      end;
    end;
    cIdxCmdLine :
    begin
      if not assigned( BECmdLine ) then
      begin
        BECmdLine := TfrmBoolExpr.Create( Self );
        with BECmdLine do
        begin
          cbCmdUse.Caption := cfseUseCmdLine_Caption;
          DefaultCmdFieldID := fidEntry;
          for j := 0 to theFields.Count - 1 do
          begin
            COT := TSearchFieldRef( theFields.Objects[ j ] ).sfrCmdObjType;
            if ( COT = cotBase ) or ( COT = cotCmdLine ) then
              ValidFields.AddObject(  theFields[ j ], theFields.Objects[ j ] );
          end;
   //ValidFields.Add( 'test2' );
          Top := BECmd.Top + BECmd.Height + cBEGutter;
          shpDiv.Top := Top - cBEGutter + ( cBEGutter div 2 );
            Left := lblProgramerTip_Cmd.Left;// cBELeft;
          cbCmdUse.Checked := SO.Searches[ i ].IsUsed;
          cbCmdUse.Enabled := fEditMode <> semLists;
          HelpContext := 1;
          Parent := Self;
        end;//with BECmdline
      end;//not assigned
    end;
  end;//case


  if not assigned( fSeOP ) then
  begin
    fSeOP := Tfrm_Search_Operator.Create( Self );
    //fSeOP.rgSeList.Hint := Don't change;
    SetBasicProps( fSeOP );
  end;

  if not assigned( fSeList ) then
  begin
    fSeList := Tfrm_Search_List.Create( Self );
    //fSeList.rgSeList.Hint := Don't change;
    fseList.cbMatchCase.Hint := ccapGenericMatchCaseHint;
    SetBasicProps( fSeList );
  end;

  if fEditMode = semLists then
    exit;//don't need any other editor types, ...., probably

  if not assigned( fSeString ) then
  begin
    fSeString := Tfrm_Search_String.Create( Self );
    fSeString.rgSeList.Hint := ccapGenericSubFormHint;
    fseString.cbMatchCase.Hint := ccapGenericMatchCaseHint;
    fseString.memSearchValue.Hint := ccapGenericTextHint;
    SetBasicProps( fSeString );
  end;

  if not assigned( fSeText ) then
  begin
    fSeText := Tfrm_Search_Text.Create( Self );
    fSeText.rgSeList.Hint := ccapGenericSubFormHint;
    fseText.cbMatchCase.Hint := ccapGenericMatchCaseHint;
    fseText.memSearchValue.Hint := ccapGenericTextHint;
    SetBasicProps( fSeText );
  end;

  if not assigned( fSeBool ) then
  begin
    fSeBool := Tfrm_Search_Bool.Create( Self );
    fSeBool.rgSeList.Hint := ccapGenericSubFormHint;
    SetBasicProps( fSeBool );
  end;

  if not assigned( fSeEnum ) then
  begin
    fSeEnum := Tfrm_Search_Enum.Create( Self );
    fSeEnum.rgSeList.Hint := ccapGenericSubFormHint;
    SetBasicProps( fSeEnum );
  end;

//The rest of these, int and double are not currently used no need to create them for now.

  //if not assigned( fSeInt ) then
  //begin
  //  fSeInt := Tfrm_Search_Integer.Create( Self );
  //fSeInt.rgSeList.Hint := ccapGenericSubFormHint;
  //  SetBasicProps( fSeInt );
  //end;
  //
  //if not assigned( fSeReal ) then
  //begin
  //  fSeReal := Tfrm_Search_Double.Create( Self );
  //fSeReal.rgSeList.Hint := ccapGenericSubFormHint;
  //  SetBasicProps( fSeReal );
  //end;


end;


procedure TfrmSearch.FormShow(Sender : TObject);
begin

  if not fHasShown then
  begin
    FCanClose := false;
    HandleFormSettings( sdLoad );
    btnOK.Caption := cObjlblGo;
    FHasShown := true;
  end;

end;

function TfrmSearch.Initialize( theFields : TStrings; SO : TSearchObj; const DoLoad : boolean;
                       const Stamp : string; const aMode : TSearchEditMode ) : boolean;
begin

  result := true;

  if theFields.Count = 0 then
    raise EErrorDevelopment.create( 'TfrmSearch.Initialize: No Fields were sent in.' );

  fEditMode := aMode;

  CreateSubForms( SO, theFields );

  if DoLoad then
  begin
    result := ReadSearchCriteria( SO );
    lblDesignProf.Caption := format( cfseDesignedProfileStamp, [ SO.DBInfo ] );
  end
  else lblDesignProf.Caption := format( cfseDesignedProfileStamp, [ Stamp ] );

  edtCurrProf.Text := format( cfseCurrentProfileStamp, [ Stamp ] );
  edtSearchFileName.Text := SO.FileName;

  BECmd.Show;
  BECmdLine.Show;
  if not DoLoad then //its new, add a field for them
  begin
    if aMode = semNormal then
      BECmdLine.btnCmdSI_R_Add.Click;
    BECmd.btnCmdSI_R_Add.Click;
  end;

end;

procedure TfrmSearch.IsCopying( const CopyEnabled : boolean );
begin
  BECmd.CopyMode( CopyEnabled );
  BECmdLine.CopyMode( CopyEnabled );
end;


procedure TfrmSearch.HandleFormSettings( const TheType : TSettingsDirective );
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

procedure TfrmSearch.FormClose(Sender : TObject; var CloseAction : TCloseAction);
begin
  HandleFormSettings( sdSave );
  fShowingForm := nil; //do not free, pointer only
  fFocusedBoolExpr := nil; //do not free, pointer only
end;

procedure TfrmSearch.btnOKClick(Sender : TObject);
begin
  FCanClose := true;
  Modalresult := mrOK;
end;


procedure TfrmSearch.btnCancelClick(Sender : TObject);
begin
  FCanClose := true;
  Modalresult := mrCancel;
end;

procedure TfrmSearch.btnUseSimpleSearchClick( Sender : TObject );
begin
  fUseSimpleSearch := true;
  FCanClose := true;
  Modalresult := mrCancel;
end;

procedure TfrmSearch.cbFieldSelect( Sender : TObject );
var
  SFR : TSearchFieldRef;
begin
  //fieldChange gets hit first
  //FieldSelect gets hit second

  if ( cbField.ItemIndex < 0 ) or not assigned( fFocusedBoolExpr ) then
    exit;

  SFR := TSearchFieldRef( cbField.Items.Objects[ cbField.ItemIndex ] );
  fFocusedBoolExpr.ReInit( SFR.sfrCmdFieldID, SFR.sfrFieldType );
  fFocusedBoolExpr.UpdateCurrCaption;
  fFocusedBoolExpr.ReSelect;

end;

procedure TfrmSearch.SetEditMode( AValue : TSearchEditMode );
begin
  if fEditMode = AValue then Exit;
  fEditMode := AValue;
end;

procedure TfrmSearch.UpdateCaption( BE : TfrmBoolExpr );
begin
  BE.UpdateCurrCaption;
end;

procedure TfrmSearch.Link_SI_BoolExpr( anSI : TSearchItem; BE : TfrmBoolExpr );
begin

  //comes here from BoolExpr MasterClick event

  fFocusedBoolExpr := BE;
  if assigned( fShowingForm ) then
    fShowingForm.Hide;

  case anSI.SearchOperatorType of
    sotNone : //it's a search item
      begin

        case anSI.FieldType of
          ftString_Key, ftString :
            begin
              fSeString.Link_SI_BoolExpr( anSI, BE );
              fShowingForm := fSeString;
            end;
          ftText :
            begin
              fSeText.Link_SI_BoolExpr( anSI, BE );
              fShowingForm := fSeText;
            end;
          ftList :
            begin
//attach the master list to the List form. We only have one list (keywords) so no need to do it every time,
//but if more lists come in future, this needs to be done every time. It has a count of 1 for item "sanitycheck"
              if fSeList.lbList.Items.Count < 2 then
              begin
                //Old method used in Initialize, here for reference, but basically call the procedure with the
                //current field displayname and the appropriate master list will be assigned to the listbox.
                //SFR := TSearchFieldRef( theFields.Objects[ i ] );
                //if SFR.sfrFieldType = ftList then
                //    if not ListFields_Linker__AttachList( theFields[ i ], fSeList.lbList.Items  ) then

                if not ListFields_Linker__AttachList(
                     TSearchFieldRef( cbField.Items.Objects[ cbField.ItemIndex ] ).ColumnName, fSeList.lbList.Items
                                                    ) then
                //if not ListFields_Linker__AttachList( cbField.Items[ cbField.ItemIndex ], fSeList.lbList.Items  ) then
                //if not ListFields_Linker__AttachList( 'Keywords', fSeList.lbList.Items  ) then
                  raise EErrorDevelopment.create( 'TfrmSearch.Initialize: List Item indicated but no List available' );
              end;
              fSeList.Link_SI_BoolExpr( anSI, BE );
              fShowingForm := fSeList;
            end;
          ftBoolean :
            begin
              fSeBool.Link_SI_BoolExpr( anSI, BE );
              fShowingForm := fSeBool;
            end;
          ftInteger :
            //begin
            //  fSeInt.Link_SI_BoolExpr( anSI, BE );
            //  fShowingForm := fSeInt;
            //end
            ;
          ftEnum :
            begin
//Enumerations need constant translatable strings for the search form
//These are returned based on the field's UniqueID
//currently only have one Enum type (ThreatLevel) so only need to fill in initially, later if more Enum types added
//then this has to run each time.
              if fSeEnum.rgEnum.Items.Count = 0 then
              begin
                if not ReturnEnumerationConstants( anSI.siUniqueID, fSeEnum.rgEnum.Items ) then
                  raise EErrorDevelopment.create( 'TfrmSearch.Initialize: Enum Item indicated but no Enumerations available' );
              end;
              fSeEnum.Link_SI_BoolExpr( anSI, BE );
              fShowingForm := fSeEnum;
            end;
          ftDouble :
            //begin
            //  fSeReal.Link_SI_BoolExpr( anSI, BE );
            //  fShowingForm := fSeReal;
            //end
            ;
        end;

      end;

    else //it's an operator: or/and/xor
      case anSI.SearchOperatorType of
        sotAND, sotOR, sotXOR :
          begin
            fSeOP.Link_SI_BoolExpr( anSI, BE );
            fShowingForm := fSeOP;
          end;
      end;
  end;

end;

procedure TfrmSearch.HideEditor;
begin
  if assigned( fShowingForm ) then
    fShowingForm.Hide;

  cbField.Text := '';
  cbField.Enabled := false;

end;

procedure TfrmSearch.SetFieldVisibility( const State, IsValid : boolean; BE : TfrmBoolExpr; Cap : string = '' );
var
  Idx : Integer;
begin
  cbField.Visible := State;
  lblFieldCaption.Visible := State;
  lblWhichBoolExpr.Visible := State;
  if State then
  begin
    cbField.Items := BE.ValidFields;

    Idx := cbField.Items.IndexOf( Cap );
    //showmessage( '==>' + Cap + LineEnding + cbField.Items.Text );
    //exit;
    if Idx < 0 then
    begin
      if not IsValid then
        Idx := 0
      else raise EErrorDevelopment.create( 'Houston! We should have no problems here!' );
    end;

    cbField.ItemIndex := Idx;
    cbField.Enabled := true;
  end else
  begin
    cbField.Text := '';
    cbField.Enabled := false;
  end;
end;

procedure TfrmSearch.SetBEFocus( const BEContext : integer );
begin
  case BEContext of
    0 :
      begin
        lblWhichBoolExpr.Caption := format( cmsgseWhichBoolExpr, [ cmsgseWhichBoolExprC ] );
        BECmd.FocusOn;
        BECmdLine.FocusOff;
      end;
    1 :
      begin
        lblWhichBoolExpr.Caption := format( cmsgseWhichBoolExpr, [ cmsgseWhichBoolExprCL ] );
        BECmd.FocusOff;
        BECmdLine.FocusOn;
      end;
  end;
end;

procedure TfrmSearch.TransferSearchCriteria( SO : TSearchObj );
begin

  if BECmd.HasItems then
    BECmd.TransferSearchCriteria( SO.Searches[ cIdxCmd ] );

  if BECmdLine.HasItems then
    BECmdLine.TransferSearchCriteria( SO.Searches[ cIdxCmdLine ] );

end;

function TfrmSearch.ReadSearchCriteria( SO : TSearchObj ) : boolean;
var
  i : Integer;
begin

  result := true;

  for i := 0 to High( SO.Searches ) do
  case i of
    cIdxCmd : result := result and BECmd.ReadSearchCriteria( SO.Searches[ cIdxCmd ] );
    cIdxCmdLine : result := result and BECmdLine.ReadSearchCriteria( SO.Searches[ cIdxCmdLine ] );
  end;

end;


procedure TfrmSearch.FormActivate(Sender : TObject);
begin
  if not FIsInitialized then
  begin
    FIsInitialized := true;
    FrameHint1.cbHints.Caption := ccbHintsEnglishOverride;
  end;
end;

procedure TfrmSearch.FormCloseQuery(Sender : TObject; var CanClose : boolean);
begin
  CanClose := FCanClose;
end;

procedure TfrmSearch.FormCreate(Sender : TObject);
begin
  font.size := cDefaultFontSize;
  ApplyChangeFont( Self );
  FHasShown := false;
  FIsInitialized := false;
  fShowingForm := nil; //do not create, pointer only
  fFocusedBoolExpr := nil; //do not create, pointer only
  fEditMode := semLists;
  BECmd := nil;
  BECmdLine := nil;
  GlobCopySI := nil;//initialize to nil, do not create
  fUseSimpleSearch := false;
end;

procedure TfrmSearch.FormDestroy( Sender : TObject );
begin
  GlobCopySI := nil; //pointer only, remove reference but do not free

//important, if there is an exception creating one of the boolexpr's, the form stays open but tries to close them here.
//====>>> NEED to check assigned
  if assigned( BECmd ) then
    BECmd.Close;
  if assigned( BECmdLine ) then
    BECmdLine.Close;
end;

procedure TfrmSearch.FormKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  //Damn bitbtn automatically take return and press OK, arghhh.
  //I need return to pass through to the boolexpr forms, so I send it on differently and this works but
  //is definitely a cludge??
    if Key = vk_return then
      Key := VK_F9;
    if Key = vk_ESCAPE then
    begin
      if assigned( BECmd ) then
        BECmd.FormKeyDown( Self, Key, Shift );
      if assigned( BECmdLine ) then
        BECmdLine.FormKeyDown( Self, Key, Shift );
    end;
  //keys clash in this form and subforms, arrows act like tabs, its a mess
    //if Key = vk_left then
    //  Key := vk_f7;
    //if Key = vk_right then
    //  Key := vk_f8;
end;

end.

{ ✗ ✘ ⎯  ⇄ ← →  ±     }

