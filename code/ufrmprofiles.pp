unit ufrmProfiles;

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
  , JIniFiles
  ;

type

  { TProfileObj }

  TProfileObj = class( TObject )
  private
    fName : string;
    fIsDB : boolean;
    fPath : string;
    fIsDefault : boolean;
  Protected
  public
    constructor Create;
    destructor Destroy; override;
    function GetExpandedPath( ProgDefPath : string ) : string;
    function Get_DBType : string;
    procedure Assign( aPO : TProfileObj );
    property IsDB : boolean read fIsDB;
    property Path : string read fPath;
    property Name : string read fName;
  end;

  { TfrmProfiles }

  TfrmProfiles = class(TForm)
    actCancel : TAction;
    actAdd : TAction;
    actEdit : TAction;
    actDelete : TAction;
    actCopy : TAction;
    actConvert : TAction;
    actCompare : TAction;
    actMergeTo : TAction;
    actSelect : TAction;
    actOK : TAction;
    ActionList1 : TActionList;
    btnCompare : TButton;
    btnCopy : TButton;
    btnEdit : TButton;
    btnCancel : TBitBtn;
    btnConvert : TButton;
    btnMergeTo : TButton;
    btnOK : TBitBtn;
    btnAdd : TButton;
    btnDelete : TButton;
    btnSelect : TBitBtn;
    FrameHint1 : TFrameHint;
    gbManageList : TGroupBox;
    lblCurrentProfile : TLabel;
    lblPathDisplay : TLabel;
    lbList : TListBox;
    lblDefaultDisplay : TLabel;
    MenuItem1 : TMenuItem;
    MenuItem2 : TMenuItem;
    mniCompare : TMenuItem;
    mniConvert : TMenuItem;
    mniMergeTo : TMenuItem;
    mniCancelS : TMenuItem;
    mniSelect : TMenuItem;
    mniConsolidate : TMenuItem;
    mniAdd : TMenuItem;
    mniEdit : TMenuItem;
    mniDelete : TMenuItem;
    mniOK : TMenuItem;
    popM : TPopupMenu;
    popS : TPopupMenu;
    procedure actAddExecute(Sender : TObject);
    procedure actCancelExecute(Sender : TObject);
    procedure actCompareExecute( Sender : TObject );
    procedure actConvertExecute( Sender : TObject );
    procedure actCopyExecute(Sender : TObject);
    procedure actDeleteExecute(Sender : TObject);
    procedure actEditExecute(Sender : TObject);
    procedure actMergeToExecute( Sender : TObject );
    procedure actOKExecute(Sender : TObject);
    procedure actSelectExecute( Sender : TObject );
    procedure FormActivate(Sender : TObject);
    procedure FormClose(Sender : TObject; var CloseAction : TCloseAction);
    procedure FormCloseQuery(Sender : TObject; var CanClose : boolean);
    procedure FormCreate(Sender : TObject);
    procedure FormDestroy( Sender : TObject );
    procedure FormShow(Sender : TObject);
    procedure lbListClick( Sender : TObject );
    procedure lbListDblClick(Sender : TObject);
    procedure lbListKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure popMPopup( Sender : TObject );
  private
    { private declarations }
    fAllowDefSql : boolean;
    fAllowDefText : boolean;
    FCanClose : boolean;
    fCurrProfileIsDB : boolean;
    fCurrProfileName : string;
    fCurrProfileNameFormatted : string;
    fCurrProfilePath : string;
    fIsSelectMode : boolean;
    fProgDefaultPath : string;
    FHasShown : boolean;
    fIFile : TJiniFile;
    FIsInitialized : boolean;
    fConsolidateMode : boolean;
    fIdxDB : Integer;
    fIdxText: Integer;
    fHeaderCount : integer;
    fTrueCaseSensitive : TStringlist;
    f_DB_Profile : TProfileObj;
    f_Text_Profile : TProfileObj;
    fSelectedProfileIsDB : boolean;

    procedure AdjustListSelectMode;
    function Compare_NameAccepted( aPO : TProfileObj ) : boolean;
    function Copy_ProfileNameCheck( const CheckStr : string ) : boolean;
    function CopySuccess( const FromF, ToF : string ) : boolean;
    procedure Delete_SavedSearches( aPO : TProfileobj );
    function DeleteSuccess( const FileName : string) : boolean;
    function DestinationCleanup( const FromF, ToF : string ) : boolean; overload;
    function DestinationCleanup( Strings : TStrings; const Path, ProfileName : string ) : boolean; overload;
    procedure EmergencyDelete_Convert( aPO : TProfileObj; const NewName : string );
    function GetDispStr( tmpPO : TProfileObj ) : string;
    function HandleSqlEdit( PO : TProfileObj; const CurrFileName : string ) : boolean;
    function HandleTextEdit( PO : TProfileObj; const CurrPath, CurrName : string ) : boolean;
    procedure AdjustList;
    function CurrentDB( DoShowMessage : boolean = true; Idx : integer = - 1 ) : boolean;
    function GetAdjustedProfileName( const ProfName : string; const isDB : boolean ) : string;
    procedure HandleFormSettings( const TheType : TSettingsDirective );
    function IsAllowed( var CheckStr : string; NameOnly : boolean = false ) : boolean;
    function Convert_IsAllowed( var CheckStr : string ) : boolean;
    function lbListInvalidChoice : boolean;
    function OptionsStatus( PO : TProfileObj) : string;
    function ProcessTextFiles( Strings : TStrings; const IsRename : boolean ) : boolean;
    function ProfileNameAlreadyUsed( const CheckStr : string; NameOnly : boolean = false ) : boolean;
    procedure RefreshList( const IndexOn : string );
    procedure ReleaseDBProfiles;
    function RenameSuccess( const FromF, ToF : string ) : boolean;
    function Reserved( const CheckStr : string) : boolean;
    procedure SetAllowDefSql( AValue : boolean );
    procedure SetAllowDefText( AValue : boolean );
    procedure SetCurrProfileIsDB( AValue : boolean );
    procedure SetCurrProfileName( AValue : string );
    procedure SetCurrProfilePath( AValue : string );
    procedure SetIsSelectMode( AValue : boolean );
    procedure SetProgDefaultPath( AValue : string );
    procedure SetIFile( AValue : TJiniFile );
    procedure SetupMode;
    function SourceExists( tmpPO : TProfileObj; DBType : string = 'Source' ) : boolean;
  public
    { public declarations }
    function MergeOne( CO : TCmdObj; Mergesource : string ) : string;
    procedure WriteList( const ProfileEntry, Path : string; DeleteMe : string = '' );
    procedure FillList;
    procedure FilterList;
    function NameAccepted( const aCaption : string; var Str : string; NameOnly : boolean = false ) : boolean;
    function Convert_NameAccepted( const aCaption : string; var Str : string; const Path : string; const IsDB : boolean ) : boolean;
    function Merge_NameAccepted( aPO : TProfileObj ) : boolean;

    property CurrProfileName : string read fCurrProfileName write SetCurrProfileName;
    property CurrProfilePath : string read fCurrProfilePath write SetCurrProfilePath;
    property CurrProfileIsDB : boolean read fCurrProfileIsDB write SetCurrProfileIsDB;
    property IFile : TJiniFile read fIFile write SetIFile;
    property ProgDefaultPath : string read fProgDefaultPath write SetProgDefaultPath;
    property IsSelectMode : boolean read fIsSelectMode write SetIsSelectMode;
    property AllowDefSql : boolean read fAllowDefSql write SetAllowDefSql;
    property AllowDefText : boolean read fAllowDefText write SetAllowDefText;
  end;


var
  frmProfiles : TfrmProfiles;


implementation

{$R *.lfm}

uses ufrmMsgDlg
  , juusgen
  , strconst_prog
  , strconst_en
  , ufrmManageProfile
  , unitDBUtils
  , ufrmProfilesMerge
  , unitGlob
  , ufrmMain
  ;

resourcestring
  cmsgProfilesInformation =
    'Just so you know.'
    + LineEnding + LineEnding
    + 'Changes made to this list are immediate. DB files, if they exist, will be renamed, moved, '
    + 'or deleted immediately. '
    + LineEnding + LineEnding
    + 'Assume Profile Name of "Bob". '
    + LineEnding + LineEnding
    + 'For sql DB''s it is a single file and the file name convention is "Bob.sqlite"  '
    + LineEnding + LineEnding
    + 'For text DB''s there are %d files named like this:  '
    + LineEnding + LineEnding
    + '%s'
    + LineEnding + LineEnding
    ;

  cmsgProfNothingSelected = 'Either nothing is selected or the only choices are the default "DB" choices.';
  ccapFileError = 'File Error';
  cmsgFileErrorDelete = 'DELETE: Data file "%s" could not be deleted. ' + LineEnding + LineEnding;
  cmsgFileErrorMove = 'RENAME / MOVE: Data file "%s" could not be renamed/moved to "%s". ' + LineEnding + LineEnding;
  cmsgFileErrorCopy = 'COPY: Data file "%s" could not be copied to "%s". ' + LineEnding + LineEnding;
  cmsgMostLikely = 'Either the file(s) do not exist, there is a permissions problem, or possibly unmounted drives.' + LineEnding;
  cmsgPathDisplay = 'Path: %s';
  cmsgDefaultDisplay = '( Default Path = %s )';
  cmsgCurrentProfile = 'Current Profile = "%s"';
  ccapProDeleteOK = 'File Deletion?';
  cmsgProDeleteOK = 'Do you also want to delete the files? This cannot be undone, you should have backups just in case.';
  ccapProDeleteFinal = 'Final Confirmation';
  cmsgProDeleteFinal = 'The Profile == AND == the Database file(s) will be deleted. Continue?.';
  cmsgProDeleteFinalNoDelete = 'The Profile will be deleted == BUT == the Database file(s) will remain. Continue?.';
  ccapProCantCopy = 'Can not Copy';
  cmsgProCantCopy = 'Data files for Profile "%s" already exist in "%s". Copy not allowed, you '
                     + 'need to decide what to do with them. If you want to use them then add '
                     + 'NEW and point the new Profile to that folder. If you don''t want them then '
                     + 'move or delete them.';
  ccapProInvalidFolder = 'Invalid Folder';
  cmsgProInvalidFolder = 'Folder "%s" does not exist. Probably the drive is not mounted?';
  ccapProSelectDB = 'Select Database Profile';
  ccapProManageDB = 'Manage Database Profiles';
  ccapVariSelect = '&A  Select Profile:  "%s"';
  ccapStandardSelect = '&A  Select';
  cmsgProfSwitchedOff = ' ( Switched off in Options )';


  ccapProConvertConfirm = 'Convert %s To %s:';
  ccapProConvertSuccess = 'DB Conversion successful. You can load it from the "Switch Database" button.';
  ccapProCopyConfirm = 'Copy Profile';
  ccapProIsCurrentDB = '"%s" is your current database and it is in use. You may not edit or delete it.';
  ccapProMergeConfirm = 'Merge?';
  cmsgProMergeConfirm = 'Merge  "%s" to "%s"?';

  cmsgProMergeSuccess = 'DB Merge successful. Load Merged DB from the "Switch Database" button.';
  cmsgProCONotAssigned = 'Command object was not assigned.';
  cmsgProPONotAssigned = 'Profile Object not assigned.';
  cmsgProPONameInvalid = 'TfrmProfiles.FillList: Program''s .formsettings file has invalid Profile name "%s", skipping it.';
  cmsgProReservedName = '"%s" is reserved and may not be used';
  ccapProNoOverWrite = 'Can not overwrite existing';
  cmsgProNoOverWrite = '"%s" DB already exists, will not overwrite it.';
  cmsgProNoInitialize = 'Could not initialize DB';

  ccapProDBFileNotExist = '%s does not exist';
  cmsgProDBFileNotExist = '"%s"  DB has not been initialized and so is empty, %s DB file must exist.';
  cmsgProDestNotExist = 'You can use copy or convert instead.';
  cmsgProSourceNotExist = 'Open the DB at least once and, preferably, put some data into it.';
  ccapProDestFileExists = '%s File Exists!';
  cmsgProDestFileExists = 'You want to move data file "%s". But... '
                          + LineEnding + LineEnding
                          + '...Destination File "%s" already exists!! '
                          + LineEnding + LineEnding
                          + 'Do you want to overwite that sql data file? '
                          + LineEnding
                          ;
  ccapProDestFilesExist = '%s Text Files Exist!';
  cmsgProDestFilesExist = 'Destination path "%s" already contains one or more files of Profile "%s". '
                          + LineEnding + LineEnding
                          + 'Do you want to overwite those text data files? '
                          + LineEnding
                          ;
  cmsgProMergeCompareProblem ='TfrmProfiles: %s Commands had a problem.';

  ccapSelectProfileType = 'Select Profile to %s TO';
  ccapSelectProfileType_Merge = 'Merge';
  ccapSelectProfileType_Compare = 'Compare';
  ccapCompareResults = 'Compare Results';
  cmsgComparing = 'Comparing  "%s" <source> to  "%s" <destination>:' + LineEnding + LineEnding;

  cmsgProSourceStr = 'Source';
  cmsgProDestStr = 'Destination';
  cmsgProDBTextStr = 'text';
const
  cmsgProDBsqlStr = 'sql';

  translatedDef = '  ( %s )';
  Custom_Separator = '-----------------------';


{ TProfileObj }

destructor TProfileObj.Destroy;
begin
  inherited Destroy;
end;

procedure TProfileObj.Assign( aPO : TProfileObj );
begin
  fName := aPO.fName;
  fPath := aPO.fPath;
  fIsDB := aPO.fIsDB;
end;

function TProfileObj.GetExpandedPath( ProgDefPath : string ) : string;
begin
  result := strif( fPath = constDefaultPathValue, ProgDefPath, fPath )
end;

function TProfileObj.Get_DBType : string;
begin
  result := strif( fIsDB, cmsgProDBsqlStr, cmsgProDBTextStr )
end;

constructor TProfileObj.Create;
begin
  fIsDefault := false;
end;

{ TfrmProfiles }

function GetManagedDisplayName( IsDB : boolean ) : string;
begin
  result := cDefaultDBProfileName
            + strif( IsDB, cDefaultDBProfileIsDBStr, cDefaultDBProfileIsDBStrNot )
            + format( translatedDef, [ constDefaultPathDisplay ] );
end;


procedure TfrmProfiles.AdjustListSelectMode;

  procedure InsertDefault( IsDB : boolean; PO : TProfileObj );
  begin
    lbList.Items.Insert( 0, GetManagedDisplayName( IsDB ) );//text
    lbList.Items.Objects[ 0 ] := PO;
    inc( fHeaderCount );
  end;

  procedure AddDefault( IsDB : boolean; PO : TProfileObj );
  var
    Idx : Integer;
  begin
    Idx := lbList.Items.Add( GetManagedDisplayName( IsDB ) );//sql
    lbList.Items.Objects[ Idx ] := PO;
    inc( fHeaderCount );
  end;

begin

//programatically managed default "DB" databases, count simply matches number of insertions
  fHeaderCount := 0;

  if fAllowDefSql or fAllowDefText then
  begin

    if lbList.Items.Count > 0 then
    begin
      lbList.Items.Insert( 0, Custom_Separator );
      inc( fHeaderCount );
      if fAllowDefText then
        InsertDefault( false, f_Text_Profile );
      if fAllowDefSql then
        InsertDefault( true, f_DB_Profile );
    end
    else
    begin
      if fAllowDefSql then
        AddDefault( true, f_DB_Profile );
      if fAllowDefText then
        AddDefault( false, f_Text_Profile );
      lbList.Items.Add( Custom_Separator );
      inc( fHeaderCount );
    end;

    popMPopup( Self );

  end;

end;

procedure TfrmProfiles.AdjustList;
begin

  if IsSelectMode then
  begin
    AdjustListSelectMode;
    exit;
  end;

//programatically managed default "DB" databases, count simply matches number of insertions
  fHeaderCount := 3;

  if lbList.Items.Count > 0 then
  begin
    lbList.Items.Insert( 0, Custom_Separator );
    lbList.Items.Insert( 0, GetManagedDisplayName( false ) );
    lbList.Items.Insert( 0, GetManagedDisplayName( true ) );
  end
  else
  begin
    lbList.Items.Add( GetManagedDisplayName( true ) );
    lbList.Items.Add( GetManagedDisplayName( false ) );
    lbList.Items.Add( Custom_Separator );
  end;

  lbList.Items.Objects[ 0 ] := f_DB_Profile;
  lbList.Items.Objects[ 1 ] := f_Text_Profile;

  popMPopup( Self );

end;

procedure TfrmProfiles.FilterList;
var
  Idx : Integer;
  PO : TProfileObj;

  procedure RemoveIt( theIdx : integer );
  begin
    lbList.Items.Objects[ theIdx ] := nil;
    lbList.Items.Delete( theIdx );
  end;

begin
  fTrueCaseSensitive.Assign( lbList.Items );
  Idx := fTrueCaseSensitive.IndexOf( fCurrProfileNameFormatted );
  if Idx < 0 then
    raise EErrorDevelopment.create( 'TfrmProfiles.FilterList: Problem with filtering out current Profile.' );

  if Idx > fHeaderCount - 1 then
  begin
    PO := TProfileObj( lbList.Items.Objects[ Idx ] );
    if assigned( PO ) then
      PO.free;
    RemoveIt( Idx );
  end
  else
  begin
    RemoveIt( Idx );
    dec( fHeaderCount );
    if fHeaderCount = 1 then
    begin
      RemoveIt( Idx );
      fHeaderCount := 0;
    end;
  end;

end;

procedure TfrmProfiles.SetupMode;
begin

  AdjustList;

  if CurrProfileName = cDefaultDBProfileName then
    fCurrProfileNameFormatted := GetManagedDisplayName( CurrProfileIsDB )
  else fCurrProfileNameFormatted := GetAdjustedProfileName( CurrProfileName, CurrProfileIsDB );

  if IsSelectMode then
  begin
    btnOK.Visible := false;
    gbManageList.Visible := false;
    lblPathDisplay.Visible := false;
    lblDefaultDisplay.Visible := false;
    btnSelect.Visible := true;
    btnCancel.Visible := true;
    Caption := ccapProSelectDB;
    lbList.PopupMenu := PopS;
    PopupMenu := PopS;
    FilterList;
  end else Caption := ccapProManageDB;

end;

procedure TfrmProfiles.FormShow(Sender : TObject);
begin

  if FHasShown then
    exit;

  FCanClose := false;
  HandleFormSettings( sdLoad );
  lblPathDisplay.Caption := format( cmsgPathDisplay, [ '' ] );
  lblDefaultDisplay.Caption := format( cmsgDefaultDisplay, [ ProgDefaultPath ] );
  FillList;
  SetupMode;
  actAdd.Enabled := true;



  FHasShown := true;

end;

function TfrmProfiles.OptionsStatus( PO : TProfileObj ) : string;
begin
  result := '';
  if PO.fIsDefault then
    if ( PO.fIsDB and not AllowDefSql )
       or ( not PO.fIsDB and not AllowDefText )
    then
      result := cmsgProfSwitchedOff;
end;

procedure TfrmProfiles.lbListClick( Sender : TObject);
var
  PO : TProfileObj;
begin

  try

    lblPathDisplay.Caption := '';
    fSelectedProfileIsDB := false;

    if lbList.ItemIndex < 0 then
      exit;

    PO := TProfileObj( lbList.Items.Objects[ lbList.Itemindex ] );
    if not assigned( PO ) then
      exit;
    fSelectedProfileIsDB := PO.fIsDB;
    lblPathDisplay.Caption := format( cmsgPathDisplay, [ PathValueToPathDisplay( PO.fPath ) ] )
                              + OptionsStatus( PO );

  finally
    popMPopup( Self );
  end;

end;

procedure TfrmProfiles.lbListDblClick(Sender : TObject);
begin
  if IsSelectMode and btnSelect.Enabled then
  begin
    btnSelect.Click;
    exit;
  end;
  if actEdit.Enabled then
    actEdit.Execute;
end;

procedure TfrmProfiles.lbListKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  if Key = vk_Return then
  begin
    if actEdit.Enabled then
      actEdit.Execute;
  end;
end;

procedure TfrmProfiles.popMPopup( Sender : TObject);
begin

  actEdit.Enabled := ( lbList.ItemIndex > fHeaderCount - 1 )
                      and ( lbList.Items.Count > fHeaderCount );

  actDelete.Enabled := actEdit.Enabled;

  actCopy.Enabled := ( lbList.ItemIndex > -1 ) and ( lbList.ItemIndex <> fHeaderCount - 1 );
  actSelect.Enabled := actCopy.Enabled;
  if lbList.ItemIndex > -1 then
    actSelect.Caption := format( ccapVariSelect, [ ExtractProfileName( lbList.Items[ lbList.ItemIndex ] ) ] );
  btnSelect.Caption := ccapStandardSelect;

  actConvert.Enabled := actCopy.Enabled;
  actMergeTo.Enabled := actCopy.Enabled;
  actCompare.Enabled := actCopy.Enabled;

end;

procedure TfrmProfiles.HandleFormSettings( const TheType : TSettingsDirective );
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


procedure TfrmProfiles.FormClose(Sender : TObject; var CloseAction : TCloseAction);
begin
  HandleFormSettings( sdSave );
end;

procedure TfrmProfiles.actOKExecute(Sender : TObject);
begin
  FCanClose := true;
  Modalresult := mrOK;
end;

procedure TfrmProfiles.actSelectExecute( Sender : TObject );
var
  PO : TProfileObj;
begin
  PO := TProfileObj( lbList.Items.Objects[ lbList.Itemindex ] );
  if not assigned( PO ) then
    exit;

  if not DirectoryExists( PO.GetExpandedPath( ProgDefaultPath ) ) then
  begin
    MsgDlgMessage( ccapProInvalidFolder, format( cmsgProInvalidFolder, [ PO.GetExpandedPath( ProgDefaultPath ) ] ) );
    MsgDlgAttention( self );
    exit;
  end;

  CurrProfileIsDB := PO.fIsDB;
  CurrProfileName := PO.fName;
  CurrProfilePath := PO.fPath;

  FCanClose := true;
  Modalresult := mrOK;
end;

procedure TfrmProfiles.actCancelExecute(Sender : TObject);
begin
  FCanClose := true;
  Modalresult := mrCancel;
end;

procedure TfrmProfiles.actCompareExecute( Sender : TObject );
var
  FromPO, ToPO: TProfileObj; //FromPO just pointer, no free
  FromIS, DestIS : TInfoServer;
  SL : TStringList;
  FromDBSpec , ToDBSpec: String;

  function ServerInit( var anIS : TInfoServer; aPO : TProfileObj ) : boolean;
  begin

    result := false;

    anIS := TInfoServer.Create;

    if not anIS.Init( aPO.GetExpandedPath( ProgDefaultPath ), aPO.fName, aPO.fIsDB ) then
    begin
      Showmessage( 'TfrmProfiles.actCompare ServerInit: ' + cmsgProNoInitialize );
      exit;
    end;

    result := true;

  end;

const
  DBSpecification = '%s (%s)';

begin

  FromPO := TProfileObj( lbList.Items.Objects[ lbList.Itemindex ] );
  if not assigned( FromPO ) or not SourceExists( FromPO ) then
    exit;

  SL := TStringlist.Create;
  try

    FromIS := nil;
    DestIS := nil;
    ToPO := TProfileObj.Create;
    try

      if not Compare_NameAccepted( ToPo ) then
        exit;

      Screen.Cursor := crHourglass;

      if not ServerInit( FromIS, FromPO ) then
        exit;

      if not ServerInit( DestIS, ToPO ) then
        exit;

      FromDBSpec := format( DBSpecification, [ FromPO.Name, FromPO.Get_DBType ] );
      ToDBSpec := format( DBSpecification, [ ToPO.Name, ToPO.Get_DBType ] );
      SL.Add( format( cmsgComparing, [ FromDBSpec, ToDBSpec ] ) );

      if not TCmdObj.CompareCommands( FromIS, DestIS, SL, FromDBSpec, ToDBSpec ) then
        Showmessage( format( cmsgProMergeCompareProblem, [ ccapSelectProfileType_Compare ] ) );

      if not TCmdListObj.CompareLists( dbltKeyWord, FromIS, DestIS, SL, FromDBSpec, ToDBSpec ) then
        Showmessage( format( cmsgProMergeCompareProblem, [ ccapSelectProfileType_Compare ] ) );

      DestIS.UnInitialize( false );
      FromIS.UnInitialize( false );

    finally //inner main try

      Screen.Cursor := crDefault;

      if assigned( DestIS ) then
        FreeAndNil( DestIS );

      if assigned( FromIS ) then
        FreeAndNil( FromIS );

      if assigned( ToPO ) then
        FreeAndNil( ToPO );
    end;

    MsgDlgMessage( ccapCompareResults, SL.text );
    MsgDlgInfo( self );

  finally //SL try
    SL.free;
  end;


end;

procedure TfrmProfiles.actConvertExecute( Sender : TObject );
var
  FromIS, DestIS : TInfoServer;
  FromPO : TProfileObj; //just pointer, no free
  ToPO : TProfileObj; //just poiner, no free;
  Str : String;

  function ServerInit( var anIS : TInfoServer; Path, Name : string; DBType : boolean ) : boolean;
  begin

    result := false;

    anIS := TInfoServer.Create;

    if not anIS.Init( Path, Name, DBType ) then
    begin
      Showmessage( 'TfrmProfiles.actConvertExecute ServerInit: ' + cmsgProNoInitialize );
      exit;
    end;

    result := true;

  end;

begin

  FromPO := TProfileObj( lbList.Items.Objects[ lbList.Itemindex ] );
  if not assigned( FromPO ) or not SourceExists( FromPO ) then
    exit;

  Str := FromPO.fName;

  if not Convert_NameAccepted( format( ccapProConvertConfirm, [ FromPO.fName, strif( FromPO.fIsDB, cmsgProDBTextStr, cmsgProDBsqlStr ) ] ),
                               Str,
                               FromPO.GetExpandedPath( ProgDefaultPath ),
                               not FromPO.fIsDB ) then
    exit;


//I've run into problems where the objects have phantom pointers. Explicit initialization is the answer.
//anything in finally needs initialization
  FromIS := nil;
  DestIS := nil;
  try

    Screen.Cursor := crHourglass;

    if not ServerInit( FromIS, FromPO.GetExpandedPath( ProgDefaultPath ), FromPO.fName, FromPO.fIsDB ) then
      exit;

    if not ServerInit( DestIS, FromPO.GetExpandedPath( ProgDefaultPath ), Str, not FromPO.fIsDB ) then
      exit;

    try
      if not TCmdObj.ConvertCommands( FromIS, DestIS ) then
        exit;

      TCmdListObj.ConvertLists( dbltKeyWord, FromIS, DestIS );
    except
      DestIS.UnInitialize( false );
      EmergencyDelete_Convert( FromPO, Str );
      raise;
    end;

    DestIS.SaveAll;
    DestIS.UnInitialize( false );
    FromIS.UnInitialize( false );

    ToPO := TProfileObj.Create;
    ToPO.fName := Str;
    ToPO.fIsDB := not FromPO.fIsDB;
    ToPO.fPath := FromPO.fPath;//FromPO.GetExpandedPath( ProgDefaultPath );

    Str := GetAdjustedProfileName( ToPO.fName, ToPO.fIsDB );

    lbList.Items.AddObject( Str, ToPO );
    WriteList( Str, ToPO.fPath );
    RefreshList( Str );
    ToPO := nil;

  finally
    Screen.Cursor := crDefault;

    if assigned( DestIS ) then
      FreeAndNil( DestIS );

    if assigned( FromIS ) then
      FreeAndNil( FromIS );

  end;

  ShowMessage( ccapProConvertSuccess );

end;

procedure TfrmProfiles.actCopyExecute(Sender : TObject);
var
  Str, FromF, ToF, AddStr, CurrPath: string;
  PO, NewPO : TProfileObj;
  SL : TStringList;
  DestIS : TInfoServer;

  function AlreadyThere : boolean;
  begin
//Unlikely but maybe they already copied the destination files in, or they deleted a profile of the same name but
//didn't choose to delete the data files. For COPY simply fail if so.
    if PO.fIsDB then
    begin
      ToF := GenerateDBFilePath(
                             PO.GetExpandedPath( ProgDefaultPath ),
                             Str,
                             cSqlDBExtension
                                );
      FromF := GenerateDBFilePath(
                             PO.GetExpandedPath( ProgDefaultPath ),
                             PO.fName,
                             cSqlDBExtension
                                  );

      result := FileExists( ToF );
    end
    else result := GetIniDBFiles( SL, PO.GetExpandedPath( ProgDefaultPath ), Str, false ) > 0;

    if result then
    begin
      MsgDlgMessage( ccapProCantCopy, format( cmsgProCantCopy, [ Str, PO.GetExpandedPath( ProgDefaultPath ) ] ) );
      MsgDlgAttention( self );
    end;
  end;

begin

  if lbList.ItemIndex < 0 then
    exit;

  Str := '';
  if not NameAccepted( ccapProCopyConfirm, Str, true ) then
    exit;

  PO := TProfileObj( lbList.Items.Objects[ lbList.ItemIndex ] );
  if not assigned( PO ) then
    exit;

  SL := TStringlist.Create;
  try

    if AlreadyThere then
      exit;

    if PO.fIsDB then
    begin
      if CopySuccess( FromF, ToF ) then
      begin

        DestIS := nil;
        try
          Screen.Cursor := crHourglass;

          DestIS := TInfoServer.Create;

          if not DestIS.Init( extractFilePath( ToF ),
                              ChangeFileExt( ExtractFileName( ToF ), '' ),
                              true ) then
          begin
            Showmessage( 'TfrmProfiles.actCopyExecute: Could not initialize sql DB' );
            if FileExists( ToF ) then
              DeleteFile( ToF );
            exit;
          end;

          DestIS.UpdateGUID( ToF, true );
          //DestIS.SaveAll;
          DestIS.UnInitialize( false );

        finally

          Screen.Cursor := crDefault;

          if assigned( DestIS ) then
            FreeAndNil( DestIS );

        end;

      end
      else exit;
    end else
    begin
      CurrPath := PO.GetExpandedPath( ProgDefaultPath );
      GetIniDBFileFromToList( SL, CurrPath, CurrPath, PO.fName, Str );
      if not ProcessTextFiles( SL, false ) then
        exit;
    end;

    NewPO := TProfileObj.Create;
    NewPO.fName := Str;
    NewPO.fIsDB := PO.fIsDB;
    NewPO.fPath := PO.fPath;
    AddStr := GetAdjustedProfileName( Str, NewPO.fIsDB );

    lbList.Items.AddObject( AddStr, NewPO );
    WriteList( AddStr, NewPO.fPath );
    RefreshList( AddStr );

  finally
    SL.free;
  end;

end;

procedure TfrmProfiles.EmergencyDelete_Convert( aPO : TProfileObj; const NewName : string );
var
  SL : TStringList;
  SqlDBFile : String;
  i : Integer;
begin
//special for exception in convert, reversing the process so watch ISdb and Name!
//need this because a failure left the newly initialized files in place but empty.
//if the files exist you can't use that name again, you would need to add it to profiles
//not what you want if the files have just been created and are empty.
  SL := TStringList.Create;
  try

    if not aPO.fIsDB then
    begin
      SqlDBFile := GenerateDBFilePath(
                             aPO.GetExpandedPath( ProgDefaultPath ),
                             NewName,
                             cSqlDBExtension
                                        );
      if FileExists( SqlDBFile ) then
        SL.Add( SqlDBFile );
    end
    else GetIniDBFiles( SL, aPO.GetExpandedPath( ProgDefaultPath ), NewName, false );

    for i := 0 to SL.Count - 1 do
      if FileExists( SL[ i ] ) then
        Deletefile( SL[ i ] );

  finally
    SL.free;
  end;
end;

procedure TfrmProfiles.actDeleteExecute(Sender : TObject);
var
  DoDeletefiles : boolean;
  PO : TProfileObj;
  OldIndex , i: Integer;
  RemoveKey, SqlDBFile: String;
  SL : TStringList;
begin

  if lbListInvalidChoice or CurrentDB then
    exit;

  PO := TProfileObj( lbList.Items.Objects[ lbList.ItemIndex ] );
  if not assigned( PO ) then
    exit;

  DoDeletefiles := false;

  SL := TStringList.Create;
  try

    MsgDlgMessage( format( ccapGenericDelete, [ PO.fName ] ), cmsgGenericDelete );
    if MsgDlgConfirmation( self ) = mrNo then
      exit
    else
    begin

      if PO.fIsDB then
      begin
        SqlDBFile := GenerateDBFilePath(
                               PO.GetExpandedPath( ProgDefaultPath ),
                               PO.fName,
                               cSqlDBExtension
                                          );
        if FileExists( SqlDBFile ) then
          SL.Add( SqlDBFile );
      end
      else GetIniDBFiles( SL, PO.GetExpandedPath( ProgDefaultPath ), PO.fName, false );

      if SL.Count > 0 then
      begin
        MsgDlgMessage( ccapProDeleteOK, cmsgProDeleteOK );
        DoDeletefiles := MsgDlgConfirmation( self ) = mrYes;
      end;

    end;

    if DoDeleteFiles then
    begin
      if MsgDlgMessage( ccapProDeleteFinal, cmsgProDeleteFinal ) then
        if MsgDlgConfirmation( self ) = mrNo then
          exit;

      Delete_SavedSearches( PO );

      for i := 0 to SL.Count - 1 do
        if not DeleteSuccess( SL[ i ] ) then
          exit;
    end
    else if SL.Count > 0 then
    begin
      if MsgDlgMessage( ccapProDeleteFinal, cmsgProDeleteFinalNoDelete ) then
        if MsgDlgConfirmation( self ) = mrNo then
          exit;
    end;

    RemoveKey := GetAdjustedProfileName( PO.fName, PO.fIsDB );
    PO.Free;
    OldIndex := lbList.ItemIndex;
    lbList.Items.Delete( OldIndex );
    lbList.ItemIndex := GetValidItemIndex( OldIndex, lbList.Items.Count );

    WriteList( '', '', RemoveKey );
    popMPopup( self );

  finally
    SL.free;
  end;

end;

procedure TfrmProfiles.Delete_SavedSearches( aPO : TProfileobj );
var
  anIS : TInfoServer;
  aGUID : String;
begin

  aGUID := '';

  if not SourceExists( aPO ) then
    exit;

  anIS := TInfoServer.Create;

  try

    if not anIS.Init( aPO.GetExpandedPath( ProgDefaultPath ), aPO.fName, aPO.fIsDB ) then
      exit;

    aGUID := anIS.GetDBGUID;

    anIS.UnInitialize( false );

  finally
    anIS.Free;
  end;

  if aGUID <> '' then
    TfrmMain( Owner ).SavedSearches_Delete( aGUID );

end;

function TfrmProfiles.lbListInvalidChoice : boolean;
begin

  result := ( lbList.Items.Count <= fHeaderCount )
            or ( lbList.ItemIndex < fHeaderCount - 1 )
            ;

  if result then
  begin
    MsgDlgMessage( cmsgNoCommandSelected, cmsgProfNothingSelected );
    MsgDlgInfo( self );
  end;

end;

procedure TfrmProfiles.SetCurrProfileIsDB( AValue : boolean );
begin
  if fCurrProfileIsDB = AValue then Exit;
  fCurrProfileIsDB := AValue;
end;

procedure TfrmProfiles.SetCurrProfileName( AValue : string );
begin
  if fCurrProfileName = AValue then Exit;
  fCurrProfileName := AValue;
end;

procedure TfrmProfiles.SetCurrProfilePath( AValue : string );
begin
  if fCurrProfilePath = AValue then Exit;
  fCurrProfilePath := AValue;
end;

procedure TfrmProfiles.SetIsSelectMode( AValue : boolean );
begin
  if fIsSelectMode = AValue then Exit;
  fIsSelectMode := AValue;
end;

procedure TfrmProfiles.SetProgDefaultPath( AValue : string );
begin
  if fProgDefaultPath = AValue then Exit;
  fProgDefaultPath := AValue;
end;

procedure TfrmProfiles.SetIFile( AValue : TJiniFile );
begin
  if fIFile = AValue then Exit;
  fIFile := AValue;
end;

function TfrmProfiles.CurrentDB( DoShowMessage : boolean = true; Idx : integer = -1 ) : boolean;
begin
  if Idx > -1 then
    result := fCurrProfileNameFormatted = lbList.Items[ Idx ]
  else result := fCurrProfileNameFormatted = lbList.Items[ lbList.ItemIndex ];
  if DoShowMessage and result then
    Showmessage( format( ccapProIsCurrentDB, [ fCurrProfileNameFormatted ] ) );
end;

procedure TfrmProfiles.actEditExecute(Sender : TObject);
var
  PO , HoldPO: TProfileObj;
  EditStr, CurrPath, CurrFileName, CurrName : String;
begin

  if lbListInvalidChoice or CurrentDB then
    exit;

  PO := TProfileObj( lbList.Items.Objects[ lbList.ItemIndex ] );
  if not assigned( PO ) then
    exit;

  HoldPO := TProfileObj.Create;
  try
    HoldPO.Assign( PO );

    CurrPath := PO.GetExpandedPath( ProgDefaultPath );// strif( PO.fPath = constDefaultPathValue, ProgDefaultPath, PO.fPath );
    CurrName := PO.fName;

    with TfrmManageProfile.Create( self ) do
    try
      Mode := mpEdit;
      SetupEditProfile( PO.fName, PathValueToPathDisplay( PO.fPath ), PO.fIsDB );
      ShowPoint := GetPreciseControlCoords( btnOk );

      ShowModal;

      if ( ModalResult = mrOK ) and IsChanged then
      begin
        PO.fName := edtProfileName.Text;
  //PO.fIsDB := Not allowed to change with Edit, once created it is always of that DB type
        PO.fPath := PathDisplayToPathValue( edtProfilePath.Text, ProgDefaultPath );

        if PO.fIsDB then
        begin
          CurrFileName := GenerateDBFilePath(
                                 CurrPath,
                                 CurrName,
                                 cSqlDBExtension
                                            );
          if not HandleSqlEdit( PO, CurrFileName ) then
          begin
            PO.Assign( HoldPO );
            exit;
          end;
        end
        else
        begin
          if not HandleTextEdit( PO, CurrPath, CurrName ) then
          begin
            PO.Assign( HoldPO );
            exit;
          end;
        end;

        EditStr := GetAdjustedProfileName( PO.fName, PO.fIsDB );

        lbList.Items[ lbList.ItemIndex ] := EditStr;
        WriteList( EditStr,
                   PO.fPath,
                   strif( CurrName <> PO.fName, GetAdjustedProfileName( CurrName, PO.fIsDB ) )
                   );
        RefreshList( EditStr );
      end;

    finally
      free;
    end;

    popMPopup( self );

  finally
    HoldPo.free;
  end;

end;


function TfrmProfiles.SourceExists( tmpPO : TProfileObj; DBType : string = 'Source' ) : boolean;
var
  Exists : Boolean;
begin

  result := false;

  if tmpPO.IsDB then
    Exists := FileExists( GenerateDBFilePath( tmpPO.GetExpandedPath( ProgDefaultPath ),
                                              tmpPO.Name,
                                              cSqlDBExtension
                                             )
                         )
  else Exists := IniDBFilesExist( tmpPO.GetExpandedPath( ProgDefaultPath ), tmpPO.Name );

  if not Exists then
  begin
    MsgDlgMessage( format( ccapProDBFileNotExist, [ cmsgProSourceStr ] ),
               format( cmsgProDBFileNotExist, [ GetDispStr( tmpPO ), DBType, cmsgProSourceNotExist ] )
                 );
    MsgDlgAttention( self );
    exit;
  end;

  result := true;

end;

function TfrmProfiles.GetDispStr( tmpPO : TProfileObj ) : string;
begin
  result := tmpPO.fName + strif( tmpPO.fIsDB, cDefaultDBProfileIsDBStr, cDefaultDBProfileIsDBStrNot );
end;

procedure TfrmProfiles.actMergeToExecute( Sender : TObject );
var
  FromPO, ToPO: TProfileObj; //FromPO just pointer, no free
  FromIS, DestIS : TInfoServer;

  function ServerInit( var anIS : TInfoServer; aPO : TProfileObj ) : boolean;
  begin

    result := false;

    anIS := TInfoServer.Create;

    if not anIS.Init( aPO.GetExpandedPath( ProgDefaultPath ), aPO.fName, aPO.fIsDB ) then
    begin
      Showmessage( 'TfrmProfiles.actMergeTo ServerInit: ' + cmsgProNoInitialize );
      exit;
    end;

    result := true;

  end;

begin

  FromPO := TProfileObj( lbList.Items.Objects[ lbList.Itemindex ] );
  if not assigned( FromPO ) or not SourceExists( FromPO ) then
    exit;

  FromIS := nil;
  DestIS := nil;
  ToPO := TProfileObj.Create;
  try
    if not Merge_NameAccepted( ToPo ) then
      exit;

    if ToPo.IsDB then
    begin
      MsgDlgMessage( ccapApology, cmsgApology, 'cmsgApology' );
      if MsgDlgConfirmation( self ) = mrNo then
        exit;
    end;

    MsgDlgMessage( ccapProMergeConfirm, format( cmsgProMergeConfirm, [ GetDispStr( FromPO ), GetDispStr( ToPO ) ] ) );
    if MsgDlgConfirmation( self ) = mrNo then
      exit;

    Screen.Cursor := crHourglass;

    if not ServerInit( FromIS, FromPO ) then
      exit;

    if not ServerInit( DestIS, ToPO ) then
      exit;

//======================================
//command objects
//======================================
    if not TCmdObj.MergeCommands( FromIS, DestIS, GetDispStr( FromPO ) ) then
      Showmessage( format( cmsgProMergeCompareProblem, [ ccapSelectProfileType_Merge ] ) );

//======================================
//Lists
//======================================
    if not TCmdListObj.MergeLists( dbltKeyWord, FromIS, DestIS ) then
      Showmessage( format( cmsgProMergeCompareProblem, [ ccapSelectProfileType_Merge ] ) );


    DestIS.SaveAll;
    DestIS.UnInitialize( false );
    FromIS.UnInitialize( false );

  finally //outer main try
    Screen.Cursor := crDefault;

    if assigned( DestIS ) then
      FreeAndNil( DestIS );

    if assigned( FromIS ) then
      FreeAndNil( FromIS );

    if assigned( ToPO ) then
      FreeAndNil( ToPO );
  end;

  ShowMessage( cmsgProMergeSuccess );

end;


function TfrmProfiles.MergeOne( CO : TCmdObj; Mergesource : string ) : string;
var
  DestPO: TProfileObj; //just pointer, no free
  DestIS : TInfoServer;

  function ServerInit( var anIS : TInfoServer ) : boolean;
  begin

    result := false;

    anIS := TInfoServer.Create;

    if not anIS.Init( DestPO.GetExpandedPath( ProgDefaultPath ), DestPO.fName, DestPO.fIsDB ) then
    begin
      Showmessage( 'TfrmProfiles.MergeOne: ' + cmsgProNoInitialize );
      exit;
    end;

    result := true;

  end;

begin

  result := '';
  DestPO := nil;

  if not assigned( CO ) then
  begin
    result := cmsgProCONotAssigned;
    exit;
  end;

  DestPO := TProfileObj( lbList.Items.Objects[ lbList.Itemindex ] );
  if not assigned( DestPO ) then
  begin
    result := cmsgProPONotAssigned;
    exit;
  end;

  if not SourceExists( DestPO, cmsgProDestStr ) then
    exit;

  DestIS := nil;
  try

    Screen.Cursor := crHourglass;

    if not ServerInit( DestIS ) then
      exit;

    result := TCmdObj.MergeCommand( DestIS, CO, MergeSource );

    if result <> '' then
      TCmdListObj.MergeListsOne( dbltKeyWord, DestIS, CO )
    else result := '<unknown>';

    DestIS.SaveAll;
    DestIS.UnInitialize( false );

  finally
    Screen.Cursor := crDefault;

    if assigned( DestIS ) then
      FreeAndNil( DestIS );
  end;

end;



function TfrmProfiles.HandleTextEdit( PO : TProfileObj; const CurrPath, CurrName : string ) : boolean;
var
  SL : TStringList;
begin

  result := true;

  if not IniDBFilesExist( Currpath, CurrName ) then
    exit;

  SL := TStringList.Create;
  try

    //Check Destination for same files already existing
    if GetIniDBFiles( SL, PO.GetExpandedPath( ProgDefaultPath ), PO.fName, false ) > 0 then
    begin
      if not DestinationCleanUp( SL, CurrPath, CurrName ) then
      begin
        result := false;
        exit;
      end;
    end;

    GetIniDBFileFromToList( SL, CurrPath, PO.GetExpandedPath( ProgDefaultPath ), CurrName, PO.fName );

    if not ProcessTextFiles( SL, true ) then
      result := false;

  finally
    SL.free;
  end;

end;


function TfrmProfiles.HandleSqlEdit( PO : TProfileObj; const CurrFileName : string ) : boolean;
var
  NewFileName : String;
begin

  result := true;
  if FileExists( CurrFileName ) then
  begin
    NewFileName := GenerateDBFilePath(
                     PO.GetExpandedPath( ProgDefaultPath ),
                     PO.fName,
                     cSqlDBExtension
                                     );

    if not DestinationCleanUp( CurrFileName, NewFileName ) then
    begin
      result := false;
      exit;
    end;

    if not RenameSuccess( CurrFileName, NewFileName ) then
      result := false;

  end;

end;

function TfrmProfiles.DestinationCleanup( Strings : TStrings; const Path, ProfileName : string ) : boolean;
var
  i : Integer;
begin

  result := true;
  MsgDlgMessage( format( ccapProDestFilesExist, [ cmsgProDestStr ] ), format( cmsgProDestFilesExist, [ Path, ProfileName ] ) );
  if MsgDlgConfirmation( self ) = mrNo then
  begin
    result := false;
    exit;
  end;

  for i := 0 to Strings.Count - 1 do
    if not DeleteSuccess( Strings[ i ] ) then
    begin
      result := false;
      exit;
    end;

end;

function TfrmProfiles.DestinationCleanup( const FromF, ToF : string ) : boolean;
begin
  result := true;
  if fileexists( ToF ) then
  begin

    MsgDlgMessage( format( ccapProDestFileExists, [ cmsgProDestStr ] ), format( cmsgProDestFileExists, [ FromF, ToF ] ) );
    if MsgDlgConfirmation( self ) = mrNo then
    begin
      result := false;
      exit;
    end;

    if not DeleteSuccess( ToF ) then
    begin
      result := false;
      exit;
    end;

  end;

end;


function TfrmProfiles.ProcessTextFiles( Strings : TStrings; const IsRename : boolean ) : boolean;
var
  i : Integer;
  Idx : SizeInt;
  Success : Boolean;
  FromF , ToF: String;
begin
  result := true;
  for i := 0 to Strings.Count - 1 do
  begin

    Idx := pos( csoNonPrintingDelimiter, Strings[ i ] );
    if Idx = 0 then
      raise EErrorDevelopment.create( 'TfrmProfiles.ProcessTextFiles: Text List has invalid format.' );

    FromF := copy( Strings[ i ], 1, Idx - 1 );
    ToF := copy( Strings[ i ], Idx + 1, MAXINT );

    if IsRename then
      Success := RenameSuccess( FromF, ToF )
    else
    begin
      Success := CopySuccess( FromF, ToF );
      if Success then
        InfoServer.UpdateGUID( ToF, false )
    end;

    if not Success then
    begin
      result := false;
      exit;
    end;

  end;
end;

function TfrmProfiles.CopySuccess( const FromF, ToF : string ) : boolean;
begin
  result := Copyfile( FromF, ToF );
  if not result then
  begin
    MsgDlgMessage( ccapFileError, format( cmsgFileErrorCopy, [ FromF, ToF ] ) + cmsgMostLikely );
    MsgDlgInfo( self );
  end;
end;

function TfrmProfiles.RenameSuccess( const FromF, ToF : string ) : boolean;
begin
  result := Renamefile( FromF, ToF );
  if not result then
  begin
    MsgDlgMessage( ccapFileError, format( cmsgFileErrorMove, [ FromF, ToF ] ) + cmsgMostLikely );
    MsgDlgInfo( self );
  end;
end;

procedure TfrmProfiles.SetAllowDefSql( AValue : boolean );
begin
  if fAllowDefSql = AValue then Exit;
  fAllowDefSql := AValue;
end;

procedure TfrmProfiles.SetAllowDefText( AValue : boolean );
begin
  if fAllowDefText = AValue then Exit;
  fAllowDefText := AValue;
end;

function TfrmProfiles.DeleteSuccess( const FileName : string ) : boolean;
begin
  result := deletefile( FileName );
  if not Result then
  begin
    MsgDlgMessage( ccapFileError, format( cmsgFileErrorDelete, [ FileName ] ) + cmsgMostLikely );
    MsgDlgInfo( self );
  end;
end;

procedure TfrmProfiles.WriteList( const ProfileEntry, Path : string; DeleteMe : string = '' );
begin

  if DeleteMe <> '' then
    IFile.DeleteKey( cSectTabDBProfiles, DeleteMe );

  if ProfileEntry <> '' then
    IFile.WriteString( cSectTabDBProfiles, ProfileEntry, Path );

  IFile.UpdateFile;

  lbListClick( Self );

end;

procedure TfrmProfiles.ReleaseDBProfiles;
var
  i : Integer;
begin
  for i := 1 to fHeaderCount do
  begin
    lbList.Items.Objects[ 0 ] := nil;
    lbList.Items.Delete( 0 );
  end;
end;

procedure TfrmProfiles.RefreshList( const IndexOn : string );
begin

  ReleaseDBProfiles;

  lbList.Sorted := true;
  lbList.Sorted := false;

  AdjustList;

  lbList.ItemIndex := lbList.Items.IndexOf( IndexOn );
  popMPopup( Self );


end;

procedure TfrmProfiles.FillList;
var
  PO : TProfileObj;
  TrimIsDB, TrimIsDBNot : String;
  i : Integer;
  IsDB : boolean;
  SL : TStringList;

  function IsValid( Str : string ) : boolean;
  begin
    IsDB := pos( TrimIsDB, Str ) > 0;
    result := IsDB or ( pos( TrimIsDBNot, Str ) > 0 );
  end;

begin

  if not assigned( Ifile ) then
    raise EErrorDevelopment.create( 'TfrmProfiles.FillList: IFile not assigned.' );

  SL := TStringlist.Create;
  try

    GetRawProfileList( IFile, SL );

    if SL.Count > 0 then
      SL.Sort
    else exit;

    TrimIsDB := trim( cDefaultDBProfileIsDBStr );
    TrimIsDBNot := trim( cDefaultDBProfileIsDBStrNot );
    for i := 0 to SL.Count - 1 do
    begin
      if not IsValid( SL[ i ] ) then
      begin
  //for the rare case that someone hand edited the .formsettings ini file by hand, incorrectly
        Showmessage( format( cmsgProPONameInvalid, [ SL[ i ] ] ) );
        continue;
      end;
      PO := TProfileObj.Create;
      PO.fName := ExtractProfileName( SL[ i ] );
      PO.fIsDB := IsDB;
      PO.fPath := IFile.ReadString( cSectTabDBProfiles, SL[ i ], constDefaultPathValue );
      lbList.Items.AddObject( SL[ i ], PO );
    end;
  finally
    SL.free;
  end;

end;

function TfrmProfiles.Copy_ProfileNameCheck( const CheckStr : string ) : boolean;
var
  i : Integer;
  PO : TProfileObj;
begin
  result := false;

  for i := fHeaderCount to lbList.Items.Count - 1 do
  begin
    PO := TProfileObj( lbList.Items.Objects[ i ] );
    if not assigned( PO ) then
      continue;
    if PO.fName = CheckStr then
    begin
      result := true;
      break;
    end;
  end;

end;

function TfrmProfiles.ProfileNameAlreadyUsed( const CheckStr : string; NameOnly : boolean = false ) : boolean;
begin
  result := false;

  if NameOnly then
  begin
    result := Copy_ProfileNameCheck( CheckStr );
    exit;
  end;

//  listboxes don't have case sensitive "indexOf" so I use a stringlist
  fTrueCaseSensitive.Assign( lbList.Items );
  fIdxDB := fTrueCaseSensitive.IndexOf( CheckStr + cDefaultDBProfileIsDBStr );
  fIdxText := fTrueCaseSensitive.IndexOf( CheckStr + cDefaultDBProfileIsDBStrNot );
  result := ( fIdxDB > -1 ) and ( fIdxText > -1 );
end;


function TfrmProfiles.Reserved( const CheckStr : string ) : boolean;

  function GetMsg( TheMsg : string ) : string;
  begin
    result := format( cmsgProReservedName, [ TheMsg ] )
  end;

begin
  result :=
    IsReservedWord( CheckStr, cDefaultDBProfileName, GetMsg( cDefaultDBProfileName ) )
    or IsReservedPhrase( CheckStr, UpperCase( trim( cDefaultDBProfileIsDBStrNot ) ), GetMsg( cDefaultDBProfileIsDBStrNot ) )
    or IsReservedPhrase( CheckStr, UpperCase( trim( cDefaultDBProfileIsDBStr ) ), GetMsg( cDefaultDBProfileIsDBStr ) );
end;

function TfrmProfiles.IsAllowed( var CheckStr : string; NameOnly : boolean = false ) : boolean;

  function GetMsg( TheMsg : string ) : string;
  begin
    result := format( cmsgProReservedName, [ TheMsg ] )
  end;

begin
  result := true;

  if Reserved( CheckStr ) then
  begin
    result := false;
    CheckStr := '';
    exit;
  end;

  if ProfileNameAlreadyUsed( CheckStr, NameOnly ) then
  begin
    MsgDlgMessage( format( ccapListManagerDuplicate, [ ccapGenericProfile + ' Name' ] ),
                   format( cmsgListManagerDuplicate, [ ccapGenericProfile, CheckStr ] )
                  );
    MsgDlgInfo( self );
    result := false;
    CheckStr := '';
  end;

end;

function TfrmProfiles.Convert_IsAllowed( var CheckStr : string ) : boolean;
begin
  result := true;

  if Reserved( CheckStr ) then
  begin
    result := false;
    CheckStr := '';
    exit;
  end;

end;

function TfrmProfiles.GetAdjustedProfileName( const ProfName : string; const isDB : boolean ) : string;
begin
  Result := ProfName + Strif( isDB, cDefaultDBProfileIsDBStr, cDefaultDBProfileIsDBStrNot );
end;

function TfrmProfiles.NameAccepted( const aCaption : string; var Str : string; NameOnly : boolean = false ) : boolean;
begin
  result := false;

  while True do
  begin

    if not DoSingleInput( aCaption, Str, simEdit, self, false ) then
      exit;

    if not IsAllowed( Str, NameOnly ) then
      Continue
    else break;

  end;

  result := true;

end;

function TfrmProfiles.Convert_NameAccepted( const aCaption : string; var Str : string; const Path : string; const IsDB : boolean ) : boolean;
var
  IsUsed : Boolean;
begin

  result := false;

  while True do
  begin

    if not DoSingleInput( aCaption, Str, simEdit, self, false ) then
      exit;

    if not Convert_IsAllowed( Str ) then
      Continue;
    //else break;

    if IsDB then
      IsUsed := FileExists( GenerateDBFilePath( Path, Str, cSqlDBExtension ) )
    else IsUsed := IniDBFilesExist( path, Str, false );

    if IsUsed then
    begin
      MsgDlgMessage( ccapProNoOverWrite,
                 format( cmsgProNoOverWrite, [ strif( IsDb, cmsgProDBsqlStr, cmsgProDBTextStr ) ] )
                   );
      MsgDlgAttention( self );
      continue;
    end else break;

  end;

  result := true;

end;

function TfrmProfiles.Merge_NameAccepted( aPO : TProfileObj ) : boolean;
var
  i : Integer;
begin

  result := false;

  with TfrmProfilesMerge.Create( self ) do
  try

    Caption := Format( ccapSelectProfileType, [ ccapSelectProfileType_Merge ] );

    for i := 0 to lbList.Items.Count - 1 do
    begin
      if not assigned( lbList.Items.Objects[ i ] )
         or CurrentDB( false, i )
         or ( i = lbList.ItemIndex  ) then
        continue;
      lbMergeList.Items.AddObject( lbList.Items[ i ], lbList.Items.Objects[ i ]  );
    end;

    lbMergeList.Sorted := true;
    lbMergeList.Sorted := false;

    ShowModal;

    if ModalResult = mrOK then
    begin
      if assigned( ResultProfile ) then
      begin
        aPO.Assign( TProfileObj( ResultProfile ) );
        result := true;
      end
      else
      begin
        if trim( SelectedDest ) <> '' then
        begin
          MsgDlgMessage(
            format( ccapProDBFileNotExist, [ cmsgProDestStr ] ),
            format( cmsgProDBFileNotExist,
              [ SelectedDest,
                strif( SelectedIsSQL, cmsgProDBsqlStr, cmsgProDBTextStr ),
                cmsgProDestNotExist
               ]
                  )
                       );
          MsgDlgAttention( self );
        end;
      end;
    end;

  finally
    free;
  end;

end;

function TfrmProfiles.Compare_NameAccepted( aPO : TProfileObj ) : boolean;
var
  i : Integer;
begin

  result := false;

  with TfrmProfilesMerge.Create( self ) do
  try

    Caption := Format( ccapSelectProfileType, [ ccapSelectProfileType_Compare ] );

    for i := 0 to lbList.Items.Count - 1 do
    begin
      if not assigned( lbList.Items.Objects[ i ] )
         or ( i = lbList.ItemIndex  ) then
        continue;
      lbMergeList.Items.AddObject( lbList.Items[ i ], lbList.Items.Objects[ i ]  );
    end;

    lbMergeList.Sorted := true;
    lbMergeList.Sorted := false;

    ShowModal;

    if ModalResult = mrOK then
    begin
      if assigned( ResultProfile ) then
      begin
        aPO.Assign( TProfileObj( ResultProfile ) );
        result := true;
      end
      else
      begin
        if trim( SelectedDest ) <> '' then
        begin
          MsgDlgMessage(
            format( ccapProDBFileNotExist, [ cmsgProDestStr ] ),
            format( cmsgProDBFileNotExist,
              [ SelectedDest,
                strif( SelectedIsSQL, cmsgProDBsqlStr, cmsgProDBTextStr ),
                cmsgProDestNotExist
               ]
                  )
                       );
          MsgDlgAttention( self );
        end;
      end;
    end;

  finally
    free;
  end;

end;


procedure TfrmProfiles.actAddExecute(Sender : TObject);
var
  Str : String = '';
  PO : TProfileObj;
  AddStr : String;
begin

  if not NameAccepted( format( ccapGenericAdd, [ ccapGenericProfile ] ), Str ) then
    exit;

  with TfrmManageProfile.Create( self ) do
  try
    Mode := mpAdd;
    SetupLabels( Str, constDefaultPathDisplay );
    ApplyTranslation;
    AdjustDBType( fIdxDB, fIdxText );

    ShowModal;
    if ModalResult = mrOK then
    begin
      PO := TProfileObj.Create;
      PO.fName := Str;
      PO.fIsDB := IsSqlDB = 1;//rgType.ItemIndex = 0;
      PO.fPath := PathDisplayToPathValue( edtProfilePath.Text );// strif( lblProfilePath.Caption = constDefaultPathDisplay, constDefaultPathValue, lblProfilePath.Caption );
      AddStr := GetAdjustedProfileName( Str, PO.fIsDB );

      lbList.Items.AddObject( AddStr, PO );
      WriteList( AddStr, PO.fPath );
      RefreshList( AddStr );

    end;

  finally
    free;
  end;

  popMPopup( self );

end;

procedure TfrmProfiles.FormActivate(Sender : TObject);
begin

  if CurrProfileName = '' then
  begin
    ShowMessage( 'Hey Programmer! You forgot to set current name, and possibly dbtype. Canceling.' );
    btnCancel.Click;
    exit;
  end;

  if not FIsInitialized then
  begin

    lblCurrentProfile.Caption := format( cmsgCurrentProfile, [ fCurrProfileNameFormatted ] );

    FIsInitialized := true;

    FCanClose := false;
    popMPopup( Self );

    if not IsSelectMode then
      if MsgDlgMessage( ccapGeneralInformation,
                        format( cmsgProfilesInformation, [ NumberOfInifiles, GetDBNameList( 'Bob' ) ] ),
                        'cmsgProfilesInformation'
                        ) then
        MsgDlgInfo( self );

  end;

end;

procedure TfrmProfiles.FormCloseQuery(Sender : TObject; var CanClose : boolean);
begin
  CanClose := FCanClose;
end;

procedure TfrmProfiles.FormCreate(Sender : TObject);

  procedure FillProf( Prof : TProfileObj; IsDB : boolean );
  begin
    Prof.fName := cDefaultDBProfileName;
    Prof.fIsDB := IsDB;
    Prof.fPath := constDefaultPathValue;
    Prof.fIsDefault := true;
  end;

begin
  ApplyChangeFont( Self );
  FHasShown := false;
  FIsInitialized := false;
  fConsolidateMode := false;
  fCurrProfileIsDB := cDefaultDBProfileIsDB;
  fSelectedProfileIsDB := cDefaultDBProfileIsDB;
  fCurrProfileName := '';
  fCurrProfilePath := constDefaultPathValue;
  fTrueCaseSensitive := TStringList.Create;
  fTrueCaseSensitive.CaseSensitive := true;
  fIsSelectMode := false;

  actAdd.Caption := '&A  ' + cbtn_Add;
  actEdit.Caption := '&B  ' + cbtn_Edit;
  actCopy.Caption := '&C  ' + cbtn_Copy;
  actDelete.Caption := '&D  ' + cbtn_Delete;

  f_DB_Profile := TProfileObj.Create;
  FillProf( f_DB_Profile, true );

  f_Text_Profile := TProfileObj.Create;
  FillProf( f_Text_Profile, false );


end;

procedure TfrmProfiles.FormDestroy( Sender : TObject );
begin

  ReleaseDBProfiles;
  FreeStringListWithObjects( lbList.Items );

  fIFile := nil; //pointer only

  if assigned( fTrueCaseSensitive ) then
    FreeAndNil( fTrueCaseSensitive );

  if assigned( f_DB_Profile ) then
    FreeAndNil( f_DB_Profile );

  if assigned( f_Text_Profile ) then
    FreeAndNil( f_Text_Profile );

end;

end.

