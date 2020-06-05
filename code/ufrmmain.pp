unit ufrmMain;

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
  Classes, SysUtils, LazFileUtils, Forms, Controls, Graphics, Dialogs,// uCmdBox,
  StdCtrls
  , ExtCtrls, ActnList, unitsharedobj
  , lcltype {THis is needed for key up keyboard constants}
  , regexpr //this is the Regexpression unit
  , Menus, HintFrame
  , lazutils, Buttons
  , uSingleInput
  , unitcommands, ComCtrls, ExtendedNotebook
  , unitGlobForm, Types
  , ufrmBusy
  , ufrmAbout
  , JIniFiles
  , unitSearch
  , unitDBConstants
  , AsyncProcess
  ;

const
  cUseItemIndexFlag = -2;

type



  { TfrmMain }

  TfrmMain = class(TForm)
    actCommandDelete: TAction;
    actCommandUndelete: TAction;
    actCmdLineDelete: TAction;
    actCmdLineUnDelete: TAction;
    actCmdLineUp: TAction;
    actCmdLineDown: TAction;
    actCopyCmdLine : TAction;
    actCopyClipCmdLine : TAction;
    actCopyCmd : TAction;
    actCopyClipCmd : TAction;
    actFindCmd : TAction;
    actFindCmdLine : TAction;
    actExit : TAction;
    actAbout : TAction;
    actOutPutCopy : TAction;
    actOutPutSave : TAction;
    actOutPutClear : TAction;
    actOutputWrap : TAction;
    actSearchRun : TAction;
    actSearchGoToCmdLine : TAction;
    actSearchFindCmdLine : TAction;
    actSearchGoToCmd : TAction;
    actSearchFindCmd : TAction;
    actSearchDisplaySearch : TAction;
    actSearchLoadSearch : TAction;
    actSearchSaveSearch : TAction;
    actSearchNewSearch : TAction;
    actSearchSearch : TAction;
    actSearchDisplayKeyCmd : TAction;
    actSearchLoadKeyCmd : TAction;
    actSearchSaveKeyCmd : TAction;
    actSearchNewKeyCmd : TAction;
    actSearchKeyCmd : TAction;
    actSwitchDB : TAction;
    actProfiles : TAction;
    actQuickRun : TAction;
    actOptions : TAction;
    actSave : TAction;
    actManageKeyWords: TAction;
    actNewCommandLine: TAction;
    actRun: TAction;
    actRevert: TAction;
    actPlus: TAction;
    ActionList1: TActionList;
    Bevel1 : TBevel;
    Bevel2 : TBevel;
    Bevel3 : TBevel;
    Bevel4 : TBevel;
    Bevel5 : TBevel;
    btnBuilder : TBitBtn;
    btnExit : TBitBtn;
    btnCmdLineDelete : TBitBtn;
    btnCmdLineUnDelete : TBitBtn;
    btnMainClear : TBitBtn;
    btnMainSave : TBitBtn;
    btnMainCopy : TBitBtn;
    btnRefreshFavorites : TBitBtn;
    btnSearchRun : TBitBtn;
    btnSimpleSearch : TBitBtn;
    btnProfileManagement : TBitBtn;
    btnFindCmdLine : TBitBtn;
    btnAbout : TBitBtn;
    btnKeyWords : TBitBtn;
    btnOptions : TBitBtn;
    btnThreatLevelInfo : TButton;
    btnThreatLevelInfoLine : TButton;
    btnVersionCommand : TBitBtn;
    btnPlus : TBitBtn;
    btnNewCommandLine : TBitBtn;
    btnHelpCommand : TBitBtn;
    btnRun_Test : TBitBtn;
    btnSortCommands : TBitBtn;
    btnRevert : TBitBtn;
    btnCommandUnDelete : TBitBtn;
    btnCommandDelete : TBitBtn;
    btnFindCmd : TBitBtn;
    btnSwitchDB : TBitBtn;
    btnCancelRun : TBitBtn;
    btnSave : TBitBtn;
    btnCmdCancel: TBitBtn;
    btnCmdEdit: TBitBtn;
    btnCmdLineDown: TButton;
    btnCmdLineUp: TButton;
    btnCmdOk: TBitBtn;
    btnSearchFindCmd : TButton;
    btnSearchFindCmdLine : TButton;
    btnSearchGoToCmd : TButton;
    btnSearchGoToCmdLine : TButton;
    btnSearchDisplaySearch : TButton;
    btnSearchLoadSearch : TButton;
    btnSearchNewSearch : TButton;
    btnKeyWordSave : TButton;
    btnKeyWordDisplay : TButton;
    btnKeyWordNewSearch : TButton;
    btnSearchSaveSearch : TButton;
    btnSearchSearch : TButton;
    btnKeyWordDelete: TButton;
    btnLineCancel: TBitBtn;
    btnLineEdit: TBitBtn;
    btnLineOk: TBitBtn;
    btnLocationPath : TBitBtn;
    btnSortDetachedProcesses : TButton;
    btnQuickRun : TButton;
    btnKeyWordSearch : TButton;
    btnKeyWordAdd: TButton;
    btnHaltProcess : TButton;
    btnKeyWordLoad : TButton;
    cbDetachProcess: TCheckBox;
    cbDetachProcessLine: TCheckBox;
    cbDispThreatLevel : TComboBox;
    cbIsFavorite: TCheckBox;
    cbIsFavoriteLine: TCheckBox;
    cbTerminalOnlyLine : TCheckBox;
    cbAlertLine : TCheckBox;
    cbTerminalOnly : TCheckBox;
    cbThreatLevel : TComboBox;
    cbThreatLevelDisp : TComboBox;
    cbThreatLevelLine : TComboBox;
    cbThreatLevelLineDisp : TComboBox;
    cbWordWrapMain : TCheckBox;
    cbWantsInputLine : TCheckBox;
    cbSuperUser: TCheckBox;
    cbSuperUserLine: TCheckBox;
    cbUseShellLine : TCheckBox;
    edtFriendlyNameLine : TEdit;
    edtHelp: TEdit;
    edtVersion: TEdit;
    lblcapFriendlyName : TLabel;
    lblCommandName : TLabel;
    lblCommandNameDisp : TLabel;
    lblCurrDB : TLabel;
    lblDispAlert : TLabel;
    lblDispThreatLevel : TLabel;
    lblNoOptions : TLabel;
    lblDispTerminalOnly : TLabel;
    lblDispIsFavorite : TLabel;
    lblDispUseShell : TLabel;
    lblDispDetachProcess : TLabel;
    lblDispWantsInput : TLabel;
    lblPathAlias : TLabel;
    lblPathAliasDisp : TLabel;
    lblTabSearch : TLabel;
    lblTabKeyWords : TLabel;
    lblTerminalOnlyDisp : TLabel;
    lblDetachProcessLineDisp : TLabel;
    lblAlertLineDisp : TLabel;
    lblDetachProcessDisp : TLabel;
    lblSuperUserDisp : TLabel;
    lblIsFavoriteDisp : TLabel;
    lblDispSuperUser : TLabel;
    lblTerminalOnlyLineDisp : TLabel;
    lblThreatLevelDisp : TLabel;
    lblThreatLevelLineDisp : TLabel;
    lblUseShellLineDisp : TLabel;
    lblSuperUserLineDisp : TLabel;
    lblSRCmds : TLabel;
    lblSRCmdLines : TLabel;
    lblIsFavoriteLineDisp : TLabel;
    lblWantsInputLineDisp : TLabel;
    lbSearchCmd : TListBox;
    lbSearchCmdLine : TListBox;
    lblKeyWords: TLabel;
    lblCmdPointer: TLabel;
    lblCmdLinePointer: TLabel;
    lblcapEntryLine: TLabel;
    lblPestr : TLabel;
    lblHelpDisp : TLabel;
    lblCapNotesLine : TLabel;
    lblcapFriendlyNameLine : TLabel;
    lblCEditing : TLabel;
    lblFriendlyNameLineDisp : TLabel;
    lblVersionDisp : TLabel;
    lbKeywordsDisp : TListBox;
    lblDispFriendlyName : TLabel;
    lblDispCommandName : TLabel;
    lblDispEntry : TLabel;
    lblDetachedProcesses : TLabel;
    lbDetachedProcesses : TListBox;
    lblPathActual: TLabel;
    lblHelp: TLabel;
    lblShowButtons: TLabel;
    lblVersion: TLabel;
    lbCmdLines: TListBox;
    lbCommands: TListBox;
    lbKeywords: TListBox;
    lbDispKeywords : TListBox;
    memNotes: TMemo;
    memNotesDisp : TMemo;
    memNotesLine: TMemo;
    memNotesLineDisp : TMemo;
    Memo1: TMemo;
    memDetachedProcesses : TMemo;
    memDispNotes : TMemo;
    memEntry : TMemo;
    MenuItem1 : TMenuItem;
    mniOutPutWrap : TMenuItem;
    mniOutPutClear : TMenuItem;
    mniOutPutSave : TMenuItem;
    mniOutputCopy : TMenuItem;
    mniOutput : TMenuItem;
    mniCmdSendTo : TMenuItem;
    pnlCEdit : TPanel;
    pnlRefreshFavorites : TPanel;
    pnlS : TPanel;
    popCmdPasteCmdLine : TMenuItem;
    popCmdPaste : TMenuItem;
    popCmdLinePaste : TMenuItem;
    mniSearchFindCmdLine : TMenuItem;
    mniSearchGotoCmdLine : TMenuItem;
    mniSearchCmdLineItemClip : TMenuItem;
    mniSearchCmdLineListClip : TMenuItem;
    mniSearchRun : TMenuItem;
    mniSearchGoToCmd : TMenuItem;
    mniSearchFindCmd : TMenuItem;
    mniSearchCmdListClip : TMenuItem;
    mniTabSearchSearch : TMenuItem;
    mniTabSearchSearchDisplay : TMenuItem;
    mniTabKeyWordsSearchLoad : TMenuItem;
    mniTabSearchSearchLoad : TMenuItem;
    mniTabSearchSearchNew : TMenuItem;
    mniTabKeyWordsSearchSave : TMenuItem;
    mniTabKeyWordsSearchDisplay : TMenuItem;
    MenuItem6 : TMenuItem;
    mniTabKeyWordsSearchNew : TMenuItem;
    mniTabKeyWordsSearch : TMenuItem;
    mniCmdCount : TMenuItem;
    mniCopyCLListClip : TMenuItem;
    mniCopyCmdListClip : TMenuItem;
    mniSwitchDB : TMenuItem;
    MenuItem2 : TMenuItem;
    mniProfiles : TMenuItem;
    mniTabSearchSearchSave : TMenuItem;
    pnlC : TPanel;
    pnlCL : TPanel;
    pnlSearchResults : TPanel;
    pmiMainOptions : TMenuItem;
    pmiMainSave : TMenuItem;
    MenuItem13 : TMenuItem;
    MenuItem14 : TMenuItem;
    pmiMainExit : TMenuItem;
    MenuItem3 : TMenuItem;
    pmiMainAbout : TMenuItem;
    pmiMainKeyWords : TMenuItem;
    pmiFindCmdLine : TMenuItem;
    MenuItem5 : TMenuItem;
    pmiFindCmd : TMenuItem;
    pnlCLEdit : TPanel;
    pnlDispCmdLine : TPanel;
    pnlDispCommand : TPanel;
    pnlDisplayObj : TPanel;
    pmiCopyClipCmd : TMenuItem;
    pmiCopyCmd : TMenuItem;
    pmiCopyClipCmdLine : TMenuItem;
    pmiCopyCmdLine : TMenuItem;
    nbCommands: TExtendedNotebook;
    FrameHint1: TFrameHint;
    lblTabFavorites: TLabel;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    pnlCommandList: TPanel;
    pnlCmdLine: TPanel;
    pnlCmdLines: TPanel;
    pnlcommand: TPanel;
    pnlEdit: TPanel;
    pnlLineControls: TPanel;
    popSearchCmdLine : TPopupMenu;
    popSearchCmd : TPopupMenu;
    popNewCommandLine: TMenuItem;
    popRun: TMenuItem;
    popCmdLineUp: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    popCmdLineDown: TMenuItem;
    popCmdLineUnDelete: TMenuItem;
    popCmdLIneDelete: TMenuItem;
    popRevert: TMenuItem;
    popPlusBrowse: TMenuItem;
    popCommandUnDelete: TMenuItem;
    popCommandDelete: TMenuItem;
    pmCommands: TPopupMenu;
    pmCmdLines: TPopupMenu;
    pmMain : TPopupMenu;
    popTabKeyWords : TPopupMenu;
    popTabSearch : TPopupMenu;
    shpCmdLineOut1 : TShape;
    shpCmdOut1 : TShape;
    shpCmdOut10 : TShape;
    shpCmdOut2 : TShape;
    shpCmdOut3 : TShape;
    shpCmdOut4 : TShape;
    shpCmdOut5 : TShape;
    shpCmdOut6 : TShape;
    shpCmdOut7 : TShape;
    shpCmdOut8 : TShape;
    shpCmdOut9 : TShape;
    shpCurrProfile : TShape;
    shpSRL : TShape;
    shpSRR : TShape;
    shpCmdIn : TShape;
    shpCmdLineIn : TShape;
    shpCmdOut : TShape;
    shpRun_Test : TShape;
    shpSRR1 : TShape;
    TimerBlink : TTimer;
    tsKeyWords : TTabSheet;
    tsSearch: TTabSheet;
    TimerDetachedProcesses : TTimer;
    tsDetachedProcesses : TTabSheet;
    tsCommands: TTabSheet;
    tsFavorites: TTabSheet;
    procedure actAboutExecute( Sender : TObject );
    procedure actCmdLineDeleteExecute(Sender: TObject);
    procedure actCmdLineDownExecute(Sender: TObject);
    procedure actCmdLineUnDeleteExecute(Sender: TObject);
    procedure actCmdLineUpExecute(Sender: TObject);
    procedure actCommandDeleteExecute(Sender: TObject);
    procedure actCommandUndeleteExecute(Sender: TObject);
    procedure actCopyClipCmdExecute(Sender : TObject);
    procedure actCopyClipCmdLineExecute(Sender : TObject);
    procedure actCopyCmdExecute(Sender : TObject);
    procedure actCopyCmdLineExecute(Sender : TObject);
    procedure actExitExecute( Sender : TObject );
    procedure actFindCmdExecute( Sender : TObject );
    procedure actFindCmdLineExecute( Sender : TObject );
    procedure actOptionsExecute( Sender : TObject );
    procedure actOutPutClearExecute( Sender : TObject );
    procedure actOutPutCopyExecute( Sender : TObject );
    procedure actOutPutSaveExecute( Sender : TObject );
    procedure actOutputWrapExecute( Sender : TObject );
    procedure actProfilesExecute( Sender : TObject );
    procedure actQuickRunExecute( Sender : TObject );
    procedure actManageKeyWordsExecute(Sender: TObject);
    procedure actNewCommandLineExecute(Sender: TObject);
    procedure actPlusExecute(Sender: TObject);
    procedure actRevertExecute(Sender: TObject);
    procedure actRunExecute( Sender: TObject );
    procedure actSaveExecute( Sender : TObject );
    procedure actSearchDisplayKeyCmdExecute( Sender : TObject );
    procedure actSearchDisplaySearchExecute( Sender : TObject );
    procedure actSearchFindCmdExecute( Sender : TObject );
    procedure actSearchFindCmdLineExecute( Sender : TObject );
    procedure actSearchGoToCmdExecute( Sender : TObject );
    procedure actSearchGoToCmdLineExecute( Sender : TObject );
    procedure actSearchKeyCmdExecute( Sender : TObject );
    procedure actSearchLoadKeyCmdExecute( Sender : TObject );
    procedure actSearchLoadSearchExecute( Sender : TObject );
    procedure actSearchNewKeyCmdExecute( Sender : TObject );
    procedure actSearchNewSearchExecute( Sender : TObject );
    procedure actSearchRunExecute( Sender : TObject );
    procedure actSearchSaveKeyCmdExecute( Sender : TObject );
    procedure actSearchSaveSearchExecute( Sender : TObject );
    procedure actSearchSearchExecute( Sender : TObject );
    procedure actSwitchDBExecute( Sender : TObject );
    procedure btnBuilderClick( Sender : TObject );
    procedure btnCancelRunClick( Sender : TObject );
    procedure btnCmdEditClick(Sender: TObject);
    procedure btnHaltProcessClick( Sender : TObject );
    procedure btnHelpCommandClick(Sender: TObject);
    procedure btnKeyWordAddClick(Sender: TObject);
    procedure btnKeyWordDeleteClick(Sender: TObject);
    procedure btnLocationPathClick(Sender: TObject);
    procedure btnRefreshFavoritesClick( Sender : TObject );
    procedure btnSortDetachedProcessesClick( Sender : TObject );
    procedure btnThreatLevelInfoClick(Sender: TObject);
    procedure btnThreatLevelInfoLineClick( Sender : TObject );
    procedure btnVersionCommandClick(Sender: TObject);
    procedure btnSimpleSearchClick(Sender: TObject);
    procedure btnSortCommandsClick(Sender: TObject);
    procedure cbSuperUserChange( Sender : TObject );
    procedure cbSuperUserLineChange(Sender : TObject);
    procedure cbThreatLevelChange(Sender: TObject);
    procedure cbThreatLevelLineChange( Sender : TObject );
    procedure edtFriendlyNameLineChange( Sender : TObject );
    procedure edtHelpChange( Sender : TObject );
    procedure edtVersionChange( Sender : TObject );
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure FormShow(Sender: TObject);
    procedure lbCmdLinesDblClick( Sender : TObject );
    procedure lbCmdLinesKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure lbCmdLinesSelectionChange(Sender: TObject; User: boolean);
    procedure lbCommandsClick(Sender: TObject);
    procedure lbCmdLinesClick(Sender: TObject);
    procedure lbCommandsKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure lbDetachedProcessesClick( Sender : TObject );
    procedure lbDetachedProcessesKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure lbKeywordsDispKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure lbKeywordsKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure lblCommandNameDblClick( Sender : TObject );
    procedure lblCommandNameDispDblClick( Sender : TObject );
    procedure lblCurrDBDblClick( Sender : TObject );
    procedure lblDispEntryDblClick( Sender : TObject );
    procedure lblDispFriendlyNameDblClick( Sender : TObject );
    procedure lblFriendlyNameLineDispDblClick( Sender : TObject );
    procedure lblPathAliasDblClick( Sender : TObject );
    procedure lblPathAliasDispDblClick( Sender : TObject );
    procedure lblThreatLevelDispDblClick( Sender : TObject );
    procedure lbSearchCmdClick( Sender : TObject );
    procedure lbSearchCmdKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure lbSearchCmdLineClick( Sender : TObject );
    procedure lbSearchCmdLineKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure memDetachedProcessesDblClick( Sender : TObject );
    procedure memDetachedProcessesKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure memDispNotesDblClick( Sender : TObject );
    procedure memDispNotesKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure memEntryKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure memNotesChange( Sender : TObject );
    procedure memNotesDispDblClick( Sender : TObject );
    procedure memNotesDispKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure memNotesEnter( Sender : TObject );
    procedure memNotesKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure memNotesLineChange( Sender : TObject );
    procedure memNotesLineDispDblClick( Sender : TObject );
    procedure memNotesLineDispKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure memNotesLineEnter( Sender : TObject );
    procedure memNotesLineKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure Memo1DblClick( Sender : TObject );
    procedure Memo1KeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure mniCmdCountClick( Sender : TObject );
    procedure mniCmdSendToClick( Sender : TObject );
    procedure mniCopyCLListClipClick( Sender : TObject );
    procedure mniCopyCmdListClipClick( Sender : TObject );
    procedure mniSearchCmdLineItemClipClick( Sender : TObject );
    procedure mniSearchCmdLineListClipClick( Sender : TObject );
    procedure mniSearchCmdListClipClick( Sender : TObject );
    procedure nbCommandsChange( Sender : TObject );
    procedure pmCmdLinesPopup(Sender: TObject);
    procedure pmCommandsPopup(Sender: TObject);
    procedure popCmdLinePasteClick( Sender : TObject );
    procedure popCmdPasteClick( Sender : TObject );
    procedure TimerBlinkStartTimer( Sender : TObject );
    procedure TimerBlinkStopTimer( Sender : TObject );
    procedure TimerBlinkTimer( Sender : TObject );
    procedure TimerDetachedProcessesTimer( Sender : TObject );
    { private declarations }
  private
    fDisplayOutPut : TStringList;
    fDisplayOutPutMax : integer;
    fKeyWordList: TcmdListObj;
    fCommandButtons: TList;
    fCommandLineButtons: TList;
    fCommandLineEditingControls : TList;
    fProfileName : String;
    fUseDB : Boolean;
    fSqliteLibrary : string;
    fProfilePath : string;
    fProfileGUID : string;
    fWritingToPath: string;
    fSavingToPath: string;
    fIsInitialized: boolean;
    fHasShown: boolean;
    fSuperUser: boolean;
    fRootFile : string;
    fLastlbCommandsIdx: integer;
    fLastlbCmdLinesIdx: integer;
    fMaxInOutPut : string;
    fLastQuickRun : string;
    fIsRunningProcess : boolean;
    fWarnUnspecified : boolean;
    fWarnHarmless : boolean;
    fWarnCareful : boolean;
    fWarnCaution : boolean;
    fWarnDanger : boolean;
    fAllowSqlDB : boolean;
    fAllowMultipleOpens : boolean;
    fAllowESCinOutput : boolean;
    fAllowTextDB : boolean;
    fDoShowSqlMissing : boolean;
    fManRefreshFavorites : boolean;
    fIFS : TJiniFile;//"I"nifile "F"orm "S"ettings
    fOpenInstances : integer;
    fFirstLocalRun : boolean;

////edit focusing
//when editing and you are typing and then click a checkbox or click a button focus is stolen and
//that is annoying. For now it refocuses to edit memos because refocusing, in QT at least, selects all
//all the text making it easy to accidentally delete it which is also annoying.
//if in future I figure out how to suppress that select all in TEdits, then use the fCLFocus / fCMDFocus
//setup and it will return to the appropriate field one was typing in.
//    fCLFocus : TWinControl; //do not create, pointer only
//    fCmdFocus : TWinControl; //do not create, pointer only

//Search handling
    fKeyWordSO : TSearchObj;
    fFavoritesSO : TSearchObj;
    fSearchSO : TSearchObj;
    fSearchFields : TStringlist;
    fSearchMayHaveChanged : boolean;
//Search Results
    fSearchSR : TStringList;
    fKeyWordSR : TStringList;
    fFavoritesSR : TStringList;


    function CheckPathOverride(var ConPath : string ) : boolean;
    procedure WritePathOverride(const ConPath : string );
    function IsFileExist( const FName : string; WithNotify : boolean = false ) : boolean;
    function CmdInPath( CheckForBuiltin : boolean = false ) : boolean;
    function AddCmdDisplayObjects( Dest : TStrings; Strings : TStringlist ) : boolean;
    procedure ApplyListChanges( theList : string; aListBox, DispListBox : TListBox );
    procedure ApplyListChangesDisplayedCmd( SrcList, aListBox, DispListBox : TListBox );
    procedure ApplyThreatLevel( aPanel : TPanel; aCB : TComboBox );
    function BadSearchStructure( SO : TSearchObj) : boolean;
    function CanRunCmdLine( const RunStr : string; const ThreatIdx : integer;
                    const IsRoot : boolean; DisAllow : boolean = false; ShowNote : string = '' ) : boolean;
    function CheckEditing: boolean;
    procedure CheckSearchChange;
    procedure ClearSearchDisplays;
    procedure CloseDownDB( const NormalClose, DoSave : boolean );
    function CommandAlreadyUsed( const CheckStr : string; const OkIdx : integer) : boolean;
    function CommandEditing : boolean;
    function CommandNameIsAllowed( const CheckStr : string; const Idx : integer) : boolean;
    procedure DevReleaseSettings;
    function DisallowMisMatch( SO : TSearchObj; const Stamp : string) : boolean;
    procedure DisplaySearchResults( Source : TStrings; TabSheet : TTabSheet; DoReset : boolean = false );
    procedure DuplicateCmd( Sender : TCmdObj );
    procedure DuplicateCmdLIne( Sender : TCmdLineObj );
    procedure EchoThreatLevelDisplay( aPnl : TPanel; aCB : TComboBox; aLBL : TLabel; const Idx : integer );
    function EditCommandLine( const aCaption, Instr : string; var OutStr : string; ShowCmd : boolean = true ) : boolean;
    procedure EditmemEntry;
    procedure EnableCmdLineUpDown( ExtraBool: boolean = True );
    procedure FillDisplayObj_Detail;
    procedure FinalException( Sender : TObject; E : Exception );
    procedure FindInMemo( aMemo : TMemo );
    procedure FindItem( TheLB : TListBox );
    procedure BlankCommand;
    procedure CmdObjToForm( aCmdObj : TCmdObj; CLOIdx : integer = cUseItemIndexFlag );
    procedure ForcePaintingrefresh;
    procedure FormToCmdObj( aCmdObj: TCmdObj );
    procedure BlankCmdLine;
    procedure CmdLineToForm( aCmdLineObj: TCmdLineObj );
    procedure FormToCmdLine( aCmdLineObj: TCmdLineObj );
    function CheckUpdates_PROG : boolean;
    function CheckUpdates_DB : boolean;

    procedure FreeCmdDisplayObjects( Strings : TStrings );
    procedure FreeCommand( const ItemIdx : integer; DoBlank : boolean = true; NewIdx : integer = - 1 );
    procedure GetClipBoardItemVersion( LB : TListbox );
    procedure GetClipBoardListVersion( LB : TListbox );
    function GetNewCommand( const PreString : string; var ResultStr : string;
                            InputMode : TSingleInputMode; const Idx : integer) : boolean;
    function GetNewCommandLine( const PreString : string; var ResultStr : string ) : boolean;
    function GetOpenInstances : boolean;
    function GetProfileStamp : string;
    function GetSearchesDirectory : string;
    procedure ShowUnsavedMessage;
    function GoTo_Command( Sender : TListBox ) : boolean;
    function GotoCommand( Sender : TListBox ) : boolean;
    procedure HandleFormSettings( TheType: TSettingsDirective );
    function GetHelpOutput( SL : TStringList) : string;
    function InitDBManager( const ProfileName : string; const UseDB : boolean ) : boolean;
    procedure InitThreatLevelComboBox( CBs : array of TComboBox );
    procedure InsertCommandSeperator( Strings : TStrings; const Idx : integer );
    function InvalidCommandLines: boolean;
    function InvalidCommands( CheckCmdObj: boolean = True ): boolean;
    function InvalidCommands_Msg : boolean;
    procedure IsBusy( const TurnOn : boolean; aMsg : string = '<>'; ProcessFlag : TProcessType = ptInfo  );
    procedure RefreshCap;
    procedure ResetCommandsToTop;
    procedure SearchVK_F_Keys( const Key : Word );
    procedure lbSearchCmdLine_ApplyActions;
    procedure LoadCurrentSearches;
    procedure LoadData;
    procedure ClearDBObjects( const IsNormal : boolean );
    procedure LoadDBObjects;
    procedure LoadFavorites;
    procedure LoadSearchObject;
    procedure LoadSearchResults( aSearch : TSearchObj; Strings : TStrings );
    procedure LoadLanguages;
    procedure MoveCmdLine( const aFactor: integer );
    function NewCommand( const InputMode : TSingleInputMode ) : boolean;
    procedure OpenProfiles( const IsSwitchMode : boolean );
    procedure ProcessCmdDisplayObj( Sender : TListBox );
    procedure ReFocus_Edit_Memo( const IsCmd : boolean );
    procedure RegisterDisplayCaptions;
    procedure RemoveDetachedProcess( const Idx, OldIdx : integer; aProcess : TAsyncProcess );
    procedure RenameCommand( const InputName : string; InputMode : TSingleInputMode );
    function NotEditing: boolean;
    function Editing: boolean;
    procedure RefreshCmdObj( CLOIdx : integer = cUseItemIndexFlag );
    procedure RunCmdDisplayObj( Sender : TListBox );
    function RunCmdLine( RunStr : string; WantsInput : boolean; const UseShell, Detach : boolean ) : string;
    procedure SaveCurrentSearches;
    procedure SaveSearch( SO : TSearchObj );
    procedure SelectProfile;
    procedure SetUp_Edit_Structure;
    procedure ShowKeyWords( const aTarget : string; aListBox : TListbox = nil; DispListBox : TListbox = nil );
    procedure ShowSearch( SO : TSearchObj; Strings : TStrings; DoLoad : boolean = true );
    procedure ShowSearch_List( FIDs : array of TCmdFieldID; SO : TSearchObj;
                               Strings : TStrings; DoLoad : boolean = true );
    procedure SwitchDB( const ProfileName : string; const UseDB : boolean; DoSave : boolean = false );
    procedure ThreatLevelInfo( const ACmd : string; aCB : TComboBox );
    procedure ToggleCmdLineDelete( const TheValue: boolean; DoToggle: boolean = True );
    procedure ToggleCommandDelete( const TheValue: boolean; DoToggle: boolean = True );
    procedure ToggleEditMode( Sender : TList; const IsEdit : boolean );
    procedure Toggle_pnlCmdLines( const State : boolean );
    procedure UnassMemosKeyDown( const Sender : TMemo; const Key : Word; const Shift : TShiftState );
    function Unsaved : boolean;
    procedure UpdateDetachedProcesses( const DisplayStr : string; aProcess : TAsyncProcess );
    procedure UpdateDisplay( const Output : string; DoIndicate : boolean = true );
    procedure UpdateLbCmdLines( const Idx: integer; SaveState: boolean = True );
    procedure UpdateLbCommands( const DoSave: boolean; TheIdx: integer = cUseItemIndexFlag );
    procedure UpdateMainActions;
    procedure UpdateNotebookCaptions;
    procedure UpdateNotebookEditingStatus;
    procedure UpdatePathsInEdit( const ThePath : string );
    procedure UpdateProfileText( const IsBad : boolean );
    procedure UpdateSaveStatus;
  public
    { public declarations }
    RegX: TRegExpr;

    function GetPODirectory: string;
    procedure UpdateSharedHintsAndCaptions;
    function RunCmdLineExternal( const RunStr : string ) : string;
    function RunExternalHelpRequest( SL : TStringList ) : string;
    procedure SavedSearches_Delete( const aGUID : string );
    function GetWidgetString : string;

    property SuperUser: boolean read FSuperUser;
    property WritingToPath: string read fWritingToPath;
    property SavingToPath: string read fSavingToPath;
    property ProfileName : String read fProfileName;
    property UseDB : Boolean read fUseDB;
    property ProfilePath : string read fProfilePath;
    property IFS : TJiniFile read fIFS;//"I"nifile "F"orm "S"ettings

  end;

var
  frmMain: TfrmMain;

resourcestring
  cmsgMainWorking = 'Working...Check if this causes the timeout';
  ccapMainBadPath = 'Config Problem, Re-set??';
  cmsgMainBasePath = 'Base DB/Settings path: ';
  cmsgMainSavingPath = 'Saving path: ';
  cmsgMainRootTemplate = 'ROOT template: ';
  cmsgMainBadPath =
      'This custom config path can not'
      + LineEnding
      + 'be written to:'
      + LineEnding
      + '==>  %s'
      + LineEnding + LineEnding
      + 'This is due to either it being'
      + LineEnding
      + 'on an unmounted drive or media,'
      + LineEnding
      + 'or is unwritable due to'
      + LineEnding
      + 'permissions.'
      //+ '(eg., it is pointing to a root folder.'
      //+ 'permissions (eg. it is pointing to a root folder like /usr/bin, etc.).'
      + LineEnding + LineEnding
      + 'I can re-set the config folder'
      + LineEnding
      + 'to the default user config'
      + LineEnding
      + 'folder:'
      + LineEnding
      + '%s'
      + LineEnding + LineEnding
      + 'Or I can leave the situation as'
      + LineEnding
      + 'is, so you can fix it youself.'
      + LineEnding + LineEnding
      + 'Tell me what you want me to do:'
      + LineEnding
      + '   Yes = reset to a usable default'
      + LineEnding
      + '   No = You will take care of it'
      + LineEnding;

  cmsgMainBadPathDoReset = 'Okay. Re-set to:'
                           + Lineending
                           + '"%s"'
                           + Lineending
                           ;
  cmsgMainBadPathResetFail = 'Was not able to delete "%s" custom file, you''ll need to manually fix it.';
  cmsgMainBadPathKillIt = 'Okay. Fix the situation and re-start commandoo.';
  ccapMainOptionsNotAvailable = 'Options not currently available.';
  cmsgMainMultipleCopiesOpen = 'Multiple copies of commandoo are running, changing options not allowed. '
                               + 'Close all instances, restart, and then change options.';

//turned in as bug!!!  https://bugs.freepascal.org/view.php?id=32091
//Had a couple tough days there where I could not debug the program anymore!!! Turns out it was because I had
//a reminder in an IFDEF section that the phrase:  'k??
//uncommented because I wanted the program to stop there. That caused the tracing to be offset by one line!! ouch.


implementation

{$R *.lfm}

uses strconst_prog, strconst_en, ufrmMsgDlg
  , unitLanguages
  , ufrmOptions
  , linuxtrix
  , ufrmListManager
  , juusgen
  , ufrmCmdLineEdit
  , Clipbrd
  , ufrmFind
  , ufrmFindText
  , unitDBUtils
  , ufrmProfiles
  , ufrmSearch
  , unitDBStructure
  , unitGlob
  , ufrmSimpleSearch
  ;

type

  TTabControlSearch = ( tcsNormal, tcsKeyWordList );

var
  CmdObj: TCmdObj;
  CmdLineObj: TCmdLineObj;

  ClipCO : TCmdObj;
  ClipCLO : TCmdLineObj;

  BadDB : boolean;
  RootModeCap : string = '';
  OpenInstancesCap : string = '';
  CommsFromFormCreate : string = '';


const
  cHarmlessColor = $FF5D12;//#125DFF;
  cCarefulColor = $28D5D5;//#D5D528
  cCautionColor = $218DD5;//#D58D21
  cDangerColor = $2929D5;//#D52929
  cUnknownColor = $7C7073;//#73707C

  cEditButtonsCommandTag = 300000;
  cEditButtonsLineTag = 300001;

  cOKButtonCommandTag = 300002;
  cCancelButtonCommandTag = 300004;
  cOKButtonLineTag = 300003;
  cCancelButtonLineTag = 300005;

  cCurrKWFileName = '.CurrKWSearch';
  cCurrSearchFileName = '.CurrSearch';
  cDisplayOutPutMax = 25000;


resourcestring
  cmsgProgramKeyWord = 'The Name "%s" at beginning of name is reserved for the program to use.';
  cmsgCommandNameDuplicate =
    'The Command "%s" is already in the command list, duplicates are not allowed.';
  ccapAddEditCommand = 'Add / Edit Command';
  ccapSearchFileName = 'Search File Name';
  ccapAddCommandLine = 'Add Command Line';
  ccapRunningDetachedProcesses = 'Running Processes!';
  cmsgRunningDetachedProcesses =
     'You have %d Running child processes (listed below). They '
     + 'will be automatically halted. Do you still want to close?'
     + LineEnding + LineEnding
     + '%s'
     + LineEnding + LineEnding
     ;
  cmsgFinExProblem = 'A problem occurred, given from:';
  cmsgFinExFatal = 'fatal error: the program will be shut down.';
  cmsgFinExDev = 'A Developer style error has occurred, a developer must fix it.';
  cmsgFinExHand = 'A code error or, possibly, poorly hand-edited data files has occurred.';
  ccapFinExMessage = 'Error Occurred';
  cmsgFinExMessage = 'Error Message: ';
  cmsgFinExClibboard = '( This message is also in your clipboard. )';
  cmsgKill_BadLinux = 'Unexpected system problem: command "id" not found! Terminating.';
//todo fix this message
//todo add the locate command to default db's
//todo is it possible (think vmware installer) or necessary to take a copy and put it in the appimage?
  cmsgSqlLibNotFound = '===> sqlite 3 system library not found <==='
     + LineEnding + LineEnding
     + 'commandoo could not find the location of this library, so only text based DB''s '
     + 'can be used until this is fixed. '
     + LineEnding + LineEnding
     + 'You can set/search this location in OPTIONS. '
     + 'But, first make sure libsqlite3 is installed on your system. Usually it is by default '
     + 'but maybe it was removed, moved, or customized. If you are not sure '
     + 'use a package manager and look to see if a package called "libsqlite3-0", or something similar, is installed. '
     + LineEnding + LineEnding
     + 'If/when it is installed, then, in OPTIONS,  try re-setting to "default". '
     + 'If commandoo still can''t find it then determine the library''s location and point to it '
     + 'manually. Typing the following in a terminal window (or using commandoo) should help you find it: '
     + LineEnding + LineEnding
     + '"locate -i -e libsqlite3.so.0" or "locate -i -e libsqlite3.so"'
     + LineEnding + LineEnding
     + 'examine the output for the proper location (should start with "/usr/" and have "x86_64" or "x64" '
     + 'similar in the path name) '
     + 'and use that location. Normally the file is named "libsqlite3.so.0".'
     + LineEnding + LineEnding
     ;

  cmsgInitISProblem = 'Could not initialize InfoServer';
  ccapDetProcRunning = 'Running';
  ccapDetProcRunningNot = 'not Running';
  ccapDetProcActive = 'active';
  ccapDetProcActiveNot = 'inactive';
  ccapDetProcOpen = 'open';
  ccapDetProcOpenNot = 'closed';
  cmsgSearchInvalid = 'Search is invalid.';
  cmsgSearchInvalidNoLoad = ' It can not be loaded.';
  ccapSearchLoad = 'Load Search';
  ccapSendToProfile = 'Select Profile to SEND TO';
  cmsgInvalidLinuxFileName = 'The filename is invalid! System does not allow "//" in a filename.';
  ccapFileExists = '%s file exists';
  cmsgFileExistsOverwrite = 'Search File "%s" exists. Do you want to overwrite it?';
  cmsgROOTUseShell_Pipe = 'Not allowed to run UseShell or piped CL''s as <ROOT>, this will run as normal user, run CL in Terminal if you want to use <ROOT>.';
  cmsgInput_Detach = 'Not allowed to send input to a detached (child) process. Input turned off.';
  cmsgSystemProcessesError = 'Problem reading system processes. Halting: ';

  //cNameItem_Problem = 'Problem';
  cNameItem_Search = 'Search';
  cNameItem_ThreatLevel = 'Threat Level';


{ TfrmMain }


procedure TfrmMain.ClearDBObjects( const IsNormal : boolean );
var
  i : Integer;
begin

  if IsNormal then
    SaveCurrentSearches;

  fLastlbCommandsIdx := cUseItemIndexFlag;
  fLastlbCmdLinesIdx := cUseItemIndexFlag;

  FreeCmdDisplayObjects( lbSearchCmdLine.Items );
  NilObjects( lbSearchCmd.Items );

  FreeCmdDisplayObjects( fSearchSR );
  FreeCmdDisplayObjects( fKeyWordSR );
  FreeCmdDisplayObjects( fFavoritesSR );

  TCmdListObj.ClearListStructures( [ fKeyWordList ] );

  CmdObj := nil;
  CmdLineObj := nil;

  BlankCmdLine;

  for i := 0 to lbCommands.Items.Count - 1 do
  begin
    CmdObj := TCmdObj(lbCommands.Items.Objects[i]);
    if not assigned(CmdObj) then
      continue;
    FreeAndNil(CmdObj);
  end;

  BlankCommand;

  lbCommands.Clear;//this proc is called from both destroy and switching db's

  if not IsNormal then //update action enabling
    UpdateMainActions;

end;


procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i: integer;
  anObj : TObject;
begin

  //fCLFocus := nil; //do not free, pointer only
  //fCmdFocus := nil; //do not free, pointer only

//first pleez !!!
//====================
 CloseDownDB( not BadDB, not BadDB );
//====================

  if assigned( frmfindtext ) then
  begin
    frmfindtext.close;
    frmfindtext.Free;
    frmfindtext := nil;
  end;

  if assigned( frmBusy ) then
  begin
    frmBusy.close;
    frmBusy.Free;
    frmBusy := nil;
  end;

  if assigned( fSearchSR ) then
    fSearchSR.Free;
  if assigned( fKeyWordSR ) then
    fKeyWordSR.Free;
  if assigned( fFavoritesSR ) then
    fFavoritesSR.Free;

  if assigned( ClipCO ) then
    FreeAndNil( ClipCO );
  if assigned( ClipCLO ) then
    FreeAndNil( ClipCLO );

////Now save general formsettings that are not in the immediately saved category
//  fIFS.WriteInteger( cSectTabFormSettings, cFormSettingsLastNoteBookTab, nbCommands.TabIndex );

  if assigned( fSearchFields ) then
  begin
    FreeCmdDisplayObjects( fSearchFields );
    FreeAndNil( fSearchFields );
  end;

  TimerDetachedProcesses.Enabled := false;
  //  lbCmdLines.Items := CmdLinesStringsPtr;
  for i := 0 to lbDetachedProcesses.Items.Count - 1 do
  begin
    if not assigned( lbDetachedProcesses.Items.Objects[ i ] ) then
      continue;
    anObj := lbDetachedProcesses.Items.Objects[ i ];
    TAsyncProcess( anObj ).Terminate( 0 );
    anObj.Free;
    lbDetachedProcesses.Items.Objects[ i ] := nil;
  end;

  if assigned( fCommandButtons ) then
  begin
    for i := 0 to fCommandButtons.Count - 1 do
      fCommandButtons[ i ] := nil;
    fCommandButtons.Free;
  end;
  if assigned( fCommandLineButtons ) then
  begin
    for i := 0 to fCommandLineButtons.Count - 1 do
      fCommandLineButtons[ i ] := nil;
    fCommandLineButtons.Free;
  end;
  if assigned( fCommandLineEditingControls ) then
  begin
    for i := 0 to fCommandLineEditingControls.Count - 1 do
      fCommandLineEditingControls[ i ] := nil;
    fCommandLineEditingControls.Free;
  end;


  HandleFormSettings( sdSave );

  FreeAndNil( fKeyWordSO );
  FreeAndNil( fFavoritesSO );
  FreeAndNil( fSearchSO );
  FreeAndNil( fDisplayOutPut );

  if assigned(RegX) then
    RegX.Free;

//=================================
//make sure this is last to be free'd because some other objects above may be writing to it, like,
//for instance HandleFormSettings
  fIFS.UpdateFile;
  fIFS.Free;

end;

function TfrmMain.Unsaved : boolean;
begin
  result := Editing or actSave.Enabled;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := false;
  if fIsRunningProcess then
    exit;
  if UnSaved then
  begin
    if MsgDlgMessage( ccapUnsavedData, cmsgUnsavedData, 'cmsgUnsavedData') then
      CanClose := MsgDlgAttentionConfirm( self ) = mrYes;
  end
  else
  if lbDetachedProcesses.Items.Count > 0 then
  begin
    if MsgDlgMessage( ccapRunningDetachedProcesses,
             format( cmsgRunningDetachedProcesses,
                    [ lbDetachedProcesses.Items.Count, lbDetachedProcesses.Items.Text ]
                   )
                    ) then
    CanClose := MsgDlgAttentionConfirm( self ) = mrYes;
  end
  else
    CanClose := True;
end;


procedure TfrmMain.HandleFormSettings(TheType: TSettingsDirective);
var
  i: integer;
  theValue: string;
begin
  theValue := '';
  i := -1;

  case TheType of
    sdSave:
    begin
      FormSettings.AddSetting(IntToStr(Height));
      FormSettings.AddSetting(IntToStr(Width));
      FormSettings.AddSetting(IntToStr(Top));
      FormSettings.AddSetting(IntToStr(Left));

      FormSettings.SaveFormSettings( Self.Name );

    end;
    sdLoad:
    begin

      while FormSettings.ReadFormSettings(Self.Name, i, theValue) do
        case i of
          0: Height := StrToInt(theValue);
          1: Width := StrToInt(theValue);
          2: Top := StrToInt(theValue);
          3: Left := StrToInt(theValue);
        end;

    end;

  end;

end;

function TfrmMain.GetSearchesDirectory: string;
begin
  Result := IncludeTrailingPathDelimiter( fWritingToPath + cSearchFolderName );
end;

function TfrmMain.GetPODirectory: string;
begin
  Result := IncludeTrailingPathDelimiter( fWritingToPath + cLanguageFolderName );
end;

procedure TfrmMain.LoadLanguages;
begin
  Languages.Init( GetPODirectory,
                  cSectTabSupportedLanguages, //cLanguageFolderName,
                  cSectTabFormSettings,
                  cFormSettingsProgramLangCol,
                  fIFS
                 );
  //if Init fails it will just load normally but only in english and be dead in the language options
  Languages.PrepAndApplyLanguage( true );

  BiDiMode := Languages.GetBiDiMode;

end;

procedure TfrmMain.SetUp_Edit_Structure;
begin
  btnCmdEdit.Tag := cEditButtonsCommandTag;
  btnCmdOK.Tag := cOKButtonCommandTag;
  btnCmdCancel.Tag := cCancelButtonCommandTag;

  fCommandButtons := TList.Create;
  fCommandButtons.Add(btnCmdEdit);
  fCommandButtons.Add(btnCmdOk);
  fCommandButtons.Add(btnCmdCancel);
  fCommandButtons.Add(pnlCommand);
  fCommandButtons.Add(pnlDispCommand);

  btnLineEdit.Tag := cEditButtonsLineTag;
  btnLineOk.Tag := cOKButtonLineTag;
  btnLineCancel.Tag := cCancelButtonLineTag;

  fCommandLineButtons := TList.Create;
  fCommandLineButtons.Add(btnLineEdit);
  fCommandLineButtons.Add(btnLineOk);
  fCommandLineButtons.Add(btnLineCancel);
  fCommandLineButtons.Add(pnlCmdLine);
  fCommandLineButtons.Add(pnlDispCmdLine);

  fCommandLineEditingControls := TList.Create;
  fCommandLineEditingControls.Add( actCmdLineUp );
  fCommandLineEditingControls.Add( actCmdLIneDown );
  fCommandLineEditingControls.Add( actNewCommandLine );
  fCommandLineEditingControls.Add( actCmdLineDelete );
  fCommandLineEditingControls.Add( actCmdLineUnDelete );
  fCommandLineEditingControls.Add( actFindCmdLine );

end;

procedure TfrmMain.DevReleaseSettings;
begin
  lblPestr.Visible := false;

//PESTER PESTER PESTER PESTER PESTER PESTER
//yes you found PESTER, list of reminders BEFORE compiling and releasing a version!!!
//================
//before a release version

//       INCREMENT c_PROG_VersionUpgradeCount = #; to the next upgrade
//       if prog upgrades (settings-wise) were necessary
//
//       cHandwrittenVersion = '#.#.#'; to match readme file
//       as of Juneish 2020 v. is 2.0.0 //as of March 2018 v. is 1.0.1

//       INCREMENT c_DB_VersionUpgradeCount = #;to the next upgrade if DB upgrades were necessary
//       c_DB_HandwrittenVersion = '1.0.2'; upgrade if DB structure has changed

//       TfrmMain.CheckUpdates_PROG any code needed for prog updates, just follow the pattern
//       TfrmMain.CheckUpdates_DB   any code needed for  DB  updates

{$IFDEF Release}
Be sure to uncomment this after building release so it shows up next build
//Did you read "pester" reminders?
//Did you, IF NECESSARY, change version number(s)?
  btnQuickRun.Visible := false;
  actQuickRun.Enabled := false;
{$ENDIF}
end;

procedure TfrmMain.FinalException( Sender : TObject; E: Exception );
var
  msg : TStringList;
  isContinuable : Boolean;
begin

  msg := TStringList.Create();
  try

    isContinuable := not ( ( E is EAccessViolation ) or GlobFatalException );

    msg.Add( cmsgFinExProblem );
    msg.Add( '' );
    msg.Add( '==> ' + DoubleQuotedString( E.ClassName ) );
    msg.Add( '' );
{$IFDEF RELEASE}
    if not IsContinuable then
      msg.Add( cmsgFinExFatal );
{$ELSE}
    if not IsContinuable then
      msg.Add( 'fatal error: Strongly consider stopping the program.' );
    GlobFatalException := false;
{$ENDIF}
    msg.Add( '' );

    if ( E is EErrorDevelopment ) then
      msg.Add( cmsgFinExDev )
    else if ( E is EErrorBadHandEdit ) then
      msg.Add( cmsgFinExHand );

    msg.Add( '' );
    msg.Add( cmsgFinExMessage );
    msg.Add( '' );

    Msg.Add( E.Message );
    Msg.Add( '' );

    Msg.Add( cmsgFinExClibboard );
    Clipboard.Astext := Msg.Text;

    if FIsInitialized then
    begin
      MsgDlgMessage( ccapFinExMessage, Msg.Text );
      MsgDlgInfo( self );
    end else Showmessage( Msg.Text );

  finally
    Msg.Free;
  end;

{$IFDEF RELEASE}
  if not isContinuable then
  begin
    Application.ProcessMessages;
    application.Terminate;
  end;
{$ENDIF}

end;

function TfrmMain.CheckPathOverride(var ConPath : string ) : boolean;
var
  Ini : TJinifile;
  str : string;
begin
  result := true;
  Str := ConPath;
//this is the system user config: ie., ~/.config
  if fileexists( ConPath + cConfigPathFileName ) then
  begin
    try
      Ini := TJiniFile.Create( ConPath + cConfigPathFileName );
      Ini.CacheUpdates := true;
      Ini.CaseSensitive := true;
      Str := Ini.ReadString( cConfigPathSection, cConfigPathWritingPath, '' );
    finally
      freeandnil( Ini );
    end;
  end else exit;

  if ( str = '' ) or ( str = ConPath ) then
    exit;

  ConPath := str;

  if not DirectoryExists( ConPath ) then
    Result := ForceDirectories( ConPath );

end;

procedure TfrmMain.WritePathOverride(const ConPath : string );
var
  Ini : TJinifile;
begin
  try
    Ini := TJiniFile.Create( GetAppConfigDir( False ) + cConfigPathFileName );
    Ini.CacheUpdates := true;
    Ini.CaseSensitive := true;
    Ini.WriteString( cConfigPathSection, cConfigPathWritingPath, ConPath );
    Ini.UpdateFile;
  finally
    freeandnil( Ini );
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  NeedsWrite : boolean;

  procedure killit;
  begin
    fHasShown := true;//set flags to fall through onshow and onactivate
    FIsInitialized := true;
    Application.Terminate;
  end;

  function GetDefaultWritingToPath : string;
  begin
    {$IFNDEF Release}
      result := IncludeTrailingPathDelimiter( Extractfilepath( Application.exename ) );
    {$ELSE}
      if fSuperUser then
        result := '/root/.config/commandoo_root/'
      else result := GetAppConfigDir( False );//gets home "." location false is xdg .config, true is system /etc
    {$ENDIF}
  end;

  function DoResetConfig( const Extra : string ) : boolean;
  var
    BadPath : string;
  begin
    result := true;
    BadPath := fWritingToPath;
    //reset to something valid!
    fWritingToPath := GetDefaultWritingToPath;

    if MessageDlg( ccapMainBadPath, format( cmsgMainBadPath, [ BadPath + Extra, fWritingToPath + Extra ] ),
                     mtConfirmation, [ mbYes, mbNo ], 0 ) = mrYes then
    begin
      //get rid of then bogus entry in the config path control file
      if fileexists( fWritingToPath + cConfigPathFileName ) then
        if not deletefile( fWritingToPath + cConfigPathFileName ) then
        begin
          Showmessage( format( cmsgMainBadPathResetFail, [ fWritingToPath + cConfigPathFileName ] ) );
          result := false;
          exit;
        end;
      Showmessage( format( cmsgMainBadPathDoReset, [ fWritingToPath + Extra ] ) );
    end
    else
    begin
      Showmessage( cmsgMainBadPathKillIt );
      result := false;
    end;
  end;

  procedure SetSqliteLibrary;
  var
    SqlLibRead, SqlLibChange : string;
  begin
    SqlLibRead := fSqliteLibrary;
    SqlLibChange := '';

    if not TInfoServer.SqliteInstalled( fSqliteLibrary, SqlLibChange ) then
    begin
      if fUseDB then
      begin
        fUseDB := false;
        try
          if not IniDBFilesExist( fProfilePath, fProfileName ) then
          begin
            fProfileName := cDefaultDBProfileName;
            fProfilePath := fWritingToPath;
          end;
        except
          fProfileName := cDefaultDBProfileName;
          fProfilePath := fWritingToPath;
        end;
      end;
      fIFS.WriteString( cSectTabCurrSqliteLibrary, cCurrSqliteLibraryPath, fSqliteLibrary );
      NeedsWrite := true;
    end
    else if fSqliteLibrary <> SqlLibRead then
      begin
        fIFS.WriteString( cSectTabCurrSqliteLibrary, cCurrSqliteLibraryPath, fSqliteLibrary );
        NeedsWrite := true;
      end;

    if SqlLibChange <> '' then
      CommsFromFormCreate := CommsFromFormCreate + SqlLibChange;
    if ( fSqliteLibrary = cSqliteDefaultLibraryLocationUNKNOWN ) and fDoShowSqlMissing then
      CommsFromFormCreate := CommsFromFormCreate + cmsgSqlLibNotFound;
  end;

begin

  font.size := cDefaultFontSize;
  NeedsWrite := false;

  Randomize;
  application.OnException := @FinalException;
//these were autocreated, but updating the translation not possible then
  frmfindtext := Tfrmfindtext.Create( self );
  frmBusy := TfrmBusy.Create( self );

  fFirstLocalRun := false;
  fSuperUser := False;
  fDisplayOutPut := TStringlist.Create;
  //fCLFocus := nil; //do not create, pointer only
  //fCmdFocus := nil;//do not create, pointer only

  ClipCO := nil;
  ClipCLO := nil;

  fIsRunningProcess := false;
  fLastlbCommandsIdx := cUseItemIndexFlag;
  fLastlbCmdLinesIdx := cUseItemIndexFlag;
  fSearchMayHaveChanged := false;

  fIsInitialized := False;
  fHasShown := False;

  if not SystemFileFound( 'id', True ) then
  begin
    Showmessage( cmsgKill_BadLinux );
    Killit;
    exit;
  end;

  fSuperUser := QuickProc( 'id', '-u' ) = '0';

//==================================
  DevReleaseSettings;
//==================================

//TODO Make a routine that will create a thumbdrive version
//ask for appimage ask for destFolder: compy appimage and config to appimage.config

//breadcrumb ==> here is writing path decision
  fWritingToPath := GetDefaultWritingToPath;

  if not DirectoryExists( fWritingToPath ) then
    if not ForceDirectories( fWritingToPath ) then
    begin
      //complete failure: ie., system gives back ~/.config/commandoo and it doesn't work!!
      showmessage( format( cmsgPermissionsError, [ fWritingToPath ] ) );
      Killit;
      exit;
    end;

//Get the config path re-route if any
  if not CheckPathOverride( fWritingToPath ) then
    if not DoResetConfig( '' ) then
    begin
      Killit;
      exit;
    end;
  //in the check above the path may be valid (/usr/bin), this check is the final check to see if I can
  //write there...
  if not DirectoryExists( fWritingToPath + cLanguageFolderName ) then
    if not CreateDir( fWritingToPath + cLanguageFolderName ) then
      if not DoResetConfig( cLanguageFolderName ) then
      begin
        Killit;
        exit;
      end;
  //if above succeeds then this will too, unless is the last byte on the hard drive.
  if not DirectoryExists( fWritingToPath + cSearchFolderName ) then
  begin
    CreateDir( fWritingToPath + cSearchFolderName );
    fFirstLocalRun := true;
  end;

  fIFS := TJiniFile.Create( fWritingToPath + cReferenceProgramName + cSectTabFormSettingsExtension );
  fIFS.CacheUpdates := true;
  fIFS.CaseSensitive := true;
  fIFS.WriteSetIniFileWarning( false );

  fProfileName := fIFS.ReadString( cSectTabCurrProfile, cCurrProfileName, cDefaultDBProfileName );
  fUseDB := fIFS.ReadBool( cSectTabCurrProfile, cCurrProfileIsDB, cDefaultDBProfileIsDB );
  fDoShowSqlMissing := fIFS.ReadBool( cSectTabFormSettings, cFormSettingsDoShowSqlMissing, true );
  fProfilePath := fIFS.ReadString( cSectTabCurrProfile, cCurrProfilePath, constDefaultPathValue );

  fAllowSqlDB := fIFS.ReadBool( cSectTabFormSettings, cFormSettingsAllowSqlDB, true );
  fAllowTextDB := fIFS.ReadBool( cSectTabFormSettings, cFormSettingsAllowTextDB, true );


  globFontsLarge := fIFS.ReadBool( cSectTabFormSettings, cFormSettingsLargerFont, false );
//===>>> Applychangefont MUST FOLLOW globFontsLarge reading
  ApplyChangeFont( Self );

//juuus sqlitelibrary stuff
  //fSqliteLibrary := fIFS.ReadString( cSectTabCurrSqliteLibrary, cCurrSqliteLibraryPath, cSqliteDefaultLibraryLocationUNKNOWN );
  fSqliteLibrary := fIFS.ReadString( cSectTabCurrSqliteLibrary, cCurrSqliteLibraryPath, '' );//new install

//====== don't move must follow initial ini file read
  SetSqliteLibrary; //don't move
//======

  fSavingToPath := fIFS.ReadString( cSectTabFormSettings, cFormSettingsSavingPath, '' );
  if fSavingToPath = '' then
  begin
    if SystemFileFound( 'xdg-user-dir' ) then
    begin
      if fSuperUser then
        fSavingToPath := fWritingToPath //'/tmp/'
      else fSavingToPath := IncludeTrailingPathDelimiter( QuickProc( 'xdg-user-dir', 'DOWNLOAD' ) );
      //just in case it not dependable....
      if not DirectoryExists( fSavingToPath ) then
        fSavingToPath := fWritingToPath;
    end
    else fSavingToPath := fWritingToPath;
    fIFS.WriteString( cSectTabFormSettings, cFormSettingsSavingPath, fSavingToPath );
    NeedsWrite := true;
  end;

  if NeedsWrite then
    fIFS.UpdateFile;

  fKeyWordSO := TSearchObj.Create( GetProfileStamp, InfoServer );
  fKeyWordSO.UserTag := Ord( tcsKeyWordList );
  fKeyWordSO.Searches[ cIdxCmdLine ].IsUsed := false;

  fSearchSO := TSearchObj.Create( GetProfileStamp, InfoServer );
  fSearchSO.UserTag := Ord( tcsNormal );

  fFavoritesSO := TSearchObj.Create( 'FavS', InfoServer );
  fFavoritesSO.Searches[ cIdxCmd ].AddSearchItem( fidIsFavorite, dbsTrueStr, coEqual, false );
  fFavoritesSO.Searches[ cIdxCmdLine ].AddSearchItem( fidIsFavorite, dbsTrueStr, coEqual, false );

  fSearchSR := TStringList.Create;
  fKeyWordSR := TStringList.Create;
  fFavoritesSR := TStringList.Create;

  Get_Cmd_Fields_Searchable( fSearchFields );

  lblFriendlyNameLineDisp.Caption := '';//I use "..." as caption so I can see it in dev environment, clear that here

  SetUp_Edit_Structure;

  RegX := TRegExpr.Create;

//init Infoserver before all others, some other shared objs may depend on it.
//============
  BadDb := not InitDBManager( fProfileName, fUseDB );
  UpdateProfileText( BadDB );
//============
//from here on Infoserver is ready to use

  FormSettings.Init( fIFS );

  LoadLanguages;

end;

procedure TfrmMain.FormKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  if Editing then
    exit;

  if key in [ VK_1, VK_2, VK_3, VK_NUMPAD1, VK_NUMPAD2, VK_NUMPAD3 ] then
  begin
    if Shift = [ ssAlt ] then
    begin
      if nbCommands.ActivePage <> tsKeyWords then
        nbCommands.ActivePage := tsKeyWords;
      case Key of
        VK_1, VK_NUMPAD1 : if actSearchKeyCmd.Enabled then actSearchKeyCmd.Execute;
        VK_2, VK_NUMPAD2 : if actSearchNewKeyCmd.Enabled then actSearchNewKeyCmd.Execute;
        VK_3, VK_NUMPAD3 : if actSearchLoadKeyCmd.Enabled then actSearchLoadKeyCmd.Execute;
      end;
    end
    else if Shift = [ ssCtrl ] then
    begin
      if nbCommands.ActivePage <> tsSearch then
        nbCommands.ActivePage := tsSearch;
      case Key of
        VK_1, VK_NUMPAD1 : if actSearchSearch.Enabled then actSearchSearch.Execute;
        VK_2, VK_NUMPAD2 : if actSearchNewSearch.Enabled then actSearchNewSearch.Execute;
        VK_3, VK_NUMPAD3 : if actSearchLoadSearch.Enabled then actSearchLoadSearch.Execute;
      end;
    end;
    exit;
  end;

////too confusing, the shortcut or a good old mouseclick is better I think.
//  if ( Shift = [ ssCtrl ] ) and ( Key = VK_S ) then
//  begin
//      if actSave.Enabled then
//        actSave.Execute;
//    exit;
//  end;
end;

procedure TfrmMain.UpdateProfileText( const IsBad : boolean );
begin
  lblCurrDB.Caption := format( cmsgProfileString,
                               [
                                 fProfileName,
                                 strif( fUseDB,  cDefaultDBProfileIsDBStr, cDefaultDBProfileIsDBStrNot ),
                                 //ExtraMessage
//                                 strif( BadDB, cmsgInvalidString )
                                 strif( IsBad, cmsgInvalidString )
                               ] );
end;

function TfrmMain.CheckUpdates_PROG : boolean;
var
  i , FromVer, ToVer: integer;
begin

  result := true;

  FromVer := GetUpgradeLevel( fIFS, cSectTab_PROG_VersionCount );
  ToVer := c_PROG_VersionUpgradeCount;

  //not checking "DB" mismatch, it is not critical, I believe, for
  // program settings, as they generally police themselves. Can always be changed in future if needed.
  //if DBMismatch( FromVer, ToVer )
  //result := false;
  //exit;

  if FromVer = ToVer then
    exit;

  for i := FromVer + 1 to ToVer do
    case i of
      1 : Update_PROG_Version_0001( fIFS, Self.Name );
      2 : Update_PROG_Version_0002( fIFS );
      3 : Update_PROG_Version_0003( fIFS, cSectTabFormSettings );
      //4 : When an update is done on the Program that needs attention in ini file write the needed code here
      //and set the c_PROG_VersionUpgradeCount const by +1
    end;

  UpdateUpgradeLevel( fIFS,
                      cSectTab_PROG_VersionCount,
                      cKey_VersionCount,
                      ToVer );

//This shows up on new installs (blank settings) which is unnecessary, but rather than put in a bunch of
//code to see if the settings file is new, just silently update it...It's all internal and probably of
//zero importance in any meaningful way.
  //Showmessage( cAttnBar
  //        + format( cmsgUpdated_DBFiles, [ ccapUpdated_Prog, cHandwrittenVersion, ToVer ] )
  //        + cAttnBar );

end;

function TfrmMain.CheckUpdates_DB : boolean;
var
  i , FromVer, ToVer: integer;
begin

  result := true;

  FromVer := InfoServer.GetDBVersionUpgradeCount;
  ToVer := c_DB_VersionUpgradeCount;

  if DBMismatch( FromVer, ToVer ) then
  begin
    result := false;
    exit;
  end;

  if FromVer = ToVer then
    exit;

//use literal names for the columns as they were at time, protects against changes to constant names
  for i := FromVer + 1 to ToVer do
    case i of
      1 : Update_DB_Version_0001( 'LineIniIdx' );
      2 : Update_DB_Version_0002( 'Entry' );
      3 : Update_DB_Version_0003( 'ThreatLevel' );
      4 : Update_DB_Version_0004( 'ObjID' );
      5 : Update_DB_Version_0005;
      //6 : When an update is done on the DB write the needed code here, increase the
      //  c_DB_VersionUpgradeCount const by 1
    end;

  InfoServer.UpdateDBVersionCount( ToVer );

  UpdateDisplay(  cAttnBar
                  + format( cmsgUpdated_DBFiles, [ ccapUpdated_DB, c_DB_HandwrittenVersion, ToVer ] )
                  + cAttnBar,
                  false
                );

  //Showmessage( );//was annoying

end;

function TfrmMain.InitDBManager( const ProfileName : string; const UseDB : boolean ) : boolean;
begin

  try

    result := false;

    fProfilePath := strif( fProfilePath = constDefaultPathValue, fWritingToPath, fProfilePath );

    result := InfoServer.Init( fProfilePath, ProfileName, UseDB );
    //result := InfoServer.Init( '/home/juus/Downloads/', 'DBTestX', true );//Testing

    CmdObjHelper.DBServer := InfoServer;

    if not result then
    begin
      UpdateMainActions;
      ShowMessage( 'TfrmMain.InitDBManager: ' + cmsgInitISProblem );
    end
    else
    begin //Upgrades to DB and Program form settings

  //DB check
      if not CheckUpdates_DB then
      begin
        result := false;
        exit;
      end;

  //Program check
      CheckUpdates_PROG;

      fProfileGUID := InfoServer.GetDBGUID;

    end;

  finally
    UpdateProfileText( result );
  end;

end;

procedure TfrmMain.CloseDownDB( const NormalClose, DoSave : boolean );
begin
  ClearDBObjects( NormalClose );
  InfoServer.UnInitialize( DoSave );
end;

procedure TfrmMain.SwitchDB( const ProfileName : string; const UseDB : boolean; DoSave : boolean );
begin
  CloseDownDB( true, DoSave );
  BadDb := not InitDBManager( ProfileName, UseDB );
  UpdateProfileText( BadDB );
  LoadDBObjects;
end;

procedure TfrmMain.LoadData;
var
  SL: TStringList;
begin

  SL := TStringList.Create;
  try

    CmdObjHelper.GetCmdList( SL );

    if SL.Count > 0 then
    begin
      SL.Sort;
      lbCommands.Items.Assign(SL);
      lbCommands.ItemIndex := 0;
      RefreshCmdObj;
    end;

  finally
    SL.Free;
  end;

  TCmdListObj.CreateListStructure( fKeyWordList, dbltKeyWord, InfoServer );
//if in future more lists are used then add them in similar fashion, List fields then automatically
//look up the appropriate list in the Search Forms
//  ListFields_Linker.AddObject( fKeyWordList.ColumnName, fKeyWordList.ListItemsSL );

end;


procedure TfrmMain.InitThreatLevelComboBox( CBs : array of TComboBox );
var
  i : Integer;
begin
  for i := 0 to High( CBs ) do
    Return_ThreatLevel_Items( CBs[ i ].Items );
end;


procedure TfrmMain.UpdateSharedHintsAndCaptions;
begin
//=========================
//this saves A LOT OF WORK for the translator to re-use hints and/or captions that are the same.

  actOutPutSave.Hint := format( cOutPutActionHint, [ cOutPutSave1, cOutPutSave2, cOutPutSave3 ] );
  actOutPutCopy.Hint := format( cOutPutActionHint, [ cOutPutCopy1, cOutPutCopy2, cOutPutCopy3 ] );

  btnSearchFindCmd.Hint := btnFindCmd.Hint;
  btnSearchFindCmdLine.Hint := btnFindCmd.Hint;
  btnSearchGoToCmdLine.Hint := btnSearchGoToCmd.Hint;

  pnlDispCommand.Hint := format( cmsgDisplayPanels, [  cconstCommandLabel ] );
  pnlDispCmdLine.Hint := format( cmsgDisplayPanels, [ cconstCommandLineLabel ] );

  btnCmdEdit.Hint := format( cmsgEditButtons, [  cconstCommandLabel, cconstCommandLabel ] );
  btnLineEdit.Hint := format( cmsgEditButtons, [  cconstCommandLineLabel, cconstCommandLineLabel ] );

  btnCmdOk.Hint := format( cmsgEditOk, [  cconstCommandLabel ] );
  btnLineOk.Hint := format( cmsgEditOk, [  cconstCommandLineLabel ] );

  btnCmdCancel.Hint := format( cmsgEditCancel, [  cconstCommandLabel ] );
  btnLineCancel.Hint := format( cmsgEditCancel, [  cconstCommandLineLabel ] );

  cbThreatLevel.Hint := format( cmsgThreatLevels, [ cmsgThreatLevelsCommand ] );
  cbThreatLevelLine.Hint := format( cmsgThreatLevels, [ cmsgThreatLevelsCommandLine ] );

  btnThreatLevelInfoLine.Hint := btnThreatLevelInfo.Hint;

  memNotesLine.Hint := memNotes.Hint;
  memNotesLineDisp.Hint := memNotesDisp.Hint;

  btnVersionCommand.Hint := format( cmsgHelpVersionInfo, [ cmsgHelpVersionInfoVersion, cmsgHelpVersionInfoVersion, cmsgHelpVersionInfoVersion ] );
  btnHelpCommand.Hint := format( cmsgHelpVersionInfo, [ cmsgHelpVersionInfoHelp, cmsgHelpVersionInfoHelp, cmsgHelpVersionInfoHelp ] );

  btnCmdLineDown.Hint := btnCmdLineUp.Hint;

  FreeCmdDisplayObjects( fSearchFields );
  Get_Cmd_Fields_Searchable( fSearchFields );

  Update_Cmd_DisplayCaptions;
  KeyWordStruct.UpdateDisplayCaption;

  InitThreatLevelComboBox( [ cbThreatLevel, cbThreatLevelLine, cbDispThreatLevel ] );
//  EchoThreatLevelDisplay( pnlThreatLevelDisp, cbThreatLevelDisp, lblThreatLevelDisp, cbThreatLevel.ItemIndex );

//set button captions etc when they are shared and/or used a lot???
  btnRun_Test.Caption := cObjlblRun;

//not static texts so need to be updated also
  UpdateProfileText( BadDB );
  UpdateDetachedProcesses( '', nil );
  RefreshCap;

//were autocreated forms, but now manually created so translations can be updated.
  frmfindtext.UpdateCaptions;
  frmBusy.UpdateCaptions;

//so that it doesn't get hidden, I set this manually and permanently
  FrameHint1.cbHints.Caption := ccbHintsEnglishOverride;

  ResetCommandsToTop;

end;

function TfrmMain.RunCmdLineExternal( const RunStr : string ) : string;
begin

  Result := '';

//is used internally
//not needed in this case, just being complete and consistent, maybe later will be something...?
  if not CanRunCmdLine( RunStr, 0, false ) then
    exit;

  result := RunCmdLine( RunStr, false, false, false );

end;

function TfrmMain.RunExternalHelpRequest( SL : TStringList ) : string;
var
  str : string;
begin
  //help displays special because of BUILTINS
  str := ExtractFilePath( SL[ 0 ] );
  if str <> '' then
//  if ( pos( '/', SL[ 0 ] ) = 1 ) then
  begin
    if not IsFileExist( SL[ 0 ] ) then
    begin
      result := format( '"%s %s" failed, invalid command', [ SL[ 0 ], SL[ 1 ] ] );
      exit;
    end;
  end;
  result := GetHelpOutput( SL );
end;

procedure TfrmMain.RegisterDisplayCaptions;
begin
  Register_Cmd_DisplayCaption( fidIsFavorite,
       [ cbIsFavorite, cbIsFavoriteLine, lblIsFavoriteDisp, lblIsFavoriteLineDisp, lblDispIsFavorite ]
                             );
  Register_Cmd_DisplayCaption( fidSuperUserMode,
       [ cbSuperUser, cbSuperUserLine, lblSuperUserDisp, lblSuperUserLineDisp, lblDispSuperUser ]
                             );
  Register_Cmd_DisplayCaption( fidDetachProcess,
       [ cbDetachProcess, cbDetachProcessLine, lblDetachProcessDisp, lblDetachProcessLineDisp,
                            lblDispDetachProcess ]
                             );
  Register_Cmd_DisplayCaption( fidUseShell,
       [ cbUseShellLine, lblUseShellLineDisp, lblDispUseShell ]
                             );
  Register_Cmd_DisplayCaption( fidWantsInput,
       [ cbWantsInputLine, lblWantsInputLineDisp, lblDispWantsInput ]
                             );

  Register_Cmd_DisplayCaption( fidTerminalOnly,
       [ cbTerminalOnlyLine, lblTerminalOnlyLineDisp, cbTerminalOnly, lblTerminalOnlyDisp, lblDispTerminalOnly ]
                             );

  Register_Cmd_DisplayCaption( fidAlert,
       [ cbAlertLine, lblAlertLineDisp, lblDispAlert ]
                               );

  Register_Cmd_DisplayCaption( fidNotes,
       [ lblCapNotesLine ]
                             );
  Register_Cmd_DisplayCaption( fidKeyWords,
       [ lblKeyWords ]
                             );
  Register_Cmd_DisplayCaption( fidFriendlyName,
       [ lblCapFriendlyNameLine, lblCapFriendlyName ]
                             );
  Register_Cmd_DisplayCaption( fidEntry,
       [ lblCapEntryLine ]
                             );
  Register_Cmd_DisplayCaption( fidHelpCommand,
       [ lblHelp, lblHelpDisp ]
                             );
  Register_Cmd_DisplayCaption( fidVersionCommand,
       [ lblVersion, lblVersionDisp ]
                             );

end;

procedure TfrmMain.LoadCurrentSearches;
begin
  fKeyWordSO.Clear( cDefSearchFileName );
  fKeyWordSO.Load( GetSearchesDirectory + cCurrKWFileName + fProfileGUID );
  fSearchSO.Clear( cDefSearchFileName );
  fSearchSO.Load( GetSearchesDirectory + cCurrSearchFileName + fProfileGUID );
end;

procedure TfrmMain.SaveCurrentSearches;
begin
  fKeyWordSO.Save( GetSearchesDirectory + cCurrKWFileName + fProfileGUID, GetProfileStamp );
  fSearchSO.Save( GetSearchesDirectory + cCurrSearchFileName + fProfileGUID, GetProfileStamp )
end;

procedure TfrmMain.SavedSearches_Delete( const aGUID : string );
begin
  deletefile( GetSearchesDirectory + cCurrKWFileName + aGUID );
  deletefile( GetSearchesDirectory + cCurrSearchFileName + aGUID );
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin

  if fHasShown then
    Exit;

  shpCurrProfile.Brush.Color := btnSwitchDB.Color;

  RegisterDisplayCaptions;
  UpdateSharedHintsAndCaptions;

  UpdateDetachedProcesses( '', nil );

  InitThreatLevelComboBox( [ cbThreatLevel, cbThreatLevelLine, cbDispThreatLevel ] );

  lbCommands.Items.Clear;
  NilObjects( lbCmdLines.Items );

  InitMsgDlgParams( fSavingToPath, cSectTabNoShows, fIFS );

  HandleFormSettings( sdLoad );

  LoadDBObjects;

//Now load general formsettings
  with fIFS do
  begin

    fMaxInOutPut := ReadString( cSectTabFormSettings, cFormSettingsMaxInOutPutCol, '1 M' );
    fDisplayOutPutMax := ReadInteger( cSectTabFormSettings, cFormSettingsOutPutDisplayMax, cDisplayOutPutMax );
    globltMaxOutputWait := ReadInteger( cSectTabFormSettings, cFormSettingsMaxOutputWait, globltMaxOutputWait );

    globltProcessMaxOutput := ConvertSizeStrToInt( fMaxInOutPut );

    fWarnUnspecified := ReadBool( cSectTabFormSettings, cFormSettingsWarnUnspecified, true );
    fWarnHarmless := ReadBool( cSectTabFormSettings, cFormSettingsWarnHarmless, false );
    fWarnCareful := ReadBool( cSectTabFormSettings, cFormSettingsWarnCareful, true );
    fWarnCaution := ReadBool( cSectTabFormSettings, cFormSettingsWarnCaution, true );
    fWarnDanger := ReadBool( cSectTabFormSettings, cFormSettingsWarnDanger, true );
    fAllowMultipleOpens := ReadBool( cSectTabFormSettings, cFormSettingsAllowMultipleOpens, false );
    fAllowESCinOutput := ReadBool( cSectTabFormSettings, cFormSettingsAllowESCOutput, false );

    fRootFile := ReadString( cSectTabFormSettings, cFormSettingsRootFile, cFormSettingsRootFileDefault );

    fLastQuickRun := ReadString( cSectTabFormSettings, cFormSettingsLastQuickRun, '' );
    fManRefreshFavorites := ReadBool( cSectTabFormSettings, cFormSettingsManRefreshFav, false );

//saved last tab position but didn't like it while it would sometimes be an empty search window
//which then made program look broken
    //nbCommands.TabIndex := ReadInteger( cSectTabFormSettings, cFormSettingsLastNoteBookTab, 0 );
    nbCommands.TabIndex := 0;

  end;

  UpdateNotebookCaptions;
  lblTabFavorites.Caption := trim( ccapTabFavorites );
  lblTabKeywords.Caption := trim( ccapTabKeyWords );
  lblTabSearch.Caption := trim( ccapTabSearch );

  nbCommandsChange( nbCommands );

  fHasShown := True;

end;

procedure TfrmMain.lbCmdLinesDblClick( Sender : TObject );
var
  LB : TListBox;
  LBContents : String;
begin
  if not ( Sender is TListbox ) then
    exit;
  LB := TListBox( Sender );
  if ( LB.Items.Count = 0 ) or ( LB.ItemIndex < 0 ) then
    exit;
  LBContents := trim( LB.Items[ LB.ItemIndex ] );
  if LBContents = '' then
    exit;
  MsgDlgMessage( '', LBContents );
  MsgDlgInfo( self );
end;

procedure TfrmMain.lbCmdLinesKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  if ( Key = VK_OEM_PLUS ) or ( Key = VK_ADD ) then
  begin
    if actNewCommandLine.Enabled then
      actNewCommandLine.Execute;
    exit;
  end;

  if ( Key = vk_return ) and ( lbCmdLines.ItemIndex > -1 ) then
  begin
    if ( Shift = [ ssCtrl ] ) then
    begin
      if btnLineEdit.Enabled then
        btnLineEdit.Click;
    end
    else
    begin
      if actRun.Enabled then
        actRun.Execute;
    end;
    exit;
  end;

  if Shift = [ ssCtrl ] then
  begin
    case Key of
      VK_F :
        begin
          if actFindCmdLine.Enabled then
            actFindCmdLine.Execute;
          Key := VK_UNKNOWN;
        end;
      VK_A :
        if actNewCommandLine.Enabled then
          actNewCommandLine.Execute;
      VK_C :
        begin
          if actCopyClipCmdLine.Enabled then
            actCopyClipCmdLine.Execute;
//listbox was overriding my ctrl-c;
//TODO you need to check other cases like this
          Key := VK_UNKNOWN;
        end;
      VK_V :
        if assigned( ClipCLO ) then //popCmdLinePaste.Enabled then
          popCmdLinePaste.Click;
      VK_DELETE, VK_OEM_MINUS, VK_SUBTRACT :
        if actCmdLineUnDelete.Enabled then
          actCmdLineUnDelete.Execute;
    end;
    exit;
  end;

  if Key = VK_C then
  begin
//order, apparently, doesn't matter, both work
    //if Shift = [ ssCtrl, ssShift ] then
    if Shift = [ ssShift, ssCtrl ] then
      mniCopyCLListClipClick( Self );
    //if ( ssCtrl in Shift ) and ( ssShift in Shift ) and ( ssAlt in Shift ) then
    if Shift = [ ssCtrl, ssShift, ssAlt ] then
      if actCopyCmdLine.Enabled then
        actCopyCmdLine.Execute;
    exit;
  end;

  if ( Key = VK_DELETE ) or ( Key = VK_OEM_MINUS ) or ( Key = VK_SUBTRACT ) then
    if actCmdLineDelete.Enabled then
      actCmdLineDelete.Execute;
end;

procedure TfrmMain.UpdateNotebookCaptions;
var
  i: integer;
  FmtStr : string;
begin
  for i := 0 to nbCommands.PageCount - 1 do
  begin
    if i = nbCommands.TabIndex then
      FmtStr := ccapTabSelected
    else FmtStr := ccapTabUnSelected;
    case nbCommands.Pages[ i ].Name of
      'tsCommands' : tsCommands.Caption := format( FmtStr, [ ccapTabCommands ] );
      'tsFavorites' : tsFavorites.Caption := format( FmtStr, [ ccapTabFavorites ] );
      'tsKeyWords' : tsKeyWords.Caption := format( FmtStr, [ ccapTabKeyWords ] );
      'tsSearch' : tsSearch.Caption := format( FmtStr, [ ccapTabSearch ] );
//No, timer overwrites      'tsDetachedProcesses' : tsDetachedProcesses.Caption := format( FmtStr, [ lblDetachedProcesses.Caption ] );
    end;
  end;
end;

procedure TfrmMain.UpdateMainActions;
begin
  pmCommandsPopup( self );
  pmCmdLinesPopup( self );
end;

procedure TfrmMain.LoadDBObjects;
begin

  if BadDB then
  begin
    UpdateMainActions;
    exit;
  end;

  try
    LoadData;
  except
    BadDB := true;
    CloseDownDB( false, false );
    raise;
  end;

  btnRefreshFavorites.Click;
  LoadCurrentSearches;
  if lbCommands.CanFocus then
    lbCommands.SetFocus;
  UpdateMainActions;

end;


procedure TfrmMain.EnableCmdLineUpDown( ExtraBool: boolean = True );
begin
  actCmdLineUp.Enabled := (lbCmdLines.ItemIndex > 0) and ExtraBool;
  actCmdLineDown.Enabled := (lbCmdLines.ItemIndex < lbCmdLines.Count - 1)
                            and (lbCmdLines.ItemIndex > -1)
                            and ExtraBool;
end;


procedure TfrmMain.lbCmdLinesSelectionChange(Sender: TObject; User: boolean);
begin
  EnableCmdLineUpDown;
end;

procedure TfrmMain.RefreshCmdObj( CLOIdx : integer = cUseItemIndexFlag );
var
  AnIdx : integer;
begin

  try

    if InvalidCommands( false ) then
    begin
 //the list is empty now (rare case of deleting everything) and so must make sure CmdObj is invalid
      CmdObj := nil;
      exit;
    end;

    AnIdx := lbCommands.ItemIndex;

    if not assigned( lbCommands.Items.Objects[ anIdx ] ) then
    begin
//load on-demand
      CmdObjHelper.LoadCmdObj( CmdObj, lbCommands.Items[ anIdx ] );

      lbCommands.Items.Objects[ anIdx ] := CmdObj;

    end
    else CmdObj := TCmdObj( lbCommands.Items.Objects[ anIdx ] );

    CmdObjToForm( CmdObj, CLOIdx );
    UpdateLbCommands( False, AnIdx );

  finally
    fLastlbCommandsIdx := cUseItemIndexFlag;
    UpdateMainActions;
  end;

end;


procedure TfrmMain.lbCommandsClick(Sender: TObject);
begin

  if fLastlbCommandsIdx = lbCommands.ItemIndex then
//Clicked same entry
    exit;

  BlankCmdLine;
  BlankCommand;
  CmdObj := nil;

  if InvalidCommands( False ) then
    exit;

  RefreshCmdObj;

  fLastlbCommandsIdx := lbCommands.ItemIndex;

end;

procedure TfrmMain.lbCmdLinesClick(Sender: TObject);
begin

  if fLastlbCmdLinesIdx = lbCmdLines.ItemIndex then
//Clicked same entry
    exit;

  CmdLineObj := nil;

  if (lbCmdLines.ItemIndex < 0) or not assigned(CmdObj) then
    exit;

  CmdLineObj := TCmdLineObj(CmdObj.CmdLines.Objects[lbCmdLines.ItemIndex]);
  CmdLineToForm(CmdLineObj);

  ToggleCmdLineDelete(not CmdLineObj.DoDelete);

  fLastlbCmdLinesIdx := lbCmdLines.ItemIndex;

end;

procedure TfrmMain.lbCommandsKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  if ( Key = vk_return ) and ( lbCommands.ItemIndex > -1 ) and ( btnCmdEdit.Enabled ) then
  begin
    btnCmdEdit.Click;
    exit;
  end;

  if ( Key = VK_OEM_PLUS ) or ( Key = VK_ADD ) then
  begin
    if btnPlus.Enabled then
      btnPlus.Click;
    exit;
  end;

  if Shift = [ ssCtrl ] then
  begin
    case Key of
      VK_F :
        if btnFindCmd.Enabled then
          btnFindCmd.Click;
      VK_A :
        if btnPlus.Enabled then
          btnPlus.Click;
      VK_C :
        if actCopyClipCmd.Enabled then
          actCopyClipCmd.Execute;
      VK_V :
        if assigned( ClipCLO ) then
          popCmdPasteCmdLine.Click;
      VK_DELETE, VK_OEM_MINUS, VK_SUBTRACT :
        if btnCommandUnDelete.Enabled then
          btnCommandUnDelete.Click;
    end;
    exit;
  end;

  if Key = VK_C then
  begin
    if Shift = [ ssShift, ssCtrl ] then
      mniCopyCmdListClipClick( Self );
    if Shift = [ ssCtrl, ssShift, ssAlt ] then
      if actCopyCmd.Enabled then
        actCopyCmd.Execute;
    exit;
  end;

  if ( Key = VK_DELETE ) or ( Key = VK_OEM_MINUS ) or ( Key = VK_SUBTRACT ) then
    if btnCommandDelete.Enabled then
      btnCommandDelete.Click;
end;

procedure TfrmMain.lbDetachedProcessesClick( Sender : TObject );
var
  aProcess : TAsyncProcess;
begin

  memDetachedProcesses.Clear;

  if ( lbDetachedProcesses.Items.Count = 0 ) or ( lbDetachedProcesses.Itemindex = -1 ) then
    exit;

  aProcess := TAsyncProcess( lbDetachedProcesses.Items.Objects[ lbDetachedProcesses.Itemindex ] );
  if not assigned( aProcess ) then
    raise EErrorDevelopment.Create( 'lbDetachedProcessesClick - list object not assigned.' );

  with memDetachedProcesses do
  begin
    Lines.Add( 'PID: ' + inttostr( aProcess.ProcessID ) );
    Lines.Add( 'Handle: ' + inttostr( aProcess.ProcessHandle ) );
    Lines.Add( 'Status: ' + StrIf( aProcess.Active, ccapDetProcActive, ccapDetProcActiveNot ) );
    Lines.Add( ccapDetProcRunning + ': '
        + StrIf( aProcess.Running, ccapDetProcRunning, ccapDetProcRunningNot ) );
    Lines.Add( 'XtermProgram: ' + aProcess.XtermProgram );
    Lines.Add( 'Desktop: ' + aProcess.Desktop );
    Lines.Add( 'Input: '
        + StrIf( assigned( aProcess.Input ), ccapDetProcOpen, ccapDetProcOpenNot ) );
    Lines.Add( 'Output: '
        + StrIf( assigned( aProcess.Output ), ccapDetProcOpen, ccapDetProcOpenNot ) );
    Lines.Add( 'StdErr: '
        + StrIf( assigned( aProcess.Stderr ), ccapDetProcOpen, ccapDetProcOpenNot ) );
    Lines.Add( 'Curr. Dir: ' + aProcess.CurrentDirectory );
    Lines.Add( 'Application Name: ' + aProcess.Executable );//ApplicationName );
    Lines.Add( 'Executable: ' + aProcess.Executable );
    Lines.Add( '' );
    Lines.Add( 'Parameters: ' );
    Lines.Add( '----------' );
    Lines.Add( '' );
    Text := Text + aProcess.Parameters.Text;
    Lines.Add( '' );
    Lines.Add( 'Environment:' );
    Lines.Add( '----------' );
    Lines.Add( '' );
    Text := Text + aProcess.Environment.Text;
  end;

  aProcess := nil;

end;

procedure TfrmMain.lbDetachedProcessesKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  FindItem( lbDetachedProcesses );
end;

procedure TfrmMain.lbKeywordsDispKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  if ( shift = [ ssCtrl ] )
     and ( key = VK_F )
     and ( lbKeywordsDisp.Items.Count > 1 ) then
    FindItem( lbKeywordsDisp );
end;

procedure TfrmMain.lbKeywordsKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  if ( shift = [ ssCtrl ] )
     and ( key = VK_F )
     and ( lbKeywords.Items.Count > 1 ) then
  begin
    FindItem( lbKeywords );
    exit;
  end;
  memNotesKeyDown( Sender, Key, Shift );
end;

procedure TfrmMain.ProcessCmdDisplayObj( Sender : TListBox );
var
  CDO : TCmdDisplayObj;
  CO : TCmdObj;
  CLO : TCmdLineObj;
  DoClearCmdDisplay , HasOptions: Boolean;
  BeginPoint : TPoint;
  Str : String;

  procedure ClearBooleans;
  begin
    lblDispIsFavorite.Visible := false;
    lblDispWantsInput.Visible := false;
    lblDispTerminalOnly.Visible := false;
    lblDispAlert.Visible := false;
    lblDispSuperUser.Visible := false;
    lblDispUseShell.Visible := false;
    lblDispDetachProcess.Visible := false;
    lblNoOptions.Visible := false;
  end;

  procedure ClearCmdDisplay;
  begin

    pnlS.Caption := '';
    cbDispThreatLevel.ItemIndex := -1;
    lblDispThreatLevel.Caption := CmdObjHelper.GetThreatLevelText( TThreatLevel( -1 ) );
    ApplyThreatLevel( pnlS, cbDispThreatLevel );

    lbDispKeywords.Clear;
    memDispNotes.Clear;

    lblDispFriendlyName.Caption := '';

    lblDispCommandName.Caption := '';
    lblDispEntry.Caption := '';

    actSearchRun.Enabled := false;

  end;

  procedure CheckLabel( LBL : TLabel; ShouldShow : boolean );
  begin
    if ShouldShow then
    begin
      if not HasOptions then
        HasOptions := true;
      LBL.Top := BeginPoint.Y;
      inc( BeginPoint.Y, LBL.Height + 4 );
    end;
    LBL.Visible := ShouldShow;
  end;

begin

  ClearBooleans;

  DoClearCmdDisplay := false;
  if Sender.ItemIndex < 0 then
    DoClearCmdDisplay := true;

  if not DoClearCmdDisplay then
    CDO := TCmdDisplayObj( Sender.Items.Objects[ Sender.ItemIndex ] );

  if DoClearCmdDisplay or not assigned( CDO ) then
  begin
    ClearCmdDisplay;
    exit;
  end;

  BeginPoint.X := 10;
  BeginPoint.Y := lbDispKeywords.top;//236;
  CO := nil;
  CLO := nil;

  try

    if CDO.IsCommandLine then
    begin

      pnlS.Caption := 'Command Line';
      CmdObjHelper.LoadCmdLineObj( CLO, CDO.CommandName, CDO.SectTab );
      lblDispEntry.Caption := CLO.Entry;

      cbDispThreatLevel.ItemIndex := CmdObjHelper.ThreatLevelToListIdx( CLO.ThreatLevel );
      lblDispThreatLevel.Caption := CmdObjHelper.GetThreatLevelText( CLO.ThreatLevel );
      lbDispKeywords.Clear;

      Str := trim( CLO.Notes );
      memDispNotes.Lines.Text := strif( str = '', cmsgDispNoNotes, str );

      HasOptions := false;
      CheckLabel( lblDispIsFavorite, CLO.IsFavorite );
      CheckLabel( lblDispSuperUser, CLO.SuperUserMode );
      CheckLabel( lblDispTerminalOnly, CLO.TerminalOnly );
      CheckLabel( lblDispDetachProcess, CLO.DetachProcess );
      CheckLabel( lblDispWantsInput, CLO.WantsInput );
      CheckLabel( lblDispAlert, CLO.Alert );
      CheckLabel( lblDispUseShell, CLO.UseShell );
      if not HasOptions then
        CheckLabel( lblNoOptions, true );


      lblDispFriendlyName.Caption := StrIf( CLO.FriendlyName <> '',
                  format( ccapDisplayCaptionAndValue,
                          [ Get_Cmd_Field_DisplayCaption( fidFriendlyName ), CLO.FriendlyName ] )
                                          );

      TCmdDisplayObj( lbSearchCmd.Items.Objects[ lbSearchCmd.ItemIndex ] ).DetailIdx := Sender.ItemIndex;

    end else
    begin

      pnlS.Caption := 'Command';
      CmdObjHelper.LoadCmdObj( CO, CDO.SectTab );
      lblDispEntry.Caption := CO.CommandName;

      cbDispThreatLevel.ItemIndex := CmdObjHelper.ThreatLevelToListIdx( CO.ThreatLevel );
      lblDispThreatLevel.Caption := CmdObjHelper.GetThreatLevelText( CO.ThreatLevel );

      Str := trim( CO.KeyWords );
      lbDispKeywords.Items.Text := strif( str = '', cmsgDispNoKeyWords, str );

      Str := trim( CO.Notes );
      memDispNotes.Lines.Text := strif( str = '', cmsgDispNoNotes, str );


      HasOptions := false;
      CheckLabel( lblDispIsFavorite, CO.IsFavorite );
      CheckLabel( lblDispSuperUser, CO.SuperUserMode );
      CheckLabel( lblDispTerminalOnly, CO.TerminalOnly );
      CheckLabel( lblDispDetachProcess, CO.DetachProcess );
      if not HasOptions then
        CheckLabel( lblNoOptions, true );

      lblDispFriendlyName.Caption := '';

    end;

    lblDispCommandName.Caption := CDO.CommandName;
    ApplyThreatLevel( pnlS, cbDispThreatLevel );
    actSearchRun.Enabled := CDO.IsCommandLine;

  finally
    if assigned( CO ) then
      FreeAndNil( CO );
    if assigned( CLO ) then
      FreeAndNil( CLO );
  end;

end;

procedure TfrmMain.SearchVK_F_Keys( const Key : Word );
begin

  if nbCommands.ActivePage = tsFavorites then
    exit; //no saves / loads / displays on favorites

  if nbCommands.ActivePage = tsKeyWords then
    case Key of
      //VK_1 : Made Global if actKeyWordSearch.Enabled then actKeyWordSearch.Execute;
      //VK_2 : Made Global if actKeyWordNewSearch.Enabled then actKeyWordNewSearch.Execute;
      //VK_3 : Made Global if actKeyWordLoad.Enabled then actKeyWordLoad.Execute;
      VK_4, VK_NUMPAD4 : if actSearchSaveKeyCmd.Enabled then actSearchSaveKeyCmd.Execute;
      VK_5, VK_NUMPAD5 : if actSearchDisplayKeyCmd.Enabled then actSearchDisplayKeyCmd.Execute;
    end
  else
  case Key of
    //VK_1 : Made Global if actKeyWordSearch.Enabled then actKeyWordSearch.Execute;
    //VK_2 : Made Global if actKeyWordNewSearch.Enabled then actKeyWordNewSearch.Execute;
    //VK_3 : Made Global if actKeyWordLoad.Enabled then actKeyWordLoad.Execute;
    VK_4, VK_NUMPAD4 : if actSearchSaveSearch.Enabled then actSearchSaveSearch.Execute;
    VK_5, VK_NUMPAD5 : if actSearchDisplaySearch.Enabled then actSearchDisplaySearch.Execute;
  end;
end;

procedure TfrmMain.FillDisplayObj_Detail;
var
  SLTo: TStringList;
  CDO : TCmdDisplayObj;
  CO : TCmdObj;
begin

  FreeCmdDisplayObjects( lbSearchCmdLine.Items );

  if lbSearchCmd.ItemIndex < 0 then
    exit;

  CO := nil;
  SLTo := TStringlist.Create;
  try

    CDO := TCmdDisplayObj( lbSearchCmd.Items.Objects[ lbSearchCmd.ItemIndex ] );

    if not assigned( CDO ) or ( CDO.IsCommandLine ) then
      exit;

    CmdObjHelper.LoadCmdObj( CO, CDO.SectTab );

    CmdObjHelper.GetCmdDisplayObjList_CmdLine_External( CO, SLTo );

    AddCmdDisplayObjects( lbSearchCmdLine.Items, SLTo );

    lbSearchCmdLine.Itemindex := GetValidItemIndex( CDO.DetailIdx, lbSearchCmdLine.Items.Count );
//at first I thought it would be a good idea to focus the cmdline when the command is clicked
//but then you never get to inspect the command's entries, so no good. this left here in case
//I need to implement it in another way, this may help??
    //if lbSearchCmdLine.Itemindex > -1 then
    //  lbSearchCmdLine.Click;

  finally
    if assigned( SLTo ) then
      SLTo.Free;
    if assigned( CO ) then
      FreeAndNil( CO );
  end;

end;

procedure TfrmMain.lbSearchCmdLine_ApplyActions;
begin
  actSearchGotoCmdLIne.Enabled := lbSearchCmdLine.ItemIndex > -1;
  actSearchFindCmdLine.Enabled := lbSearchCmdLine.Items.Count > 1;
end;

procedure TfrmMain.lblCommandNameDblClick( Sender : TObject );
begin
  RenameCommand(
       CmdObjHelper.GetNormalizedPath( lblPathAlias.Caption, lblPathActual.Caption, lblCommandName.Caption ),
       simFile
               );
  ReFocus_Edit_Memo( true );
end;

procedure TfrmMain.lblCommandNameDispDblClick( Sender : TObject );
begin
  MsgDlgMessage( ccapOverflow, lblCommandNameDisp.Caption );
  MsgDlgInfo( self );
end;

procedure TfrmMain.lblCurrDBDblClick( Sender : TObject );
begin
  btnSwitchDB.Click;
  //MsgDlgMessage( ccapOverflow, lblCurrDb.Caption );
  //MsgDlgInfo( self );
end;

procedure TfrmMain.lblDispEntryDblClick( Sender : TObject );
begin
  MsgDlgMessage( ccapOverflow, lblDispEntry.Caption );
  MsgDlgInfo( self );
end;

procedure TfrmMain.lblDispFriendlyNameDblClick( Sender : TObject );
begin
  MsgDlgMessage( ccapOverflow, lblDispFriendlyName.Caption );
  MsgDlgInfo( self );
end;

procedure TfrmMain.lblFriendlyNameLineDispDblClick( Sender : TObject );
begin
  MsgDlgMessage( ccapOverflow, lblFriendlyNameLineDisp.Caption );
  MsgDlgInfo( self );
end;

procedure TfrmMain.lblPathAliasDblClick( Sender : TObject );
var
  str : string;
begin
  if CmdInPath then
    str := lblPathAlias.Caption + format( '   (%s)', [ lblPathActual.caption ] )
  else str := lblPathAlias.Caption;
  MsgDlgMessage( ccapOverflow, str );
  MsgDlgInfo( self );
  ReFocus_Edit_Memo( true );
end;

procedure TfrmMain.lblPathAliasDispDblClick( Sender : TObject );
var
  str : string;
begin
  if CmdInPath then
    str := lblPathAliasDisp.Caption + format( '   (%s)', [ lblPathActual.caption ] )
  else str := lblPathAliasDisp.Caption;
  MsgDlgMessage( ccapOverflow, str );
  MsgDlgInfo( self );
end;

procedure TfrmMain.lblThreatLevelDispDblClick( Sender : TObject );
begin
  MsgDlgMessage( ccapOverflow, lblThreatLevelDisp.Caption );
  MsgDlgInfo( self );
end;

procedure TfrmMain.lbSearchCmdClick( Sender : TObject );
begin

  ProcessCmdDisplayObj( lbSearchCmd );
  actSearchGotoCmd.Enabled := lbSearchCmd.ItemIndex > -1;
  actSearchFindCmd.Enabled := lbSearchCmd.Items.Count > 1;

  FillDisplayObj_Detail;
  lbSearchCmdLine_ApplyActions;

//store last viewed indices on the current tabsheet
  nbCommands.ActivePage.Tag := lbSearchCmd.ItemIndex;

end;

procedure TfrmMain.lbSearchCmdKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  if Shift = [ ssCtrl ] then
  begin
    case Key of
      VK_F :
        if btnSearchFindCmd.Enabled then
          btnSearchFindCmd.Click;
      VK_G :
        if btnSearchGotoCmd.Enabled then
          btnSearchGotoCmd.Click;
    end;
    exit;
  end;

  if Shift = [ ssShift, ssCtrl ] then
  begin
    if Key = VK_C then
      mniSearchCmdListClipClick( Self );
    exit;
  end;

  if Key in [ VK_4, VK_5 ] then
    SearchVK_F_Keys( Key );
end;

procedure TfrmMain.lbSearchCmdLineClick( Sender : TObject );
begin
  if lbSearchCmdLine.Items.Count > 0 then
    ProcessCmdDisplayObj( lbSearchCmdLine );
  lbSearchCmdLine_ApplyActions;
end;

procedure TfrmMain.lbSearchCmdLineKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  if ( Key = vk_return ) and ( lbSearchCmdLine.ItemIndex > -1 ) then
  begin
    if btnSearchRun.Enabled then
      btnSearchRun.Click;
    exit;
  end;

  if Shift = [ ssCtrl ] then
  begin
    case Key of
      VK_F :
        if btnSearchFindCmdLine.Enabled then
          btnSearchFindCmdLine.Click;
      VK_G :
        if btnSearchGotoCmdLine.Enabled then
          btnSearchGotoCmdLine.Click;
      VK_C :
        mniSearchCmdLineItemClipClick( Self );
    end;
    exit;
  end;

  if Shift = [ ssShift, ssCtrl ] then
  begin
    if Key = VK_C then
      mniSearchCmdLineListClipClick( Self );
    exit;
  end;

  if Key in [ VK_4, VK_5 ] then
    SearchVK_F_Keys( Key );
end;

procedure TfrmMain.memDetachedProcessesDblClick( Sender : TObject );
begin
  MsgDlgMessage( '', memDetachedProcesses.Text );
  MsgDlgInfo( Self );
end;

procedure TfrmMain.memDetachedProcessesKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  UnassMemosKeyDown( memDetachedProcesses, Key, Shift );
end;

procedure TfrmMain.memDispNotesDblClick( Sender : TObject );
begin
  MsgDlgMessage( '', memDispNotes.Text );
  MsgDlgInfo( Self );
end;

procedure TfrmMain.memDispNotesKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  UnassMemosKeyDown( memDispNotes, Key, Shift );
end;

procedure TfrmMain.EditmemEntry;
var
  InPut, OutPut : string;
begin
  Input := trim( memEntry.Lines.Text );
  OutPut := '';
  if not EditCommandLine( format( ccapGenericEdit, [ cNameItem_CommandLine ] ), Input, Output ) then
    exit;
  memEntry.Lines.Text := Output;
end;

procedure TfrmMain.memEntryKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  if ( Shift = [ ssctrl, ssalt ] ) and ( Key = VK_RETURN ) then
  begin
    actRun.Execute;
    exit;
  end;

  if key = vk_return then
    EditmemEntry
  else memNotesLineKeyDown( Sender, Key, Shift );
end;

procedure TfrmMain.memNotesChange( Sender : TObject );
begin
  memNotesDisp.Lines.Text := memNotes.Lines.Text;
end;

procedure TfrmMain.memNotesDispDblClick( Sender : TObject );
begin
  MsgDlgMessage( '', memNotesDisp.Text );
  MsgDlgInfo( Self );
end;

procedure TfrmMain.memNotesDispKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  UnassMemosKeyDown( MemNotesDisp, Key, Shift );
end;

procedure TfrmMain.memNotesEnter( Sender : TObject );
begin
  //fCmdFocus := memNotes;
end;

procedure TfrmMain.memNotesKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  if ( Shift = [ ssCtrl ] ) then
  begin
    if key = VK_S then
      btnCmdOk.Click;
  end;

  if ( ssCtrl in Shift ) and not ( ssAlt in Shift ) then
  begin
    if key = VK_F then
    begin
      if ssShift in Shift then
        frmFindText.ReFindinMemo( memNotes )
      else FindInMemo( memNotes );
    end;
    if key = VK_L then
      frmFindText.ReFindinMemo( memNotes );
  end;

  if Key = vk_escape then
    btnCmdCancel.Click;

end;

procedure TfrmMain.memNotesLineChange( Sender : TObject );
begin
  memNotesLineDisp.Lines.Text := memNotesLine.Lines.Text;
end;

procedure TfrmMain.memNotesLineDispDblClick( Sender : TObject );
begin
  MsgDlgMessage( '', memNotesLineDisp.Text );
  MsgDlgInfo( Self );
end;

procedure TfrmMain.memNotesLineDispKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  UnassMemosKeyDown( memNotesLineDisp, Key, Shift );
end;

procedure TfrmMain.memNotesLineEnter( Sender : TObject );
begin
  //fCLFocus := memNotesLine;
end;

procedure TfrmMain.memNotesLineKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  if ( Shift = [ ssctrl, ssalt ] ) and ( Key = VK_RETURN ) then
  begin
    actRun.Execute;
    exit;
  end;

  if ( Shift = [ ssCtrl ] ) then
  begin
    if key = VK_S then
      btnLineOk.Click;
  end;

  if ( ssCtrl in Shift ) and not ( ssAlt in Shift ) then
  begin
    if key = VK_F then
    begin
      if ssShift in Shift then
        frmFindText.ReFindinMemo( memNotesLine )
      else FindInMemo( memNotesLine );
    end;
    if key = VK_L then
      frmFindText.ReFindinMemo( memNotesLine );
  end;

  if Key = vk_escape then
    btnLineCancel.Click;

  //self.FormKeyDown( Sender, Key, Shift ); memo1 dead when editing, hmmmmm.
end;

procedure TfrmMain.Memo1DblClick( Sender : TObject );
begin
  MsgDlgMessage( '', Memo1.Text );
  MsgDlgInfo( Self );
end;

procedure TfrmMain.Memo1KeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  UnassMemosKeyDown( Memo1, Key, Shift );
end;

procedure TfrmMain.UnassMemosKeyDown( const Sender : TMemo; const Key : Word; const Shift : TShiftState );
begin
  if ( ssCtrl in Shift ) and not ( ssAlt in Shift ) then
  begin
    if key = VK_F then
    begin
      if ssShift in Shift then
        frmFindText.ReFindinMemo( Sender )
      else FindInMemo( Sender );
    end;
    if key = VK_L then
      frmFindText.ReFindinMemo( Sender );
  end;
end;

procedure TfrmMain.mniCmdCountClick( Sender : TObject );
begin
  UpdateDisplay( mniCmdCount.Caption + ': ' + inttostr( lbCommands.Items.Count ), false );
end;

procedure TfrmMain.mniCmdSendToClick( Sender : TObject );
begin
  SelectProfile;
end;

procedure TfrmMain.mniCopyCLListClipClick( Sender : TObject );
var
  Str : String;
  i : Integer;
begin
  Str := '';
  for i := 0 to lbCmdLines.Items.Count - 1 do
  begin
//separate ClipBoardVersion because these can have <UPDATE> etc on them and "Entry" cleans this.
    Str := Str
           + CmdObjHelper.ClipBoardVersion(
                                TCmdLineObj( CmdObj.CmdLines.Objects[ i ] ).Entry,
                                             fRootFile
                                           )
           + LineEnding;
  end;
  ClipBoard.AsText := Str;

end;

procedure TfrmMain.mniCopyCmdListClipClick( Sender : TObject );
begin
  ClipBoard.AsText := lbCommands.Items.Text;
end;

procedure TfrmMain.mniSearchCmdLineItemClipClick( Sender : TObject );
begin
  GetClipBoardItemVersion( lbSearchCmdLine );
end;

procedure TfrmMain.mniSearchCmdLineListClipClick( Sender : TObject );
begin
  GetClipBoardListVersion( lbSearchCmdLine );
end;

procedure TfrmMain.mniSearchCmdListClipClick( Sender : TObject );
begin
  GetClipBoardListVersion( lbSearchCmd );
end;

procedure TfrmMain.GetClipBoardItemVersion( LB : TListbox );
begin
  ClipBoard.AsText := '';
  if LB.ItemIndex < 0 then
    exit;
  if assigned( LB.Items.Objects[ LB.ItemIndex ] ) then
    ClipBoard.AsText := CmdObjHelper.ClipBoardVersion(
             TCmdDisplayObj( LB.Items.Objects[ LB.ItemIndex ] ).Entry,
                             fRootFile
                            );
end;

procedure TfrmMain.GetClipBoardListVersion( LB : TListbox );
var
  Str : String;
  i : Integer;
  CDO : TCmdDisplayObj;
begin
  ClipBoard.AsText := '';
  Str := '';
  for i := 0 to LB.Items.Count - 1 do
  begin
    CDO := TCmdDisplayObj( LB.Items.Objects[ i ] );
    if not assigned( CDO ) then
      continue;
    Str := Str
           + CmdObjHelper.ClipBoardVersion( CDO.Entry, fRootFile )
           + LineEnding;
  end;
  ClipBoard.AsText := Str;

end;

procedure TfrmMain.ClearSearchDisplays;
begin
  FreeCmdDisplayObjects( lbSearchCmdLine.Items );
  NilObjects( lbSearchCmd.Items );
  lbSearchCmd.Items.Add( '' );
  lbSearchCmd.Items.Add( cmsgEmptySearchResults );
end;

procedure TfrmMain.DisplaySearchResults( Source : TStrings; TabSheet : TTabSheet; DoReset : boolean = false );
var
  HasItems : Boolean;
begin

  ClearSearchDisplays;

  HasItems := Source.Count > 0;

  if HasItems then
    lbSearchCmd.Items.Assign( Source );

  if not HasItems or DoReset then
    TabSheet.Tag := -1;

  TabSheet.Tag := GetValidItemIndex( TabSheet.Tag, lbSearchCmd.Items.Count );
  lbSearchCmd.ItemIndex := TabSheet.Tag;
  lbSearchCmd.Click;

end;

procedure TfrmMain.nbCommandsChange( Sender : TObject);

  procedure MoveSearchResults( TabSheet : TTabSheet; HintIdx : integer );
  begin
    pnlDisplayObj.Parent := TabSheet;
    pnlSearchResults.Parent := TabSheet;
    pnlSearchResults.PopupMenu := TabSheet.PopupMenu;
    case HintIdx of
      0 :
        begin
          lbSearchCmd.Hint := cSearchHintGeneralCmd + cSearchHintFavorites + cSearchHintEnding;
          lbSearchCmdLIne.Hint := cSearchHintGeneralCmdLine + cSearchHintEnding;
        end;
      1 :
        begin
          lbSearchCmd.Hint := cSearchHintGeneralCmd + cSearchHintSearch;
          lbSearchCmdLIne.Hint := cSearchHintGeneralCmdLine + cSearchHintSearch;
        end;
      2 :
        begin
          lbSearchCmd.Hint := cSearchHintGeneralCmd + cSearchHintKeyWords;
          lbSearchCmdLIne.Hint := cSearchHintGeneralCmdLine + cSearchHintKeyWords;
        end;
    end;
  end;

begin

  UpdateNotebookCaptions;

  if nbCommands.ActivePage = tsFavorites then
  begin
    DisplaySearchResults( fFavoritesSR, tsFavorites );
    MoveSearchResults( tsFavorites, 0 );
  end
  else if nbCommands.ActivePage = tsSearch then
  begin
    DisplaySearchResults( fSearchSR, tsSearch );
    MoveSearchResults( tsSearch, 1 );
    CheckSearchChange;
  end
  else if nbCommands.ActivePage = tsKeyWords then
  begin
    DisplaySearchResults( fKeyWordSR, tsKeyWords );
    MoveSearchResults( tsKeyWords, 2 );
    CheckSearchChange;
  end;

end;

procedure TfrmMain.UpdateDisplay( const Output : string; DoIndicate : boolean = true );
var
{$IFDEF Bit32}
  NumLines : Integer;
{$ELSE}
  NumLines : Int64;
{$ENDIF}
  GoToPos : Integer;
  Bad : Boolean;
begin
  //fDisplayOutPutMax := 500; //testing

  Bad := false; //fix for curl output that has escape chars in it.

  Memo1.SelLength := 0;
  GoToPos := Length( fDisplayOutPut.Text );

  //do the output chopping
  if GoToPos > fDisplayOutPutMax + 5000 then
  begin
    fDisplayOutPut.Text := format( cmsgDisplayOutputTrimmed, [ fDisplayOutPutMax, TimeToStr( now ) ] )
                           + copy( fDisplayOutPut.Text, GoToPos - fDisplayOutPutMax, GoToPos );
    GoToPos := Length( fDisplayOutPut.Text );
  end;

  if pos( char(#27), output ) > 0 then
    Bad := true;

  if Bad and not fAllowESCinOutput then
    fDisplayOutPut.Text := fDisplayOutPut.Text
                           + LineEnding
                           + format( cmsgBadData_ESC, [ cmsgBadData_ESC_BASE, cReferenceProgramName ] )
                           + cmsgOutputEndIndicator
  else
    fDisplayOutPut.Text := fDisplayOutPut.Text
                           + LineEnding
                           + Output
                           + strif( Bad, format( cmsgBadData_ESC_OK, [ cmsgBadData_ESC_BASE ] ) )
                           + strif( DoIndicate , cmsgOutputEndIndicator );

  Memo1.Lines := TStrings( fDisplayOutPut );

  Memo1.SelStart := GoToPos + 1;
  if FIsInitialized and Memo1.CanFocus then
    Memo1.SetFocus;

  NumLines := trunc( memo1.Height / memo1.Font.GetTextHeight( 'X' ) );
  if Memo1.CaretPos.Y > NumLines div 2 then
    Memo1.ScrollBy( 0, trunc( Memo1.Height * 0.7 ) * -1 );

end;

procedure TfrmMain.CheckSearchChange;
begin
  if fSearchMayHaveChanged then
  begin
    fSearchMayHaveChanged := false;
    if ( fKeyWordSR.Count > 0 ) or ( fSearchSR.Count > 0 ) then
      UpdateDisplay( cAttnBar + cmsgSearchMayBeInvalid + cAttnBar );
  end;
end;

procedure TfrmMain.RenameCommand( const InputName : string; InputMode : TSingleInputMode );
var
  ComName, ComFile, ComPath: string;
begin

  ComName := '';
  ComFile := '';
  ComPath := '';
  if InvalidCommands(False) then
    exit;

  if not GetNewCommand( InputName, ComName, InputMode, lbCommands.ItemIndex ) then
    exit;

  CmdObjHelper.ProcessFileNamePath(ComName, ComFile, ComPath);

  lblCommandName.Caption := ComFile;
  UpdatePathsInEdit( ComPath );

end;

procedure TfrmMain.ToggleCmdLineDelete( const TheValue : boolean; DoToggle : boolean = True );
begin

  actCmdLineDelete.Enabled := TheValue;

  if DoToggle then
    actCmdLineUnDelete.Enabled := not actCmdLineDelete.Enabled
  else
    actCmdLineUnDelete.Enabled := TheValue;
end;

procedure TfrmMain.ToggleCommandDelete( const TheValue : boolean; DoToggle : boolean = True );
begin
  if lbCommands.Items.Count = 0 then
  begin
    actCommandDelete.Enabled := false;
    actCommandUnDelete.Enabled := false;
    exit;
  end;

  actCommandDelete.Enabled := TheValue;
  if DoToggle then
    actCommandUnDelete.Enabled := not actCommandDelete.Enabled
  else
    actCommandUnDelete.Enabled := TheValue;

end;

procedure TfrmMain.pmCmdLinesPopup(Sender: TObject);
var
  aCmdLineObj: TCmdLIneObj;
  locDoShow, ValidSelection: boolean;
begin

//strangely, to me, the popupmenu is still enabled even if the lbCmdLines is disabled!!
//so... locDoShow (DoShow conflicts with LCL)!
  locDoShow := lbCmdLines.Enabled;//Flag that CL is being edited

  actNewCommandLine.Enabled := locDoShow and not InValidCommands;

  mniCopyCLListClip.Enabled := lbCmdLines.Items.Count > 0;

  if InvalidCommandLInes then
    ToggleCmdLineDelete( False, False )
  else
  begin
    aCmdLineObj := TCmdLineObj( CmdObj.CmdLines.Objects[ lbCmdLines.ItemIndex ] );
    ToggleCmdLineDelete( not aCmdLineObj.DoDelete and locDoShow, locDoShow );
  end;

  popCmdLinePaste.Enabled := assigned( ClipCLO ) and locDoShow;

  ValidSelection := ( lbCmdLines.Items.Count > 0 ) and ( lbCmdLines.ItemIndex > -1 );

  actRun.Enabled := ValidSelection or pnlCmdLine.Visible;

  actCopyCmdLine.Enabled := ValidSelection and locDoShow;//actRun.Enabled;
  actCopyClipCmdLine.Enabled := actCopyCmdLine.Enabled;

  actFindCmdLine.Enabled := ( lbCmdLines.Items.Count > 1 ) and locDoShow;

  EnableCmdLineUpDown( locDoShow );

end;

function TfrmMain.InvalidCommands_Msg : boolean;
begin
  result := InvalidCommands;
  if result then
  begin
    MsgDlgMessage( ccapNothingLoaded, cmsgNothingLoaded );
    MsgDlgInfo( Self );
  end;
end;

function TfrmMain.InvalidCommands(CheckCmdObj: boolean = True): boolean;
begin

  Result := ( lbCommands.Count = 0 ) or ( lbCommands.ItemIndex < 0 );

  if CheckCmdObj then
    Result := Result or not assigned( CmdObj );

end;

function TfrmMain.InvalidCommandLines: boolean;
begin
  Result := (lbCmdLines.Count = 0) or (lbCmdLines.ItemIndex < 0) or
    not assigned( CmdObj );
end;


procedure TfrmMain.pmCommandsPopup(Sender: TObject);
var
  NoShow: boolean;
begin

  NoShow := pnlCommandList.Enabled;

  actPlus.Enabled := NoShow and InfoServer.IsInitialized and not BadDB;

  popCmdPaste.Enabled := assigned( ClipCO );
  popCmdPasteCmdLine.Enabled := assigned( ClipCLO );
  actFindCmd.Enabled := ( lbCommands.Items.Count > 1 );

  if InvalidCommands then
  begin
    ToggleCommandDelete(False, False);
    actRevert.Enabled := False;
    actCopyCmd.Enabled := false;
    actCopyClipCmd.Enabled := false;
    mniCmdSendTo.Enabled := false;

  end
  else
  begin

    ToggleCommandDelete(not CmdObj.DoDelete and NoShow, NoShow);
    actRevert.Enabled := CmdObj.DoUpdate and NoShow;
    actCopyCmd.Enabled := NoShow;
    actCopyClipCmd.Enabled := NoShow;
    mniCmdSendTo.Enabled := NoShow;

  end;

end;

procedure TfrmMain.popCmdLinePasteClick( Sender : TObject );
begin
  DuplicateCmdLine( ClipCLO );
end;

procedure TfrmMain.popCmdPasteClick( Sender : TObject );
begin
  DuplicateCmd( ClipCO );
end;

procedure TfrmMain.TimerBlinkStartTimer( Sender : TObject );
begin
  lblCEditing.ParentColor := true;
  lblCEditing.Visible := false;
  shpCmdLineIn.Visible := true;
  shpCmdIn.Visible := true;
end;

procedure TfrmMain.TimerBlinkStopTimer( Sender : TObject );
begin
  lblCEditing.Visible := false;
end;

procedure TfrmMain.TimerBlinkTimer( Sender : TObject );
begin
  lblCEditing.Visible := not lblCEditing.Visible;
  shpCmdLineIn.Visible := not lblCEditing.Visible;
  shpCmdIn.Visible := not lblCEditing.Visible;
end;



procedure TfrmMain.BlankCommand;
begin

  if lblCommandName.Caption = cReservedBlankCommand then
//already in blank mode
    exit;

//see note in BlankCmdLine

  lblCommandName.Caption := cReservedBlankCommand;//labels have on OnChange event
  lblCommandNameDisp.Caption := lblCommandName.Caption;

  lblPathAlias.Caption := '';
  lblPathAliasDisp.Caption := lblPathAlias.Caption;

  lblPathActual.Caption := '';

  cbSuperUser.Checked := False;
  cbTerminalOnly.Checked := False;

  cbDetachProcess.Checked := False;
  cbIsFavorite.Checked := False;

  memNotes.Lines.Text := '';//assigning text does not fire OnChange Event
  memNotesDisp.Lines.Text := memNotes.Lines.Text;

  lbKeywords.Items.Text := '';//listbox has no OnChange Event;
  lbKeywordsDisp.Items.Text := lbKeywords.Items.Text;

  cbThreatLevel.ItemIndex := -1;
  ApplyThreatLevel( pnlCEdit, cbThreatLevel );
  EchoThreatLevelDisplay( pnlC, cbThreatLevelDisp, lblThreatLevelDisp, cbThreatLevel.ItemIndex );

  edtHelp.Text := '';
  edtVersion.Text := '';
  fLastlbCommandsIdx := -1;
  fLastlbCmdLinesIdx := -1;
  actRun.Enabled := False;

end;


procedure TfrmMain.BlankCmdLine;
begin

  if memEntry.Lines.Text = cReservedBlankCommand then
//already in blank mode
    exit;

//================Note:
//Why the duplication (through events)??? Well...I want the commands and command lines to be independantly editable, no
//need to edit the command first to edit the command lines. Also Some buttons can be left active like "Quick Run" so
//they have tools to use. Can't do any of this with modal forms.
//But...I wanted single panels to suffice. But nooooo. Turns out ReadOnly properties are not applicable to all
//controls and they work differently on different controls. I was bummed. To keep my plan and have it all readable
//I opted for a "display" panel vs. and "edit" panel. Pain in the ass, can't think of any other way, but it works
//and is pretty. So, for me at least, worth the pain.


  memNotesLine.Lines.Text := '';//assigning text does not fire OnChange Event
  memNotesLineDisp.Lines.Text := memNotesLine.Lines.Text;

  cbSuperUserLine.Checked := False;

  cbDetachProcessLine.Checked := False;

  cbWantsInputLine.Checked := False;

  cbTerminalOnlyLine.Checked := False;

  cbAlertLine.Checked := False;

  cbUseShellLine.Checked := False;

  cbThreatLevelLine.ItemIndex := -1;
  ApplyThreatLevel( pnlCLEdit, cbThreatLevelLine );
  EchoThreatLevelDisplay( pnlCL, cbThreatLevelLineDisp, lblThreatLevelLineDisp, cbThreatLevelLine.ItemIndex );

  cbIsFavoriteLine.Checked := False;

  memEntry.Lines.Text := cReservedBlankCommand;

  edtFriendlyNameLine.Text := '';

  ToggleCmdLineDelete( False, False );

  NilObjects( lbCmdLines.Items );

  fLastlbCmdLinesIdx := -1;
  actRun.Enabled := False;

end;

procedure TfrmMain.EchoThreatLevelDisplay( aPnl : TPanel; aCB : TComboBox; aLBL : TLabel; const Idx : integer );
begin
  aCB.ItemIndex := Idx;
  aLBL.Caption := CmdObjHelper.GetThreatLevelText( CmdObjHelper.ListIdxToThreatLevel( Idx ) );
  ApplyThreatLevel( aPnl, aCB );
end;

procedure TfrmMain.CmdLineToForm(aCmdLineObj: TCmdLineObj);
var
  FixedEntry : String;
begin

  if InvalidCommandLines or not assigned(aCmdLineObj) then
    exit;

  with aCmdLineObj do
  begin

//see note in BlankCmdLine

    memNotesLine.Lines.Text := Notes;//assigning text does not fire OnChange Event
    memNotesLineDisp.Lines.Text := memNotesLine.Lines.Text;

    cbDetachProcessLine.Checked := DetachProcess;

    cbSuperUserLine.Checked := SuperUserMode;

    cbWantsInputLine.Checked := WantsInput;

    cbTerminalOnlyLine.Checked := TerminalOnly;

    cbAlertLine.Checked := Alert;

    cbUseShellLine.Checked := UseShell;

    cbThreatLevelLine.ItemIndex := CmdObjHelper.ThreatLevelToListIdx( ThreatLevel );
    ApplyThreatLevel( pnlCLEdit, cbThreatLevelLine );
    EchoThreatLevelDisplay( pnlCL, cbThreatLevelLineDisp, lblThreatLevelLineDisp, cbThreatLevelLine.ItemIndex );

    cbIsFavoriteLine.Checked := IsFavorite;

    edtFriendlyNameLine.Text := FriendlyName;

    FixedEntry := Entry;
    if pos( cReservedSuperUser, Entry ) = 1 then
      FixedEntry := trim( stringreplace( FixedEntry, cReservedSuperUser, '', [] ) );
    memEntry.Lines.Text := FixedEntry;

  end;

end;

procedure TfrmMain.UpdateLbCommands( const DoSave : boolean; TheIdx : integer );
begin
  if TheIdx = cUseItemIndexFlag then
    TheIdx := lbCommands.ItemIndex;
  if TheIdx > -1 then
    lbCommands.Items[lbCommands.ItemIndex] := CmdObj.GetDisplayString;
  if DoSave then
    actSave.Enabled := True
  else UpdateSaveStatus;
  pmCommandsPopup( self );
end;

procedure TfrmMain.UpdateNotebookEditingStatus;
var
  i : Integer;
begin
//Don't want them switching pages while editing, decided visibility was easier and clearer than enabled
  for i := 1 to nbCommands.PageCount - 1 do
    nbCommands.Pages[ i ].TabVisible := not Editing;
end;


procedure TfrmMain.UpdateLbCmdLines( const Idx : integer; SaveState : boolean = true );
begin

  actRun.Enabled := False;

  if not Assigned(CmdObj) then
  begin
    NilObjects( lbCmdLines.Items );
    exit;
  end;

  if ( Idx < -1 ) then
    exit;

  NilObjects( lbCmdLines.Items );
  lbCmdLines.Items.Assign( CmdObj.CmdLines );

  lbCmdLines.ItemIndex := GetValidItemIndex( Idx, lbCmdLines.Items.Count );
  lbCmdLinesClick(lbCmdLines);

  if lbCmdLines.ItemIndex > -1 then
  begin
    if SaveState then
      actSave.Enabled := True;
    actRun.Enabled := True;
  end;

  UpdateLbCommands( False );

end;

procedure TfrmMain.FormToCmdLine(aCmdLineObj: TCmdLineObj);
begin

  //see note in BlankCmdLine

  if InvalidCommandLines or not assigned(aCmdLineObj) then
    exit;

  with aCmdLineObj do
  begin

    memEntry.Lines.Text := trim( memEntry.Lines.Text );
    Entry := strif( cbSuperUserLine.Checked, cReservedSuperUser + memEntry.Lines.Text, memEntry.Lines.Text );

    edtFriendlyNameLine.Text := trim( edtFriendlyNameLine.Text );
    FriendlyName := edtFriendlyNameLine.Text;

    Notes := memNotesLine.Lines.Text;//assigning text does not fire OnChange Event
    memNotesLineDisp.Lines.Text := memNotesLine.Lines.Text;

    DetachProcess := cbDetachProcessLine.Checked;

    WantsInput := cbWantsInputLine.Checked;

    TerminalOnly := cbTerminalOnlyLine.Checked;

    Alert := cbAlertLine.Checked;

    UseShell := cbUseShellLine.Checked;

    ThreatLevel := CmdObjHelper.ListIdxToThreatLevel( cbThreatLevelLine.ItemIndex );

    SuperUserMode := cbSuperUserLine.Checked;

    IsFavorite := cbIsFavoriteLine.Checked;

  end;

  CmdObj.UpdateCmdLinesDisplay(aCmdLineObj);

  UpdateLbCmdLines(lbCmdLines.ItemIndex, actSave.Enabled or aCmdLineObj.DoUpdate);

end;

procedure TfrmMain.UpdatePathsInEdit( const ThePath : string );
begin
  lblPathAlias.Caption := format( ccapDisplayCaptionAndValue,
              [ Get_Cmd_Field_DisplayCaption( fidLocationPath ), CmdObjHelper.GetPathAlias( ThePath ) ]
                                );
  lblPathAliasDisp.Caption := lblPathAlias.Caption;
  lblPathActual.Caption := ThePath;
end;

procedure TfrmMain.CmdObjToForm( aCmdObj: TCmdObj; CLOIdx : integer = cUseItemIndexFlag );
begin

//see note in BlankCmdLine

  with aCmdObj do
  begin

    lblCommandName.Caption := CommandName;
    lblCommandNameDisp.Caption := lblCommandName.Caption;

    cbSuperUser.Checked := SuperUserMode;
    cbTerminalOnly.Checked := TerminalOnly;

    UpdatePathsInEdit( LocationPath );

    cbDetachProcess.Checked := DetachProcess;
    cbIsFavorite.Checked := IsFavorite;

    memNotes.Lines.Text := Notes;//assigning text does not fire OnChange Event
    memNotesDisp.Lines.Text := memNotes.Lines.Text;

    lbKeywords.Items.Text := Keywords;
    lbKeywordsDisp.Items.Text := lbKeywords.Items.Text;

    cbThreatLevel.ItemIndex := CmdObjHelper.ThreatLevelToListIdx( ThreatLevel );
    ApplyThreatLevel( pnlCEdit, cbThreatLevel );
    EchoThreatLevelDisplay( pnlC, cbThreatLevelDisp, lblThreatLevelDisp, cbThreatLevel.ItemIndex );

    edtHelp.Text := HelpCommand;
    edtVersion.Text := VersionCommand;

    ToggleCommandDelete(not DoDelete);

    actRevert.Enabled := DoUpdate;

    if CLOIdx = cUseItemIndexFlag then
    case CmdLines.Count of
      0 : CLOIdx := -1;
//cancel edit on CmdObj no longer resets lbcmdlines to 0 index.
      else if lbCmdLines.Count = 0 then
             CLOIdx := 0;
    end;

    UpdateLbCmdLines( CLOIdx, False );

    pmCmdLinesPopup( self );

  end;

end;

procedure TfrmMain.FormToCmdObj(aCmdObj: TCmdObj);
begin

//see note in BlankCmdLine

  with aCmdObj do
  begin

    CommandName := lblCommandName.Caption;
    lblCommandNameDisp.Caption := lblCommandName.Caption;

    SuperUserMode := cbSuperUser.Checked;
    TerminalOnly := cbTerminalOnly.Checked;

    DetachProcess := cbDetachProcess.Checked;
    IsFavorite := cbIsFavorite.Checked;

    Notes := memNotes.Lines.Text;//assigning text does not fire OnChange Event
    memNotesDisp.Lines.Text := memNotes.Lines.Text;

    Keywords := lbKeywords.Items.Text;
    lbKeywordsDisp.Items.Text := lbKeywords.Items.Text;

    edtHelp.Text := trim( edtHelp.Text );
    HelpCommand := edtHelp.Text;

    edtVersion.Text := trim( edtVersion.Text );
    VersionCommand := edtVersion.Text;

    ThreatLevel := CmdObjHelper.ListIdxToThreatLevel( cbThreatLevel.ItemIndex );

    actRevert.Enabled := DoUpdate;

    LocationPath := lblPathActual.Caption;

    UpdateLbCommands(False);

  end;

end;

procedure TfrmMain.FreeCmdDisplayObjects( Strings : TStrings );
var
  i : Integer;
begin
  for i := 0 to Strings.Count - 1 do
  begin
    if assigned( Strings.Objects[ i ] ) then
    begin
      Strings.Objects[ i ].free;
      Strings.Objects[ i ] := nil;
    end;
  end;
  Strings.Clear;
end;

function TfrmMain.AddCmdDisplayObjects( Dest : TStrings; Strings : TStringlist ) : boolean;
var
  i : Integer;
begin
  result := false;
  if Strings.Count > 0 then
  begin
    for i := 0 to Strings.Count - 1 do
    begin
      Dest.AddObject( Strings[ i ], Strings.Objects[ i ] );
      Strings.Objects[ i ] := nil;
    end;
    result := true;
    Strings.Clear;
  end;

end;

procedure TfrmMain.InsertCommandSeperator( Strings : TStrings; const Idx : integer );
begin
  Strings.Insert( Idx, '' );
  Strings.Insert( Idx, cstrCmdDisplayObjects );
  Strings.Insert( Idx, '' );
end;

procedure TfrmMain.LoadFavorites;
var
  Idx : Integer;
  SL : TStringList;
begin

  if InvalidCommands then
    exit;

  FreeCmdDisplayObjects( fFavoritesSR );

  SL := TStringlist.Create;
  try

    Idx := CmdObjHelper.Search( fFavoritesSO, SL );

    AddCmdDisplayObjects( fFavoritesSR, SL );

    if Idx > 0 then
      InsertCommandSeperator( fFavoritesSR, Idx );

    DisplaySearchResults( fFavoritesSR, tsFavorites, true );

  finally
    SL.free;
  end;

end;


procedure TfrmMain.LoadSearchResults( aSearch : TSearchObj; Strings : TStrings );
var
  SL : TStringList;
  Idx : Integer;
begin

  FreeCmdDisplayObjects( Strings );

  SL := TStringlist.Create;
  try

    Idx := CmdObjHelper.Search( aSearch, SL );

    AddCmdDisplayObjects( Strings, SL );

    if Idx > 0 then
      InsertCommandSeperator( Strings, Idx );

    DisplaySearchResults( Strings, nbCommands.ActivePage, true );

  finally
    SL.free;
  end;

end;


procedure TfrmMain.FindItem( TheLB : TListBox );
var
  PosPt : TPoint;
  Idx : SizeInt;
begin
  with TfrmFind.Create( self ) do
  try
    LB := TheLB;
    PosPt := GetPreciseControlCoords( TheLB, 30, 30 );
    Top := PosPt.y;
    Left := PosPt.x;
    Width := TheLB.Width;
    ShowModal;
    if ItemToFind <> '' then
    begin
      Idx := TheLB.Items.IndexOf( ItemToFind );
      TheLB.ItemIndex := Idx;
      TheLB.Click;
    end;
  finally
    free;
  end;
end;

procedure TfrmMain.ShowUnsavedMessage;
begin
  if UnSaved
     and MsgDlgMessage( ccapShowUnsavedMessage, cmsgShowUnsavedMessage, 'cmsgShowUnsavedMessage')
  then
       MsgDlgAttention( self );
end;

procedure TfrmMain.btnSimpleSearchClick(Sender: TObject);
var
 SimpleSearchSO : TSearchObj;
 OtherChoice : integer;
begin

  if InvalidCommands_Msg then
    exit;

  ShowUnsavedMessage;

  SimpleSearchSO := nil;

  with TfrmSimpleSearch.Create( self ) do
  try
    Initialize( fSearchFields );
    Showmodal;
    if ModalResult = mrOK then
    begin
      FillSearchObj( SimpleSearchSO );//will be created there
      if assigned( SimpleSearchSO ) then
      begin
        SimpleSearchSO.UserTag := Ord( tcsNormal );
        LoadSearchResults( SimpleSearchSO, fSearchSR );
        if nbCommands.ActivePage <> tsSearch then
          nbCommands.ActivePage := tsSearch;
      end;
    end;
    OtherChoice := UseAdancedSearch;

  finally
    free;
    if assigned( SimpleSearchSO ) then
      FreeAndNil( SimpleSearchSO );
  end;

  if OtherChoice > -1 then
  begin
    case OtherChoice of
      1 : btnSearchNewSearch.Click;
      2 : btnSearchLoadSearch.Click;
      else btnSearchSearch.Click;
    end;

    if nbCommands.ActivePage <> tsSearch then
      nbCommands.ActivePage := tsSearch;
  end;


end;

function TfrmMain.BadSearchStructure( SO : TSearchObj ) : boolean;
var
  Str : String;
begin
  result := false;
  Str := SO.InvalidStructure;
  if Str <> '' then
  begin
    MyShowmessage( cmsgSearchInvalid
                   + cmsgSearchInvalidNoLoad
                   + LineEnding + LineEnding
                   + Str,
                   self
                  );
    result := true;
  end;

end;

procedure TfrmMain.LoadSearchObject;
var
  SO : TSearchObj;//pointer only
  LoadedUT : Integer;
begin

  if InvalidCommands_Msg then
    exit;

  with TOpenDialog.Create( self ) do
  try

    Title := ccapSearchLoad;
    InitialDir := GetSearchesDirectory;
    Options := Options + [ ofNoChangeDir, ofPathMustExist, ofForceShowHidden, ofFileMustExist ];

    if Execute then
    begin

      if TSearchObj.CanLoad( FileName, LoadedUT ) then
      begin
        case TTabControlSearch( LoadedUT ) of
          tcsNormal : SO := fSearchSO;
          tcsKeyWordList : SO := fKeyWordSO;
          else raise EErrorDevelopment.Create( 'TfrmMain.LoadSearchObject: Invalid TTabControlSearch.' );
        end
      end
      else exit;

      if not SO.Load( FileName ) then
      begin
        MsgDlgMessage( ccapSearchLoadError, format( cmsgSearchLoadError, [ FileName ] ) );
        MsgDlgInfo( Self );
        exit;
      end;

 //should only need to check this on loaded searches where search file was hand edited
      if BadSearchStructure( SO ) then
        exit;

      if DisallowMisMatch( SO, GetProfileStamp ) then
        exit;

      case TTabControlSearch( SO.UserTag ) of
        tcsNormal : //Normal search
          begin
//meant to push SO to proper Tab (normal or Keywords) when loaded.
            ShowSearch( fSearchSO, fSearchSR );

            if nbCommands.ActivePage <> tsSearch then
              nbCommands.ActivePage := tsSearch;

          end;
        tcsKeyWordList :
          begin
//I had meant to make this more generic by using constant array, but turns out it is probably useless.
//but I leave it, who knows what comes in fuTure.
            ShowSearch_List( [ fidKeyWords ], fKeyWordSO, fKeyWordSR );

            if nbCommands.ActivePage <> tsKeyWords then
              nbCommands.ActivePage := tsKeyWords;
          end;
      end;

    end;

  finally
    Free;
  end;

end;

function TfrmMain.DisallowMisMatch( SO : TSearchObj; const Stamp : string ) : boolean;
begin

  result := false;
  if SO.DBInfo <> Stamp then
  begin
    MsgDlgMessage( ccapSearchDesignedInDiffDataBase,
                   format( cmsgSearchDesignedInDiffDataBase, [ SO.DBInfo, Stamp ] )
                 );
    if MsgDlgAttentionConfirm( self ) = mrNo then
    begin
      SO.Clear;
      result := true;
    end;
  end;

end;


function TfrmMain.GetProfileStamp : string;
begin
  result := fProfileName + strif( fUseDB, '  (sql)', '  (text)' );
end;

function TfrmMain.GetHelpOutput( SL : TStringList ) : string;
begin

  result := '';

//needs to be a separate section for help's because of builtin's
  StandardOutputHeader( Result, SL[ 0 ]  + ' ' + SL[ 1 ] );

  Result := Result
           + LineEnding
           + CmdObjHelper.RunCommand( SL, false, '', 'help' );

end;

function TfrmMain.CmdInPath( CheckForBuiltin : boolean = false ) : boolean;
begin
  Result := ( pos( cCommandInPathStr, lblPathAliasDisp.Caption ) > 0 )
       or ( pos( cCommandInPathStr, lblPathAlias.Caption ) > 0 );
  if Result then
    exit;
  if CheckForBuiltin then
    result := lblPathActual.Caption = cLinuxBuiltInStr;
end;

function TfrmMain.IsFileExist( const FName : string; WithNotify : boolean = false ) : boolean;
begin
  result := FileExists( FName );
  if not result and WithNotify then
  begin
    MsgDlgMessage( ccapError, format( cFileNotExist, [ FName ] ) );
    MsgDlgAttention( self );
  end;
end;

procedure TfrmMain.btnHelpCommandClick(Sender: TObject);
var
  SL : TStringList;
  str : string;
begin

  if not assigned(CmdObj) then
    exit;

//lblPathActual.Caption (not viisble in running prog ) and edtHelp.text are always correct when
//editing or not editing In editing the actual cmd obj is "buffered" and so anyone working on a
//command can change it at will and then correct help will be shown if it exists. This fixes a
//bug where when using the command object editing changes were not used.

  SL := TStringList.Create;
  try

    //cCommandInPathStr = '$PATH';
    //cLinuxBuiltInStr = '$BUILTIN';

    if lblPathActual.Caption <> cLinuxBuiltInStr then
    begin
      if CmdInPath then
      begin
        SL.Add( lblCommandName.Caption );
//        SL.Add( lblPathActual.Caption + lblCommandName.Caption );
        SL.Add( edtHelp.Text )
      end else
      begin
        str := lblPathActual.Caption + lblCommandName.Caption;
        if not IsFileExist( str, true ) then
          exit;
        //if not FileExists( str ) then
        //begin
        //  MsgDlgMessage( ccapError, format( cFileNotExist, [ str ] ) );
        //  MsgDlgAttention( self );
        //  exit;
        //end;
        SL.Add( str );
        SL.Add( edtHelp.Text )
      end;
    end else
    begin
      SL.Add( lblCommandName.Caption );
      SL.Add( 'help' );
    end;

    UpdateDisplay( GetHelpOutput( SL ) );

  finally
    SL.free;
  end;

end;


procedure TfrmMain.btnLocationPathClick(Sender: TObject);
begin

  if not assigned(CmdObj) then
    exit;

  MsgDlgMessage( format( ccapPathCaption, [ lblCommandName.Caption ] ), CmdObj.GetLiteralPath( true ) );
  MsgDlgInfo( self );

end;

procedure TfrmMain.btnRefreshFavoritesClick( Sender : TObject );
begin
  LoadFavorites;
  btnRefreshFavorites.enabled := false;
//  pnlRefreshFavorites.Visible := false;
end;

procedure TfrmMain.btnSortDetachedProcessesClick( Sender : TObject );
begin
  lbDetachedProcesses.Sorted := true;
  lbDetachedProcesses.Sorted := false;
end;

procedure TfrmMain.MoveCmdLine( const aFactor : integer );
var
  idx: integer;
begin

  idx := CmdObj.ReorderCmdLine(lbCmdLines.ItemIndex, aFactor);

  BlankCmdLine;

  UpdateLbCmdLines(Idx);

end;

procedure TfrmMain.btnVersionCommandClick(Sender: TObject);
var
  CmdStr : TCaption;
begin

  if not assigned(CmdObj) then
    exit;

  if trim( edtVersion.Text ) = '' then
  begin
    UpdateDisplay( format( cmsgNoVersionFlagSpecified, [ lblCommandName.Caption, btnVersionCommand.Caption ] ) );
    exit;
  end;

  //see note in btnHelpCommandClick;
    if lblPathActual.Caption <> cLinuxBuiltInStr then
    begin
      if CmdInPath then
      begin
        CmdStr := lblCommandName.Caption + ' ' + edtVersion.Text
      end else
      begin
        CmdStr := lblPathActual.Caption + lblCommandName.Caption;
        if not IsFileExist( CmdStr, true ) then
          exit;
        //if not FileExists( CmdStr ) then
        //begin
        //  MsgDlgMessage( ccapError, format( cFileNotExist, [ CmdStr ] ) );
        //  MsgDlgAttention( self );
        //  exit;
        //end;
        CmdStr := CmdStr + ' ' + edtVersion.Text;
      end;
    end else CmdStr := lblCommandName.Caption + ' ' + edtVersion.Text;

  //if lblPathActual.Caption <> cLinuxBuiltInStr then
  //  CmdStr := lblPathActual.Caption + lblCommandName.Caption + ' ' + edtVersion.Text
  //else CmdStr := lblCommandName.Caption + ' ' + edtVersion.Text;

  UpdateDisplay( RunCmdLineExternal( CmdStr ) );

end;

function TfrmMain.NotEditing: boolean;
begin
  Result := not Editing;
end;

function TfrmMain.CommandEditing: boolean;
begin
  Result := not TControl( fCommandButtons[ 0 ] ).Enabled;
end;

function TfrmMain.Editing: boolean;
begin
  Result := not TControl( fCommandButtons[ 0 ] ).Enabled or not
    TControl( fCommandLineButtons[ 0 ] ).Enabled;
end;

procedure TfrmMain.Toggle_pnlCmdLines( const State : boolean );
var
  i : Integer;
begin

  lbCmdLines.Enabled := State;//used as flag of CL editing status

  btnRun_Test.Caption := strif( not State, cObjlblRunTest, cObjlblRun );

  for i := 0 to fCommandLineEditingControls.Count - 1 do
    TAction( fCommandLineEditingControls[ i ] ).Enabled := State;

end;

procedure TfrmMain.ToggleEditMode( Sender : TList; const IsEdit : boolean );
var
  LocEditing : Boolean;
begin

  TControl( Sender[ 0 ] ).Enabled := not IsEdit;//Edit
  TControl( Sender[ 1 ] ).Visible := IsEdit;    //OK
  TControl( Sender[ 2 ] ).Visible := IsEdit;    //Cancel
  TControl( Sender[ 3 ] ).Visible := IsEdit;    //Editing panel
  TControl( Sender[ 4 ] ).Visible := not IsEdit;//Display panel

  LocEditing := Editing;
  pnlCommandList.Enabled := not LocEditing;

  Toggle_pnlCmdLines( TControl( fCommandLineButtons[ 0 ] ).Enabled );

  if LocEditing then
    TimerBlink.Enabled := true
  else TimerBlink.Enabled := false;

end;

procedure TfrmMain.btnCmdEditClick(Sender: TObject);
var
  aList: TList;
  IsEdit : boolean;
  DoContinue: boolean;
begin

  DoContinue := True;
  //pnlThreatlevel.ParentColor := false;
  //pnlThreatLevel.Color := clDefault;

  case TControl(Sender).Tag of
    //================
    cEditButtonsCommandTag:
      begin
        if InvalidCommands then
        begin
          ShowMessage( cmsgNoCommandSelected );
          DoContinue := False;
        end;
        aList := fCommandButtons;
        IsEdit := true
      end;
    cOKButtonCommandTag:
      begin
        aList := fCommandButtons;
        IsEdit := false;
        FormToCmdObj(CmdObj);
        actSave.Enabled := True;
      end;
    cCancelButtonCommandTag:
      begin
        aList := fCommandButtons;
        IsEdit := false;
        CmdObjToForm(CmdObj);
      end;
      //================
    cEditButtonsLineTag:
      begin
        if InvalidCommandLines then
        begin
          if not InvalidCommands then
            actNewCommandLine.Execute
          else ShowMessage( cmsgNoCommandSelected );
          DoContinue := False;
        end;
        aList := fCommandLineButtons;
        IsEdit := true;
      end;
    cOKButtonLineTag:
      begin
        aList := fCommandLineButtons;
        IsEdit := false;
        FormToCmdLine(CmdLineObj);
        actSave.Enabled := True;
      end;
    cCancelButtonLineTag:
      begin
        aList := fCommandLineButtons;
        IsEdit := false;
        CmdLineToForm(CmdLineObj);
      end;
      //================
    else
      begin
        DoContinue := False;
        raise EErrorDevelopment.Create( 'Bad Tag in btnCmdEditClick' );
      end;
  end; //case

  if not DoContinue then
    exit;

//  RefreshCmdObj;
//  ApplyThreatLevel( pnlCEdit, cbThreatLevel );

  ToggleEditMode( aList, IsEdit );
  case TControl(Sender).Tag of
    cEditButtonsCommandTag:
      if memNotes.CanFocus then memNotes.SetFocus;
    cEditButtonsLineTag:
      if memEntry.Canfocus then
      begin
        memEntry.SelStart := length( memEntry.Lines.Text );
        memEntry.SetFocus;
      end;
    cOKButtonCommandTag, cCancelButtonCommandTag :
      if lbCommands.CanFocus then lbCommands.SetFocus;
    cOKButtonLineTag, cCancelButtonLineTag :
      if lbCmdLines.Canfocus then lbCmdLines.SetFocus;
  end;

  UpdateMainActions;
  UpdateNotebookEditingStatus;

end;

procedure TfrmMain.RemoveDetachedProcess( const Idx, OldIdx : integer; aProcess : TAsyncProcess );
begin

  if assigned( aProcess ) then
  begin
    aProcess.Terminate( 0 );
    FreeAndNil( aProcess );
  end else raise EErrorDevelopment.Create( 'TfrmMain.RemoveDetachedProcess = list object not assigned.' );

  lbDetachedProcesses.Items.Delete( Idx );

  if OldIdx > -1 then
  begin
    lbDetachedProcesses.ItemIndex := GetValidItemIndex( OldIdx, lbDetachedProcesses.Items.Count );
    if lbDetachedProcesses.ItemIndex > -1 then
      lbDetachedProcesses.Click;
  end;

end;

procedure TfrmMain.btnHaltProcessClick( Sender : TObject );
var
  aProcess : TAsyncProcess;
  OldIndex : Integer;
begin
  if ( lbDetachedProcesses.Items.Count = 0 ) or ( lbDetachedProcesses.ItemIndex = -1 ) then
  begin
    ShowMessage( cmsgNoCommandSelected );
    exit;
  end;

  if MsgDlgMessage( ccapHaltProcess, cmsgHaltProcess, 'cmsgHaltProcess') then
    if MsgDlgAttentionConfirm( self ) = mrNo then
      exit;

  memDetachedProcesses.Clear;
  OldIndex := lbDetachedProcesses.ItemIndex;

  aProcess := TAsyncProcess( lbDetachedProcesses.Items.Objects[ OldIndex ] );
  RemoveDetachedProcess( OldIndex, OldIndex, aProcess );

  UpdateDetachedProcesses( '', nil );

end;

procedure TfrmMain.TimerDetachedProcessesTimer( Sender : TObject );
var
  aProcess : TAsyncProcess;
  i : Integer;
begin
  for i := lbDetachedProcesses.Items.Count - 1 downto 0 do
  begin
    aProcess := TAsyncProcess( lbDetachedProcesses.Items.Objects[ i ] );
    if not aProcess.Running and not aProcess.Active then
      RemoveDetachedProcess( i, -1, aProcess );
  end;
  UpdateDetachedProcesses( '', nil );
end;


procedure TfrmMain.actCommandDeleteExecute(Sender: TObject);
begin

  if InvalidCommands then
    exit;

  CmdObj.DoDelete := True;
  RefreshCmdObj( lbCmdLines.ItemIndex );

  UpdateSaveStatus;

  ToggleCommandDelete(False);

end;

procedure TfrmMain.actCmdLineDeleteExecute(Sender: TObject);
var
  Idx: integer;
begin

  if InvalidCommandLines then
    exit;

  Idx := lbCmdLines.ItemIndex;

  CmdObj.CmdLineDelete( Idx, True );

  UpdateLbCmdLines( Idx );

  ToggleCmdLineDelete( False );

end;

procedure TfrmMain.actAboutExecute( Sender : TObject );
begin
  with TfrmAbout.Create( self ) do
  try
    ShowModal;
  finally
    free;
  end;
end;

procedure TfrmMain.actCmdLineDownExecute(Sender: TObject);
begin

  if InvalidCommandLines or (lbCmdLines.ItemIndex = lbCmdLines.Count - 1) then
    exit;

  MoveCmdLine( 1 );

end;

procedure TfrmMain.actCmdLineUnDeleteExecute(Sender: TObject);
var
  Idx: integer;
begin

  if InvalidCommandLines then
    exit;

  Idx := lbCmdLines.ItemIndex;

  CmdObj.CmdLineDelete( Idx, False );

  UpdateLbCmdLines( Idx );

  ToggleCmdLineDelete( True );

end;

procedure TfrmMain.actCmdLineUpExecute(Sender: TObject);
begin

  if InvalidCommandLines or (lbCmdLines.ItemIndex = 0) then
    exit;

  MoveCmdLine( -1 );

end;

procedure TfrmMain.actCommandUndeleteExecute(Sender: TObject);
begin

  if InvalidCommands then
    exit;

  CmdObj.DoDelete := False;
  RefreshCmdObj( lbCmdLines.ItemIndex );
  UpdateSaveStatus;

  ToggleCommandDelete( True );

end;

procedure TfrmMain.actCopyClipCmdExecute(Sender : TObject);
begin
  if InvalidCommands then
    exit;
  ClipBoard.AsText := CmdObj.AsText;

  if not assigned( ClipCO ) then
    CmdObjHelper.CreateCmdObj( ClipCO );

  ClipCO.Copy( CmdObj, CmdObj.CommandName );

end;

procedure TfrmMain.actCopyClipCmdLineExecute(Sender : TObject);
var
  CLO : TCmdLineObj;
begin
  if lbCmdLines.ItemIndex < 0 then
    exit;

  CLO := TCmdLineObj( CmdObj.CmdLines.Objects[ lbCmdLines.ItemIndex ] );
//separate ClipBoardVersion because these can have <UPDATE> etc on them and "Entry" cleans this.
  ClipBoard.asText := CmdObjHelper.ClipBoardVersion( CLO.Entry, fRootFile );
  if not assigned( ClipCLO ) then
    ClipCLO := TCmdLineObj.Create( CLO.DBServer, CLO.IniSection, 999 );
  ClipCLO.Assign( CLO );
end;

procedure TfrmMain.DuplicateCmd( Sender : TCmdObj );
var
  NewCmdObj : TCmdObj;
  Idx , UidInt: Integer;
  NewName , Comfile, ComPath: String;
begin

  if not assigned( Sender ) then
    exit;

  NilObjects( lbCmdLines.Items );

  UidInt := 1;
  NewCmdObj := nil;
  NewName := Sender.CommandName;
  if CommandAlreadyUsed( NewName, -1 ) then
  repeat
    inc( UidInt );
    NewName := Sender.CommandName + inttoStr( UidInt );
  until not CommandAlreadyUsed( NewName, -1 );

  CmdObjHelper.CreateCmdObj( NewCmdObj );
  NewCmdObj.Copy( Sender, NewName );

  Comfile := '';
  ComPath := '';
  CmdObjHelper.ProcessFileNamePath( NewName, ComFile, ComPath );
  NewCmdObj.CommandName := ComFile;
  NewCmdObj.LocationPath := ComPath;

  Idx := lbCommands.Items.AddObject('<>', NewCmdObj);
  lbCommands.ItemIndex := Idx;

  RefreshCmdObj;

  UpdateLbCommands(True);
end;

procedure TfrmMain.actCopyCmdExecute(Sender : TObject);
begin
  If InvalidCommands then
    exit;
  DuplicateCmd( CmdObj );
end;

procedure TfrmMain.DuplicateCmdLIne( Sender : TCmdLineObj );
var
  NewCmdLineObj: TCmdLineObj;
  ObjLineIdx: integer;
begin
  if not assigned( Sender ) then
    exit;
  ObjLineIdx := CmdObj.AddCommandLine( Sender.Entry );
  NewCmdLineObj := TCmdLineObj( CmdObj.CmdLines.Objects[ ObjLineIdx ] );
  NewCmdLineObj.Copy( Sender );
  BlankCmdLine;
  UpdateLbCmdLines(ObjLineIdx);
end;

procedure TfrmMain.actCopyCmdLineExecute(Sender : TObject);
begin

  if InvalidCommands or InvalidCommandLines then
    exit;

  DuplicateCmdLIne( TCmdLineObj( CmdObj.CmdLines.Objects[ lbCmdLines.ItemIndex ] ) );

end;

procedure TfrmMain.actExitExecute( Sender : TObject );
begin
  Close;
end;

procedure TfrmMain.actFindCmdExecute( Sender : TObject );
begin
  FindItem( lbCommands );
end;

procedure TfrmMain.actFindCmdLineExecute( Sender : TObject );
begin
  FindItem( lbCmdLines );
end;

function TfrmMain.GoTo_Command( Sender : TListBox ) : boolean;
var
  sqlFix : String;
  CDO : TCmdDisplayObj;
  CLO : TCmdLineObj;
begin

  result := true;

  sqlFix := trim( lblDispCommandName.Caption );
  CDO := nil;
  CLO := nil;

  if sqlFix = '' then
  begin
//TODO sql searches don't load commmand name like text searches (main listbox only, sub list is ok)
//so, for sql, must go ask for CL's Command entry if it wasn't loaded. Only one case: CL is in top search Listbox
//quick fix for release. Deluxe version (later?) will address this better.
    if ( Sender.ItemIndex > -1 ) then
    try
      CDO := TCmdDisplayObj( Sender.Items.Objects[ Sender.ItemIndex ] );
      if assigned( CDO ) then
      begin
        CmdObjHelper.LoadCmdLineObj( CLO, CDO.CommandName, CDO.SectTab );
        sqlFix := CLO.sqlGetCommandName;
      end;
    finally
      CDO := nil; //pointer only
      if assigned( CLO ) then
        FreeAndNil( CLO );
    end;
  end;

  if CanJumpToCommand( sqlFix, //lblDispCommandName.Caption,
                       lblDispEntry.Caption,
                       lbCommands,
                       lbCmdLines ) then
    nbCommands.ActivePage := tsCommands
  else result := false;

end;

function TfrmMain.GotoCommand( Sender : TListBox ) : boolean;
begin
  result := false;
  if ( Sender.ItemIndex > -1 ) and assigned( Sender.Items.Objects[ Sender.ItemIndex ] ) then
    Result := GoTo_Command( Sender );
end;

procedure TfrmMain.ForcePaintingrefresh;
begin
  fLastlbCommandsIdx := cUseItemIndexFlag;//-2;
  lbCommandsClick( self );
end;

procedure TfrmMain.actOptionsExecute( Sender : TObject );
var
  //origSqlLib : String;
  LanguageIdx: Integer;
  MsgStr : string;
begin

  if CheckEditing then
    exit;

{$IFDEF Release}
//Todo test in all cases
  if fOpenInstances > 1 then
  begin
    MsgDlgMessage( ccapMainOptionsNotAvailable, cmsgMainMultipleCopiesOpen );
    MsgDlgAttention( self );
    exit;
  end;
{$ENDIF}

  with TfrmOptions.Create(Self) do
    try
      Caption := format( cmsgAdvancedOptions, [ btnOptions.caption ] );

      cbLanguage.Items.Assign(Languages.LanguageSL);
      cbLanguage.ItemIndex := GetMainLanguageIdx(Languages.ProgramLang, cbLanguage.Items);

//they can change and change the language, this keeps track of a real, final language change
      LanguageIdx := cbLanguage.ItemIndex;
      VerifyLangIndex := LanguageIdx;

      cbMaxOutput.Text := fMaxInOutPut;
      speDisplayMax.Value := fDisplayOutPutMax;
      speMaxOutputWait.MinValue := cMaxOutputWaitMinimum;
      speMaxOutputWait.MaxValue := cMaxOutputWaitMaximum;
      speMaxOutputWait.Value := globltMaxOutputWait;

      cbUnspecified.Checked := fWarnUnspecified;
      cbHarmless.Checked := fWarnHarmless;
      cbCareful.Checked := fWarnCareful;
      cbCaution.Checked := fWarnCaution;
      cbDanger.Checked := fWarnDanger;
      cbAllowMultipleOpens.Visible := not fSuperUser;
      cbAllowMultipleOpens.Checked := fAllowMultipleOpens;
      cbAllowESCOutput.Checked := fAllowESCinOutput;
      cbMissingSqlMsg.Checked := fDoShowSqlMissing;
      cbManRefreshFavorites.Checked := fManRefreshFavorites;

      Root_File := fRootFile;
      lblRootFile.Caption := Root_File;

      cbSqlDB.Checked := fAllowSqlDB;
      cbSqlDB.Enabled := ( fProfileName <> cDefaultDBProfileName )
                         or ( ( fProfileName = cDefaultDBProfileName ) and not fUseDB );
      cbTextDB.Checked := fAllowTextDB;
      cbTextDB.Enabled := ( fProfileName <> cDefaultDBProfileName )
                         or ( ( fProfileName = cDefaultDBProfileName ) and fUseDB );

      //origSqlLib := fSqliteLibrary;
      lblSqlLib.Caption := fSqliteLibrary;

      cbLargerFont.Checked := globFontsLarge;
      lblSavePath.Caption := fSavingToPath;
      lblBaseFolder.Caption := fWritingToPath;

      Showmodal;

      if cbLargerFont.checked <> globFontsLarge then
      begin
        globFontsLarge := cbLargerFont.checked;
        ApplyChangeFont( Self, true );
      end;

      if VerifyLangIndex <> LanguageIdx then //cbLanguage.ItemIndex then
        if MsgDlgMessage(ccapChangeLangDoReset, cmsgChangeLangDoReset, 'cmsgChangeLangDoReset') then
          if MsgDlgConfirmation( self ) = mrYes then
            MsgDlgParams.ResetDoNotShowList;

      fWarnUnspecified := cbUnspecified.Checked;
      fWarnHarmless    := cbHarmless.Checked;
      fWarnCareful     := cbCareful.Checked;
      fWarnCaution     := cbCaution.Checked;
      fWarnDanger      := cbDanger.Checked;
      fDoShowSqlMissing := cbMissingSqlMsg.Checked;
      fAllowMultipleOpens := cbAllowMultipleOpens.Checked;
      fAllowESCinOutput := cbAllowESCOutput.Checked;

      fManRefreshFavorites := cbManRefreshFavorites.Checked;

      fAllowSqlDB  := cbSqlDB.Checked;
      fAllowTextDB := cbTextDB.Checked;
      fMaxInOutPut := cbMaxOutput.Text;
      globltProcessMaxOutput := ConvertSizeStrToInt( fMaxInOutPut );
      globltMaxOutputWait := speMaxOutputWait.Value;
      fDisplayOutPutMax := speDisplayMax.Value;

//Now save general formsettings, I like formsettings to be saved immediately
      with fIFS do
      begin

        WriteString( cSectTabFormSettings, cFormSettingsMaxInOutPutCol, fMaxInOutPut );
        WriteInteger( cSectTabFormSettings, cFormSettingsOutPutDisplayMax, fDisplayOutPutMax );
        WriteInteger( cSectTabFormSettings, cFormSettingsMaxOutputWait, globltMaxOutputWait );
        WriteBool( cSectTabFormSettings, cFormSettingsAllowSqlDB, fAllowSqlDB );
        WriteBool( cSectTabFormSettings, cFormSettingsAllowTextDB, fAllowTextDB );
        WriteBool( cSectTabFormSettings, cFormSettingsManRefreshFav, fManRefreshFavorites );
        WriteBool( cSectTabFormSettings, cFormSettingsLargerFont, globFontsLarge );
        WriteBool( cSectTabFormSettings, cFormSettingsDoShowSqlMissing, fDoShowSqlMissing );

        if ( fSqliteLibrary <> lblSqlLib.Caption ) then
        begin
          fSqliteLibrary := lblSqlLib.Caption;
          TInfoServer.Rechecking_SqliteIsActive;
          if not TInfoServer.SqliteInstalled( fSqliteLibrary, MsgStr ) then
          begin
            UpdateDisplay( format( cmsgSQLiteLibNotFound, [ fSqliteLibrary ] )
                           + LineEnding + LineEnding
                           + cmsgSqlLibNotFound,
                           false );
            if MsgStr <> '' then
              UpdateDisplay( MsgStr, false );
          end;
          WriteString( cSectTabCurrSqliteLibrary, cCurrSqliteLibraryPath, fSqliteLibrary );
        end;

        if ( Root_File <> fRootFile ) then
        begin
          fRootFile := Root_File;
          WriteString( cSectTabFormSettings, cFormSettingsRootFile, fRootFile );
        end;

        if lblSavePath.Caption <> fSavingToPath then
        begin
          fSavingToPath := lblSavePath.Caption;
          MsgDlgParams.SavePath := fSavingToPath;
          WriteString( cSectTabFormSettings, cFormSettingsSavingPath, fSavingToPath );
        end;

        WriteBool( cSectTabFormSettings, cFormSettingsWarnUnspecified, fWarnUnspecified );
        WriteBool( cSectTabFormSettings, cFormSettingsWarnHarmless, fWarnHarmless );
        WriteBool( cSectTabFormSettings, cFormSettingsWarnCareful, fWarnCareful );
        WriteBool( cSectTabFormSettings, cFormSettingsWarnCaution, fWarnCaution );
        WriteBool( cSectTabFormSettings, cFormSettingsWarnDanger, fWarnDanger );
        WriteBool( cSectTabFormSettings, cFormSettingsAllowMultipleOpens, fAllowMultipleOpens );
        WriteBool( cSectTabFormSettings, cFormSettingsAllowESCOutput, fAllowESCinOutput );

        UpdateFile;

      end;

      if lblBaseFolder.Caption <> fWritingToPath then
      begin
        WritePathOverride( lblBaseFolder.Caption );
        MyShowmessage( cOutPutChangedBaseFolder, self );
        UpdateDisplay( cOutPutChangedBaseFolder, false );
      end;

    finally
      Free;
    end;

end;

procedure TfrmMain.actOutPutClearExecute( Sender : TObject );
begin
  fDisplayOutPut.Clear;
  UpdateDisplay( format( cmsgDisplayOutputCleared, [ TimeToStr( now ) ] ), false );
end;

procedure TfrmMain.actOutPutCopyExecute( Sender : TObject );
begin
  if Memo1.SelLength > 0 then
    Clipboard.AsText := Memo1.SelText
  else Clipboard.AsText := Memo1.Text;
end;

procedure TfrmMain.actOutPutSaveExecute( Sender : TObject );
var
  aFile, aName : string;
  SL : TStringList;
begin

  aName := stringreplace( DateTimetostr( now ), ' ', '_', [ rfreplaceall ] );
  aName := format( cSaveToFileOutput, [ aName ] );

  aFile := fSavingToPath + format( cSaveToFileTemplate, [ aName ] );

  if DoSingleInput( csiChooseAFile, aFile, simFile, self, false, true ) then
  begin
    if fileexists( aFile ) then
      if MsgdlgMessage( ccapSaveFileExists, format( cmsgSaveFileExists, [ aFile ] ) ) then
        if MsgDlgAttentionConfirm( self ) = mrNo then
          exit;

    if Memo1.SelLength > 0 then
    begin
      try
        SL := TStringList.Create;
        SL.Text := Memo1.SelText;
        SL.SaveToFile( aFile );
      finally
        SL.free;
      end;
    end
    else Memo1.Lines.SaveToFile( aFile );

  end;

end;

procedure TfrmMain.actOutputWrapExecute( Sender : TObject );
begin
  actOutPutWrap.Checked := not actOutPutWrap.checked;
  Memo1.WordWrap := actOutPutWrap.Checked;//cbWordWrapMain.Checked;
end;

procedure TfrmMain.OpenProfiles( const IsSwitchMode : boolean );
begin

  if Editing then
  begin
    MsgDlgMessage( ccapSwitchDBNotAllowed, cmsgSwitchDBNotAllowed );
    MsgDlgInfo( self );
    exit;
  end;

  if actSave.Enabled then
  begin
    if IsSwitchMode then
    begin
      MsgDlgMessage( ccapSwitchDBSave, cmsgSwitchDBSave );
      if MsgDlgConfirmation( self ) = mrYes then
        actSave.Execute;
    end
    else
    begin
      MsgDlgMessage( ccapSwitchDBSave, cmsgManageDBSave );
      MsgDlgInfo( self );
      exit;
    end;
  end;

  with TfrmProfiles.Create( self ) do
    try

      CurrProfileIsDB := fUseDB;
      CurrProfileName := fProfileName;
      ProgDefaultPath := fWritingToPath;
      IFile := fIFS;
      AllowDefSql := fAllowSqlDB;
      AllowDefText := fAllowTextDB;

      IsSelectMode := IsSwitchMode;

      Showmodal;

      if IsSelectMode and ( ModalResult = mrOK ) then
      begin

        fProfileName := CurrProfileName;
        fUseDB := CurrProfileIsDB;
        fProfilePath := CurrProfilePath;

        fIFS.WriteString( cSectTabCurrProfile, cCurrProfileName, fProfileName );
        fIFS.WriteBool( cSectTabCurrProfile, cCurrProfileIsDB, fUseDB );
        fIFS.WriteString( cSectTabCurrProfile, cCurrProfilePath, fProfilePath );
        fIFS.UpdateFile;

        SwitchDB( fProfileName, fUseDB );
//SwitchDB( '//g//gll', true/false ); //bad file name testing
        nbCommands.ActivePage := tsCommands;
      end;

    finally
      Free;
    end;

end;

procedure TfrmMain.SelectProfile;
begin

  if InvalidCommands then
    exit;

  with TfrmProfiles.Create( self ) do
    try
      Caption := ccapSendToProfile;

      CurrProfileIsDB := fUseDB;
      CurrProfileName := fProfileName;
      ProgDefaultPath := fWritingToPath;
      IFile := fIFS;
      AllowDefSql := fAllowSqlDB;
      AllowDefText := fAllowTextDB;

      IsSelectMode := True;

      Showmodal;

      if ModalResult = mrOK then
        UpdateDisplay( MergeOne( CmdObj,
                                 fProfileName
                                 + strif( fUseDB, cDefaultDBProfileIsDBStr, cDefaultDBProfileIsDBStrNot ) ),
                                 false );
    finally
      Free;
    end;

end;


procedure TfrmMain.actSwitchDBExecute( Sender : TObject );
begin
  OpenProfiles( true );
end;

procedure TfrmMain.btnBuilderClick( Sender : TObject );
var
  Input, Output : string;
begin
  Input := memEntry.Text;
  OutPut := '';
  if not EditCommandLine( format( ccapGenericEdit, [ cNameItem_CommandLine ] ), Input, Output, false ) then
    exit;
  memEntry.Text := Output;
end;

procedure TfrmMain.btnCancelRunClick( Sender : TObject );
begin
  globltDoCancelProcess := true;
end;

procedure TfrmMain.actProfilesExecute( Sender : TObject );
begin
  OpenProfiles( false );
end;

procedure TfrmMain.actQuickRunExecute( Sender : TObject );
begin
//only shown in developer mode, decided to dangerous (no checks) to have it as a general option
{$IFNDEF RELEASE}
  if not DoSingleInput( ccapQuickRun, fLastQuickRun, simEdit, self, false ) then
    exit;

  if not CanRunCmdLine( fLastQuickRun, 0, false ) then
    exit;

  UpdateDisplay( RunCmdLineExternal( fLastQuickRun ) );

  fIFS.WriteString( cSectTabFormSettings, cFormSettingsLastQuickRun, fLastQuickRun );
{$ENDIF}
end;

procedure TfrmMain.RunCmdDisplayObj( Sender : TListBox );
begin
  if Sender.ItemIndex < 0 then
    exit;

  if not CanRunCmdLine( lblDispEntry.Caption,
                        cbDispThreatLevel.Itemindex,
                        lblDispSuperUser.Visible,
                        lblDispTerminalOnly.Visible,
                        strif( lblDispAlert.Visible, memDispNotes.Text  )
                        ) then
    exit;

  UpdateDisplay(
    RunCmdLine(
                lblDispEntry.Caption,
                lblDispWantsInput.Visible,
                lblDispUseShell.Visible,
                lblDispDetachProcess.Visible )
               );
end;

procedure TfrmMain.actSaveExecute( Sender : TObject );
var
  OldIndex, i, OldCLOIndex: integer;
  aCmdObj : TCmdObj;
begin

  if CheckEditing then
    exit;

  OldCLOIndex := lbCmdLines.ItemIndex;
  Screen.Cursor := crHourGlass;
  try

    BlankCommand;
    BlankCmdLine;

    OldIndex := lbCommands.ItemIndex;

    InfoServer.Initiate_Save;
    try

      for i := lbCommands.Items.Count - 1 downto 0 do
      begin
        aCmdObj := TCmdObj(lbCommands.Items.Objects[i]);
        if not Assigned(aCmdObj) then
          continue;
        if aCmdObj.DoUpdate then
        begin
          if aCmdObj.DoDelete then
          begin
            aCmdObj.RemoveData;
            FreeCommand( i );
            lbCommands.Items.Delete( i );
            if OldIndex >= i then
              Dec( OldIndex );
            continue;
          end;
          aCmdObj.Save;
          if not fSearchMayHaveChanged then
            fSearchMayHaveChanged := true;
          lbCommands.Items[i] := aCmdObj.CommandName;
        end;
        FreeCommand(i);
      end;

      //raise exception.create( 'boo!' );//testing
      InfoServer.Finalize_Save( 1 );

    except
      on e:exception do
      begin
        InfoServer.Finalize_Save( -1 );
        GlobFatalException := true;//list is in disarray! Restart and reload.
        EErrorRaise( e, cEErrorDatabaseProblem, 'TfrmMain.actSaveExecute: ' );
      end;
    end;

    OldIndex := GetValidItemIndex( OldIndex, lbCommands.Items.Count );
    lbCommands.ItemIndex := OldIndex;

    if not InfoServer.SaveAll then
    begin
      UpdateDisplay( format( cmsgFailedSave, [ TimeToStr( now ) ] ), false );
      exit;
    end;

    CmdObj := nil;
    RefreshCmdObj( OldCLOIndex );

    if not fManRefreshFavorites then
      btnRefreshFavorites.Click
    else btnRefreshFavorites.Enabled := true;

    UpdateDisplay( format( cmsgCommandsSaved, [ TimeToStr( now ) ] ), false );

    CheckSearchChange;

  finally
    Screen.Cursor := crDefault;
    actSave.Enabled := False;
  end;

end;

procedure TfrmMain.actSearchDisplayKeyCmdExecute( Sender : TObject );
begin
  UpdateDisplay( fKeyWordSO.GetAllExpressions );
end;

procedure TfrmMain.actSearchDisplaySearchExecute( Sender : TObject );
begin
  UpdateDisplay( fSearchSO.GetAllExpressions );
end;

procedure TfrmMain.actSearchFindCmdExecute( Sender : TObject );
begin
  FindItem( lbSearchCmd );
end;

procedure TfrmMain.actSearchFindCmdLineExecute( Sender : TObject );
begin
  FindItem( lbSearchCmdLine );
end;

procedure TfrmMain.actSearchGoToCmdExecute( Sender : TObject );
begin
  GotoCommand( lbSearchCmd );
end;

procedure TfrmMain.actSearchGoToCmdLineExecute( Sender : TObject );
begin
  GotoCommand( lbSearchCmdLine );
end;

procedure TfrmMain.actSearchKeyCmdExecute( Sender : TObject );
begin
  ShowSearch_List( [ fidKeyWords ], fKeyWordSO, fKeywordSR );
end;

procedure TfrmMain.actSearchLoadKeyCmdExecute( Sender : TObject );
begin
  LoadSearchObject;//( fKeyWordSO );
end;

procedure TfrmMain.actSearchLoadSearchExecute( Sender : TObject );
begin
  LoadSearchObject;//( fSearchSO );
end;

procedure TfrmMain.actSearchNewKeyCmdExecute( Sender : TObject );
begin
  ShowSearch_List( [ fidKeyWords ], fKeyWordSO, fKeywordSR, false );
end;

procedure TfrmMain.actSearchNewSearchExecute( Sender : TObject );
begin
  ShowSearch( fSearchSO, fSearchSR, false );
end;

procedure TfrmMain.actSearchRunExecute( Sender : TObject );
begin
  if lbSearchCmdLine.Items.Count = 0 then
    RunCmdDisplayObj( lbSearchCmd )
  else RunCmdDisplayObj( lbSearchCmdLine );
end;

procedure TfrmMain.actSearchSaveKeyCmdExecute( Sender : TObject );
begin
  SaveSearch( fKeyWordSO );
end;

procedure TfrmMain.actSearchSaveSearchExecute( Sender : TObject );
begin
  SaveSearch( fSearchSO );
end;

procedure TfrmMain.actSearchSearchExecute( Sender : TObject );
begin
  ShowSearch( fSearchSO, fSearchSR );
end;

procedure TfrmMain.ApplyListChangesDisplayedCmd( SrcList, aListBox, DispListBox : TListBox );
var
  i : Integer;
begin
//List changes were made from WITHIN a command, update all new additions, deletions, edits
  if not assigned( SrcList ) then
    exit;
  if assigned( aListBox ) then
  begin
    alistbox.Items.BeginUpdate;
    for i := 0 to SrcList.Items.Count - 1 do
    begin
      if SrcList.Selected[ i ] and ( aListBox.Items.IndexOf( SrcList.Items[ i ] ) < 0 ) then
        alistbox.Items.Add( SrcList.Items[ i ] );
    end;
    alistbox.Items.EndUpdate;
    alistbox.Sorted := True;
    alistbox.Sorted := False;
    alistbox.ItemIndex := -1;
    if assigned( DispListBox ) then
      DispListBox.Items.Text := aListBox.Items.Text;
  end;
end;

procedure TfrmMain.ApplyListChanges( theList: string; aListBox, DispListBox : TListBox );
var
  i : Integer;
begin
//List changes made either from Master List handler or from within a command.
//either way all commands must be updated as appropriate
  if fSearchMayHaveChanged then
  begin
//for List changes to db to be seen, any opened cmd obj's need to be "free"'d to force them
//to load from the database again and/or refreshed appropriately based on their update status.
    for i := 0 to lbCommands.Items.Count - 1 do
    begin
      if not Assigned( lbCommands.Items.Objects[ i ] )
         or ( i = lbCommands.ItemIndex ) //the currently displayed command
         or TCmdObj( lbCommands.Items.Objects[ i ] ).DoUpdate then
        continue;
      FreeCommand( i, false );
    end;
    if not assigned( aListBox ) or not assigned( DisplistBox ) then
      exit;
//displayed cmdobj may have changed, so just reset just in case
    aListBox.Items.Text := theList;
    aListBox.Sorted := true;
    aListBox.Sorted := false;
    DispListBox.Items.Text := aListBox.Items.Text;
  end;

  CheckSearchChange;

end;

procedure TfrmMain.ShowKeyWords( const aTarget : string; aListBox : TListbox; DispListBox : TListbox );
begin

  if not assigned( fKeyWordList ) then
    exit;  //switch DB failed and no new DB has been found / loaded

  with TfrmListManager.Create(self) do
  try

    ListObj := fKeyWordList;

    DisplayWordPlural := KeyWordStruct.DisplayCaptionPlural;
    DisplayWordSingular := KeyWordStruct.DisplayCaptionSingular;
    ClueTarget := aTarget;
    ManagementMode := not assigned(aListbox);//IsManagement;
    ListType := ord( tcsKeyWordList );

    Showmodal;

    fSearchMayHaveChanged := fKeyWordList.NeedsDBUpdate;
    if fSearchMayHaveChanged then
      IsBusy( True, format( cDisplayUpdating, [ KeyWordStruct.DisplayCaptionPlural ] ) );

    fKeyWordList.ApplyUpdateList( dbltKeyWord, lbCommands.Items, lbCommands.ItemIndex );

    if assigned( CmdObj ) then
      ApplyListChanges( CmdObj.KeyWords, lbKeyWords, lbKeyWordsDisp );

    if ModalResult = mrOk then
      ApplyListChangesDisplayedCmd( lbList, aListBox, DispListBox );

  finally
    IsBusy( False );
    Free;
  end;

end;

procedure TfrmMain.IsBusy( const TurnOn : boolean; aMsg : string; ProcessFlag : TProcessType );

  //procedure EnsureDamnMessageShowsUp;
  //begin
  //  //make sure labal shows, probably overkill but I've had problems with this
  //  //when everything is running full speed.
  //  frmBusy.lblMsg.Invalidate;
  //  application.ProcessMessages;
  //end;

begin
  if TurnOn then
  begin
//todo figure this out pleez.
{$IFNDEF Release}
    UpdateDisplay( cmsgMainWorking, false );
{$ENDIF}
UpdateDisplay( '<busy...>', false );
Screen.Cursor := crHourglass;
    //frmBusy.lblMsg.Caption := aMsg;
    //frmBusy.ShowPoint := GetPreciseControlCoords( btnSave );
    //case ProcessFlag of
    //  ptProcess : frmBusy.ProcessMode;//( ptProcess );
    //  ptPipe    : frmBusy.ProcessMode;//( ptPipe );
    //  else
    //  begin
    //    frmBusy.Show;
    //    EnsureDamnMessageShowsUp;
    //    Screen.Cursor := crHourglass;
    //  end;
    //end;
  end
  else
  begin
    Screen.Cursor := crDefault;
    UpdateDisplay( '<idle...>', false );
    //if not frmBusy.Showing then
    //begin
    //  frmBusy.Timer1.Enabled := false;
    //  exit;
    //end;
    //EnsureDamnMessageShowsUp;
    //frmBusy.Shutdown;//Hide;
  end;
end;


procedure TfrmMain.btnKeyWordAddClick(Sender: TObject);
begin
  ShowKeyWords( lblCommandName.Caption, lbKeywords, lbKeyWordsDisp );
  ReFocus_Edit_Memo( true );
end;

procedure TfrmMain.btnKeyWordDeleteClick(Sender: TObject);
var
  i: integer;
begin
  if lbKeyWords.SelCount = 0 then
  begin
    ShowMessage( cmsgNoCommandSelected );
    exit;
  end;

  for i := lbKeywords.Items.Count - 1 downto 0 do
    if lbKeywords.Selected[ i ] then
      lbKeywords.Items.Delete( i );
  lbKeywordsDisp.Items.Text := lbKeywords.Items.Text;
  ReFocus_Edit_Memo( true );

end;


procedure TfrmMain.ShowSearch(SO : TSearchObj; Strings : TStrings; DoLoad : boolean = true );
var
  OtherChoice : boolean;
begin

  if InvalidCommands_Msg then
    exit;
  ShowUnsavedMessage;

  with TfrmSearch.Create( Self ) do
  try

    Caption := format( cfseSearchCaption, [ cfseSearchCaptionFields ] );

    if not Initialize( fSearchFields, SO, DoLoad, GetProfileStamp, semNormal ) then
      MyShowmessage( cmsgSearchInvalid, self );

    Showmodal;

    if ModalResult = mrOK then
    begin

      SO.Clear( edtSearchFileName.Text );
      SO.DBInfo := edtCurrProf.Text;

      TransferSearchCriteria( SO );

      if not SO.Loaded then
      begin
        MyShowmessage( cmsgSearchInvalid, self );
        exit;
      end;

      LoadSearchResults( SO, Strings );

    end;
    OtherChoice := UseSimpleSearch;

  finally
    free;
  end;

  if OtherChoice then
    btnSimpleSearch.Click;

end;

procedure TfrmMain.ShowSearch_List( FIDs : array of TCmdFieldID; SO : TSearchObj;
                                    Strings : TStrings; DoLoad : boolean = true );
var
  SL : TStringList;
  Idx , i: Integer;
begin

  if InvalidCommands_Msg then
    exit;
  ShowUnsavedMessage;

  SL := TStringList.Create;
  with TfrmSearch.Create( Self ) do
  try

    Caption := format( cfseSearchCaption, [ cfseSearchCaptionLists ] );

//this process makes a special list of Fields to be used / selected from
    for i := Low( FIDs ) to High( FIDs ) do
    begin
      Idx := ord( FIDs[ i ] );

//don't display if there are no Master keywords or more generically Master "Lists"!
      //if TCmdListObj.IsInListAndEmpty( CmdStruct[ Idx ].DisplayCaption ) then
      if TCmdListObj.IsInListAndEmpty( CmdStruct[ Idx ].ColumnName ) then
      begin
        MsgDlgMessage( ccapMasterListEmpty, format( cmsgMasterListEmpty, [ CmdStruct[ Idx ].ColumnName ] ) );
        MsgDlgInfo( self );
        continue;
      end;

      SL.AddObject(
               CmdStruct[ Idx ].DisplayCaption,
               TSearchFieldRef.Create( CmdStruct[ Idx ].CmdObjType, CmdStruct[ Idx ].FieldID )
                  );
    end;

    if SL.Count = 0 then
      exit;

    if not Initialize( SL, SO, DoLoad, GetProfileStamp, semLists ) then
      MyShowmessage( cmsgSearchInvalid, self );

    Showmodal;

    if ModalResult = mrOK then
    begin

      SO.Clear( edtSearchFileName.Text );
      SO.DBInfo := edtCurrProf.Text;

      TransferSearchCriteria( SO );

      if not SO.Loaded then
      begin
        MyShowmessage( cmsgSearchInvalid, self );
        exit;
      end;

      LoadSearchResults( SO, Strings );

    end;

  finally
    free;
    for i := 0 to SL.Count - 1 do
      SL.Objects[ i ].Free;
    SL.Free;
  end;

end;

procedure TfrmMain.SaveSearch( SO : TSearchObj );
var
  NewName, Path: String;
begin

  if InvalidCommands_Msg then
    exit;

  if not SO.Loaded then
  begin
    MyShowmessage( cmsgSearchInvalid, self );
    exit;
  end;

  if pos( '//', SO.Filename ) > 0 then
  begin
    MyShowmessage( cmsgInvalidLinuxFileName, self );
    exit;
  end;

  NewName := SO.FileName;

  if not DoSingleInput( ccapSearchFileName, NewName, simEdit, self, false ) then
    exit;

  Path := GetSearchesDirectory + NewName;

  if fileexists( Path ) then
  begin
    MsgDlgMessage( format( ccapFileExists, [ cNameItem_Search ] ),
                   format( cmsgFileExistsOverwrite, [ Path ] )
                  );
    if MsgDlgConfirmation( Self ) = mrNo then
      exit;
  end;

  SO.FileName := NewName;

  SO.Save( Path, GetProfileStamp );

end;

procedure TfrmMain.actManageKeyWordsExecute(Sender: TObject);
begin
  if CheckEditing or BadDB then
    exit;
  ShowKeyWords( '' );
end;

function TfrmMain.GetNewCommandLine( const PreString : string; var ResultStr : string ) : boolean;
var
  Str : string;
begin

  Result := False;
  Str := PreString;

  while True do
  begin

    if not DoSingleInput( ccapAddCommandLine, Str, simEdit, self ) then
      exit;

    if not CommandNameIsAllowed( Str, -1 ) then
      Continue
    else break;

  end;

  ResultStr := Str;
  Result := True;

end;


function TfrmMain.EditCommandLine( const aCaption, Instr : string; var OutStr : string; ShowCmd : boolean = true ) : boolean;
begin

  result := false;
  with TfrmCmdLineEdit.Create( self ) do
  try
    if ShowCmd then
      caption := aCaption + format( ' %s:  ', [ UpperCase( cNameItem_Command ) ] ) + lblCommandName.Caption;

    HelpCommand := CmdObjHelper.GetNormalizedPath( lblPathAlias.Caption,
                                                   lblPathActual.Caption,
                                                   lblCommandName.Caption );
    HelpParameter := edtHelp.Text;
    btnDefaultHelp.Caption := format( cclecapDefaultHelp, [ lblCommandName.Caption ] );

    memCmdLine.Lines.Text := Instr;
    ShowModal;
    if ModalResult = mrOK then
    begin
      OutStr := trim( memCmdLine.Lines.Text );
      if OutStr <> '' then
        result := true;
    end;

  finally
    free;
  end;
end;

procedure TfrmMain.edtFriendlyNameLineChange( Sender : TObject );
begin
  lblFriendlyNameLineDisp.Caption := StrIf( edtFriendlyNameLine.Text <> '', DoubleQuotedString( edtFriendlyNameLine.Text ) );
end;

procedure TfrmMain.edtHelpChange( Sender : TObject );
begin
  lblHelpDisp.Caption := format( ccapDisplayCaptionAndValue,
                        [ Get_Cmd_Field_DisplayCaption( fidHelpCommand ), edtHelp.Text ]
                               );
end;

procedure TfrmMain.edtVersionChange( Sender : TObject );
begin
  lblVersionDisp.Caption := format( ccapDisplayCaptionAndValue,
                           [ Get_Cmd_Field_DisplayCaption( fidVersionCommand ), edtVersion.Text ]
                                  );
end;

procedure TfrmMain.actNewCommandLineExecute(Sender: TObject);
var
  ObjLineIdx: integer;
  InPut, OutPut : string;
begin

  if InvalidCommands then
  begin
    ShowMessage( cmsgNoCommandSelected );
    exit;
  end;

  try
    screen.Cursor := crHourglass;

    if CmdInPath( true ) then
      Input := trim( lblCommandName.Caption ) + ' '
    else Input := trim( lblPathActual.Caption + lblCommandName.Caption ) + ' ';

    OutPut := '';
    if not EditCommandLine( format( ccapGenericAdd, [ cNameItem_CommandLine ] ), Input, Output ) then
      exit;

    BlankCmdLine;

    OutPut := strif( CmdObj.SuperUserMode, cReservedSuperUser ) + Output;

    ObjLineIdx := CmdObj.AddCommandLine( Output );
    TCmdLineObj( CmdObj.CmdLines.Objects[ ObjLineIdx ] ).SuperUserMode := cbSuperUser.Checked;
    TCmdLineObj( CmdObj.CmdLines.Objects[ ObjLineIdx ] ).DetachProcess := cbDetachProcess.Checked;
    TCmdLineObj( CmdObj.CmdLines.Objects[ ObjLineIdx ] ).TerminalOnly := cbTerminalOnly.Checked;

    //here copy from the COMMAND, not the CL threatlevel, its a convenience for the user.
    TCmdLineObj( CmdObj.CmdLines.Objects[ ObjLineIdx ] ).ThreatLevel := CmdObjHelper.ListIdxToThreatLevel( cbThreatLevel.ItemIndex );

    UpdateLbCmdLines(ObjLineIdx);
    btnLineEdit.Click;

  finally
    screen.Cursor := crDefault;
  end;

end;

procedure TfrmMain.actPlusExecute(Sender: TObject);
begin
  if NewCommand( simFile ) then
    btnCmdEdit.Click;
end;

function TfrmMain.NewCommand( const InputMode : TSingleInputMode ) : boolean;
var
  aCmdObj: TCmdObj;
  Idx: integer;
  ComName, ComFile, ComPath: string;
begin

  result := false;

  ComName := '';//get rid of compiler hints
  ComFile := '';
  ComPath := '';
  aCmdObj := nil;
  if not GetNewCommand( '', ComName, InputMode, -1 ) then
    exit;

  CmdObjHelper.ProcessFileNamePath(ComName, ComFile, ComPath);

  CmdObjHelper.CreateCmdObj( aCmdObj );

  aCmdObj.CommandName := ComFile;
  aCmdObj.LocationPath := ComPath;
  aCmdObj.HelpCommand := '--help';
  aCmdObj.VersionCommand := '--version';
  aCmdObj.IsNew := True;

  Idx := lbCommands.Items.AddObject('<newXcmd>', aCmdObj);

  lbCommands.ItemIndex := Idx;

  RefreshCmdObj;

  UpdateLbCommands(True);

  result := true;

end;


procedure TfrmMain.FreeCommand( const ItemIdx : integer; DoBlank : boolean; NewIdx : integer );
var
  aCmdObj : TCmdObj;
begin

  if DoBlank then
  begin
    BlankCmdLine;
    BlankCommand;
  end;

  aCmdObj := TCmdObj( lbCommands.Items.Objects[ ItemIdx ] );
  if assigned( aCmdObj ) then
  begin
    FreeAndNil( aCmdObj );
    lbCommands.Items.Objects[ ItemIdx ] := nil;
  end;
  if NewIdx > -1 then
  begin
    lbCommands.ItemIndex := NewIdx;
    RefreshCmdObj;
  end;
end;

procedure TfrmMain.UpdateSaveStatus;
var
  i: integer;
  aCmdObj: TCmdObj;
  NeedSave: boolean;
begin

  NeedSave := False;

  for i := 0 to lbCommands.Items.Count - 1 do
  begin
    aCmdObj := TCmdObj(lbCommands.Items.Objects[i]);
    if not Assigned(aCmdObj) then
      continue;
    if aCmdObj.DoUpdate then
    begin
      NeedSave := True;
      break;
    end;
  end;

  actSave.Enabled := NeedSave;

end;

procedure TfrmMain.actRevertExecute(Sender: TObject);
begin

  if InvalidCommands then
    exit;

  if MsgDlgMessage( ccapRevert, cmsgRevert, 'cmsgRevert') then
    if MsgDlgAttentionConfirm( self ) = mrNo then
      exit;

  if CmdObj.IsNew then
  begin
    actCommandDelete.Execute;
    exit;
  end;

  lbCommands.Items[lbCommands.ItemIndex] := CmdObj.GetLoadedName;
  FreeCommand(lbCommands.ItemIndex);
  RefreshCmdObj;
  UpdateSaveStatus;
end;

procedure TfrmMain.UpdateDetachedProcesses( const DisplayStr : string; aProcess : TAsyncProcess );
begin

  if assigned( aProcess ) then
    lbDetachedProcesses.Items.AddObject( DisplayStr, aProcess);

  TimerDetachedProcesses.Enabled := lbDetachedProcesses.Items.Count > 0;

  lblDetachedProcesses.Caption := format( ccapDetachedProcesses,
                                         [ lbDetachedProcesses.Items.Count,
                                           Strif( ( lbDetachedProcesses.Items.Count > 1 )
                                                    or ( lbDetachedProcesses.Items.Count = 0 ),
                                                  cmsgProcessPlural,
                                                  cmsgProcessSingular
                                                 )
                                         ] );
  tsDetachedProcesses.Caption := ccapTabProcesses + lblDetachedProcesses.Caption + '  <==';
end;


function TfrmMain.CanRunCmdLine( const RunStr : string; const ThreatIdx : integer; const IsRoot : boolean;
                                 DisAllow : boolean = false; ShowNote : string = '' ) : boolean;

  function ThreatNotAllowed( TI : integer ) : boolean;
  var
    TL : TThreatLevel;
    DoAsk : Boolean;
    TLStr : String;
  begin
    result := false;
    TL := CmdObjHelper.ListIdxToThreatLevel( TI );
    DoAsk := false;
    case TL of
      tlHarmless : DoAsk := fWarnHarmless;
      tlCareful :  DoAsk := fWarnCareful;
      tlCaution :  DoAsk := fWarnCaution;
      tlDanger :   DoAsk := fWarnDanger;
      else DoAsk := fWarnUnspecified;
    end;

    if DoAsk then
    begin
      TLStr := uppercase( CmdObjHelper.GetThreatLevelText( TL ) );
      if MsgDlgMessage( format( ccapThreatLevelWarning, [ TLStr ] ),
                        format( cmsgThreatLevelWarning, [ TLStr, RunStr, TLStr ] ) ) then
        if MsgDlgConfirmation( self ) = mrNo then
          result := true;
    end;

  end;

begin
  result := false;

  if DisAllow then
  begin
    MsgDlgMessage( ccapCommandLineDisallowed, cmsgCommandLineDisallowed );
    MsgDlgInfo( self );
    exit;
  end;

//juuus here is the place to stop run if TERM or SUPERUSER
  if IsRoot then
  begin
    MsgDlgMessage( ccapRootDisallowed, cmsgRootDisallowed );
    MsgDlgInfo( self );
    exit;
  end;

  if ThreatNotAllowed( ThreatIdx ) then
    exit;

  if ShowNote <> '' then
  begin
    MsgDlgMessage( ccapRequiredReading, format( cmsgRequiredReading, [ ShowNote ] ) );
    if MsgDlgConfirmation( self ) = mrNo then
      exit;
  end;

  result := true;

end;

function TfrmMain.RunCmdLine( RunStr : string; WantsInput : boolean; const UseShell, Detach : boolean ) : string;
var
  Piped : Boolean;
  aProcess : TAsyncProcess;
  LimitInfinityCnt : Integer;
  ExtraInfo : string;

  procedure LimitInfinity( CmdString : string );
  begin
    LimitInfinityCnt := globltProcessMaxOutput;
    if ( pos( '/dev/zero', CmdString ) > 0 )
       or ( pos( '/dev/full', CmdString ) > 0 )
       or ( pos( '/dev/random', CmdString ) > 0 )
       or ( pos( '/dev/urandom', CmdString ) > 0 )
       then
    begin
      extraInfo := format( cmsgLimitInfinity, [ cLimitInfinityMaxCnt ] );
      globltProcessMaxOutput := cLimitInfinityMaxCnt;
    end;
  end;

  function Literal_Su_Sudo : boolean;
  var
    idx : integer;

    function IsSU( const CheckStr : string ) : boolean;
    begin
      result := ( CheckStr = 'su' ) or ( CheckStr = 'sudo' );
    end;

  begin
    result := false;
    idx := pos( ' ', RunStr );
    if idx = 0 then
      result := IsSU( ExtractfileName( RunStr ) )
    else result := IsSU( ExtractfileName( copy( RunStr, 1, Idx - 1 ) ) );
//regexp was simply too complicated and still let things fall thru like cat /home/su
//    RegX.Expression := '[^ ]/su[do]* +|^su[do]* +|^su[do]*$';
//    if RegX.Exec( RunStr + ' ' ) then  //the space is a hack to if last word in string, I not a regex expert!
//    begin
//      result := true;
//    end;
  end;

begin

  result := '';

  RunStr := trim( RunStr );

  if ( RunStr = '' ) then
    exit;

  ExtraInfo := '';
  Piped := false;

  if Detach and WantsInput then
  begin
    MyShowMessage( cmsgInput_Detach, self );
    WantsInput := false;
  end;

  RunStr := CmdObjHelper.InitializeAndNormalizeCommandsEntry( RunStr,  Piped, WantsInput );

  if ( RunStr = '' ) then
  exit;

  if Literal_Su_Sudo then
  begin
    result := RunStr + cmsgSudoSuProblem;
    exit;
  end;

  LimitInfinity( RunStr );

  if ( Piped or UseShell ) and ( pos( cReservedSuperUser, RunStr ) = 1 ) then
    MyShowMessage( cmsgROOTUseShell_Pipe, self );
//===>>> this  below MUST follow that above because RunStr is modified.
  if UseShell then
    RunStr := csoNonPrintingDelimiter + RunStr;

  try
    screen.Cursor := crHourglass;

    StandardOutputHeader( result, RunStr );

    IsBusy( True, ccapDetProcRunning + ': ' + RunStr, ptProcess );

    btnCancelRun.Visible := true;
    shpSRL.Visible := true;
    shpSRR.Visible := true;

    aProcess := nil;
    fIsRunningProcess := true;

    result := result
              + LineEnding
              + ExtraInfo
              + CmdObjHelper.RouteCommand( RunStr, Detach, aProcess );

    if Detach and assigned( aProcess ) then
    begin
      UpdateDetachedProcesses( RunStr, aProcess );
      result := result + LineEnding + cltDetachedProcess;
      aProcess := nil;
    end;

  finally
    IsBusy( false );
    btnCancelRun.Visible := false;
    shpSRL.Visible := false;
    shpSRR.Visible := false;

    fIsRunningProcess := false;
    globltProcessMaxOutput := LimitInfinityCnt;

  end;

end;


procedure TfrmMain.actRunExecute( Sender: TObject );
var
  RunStr : string;
begin

  if lbCmdLines.Enabled then //lbCmdLines is flag when CL is being edited
  begin
    if lbCmdLines.ItemIndex < 0 then
      exit;
    RunStr := lbCmdLines.Items[ lbCmdLines.Itemindex ];
  end
  else
  begin
    memEntry.Lines.Text := trim( memEntry.Lines.Text );
    if memEntry.Lines.Text = '' then
      exit;
    RunStr := strif( cbSuperUserLine.Checked, cReservedSuperUser + memEntry.Lines.Text, memEntry.Lines.Text );
  end;

  if not CanRunCmdLine( RunStr,
                        cbThreatLevelLine.Itemindex,
                        cbSuperUserLine.Checked,
                        cbTerminalOnlyLine.Checked,
                        strif( cbAlertLine.Checked, memNotesLine.Text  )
                        ) then
    exit;

  UpdateDisplay(
    RunCmdLine( RunStr,
                cbWantsInputLine.Checked,
                cbUseShellLine.Checked,
                cbDetachProcessLine.Checked )
               );

end;

function TfrmMain.CheckEditing: boolean;
begin
  Result := Editing;
  if Result then
  begin
    MsgDlgMessage( ccapCurrentlyEditing, cmsgCurrentlyEditing );
    MsgDlgInfo( self );
  end;
end;

procedure TfrmMain.ResetCommandsToTop;
begin
  if InvalidCommands( False ) then
    exit;
  fLastlbCommandsIdx := -1;
  lbCommands.ItemIndex := 0;
  lbCommandsClick( Self );
  pmCmdLinesPopup( Self );

end;

procedure TfrmMain.btnSortCommandsClick(Sender: TObject);
begin
  if InvalidCommands( False ) then
    exit;
  lbCommands.Sorted := True;
  lbCommands.Sorted := False;
  ResetCommandsToTop;
end;

procedure TfrmMain.cbSuperUserChange( Sender : TObject );
begin
  case TControl( Sender ).Tag of
    cCmdColSuperUserModeUniqueID :
        lblSuperUserDisp.Visible := cbSuperUser.Checked;
    cCmdColIsFavoriteUniqueID :
        lblIsFavoriteDisp.Visible := cbIsFavorite.Checked;
    cCmdColDetachProcessUniqueID :
        lblDetachProcessDisp.Visible := cbDetachProcess.Checked;
    cCmdColTerminalOnlyUniqueID :
      lblTerminalOnlyDisp.Visible := cbTerminalOnly.Checked;
  end;
  ReFocus_Edit_Memo( true );
end;

procedure TfrmMain.ReFocus_Edit_Memo( const IsCmd : boolean );
begin
  if IsCmd then
  begin
    if memNotes.Canfocus then
      memNotes.SetFocus;
  end else
  begin
    if memEntry.Canfocus then
      memEntry.SetFocus;
  end;
  //if assigned( fCLFocus ) and ( fCLFocus.Canfocus ) then
  //  fCLFocus.SetFocus;
end;


procedure TfrmMain.cbSuperUserLineChange(Sender : TObject);
begin
  case TControl( Sender ).Tag of
    cCmdColSuperUserModeUniqueID :
      lblSuperUserLineDisp.Visible := cbSuperUserLine.Checked;
    cCmdColIsFavoriteUniqueID :
      lblIsFavoriteLineDisp.Visible := cbIsFavoriteLine.Checked;
    cCmdColDetachProcessUniqueID :
      lblDetachProcessLineDisp.Visible := cbDetachProcessLine.Checked;
    cCmdColWantsInputUniqueID :
      lblWantsInputLineDisp.Visible := cbWantsInputLine.Checked;
    cCmdColUseShellUniqueID :
      lblUseShellLineDisp.Visible := cbUseShellLine.Checked;
    cCmdColTerminalOnlyUniqueID :
      lblTerminalOnlyLineDisp.Visible := cbTerminalOnlyLine.Checked;
    cCmdColAlertUniqueID :
      lblAlertLineDisp.Visible := cbAlertLine.Checked;
  end;
  ReFocus_Edit_Memo( false );
end;

procedure TfrmMain.ThreatLevelInfo( const ACmd : string; aCB : TComboBox );
var
  Msg: string;
begin
  Msg := ACmd +
    LineEnding + LineEnding + format( '%s: %s', [ cNameItem_ThreatLevel, aCB.Text ] ) +
    LineEnding + LineEnding;

    case CmdObjHelper.ListIdxToThreatLevel( aCB.ItemIndex ) of
      tlHarmless:
        begin
          Msg := Msg + cMsgHarmless;
        end;
      tlCareful:
        begin
          Msg := Msg + cMsgCareful;
        end;
      tlCaution:
        begin
          Msg := Msg + cMsgCaution;
        end;
      tlDanger:
        begin
          Msg := Msg + cMsgDanger;
        end;
      else
        raise EErrorDevelopment.Create( 'TfrmMain.ThreatLevelInfo: unspecified Threat Level.' );
    end;

  MsgDlgMessage( cNameItem_ThreatLevel, Msg );
  MsgDlgInfo( self );

end;


procedure TfrmMain.btnThreatLevelInfoClick(Sender: TObject);
begin
  ThreatLevelInfo( format('%s "%s":', [ cNameItem_Command, lblCommandName.Caption ] ), cbThreatLevel );
  ReFocus_Edit_Memo( true );
end;

procedure TfrmMain.btnThreatLevelInfoLineClick( Sender : TObject );
begin
  ThreatLevelInfo( format( '%s "%s":', [ cNameItem_CommandLine, memEntry.Lines.Text ] ), cbThreatLevelLine );
  ReFocus_Edit_Memo( false );
end;


procedure TfrmMain.ApplyThreatLevel( aPanel : TPanel; aCB : TComboBox );
begin

  case CmdObjHelper.ListIdxToThreatLevel( aCB.ItemIndex ) of
    tlHarmless:
      begin
        aPanel.Color := cHarmlessColor;
      end;
    tlCareful:
      begin
        aPanel.Color := cCarefulColor;
      end;
    tlCaution:
      begin
        aPanel.Color := cCautionColor;
      end;
    tlDanger:
      begin
        aPanel.Color := cDangerColor;
      end;
    else
      begin
        aPanel.Color := cUnknownColor;
        aCB.Text := cmsgNotSpecified;
      end;
  end;

end;


procedure TfrmMain.cbThreatLevelChange(Sender: TObject);
begin
  ApplyThreatLevel( pnlCEdit, cbThreatLevel );
  EchoThreatLevelDisplay( pnlC, cbThreatLevelDisp, lblThreatLevelDisp, cbThreatLevel.ItemIndex );
  ReFocus_Edit_Memo( true );
end;

procedure TfrmMain.cbThreatLevelLineChange( Sender : TObject );
begin
  ApplyThreatLevel( pnlCLEdit, cbThreatLevelLine );
  EchoThreatLevelDisplay( pnlCL, cbThreatLevelLineDisp, lblThreatLevelLineDisp, cbThreatLevelLine.ItemIndex );
  ReFocus_Edit_Memo( false );
end;

function TfrmMain.GetOpenInstances : boolean;
var
  Str: string;
  ProgName: string;
  idx: SizeInt;
begin

  try

    result := true;

    if not SystemFileFound( 'ps', True ) then
      exit;

    ProgName := extractfilename(application.exename);
    Str := QuickProc( 'ps', '-A' );
    //Str := QuickProc( 'ps', '-aux' );
    fOpenInstances := 1;
    idx := pos(ProgName, Str);
    Str := Copy(Str, idx + Length(ProgName), length(Str));
    while True do
    begin
      idx := pos(ProgName, Str);
      if idx = 0 then
        break;
      Inc( fOpenInstances );
      Str := Copy(Str, idx + Length(ProgName), length(Str));
    end;
    if fOpenInstances > 1 then
    begin
  {$IFDEF Release}
      if not fAllowMultipleOpens then
      begin
        MsgDlgMessage( ccapMultipleDisallowed, cmsgMultipleDisallowed );
        MsgDlgInfo( self );
        result := false;//true;
        exit;
      end;
  {$ENDIF}
      OpenInstancesCap := format( ccapMulipleInstances, [ fOpenInstances ] );
  {$IFDEF Release}
      if MsgDlgMessage( ccapMulipleInstances, format( cmsgMulipleInstances, [ fOpenInstances ] ) ) then
        if MsgDlgConfirmation( self ) = mrNo then
          result := false;
  {$ENDIF}
      Color := $006E4BE4; //red-y
    end;
  except
    on e:exception do
    begin
      result := false;
      ShowMessage( 'TfrmMain.GetOpenInstances: ' + cmsgSystemProcessesError + e.message );
    end;
  end;
end;


procedure TfrmMain.FormActivate(Sender: TObject);
var
  OpeningState : string;

  function GetSpecialMode : boolean;
  begin
    result := true;

    if fSuperUser then
    begin
      RootModeCap := cRootMode;
      Color := $003C71AE;//#AE713C

      if MsgDlgMessage(ccapRootModeConfirmation, cmsgRootModeConfirmation ) then
        if MsgDlgAttentionConfirm( self ) = mrNo then
          result := false;

//only used if I give a button to turn on root mode, not happening, no reason program should be run as superuser.
      ////Showmessage( 'Waiting for ROOT MODE to close...' );
    end
    else
      result := GetOpenInstances;
  end;

begin

  if FIsInitialized then
    Exit;

  FIsInitialized := True;

  Application.HintHidePause := 600000;

  pnlCmdLine.top := shpCmdOut9.top;//lbCmdLines.top;
  pnlCmdLine.Left := pnlDispCmdLine.Left;//lbCmdLines.Left;
  pnlCommand.top := pnlDispCommand.top;
  pnlCommand.Left := pnlDispCommand.Left;

  if not GetSpecialMode then
  begin
    Application.Terminate;
    exit;
  end;

  if fFirstLocalRun then
    UpdateDisplay( cmsgFirstLocalRun
                   + LineEnding + LineEnding
                   + cmsgOwnRisk,
                   false );

  OpeningState := 'Opening State:'
    + LineEnding
    + lblCurrDB.Caption
    + LineEnding
    + cmsgMainBasePath + fWritingToPath
    + LineEnding
    + cmsgMainSavingPath + fSavingToPath
    + LineEnding
    + cmsgMainRootTemplate + fRootFile
    + LineEnding + LineEnding
    ;

  UpdateDisplay( OpeningState, false );

  if CommsFromFormCreate <> '' then
    UpdateDisplay( trim( CommsFromFormCreate) + LineEnding, false );

  memo1.SelStart := 1;

  globltHasBASH := SystemFileFound( 'bash' );

  RefreshCap;

  if lbCommands.CanFocus then
    lbCommands.SetFocus;

end;

function TfrmMain.GetWidgetString : string;
begin
  result := '';
  {$IFDEF WidgQT}
    result := '  [QT]';
  {$ENDIF}
  {$IFDEF WidgGTK}
    result := '  [GTK]';
  {$ENDIF}
  if result = '' then
    result := '  [DEV_ERR]';
end;


procedure TfrmMain.RefreshCap;
begin
  if OpenInstancesCap <> '' then
    OpenInstancesCap := format( ccapMulipleInstances, [ fOpenInstances ] );
  Self.Caption :=  trim( RootModeCap + ' ' + ccapProgram + ' ' + OpenInstancesCap );
end;

function TfrmMain.CommandAlreadyUsed( const CheckStr : string; const OkIdx : integer ) : boolean;
var
  i : Integer;
begin
  result := false;
//search for duplicate command name, list of commands must be unique
  for i := 0 to lbCommands.Items.Count - 1 do
  begin
    if i = OkIdx then
      continue;
    if CheckStr = CmdObjHelper.CleanCommandsEntry( lbCommands.Items[i] ) then
    begin
      result := true;
      break;
    end;
  end;
end;

function TfrmMain.CommandNameIsAllowed( const CheckStr : string; const Idx : integer ) : boolean;
var
  UpperStr : String;

  function ReservedWordAtPosOne(ToChk: string): boolean;
  begin
    Result := False;
    if pos( uppercase( trim( ToChk ) ), upperStr ) = 1 then
      Result := IsReservedWord(ToChk, ToChk, cmsgProgramKeyWord);
  end;

begin

  result := true;
  UpperStr := uppercase( CheckStr );

  if IsReservedWord( CheckStr, '', cmsgEmptyString )
    or IsReservedPhrase( CheckStr, ciniSubSeparator, cmsgDisallowedPhrase )
    or ReservedWordAtPosOne( cReservedDel )
    or ReservedWordAtPosOne( cReservedUpdate )
    or ReservedWordAtPosOne( cReservedAdd )
    or ReservedWordAtPosOne( cReservedBlankCommand ) then
    begin
      result := false;
      exit;
    end;

  if CommandAlreadyUsed( ExtractFileName( CheckStr ), Idx ) then
  begin
    MyShowMessage( format( cmsgCommandNameDuplicate, [ CheckStr ] ), self );
    result := false;
    exit;
  end;

end;


function TfrmMain.GetNewCommand( const PreString : string; var ResultStr : string;
                                 InputMode : TSingleInputMode; const Idx : integer ) : boolean;
var
  Str : string;
begin

  Result := False;
  Str := PreString;

  while True do
  begin

    if not DoSingleInput( ccapAddEditCommand, Str, InputMode, self, false ) then
      exit;

    if not CommandNameIsAllowed( Str, Idx ) then
      Continue
    else break;

  end;

  ResultStr := Str;
  Result := True;

end;

procedure TfrmMain.FindInMemo( aMemo : TMemo );
var
  PosPt : TPoint;
  MoveLeft , MoveUp: Integer;
begin

  with frmFindText do
  begin
    Memo := aMemo;
    MoveLeft := 0;
    MoveUp := 0;
    if Memo = Memo1 then
      MoveUp := Height * -1
    else MoveLeft := Width * - 1;
    PosPt := GetPreciseControlCoords( Memo, 5 + MoveLeft, 5 + MoveUp );
    Top := PosPt.y;
    Left := PosPt.x;
    Show;
  end;

end;


end.


{
        ↑
since I ALWAYS forget where the vk_ definiations are:
, lcltype //THis is needed for key up keyboard constants

tags / hints / anchors / events / enabled&visible / modalresults / popup menus

strconst_en
FrameHint1.cbHints.Caption := ccbHintsEnglishOverride;

Cleanup checks and other notes
end ;      autoformat problem
) ;        autoformat problem
menu lines
⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯

use const params
check constants / resstr to see if they are used (mostly)
MsgDlgMessage
edits autosize Off
Form resize
lit strings
Shortcuts
exceptions testing

compiler warnings
po files

check T O D O 'S
everybody have a hint???
Finish DB, clean up favorites / add some premade searches for DB
test multiple instances
reset show no mores
test option and set to don't want either DB sql/text
Didn't you know a way to send email from a button setting subject??
fresh install with and without data




 }
