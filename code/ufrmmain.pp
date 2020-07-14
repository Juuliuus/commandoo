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
  , ufrmAbout
  , JIniFiles
  , unitSearch
  , unitDBConstants
  , AsyncProcess
  ;

const
  cUseItemIndexFlag = -2;
//CLIndex_Display
  cUseCmdObjIndexFlag = 1000;

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
    btnCmdLineReCenter : TButton;
    btnEditCmdName : TBitBtn;
    btnBuilder : TBitBtn;
    btnExit : TBitBtn;
    btnCmdLineDelete : TBitBtn;
    btnCmdLineUnDelete : TBitBtn;
    btnMainClear : TBitBtn;
    btnMainInfo : TBitBtn;
    btnMainSave : TBitBtn;
    btnMainCopy : TBitBtn;
    btnPkexecMain : TBitBtn;
    btnInsertPath : TBitBtn;
    btnRefreshFavorites : TBitBtn;
    btnSearchRun : TBitBtn;
    btnSimpleSearch : TBitBtn;
    btnProfileManagement : TBitBtn;
    btnFindCmdLine : TBitBtn;
    btnAbout : TBitBtn;
    btnKeyWords : TBitBtn;
    btnOptions : TBitBtn;
    btnReCenter : TBitBtn;
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
    lblPA : TLabel;
    lblPathAliasDisp : TLabel;
    lblPADisp : TLabel;
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
    memDetachedProcesses : TMemo;
    memDispNotes : TMemo;
    memEntry : TMemo;
    Memo1 : TMemo;
    MenuItem1 : TMenuItem;
    MenuItem10 : TMenuItem;
    MenuItem11 : TMenuItem;
    MenuItem12 : TMenuItem;
    MenuItem13 : TMenuItem;
    MenuItem14 : TMenuItem;
    MenuItem15 : TMenuItem;
    MenuItem16 : TMenuItem;
    MenuItem17 : TMenuItem;
    MenuItem18 : TMenuItem;
    MenuItem19 : TMenuItem;
    MenuItem20 : TMenuItem;
    MenuItem22 : TMenuItem;
    MenuItem29 : TMenuItem;
    mniKeyWordsRoot : TMenuItem;
    mniKeyWordsAdd : TMenuItem;
    mniKeyWordsDelete : TMenuItem;
    popCmdLineReCenter : TMenuItem;
    MenuItem26 : TMenuItem;
    mniOutputCommandooInfo : TMenuItem;
    mniCmdReCenter : TMenuItem;
    mniCmdSortcommands : TMenuItem;
    mniMainFindOutput : TMenuItem;
    MenuItem3 : TMenuItem;
    mniMainRoot : TMenuItem;
    MenuItem23 : TMenuItem;
    pmiMainRoot : TMenuItem;
    MenuItem25 : TMenuItem;
    mniSearchCmdRoot : TMenuItem;
    MenuItem27 : TMenuItem;
    MenuItem28 : TMenuItem;
    mniSearchCmdLineRoot : TMenuItem;
    MenuItem30 : TMenuItem;
    MenuItem31 : TMenuItem;
    mniTabKeyWordsSearchRoot : TMenuItem;
    MenuItem33 : TMenuItem;
    MenuItem34 : TMenuItem;
    mniTabSearchSearchRoot : TMenuItem;
    MenuItem36 : TMenuItem;
    mniCommansRoot : TMenuItem;
    MenuItem38 : TMenuItem;
    mniCmdLinesRoot : TMenuItem;
    mniDblCRoot : TMenuItem;
    MenuItem21 : TMenuItem;
    mniDblCSearchKey : TMenuItem;
    mniDblCCmd : TMenuItem;
    mniDblCCmdKey : TMenuItem;
    mniDblCCmdLineFriendly : TMenuItem;
    mniDblCCmdLineNotes : TMenuItem;
    mniDblCCmdLines : TMenuItem;
    mniDblCCmdName : TMenuItem;
    mniDblCCmdNotes : TMenuItem;
    mniDblCCmdPath : TMenuItem;
    mniDblCSearchCmdLine : TMenuItem;
    mniDblCSearchCmd : TMenuItem;
    mniDblCDetPProcs : TMenuItem;
    mniDblCDetPInfo : TMenuItem;
    mniDblCDetP : TMenuItem;
    mniDblCSearch : TMenuItem;
    mniDblCSearchEntry : TMenuItem;
    mniDblCSearchFriendly : TMenuItem;
    mniDblCSearchNotes : TMenuItem;
    mniDblCDisplay : TMenuItem;
    mniMainMainPopup : TMenuItem;
    MenuItem2 : TMenuItem;
    MenuItem5 : TMenuItem;
    MenuItem6 : TMenuItem;
    MenuItem7 : TMenuItem;
    MenuItem8 : TMenuItem;
    mniMainHint : TMenuItem;
    mniMainRun : TMenuItem;
    mniMainTabsProcs : TMenuItem;
    mniMaintabsSearch : TMenuItem;
    mniMainTabsKeyWords : TMenuItem;
    mniMainTabsFav : TMenuItem;
    mniMainTabsCommands : TMenuItem;
    mniMainTabs : TMenuItem;
    mniMainProcs : TMenuItem;
    mniMainCmdLineSearch : TMenuItem;
    mniMainCmdLineKey : TMenuItem;
    mniMainCmdLineFav : TMenuItem;
    mniMainCmdListSearch : TMenuItem;
    mniMainCmdListKey : TMenuItem;
    mniMainCmdListFav : TMenuItem;
    mniMainCmdLineList : TMenuItem;
    mniMainCmdList : TMenuItem;
    mniMainCmdLineNotes : TMenuItem;
    mniMainCmdLineEntry : TMenuItem;
    mniMainCmdNotes : TMenuItem;
    mniMainCmdNameEdit : TMenuItem;
    MenuItem4 : TMenuItem;
    MenuItem9 : TMenuItem;
    mniMainCommandLines : TMenuItem;
    mniMainFindCmdLineNotes : TMenuItem;
    mniMainFindCmdNotes : TMenuItem;
    mniMainDisplay : TMenuItem;
    mniMainCommands : TMenuItem;
    mniOutPutWrap : TMenuItem;
    mniOutPutClear : TMenuItem;
    mniOutPutSave : TMenuItem;
    mniOutputCopy : TMenuItem;
    mniOutput : TMenuItem;
    mniCmdSendTo : TMenuItem;
    pnlCEdit : TPanel;
    pnlS : TPanel;
    mniCmdPasteCmdLine : TMenuItem;
    mniCmdPaste : TMenuItem;
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
    mniTabKeyWordsSearchNew : TMenuItem;
    mniTabKeyWordsSearch : TMenuItem;
    mniCmdCount : TMenuItem;
    mniCopyCLListClip : TMenuItem;
    mniCopyCmdListClip : TMenuItem;
    mniSwitchDB : TMenuItem;
    mniProfiles : TMenuItem;
    mniTabSearchSearchSave : TMenuItem;
    pnlC : TPanel;
    pnlCL : TPanel;
    pnlSearchResults : TPanel;
    pmiMainOptions : TMenuItem;
    pmiMainSave : TMenuItem;
    pmiMainExit : TMenuItem;
    pmiMainAbout : TMenuItem;
    pmiMainKeyWords : TMenuItem;
    pmiFindCmdLine : TMenuItem;
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
    pnlCommandList: TPanel;
    pnlCmdLine: TPanel;
    pnlCmdLines: TPanel;
    pnlcommand: TPanel;
    pnlEdit: TPanel;
    pnlLineControls: TPanel;
    popDblC : TPopupMenu;
    popKeyWords : TPopupMenu;
    popSearchCmdLine : TPopupMenu;
    popSearchCmd : TPopupMenu;
    popNewCommandLine: TMenuItem;
    popRun: TMenuItem;
    popCmdLineUp: TMenuItem;
    popCmdLineDown: TMenuItem;
    popCmdLineUnDelete: TMenuItem;
    popCmdLIneDelete: TMenuItem;
    mniRevert: TMenuItem;
    mniCmdAdd: TMenuItem;
    mniCommandUnDelete: TMenuItem;
    mniCommandDelete: TMenuItem;
    popCommands: TPopupMenu;
    popCmdLines: TPopupMenu;
    popMain : TPopupMenu;
    popTabKeyWords : TPopupMenu;
    popTabSearch : TPopupMenu;
    popGoto : TPopupMenu;
    shpCmdLineOut1 : TShape;
    shpCmdOut1 : TShape;
    shpCmdOut10 : TShape;
    shpCmdOut11 : TShape;
    shpCmdOut12 : TShape;
    shpCmdOut13 : TShape;
    shpCmdOut14 : TShape;
    shpCmdOut2 : TShape;
    shpCmdOut3 : TShape;
    shpCmdOut4 : TShape;
    shpCmdOut5 : TShape;
    shpCmdOut6 : TShape;
    shpCmdOut7 : TShape;
    shpCmdOut8 : TShape;
    shpCmdOut9 : TShape;
    shpRefreshFavorites : TShape;
    shpSave : TShape;
    shpSRL : TShape;
    shpSRR : TShape;
    shpCmdIn : TShape;
    shpCmdLineIn : TShape;
    shpCmdOut : TShape;
    shpRun_Test : TShape;
    shpSearchRun : TShape;
    tmrCancelRunBlink : TTimer;
    tmrCancelRun : TTimer;
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
    procedure btnCmdLineReCenterClick( Sender : TObject );
    procedure btnHaltProcessClick( Sender : TObject );
    procedure btnHelpCommandClick(Sender: TObject);
    procedure btnInsertPathClick( Sender : TObject );
    procedure btnKeyWordAddClick(Sender: TObject);
    procedure btnKeyWordDeleteClick(Sender: TObject);
    procedure btnLocationPathClick(Sender: TObject);
    procedure btnMainInfoClick( Sender : TObject );
    procedure btnPkexecMainClick( Sender : TObject );
    procedure btnReCenterClick( Sender : TObject );
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
    procedure lbDispKeywordsDblClick( Sender : TObject );
    procedure lbKeywordsDispKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure lbKeywordsKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure lblCommandNameDblClick( Sender : TObject );
    procedure lblCommandNameDispDblClick( Sender : TObject );
    procedure lblCurrDBDblClick( Sender : TObject );
    procedure lblDispEntryDblClick( Sender : TObject );
    procedure lblPathAliasDblClick( Sender : TObject );
    procedure lbSearchCmdClick( Sender : TObject );
    procedure lbSearchCmdKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure lbSearchCmdLineClick( Sender : TObject );
    procedure lbSearchCmdLineKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure memDetachedProcessesKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure memDispNotesKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure memEntryDblClick( Sender : TObject );
    procedure memEntryKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure memNotesChange( Sender : TObject );
    procedure memNotesDispKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure memNotesKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure memNotesLineChange( Sender : TObject );
    procedure memNotesLineDispKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure memNotesLineKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure Memo1DblClick( Sender : TObject );
    procedure Memo1KeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure mniCmdCountClick( Sender : TObject );
    procedure mniCmdSendToClick( Sender : TObject );
    procedure mniCopyCLListClipClick( Sender : TObject );
    procedure mniCopyCmdListClipClick( Sender : TObject );
    procedure mniKeyWordsAddClick( Sender : TObject );
    procedure mniKeyWordsDeleteClick( Sender : TObject );
    procedure mniMainCommandsClick( Sender : TObject );
    procedure mniDblCCmdNameClick( Sender : TObject );
    procedure mniOutputCommandooInfoClick( Sender : TObject );
    procedure mniSearchCmdLineItemClipClick( Sender : TObject );
    procedure mniSearchCmdLineListClipClick( Sender : TObject );
    procedure mniSearchCmdListClipClick( Sender : TObject );
    procedure nbCommandsChange( Sender : TObject );
    procedure popCmdLineReCenterClick( Sender : TObject );
    procedure popCmdLinesPopup(Sender: TObject);
    procedure popCommandsPopup(Sender: TObject);
    procedure popCmdLinePasteClick( Sender : TObject );
    procedure mniCmdPasteClick( Sender : TObject );
    procedure popDblCPopup( Sender : TObject );
    procedure popGotoPopup( Sender : TObject );
    procedure popSearchCmdLinePopup( Sender : TObject );
    procedure TimerBlinkStartTimer( Sender : TObject );
    procedure TimerBlinkStopTimer( Sender : TObject );
    procedure TimerBlinkTimer( Sender : TObject );
    procedure TimerDetachedProcessesTimer( Sender : TObject );
    procedure tmrCancelRunBlinkTimer( Sender : TObject );
    procedure tmrCancelRunTimer( Sender : TObject );
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
    fAllowPkexec : boolean;
    fAllowPkexecInstalled : boolean;
    fAllowTextDB : boolean;
    fDoShowSqlMissing : boolean;
    fManRefreshFavorites : boolean;
    fIFS : TJiniFile;//"I"nifile "F"orm "S"ettings
    fOpenInstances : integer;
    fFirstLocalRun : boolean;
    fInternalComs : boolean;
    fUpdateDisplayOffset : Int64;
    fUpdateDisplayOffsetFlag : boolean;

//Search handling
    fIsSimpleSearch : boolean;
    fKeyWordSO : TSearchObj;
    fFavoritesSO : TSearchObj;
    fSearchSO : TSearchObj;
    fSimpleSearchSO : TSearchObj;
    fSearchFields : TStringlist;
    fSearchMayHaveChanged : boolean;
//Search Results
    fSearchSR : TStringList;
    fKeyWordSR : TStringList;
    fFavoritesSR : TStringList;

    function GetCmdRec( const anIdx : integer = -1 ) : TCmdRec;
    procedure JumpToCommand( anIdx : integer; CmdLineStr : string );
    function CanJumpToCommand( const CmdStr, CmdLineStr : string ) : boolean;
    function TryToFindEditedCommand( const CmdSearch : string ) : integer;
    procedure MoveBetweenMajorAreas( aTag : integer; const Key : Word );
    function CheckPathOverride(var ConPath : string ) : boolean;
    procedure WritePathOverride(const ConPath : string );
    function IsFileExist( const FName : string; WithNotify : boolean = false ) : boolean;
    //function CmdInPath( CheckForBuiltin : boolean = false ) : boolean;
    function CmdInPath : boolean;
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
    procedure FormToCmdObj( aCmdObj: TCmdObj );
    procedure ClearCmdLineDisps;
    procedure BlankCmdLine;
    procedure CmdLineToForm( aCmdLineObj: TCmdLineObj );
    procedure FormToCmdLine( aCmdLineObj: TCmdLineObj );
    function CheckUpdates_PROG : boolean;
    function CheckUpdates_DB : boolean;

    procedure FreeCmdDisplayObjects( Strings : TStrings );
    procedure FreeCommand( const ItemIdx : integer; const DoBlank, DoFullClear : boolean );
    procedure GetClipBoardItemVersion( LB : TListbox );
    procedure GetClipBoardListVersion( LB : TListbox );
    function GetNewCommand( const PreString : string; var ResultStr : string;
                            InputMode : TSingleInputMode; const Idx : integer) : boolean;
    function GetNewCommandLine( const PreString : string; var ResultStr : string ) : boolean;
    function GetOpenInstances : boolean;
    function GetProfileStamp : string;
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
    procedure RefreshCap;
    procedure ResetCommandsToTop;
    procedure SearchVK_F_Keys( const Key : Word );
    procedure lbSearchCmdLine_ApplyActions( const IsCmd : byte );
    procedure LoadCurrentSearches;
    procedure LoadData;
    procedure ClearDBObjects( const IsNormal : boolean );
    procedure LoadDBObjects;
    procedure LoadFavorites;
    procedure LoadSearchObject;
    procedure LoadSearchResults_Internal( aSearch : TSearchObj; Strings : TStrings; const IsSimple : boolean );
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
    procedure RefreshCmdObj( CLOIdx : integer = cUseItemIndexFlag );
    procedure RunCmdDisplayObj( Sender : TListBox );
    function RunCmdLine( RunStr : string; WantsInput : boolean; const UseShell, Detach : boolean ) : string;
    procedure CloseCancelRun;
    procedure SaveCurrentSearches;
    procedure SaveSearch( SO : TSearchObj );
    procedure SelectProfileToMergeTo;
    procedure SetActivePage( Page : TTabSheet );
    procedure SetUp_Edit_Structure;
    procedure SetUp_KeyBoardMenu_Structure;
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
    procedure UnassMemosKeyDown( Sender : TMemo; var Key : Word; const Shift : TShiftState );
    function Unsaved : boolean;
    procedure UpdateDetachedProcesses( const DisplayStr : string; aProcess : TAsyncProcess );
    procedure UpdateDisplay( const Output : string; const DoIndicate, DoSetFocus : boolean );
    procedure UpdateDisplay_Internal( const Output : string; const DoIndicate : boolean );
    procedure UpdateLbCmdLines( const Idx: integer; SaveState: boolean = True );
    procedure UpdateLbCommands( const DoSave: boolean; TheIdx: integer = cUseItemIndexFlag );
    procedure UpdateMainActions;
    procedure UpdateNotebookCaptions;
    procedure UpdateNotebookEditingStatus;
    procedure UpdatePathsInEdit( const ThePath : string; const Complete : boolean );
    procedure UpdateProfileText( const IsBad : boolean );
    procedure UpdateSaveStatus;
    procedure SetNotificationState( const TurnOn : boolean; Obj : TComponent; Shp : TShape );
  public
    { public declarations }
    RegX: TRegExpr;

    function GetSearchesDirectory : string;
    function GetPODirectory: string;
    procedure UpdateSharedHintsAndCaptions;
    function RunCmdLineExternal( const RunStr : string ) : string;
    function RunExternalHelpRequest( SL : TStringList ) : string;
    procedure SavedSearches_Delete( const aGUID : string );
    function GetCurrentState : string;
    function GetWidgetString : string;
    function Editing: boolean;
    function EditingCmd: boolean;
    function EditingCL: boolean;
    function DifferingCommandPaths : boolean;
    function GetProperCmdNameCaption : string;
    function GetProperPathLabelCaption( const ForDisplay : boolean = false ) : string;
    function GetRealPath : string;

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
  ccapMainBadPath = 'Config Problem, Re-set??';
  cmsgMainBasePath = 'Base DB/Settings path: ';
  cmsgMainSavingPath = 'Saving path: ';
  cmsgMainSavingPathSearch = 'Searches %s';
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
  cmsgMainHowToEditCmdName =
    LineEnding + LineEnding
    + 'If you want to edit the Command Name and/or path, open the command in EDIT mode and use the button '
    + 'or Dbl-Click the name. '
    + LineEnding + LineEnding
    + 'If the new command is (or should be) in the path simply typing the name is sufficient, '
    + 'otherwise type in the full path or use the folder search button.'
    ;

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
  SpecialComms : string = '';


const
  cmsgMainShellPhrase = 'SHELL: ';
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

//500000 special tags
  cArrowKeyCommands     = 500000;
  cArrowKeymemNotes     = 500001;
  cArrowKeyCmdLines     = 500002;
  cArrowKeymemEntry     = 500003;
  cArrowKeymemNotesLine = 500004;
  cArrowKeyMemo1        = 500005;
  cArrowKeySearchCmd             = 500006;
  cArrowKeySearchCmdLine         = 500007;
  cArrowKeyDetachedProcesses     = 500008;
  cArrowKeymemDetachedProcesses  = 500009;
  cArrowKeylbKeywords            = 500010;


  cCurrKWFileName = '.CurrKWSearch';
  cCurrSearchFileName = '.CurrSearch';
  cDisplayOutPutMax = 25000;
  clblCmdPointer = 'Command';
  clblCmdlinePointer = 'Command Lines';
  clblPointers = '  ↓';
  ccapGotoCmdAbbrev = 'Cmd';
  ccapGotoCmdlineAbbrev = 'CmdLine';

  cmniMainCommands = 120;
  cmniMainDisplay = 121;
  cmniMainCommandLines = 122;
  cmniMainFindCmdLineNotes = 123;
  cmniMainFindCmdNotes = 124;
  cmniMainFindOutput = 125;
  //cXXX = 126;
  cmniMainCmdNameEdit = 127;
  cmniMainCmdNotes = 128;
  cmniMainCmdLineEntry = 129;
  cmniMainCmdLineNotes = 130;
  cmniMainRun = 131;
  //cmniMainCmdList : TMenuItem; HUB
  //cmniMainCmdLineList : TMenuItem; HUB
  cmniMainCmdListFav = 221;
  cmniMainCmdLineFav = 222;

  cmniMainCmdListKey = 321;
  cmniMainCmdLineKey = 322;

  cmniMainCmdListSearch = 421;
  cmniMainCmdLineSearch = 422;

  cmniMainProcs = 521;
//Tabs separated by beginning digit
//cmniMainTabs : TMenuItem; HUB
  cmniMainTabsCommands = 180;
  cmniMainTabsFav = 280;
  cmniMainTabsKeyWords = 380;
  cmniMaintabsSearch = 480;
  cmniMainTabsProcs = 580;
//tab independent
  cmniMainHint = 1001;


  cmniDblCCmd = 1;
  cmniDblCCmdName = 2;
  cmniDblCCmdPath = 3;
  cmniDblCCmdNotes = 4;
  cmniDblCCmdKey = 5;
  cmniDblCCmdLines = 6;
  //               = 7; Free to use
  cmniDblCCmdLineFriendly = 8;
  cmniDblCCmdLineNotes = 9;
  cmniDblCSearch = 10;
  cmniDblCSearchCmd = 11;
  cmniDblCSearchCmdLine = 12;
  cmniDblCSearchEntry = 13;
  cmniDblCSearchFriendly = 14;
  cmniDblCSearchNotes = 15;
  cmniDblCSearchKey = 16;
  cmniDblCDetP = 17;
  cmniDblCDetPProcs = 18;
  cmniDblCDetPInfo = 19;
  cmniDblCDisplay = 20;


resourcestring
  ccapcleUncertainPath = 'Uncertain Path...';
  cmsgcleUncertainPath =
    'You are currently editing a Command and the path of the edited command is not '
    + LineEnding
    + 'the same as the path in the saved version. Which would you like to use:'
    + LineEnding + LineEnding
    + 'Yes = %s'
    + LineEnding
    + 'No  = %s'
    + LineEnding
    ;
  ccapGotoFindIn = 'Find in %s %s';
  ccapGotoNotesStr = 'Notes';
  ccapGotoListStr = 'List';
  ccapDblCProcessStr = 'Process';
  ccapGotoEditIn = 'Edit %s %s';
  ccapGotoEditInName = 'Name';
  ccapDblCFriendlyName = 'Friendly';
  ccapGotoEditInEntry = 'Entry';
  ccapGotoEditInNotes = 'Notes';
  ccapGotoProcs = 'Detached Processes';
  ccapGotoFindConst = 'Find';
  ccapGotoReFindConst = 'Find again';
  ccapGotoMainDisplay = 'Output area';

  cmsgProgramKeyWord = 'The Name "%s" at beginning of name is reserved for the program to use.';
  cmsgCommandNameDuplicate =
    'The Command "%s" is already in the command list, duplicates are not allowed.';
  ccapAddEditCommand = 'Add / Edit Command';
  ccapSearchFileName = 'Search File Name';
  ccapAddCommandLine = 'Add Command Line';
  ccapRunningDetachedProcesses = 'Running Processes!';
  cmsgRunningDetachedProcesses =
     'You have %d Running child processes (listed below). They '
     + 'will be automatically summarily halted. If they are in an '
     + 'unsaved state you will lose the changes and/or the data.'
     + LineEnding + LineEnding
     + 'Do you still want to close?'
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
  cmsgSqlLibNotFound = '===> sqlite 3 system library not found <==='
     + LineEnding + LineEnding
     + 'commandoo could not find the location of this library, so only text based DB''s '
     + 'can be used until this is fixed. '
     + LineEnding + LineEnding
     + 'You can set/search the location in OPTIONS. '
     + 'But, first make sure the libsqlite3 package is installed on your system. Usually it is by default '
     + 'but maybe it was removed, moved, or customized. If you are not sure '
     + 'use a package manager and look to see if a package called "libsqlite3-0", or something similar, is installed. '
     + LineEnding + LineEnding
     + 'If/when it is installed, then, in OPTIONS,  try re-setting to "default". '
     + 'If commandoo still can''t find it then determine the library''s location and point to it '
     + 'manually. Typing the following in a commandoo CL or a terminal window should help you find it: '
     + LineEnding + LineEnding
     + '"locate -i -e libsqlite3.so.0" or "locate -i -e libsqlite3.so"'
     + LineEnding + LineEnding
     + 'examine the output for the proper location (usually starts with "/usr/" and has "x86_64" or "x64" '
     + 'or similar in the path name) '
     + 'and use that location. In most cases the library file is named "libsqlite3.so.0".'
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
  cmsgROOTUseShell_Pipe = 'Not allowed to run UseShell or piped CL''s with elevated privileges '
    + '(<ROOT> or pkexec), this will run as normal user, run CL in Terminal if you want to use <ROOT>.';
  cmsgInput_Detach = 'Not allowed to send input to a detached (child) process. Input turned off.';
  cmsgSystemProcessesError = 'Problem reading system processes. Halting: ';

  //cNameItem_Problem = 'Problem';
  cNameItem_Search = 'Search';
  cNameItem_SearchSimple = 'Simple';
  cNameItem_SearchNormal = 'Normal';

  cNameItem_ThreatLevel = 'Threat Level';


{ TfrmMain }

function TfrmMain.GetCmdRec( const anIdx : integer = -1 ) : TCmdRec;
begin
  if anIdx = -1 then
    Result := TCmdRec( lbCommands.Items.Objects[ lbCommands.ItemIndex ] )
  else Result := TCmdRec( lbCommands.Items.Objects[ anIdx ] );
end;

procedure TfrmMain.ClearDBObjects( const IsNormal : boolean );
var
  i : Integer;
  CmdRec : TCmdRec;
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
    CmdRec := TCmdRec( lbCommands.Items.Objects[ i ] );
    if assigned( CmdRec.Cmd ) then
    begin
      CmdRec.Cmd.Free;
      CmdRec.Cmd := nil;
    end;
    CmdRec := nil;
    TCmdRec( lbCommands.Items.Objects[ i ] ).Free;
    lbCommands.Items.Objects[ i ] := nil;
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
  FreeAndNil( fSimpleSearchSO );
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
    //if MsgDlgMessage( ccapUnsavedData, cmsgUnsavedData, 'cmsgUnsavedData') then
    //  if MsgDlgAttentionConfirm( self ) = mrNo then
    //    exit;
    MsgDlgMessage( ccapUnsavedData, cmsgUnsavedData);
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
//before a releasing a version

//       INCREMENT c_PROG_VersionUpgradeCount = #; to the next upgrade
//       if prog upgrades (settings-wise) were necessary
//
//       cHandwrittenVersion = '#.#.#'; to match readme file
////       as of Juneish 2020 v. is 2.0.0 //as of March 2018 v. is 1.0.1

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

  procedure GetShellName;
  begin
    //globltShellName := SystemFileLocation( 'bash' );
    //globltShellName := SystemFileLocation( 'sh' );
    //exit; //testing
//since I'm confused as to why there is a /bin/bash and a /usr/bin/bash
//I try to be complete here but prefer /bin/ variants.
    globltShellName := trim( GetEnvironmentVariable( 'SHELL' ) );
    if globltShellName = '' then
    begin
      if fileexists( '/bin/bash' ) then
        globltShellName := '/bin/bash'
      else globltShellName := SystemFileLocation( 'bash' );
      if globltShellName = '' then
      begin
        if fileexists( '/bin/sh' ) then
          globltShellName := '/bin/sh'
        else globltShellName := SystemFileLocation( 'sh' );
      end;
    end;
  end;

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
      SpecialComms := SpecialComms + SqlLibChange;
    if ( fSqliteLibrary = cSqliteDefaultLibraryLocationUNKNOWN ) and fDoShowSqlMissing then
      SpecialComms := SpecialComms + cmsgSqlLibNotFound;
  end;

begin

  font.size := cDefaultFontSize;
  NeedsWrite := false;
  fInternalComs := false;
  fIsSimpleSearch := false;
  fUpdateDisplayOffset := 0;
  fUpdateDisplayOffsetFlag := false;
  lblPathAlias.Caption := '';
  lblPathAliasDisp.Caption := '';
  lblCmdLinePointer.caption := clblCmdlinePointer + clblPointers;
  lblCmdPointer.caption := clblCmdPointer + clblPointers;

  Randomize;
  application.OnException := @FinalException;
//this was autocreated, but updating the translation not possible then
  frmfindtext := Tfrmfindtext.Create( self );

  fFirstLocalRun := false;
  fSuperUser := False;
  fDisplayOutPut := TStringlist.Create;

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

  //fAllowPkexecInstalled := false; //testing
  fAllowPkexecInstalled := SystemFileFound( trim( cprogPkexecStr ) );
  fSuperUser := QuickProc( 'id', '-u' ) = '0';

//==================================
  DevReleaseSettings;
//==================================

//juuus Make a routine that will create a thumbdrive version
//ask for appimage ask for destFolder: compy appimage and config to appimage.config

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

  GetShellName;

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

  fSimpleSearchSO := TSearchObj.Create( 'SimSearch', InfoServer );
  fSimpleSearchSO.UserTag := Ord( tcsNormal );

  fFavoritesSO := TSearchObj.Create( 'FavS', InfoServer );
  fFavoritesSO.Searches[ cIdxCmd ].AddSearchItem( fidIsFavorite, dbsTrueStr, coEqual, false );
  fFavoritesSO.Searches[ cIdxCmdLine ].AddSearchItem( fidIsFavorite, dbsTrueStr, coEqual, false );

  fSearchSR := TStringList.Create;
  fKeyWordSR := TStringList.Create;
  fFavoritesSR := TStringList.Create;

  Get_Cmd_Fields_Searchable( fSearchFields );

  SetUp_Edit_Structure;
  SetUp_KeyBoardMenu_Structure;

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

procedure TfrmMain.MoveBetweenMajorAreas( aTag : integer; const Key : Word );
begin

//someday when refactoring probably could be a cool structure that would make this easier.
  case aTag of
    cArrowKeyCommands :
      case Key of
        VK_RIGHT :
          if EditingCmd then
            TryFocus( memNotes )
          else if EditingCL then
            TryFocus( memEntry )
          else TryFocus( lbCmdLines );
        VK_LEFT : TryFocus( Memo1 );
      end;
    cArrowKeymemNotes :
      case Key of
        VK_RIGHT : TryFocus( lbKeywords );
          //if EditingCL then
          //  TryFocus( memEntry )
          //else TryFocus( lbCmdLines );
        VK_LEFT :
          if lbCommands.Canfocus then
            TryFocus( lbCommands )
          else TryFocus( Memo1 );
      end;
    cArrowKeylbKeywords :
      case Key of
        VK_RIGHT :
          if EditingCL then
            TryFocus( memEntry )
          else TryFocus( lbCmdLines );
        VK_LEFT : TryFocus( memNotes );
      end;
    cArrowKeyCmdLines :
      case Key of
        VK_RIGHT : TryFocus( Memo1 );
        VK_LEFT :
          if EditingCmd then
            //TryFocus( memNotes )
            TryFocus( lbKeyWords )
          else if lbCommands.CanFocus then
            TryFocus( lbCommands )
          else TryFocus( Memo1 );
      end;
    cArrowKeymemEntry :
      case Key of
        VK_RIGHT : TryFocus( MemNotesLine );
        VK_LEFT :
          if EditingCmd then
            TryFocus( lbKeyWords )
          else if lbCommands.Canfocus then
            TryFocus( lbCommands )
          else TryFocus( Memo1 );
      end;
    cArrowKeymemNotesLine :
      case Key of
        VK_RIGHT : TryFocus( Memo1 );
        VK_LEFT :  TryFocus( memEntry );
      end;
    cArrowKeyMemo1 :
      begin
        if nbCommands.ActivePage = tsCommands then
          case Key of
            VK_RIGHT :
              if lbCommands.Canfocus then
                TryFocus( lbCommands )
              else if EditingCmd then
                TryFocus( memNotes )
              else if EditingCL then
                TryFocus( memEntry )
              else TryFocus( lbCmdLines );
            VK_LEFT :
              if EditingCL then
                TryFocus( memNotesLine )
              else TryFocus( lbCmdLines );
          end
        else if nbCommands.ActivePage = tsDetachedProcesses then
          case Key of
            VK_RIGHT : TryFocus( lbDetachedProcesses );
            VK_LEFT :  TryFocus( memDetachedProcesses );
          end
        else
          case Key of
            VK_RIGHT : TryFocus( lbSearchCmd );
            VK_LEFT :  TryFocus( lbSearchCmdLine );
          end;
      end;
    cArrowKeySearchCmd :
      case Key of
        VK_RIGHT : TryFocus( lbSearchCmdLine );
        VK_LEFT :  TryFocus( Memo1 );
      end;
    cArrowKeySearchCmdLine :
      case Key of
        VK_RIGHT : TryFocus( Memo1 );
        VK_LEFT : TryFocus( lbSearchCmd );
      end;
    cArrowKeyDetachedProcesses :
      case Key of
        VK_RIGHT : TryFocus( memDetachedProcesses );
        VK_LEFT : TryFocus( Memo1 );
      end;
    cArrowKeymemDetachedProcesses :
      case Key of
        VK_RIGHT : TryFocus( Memo1 );
        VK_LEFT : TryFocus( lbDetachedProcesses );
      end;

    else
      begin
        if nbCommands.ActivePage = tsCommands then
        begin
          if lbCommands.CanFocus then
            TryFocus( lbCommands )
          else if EditingCL then
            TryFocus( memEntry )
          else TryFocus( lbCmdLines );
        end
        else if nbCommands.ActivePage = tsDetachedProcesses then
        begin
          TryFocus( lbDetachedProcesses );
        end
        else TryFocus( lbSearchCmd );

      end;
  end;

end;

procedure TfrmMain.FormKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
var
  PosPt : TPoint;

  procedure ShowMainMenu;
  begin
    PosPt := GetPreciseControlCoords( btnPlus, 30, 30 );
    popMain.Popup( PosPt.x, PosPt.y );
  end;

  procedure InsertDate( Memo : TMemo );
  begin
    if Memo.Focused then
    begin
      Memo.SelLength := 0;
      Memo.SelText := ':' + DateTimeToStr( now ) + ': ';
    end;
  end;

begin

  if Shift = [ ssShift, ssCtrl ] then
  begin
    case key of
      VK_LEFT, VK_RIGHT :
          if assigned( self.ActiveControl ) then
          begin
            MoveBetweenMajorAreas( self.ActiveControl.Tag, Key );
            Key := VK_UNKNOWN;
          end;
//The use of ctrl-shift- VK_d g k m p  are global, do not use anywhere else.
      VK_D :
        if assigned( self.ActiveControl ) and ( self.ActiveControl is TMemo )  then
        begin
          case TMemo( self.ActiveControl ).Name of
            'Memo1' : InsertDate( Memo1 );
            'memNotes' : InsertDate( memNotes );
            'memNotesLine' : InsertDate( memNotesLine );
          end;
          Key := VK_UNKNOWN; //just in case
        end;
      VK_G :
        begin
          PosPt := GetPreciseControlCoords( btnPlus, 30, 30 );
          popGoto.Popup( PosPt.x, PosPt.y );
        end;
      VK_K :
        begin
          PosPt := GetPreciseControlCoords( btnPlus, 30, 60 );
          popDblC.PopUp( PosPt.x, PosPt.y );
        end;
      VK_M : ShowMainMenu;
      VK_P :
        if assigned( self.ActiveControl ) and assigned( self.ActiveControl.PopupMenu ) then
        begin
          PosPt := GetPreciseControlCoords( self.ActiveControl, 30, 30 );
          self.ActiveControl.PopupMenu.PopUp( PosPt.x, PosPt.y );
        end; // else ShowMainMenu;
    end;
  end;

  if Editing then
    exit;

  if key in [ VK_1, VK_2, VK_3, VK_NUMPAD1, VK_NUMPAD2, VK_NUMPAD3 ] then
  begin
    if Shift = [ ssAlt ] then
    begin
      if nbCommands.ActivePage <> tsKeyWords then
        SetActivePage( tsKeyWords );
      case Key of
        VK_1, VK_NUMPAD1 : if actSearchKeyCmd.Enabled then actSearchKeyCmd.Execute;
        VK_2, VK_NUMPAD2 : if actSearchNewKeyCmd.Enabled then actSearchNewKeyCmd.Execute;
        VK_3, VK_NUMPAD3 : if actSearchLoadKeyCmd.Enabled then actSearchLoadKeyCmd.Execute;
      end;
    end
    else if Shift = [ ssCtrl ] then
    begin
      if nbCommands.ActivePage <> tsSearch then
        SetActivePage( tsSearch );
      case Key of
        VK_1, VK_NUMPAD1 : if actSearchSearch.Enabled then actSearchSearch.Execute;
        VK_2, VK_NUMPAD2 : if actSearchNewSearch.Enabled then actSearchNewSearch.Execute;
        VK_3, VK_NUMPAD3 : if actSearchLoadSearch.Enabled then actSearchLoadSearch.Execute;
      end;
    end;
    exit;
  end;

end;

procedure TfrmMain.UpdateProfileText( const IsBad : boolean );
begin
  lblCurrDB.Caption := format( cmsgProfileString,
                               [
                                 fProfileName,
                                 strif( fUseDB,  cDefaultDBProfileIsDBStr, cDefaultDBProfileIsDBStrNot ),
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
      4 : Update_PROG_Version_0004( fIFS, cSectTabFormSettings );
      //5 : When an update is done on the Program that needs attention in ini file write the needed code here
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
  Comms : string;
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

  Comms := '';
//use literal names for the columns as they were at time, protects against changes to constant names
  for i := FromVer + 1 to ToVer do
    case i of
      1 : Update_DB_Version_0001( 'LineIniIdx' );
      2 : Update_DB_Version_0002( 'Entry' );
      3 : Update_DB_Version_0003( 'ThreatLevel' );
      4 : Update_DB_Version_0004( 'ObjID' );
      5 :
        begin
          if Update_DB_Version_0005 then
            Comms := Comms + LineEnding
                     + 'Text DB''s: Added TableType to better differentiate files on import.'
                     + LineEnding
                     + 'Text DB''s: Enforce Shared DB GUID to be accurate with DB imports.';
        end;
      6 :
        begin
          Update_DB_Version_0006;
          Comms := Comms + LineEnding
                    + 'Text and sql DB''s: Commands in $PATH now store $PATH instead of '
                    + 'literal path, allowing portability to various gun/linux distros.';
        end;
      7 :
        begin
          Update_DB_Version_0007;
          Comms := Comms + LineEnding
                    + 'Text and sql DB''s: Mistakenly added bash "builtins" support removed, '
                    + 'and the path changed to $BAD_PATH ("builtins" should be added to the '
                    + 'shell command entry like bash, zsh, csh, etc.).';
        end;

      //8 : When an update is done on the DB write the needed code here, increase the
      //  c_DB_VersionUpgradeCount const by 1
    end;

  InfoServer.UpdateDBVersionCount( ToVer );

  SpecialComms := cAttnBar
                   + format( cmsgUpdated_DBFiles, [
                                   fProfileName + ' ' + trim( strif( fUseDB, cDefaultDBProfileIsDBStr,  cDefaultDBProfileIsDBStrNot ) ),
                                   c_DB_HandwrittenVersion,
                                   FromVer,
                                   ToVer
                                                        ] )
                   + strif( Comms <> '', Comms + LineEnding )
                   + cAttnBar;

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

      if FIsInitialized and ( SpecialComms <> '' ) then
      begin
        UpdateDisplay( trim( SpecialComms ) + LineEnding, false, false );
        SpecialComms := '';
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
  i : integer;
begin

  SL := TStringList.Create;
  try

    CmdObjHelper.GetCmdList( SL );

    if SL.Count > 0 then
    begin
      SL.Sort;
      lbCommands.Items.Assign(SL);
      lbCommands.ItemIndex := 0;
      for i := 0 to lbCommands.Items.Count - 1 do
        lbCommands.Items.Objects[ i ] := TCmdRec.Create;
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

  pnlDispCommand.Hint := format( cmsgDisplayPanels, [  cconstCommandLabel, cmsgSearchTextHint ] );
  pnlDispCmdLine.Hint := format( cmsgDisplayPanels, [ cconstCommandLineLabel, cmsgSearchTextHint ] );

  btnCmdEdit.Hint := format( cmsgEditButtons, [  cconstCommandLabel, cconstCommandLabel ] );
  btnLineEdit.Hint := format( cmsgEditButtons, [  cconstCommandLineLabel, cconstCommandLineLabel ] );

  btnCmdOk.Hint := format( cmsgEditOk, [  cconstCommandLabel ] );
  btnLineOk.Hint := format( cmsgEditOk, [  cconstCommandLineLabel ] );

  btnCmdCancel.Hint := format( cmsgEditCancel, [  cconstCommandLabel ] );
  btnLineCancel.Hint := format( cmsgEditCancel, [  cconstCommandLineLabel ] );

  cbThreatLevel.Hint := format( cmsgThreatLevels, [ cmsgThreatLevelsCommand ] );
  cbThreatLevelLine.Hint := format( cmsgThreatLevels, [ cmsgThreatLevelsCommandLine ] );

  btnThreatLevelInfoLine.Hint := btnThreatLevelInfo.Hint;

  memNotes.Hint := format( cmsgNotesHints, [  cconstCommandLabel, cmsgSearchTextHint ] );
  memNotesLine.Hint := format( cmsgNotesHints, [ cconstCommandLineLabel, cmsgSearchTextHint ] );
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

  btnRun_Test.Caption := cObjlblRun;

//not static texts so need to be updated also
  UpdateProfileText( BadDB );
  UpdateDetachedProcesses( '', nil );
  RefreshCap;

  mniMainCommands.Caption := clblCmdPointer + 's';
  mniDblCCmd.Caption := mniMainCommands.Caption + '...';
  mniMainDisplay.Caption := ccapGotoMainDisplay;
  mniDblCDisplay.Caption := mniMainDisplay.Caption;

  mniMainTabsCommands.Caption := trim( ccapTabCommands );
//-----
  mniMainTabsFav.Caption := trim( ccapTabFavorites );
  mniMainCmdListFav.Caption := mniMainTabsFav.Caption;
  mniMainCmdLineFav.Caption := mniMainTabsFav.Caption;
//-----
  mniMainTabsKeyWords.Caption := trim( ccapTabKeyWords );
  mniMainCmdListKey.Caption := mniMainTabsKeyWords.Caption;
  mniMainCmdLineKey.Caption := mniMainTabsKeyWords.Caption;
//-----
  mniMaintabsSearch.Caption := trim( ccapTabSearch );
  mniMainCmdListSearch.Caption := mniMaintabsSearch.Caption;
  mniMainCmdLineSearch.Caption := mniMaintabsSearch.Caption;
//-----
  mniMainTabsProcs.Caption := ccapGotoProcs;

  mniMainFindOutput.Caption := format( ccapGotoFindIn, [ '', ccapGotoMainDisplay ] );
  mniMainCommandLines.Caption := clblCmdlinePointer;
  mniMainFindCmdNotes.Caption := format( ccapGotoFindIn, [ ccapGotoCmdAbbrev, ccapGotoNotesStr ] );
  mniMainFindCmdLineNotes.Caption := format( ccapGotoFindIn, [ ccapGotoCmdlineAbbrev, ccapGotoNotesStr ] );

  mniMainCmdList.Caption := format( cDoubleS, [ clblCmdPointer, ccapGotoListStr ] ) + Dots;
  mniMainCmdLineList.Caption := format( cDoubleS, [ ccapGotoCmdlineAbbrev, ccapGotoListStr ] ) + Dots;


  mniDblCCmdNotes.Caption := format( cDoubleS, [ ccapGotoCmdAbbrev, ccapGotoNotesStr ] );
  mniDblCCmdLineNotes.Caption := format( cDoubleS, [ ccapGotoCmdlineAbbrev, ccapGotoNotesStr ] );
  mniDblCCmdKey.Caption := trim( ccapTabKeyWords );
  mniDblCCmdLines.Caption := clblCmdlinePointer;
  mniDblCSearchCmd.Caption := format( cDoubleS, [ clblCmdPointer, ccapGotoListStr ] );
  mniDblCSearchCmdLine.Caption := format( cDoubleS, [ clblCmdlinePointer, ccapGotoListStr ] );
  mniDblCCmdLineFriendly.Caption := format( cDoubleS, [ ccapDblCFriendlyName, ccapGotoEditInName ] );
  mniDblCSearchFriendly.Caption := mniDblCCmdLineFriendly.Caption;
  mniDblCSearchNotes.Caption := ccapGotoEditInNotes;
  mniDblCSearchKey.Caption := trim( ccapTabKeyWords );
  mniDblCDetPProcs.Caption := format( cDoubleS, [ ccapDblCProcessStr, ccapGotoListStr ] );
  mniKeyWordsRoot.Caption := format( ccapPopMenuRootRoot, [ trim( ccapTabKeyWords ), ccapPopMenuRootMenuStr ] );
  mniKeyWordsAdd.Caption := format( cDoubleS, [ ccapGenericAdd, trim( ccapTabKeyWords ) ] );
  mniKeyWordsDelete.Caption := format( cDoubleS, [ ccapGenericDelete, trim( ccapTabKeyWords ) ] );

  mniDblCRoot.Caption := format( ccapPopMenuRootRoot, [ ccapDblCRootName, ccapPopMenuRootMenuStr ] );
  mniMainRoot.Caption := format( ccapPopMenuRootRoot, [ ccapMainRootName, ccapPopMenuRootMenuStr ] );
  pmiMainRoot.Caption := format( ccapPopMenuRootRoot, [ ccappmiMainRootName, ccapPopMenuRootMenuStr ] );
  mniSearchCmdRoot.Caption := format( ccapPopMenuRootRoot, [ clblCmdPointer, ccapGotoListStr ] );
  mniSearchCmdLineRoot.Caption := format( ccapPopMenuRootRoot, [ ccapGotoCmdlineAbbrev, ccapGotoListStr ] );
  mniTabKeyWordsSearchRoot.Caption := format( ccapPopMenuRootRoot, [ trim( ccapTabKeyWords ), trim( ccapTabSearch ) ] );
  mniTabSearchSearchRoot.Caption := format( ccapPopMenuRootRoot, [ trim( ccapTabSearch ), ccapPopMenuRootMenuStr ] );
  mniCommansRoot.Caption := format( ccapPopMenuRootRoot, [ clblCmdPointer, ccapPopMenuRootMenuStr ] );
  mniCmdLinesRoot.Caption := format( ccapPopMenuRootRoot, [ clblCmdlinePointer, ccapPopMenuRootMenuStr ] );

  mniMainCmdNameEdit.Caption := format( ccapGotoEditIn, [ ccapGotoCmdAbbrev, ccapGotoEditInName ] );
  mniDblCCmdName.Caption := format( cDoubleS, [ ccapGotoCmdAbbrev, ccapGotoEditInName ] );
  mniMainCmdNotes.Caption := format( cDoubleS, [ ccapGotoCmdAbbrev, ccapGotoEditInNotes ] );
  mniMainCmdLineEntry.Caption := format( ccapGotoEditIn, [ ccapGotoCmdlineAbbrev, ccapGotoEditInEntry ] );
  mniDblCSearchEntry.Caption := ccapGotoEditInEntry;

  mniMainCmdLineNotes.Caption := format( cDoubleS, [ ccapGotoCmdlineAbbrev, ccapGotoEditInNotes ] );

  mniMainProcs.Caption := ccapGotoProcs;
  mniDblCDetP.Caption := mniMainProcs.Caption + '...';

//were autocreated forms, but now manually created so translations can be updated.
  frmfindtext.UpdateCaptions;

//so that it doesn't get hidden, I set this manually and permanently
  FrameHint1.cbHints.Caption := ccbHintsEnglishOverride;
  btnPkexecMain.Hint := cPkexecHint;
  btnInsertPath.Hint := chintInsertFilePaths;
  btnInsertPath.Caption := '&J  ' + ccapInsertFilePaths;
  btnEditCmdName.Caption := ccapGenericEdit;
  ResetCommandsToTop;

end;

function TfrmMain.RunCmdLineExternal( const RunStr : string ) : string;
begin
//is used internally, calls to forms using this function must be wrapped in a fInternalComs := true/false pair;
//ensure the command is safe! There are no checks here!

  result := format( '<failed permission to run "%s">', [ RunStr ] );
  if not fInternalComs then
    exit;

  if not CanRunCmdLine( RunStr, 0, false ) then
    exit;

  result := RunCmdLine( RunStr, false, false, false );

end;

function TfrmMain.RunExternalHelpRequest( SL : TStringList ) : string;
var
  str, failed : string;
begin
  failed := format( '"%s %s" failed, invalid command', [ SL[ 0 ], SL[ 1 ] ] );
  str := ExtractFilePath( SL[ 0 ] );
  if str <> '' then
  begin
    if not IsFileExist( SL[ 0 ] ) then
    begin
      result := failed;
      exit;
    end;
  end
  else if not SystemFileFound( SL[ 0 ] ) then
       begin
         result := failed;
         exit;
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
    fAllowPkexec := ReadBool( cSectTabFormSettings, cFormSettingsAllowPkexec, false );
    btnPkexecMain.Enabled := fAllowPkexec and fAllowPkexecInstalled;

    fRootFile := ReadString( cSectTabFormSettings, cFormSettingsRootFile, cRootFileSudo );

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
begin
  btnLineEdit.Click;
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
      if actRun.Enabled then
        actRun.Execute;
    end
    else
    begin
      if btnLineEdit.Enabled then
        btnLineEdit.Click;
    end;
    exit;
  end;

  if ( Shift = [ ssCtrl ] ) or ( Shift = [ ssShift, ssCtrl ] ) then
  begin
    case Key of
      VK_F :
        begin
          if actFindCmdLine.Enabled then
            actFindCmdLine.Execute;
          Key := VK_UNKNOWN;
          exit;
        end;
      VK_A :
        begin
          if actNewCommandLine.Enabled then
            actNewCommandLine.Execute;
          exit;
        end;
      VK_V :
        begin
          if assigned( ClipCLO ) then //popCmdLinePaste.Enabled then
            popCmdLinePaste.Click;
          exit;
        end;
      VK_DELETE, VK_OEM_MINUS, VK_SUBTRACT :
        begin
          if actCmdLineUnDelete.Enabled then
            actCmdLineUnDelete.Execute;
          exit;
        end;
    end;
  end;

  if Key = VK_C then
  begin
    if Shift = [ ssCtrl ] then
    begin
      if actCopyClipCmdLine.Enabled then
        actCopyClipCmdLine.Execute;
      Key := VK_UNKNOWN;//listbox was overriding my ctrl-c;
      exit;
    end;

//order, apparently, doesn't matter, both work
    //if Shift = [ ssCtrl, ssShift ] then
    if Shift = [ ssShift, ssCtrl ] then
    begin
      mniCopyCLListClipClick( Self );
      exit;
    end;

    if Shift = [ ssCtrl, ssShift, ssAlt ] then
    begin
      if actCopyCmdLine.Enabled then
        actCopyCmdLine.Execute;
      exit;
    end;
    exit;
  end;

  if ( Key = VK_DELETE ) or ( Key = VK_OEM_MINUS ) or ( Key = VK_SUBTRACT ) then
  begin
    if actCmdLineDelete.Enabled then
      actCmdLineDelete.Execute;
    Key := VK_UNKNOWN;
  end;
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
  popCommandsPopup( self );
  popCmdLinesPopup( self );
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
  TryFocus( lbCommands );
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

    if not assigned( GetCmdRec( anIdx ).Cmd ) then
    begin
//load on-demand
      CmdObjHelper.LoadCmdObj( CmdObj, lbCommands.Items[ anIdx ] );
      GetCmdRec( anIdx ).Cmd := CmdObj;
    end
    else CmdObj := GetCmdRec( anIdx ).Cmd;

    if CLOIdx = cUseCmdObjIndexFlag then
      CmdObjToForm( CmdObj, GetCmdRec.CLIndex )
    else CmdObjToForm( CmdObj, CLOIdx );

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

  RefreshCmdObj( cUseCmdObjIndexFlag );
  //RefreshCmdObj;

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

  GetCmdRec.CLIndex := lbCmdLines.ItemIndex;

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

  if ( Shift = [ ssCtrl ] ) or ( Shift = [ ssShift, ssCtrl ] ) then
  begin
    case Key of
      VK_F :
        begin
          if btnFindCmd.Enabled then
            btnFindCmd.Click;
          exit;
        end;
      VK_A :
        begin
          if btnPlus.Enabled then
            btnPlus.Click;
          exit;
        end;
      VK_V :
        begin
          if assigned( ClipCLO ) then
            mniCmdPasteCmdLine.Click;
          exit;
        end;
      VK_DELETE, VK_OEM_MINUS, VK_SUBTRACT :
        begin
          if btnCommandUnDelete.Enabled then
            btnCommandUnDelete.Click;
          exit;
        end;
    end;
  end;

  if Key = VK_C then
  begin
    if Shift = [ ssCtrl ] then
    begin
      if actCopyClipCmd.Enabled then
        actCopyClipCmd.Execute;
      Key := VK_UNKNOWN; //listbox was overriding my ctrl-c;
      exit;
    end;
    if Shift = [ ssShift, ssCtrl ] then
    begin
      mniCopyCmdListClipClick( Self );
      exit;
    end;
    if Shift = [ ssCtrl, ssShift, ssAlt ] then
    begin
      if actCopyCmd.Enabled then
        actCopyCmd.Execute;
      exit;
    end;
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
  if ( ( shift = [ ssCtrl ] ) or ( shift = [ ssshift, ssCtrl ] ) )
     and ( key = VK_F )
     and ( lbDetachedProcesses.Items.Count > 1 ) then
    FindItem( lbDetachedProcesses );
end;

procedure TfrmMain.lbDispKeywordsDblClick( Sender : TObject );
begin
  if not ( Sender is TListbox ) then
    exit;
  MsgDlgMessage( ccapOverflow, trim( TListbox( Sender ).Items.Text ) );
  MsgDlgInfo( self );
end;

procedure TfrmMain.lbKeywordsDispKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin

  if ( Shift = [ ssCtrl ] ) or ( Shift = [ ssCtrl, ssShift ] ) then
  begin
    if ( key = VK_F ) and ( lbKeywords.Items.Count > 1 ) then
      FindItem( lbKeywordsDisp );
  end;

end;

procedure TfrmMain.lbKeywordsKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  case key of
    VK_OEM_PLUS, VK_ADD :
      begin
        btnKeyWordAdd.Click;
        exit;
      end;
    VK_DELETE, VK_OEM_MINUS, VK_SUBTRACT :
      begin
        btnKeyWordDelete.Click;
        exit;
      end;
  end;

  if ( Shift = [ ssCtrl ] ) or ( Shift = [ ssCtrl, ssShift ] ) then
  begin
    if ( key = VK_F ) and ( lbKeywords.Items.Count > 1 ) then
      FindItem( lbKeywords )
    else if ( key = VK_S ) and btnCmdOk.enabled then
           btnCmdOk.Click;
  end;

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

    SetNotificationState( false, actSearchRun, shpSearchRun );

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
    SetNotificationState( CDO.IsCommandLine, actSearchRun, shpSearchRun );

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

procedure TfrmMain.lbSearchCmdLine_ApplyActions( const IsCmd : byte );
begin
  if IsCmd = 0 then
  begin
    actSearchGotoCmd.Enabled := trim( lbSearchCmd.Items[ 0 ] ) <> '';
    actSearchFindCmd.Enabled := actSearchGotoCmd.Enabled and ( lbSearchCmd.Items.Count > 4 );
  end else
  begin
    actSearchGotoCmdLine.Enabled := lbSearchCmdLine.ItemIndex > -1;
    actSearchFindCmdLine.Enabled := actSearchGotoCmdLine.Enabled and ( lbSearchCmdLine.Items.Count > 1 );
  end;
end;

procedure TfrmMain.lblCommandNameDblClick( Sender : TObject );
begin
  RenameCommand(
       CmdObjHelper.GetNormalizedPath( lblPathAlias.Caption, lblCommandName.Caption ),
       simFile
               );
  ReFocus_Edit_Memo( true );
end;

procedure TfrmMain.lblCommandNameDispDblClick( Sender : TObject );
begin
  MsgDlgMessage( ccapOverflow, lblCommandNameDisp.Caption + cmsgMainHowToEditCmdName );
  MsgDlgInfo( self );
end;

procedure TfrmMain.lblCurrDBDblClick( Sender : TObject );
begin
  btnSwitchDB.Click;
end;

procedure TfrmMain.lblDispEntryDblClick( Sender : TObject );
begin
  if not ( Sender is TLabel ) then
    exit;
  MsgDlgMessage( ccapOverflow, trim( TLabel( Sender ).Caption ) );
  MsgDlgInfo( self );
end;

procedure TfrmMain.lblPathAliasDblClick( Sender : TObject );
var
  str, Desc, Cap : string;
begin
  if not assigned(CmdObj) then
    exit;

  Cap := GetProperPathLabelCaption( true );
  Desc := Get_Cmd_Field_DisplayCaption( fidLocationPath ) + ': ';

  if Cap = cCommandInPathStr then
    str := Desc + Cap + format( '   (%s)', [ GetLiteralPath_Generic( Cap, GetProperCmdNameCaption ) ] )
  else str := Desc + Cap;

  MsgDlgMessage( ccapOverflow, str );
  MsgDlgInfo( self );
  ReFocus_Edit_Memo( true );
end;

procedure TfrmMain.lbSearchCmdClick( Sender : TObject );
begin

  ProcessCmdDisplayObj( lbSearchCmd );
  FillDisplayObj_Detail;//load its CL's
  SetNotificationState( actSearchRun.Enabled or ( lbSearchCmdline.Items.Count > 0 ), actSearchRun, shpSearchRun );
  lbSearchCmdLine_ApplyActions( 0 );
  lbSearchCmdLine_ApplyActions( 1 );

//store last viewed indices on the current tabsheet
  nbCommands.ActivePage.Tag := lbSearchCmd.ItemIndex;

end;

procedure TfrmMain.lbSearchCmdKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  if ( Shift = [ ssCtrl ] ) or ( Shift = [ ssShift, ssCtrl ] ) then
  begin
    case Key of
      VK_F :
        begin
          if btnSearchFindCmd.Enabled then
            btnSearchFindCmd.Click;
          exit;
        end;
      VK_T :
        begin
          if btnSearchGotoCmd.Enabled then
            btnSearchGotoCmd.Click;
          exit;
        end;
    end;
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
  lbSearchCmdLine_ApplyActions( 1 );
end;

procedure TfrmMain.lbSearchCmdLineKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  if ( Key = vk_return ) and ( lbSearchCmdLine.ItemIndex > -1 ) then
  begin
    if actSearchRun.Enabled then
      btnSearchRun.Click;
    exit;
  end;

  if ( Shift = [ ssCtrl ] ) or ( Shift = [ ssShift, ssCtrl ] ) then
  begin
    case Key of
      VK_F :
        begin
          if btnSearchFindCmdLine.Enabled then
            btnSearchFindCmdLine.Click;
          exit;
        end;
      VK_T :
        begin
          if btnSearchGotoCmdLine.Enabled then
            btnSearchGotoCmdLine.Click;
          exit;
        end;
      VK_C :
        if ( Shift = [ ssShift, ssCtrl ] ) then
        begin
          mniSearchCmdLineListClipClick( Self );
          exit;
        end
        else
        begin
          mniSearchCmdLineItemClipClick( Self );
          Key := VK_UNKNOWN; //listbox was overriding my ctrl-c;
          exit;
        end;
    end;
  end;

  if Key in [ VK_4, VK_5 ] then
    SearchVK_F_Keys( Key );
end;

procedure TfrmMain.memDetachedProcessesKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  UnassMemosKeyDown( memDetachedProcesses, Key, Shift );
end;

procedure TfrmMain.memDispNotesKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  UnassMemosKeyDown( memDispNotes, Key, Shift );
end;

procedure TfrmMain.memEntryDblClick( Sender : TObject );
begin
  btnBuilder.Click;
end;

procedure TfrmMain.EditmemEntry;
var
  InPut, OutPut : string;
begin
  Input := trim( memEntry.Lines.Text );
  OutPut := '';
  if not EditCommandLine( format( cDoubleS, [ ccapGenericEdit, cNameItem_CommandLine ] ), Input, Output ) then
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

procedure TfrmMain.memNotesDispKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  UnassMemosKeyDown( MemNotesDisp, Key, Shift );
end;

procedure TfrmMain.memNotesKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  if ( Shift = [ ssCtrl ] ) then
  begin
    if ( key = VK_S ) and btnCmdOk.enabled then
    begin
      btnCmdOk.Click;
      exit;
    end;
  end;

  if Shift = [ ssShift, ssCtrl ] then
  begin
    if key = VK_F then
      FindInMemo( memNotes );
    if key = VK_L then
      frmFindText.ReFindinMemo( memNotes );
  end;

  if ( Key = vk_escape ) and btnCmdCancel.Enabled then
    btnCmdCancel.Click;

end;

procedure TfrmMain.memNotesLineChange( Sender : TObject );
begin
  memNotesLineDisp.Lines.Text := memNotesLine.Lines.Text;
end;

procedure TfrmMain.memNotesLineDispKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  UnassMemosKeyDown( memNotesLineDisp, Key, Shift );
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
    if ( key = VK_S ) and btnLineOk.Enabled then
    begin
      btnLineOk.Click;
      exit;
    end;
  end;

  if Shift = [ ssShift, ssCtrl ] then
  begin
    if key = VK_F then
      FindInMemo( memNotesLine );
    if key = VK_L then
      frmFindText.ReFindinMemo( memNotesLine );
  end;

  if ( Key = vk_escape ) and btnLineCancel.Enabled then
    btnLineCancel.Click;

  //self.FormKeyDown( Sender, Key, Shift ); memo1 dead when editing, hmmmmm.
end;

procedure TfrmMain.Memo1DblClick( Sender : TObject );
begin
  if not ( Sender is TMemo ) then
    exit;
  MsgDlgMessage( '', trim( TMemo( Sender ).Text ) );
  MsgDlgInfo( Self );
end;

procedure TfrmMain.Memo1KeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  UnassMemosKeyDown( Memo1, Key, Shift );
end;

procedure TfrmMain.UnassMemosKeyDown( Sender : TMemo; var Key : Word; const Shift : TShiftState );
begin
  if Shift = [ ssShift, ssCtrl ] then
  begin
    if key = VK_F then
      FindInMemo( Sender );
    if key = VK_L then
      frmFindText.ReFindinMemo( Sender );
  end;
end;

procedure TfrmMain.mniCmdCountClick( Sender : TObject );
begin
  UpdateDisplay( mniCmdCount.Caption + ': ' + inttostr( lbCommands.Items.Count ), false, false );
end;

procedure TfrmMain.mniCmdSendToClick( Sender : TObject );
begin
  SelectProfileToMergeTo;
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

procedure TfrmMain.mniKeyWordsAddClick( Sender : TObject );
begin
  btnKeywordAdd.Click;
end;

procedure TfrmMain.mniKeyWordsDeleteClick( Sender : TObject );
begin
  btnKeywordDelete.Click;
end;

procedure TfrmMain.SetUp_KeyBoardMenu_Structure;
begin

  mniMainCommands.tag := cmniMainCommands;
  mniMainDisplay.tag := cmniMainDisplay;

  //mniMainTabs : TMenuItem; HUB
  mniMainTabsCommands.Tag := cmniMainTabsCommands;
  mniMainTabsFav.Tag := cmniMainTabsFav;
  mniMainTabsKeyWords.Tag := cmniMainTabsKeyWords;
  mniMaintabsSearch.Tag := cmniMaintabsSearch;
  mniMainTabsProcs.Tag := cmniMainTabsProcs;

  mniMainFindOutput.Tag := cmniMainFindOutput;
  mniMainCommandLines.tag := cmniMainCommandLines;
  mniMainFindCmdLineNotes.Tag := cmniMainFindCmdLineNotes;
  mniMainFindCmdNotes.Tag := cmniMainFindCmdNotes;

  mniMainCmdNameEdit.tag := cmniMainCmdNameEdit;
  mniMainCmdNotes.tag := cmniMainCmdNotes;
  mniMainCmdLineEntry.tag := cmniMainCmdLineEntry;
  mniMainCmdLineNotes.tag := cmniMainCmdLineNotes;

  //mniMainCmdList : TMenuItem; HUB
  mniMainCmdListFav.tag := cmniMainCmdListFav;
  mniMainCmdListKey.tag := cmniMainCmdListKey;
  mniMainCmdListSearch.tag := cmniMainCmdListSearch;

  //mniMainCmdLineList : TMenuItem; HUB
  mniMainCmdLineFav.tag := cmniMainCmdLineFav;
  mniMainCmdLineKey.tag := cmniMainCmdLineKey;
  mniMainCmdLineSearch.tag := cmniMainCmdLineSearch;

  mniMainProcs.tag := cmniMainProcs;

  mniMainRun.tag := cmniMainRun;
  mniMainHint.tag := cmniMainHint;

  mniDblCCmd.Tag := cmniDblCCmd;
  mniDblCCmdName.Tag := cmniDblCCmdName;
  mniDblCCmdPath.Tag := cmniDblCCmdPath;
  mniDblCCmdNotes.Tag := cmniDblCCmdNotes;
  mniDblCCmdKey.Tag := cmniDblCCmdKey;
  mniDblCCmdLines.Tag := cmniDblCCmdLines;
  mniDblCCmdLineFriendly.Tag := cmniDblCCmdLineFriendly;
  mniDblCCmdLineNotes.Tag := cmniDblCCmdLineNotes;
  mniDblCSearch.Tag := cmniDblCSearch;
  mniDblCSearchCmd.Tag := cmniDblCSearchCmd;
  mniDblCSearchCmdLine.Tag := cmniDblCSearchCmdLine;
  mniDblCSearchEntry.Tag := cmniDblCSearchEntry;
  mniDblCSearchFriendly.Tag := cmniDblCSearchFriendly;
  mniDblCSearchNotes.Tag := cmniDblCSearchNotes;
  mniDblCSearchKey.Tag := cmniDblCSearchKey;
  mniDblCDetP.Tag := cmniDblCDetP;
  mniDblCDetPProcs.Tag := cmniDblCDetPProcs;
  mniDblCDetPInfo.Tag := cmniDblCDetPInfo;
  mniDblCDisplay.Tag := cmniDblCDisplay;

//arrow movement tags
  lbCommands.Tag := cArrowKeyCommands;
  memNotes.Tag := cArrowKeymemNotes;
  lbCmdLines.Tag := cArrowKeyCmdLines;
  memEntry.Tag := cArrowKeymemEntry;
  memNotesLine.Tag := cArrowKeymemNotesLine;
  lbKeyWords.Tag := cArrowKeylbKeywords;
  Memo1.Tag := cArrowKeyMemo1;
  lbSearchCmd.Tag := cArrowKeySearchCmd;
  lbSearchCmdLine.Tag := cArrowKeySearchCmdLine;
  lbDetachedProcesses.Tag := cArrowKeyDetachedProcesses;
  memDetachedProcesses.Tag := cArrowKeymemDetachedProcesses;

end;

procedure TfrmMain.mniDblCCmdNameClick( Sender : TObject );
begin
  case TControl(Sender).Tag of
    //cmniDblCCmd : ;
    cmniDblCCmdName :
      if EditingCmd then
        lblCommandNameDblClick( lblCommandName )
      else lblCommandNameDispDblClick( lblCommandNameDisp );
    cmniDblCCmdPath : lblPathAliasDblClick( self );
      //if EditingCmd then
      //  lblPathAliasDblClick( lblPathAlias )
      //else lblPathAliasDblClick( lblPathAliasDisp );
    cmniDblCCmdNotes :
      if EditingCmd then
        Memo1DblClick( memNotes )
      else Memo1DblClick( MemNotesDisp );
    cmniDblCCmdKey :
      if EditingCmd then
        lbDispKeywordsDblClick( lbKeywords )
      else lbDispKeywordsDblClick( lbKeywordsDisp );
    cmniDblCCmdLines :
      if EditingCL then
        memEntryDblClick( memEntry )
      else lbCmdLinesDblClick( lbCmdLines );
    cmniDblCCmdLineFriendly :
      if EditingCL then
        TryFocus( edtFriendlyNameLine )
      else lblDispEntryDblClick( lblFriendlyNameLineDisp );
    cmniDblCCmdLineNotes :
      if EditingCL then
        Memo1DblClick( memNotesLine )
      else Memo1DblClick( memNotesLineDisp );
    //cmniDblCSearch : ;
    cmniDblCSearchCmd : lbDispKeywordsDblClick( lbSearchCmd );
    cmniDblCSearchCmdLine : lbDispKeywordsDblClick( lbSearchCmdLine );
    cmniDblCSearchEntry : lblDispEntryDblClick( lblDispEntry );
    cmniDblCSearchFriendly : lblDispEntryDblClick( lblDispFriendlyName );
    cmniDblCSearchNotes : Memo1DblClick( memDispNotes );
    cmniDblCSearchKey : lbDispKeywordsDblClick( lbDispKeyWords );
    //cmniDblCDetP : ;
    cmniDblCDetPProcs : lbDispKeywordsDblClick( lbDetachedProcesses );
    cmniDblCDetPInfo : Memo1DblClick( memDetachedProcesses );
    cmniDblCDisplay : Memo1DblClick( Memo1 );
  end;
end;

procedure TfrmMain.mniOutputCommandooInfoClick( Sender : TObject );
begin
  btnMainInfo.Click;
end;

procedure TfrmMain.mniMainCommandsClick( Sender : TObject );
var
  menuIdx, tabIdx : integer;

  procedure SetTab( aTab : TTabSheet );
  begin
    if nbCommands.ActivePage <> aTab then
      SetActivePage( aTab );
  end;

begin

  menuIdx := TControl(Sender).Tag;

  if menuIdx = 0 then
    exit;

  if menuIdx >= 1000 then
  begin
    case menuIdx of
      cmniMainHint : FrameHint1.cbHints.Checked := not FrameHint1.cbHints.Checked;
    end;
    exit;
  end;

  tabIdx := trunc( menuIdx / 100 );
  case tabIdx of
    1 : SetTab( tsCommands );
    2,3,4,5 :
      begin
        if Editing then exit;
        case tabIdx of
          2 : SetTab( tsFavorites );
          3 : SetTab( tsKeyWords );
          4 : SetTab( tsSearch );
          5 : SetTab( tsDetachedProcesses );
        end;
      end;
  end;

  case menuIdx of
    cmniMainCommands : TryFocus( lbCommands );//
    cmniMainDisplay : TryFocus( Memo1 );
    cmniMainFindOutput : FindInMemo( Memo1 );
    cmniMainCommandLines : TryFocus( lbCmdLines );
    cmniMainFindCmdNotes :
      if EditingCmd then
        FindInMemo( memNotes )
      else FindInMemo( memNotesDisp );
    cmniMainFindCmdLineNotes :
      if EditingCL then
        FindInMemo( memNotesLine )
      else FindInMemo( memNotesLineDisp );
    cmniMainCmdNameEdit : lblCommandNameDblClick( Self );
    cmniMainCmdNotes : TryFocus( memNotes );
    cmniMainCmdLineEntry : TryFocus( memEntry );
    cmniMainCmdLineNotes : TryFocus( memNotesLine );
    cmniMainCmdListFav, cmniMainCmdListKey, cmniMainCmdListSearch :
      TryFocus( lbSearchCmd );
    cmniMainCmdLineFav, cmniMainCmdLineKey, cmniMainCmdLineSearch :
      TryFocus( lbSearchCmdLine );
    cmniMainProcs : TryFocus( lbDetachedProcesses );
    cmniMainRun : if actRun.Enabled then actRun.Execute;
  end;

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
          lbSearchCmdLine.Hint := cSearchHintGeneralCmdLine + cSearchHintEnding;
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
    TryFocus( lbSearchCmd );
  end;

begin

  UpdateNotebookCaptions;

  if nbCommands.ActivePage = tsCommands then
    TryFocus( lbCommands )
  else if nbCommands.ActivePage = tsFavorites then
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
  end
  else if nbCommands.ActivePage = tsDetachedProcesses then
    TryFocus( lbDetachedProcesses );

end;

procedure TfrmMain.popCmdLineReCenterClick( Sender : TObject );
begin
  btnCmdLineReCenter.Click;
end;

procedure TfrmMain.UpdateDisplay_Internal( const Output : string; const DoIndicate : boolean );
begin
  fUpdateDisplayOffsetFlag := true;
  //needs love but not now. There are two very hard to find/manage LineEnding's out from OUTPUT routine
  //and that makes the adjustment inexact. Hence, + 2
  fUpdateDisplayOffset := length( Output ) + 2;
  UpdateDisplay( Output, DoIndicate, false );
end;

procedure TfrmMain.UpdateDisplay( const Output : string; const DoIndicate, DoSetFocus : boolean );
var
  NumLines : Int64;
  GoToPos : Integer;
begin
  //fDisplayOutPutMax := 500; //testing

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
  begin
    fDisplayOutPut.Text := fDisplayOutPut.Text
                           + LineEnding
                           + format( cmsgBadData_ESC_MSG, [ cprogEscapeReplacement ] )
                           + stringreplace( Output, char(#27), cprogEscapeReplacement, [ rfreplaceall ] )
                           + strif( DoIndicate, cmsgOutputEndIndicator );
  end
  else
    fDisplayOutPut.Text := fDisplayOutPut.Text
                           + LineEnding
                           + Output
                           + strif( DoIndicate, cmsgOutputEndIndicator );

  Memo1.Lines := TStrings( fDisplayOutPut );

  if fUpdateDisplayOffsetFlag then
  begin
    Memo1.SelStart := GoToPos + 1;
    fUpdateDisplayOffsetFlag := false;
  end
  else
  begin
    Memo1.SelStart := GoToPos + 1 - fUpdateDisplayOffset;
    fUpdateDisplayOffset := 0;
  end;

  if FIsInitialized and DoSetFocus then
    TryFocus( Memo1 );

  NumLines := trunc( memo1.Height / memo1.Font.GetTextHeight( 'X' ) );
  if Memo1.CaretPos.Y > NumLines - 3 then
    Memo1.ScrollBy( 0, trunc( Memo1.Height * 0.8 ) * -1 );

end;

procedure TfrmMain.CheckSearchChange;
begin
  if fSearchMayHaveChanged then
  begin
    fSearchMayHaveChanged := false;
    if ( fKeyWordSR.Count > 0 ) or ( fSearchSR.Count > 0 ) then
      UpdateDisplay( cAttnBar + cmsgSearchMayBeInvalid + cAttnBar, true, false );
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

//this just goes to the OKidx of checking if command name exists
  if not GetNewCommand( InputName, ComName, InputMode, lbCommands.ItemIndex ) then
    exit;

  CmdObjHelper.ProcessFileNamePath(ComName, ComFile, ComPath);

  lblCommandName.Caption := ComFile;
  UpdatePathsInEdit( ComPath, false );

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

procedure TfrmMain.popCmdLinesPopup(Sender: TObject);
var
  aCmdLineObj: TCmdLIneObj;
  locDoShow, ValidSelection: boolean;
begin

//strangely, to me, the popupmenu is still enabled even if the lbCmdLines is disabled!!
//so... locDoShow (DoShow conflicts with LCL)!

  //locDoShow := lbCmdLines.Enabled;//Flag that CL is being edited
  locDoShow := not EditingCL;
  popCmdLineReCenter.Caption := btnReCenter.Caption + ' ' + ccapGotoCmdlineAbbrev;

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

  SetNotificationState( ValidSelection or pnlCmdLine.Visible, actRun, shpRun_Test );

  actCopyCmdLine.Enabled := ValidSelection and locDoShow;
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


procedure TfrmMain.popCommandsPopup(Sender: TObject);
var
  NoShow: boolean;
begin

  NoShow := pnlCommandList.Enabled;

  actPlus.Enabled := NoShow and InfoServer.IsInitialized and not BadDB;

  mniCmdPaste.Enabled := assigned( ClipCO );
  mniCmdPasteCmdLine.Enabled := assigned( ClipCLO );
  actFindCmd.Enabled := ( lbCommands.Items.Count > 1 );
  mniCmdSortcommands.Caption := btnSortCommands.Caption;
  mniCmdReCenter.Caption := btnReCenter.Caption;

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

procedure TfrmMain.mniCmdPasteClick( Sender : TObject );
begin
  DuplicateCmd( ClipCO );
end;

procedure TfrmMain.popDblCPopup( Sender : TObject );
begin
  if nbCommands.ActivePage = tsCommands then
  begin
    mniDblCCmd.Visible := true;
    mniDblCSearch.Visible := false;
    mniDblCDetP.Visible := false;
  end else if nbCommands.ActivePage = tsDetachedProcesses then
  begin
    mniDblCCmd.Visible := false;
    mniDblCSearch.Visible := false;
    mniDblCDetP.Visible := true;
  end else
  begin
    mniDblCCmd.Visible := false;
    mniDblCSearch.Visible := true;
    mniDblCDetP.Visible := false;
    if nbCommands.ActivePage = tsFavorites then
      mniDblCSearch.Caption := trim( ccapTabFavorites ) + Dots
    else if nbCommands.ActivePage = tsKeyWords then
      mniDblCSearch.Caption := trim( ccapTabKeyWords ) + Dots
    else mniDblCSearch.Caption := trim( ccapTabSearch ) + Dots;
  end;

end;

procedure TfrmMain.popGotoPopup( Sender : TObject );
var
  HasCommands, IsEditing, CmdEditing, CLEditing : boolean;
begin
  mniMainRun.Caption := format( '%s %s', [ trim( copy(btnRun_Test.Caption, 3, maxint ) ), ccapGotoCmdlineAbbrev ] );

  CmdEditing := EditingCmd;
  CLEditing := EditingCL;
  IsEditing := CmdEditing or ClEditing;
  HasCommands := lbCommands.Items.Count > 0;

  mniMainCommands.Enabled := not IsEditing;

  mniMainTabs.Enabled := not IsEditing;
  mniMainCommandLines.Enabled := not IsEditing and HasCommands;

  mniMainFindCmdNotes.Enabled := HasCommands and
                                ( ( not CmdEditing and ( trim( memNotesDisp.Text ) <> '' ) )
                                    or ( CmdEditing and ( trim( memNotes.Text ) <> '' ) ) );
  mniMainFindCmdLineNotes.Enabled := HasCommands and
                                    ( ( not CLEditing and ( trim( memNotesLineDisp.Text ) <> '' ) )
                                        or  ( CLEditing and ( trim( memNotesLine.Text ) <> '' ) ) );

  mniMainCmdNameEdit.Enabled := CmdEditing;
  mniMainCmdNotes.Enabled := CmdEditing;
  mniMainCmdLineEntry.Enabled := CLEditing;
  mniMainCmdLineNotes.Enabled := CLEditing;

  mniMainCmdList.enabled := not IsEditing;

  mniMainCmdLineList.enabled := not IsEditing;

  mniMainProcs.Enabled := not IsEditing;

  mniMainRun.Enabled := actRun.Enabled;

end;

procedure TfrmMain.popSearchCmdLinePopup( Sender : TObject );
begin
  mniSearchCmdLineListClip.Enabled := mniSearchRun.Enabled;
  mniSearchCmdLineItemClip.Enabled := mniSearchCmdLineListClip.Enabled;
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
  SetNotificationState( False, actRun, shpRun_Test );

end;

procedure TfrmMain.ClearCmdLineDisps;
begin
//edge case on new commands, "disp"s aren't cleared
  memNotesLineDisp.Lines.Text := '';
  lblSuperUserLineDisp.Visible := false;
  lblIsFavoriteLineDisp.Visible := false;
  lblDetachProcessLineDisp.Visible := false;
  lblWantsInputLineDisp.Visible := false;
  lblUseShellLineDisp.Visible := false;
  lblTerminalOnlyLineDisp.Visible := false;
  lblAlertLineDisp.Visible := false;

  cbThreatLevelLine.ItemIndex := -1;
  ApplyThreatLevel( pnlCLEdit, cbThreatLevelLine );
  EchoThreatLevelDisplay( pnlCL, cbThreatLevelLineDisp, lblThreatLevelLineDisp, cbThreatLevelLine.ItemIndex );

  edtFriendlyNameLine.Text := dots;

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
  SetNotificationState( False, actRun, shpRun_Test );

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

    edtFriendlyNameLine.Text := strif( FriendlyName <> '', FriendlyName );

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
     SetNotificationState( True, actSave, shpSave )
  else UpdateSaveStatus;
  popCommandsPopup( self );
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

  SetNotificationState( False, actRun, shpRun_Test );

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
      SetNotificationState( True, actSave, shpSave );

    SetNotificationState( True, actRun, shpRun_Test );
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
    if edtFriendlyNameLine.Text <> '...' then
      FriendlyName := edtFriendlyNameLine.Text;
    //else FriendlName := '';

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

procedure TfrmMain.UpdatePathsInEdit( const ThePath : string; const Complete : boolean );
begin
  if thePath = '' then
     lblPathAlias.Caption := cmsgcleBadPath
  else lblPathAlias.Caption := ThePath;
  if Complete then
    lblPathAliasDisp.Caption := lblPathAlias.Caption;
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

    UpdatePathsInEdit( LocationPath, true );

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

    popCmdLinesPopup( self );

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

    LocationPath := lblPathAlias.Caption;
    UpdatePathsInEdit( LocationPath, true );

    actRevert.Enabled := DoUpdate;

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

procedure TfrmMain.LoadSearchResults_Internal( aSearch : TSearchObj; Strings : TStrings; const IsSimple : boolean );
begin
  fIsSimpleSearch := IsSimple;
  LoadSearchResults( aSearch, Strings );
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
 OtherChoice : integer;
begin

  if InvalidCommands_Msg then
    exit;

  if CheckEditing or BadDB then
    exit;

  ShowUnsavedMessage;

  fSimpleSearchSO.Clear( '' );

  with TfrmSimpleSearch.Create( self ) do
  try
    Initialize( fSearchFields );
    Showmodal;
    if ModalResult = mrOK then
    begin
      FillSearchObj( fSimpleSearchSO );
      LoadSearchResults_Internal( fSimpleSearchSO, fSearchSR, true );
      if nbCommands.ActivePage <> tsSearch then
        SetActivePage( tsSearch );
    end;
    OtherChoice := UseAdancedSearch;

  finally
    free;
  end;

  if OtherChoice > -1 then
  begin
    case OtherChoice of
      1 : btnSearchNewSearch.Click;
      2 : btnSearchLoadSearch.Click;
      else btnSearchSearch.Click;
    end;

    if nbCommands.ActivePage <> tsSearch then
      SetActivePage( tsSearch );
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
    Options := Options + [ ofPathMustExist, ofFileMustExist ];

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
              SetActivePage( tsSearch );

          end;
        tcsKeyWordList :
          begin
//I had meant to make this more generic by using constant array, but turns out it is probably useless.
//but I leave it, who knows what comes in fuTure.
            ShowSearch_List( [ fidKeyWords ], fKeyWordSO, fKeyWordSR );

            if nbCommands.ActivePage <> tsKeyWords then
              SetActivePage( tsKeyWords );
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
{$IFNDEF Release}
  if SL.Count < 2 then
  begin
    UpdateDisplay( 'TfrmMain.GetHelpOutput: Developer. Malformed Help request.', false, false );
    exit;
  end;
{$ENDIF}
  if trim( SL[ 1 ] ) = '' then
  begin
    UpdateDisplay( format( cmsgcleNoHelpParam, [ SL[ 0 ] ] ), false, false );
    exit;
  end;
//needs to be a separate section for help's because of remote calls
  if not fInternalComs then
    UpdateDisplay_Internal( StandardOutputHeader( SL[ 0 ]  + ' ' + SL[ 1 ] ), false );

  Result := CmdObjHelper.RunCommand( SL, false );

end;

function TfrmMain.CmdInPath : boolean;
var
  str : string;
begin
  str := GetProperPathLabelCaption;

  Result := str = cCommandInPathStr;

  //if Result then
  //  exit;
  //if CheckForBuiltin then
  //  result := str = cLinuxBuiltInStr;
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
  Path, Cmd, Help : string;
begin

  if not assigned(CmdObj) then
    exit;

  //this display the help for the proper command, it differentiates depending if you are
  //editing or not, and display appropriately for the situation. The actual cmd obj is "buffered"
  //and so anyone working on a command can change it at will, but correct help will be shown if it exists.
  //This fixes a bug where when using the command object editing changes were not used.
  Path := GetProperPathLabelCaption;
  Cmd := GetProperCmdNameCaption;
  Help := trim( edtHelp.Text );

  SL := TStringList.Create;
  try

    if CmdInPath then
    begin
      if not SystemFileFound( Cmd ) then
      begin
        UpdateDisplay( format( cFileNotExist, [ Cmd + ' (' + Path + ')' ] ), true, false );
        exit;
      end;
      SL.Add( Cmd );
      SL.Add( Help )
    end else
    begin
      if Path = cmsgcleBadPath then
      begin
        UpdateDisplay( format( cFileNotExist, [ Cmd ] ), true, false );
        exit;
      end;
      if not IsFileExist( Path + Cmd, true ) then
        exit;
      SL.Add( Path + Cmd );
      SL.Add( Help )
    end;

    UpdateDisplay( GetHelpOutput( SL ), true, true );

  finally
    SL.free;
  end;

end;

procedure TfrmMain.btnInsertPathClick( Sender : TObject );
var
  str : string;
begin
  str := GetRealPath;
  if str <> '' then
    memEntry.Text := str + memEntry.Text;
  TryFocus( memEntry );
end;


procedure TfrmMain.btnLocationPathClick(Sender: TObject);
begin
  lblPathAliasDblClick( self );
end;

procedure TfrmMain.btnMainInfoClick( Sender : TObject );
begin
  UpdateDisplay( GetCurrentState, false, false );
end;

procedure TfrmMain.btnPkexecMainClick( Sender : TObject );
begin
  memEntry.Text := TogglePkexec( memEntry.Text );
  TryFocus( memEntry );
end;

procedure TfrmMain.btnReCenterClick( Sender : TObject );
begin
  TryFocus( lbCommands );
  lbCommands.MakeCurrentVisible;
end;

procedure TfrmMain.btnRefreshFavoritesClick( Sender : TObject );
begin
  LoadFavorites;
  SetNotificationState( false, btnRefreshFavorites, shpRefreshFavorites );
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

function TfrmMain.DifferingCommandPaths : boolean;
begin
  result := lblPathAlias.Caption <> lblPathAliasDisp.Caption;
end;


function TfrmMain.GetProperCmdNameCaption : string;
begin
  if EditingCmd then
    result := lblCommandName.Caption
  else result := lblCommandNameDisp.Caption;
end;

function TfrmMain.GetRealPath : string;
var
  Pathstr, PathStrDisp : string;
  PathStrOK, PathStrDispOK : boolean;
begin
//this is for inserting a path under editing situations where a decision must be made of which
//CmdName/pathalias should be used.
  Result := '';
  PathStr := lblPathAlias.Caption;
  PathStrDisp := lblPathAliasDisp.Caption;

  PathStrOK := CmdObjHelper.HasRealPath( PathStr );
  PathStrDispOK := CmdObjHelper.HasRealPath( PathStrDisp );

  if not PathStrOK and not PathStrDispOK then
  begin
    MyShowmessage(
      cmsgNoPathInsertionTop
      + strif(
          EditingCmd,
          format( cmsgNoPathInsertionEditing, [ lblCommandNameDisp.Caption, PathStrDisp, lblCommandName.Caption, PathStr  ] ),
          format( cmsgNoPathInsertionNotEditing, [ lblCommandNameDisp.Caption, PathStrDisp ] ) )
      + cmsgNoPathInsertionBottom,
      self );
    exit;
  end;

  if ( PathStr <> PathStrDisp ) and PathStrOK and PathStrDispOK then
  begin
  //if TfrmMain( Owner ).EditingCmd then
    if DifferingCommandPaths then
    begin
      MsgDlgMessage( ccapcleUncertainPath, format( cmsgcleUncertainPath, [ PathStr, PathStrDisp ] ) );
      if MsgDlgConfirmation( self ) = mrYes then
        result := PathStr
      else result := PathStrDisp;
    end;
  end else
  begin
    if PathStrOK then
      result := PathStr
    else result := PathStrDisp;
  end;

  //memCmdLine.Text := str + memCmdLine.Text;

end;

function TfrmMain.GetProperPathLabelCaption( const ForDisplay : boolean = false ) : string;
begin
  if EditingCmd then
    result := lblPathAlias.Caption
  else result := lblPathAliasDisp.Caption;

  if ( result = cmsgcleBadPath ) and ForDisplay then
    result := cmsgNotSpecified;
end;

procedure TfrmMain.btnVersionCommandClick(Sender: TObject);
var
  CmdStr, Path, Cmd, Ver : string;
begin

  if not assigned(CmdObj) then
    exit;

//see comment in btnHelpCommandClick
  Path := GetProperPathLabelCaption;
  Cmd := GetProperCmdNameCaption;
  Ver := trim( edtVersion.Text );

  if Ver = '' then
  begin
    UpdateDisplay( format( cmsgNoVersionFlagSpecified, [ GetProperCmdNameCaption, cmsgHelpVersionInfoVersion ] ), true, false );
    exit;
  end;

  if CmdInPath then
  begin
    if not SystemFileFound( Cmd, true ) then
      exit
    else CmdStr := trim( Cmd + ' ' + Ver );
  end else
  begin
    if Path = cmsgcleBadPath then
    begin
      UpdateDisplay( format( cFileNotExist, [ Cmd ] ), true, false );
      exit;
    end;
    CmdStr := Path + Cmd;
    if not IsFileExist( CmdStr, true ) then
      exit
    else CmdStr := trim( CmdStr + ' ' + Ver );
  end;

  if not CanRunCmdLine( CmdStr, 0, false ) then
    exit;

  UpdateDisplay( RunCmdLine( CmdStr, false, false, false ), true, true );

end;

function TfrmMain.NotEditing: boolean;
begin
  Result := not Editing;
end;

function TfrmMain.EditingCmd: boolean;
begin
  result := not TControl( fCommandButtons[ 0 ] ).Enabled;
end;

function TfrmMain.EditingCL: boolean;
begin
  Result := not TControl( fCommandLineButtons[ 0 ] ).Enabled;
end;

function TfrmMain.Editing: boolean;
begin
  Result := EditingCmd or EditingCL;
  //Result := not TControl( fCommandButtons[ 0 ] ).Enabled
  //          or not TControl( fCommandLineButtons[ 0 ] ).Enabled;
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
        SetNotificationState( CmdObj.DoUpdate, actSave, shpSave );
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
        SetNotificationState( CmdLineObj.DoUpdate, actSave, shpSave );
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

  ToggleEditMode( aList, IsEdit );
  case TControl(Sender).Tag of
    cEditButtonsCommandTag: TryFocus( MemNotes );
    cEditButtonsLineTag:
      if memEntry.Canfocus then
      begin
        memEntry.SelStart := length( memEntry.Lines.Text );
        memEntry.SetFocus;
      end;
    cOKButtonCommandTag, cCancelButtonCommandTag : TryFocus( lbCommands );
    cOKButtonLineTag, cCancelButtonLineTag : TryFocus( lbCmdLines );
  end;

  UpdateMainActions;
  UpdateNotebookEditingStatus;

end;

procedure TfrmMain.btnCmdLineReCenterClick( Sender : TObject );
begin
  TryFocus( lbCmdLines );
  lbCmdLines.MakeCurrentVisible;
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
    if not assigned( aProcess ) then
      continue;
    if not aProcess.Running and not aProcess.Active then
      RemoveDetachedProcess( i, -1, aProcess );
  end;
  UpdateDetachedProcesses( '', nil );
end;

procedure TfrmMain.tmrCancelRunBlinkTimer( Sender : TObject );
begin
  if tmrCancelRunBlink.Interval > 2000 then
  begin
    tmrCancelRunBlink.Interval := 750;
    shpSRL.Brush.Color := $002C78CA;
    shpSRR.Brush.Color := $002C78CA;
  end else
  begin
    tmrCancelRunBlink.Interval := 2300;
    shpSRL.Brush.Color := $000000A0;
    shpSRR.Brush.Color := $000000A0;
  end;
end;

procedure TfrmMain.tmrCancelRunTimer( Sender : TObject );
begin
  tmrCancelRun.enabled := false;
  btnCancelRun.Visible := true;
  shpSRL.Brush.Color := $000000A0;
  shpSRR.Brush.Color := $000000A0;
  shpSRL.Visible := true;
  shpSRR.Visible := true;
  tmrCancelRunBlink.Interval := 2300;
  tmrCancelRunBlink.Enabled := true;
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

  TryFocus( lbCmdLines );

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

  TryFocus( lbCmdLines );

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

  //Idx := lbCommands.Items.AddObject('<>', NewCmdObj);
  Idx := lbCommands.Items.Add( '<>' );
  lbCommands.Items.Objects[ Idx ] := TCmdRec.Create;
  TCmdRec( lbCommands.Items.Objects[ Idx ] ).Cmd := NewCmdObj;
  NewCmdObj := nil;

  lbCommands.ItemIndex := Idx;
  fLastlbCmdLinesIdx := -1;

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
//punt for now: sql searches don't load commmand name like text searches (main listbox only, sub list is ok)
//so, for sql, must go ask for CL's Command entry if it wasn't loaded. Only one case: CL is in top search Listbox
//quick fix for first version. Deluxe version (later?) will address this better.
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

  if CanJumpToCommand( sqlFix, lblDispEntry.Caption ) then
    SetActivePage( tsCommands )
  else result := false;

end;

function TfrmMain.GotoCommand( Sender : TListBox ) : boolean;
begin
  result := false;
  if ( Sender.ItemIndex > -1 ) and assigned( Sender.Items.Objects[ Sender.ItemIndex ] ) then
    Result := GoTo_Command( Sender );
end;

procedure TfrmMain.actOptionsExecute( Sender : TObject );
var
  LanguageIdx: Integer;
  MsgStr : string;
begin

  if CheckEditing then
    exit;

{$IFDEF Release}
  if fOpenInstances > 1 then
  begin
    MsgDlgMessage( ccapMainOptionsNotAvailable, cmsgMainMultipleCopiesOpen );
    MsgDlgAttention( self );
    exit;
  end;
{$ENDIF}
  MsgStr := '';
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
      cbAllowPkexec.Checked := fAllowPkexec;// and fAllowPkexecInstalled;
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

      lblSqlLib.Caption := fSqliteLibrary;

      cbLargerFont.Checked := globFontsLarge;
      lblSavePath.Caption := fSavingToPath;
      lblBaseFolder.Caption := fWritingToPath;

      fInternalComs := true;
      Showmodal;
      fInternalComs := false;

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

      fAllowPkexec := cbAllowPkexec.Checked;
      if fAllowPkexec and not fAllowPkexecInstalled then
      begin
        fAllowPkexec := false;
        MyShowMessage( cmsgPkexecNotInstalled, self );
      end;
      btnPkexecMain.Enabled := fAllowPkexec and fAllowPkexecInstalled;

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
                           false,
                           false );
            if MsgStr <> '' then
              UpdateDisplay( MsgStr, false, false );

            MsgDlgMessage( ccapOptSqliteSearch, cmsgOptSqliteSearch );
            if MsgDlgConfirmation( self ) = mrYes then
              if CanRunCmdLine( cSqliteLocateCommand, 0, false ) then
                UpdateDisplay( RunCmdLine( cSqliteLocateCommand, false, false, false ), false, true );
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
        WriteBool( cSectTabFormSettings, cFormSettingsAllowPkexec, fAllowPkexec );

        UpdateFile;

      end;

      if lblBaseFolder.Caption <> fWritingToPath then
      begin
        WritePathOverride( lblBaseFolder.Caption );
        MyShowmessage( cOutPutChangedBaseFolder, self );
        UpdateDisplay( cOutPutChangedBaseFolder, false, true );
      end;

    finally
      Free;
    end;

end;

procedure TfrmMain.actOutPutClearExecute( Sender : TObject );
begin
  fDisplayOutPut.Clear;
  UpdateDisplay( format( cmsgDisplayOutputCleared, [ TimeToStr( now ) ] ), false, false );
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
      MsgDlgMessage( ccapSwitchDBSave, cmsgSwitchDBSave0 );
      if MsgDlgConfirmation( self ) = mrNo then
        exit;

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
        SetActivePage( tsCommands );
        UpdateDisplay( format( cmsgSwitchProfileString, [ DateTimeToStr( now ), lblCurrDB.Caption ] ), false, false );
      end;

    finally
      Free;
    end;

end;

procedure TfrmMain.SetActivePage( Page : TTabSheet );
begin
  nbCommands.ActivePage := Page;
  nbCommandsChange( nbCommands );
end;

procedure TfrmMain.SelectProfileToMergeTo;
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
                                 + strif( fUseDB, cDefaultDBProfileIsDBStr, cDefaultDBProfileIsDBStrNot ) )
                                 + ':  "'
                                 + CurrProfileName + ' '
                                 + trim( strif( CurrProfileIsDB, cDefaultDBProfileIsDBStr, cDefaultDBProfileIsDBStrNot ) )
                                 + '"'
                                 ,
                                 false,
                                 false )
                       ;
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
  if not EditCommandLine( format( cDoubleS, [ ccapGenericEdit, cNameItem_CommandLine ] ), Input, Output, false ) then
  begin
    TryFocus( memEntry );
    exit;
  end;
  memEntry.Text := Output;
  TryFocus( memEntry );
end;

procedure TfrmMain.btnCancelRunClick( Sender : TObject );
begin
  CloseCancelRun;
  globltDoCancelProcess := true;
end;

procedure TfrmMain.actProfilesExecute( Sender : TObject );
begin
  OpenProfiles( false );
end;

procedure TfrmMain.actQuickRunExecute( Sender : TObject );
begin
//only shown in developer mode, decided too dangerous (no checks) to have it as a general option
{$IFNDEF RELEASE}
  if not DoSingleInput( ccapQuickRun, fLastQuickRun, simEdit, self, false ) then
    exit;
  UpdateDisplay( RunCmdLine( fLastQuickRun, false, false, false ), true, true );
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
                lblDispDetachProcess.Visible ),
                true,
                true
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
        aCmdObj := GetCmdRec( i ).Cmd;
        if not Assigned(aCmdObj) then
          continue;
        if aCmdObj.DoUpdate then
        begin
          if aCmdObj.DoDelete then
          begin
            aCmdObj.RemoveData;
            FreeCommand( i, true, true );
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
        FreeCommand(i, true, false);
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
      UpdateDisplay( format( cmsgFailedSave, [ TimeToStr( now ) ] ), false, true );
      exit;
    end;

    CmdObj := nil;
    RefreshCmdObj( OldCLOIndex );

    if not fManRefreshFavorites then
      btnRefreshFavorites.Click
    else SetNotificationState( true, btnRefreshFavorites, shpRefreshFavorites );

    UpdateDisplay( format( cmsgCommandsSaved, [ lblCurrDB.Caption, TimeToStr( now ) ] ), false, false );

    CheckSearchChange;

  finally
    Screen.Cursor := crDefault;
    SetNotificationState( False, actSave, shpSave );
  end;

end;

procedure TfrmMain.actSearchDisplayKeyCmdExecute( Sender : TObject );
begin
  UpdateDisplay( fKeyWordSO.GetAllExpressions, true, true );
end;

procedure TfrmMain.actSearchDisplaySearchExecute( Sender : TObject );
begin
  if fIsSimpleSearch then
    UpdateDisplay( format( cDoubleS + ':', [ cNameItem_SearchSimple, cNameItem_Search ] )
                   + LineEnding + fSimpleSearchSO.GetAllExpressions, true, true )
  else UpdateDisplay( format( cDoubleS + ':', [ cNameItem_SearchNormal, cNameItem_Search ] )
                      + LineEnding + fSearchSO.GetAllExpressions, true, true );
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
  begin
    if not lbSearchCmd.Focused then
      if lbSearchCmd.CanFocus then
      begin
        lbSearchCmd.SetFocus;
        lbSearchCmd.Click;
      end;
    RunCmdDisplayObj( lbSearchCmd );
  end else
  begin
    if not lbSearchCmdLine.Focused then
      if lbSearchCmdLine.CanFocus then
      begin
        lbSearchCmdLine.SetFocus;
        lbSearchCmdLine.Click;
      end;
    RunCmdDisplayObj( lbSearchCmdLine );
  end;


  //if lbSearchCmdLine.Items.Count = 0 then
  //  RunCmdDisplayObj( lbSearchCmd )
  //else RunCmdDisplayObj( lbSearchCmdLine );
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
  CmdRec : TCmdRec;
begin
//List changes made either from Master List handler or from within a command.
//either way all commands must be updated as appropriate
  if fSearchMayHaveChanged then
  begin
//for List changes to db to be seen, any opened cmd obj's need to be "free"'d to force them
//to load from the database again and/or refreshed appropriately based on their update status.
    for i := 0 to lbCommands.Items.Count - 1 do
    begin
      CmdRec := TCmdRec( lbCommands.Items.Objects[ i ] );
      if not Assigned( CmdRec.Cmd )
         or ( i = lbCommands.ItemIndex ) //the currently displayed command
         or CmdRec.Cmd.DoUpdate then
        continue;
      FreeCommand( i, false, false );
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
      Screen.Cursor := crHourglass;

    fKeyWordList.ApplyUpdateList( dbltKeyWord, lbCommands.Items, lbCommands.ItemIndex );

    if assigned( CmdObj ) then
      ApplyListChanges( CmdObj.KeyWords, lbKeyWords, lbKeyWordsDisp );

    if ModalResult = mrOk then
      ApplyListChangesDisplayedCmd( lbList, aListBox, DispListBox );

  finally
    Screen.Cursor := crDefault;
    Free;
  end;

end;

procedure TfrmMain.btnKeyWordAddClick(Sender: TObject);
begin
  ShowKeyWords( lblCommandName.Caption, lbKeywords, lbKeyWordsDisp );
  TryFocus( lbKeyWords );
//no more since easy to focus  ReFocus_Edit_Memo( true );
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
  TryFocus( lbKeyWords );

//no more since easy to focus  ReFocus_Edit_Memo( true );

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

      LoadSearchResults_Internal( SO, Strings, false );

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
                                                   lblCommandName.Caption );
    HelpParameter := edtHelp.Text;
    btnDefaultHelp.Caption := format( cclecapDefaultHelp, [ lblCommandName.Caption ] );
    btnPkexec.Enabled := fAllowPkexec and fAllowPkexecInstalled;

    memCmdLine.Lines.Text := Instr;
    fInternalComs := true;
    ShowModal;
    fInternalComs := false;
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
  lblFriendlyNameLineDisp.Caption := StrIf( edtFriendlyNameLine.Text <> '', DoubleQuotedString( edtFriendlyNameLine.Text ), dots );
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
  InPut, OutPut, Path : string;
begin

  if InvalidCommands then
  begin
    ShowMessage( cmsgNoCommandSelected );
    exit;
  end;

  try
    screen.Cursor := crHourglass;

    if CmdInPath then
      Input := trim( GetProperCmdNameCaption ) + ' '
    else
    begin
      Path := GetProperPathLabelCaption;
      Input := trim( strif( CmdObjHelper.HasRealPath( Path ), Path ) + GetProperCmdNameCaption ) + ' ';
    end;

    OutPut := '';
    if not EditCommandLine( format( cDoubleS, [ ccapGenericAdd, cNameItem_CommandLine ] ), Input, Output ) then
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

  //Idx := lbCommands.Items.AddObject('<newXcmd>', aCmdObj);
  Idx := lbCommands.Items.Add( '<newXcmd>' );
  lbCommands.Items.Objects[ Idx ] := TCmdRec.Create;
  TCmdRec( lbCommands.Items.Objects[ Idx ] ).Cmd := aCmdObj;
  aCmdObj := nil;

  lbCommands.ItemIndex := Idx;

  RefreshCmdObj;
  ClearCmdLineDisps;

  UpdateLbCommands(True);

  result := true;

end;


procedure TfrmMain.FreeCommand( const ItemIdx : integer; const DoBlank, DoFullClear : boolean );
var
  CmdRec : TCmdRec;
begin

  if DoBlank then
  begin
    BlankCmdLine;
    BlankCommand;
  end;

  CmdRec := TCmdRec( lbCommands.Items.Objects[ ItemIdx ] );
  if assigned( CmdRec.Cmd ) then
  begin
    CmdRec.Cmd.Free;
    CmdRec.Cmd := nil;
  end;
  CmdRec := nil;

  if DoFullClear then
  begin
    TCmdRec( lbCommands.Items.Objects[ ItemIdx ] ).Free;
    lbCommands.Items.Objects[ ItemIdx ] := nil;
////was in old NewIdx part
    //lbCommands.ItemIndex := NewIdx;
    //RefreshCmdObj;
  end;

end;

procedure TfrmMain.SetNotificationState( const TurnOn : boolean; Obj : TComponent; Shp : TShape );
begin
  if ( Obj is TAction ) then
    TAction( Obj ).Enabled := TurnOn
  else if ( Obj is TBitBtn ) then
    TBitBtn( Obj ).Enabled := TurnOn
    else raise exception.Create( 'Developer: bad object in TfrmMain.SetNotificationState' );

  if TurnOn then
    Shp.Brush.Color := $000000A0
  else Shp.Brush.Color := $A07300; // #0073A0

end;

procedure TfrmMain.UpdateSaveStatus;
var
  i: integer;
  aCmdObj: TCmdObj;
  NeedSave: boolean;
  CmdRec : TCmdRec;
begin

  NeedSave := False;

  for i := 0 to lbCommands.Items.Count - 1 do
  begin
    CmdRec := TCmdRec( lbCommands.Items.Objects[ i ] );
    if not Assigned( CmdRec.Cmd ) then
      continue;
    if CmdRec.Cmd.DoUpdate then
    begin
      NeedSave := True;
      break;
    end;
  end;

  SetNotificationState( NeedSave, actSave, shpSave );

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
  FreeCommand( lbCommands.ItemIndex, true, false );
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
  tsDetachedProcesses.Caption := ccapTabProcesses + lblDetachedProcesses.Caption + '  --|  ';
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
      if globltProcessMaxOutput > cLimitInfinityMaxCnt then
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

  if IsPkexec( RunStr ) and not ( fAllowPkexec and fAllowPkexecInstalled ) then
  begin
    result := RunStr + LineEnding + LineEnding
              + format( cmsgPkexecNotAllowed, [ trim( cprogPkexecStr ) ] )
              + LineEnding + LineEnding
              ;
    exit;
  end;

  LimitInfinity( RunStr );

  if ( Piped or UseShell )
     and ( ( pos( cReservedSuperUser, RunStr ) = 1 ) or IsPkexec( RunStr ) ) then
    MyShowMessage( cmsgROOTUseShell_Pipe, self );

  if not fInternalComs then
    UpdateDisplay_Internal( StandardOutputHeader( RunStr ), false );

//===>>> this  below MUST follow that above because RunStr is modified.
  if UseShell then
    RunStr := csoNonPrintingDelimiter + RunStr;

  try
    Screen.Cursor := crHourglass;

    tmrCancelRun.Enabled := true;

    aProcess := nil;
    fIsRunningProcess := true;

    result := ExtraInfo + CmdObjHelper.RouteCommand( RunStr, Detach, aProcess );

    if Detach and assigned( aProcess ) then
    begin
      UpdateDetachedProcesses( RunStr, aProcess );
      result := result + LineEnding + cltDetachedProcess;
      aProcess := nil;
    end;

  finally
    tmrCancelRun.Enabled := false;
    Screen.Cursor := crDefault;
    CloseCancelRun;
    fIsRunningProcess := false;
    globltProcessMaxOutput := LimitInfinityCnt;
  end;

end;

procedure TfrmMain.CloseCancelRun;
begin
  tmrCancelRunBlink.Enabled := false;
  btnCancelRun.Visible := false;
  shpSRL.Visible := false;
  shpSRR.Visible := false;
end;


procedure TfrmMain.actRunExecute( Sender: TObject );
var
  RunStr : string;
begin

  if not EditingCL then
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
                cbDetachProcessLine.Checked ),
                true,
                true
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
  popCmdLinesPopup( Self );

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
    TryFocus( memNotes )
  else TryFocus( memEntry );
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

//if commandoo is opened, form re-sized, then closed, there was bug where the
//editing panels had not obeyed their anchor settings and everything was off.
//a full settings of sizes seems to fix this.
  pnlCmdLine.top := shpCmdOut9.top;
  pnlCmdLine.Left := pnlDispCmdLine.Left;
  pnlCmdLine.Width :=  pnlDispCmdLine.Width;
  pnlCmdLine.Height :=  pnlCmdLines.Height;
  pnlCommand.top := pnlDispCommand.top;
  pnlCommand.Left := pnlDispCommand.Left;
  pnlCommand.Width := pnlDispCommand.Width;
  pnlCommand.Height := pnlDispCommand.Height;

  if not GetSpecialMode then
  begin
    Application.Terminate;
    exit;
  end;

  if fFirstLocalRun then
    UpdateDisplay( format( cmsgFirstLocalRun, [ cmsgFormHotKeys ] )
                   + LineEnding + LineEnding
                   + cmsgOwnRisk,
                   false,
                   false );

  UpdateDisplay( GetCurrentState, false, false );

  if SpecialComms <> '' then
    UpdateDisplay( trim( SpecialComms) + LineEnding, false, false );
  SpecialComms := '';

  RefreshCap;

  Memo1.SelStart := 1;

  TryFocus( lbCommands );

end;

function TfrmMain.GetCurrentState : string;
begin
  Result := DatetoStr( now ) + ' >>  ' + cmsgProfileStringCurrDB + lblCurrDB.Caption
    + LineEnding
    + cmsgMainBasePath + fWritingToPath
    + LineEnding
    + cmsgMainSavingPath + fSavingToPath
    + LineEnding
    + format( cmsgMainSavingPathSearch, [ cmsgMainSavingPath ] ) + GetSearchesDirectory
    + LineEnding
    + cmsgMainShellPhrase + globltShellName + '     ' +  cmsgMainRootTemplate + fRootFile
    + LineEnding
    ;
end;

function TfrmMain.GetWidgetString : string;
begin
  result := '';
  {$IFDEF WidgQT}
    result := '  [QT5]';
  {$ENDIF}
  {$IFDEF WidgGTK}
    result := '  [GTK2]';
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

  TryFocus( aMemo );
  with frmFindText do
  begin
    Memo := aMemo;
//Realized that a from cursor search in read only memos was very confusing so if its RO
//Alwasys start from the top, disabling the rgroup is a nicety.
    rgTopCursor.Enabled := not memo.ReadOnly;

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

procedure TfrmMain.JumpToCommand( anIdx : integer; CmdLineStr : string );
begin
  lbCommands.ItemIndex := anIdx;
  lbCommands.Click;
  if lbCmdLines.Items.Count = 0 then
    exit;
  anIdx := lbCmdLines.Items.IndexOf( CmdLineStr );
  if anIdx > -1 then
  begin
    lbCmdLines.ItemIndex := anIdx;
    lbCmdLines.Click;
  end;

end;

function TfrmMain.CanJumpToCommand( const CmdStr, CmdLineStr : string ) : boolean;
var
  Idx : Integer;
begin
  result := false;

  if CmdStr <> '' then
  begin

    Idx := lbCommands.Items.IndexOf( CmdStr );

    if Idx = -1 then
    begin
      Idx := TryToFindEditedCommand( CmdStr );
      if Idx = -1 then
      begin
        MessageDlg( format( cmsggfCommandNotFound, [ CmdStr ] ), mtInformation, [mbOK], 0);
        exit;
      end;
    end;

    JumpToCommand( Idx, CmdLineStr );
    result := true;

  end;

end;

function TfrmMain.TryToFindEditedCommand( const CmdSearch : string ) : integer;
var
  i : Integer;
  CmdRec : TCmdRec;
begin
  result := -1;
  for i := lbCommands.Items.Count - 1 downto 0 do
  begin
    CmdRec := TCmdRec( lbCommands.Items.Objects[ i ] );
    if not Assigned( CmdRec.Cmd ) then
      continue;
    if CmdRec.Cmd.DoUpdate then
    begin
 //find entries like "<UPDATE> gimp'
      if pos(' ' + CmdSearch, lbCommands.Items[ i ] ) > 0 then
      begin
        result := i;
        break;
      end;
    end;
  end;
end;


end.


{
cCommandInPathStr = '$PATH';
cLinuxBuiltInStr = '$BUILTIN';
cmsgcleBadPath = '$BAD_PATH';
  ¿∉∉∀⍫‡
        ↑
since I ALWAYS forget where the vk_ definiations are:
, lcltype //THis is needed for key up keyboard constants

tags / hints / anchors / events / enabled&visible / modalresults / popup menus

Cleanup checks and other notes
end ;      autoformat problem
) ;        autoformat problem


lit strings
Shortcuts
exceptions testing

compiler warnings
po files

check T O D O 'S
Didn't you know a way to send email from a button setting subject??
fresh install with and without data

 }
