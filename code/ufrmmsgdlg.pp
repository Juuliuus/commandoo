unit ufrmMsgdlg;

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
  Classes, SysUtils, LazFileUtils,
  Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls, Clipbrd, unitsharedobj
  , unitGlobForm
  , Jinifiles
  ;

type

  TmdpType = (mdpNone, mdpInformation, mdpWarning, mdpAttention, mdpConfirmation);

  { TMsgDLgParams }

  TMsgDLgParams = class(TObject)
  private
    fIsInitialized : boolean;
    fSectTabName : string;

    fMode: TmdpType;
    fIsConfirmDlg: boolean;
    fMsg: string;
    fMsgCaption: string;
    fShow_NoMoreMsg: boolean;
    fTheShowNoMoreKey : string;
    fDoNotShowList : TStringList;
    fIniFile : TJiniFile;

    function Load : boolean;
    procedure SetDoNotShowList(AValue : TStringlist);
    procedure SetIniFile( AValue : TJiniFile );
    procedure SetIsInitialized(AValue : boolean);
    procedure DoTheReset;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ResetDoNotShowList(DontUpdateDB : boolean = false);

    property IsInitialized : boolean read FIsInitialized write SetIsInitialized;
    property DoNotShowList : TStringlist read FDoNotShowList;// write SetDoNotShowList;
    property IniFile : TJiniFile read fIniFile write SetIniFile;
  end;


  { TfrmMsgDlg }

  TfrmMsgDlg = class(TForm)
    btnOk: TBitBtn;
    btnNo: TBitBtn;
    btnYes: TBitBtn;
    btnClip : TButton;
    cbDoNotShow : TCheckBox;
    lblType: TLabel;
    memMsg: TMemo;
    pnlGTK : TPanel;
    pnlNoShow : TPanel;
    Timer1: TTimer;
    procedure btnClipClick(Sender : TObject);
    procedure btnNoClick(Sender: TObject);
    procedure btnYesClick(Sender: TObject);
    procedure FormClose(Sender : TObject; var CloseAction : TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate( Sender : TObject );
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    FdoBlink: boolean;
    FCanClose: boolean;
    procedure HandleFormSettings( const TheType : TSettingsDirective );
    procedure InitForm;
    procedure SetupConfirm( const IsConfirm : boolean );
  public
    { public declarations }
  end;


  procedure InitMsgDlgParams( const SectionOrTableName : string; anIniFile : TJinifile );

  function MsgDlgMessage( const ACaption, AMessage : string; ShowNoMoreKey : string = ''  ) : boolean;
  function MsgDlg( const TheType : TmdpType; const IsConfirmDialog : boolean; AOwner : TForm ) : TModalResult;

  function MsgDlgInfo( AOwner : TForm ) : TModalResult;
  function MsgDlgConfirmation( AOwner : TForm ) : TModalResult;

  function MsgDlgWarn( AOwner : TForm ) : TModalResult;
  function MsgDlgWarnConfirm( AOwner : TForm ) : TModalResult;

  function MsgDlgAttention( AOwner : TForm ) : TModalResult;
  function MsgDlgAttentionConfirm( AOwner : TForm ) : TModalResult;

  procedure MyShowmessage( const Msg : string; aForm : TForm );



var
  frmMsgDlg: TfrmMsgDlg;
  MsgDlgParams : TMsgDlgParams;


implementation

{$R *.lfm}

uses  juusgen
   ;
const
  cForgot = '<< YOU FORGOT TO SET THE %s >>';
  cForgotHintMsg = 'MESSAGE (FMsg)';
  cForgotHintMsgCap = 'FORM CAPTION (FMsgCaption)';
  cTableColumnEmpty = 'Hey Programmer! InitMsgDlgParams was not called a Tablename and/or ColumnName.';

resourcestring
  { TMsgDLgParams }
  cMsgDlgWarning = '<< Warning >>';
  cMsgDlgInformation = 'Information';
  cMsgDlgAttention = '<< == Attention! == >>';
  cMsgConfirmation = '= Confirm =';
  cUndefined = '== U N D E F I N E D ==';
  cMsgDlgGeneralInformation = 'General Information';

{

call InitMsgDlgParams( NoShowList : TStringList ) from your main form create, activate or whatever
The list is a list owned by the main form that will keep track of no shows and that is written
out to some saved format and read in at startup.

then:

MsgDlgMessage ( 'a caption', 'a message', 'SomeuniqueTextifYOuwantShowNoMoreCapability' )
MsgDlg( mdpInformation, false, someTForm )

if it is a confirm message and has the potential to break out of loops and
can be set to show no more by user then check the result from MsgDlgMessage first:

if MsgDlgMessage ( 'a caption', 'a message', 'SomeuniqueTextifYOuwantShowNoMoreCapability' ) then
  if MsgDlg( mdpInformation, true, someTForm ) = mrNo then
    exit;

}


function NotInitialized : boolean;
begin
  Result := false;
  if not MsgDLgParams.IsInitialized then
  begin
    ShowMessage( 'Hey Programmer! MsgDlgParams is not initialized! Use InitMsgDlgParams in your form create with a stringlist that will hold the values' );
    Result := true;
  end;
end;

function MsgDlgWillShow : boolean;
begin
  result := false;

  if NotInitialized then
    exit;

  if ( MsgDLgParams.fTheShowNoMoreKey <> '' )
      and ( MsgDlgParams.DoNotShowList.IndexOf( MsgDLgParams.fTheShowNoMoreKey ) > -1 ) then
//this constant for the message has been listed as do not show anymore
    exit;

  result := true;

end;

procedure InitMsgDlgParams( const SectionOrTableName : string; anIniFile : TJinifile );
begin
  if MsgDLgParams.IsInitialized then
  begin
    Showmessage( 'Hey Programmer! InitMsgDlgParams should only be called once and, generally, only from Main Form.' );
    exit;
  end;

  MsgDLgParams.DoTheReset;

  MsgDLgParams.fSectTabName := SectionOrTableName;

  MsgDLgParams.IniFile := anIniFile;

  if not MsgDLgParams.Load then
    exit;

  MsgDLgParams.IsInitialized := true;
end;

function MsgDlgMessage( const ACaption, AMessage : string; ShowNoMoreKey : string = '' ) : boolean;
begin

  result := false;

  //If ShowNoMoreKey is "" then the "Don't show this message" is not shown
  //If not blank it must be unique to faciliate the show no more functionality
  //best is to simply send the literal ACaption surrounded by quotes and the
  //ACaption should be a resourcestring, in this way uniqueness is guaranteed
  //eg.: string msgMyCaption = 'My Caption' is sent in as "msgMyCaption"
  //see example at end of this file
  MsgDLgParams.fMsg:= AMessage;
  MsgDLgParams.fMsgCaption:= strif( ACaption = '', cMsgDlgInformation, ACaption );
  MsgDLgParams.fTheShowNoMoreKey:= trim( ShowNoMoreKey );
  MsgDLgParams.fShow_NoMoreMsg := MsgDLgParams.fTheShowNoMoreKey <> '';

  Result := MsgDlgWillShow;

  //because some confirmation questions can break out of loops need know beforehand if
  //they will show. Otherwise you can't be sure of a yes or no result.
  //you would have to check that the result is not equal to mrNone then you can
  //check if the result is mrNo or mrYes. This is a pain.
  //
  //So for those situations, confirmation only and can break loops or perform exits etc
  //you need to run this first before running MsgDlg.
  //example:
  //
  // if MsgDlgMessage( params ) then
  //   if MsgDlg( params ) = mrNo then
  //     exit;
  //
  //if you don't use the result MsgDlgMessage then you need a construction like this:
  //  MsgDlgMessage( params )
  //  modalres := MsgDlg();
  //  if modalres <> mrNone then
  //    if modalres = mrNo then
  //      exit;
  //
  //the first is clearer and simpler IMO but do as you like, just be aware of this

end;


function MsgDlg( const TheType : TmdpType; const IsConfirmDialog : boolean; AOwner : TForm ) : TModalResult;
begin

  Result := mrNone;

  if not MsgDlgWillShow then
    exit;

  MsgDlgParams.fMode := TheType;
  MsgDlgParams.fIsConfirmDlg := IsConfirmDialog;

  with TfrmMsgDlg.Create( AOwner ) do
  try

    if AOwner = nil then
      Position := poScreenCenter;

    Showmodal;
    Result := ModalResult;

    if cbDoNotShow.Checked then
    begin
      MsgDlgParams.DoNotShowList.Add( MsgDLgParams.fTheShowNoMoreKey );

      MsgDLgParams.IniFile.WriteString(
                              MsgDLgParams.fSectTabName,
                              MsgDLgParams.fTheShowNoMoreKey,
                              MsgDLgParams.fTheShowNoMoreKey );
      MsgDLgParams.IniFile.UpdateFile;
    end;

    MsgDlgParams.DoTheReset;

  finally
    Free;
  end;

end;

function MsgDlgInfo( AOwner : TForm) : TModalResult;
begin
  result := MsgDlg( mdpInformation, false, AOwner );
end;

function MsgDlgWarn( AOwner : TForm) : TModalResult;
begin
  result := MsgDlg( mdpWarning, false, AOwner );
end;

function MsgDlgWarnConfirm( AOwner : TForm) : TModalResult;
begin
  result := MsgDlg( mdpWarning, true, AOwner );
end;

function MsgDlgAttention( AOwner : TForm) : TModalResult;
begin
  result := MsgDlg( mdpAttention, false, AOwner );
end;

procedure MyShowmessage( const Msg : string; aForm : TForm );
begin
  MsgDlgMessage( cMsgDlgGeneralInformation, Msg );
  MsgDlgInfo( aForm );
end;


function MsgDlgAttentionConfirm( AOwner : TForm) : TModalResult;
begin
  result := MsgDlg( mdpAttention, true, AOwner );
end;

function MsgDlgConfirmation( AOwner : TForm) : TModalResult;
begin
  result := MsgDlg( mdpConfirmation, true, AOwner );
end;

{ TMsgDLgParams }


procedure TMsgDLgParams.DoTheReset;
begin

  fMode := mdpNone;

  fIsConfirmDlg := False;
  fMsg := format(cForgot, [cForgotHintMsg]);
  fMsgCaption := format(cForgot, [cForgotHintMsgCap]);
  fShow_NoMoreMsg := False;
  fTheShowNoMoreKey := '';

end;

procedure TMsgDLgParams.SetIsInitialized(AValue : boolean);
begin
  if not FIsInitialized and ( AValue = true ) then
    FIsInitialized := True;
end;

procedure TMsgDLgParams.SetDoNotShowList(AValue : TStringlist);
begin
  if FDoNotShowList = AValue then Exit;
  FDoNotShowList := AValue;
end;

procedure TMsgDLgParams.SetIniFile( AValue : TJiniFile );
begin
  if fIniFile = AValue then Exit;
  fIniFile := AValue;
end;


constructor TMsgDLgParams.Create;
begin
  inherited;
  FIsInitialized := false;
  FDoNotShowList := TStringList.Create;
  fSectTabName := '';
  //fColName := '';
  DoTheReset;
end;

destructor TMsgDLgParams.Destroy;
begin
  if assigned( FDoNotShowList ) then
    FreeAndNil( FDoNotShowList );
  inherited Destroy;
end;

procedure TMsgDLgParams.ResetDoNotShowList( DontUpdateDB : boolean = false );
begin

  fDoNotShowList.Clear;

//added mostly because I don't want database updated if it is superuser running
  if DontUpdateDB then
    exit;

  if DontRun( ( MsgDLgParams.fSectTabName = '' ), cTableColumnEmpty ) then
    exit;

  IniFile.EraseSection( fSectTabName );
  IniFile.UpdateFile;

end;

function TMsgDLgParams.Load : boolean;
begin

  result := false;

  if DontRun( ( MsgDLgParams.fSectTabName = '' ), cTableColumnEmpty ) then
    exit;

  result := IniFile.LoadList( fSectTabName,
                              fDoNotShowList );
end;

{ TfrmMsgDlg }


procedure TfrmMsgDlg.SetupConfirm( const IsConfirm : boolean );
begin

  if not IsConfirm then
    exit;

  btnOk.Enabled := False;
  btnOk.Visible := False;

  btnNo.Enabled := True;
  btnNo.Visible := True;
  btnYes.Enabled := True;
  btnYes.Visible := True;
end;

procedure TfrmMsgDlg.FormShow(Sender: TObject);
begin

  HandleFormSettings( sdLoad );

  FCanClose := False;
  if not Assigned( MsgDlgParams ) then
  begin
    ShowMessage('Hey Programmer! MsgDlg "Params" is not assigned (should never be)');
    FCanClose := true;
    Close;
    exit;
  end;

  InitForm;

  if btnNo.Enabled then
    ActiveControl := btnNo
  else
    ActiveControl := btnOk;

  Timer1.Enabled := True;

end;

procedure TfrmMsgDlg.btnNoClick(Sender: TObject);
begin
  FCanClose := True;
  ModalResult := mrNo;
end;

procedure TfrmMsgDlg.btnClipClick(Sender : TObject);
begin
  Clipboard.AsText := memMsg.Text;
end;

procedure TfrmMsgDlg.btnYesClick(Sender: TObject);
begin
  FCanClose := True;
  ModalResult := mrYes;
end;

procedure TfrmMsgDlg.FormClose( Sender : TObject; var CloseAction : TCloseAction );
begin
  HandleFormSettings( sdSave );
end;

procedure TfrmMsgDlg.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := FCanClose;
end;

procedure TfrmMsgDlg.FormCreate( Sender : TObject );
begin
  ApplyChangeFont( Self );
end;

procedure TfrmMsgDlg.Timer1Timer(Sender: TObject);

  procedure SetEndState;
  begin
    Timer1.Enabled := False;
    lblType.Visible := True;
    lblType.Invalidate;
  end;

begin

  if not FdoBlink then
  begin
    SetEndState;
    exit;
  end;

  if lblType.visible then
  begin
    lblType.visible := false;
    Timer1.Interval := Timer1.Interval + 25;
  end
  else
  begin
    lblType.Visible := true;
    Timer1.Interval := Timer1.Interval + 100;
  end;

  lblType.Invalidate;
  if Timer1.Interval >= 1200 then
    SetEndState;

end;


procedure TfrmMsgDlg.InitForm;
begin

  with MsgDlgParams do
  begin
    Caption := fMsgCaption;

    memMsg.Text := fMsg;
    memMsg.SelStart := 0;

    FDoBlink := true;

    case fMode of
      mdpInformation:
      begin
        lblType.Caption := cMsgDlgInformation;
        FDoBlink := False;
      end;
      mdpWarning: lblType.Caption := cMsgDlgWarning;
      mdpAttention: lblType.Caption := cMsgDlgAttention;
      mdpConfirmation:
        begin
          lblType.Caption := cMsgConfirmation;
          FDoBlink := False;
        end
      else
        lblType.Caption := cUndefined;
    end;

    pnlNoShow.Visible := fShow_NoMoreMsg;
    //cbDoNotShow.Visible := fShow_NoMoreMsg;

    SetUpConfirm( fIsConfirmDlg );
    FCanClose := not fIsConfirmDlg;

    if btnNo.Enabled then
      ActiveControl := btnNo
    else
      ActiveControl := btnOk;

  end;

end;

procedure TfrmMsgDlg.HandleFormSettings( const TheType : TSettingsDirective );
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



initialization

  MsgDlgParams := TMsgDlgParams.Create;

finalization

 if assigned( MsgDlgParams ) then
   FreeAndNil( MsgDlgParams );

end.


{

resoucestring
  capMyMessage = 'This is my caption';
  msgMyMessage = '...And this is the message';

implementation

//this will show the "Show this message no more" checkbox
//and it will be shown or not shown keyed on the string 'capMyMessage'
  MsgDlgMessage( capMyMessage, msgMyMessage, 'capMyMessage'  );

//this will show NOT the "Show this message no more" checkbox
//that is, this message will always be shown, the user cannot turn it off
  MsgDlgMessage( capMyMessage, msgMyMessage, ''  );

}
