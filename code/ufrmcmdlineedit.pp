unit ufrmcmdlineedit;

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
  , unitsharedobj, Buttons, StdCtrls
  , unitGlobForm, ExtCtrls
  , ufrmFindText
  ;

type

  { TfrmCmdLineEdit }

  TfrmCmdLineEdit = class(TForm)
    btnCancel : TBitBtn;
    bntOK : TBitBtn;
    btnDefaultHelp : TBitBtn;
    btnPkexec : TBitBtn;
    btnRefresh : TBitBtn;
    btnVarDecimal : TBitBtn;
    btnVarFilename : TBitBtn;
    btnPath : TBitBtn;
    btnVarInteger : TBitBtn;
    btnVarString : TBitBtn;
    cbWrapText : TCheckBox;
    edtOutputRefresh : TEdit;
    FrameHint1 : TFrameHint;
    lblVarHint : TLabel;
    memCmdLine : TMemo;
    memOutput : TMemo;
    procedure bntOKClick(Sender : TObject);
    procedure btnCancelClick(Sender : TObject);
    procedure btnDefaultHelpClick(Sender : TObject);
    procedure btnPathClick( Sender : TObject );
    procedure btnPkexecClick( Sender : TObject );
    procedure btnRefreshClick(Sender : TObject);
    procedure btnVarStringClick( Sender : TObject );
    procedure cbWrapTextChange( Sender : TObject );
    procedure edtOutputRefreshKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure FormActivate(Sender : TObject);
    procedure FormClose(Sender : TObject; var CloseAction : TCloseAction);
    procedure FormCreate(Sender : TObject);
    procedure FormKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure FormShow(Sender : TObject);
    procedure memCmdLineKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
  private
    fEditPosFix : integer;
    { private declarations }
    //FCanClose : boolean;
    FHasShown : boolean;
    fhelpCommand : string;
    fHelpParameter : string;
    FIsInitialized : boolean;
    fFindText : TfrmFindText;
    procedure DoOutput;
    procedure FindInMemo;
    procedure HandleFormSettings( const TheType : TSettingsDirective );
    procedure SethelpCommand( const AValue : string );
    procedure SetHelpParameter( const AValue : string );
    procedure ShowInitialHelp;
    procedure UpdateMemOutPut( const ToAdd : string );
  public
    { public declarations }

    property helpCommand : string read fhelpCommand write SethelpCommand;
    property HelpParameter : string read fHelpParameter write SetHelpParameter;
  end;

var
  frmCmdLineEdit : TfrmCmdLineEdit;

resourcestring
  cclecapDefaultHelp = 'Show Help:  %s';

implementation

{$R *.lfm}

uses ufrmMsgDlg
     , linuxtrix
     , unitcommands
     , ufrmMain
     , juusgen
     , strconst_en
     , strconst_prog
     , unitglob
     ;

resourcestring
  cmsgcleNoEmpties = 'This cannot be empty';
  cmsgCLEVariableAlpha = 'AlphaNumeric (string) text';
  cmsgCLEVariableInt = 'an integer';
  cmsgCLEVariableDecimal = 'a decimal number';
  cmsgCLEVariableFileName = 'a File Name';

  cmsgcleNoNaked = 'May not run "naked" commands from here.';
  //cmsgcleOnlyOneParam = 'Only one param / flag is allowed from here.';
  cmsgcleOnlyHelp = LineEnding + LineEnding
                    + 'For safety reason only help commands (CMD --help or CMD -h ) '
                    + LineEnding
                    + 'can be run from here';

  cmsgCLEVariable =
    'Adds a variable for %s at '
    + LineEnding
    + 'the cursor or selected text. '
    + LineEnding + LineEnding
    + 'At run time you can fill in the information. '
    + LineEnding + LineEnding
    + 'NOTE : In the case of Filenames (<<F>>) quotes '
    + LineEnding
    + 'will automatically be added around the filename, '
    + LineEnding
    + 'no need to add quotes to your command line. This, '
    + LineEnding
    + 'of course, is necessary for filenames that have spaces '
    + LineEnding
    + 'in them and is taken care of for you. '
    + LineEnding + LineEnding
    + '<end>'
    + LineEnding
    ;

  cmsgCLENoCommandSpecified = 'No command specified.';

const
  cOutputSeparator =
   LineEnding + LineEnding
   + '==============================================='
   + LineEnding
   + 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
   + LineEnding
   + '==============================================='
   + LineEnding + LineEnding
   ;

  cArrowKeymemCmdLine       = 500000;
  cArrowKeyedtOutputRefresh = 500001;
  cArrowKeymemOutput        = 500002;

{ TfrmCmdLineEdit }

procedure TfrmCmdLineEdit.UpdateMemOutPut( const ToAdd : string );
var
  GoToPos : Integer;
  NumLines : Int64;
begin

  GoToPos := length( memOutput.Lines.Text );

  memOutput.Lines.Text := memOutput.Lines.Text
                         + LineEnding
                         + ToAdd
                         + cOutputSeparator;

  memOutput.SelStart := GoToPos + 1;
  //memOutput.SetFocus;

  NumLines := trunc( memOutput.Height / memOutput.Font.GetTextHeight( 'X' ) );
  if memOutput.CaretPos.Y > NumLines div 2 then
    memOutput.ScrollBy( 0, trunc( memOutput.Height * 0.7 ) * -1 );

end;

procedure TfrmCmdLineEdit.DoOutput;
var
  executeStr, CheckStr: string;
  Idx : SizeInt;
begin

  executeStr := trim( edtOutputRefresh.Text );

  if executeStr = '' then
  begin
    updateMemOutPut( cmsgCLENoCommandSpecified );
    exit;
  end;

//intended for help with other commands, running naked CL's from here is not safe.
  Idx := pos( ' ', executeStr );
  if Idx = 0 then
  begin
    updateMemOutPut( cmsgcleNoNaked + cmsgcleOnlyHelp );
    exit;
  end else
  begin
    CheckStr := trim( copy( executeStr, Idx + 1, MAXINT ) );
//there are some forms of help that require two params
    //if pos( ' ', CheckStr ) > 0  then//More than one param
    //begin
    //  updateMemOutPut( cmsgcleOnlyOneParam + cmsgcleOnlyHelp );
    //  exit;
    //end;
  end;

  if ( CheckStr = '-h' )
     or ( pos( '-h ', CheckStr ) = 1 )
     or ( pos( 'help ', CheckStr ) = 1 )
     or ( pos( '--help ', CheckStr ) = 1 ) then
    UpdateMemOutPut( TfrmMain( Owner ).RunCmdLineExternal( executeStr ) )
  else updateMemOutPut( cmsgcleOnlyHelp );

end;

procedure TfrmCmdLineEdit.ShowInitialHelp;
var
  SL : TStringList;
begin
  if trim( HelpParameter ) = '' then
  begin
    UpdateMemOutPut( format( cmsgcleNoHelpParam, [ helpCommand ] ) );
    exit;
  end;

  SL := TStringList.Create;
  try
    SL.Add( helpCommand );
    SL.Add( HelpParameter );
    UpdateMemOutPut( TfrmMain( Owner ).RunExternalHelpRequest( SL ) );
  finally
    SL.free;
  end;
end;

procedure TfrmCmdLineEdit.FormShow(Sender : TObject);
begin

  if not FHasShown then
  begin
    HandleFormSettings( sdLoad );
    FHasShown := true;

    btnVarString.Hint := format( cmsgCLEVariable, [ cmsgCLEVariableAlpha ] );
    btnVarInteger.Hint := format( cmsgCLEVariable, [ cmsgCLEVariableInt ] );
    btnVarDecimal.Hint := format( cmsgCLEVariable, [ cmsgCLEVariableDecimal ] );
    btnVarFilename.Hint := format( cmsgCLEVariable, [ cmsgCLEVariableFileName ] );
    FrameHint1.cbHints.Caption := ccbHintsEnglishOverride;
  end;

end;

procedure TfrmCmdLineEdit.memCmdLineKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  if key = vk_return then
  begin
    bntOK.Click;
    key := vk_unknown;
  end;
end;

procedure TfrmCmdLineEdit.HandleFormSettings( const TheType : TSettingsDirective );
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
        FormSettings.AddSetting( BoolToStr( cbWrapText.Checked ) );

        FormSettings.SaveFormSettings( Self.Name );
      end;
    sdLoad :
      While FormSettings.ReadFormSettings( Self.Name, i, theValue ) do
      case i of
        0 : Height := strtoint( theValue );
        1 : Width := strtoint( theValue );
        2 : cbWrapText.Checked := StrtoBool( theValue );
      end;
  end;

end;

procedure TfrmCmdLineEdit.SethelpCommand( const AValue : string );
begin
  if fhelpCommand = AValue then Exit;
  fhelpCommand := AValue;
end;

procedure TfrmCmdLineEdit.SetHelpParameter( const AValue : string );
begin
  if fHelpParameter = AValue then Exit;
  fHelpParameter := AValue;
end;

procedure TfrmCmdLineEdit.FormClose(Sender : TObject; var CloseAction : TCloseAction);
begin
  HandleFormSettings( sdSave );
  if assigned( fFindText ) then
  begin
    fFindText.Close;
    freeandnil( fFindtext );
  end;
end;

procedure TfrmCmdLineEdit.bntOKClick(Sender : TObject);
begin
  if trim( memCmdLine.Lines.Text ) = '' then
  begin
    memCmdLine.Lines.Text := cmsgcleNoEmpties;
    memCmdLine.SetFocus;
    exit;
  end;
  Modalresult := mrOK;
end;

procedure TfrmCmdLineEdit.btnCancelClick(Sender : TObject);
begin
  //FCanClose := true;
  Modalresult := mrCancel;
end;

procedure TfrmCmdLineEdit.btnDefaultHelpClick(Sender : TObject);
begin
  ShowInitialHelp;
end;

procedure TfrmCmdLineEdit.btnPathClick( Sender : TObject );
var
  str : string;
begin
  str := TfrmMain( Owner ).GetRealPath;
  if str <> '' then
    memCmdLine.Text := str + memCmdLine.Text;
end;

procedure TfrmCmdLineEdit.btnPkexecClick( Sender : TObject );
//var
//  Idx : integer;
begin
  memCmdLine.Text := TogglePkexec( memCmdLine.Text );
  //Idx := pos( cprogPkexecStr + ' ', memCmdLine.Text );
  //if Idx = 0 then
  //  memCmdLine.Text := cprogPkexecStr + ' ' + memCmdLine.Text
  //else memCmdLine.Text := copy( memCmdLine.Text, length( cprogPkexecStr ) + 2, maxint );
end;

procedure TfrmCmdLineEdit.btnRefreshClick(Sender : TObject);
begin
  btnDefaultHelp.Enabled := true;
  DoOutput;
end;


procedure TfrmCmdLineEdit.btnVarStringClick( Sender : TObject );
begin

  fEditPosFix := memCmdLine.SelStart + cucoVariableSize;
  case TBitBtn( Sender ).Tag of
    1 : memCmdLine.SelText := cucoVariableString;
    2 : memCmdLine.SelText := cucoVariableInteger;
    3 : memCmdLine.SelText := cucoVariableNumber;
    4 : memCmdLine.SelText := cucoVariableFile;
  end;
  memCmdLine.SetFocus;

  memCmdLine.SelLength := 0;
  memCmdLine.SelStart := fEditPosFix;

end;

procedure TfrmCmdLineEdit.cbWrapTextChange( Sender : TObject );
begin
  memOutput.WordWrap := cbWrapText.Checked;
end;

procedure TfrmCmdLineEdit.edtOutputRefreshKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  if key = vk_return then
  begin
    key := VK_UNKNOWN;
    btnRefresh.Click;
  end;
end;

procedure TfrmCmdLineEdit.FormActivate(Sender : TObject);
begin
  if not FIsInitialized then
  begin
    memCmdLine.SelLength := 0;
    memCmdLine.SelStart := length( memCmdLine.Lines.Text );
    memCmdLine.SetFocus;
    ShowInitialHelp;
    FIsInitialized := true;
    btnPkexec.Hint := cPkexecHint;
  end;

end;

procedure TfrmCmdLineEdit.FormCreate(Sender : TObject);
begin
  font.size := cDefaultFontSize;
  ApplyChangeFont( Self );
  btnPath.Hint := chintInsertFilePaths;
  btnPath.Caption := '&A  ' + ccapInsertFilePaths;
  FHasShown := false;
  FIsInitialized := false;
  fFindText := nil;
  memCmdLine.Tag := cArrowKeymemCmdLine;
  edtOutputRefresh.Tag := cArrowKeyedtOutputRefresh;
  memOutput.Tag := cArrowKeymemOutput;

end;

procedure TfrmCmdLineEdit.FormKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  if Shift = [ ssShift, ssCtrl ] then
  case key of
    VK_LEFT, VK_RIGHT :
      if assigned( self.ActiveControl ) then
      begin
        case self.ActiveControl.Tag of
          cArrowKeymemCmdLine :
            case Key of
              VK_RIGHT : TryFocus( edtOutputRefresh );
              VK_LEFT : TryFocus( memOutput );
            end;
          cArrowKeyedtOutputRefresh :
            case Key of
              VK_RIGHT : TryFocus( memOutput );
              VK_LEFT : TryFocus( memCmdLine );
            end;
          cArrowKeymemOutput :
            case Key of
              VK_RIGHT : TryFocus( memCmdLine );
              VK_LEFT : TryFocus( edtOutputRefresh );
            end;
          else TryFocus( memCmdLine );
        end;
        Key := VK_UNKNOWN;
      end;
    VK_F : FindInMemo;
    VK_L : fFindText.ReFindinMemo( memOutput );
  end;
end;

procedure TfrmCmdLineEdit.FindInMemo;
var
  PosPt : TPoint;
begin
  if not assigned( fFindText ) then
  begin
    fFindText := TfrmFindText.Create( self );
    fFindText.UpdateCaptions;
    fFindText.Memo := memOutput;
  end;
  PosPt := GetPreciseControlCoords( MemOutput, MemOutput.Width - fFindText.Width - 50, 25 );
  fFindText.Top := PosPt.y;
  fFindText.Left := PosPt.x;
  fFindText.Show;
end;

end.

