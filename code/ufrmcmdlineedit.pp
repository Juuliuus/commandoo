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
  ;

type

  { TfrmCmdLineEdit }

  TfrmCmdLineEdit = class(TForm)
    btnCancel : TBitBtn;
    bntOK : TBitBtn;
    btnVarFilename : TButton;
    btnRefresh : TButton;
    btnDefaultHelp : TButton;
    btnVarDecimal : TButton;
    btnVarString : TButton;
    btnVarInteger : TButton;
    cbWrapText : TCheckBox;
    edtOutputRefresh : TEdit;
    FrameHint1 : TFrameHint;
    lblVarHint : TLabel;
    memCmdLine : TMemo;
    memOutput : TMemo;
    procedure bntOKClick(Sender : TObject);
    procedure btnCancelClick(Sender : TObject);
    procedure btnDefaultHelpClick(Sender : TObject);
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
     , ufrmFindText
     , juusgen
     ;

resourcestring
  cmsgcleNoEmpties = 'This cannot be empty';
  cmsgCLEVariableAlpha = 'AlphaNumeric (string) text';
  cmsgCLEVariableInt = 'an integer';
  cmsgCLEVariableDecimal = 'a decimal number';
  cmsgCLEVariableFileName = 'a File Name';

  cmsgcleNoNaked = 'May not run "naked" commands from here.';
  cmsgcleOnlyOneParam = 'Only one param / flag is allowed from here.';
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

{ TfrmCmdLineEdit }

procedure TfrmCmdLineEdit.UpdateMemOutPut( const ToAdd : string );
var
  GoToPos : Integer;
{$IFDEF Bit32}
  NumLines : Integer;
{$ELSE}
  NumLines : Int64;
{$ENDIF}
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
    if pos( ' ', CheckStr ) > 0  then//More than one param
    begin
      updateMemOutPut( cmsgcleOnlyOneParam + cmsgcleOnlyHelp );
      exit;
    end;
  end;

  if ( CheckStr = '-h' ) or ( pos( ' --help', executeStr ) > 0 ) then
    UpdateMemOutPut( TfrmMain( Owner ).RunCmdLineExternal( executeStr ) )
  else updateMemOutPut( cmsgcleOnlyHelp );

end;

procedure TfrmCmdLineEdit.ShowInitialHelp;
var
  SL : TStringList;
begin
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

  end;

end;

procedure TfrmCmdLineEdit.memCmdLineKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  if key = vk_return then
    key := vk_unknown;
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

procedure TfrmCmdLineEdit.btnRefreshClick(Sender : TObject);
begin
  btnDefaultHelp.Enabled := true;
  DoOutput;
end;


procedure TfrmCmdLineEdit.btnVarStringClick( Sender : TObject );
begin

  fEditPosFix := memCmdLine.SelStart + cucoVariableSize;
  case TButton( Sender ).Tag of
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
  end;

end;

procedure TfrmCmdLineEdit.FormCreate(Sender : TObject);
begin
  ApplyChangeFont( Self );
  FHasShown := false;
  FIsInitialized := false;
end;

procedure TfrmCmdLineEdit.FormKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  if ( ssCtrl in Shift ) and not ( ssAlt in Shift ) then
  begin
    if key = VK_F then
    begin
      if ssShift in Shift then
        frmFindText.ReFindinMemo( memOutput )
      else FindInMemo;
    end;
    if key = VK_L then
      frmFindText.ReFindinMemo( memOutput );
  end;
end;

procedure TfrmCmdLineEdit.FindInMemo;
var
  PosPt : TPoint;
begin
  with frmFindText do
  begin
    Memo := memOutput;
    PosPt := GetPreciseControlCoords( Memo, MemOutput.Width - frmFindText.Width - 50, 25 );
    Top := PosPt.y;
    Left := PosPt.x;
    Show;
  end;
end;

end.

