unit ufrmListManagerConsolidate;

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
  , unitsharedobj, Buttons
  , unitGlobForm, StdCtrls
  ;

type

  { TfrmListManagerConsolidate }

  TfrmListManagerConsolidate = class(TForm)
    btnCancel : TBitBtn;
    bntOK : TBitBtn;
    cbChoose : TComboBox;
    FrameHint1 : TFrameHint;
    lblInfo : TLabel;
    lblItem : TLabel;
    Label3 : TLabel;
    procedure bntOKClick(Sender : TObject);
    procedure btnCancelClick(Sender : TObject);
    procedure FormActivate(Sender : TObject);
    procedure FormClose(Sender : TObject; var CloseAction : TCloseAction);
    procedure FormCloseQuery(Sender : TObject; var CanClose : boolean);
    procedure FormCreate(Sender : TObject);
    procedure FormShow(Sender : TObject);
  private
    { private declarations }
    FCanClose : boolean;
    fDisplayWord : string;
    FHasShown : boolean;
    FIsInitialized : boolean;
    procedure HandleFormSettings( const TheType : TSettingsDirective );
    procedure SetDisplayWord( const AValue : string );
  public
    { public declarations }
    property DisplayWord : string read fDisplayWord write SetDisplayWord;
  end;

var
  frmListManagerConsolidate : TfrmListManagerConsolidate;

implementation

{$R *.lfm}

uses ufrmMsgDlg, strconst_en, unitglob;

resourcestring
  cmsglmcNoSelection = 'You have not chosen a %s to consolidate to.';
  cmsgLMC =
    'When you press OK the old Item disappears '
    + LineEnding
    + 'and all instances of it are changed to the newly '
    + LineEnding
    + 'chosen Item. '
    ;


  cmsgLMCLabel =
    'This is the List Item you want to consolidate '
    + LineEnding
    + '(combine into) another List Item. '
    + LineEnding + LineEnding
    + 'You choose the List Item to consolidate to '
    + LineEnding
    + 'from the combobox below. '
    + LineEnding + LineEnding
    + '%s '
    + LineEnding + LineEnding
    + '<end.> '
    ;

  cmsgLMCComboBox =
    'This is the List Item you want to consolidate '
    + LineEnding
    + 'TO. '
    + LineEnding + LineEnding
    + 'The List Item shown above will be consolidated '
    + LineEnding
    + 'to this List Item. '
    + LineEnding + LineEnding
    + '%s '
    + LineEnding + LineEnding
    + '<end.> '
    ;


{ TfrmListManagerConsolidate }

procedure TfrmListManagerConsolidate.FormShow(Sender : TObject);
begin

  if FHasShown then
    exit;
  FHasShown := true;
  FCanClose := false;
  HandleFormSettings( sdLoad );

  lblItem.Hint := format( cmsgLMCLabel, [ cmsgLMC ] );
  cbChoose.Hint := format( cmsgLMCComboBox, [ cmsgLMC ] );

end;

procedure TfrmListManagerConsolidate.HandleFormSettings( const TheType : TSettingsDirective );
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

procedure TfrmListManagerConsolidate.SetDisplayWord( const AValue : string );
begin
  if fDisplayWord = AValue then Exit;
  fDisplayWord := AValue;
end;

procedure TfrmListManagerConsolidate.FormClose(Sender : TObject; var CloseAction : TCloseAction);
begin
  HandleFormSettings( sdSave );
end;

procedure TfrmListManagerConsolidate.bntOKClick(Sender : TObject);
begin

  if cbChoose.Text = '' then
  begin
    MsgDlgMessage( cmsgNoCommandSelected, format( cmsglmcNoSelection, [ DisplayWord ] ) );
    MsgDlgAttention( self );
    exit;
  end;
  FCanClose := true;
  Modalresult := mrOK;
end;

procedure TfrmListManagerConsolidate.btnCancelClick(Sender : TObject);
begin
  FCanClose := true;
  Modalresult := mrCancel;
end;

procedure TfrmListManagerConsolidate.FormActivate(Sender : TObject);
begin
  if not FIsInitialized then
  begin
    FCanClose := false;
    FIsInitialized := true;
  end;

end;

procedure TfrmListManagerConsolidate.FormCloseQuery(Sender : TObject; var CanClose : boolean);
begin
  CanClose := FCanClose;
end;

procedure TfrmListManagerConsolidate.FormCreate(Sender : TObject);
begin
  ApplyChangeFont( Self );
  FHasShown := false;
  FIsInitialized := true;
end;

end.

