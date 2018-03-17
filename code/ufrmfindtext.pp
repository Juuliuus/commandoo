unit ufrmfindtext;

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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, LCLType, ExtCtrls;

type

  { TfrmFindText }

  TfrmFindText = class( TForm)
    edtFindText : TEdit;
    lblFindNextText : TLabel;
    lbOldFinds : TListBox;
    rgTopCursor : TRadioGroup;
    procedure edtFindTextKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure FormCreate( Sender : TObject );
    procedure FormDeactivate( Sender : TObject );
    procedure FormKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure FormShow( Sender : TObject );
    procedure lbOldFindsDblClick( Sender : TObject );
    procedure lbOldFindsKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure rgTopCursorSelectionChanged( Sender : TObject );
  private
    fCurrPos : SizeInt;
    fItemToFind : string;
    fMemo : TMemo;
    function FindText( const FromPos : SizeInt ) : SizeInt;
    procedure LBItemToFind;
    procedure ReSet;
    procedure SetCurrPos( const AValue : SizeInt );
    procedure SetItemToFind( const AValue : string );
    procedure SetMemo( AValue : TMemo );
    procedure UpdateLB( const Idx : integer; const Value : string );
    { private declarations }
  public
    { public declarations }
    procedure RefindInMemo( aMemo : TMemo );
    procedure DoInitialFind( FromTop : boolean = false; aMemo : TMemo = nil );
    procedure UpdateCaptions;

    property Memo : TMemo read fMemo write SetMemo;
    property CurrPos : SizeInt read fCurrPos write SetCurrPos;
    property ItemToFind  : string read fItemToFind write SetItemToFind;
  end;

var
  frmFindText : TfrmFindText;

implementation

{$R *.lfm}

uses unitGlob;

resourcestring
  cmsgfftEndOfText = 'Search "%s" reached the end. Start again from top?';
  cmsgfftFromCursorNotFound = 'Find "%s" from cursor position "%d" not found. Do you want to search from top?';
  cmsgfftNotFound = '"%s" not found.';

  ccaprgChoices = '&A  Top' + LineEnding + '&B  Cursor';
  ccapFindAgain = 'ctrl-shift-F or ctrl-L finds again';

{ TfrmFindText }
procedure TfrmFindText.ReSet;
begin
  if Showing and edtFindText.CanFocus then
    edtFindText.SetFocus;
  lbOldFinds.Itemindex := -1;
end;

procedure TfrmFindText.RefindInMemo( aMemo : TMemo );
var
  Idx : SizeInt;
begin

  if ItemToFind = '' then
    exit;

  if ( CurrPos = 0 ) and ( Memo = aMemo ) then
  begin
//we've already checked this memo
    showmessage( format( cmsgfftNotFound, [ edtFindText.Text ] ) );
    exit;
  end;

  if Memo <> aMemo then
  begin
//they've switched memos, start fresh
    CurrPos := 0;
    DoInitialFind( True, aMemo );
    exit;
  end;

  if CurrPos = -1 then
  begin
//they've exhaustively searched this memo, start fresh
    CurrPos := 0;
    DoInitialFind( true );
    exit;
  end;

//subsequent find in same memo
  Idx := FindText( CurrPos );

  if Idx > 0 then
  begin
    memo.SelStart := CurrPos + Idx - 1;//2;
    memo.SelLength := length( edtFindText.Text );
    CurrPos := memo.SelStart + memo.SelLength;
  end
  else
  begin
    if MessageDlg( format( cmsgfftEndOfText, [ edtFindText.Text ] ),
                   mtConfirmation, [ mbYes, mbNo ], 0 ) = mrYes then
    begin
      CurrPos := 0;
      DoInitialFind( true );
      exit;
    end
    else CurrPos := -1;

  end;

end;

function TfrmFindText.FindText( const FromPos : SizeInt ) : SizeInt;
begin
  case FromPos of
    0, -1 : result := Pos( ItemToFind, Uppercase( memo.Text ) );
    else result := Pos( ItemToFind, Uppercase( copy( memo.Text, FromPos + 1, MAXINT ) ) );
  end;
end;

procedure TfrmFindText.DoInitialFind( FromTop : boolean = false; aMemo : TMemo = nil );
var
  Idx, NewPos : SizeInt;
begin

  if not assigned( Memo ) and not assigned( aMemo ) then
    raise EErrorDevelopment.create( 'TfrmFindText.DoInitialFind: No Memo assigned.' );

  if assigned( aMemo ) then
    Memo := aMemo;

  case rgTopCursor.ItemIndex of
    0 : CurrPos := FindText( 0 );
    1 :
      begin
        if FromTop then
//re-entry from ReFindInMemo
          CurrPos := FindText( 0 )
        else
        begin
          Idx := Memo.SelStart + Memo.SelLength + 1;
          NewPos := FindText( Idx );
          if NewPos = 0 then
          begin
//if it was a new memo and the selstart was beyond a possible hit, try again
            if MessageDlg( format( cmsgfftFromCursorNotFound, [ edtFindText.Text, Idx ] ),
                           mtConfirmation, [ mbYes, mbNo ], 0 ) = mrYes then
              CurrPos := FindText( 0 )
            else CurrPos := -1;//0;
          end
          else CurrPos := Idx + NewPos - 1;
        end;
      end;
  end;

  if CurrPos > 0 then
  begin
    memo.SelStart := CurrPos - 1;
    memo.SelLength := length( edtFindText.Text );
    CurrPos := memo.SelStart + memo.SelLength;
  end
  else if CurrPos <> -1 then
    showmessage( format( cmsgfftNotFound, [ edtFindText.Text ] ) );

  Hide;

end;

procedure TfrmFindText.UpdateCaptions;
var
  Idx : Integer;
begin
  lblFindNextText.Caption := ccapFindAgain;

  Idx := rgTopCursor.ItemIndex;
  rgTopCursor.ItemIndex := -1;
  rgTopCursor.Items.Text := ccaprgChoices;
  if rgTopCursor.Items.Count < 2 then
    raise EErrorDevelopment.Create( 'TfrmManageProfile.FormShow: Bad Translation, not enough items.' );
  rgTopCursor.ItemIndex := Idx;
end;

procedure TfrmFindText.UpdateLB( const Idx : integer; const Value : string );
begin
  if Idx > -1 then
    lbOldFinds.Items.Delete( Idx );
  lbOldFinds.Items.Insert( 0, Value );
end;

procedure TfrmFindText.edtFindTextKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  if ( Key = vk_return ) and ( trim( edtFindText.Text ) <> '' ) then
  begin
    UpdateLB( lbOldFinds.Items.IndexOf( edtFindText.Text ), edtFindText.Text );
    ItemToFind := Uppercase( edtFindText.Text );
    DoInitialFind;
  end;

  if ( Key = vk_down ) and ( lbOldFinds.Items.Count > 0 ) then
  begin
    lbOldFinds.ItemIndex := 0;
    lbOldFinds.SetFocus;
  end;
end;

procedure TfrmFindText.FormCreate( Sender : TObject );
begin
  fItemToFind := '';
  fCurrPos := 0;
  fMemo := nil;
  lbOldFinds.Items.Add( '' );
end;

procedure TfrmFindText.FormDeactivate( Sender : TObject );
begin
  Hide;
end;

procedure TfrmFindText.FormKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  if Key = vk_escape then
  begin
    ReSet;
    Hide;
  end;
end;

procedure TfrmFindText.FormShow( Sender : TObject );
begin
  if assigned( memo ) and ( memo.SelLength > 0 ) then
    edtFindText.Text := Memo.SelText;
end;

procedure TfrmFindText.LBItemToFind;
begin

  if ( lbOldFinds.Items.Count = 0 )
     or ( lbOldFinds.ItemIndex < 0 )
     or ( trim( lbOldFinds.Items[ lbOldFinds.ItemIndex ] ) = '' ) then
    exit;

  ItemToFind := lbOldFinds.Items[ lbOldFinds.ItemIndex ];

  UpdateLB( lbOldFinds.ItemIndex, ItemToFind );

  edtFindText.Text := ItemToFind;

  ItemToFind := Uppercase( ItemToFind );

  Reset;

  DoInitialFind;

end;


procedure TfrmFindText.SetCurrPos( const AValue : SizeInt );
begin
  if fCurrPos = AValue then Exit;
  fCurrPos := AValue;
end;

procedure TfrmFindText.lbOldFindsDblClick( Sender : TObject );
begin
  LBItemToFind;
end;

procedure TfrmFindText.lbOldFindsKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin

    if key = vk_return then
      LBItemToFind;

    if ( Key = vk_up ) and ( lbOldFinds.ItemIndex <= 0 ) then
      ReSet;

end;

procedure TfrmFindText.rgTopCursorSelectionChanged( Sender : TObject );
begin
  ReSet;
end;

procedure TfrmFindText.SetItemToFind( const AValue : string );
begin
  if fItemToFind = AValue then Exit;
  fItemToFind := AValue;
end;

procedure TfrmFindText.SetMemo( AValue : TMemo );
begin
  if fMemo = AValue then Exit;
  fMemo := AValue;
  fCurrPos := 0;
end;

end.

