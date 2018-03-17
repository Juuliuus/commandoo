unit ufrmaddlanguage;

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
  Classes, SysUtils, LazFileUtils, FileUtil,
  Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, EditBtn, ExtCtrls
  , unitSharedObj, HintFrame
  , unitGlobForm
  ;

type

  { TfrmAddLanguage }

  TfrmAddLanguage = class (TForm)
    btnCancel : TBitBtn;
    btnOk : TBitBtn;
    edtLang : TEdit;
    edtFileName : TFileNameEdit;
    FrameHint1 : TFrameHint;
    Label1 : TLabel;
    lblLangAbbrev : TLabel;
    lblLangAbbrev1 : TLabel;
    Panel1 : TPanel;
    procedure btnCancelClick(Sender : TObject);
    procedure btnOkClick(Sender : TObject);
    procedure edtFileNameButtonClick(Sender : TObject);
    procedure edtFileNameExit(Sender : TObject);
    procedure FormActivate(Sender : TObject);
    procedure FormClose(Sender : TObject; var CloseAction : TCloseAction);
    procedure FormCloseQuery(Sender : TObject; var CanClose : boolean);
    procedure FormCreate(Sender : TObject);
    procedure FormShow(Sender : TObject);
  private
    { private declarations }
    FCanClose : boolean;
    FLastFolder : string;
    FIsInitialized : boolean;
    FSaveToPath : string;
    procedure HandleFormSettings(TheType : TSettingsDirective);
    function IsGoodFileName( FiName : string ) : boolean;
    procedure SetSaveToPath( const AValue : string );
  public
    { public declarations }
    LangSymbol : string;
    SymbolList : TStringlist;
    MenuList : TStringlist;
    CurrLangText : string;
    property SaveToPath : string read FSaveToPath write SetSaveToPath;
  end;

var
  frmAddLanguage : TfrmAddLanguage;

resourcestring
  cAddLangHasBlanks = 'All fields must be filled in...' ;
  cNoParenthese = '"%s" can not contain parentheses: "(" or ")", those are used by the system.';
  cNoParentheseLang = 'Language Name';


implementation

uses Strconst_prog, StrConst_EN, unitLanguages;

{$R *.lfm}

{ TfrmAddLanguage }

procedure TfrmAddLanguage.FormActivate(Sender : TObject);
begin
  if FIsInitialized then
    exit;
  FCanClose := false;
  FIsInitialized := true;

end;

procedure TfrmAddLanguage.FormClose(Sender : TObject; var CloseAction : TCloseAction);
begin

  HandleFormSettings( sdSave );

  if Assigned( SymbolList ) then
    SymbolList.Free;
  if Assigned( MenuList ) then
    MenuList.Free;

end;

procedure TfrmAddLanguage.btnOkClick(Sender : TObject);
var
  fiName : String;
begin

  fiName := extractfilename( edtFileName.Text );

  //need to hold current value because IsGoodFileName updates the field
  CurrLangText := trim( edtLang.Text );
  if CurrLangText = '' then
  begin
    if edtLang.Canfocus then
      edtLang.Setfocus;
    exit;
  end;

  If not IsGoodFileName( fiName ) then
  begin
    Showmessage( format( cNotValidPOFile, [ fiName ] ) );
    exit;
  end;

  if not FileExists( edtFileName.Text ) then
  begin
    Showmessage( format( cFileNotExist, [ edtFileName.Text ] ) );
    exit;
  end;


  if ( trim( edtFileName.Text ) = '' )
    //or ( LangSymbol = '' )
    or ( CurrLangText = '' )
    //or ( trim( edtLangEnglish.Text ) = '' )
    then
  begin
    Showmessage( cAddLangHasBlanks );
    exit;
  end;

  if ( pos( '(', CurrLangText ) > 0 ) or ( pos( ')', CurrLangText ) > 0 ) then
  begin
    Showmessage( format( cNoParenthese, [ cNoParentheseLang ] ) );
    exit;
  end;

  if extractfilepath( edtFileName.Text ) <> fSaveToPath then
    if not Copyfile( edtFileName.Text, fSaveToPath + extractfilename( edtFileName.Text ) ) then
      Showmessage( format( cCantCopy, [ edtFileName.Text, fSaveToPath ] ) );

  FCanClose := True;
  ModalResult := mrOK;
end;

function TfrmAddLanguage.IsGoodFileName( FiName : string ) : boolean;
var
  tmpStr : String;
  idx : SizeInt;
  fiExt : String;
  fiLang : String;
  EmergencyName : string;
begin

  Result := false;

  tmpStr := FiName;
  fiLang := '...';
  LangSymbol := '';
  lblLangAbbrev.Caption := fiLang;
  EmergencyName := ExtractFileName( changefileExt( FiName, '' ) );

  idx := pos( '.', tmpStr );
  if idx > 0 then
  begin
    fiName := copy( tmpStr, 1, idx -1 );
    tmpStr := copy( tmpStr, idx + 1, length( tmpStr ) );
    idx := pos( '.', tmpStr );
    if idx > 0 then
    begin
      LangSymbol := copy( tmpStr, 1, idx - 1 );
      fiLang := format( cLangSymbol, [ LangSymbol ] );
      fiExt := copy( tmpStr, idx + 1, length( tmpStr ) );
      if pos( '.', fiExt ) > 0 then
      begin
        LangSymbol := '';
        fiLang := '...';
        exit;
      end;
    end else exit;
  end else exit;

  if fiExt <> 'po' then
    exit;

  if fiName <> cReferenceProgramName then
    exit;

  lblLangAbbrev.Caption := fiLang;

  Idx := SymbolList.IndexOf( LangSymbol );
  if Idx > -1 then
    edtLang.Text := MenuList[ Idx ]
  else edtLang.Text := EmergencyName;

  if edtLang.CanFocus then
    edtLang.SetFocus;

  Result := true;

end;

procedure TfrmAddLanguage.SetSaveToPath( const AValue : string );
begin
  if FSaveToPath = AValue then Exit;
  FSaveToPath := AValue;
end;

procedure TfrmAddLanguage.edtFileNameButtonClick(Sender : TObject);
begin
  if FLastFolder <> '' then
   edtFileName.InitialDir := FLastFolder
  else edtFileName.InitialDir := fSaveToPath;
end;

procedure TfrmAddLanguage.edtFileNameExit(Sender : TObject);
var
  fiName : String;
begin

  edtFileName.Text := trim( edtFileName.Text );
  if edtFileName.Text = '' then
    exit;
  fLastFolder := extractfilepath( edtFileName.Text );

  fiName := extractfilename( edtFileName.Text );

  If not IsGoodFileName( fiName ) then
    Showmessage( format( cNotValidPOFile, [ fiName ] ) );

end;


procedure TfrmAddLanguage.btnCancelClick(Sender : TObject);
begin
  FCanClose := true;
  ModalResult := mrCancel;
end;

procedure TfrmAddLanguage.FormCloseQuery(Sender : TObject; var CanClose : boolean);
begin
  CanClose := FCanClose;
end;

procedure TfrmAddLanguage.FormCreate(Sender : TObject);
begin
  ApplyChangeFont( Self );
  FLastFolder := '';
  SymbolList := TStringlist.Create;
  MenuList := TStringlist.Create;
  edtFileName.DialogTitle := caption;
  FIsInitialized := false;
  LangSymbol := '';

end;

procedure TfrmAddLanguage.FormShow(Sender : TObject);
begin
  HandleFormSettings( sdLoad );
end;


procedure TfrmAddLanguage.HandleFormSettings( TheType : TSettingsDirective );
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
        FormSettings.AddSetting( FLastFolder );

        FormSettings.SaveFormSettings( Self.Name );

      end;
    sdLoad :
      begin

        While FormSettings.ReadFormSettings( Self.Name, i, theValue ) do
        case i of
          0 : Height := strtoint( theValue );
          1 : Width := strtoint( theValue );
          2 : FLastFolder := theValue;
        end;

      end;

  end;

end;

end.

