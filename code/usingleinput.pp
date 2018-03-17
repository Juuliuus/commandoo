unit uSingleInput;

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
  Forms, Controls, Graphics, Dialogs,
  EditBtn, StdCtrls
  , lcltype {THis is needed for key up keyboard constants}
  , unitGlobForm, Buttons
  ;

type

  TSingleInputMode = ( simEdit, simFile, simDir );

  { TfrmSingleInput }

  TfrmSingleInput = class(TForm)
    btnOk : TBitBtn;
    btnCancel : TBitBtn;
    deDir: TDirectoryEdit;
    edtInput: TEdit;
    feFile: TFileNameEdit;
    lblHint : TLabel;
    procedure btnCancelClick( Sender : TObject );
    procedure btnOkClick( Sender : TObject );
    procedure edtInputKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender : TObject; var CloseAction : TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender : TObject);
  private
    { private declarations }
    fNewFolder : string;
    fOriginalFolder : string;
    FIsInitialized : boolean;
    FHasShown : boolean;
    FCanClose : boolean;
    ActiveEdit : TObject;
    FormMode : TSingleInputMode;
    procedure SetTheFocus;
    procedure HandleFormSettings( const TheType : TSettingsDirective );
  public
    { public declarations }
    procedure SetActiveEdit( const AValue : string );
    function ReadActiveEdit : string;
    procedure SetSingleInputMode( const FM : TSingleInputMode );
  end;

  function DoSingleInput( const ATitle: string; var AValue: string; const AFormMode: TSingleInputMode;
    aForm: TForm; DisallowSameValue : boolean = true ): boolean;

var
  frmSingleInput: TfrmSingleInput;

const
  cFake_mrCancel = '<^cancel^>';

resourcestring
  csiChooseAFile = 'Type in or choose a File Name';
  csiChooseAFolder = 'Type in or choose a Folder Name';

implementation

uses StrConst_EN
  , unitsharedobj
  , juusgen
  ;
{$R *.lfm}

function DoSingleInput( const ATitle : string; var AValue : string;
  const AFormMode : TSingleInputMode; aForm : TForm; DisallowSameValue : boolean ) : boolean;
var
  CompareMe: string;
begin

  Result := False;

  with TfrmSingleInput.Create(aForm) do
    try
      Caption := ATitle;
      SetSingleInputMode(AFormMode);
      SetActiveEdit(AValue);
      showmodal;

      if ModalResult = mrOk then
      begin

        CompareMe := ReadActiveEdit;

        if DisallowSameValue and ( CompareStr(AValue, CompareMe) = 0 ) then
          exit;

        AValue := CompareMe;
        Result := True;

      end;

    finally
      Free;
    end;

end;


{ TfrmSingleInput }

procedure TfrmSingleInput.SetTheFocus;
begin
  if ActiveEdit is TEdit then
    TEdit( ActiveEdit ).SetFocus
  else TCustomControl( ActiveEdit ).SetFocus;
end;

procedure TfrmSingleInput.HandleFormSettings( const TheType : TSettingsDirective );
var
  i : Integer;
  theValue: string;
begin
  theValue := '';
  i := -1;

  case TheType of
    sdSave :
      begin
        FormSettings.AddSetting( inttoStr( Width ) );
        FormSettings.AddSetting(
              StrIf( ( fNewFolder = '' ) or ( FormMode = simEdit ), fOriginalFolder, fNewFolder )
                                          );

        FormSettings.SaveFormSettings( Self.Name );

      end;
    sdLoad :
      begin

        While FormSettings.ReadFormSettings( Self.Name, i, theValue ) do
        case i of
          0 : Width := strtoint( theValue );
          1 : fOriginalFolder := theValue;
        end

      end;

  end;

end;

procedure TfrmSingleInput.FormActivate(Sender: TObject);
begin
  if not FIsInitialized then
  begin
    FCanClose := false;
    FIsInitialized := true;
  end;
  SetTheFocus;
end;

procedure TfrmSingleInput.FormClose(Sender : TObject; var CloseAction : TCloseAction);
begin
  HandleFormSettings( sdSave );
end;

procedure TfrmSingleInput.btnOkClick( Sender : TObject );
begin
  if Trim( ReadActiveEdit ) = '' then
  begin
    ShowMessage( cInputCantBeBlank );
    SetTheFocus;
    exit;
  end;
  //in set mode, check for dups
  FCanClose := true;
  ModalResult := mrOK;
end;

procedure TfrmSingleInput.edtInputKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  if ReadActiveEdit = '' then
    exit;
  if Key = vk_Return then
    btnOK.Click;
end;

procedure TfrmSingleInput.btnCancelClick( Sender : TObject );
begin
  FCanClose := true;
  ModalResult := mrCancel;
end;

procedure TfrmSingleInput.FormCloseQuery(Sender: TObject; var CanClose: boolean );
begin
  CanClose := FCanClose;
end;

procedure TfrmSingleInput.FormCreate(Sender: TObject);
begin
  ApplyChangeFont( Self );
  fNewFolder := '';
  fOriginalFolder := '';
  FIsInitialized := false;
  FHasShown := false;
end;

procedure TfrmSingleInput.FormShow(Sender : TObject);
begin

  if FHasShown then
    exit;

  FHasShown := true;
  HandleFormSettings( sdLoad );

end;

procedure TfrmSingleInput.SetActiveEdit( const AValue : string );
var
  FolderName : string;
begin
  FolderName := fOriginalFolder;
  if aValue <> '' then
    FolderName := aValue;
  case FormMode of
    simEdit:
      begin
        TEdit( ActiveEdit ).Text := AValue;
      end;
    simFile:
      begin
        TFileNameEdit( ActiveEdit ).FileName := FolderName;//AValue;
      end;
    simDir:
      begin
        TDirectoryEdit( ActiveEdit ).Directory := FolderName;//AValue;
      end;
  end;

end;

function TfrmSingleInput.ReadActiveEdit: string;
begin
  Result := '<error>';
  case FormMode of
    simEdit:
      begin
        Result := TEdit( ActiveEdit ).Text;
        fNewFolder := '';
      end;
    simFile:
      begin
        Result := TFileNameEdit( ActiveEdit ).Text;
        fNewFolder := extractFilePath( Result );
      end;
    simDir:
      begin
        Result := TDirectoryEdit( ActiveEdit ).Text;
        fNewFolder := Result;
      end;
  end;
  Result := Trim( Result );
end;

procedure TfrmSingleInput.SetSingleInputMode( const FM : TSingleInputMode );
begin
  FormMode := FM;
  case FormMode of
    simEdit:
      begin
        edtInput.Visible := true;
        edtInput.Top := 35;
        edtInput.Text := '';
        ActiveEdit := edtInput;//TEdit;
      end;
    simFile:
      begin
        feFile.Visible := true;
        feFile.Top := 35;
        feFile.Text := '';
        lblHint.Visible := true;
        ActiveEdit := feFile;//TEdit;
      end;
    simDir:
      begin
        deDir.Visible := true;
        deDir.Top := 35;
        deDir.Text := '';
        lblHint.Visible := true;
        ActiveEdit := deDir;//TEdit;
      end;
  end;

end;

end.

