unit ufrmManageProfile;

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
  , unitGlobForm, StdCtrls, ExtCtrls
  ;

type

  TMPType = ( mpAdd, mpEdit, mpCopy );
  { TfrmManageProfile }

  TfrmManageProfile = class(TForm)
    bntOK : TBitBtn;
    btnCancel : TBitBtn;
    btnChangePath : TButton;
    btnChangeName : TButton;
    btnDefaultPath : TButton;
    edtProfileName : TEdit;
    edtProfilePath : TEdit;
    FrameHint1 : TFrameHint;
    Label1 : TLabel;
    lblName : TLabel;
    rgType : TRadioGroup;
    procedure bntOKClick(Sender : TObject);
    procedure btnCancelClick(Sender : TObject);
    procedure btnChangeNameClick( Sender : TObject );
    procedure btnChangePathClick( Sender : TObject );
    procedure btnDefaultPathClick( Sender : TObject );
    procedure FormActivate(Sender : TObject);
    procedure FormClose(Sender : TObject; var CloseAction : TCloseAction);
    procedure FormCloseQuery(Sender : TObject; var CanClose : boolean);
    procedure FormCreate(Sender : TObject);
    procedure FormShow(Sender : TObject);
  private
    { private declarations }
    FCanClose : boolean;
    FHasShown : boolean;
    fIsChanged : boolean;
    FIsInitialized : boolean;
    fIsSqlDB : Smallint;
    fMode : TMPType;
    fLastPath : string;
    fOrigName : string;
    fOrigPath : string;
    procedure HandleFormSettings( const TheType : TSettingsDirective );
    procedure SetIsChanged( const AValue : boolean );
    procedure SetMode( AValue : TMPType );

  public
    { public declarations }
    ShowPoint : TPoint;
    procedure AdjustDBType( const DBNameIdx, DBTextIdx : integer );
    procedure ApplyTranslation;
    procedure SetupEditProfile( const theName, thePath : string; const IsDB : boolean );
    procedure SetupLabels( const theName, thePath : string );
    property IsSqlDB : smallint read fIsSqlDB;
    property Mode : TMPType read fMode write SetMode;
    property IsChanged : boolean read fIsChanged write SetIsChanged;
  end;

var
  frmManageProfile : TfrmManageProfile;

implementation

{$R *.lfm}

uses ufrmMsgDlg
     , uSingleInput
     , ufrmProfiles
     , unitDBUtils
     //, strconst_en
     , unitglob
     ;

resourcestring
  cmpNameLabel = 'Name: ';
  cmpChooseDBType = 'You must choose a DB Type.';
  ccapdbTypes = '&S  sqlite DB' + LineEnding + '&T  Text File Based';


{ TfrmManageProfile }

procedure TfrmManageProfile.FormShow(Sender : TObject);
begin

  if not FHasShown then
  begin
    //Top := ShowPoint.Y;
    if ShowPoint.X <> 0 then
      Left := ShowPoint.X;
    FCanClose := false;
    HandleFormSettings( sdLoad );
    FHasShown := true;

    case Mode of
      mpAdd : ;
      mpEdit :
        begin
          Caption := format( ccapGenericEdit, [ ccapGenericProfile ] );
          lblName.Caption := cmpNameLabel;
          btnChangeName.Visible := true;
          if IsSqlDb = 1 then
            rgType.ItemIndex := 0
          else rgType.ItemIndex := 1;
          rgType.Enabled := false;
        end;

      mpCopy : ;
    end;
  end;

end;

procedure TfrmManageProfile.SetupLabels( const theName, thePath : string );
begin
  edtProfileName.Text := theName;
  edtProfilePath.Text := thePath;
  IsChanged := false;
end;

procedure TfrmManageProfile.SetupEditProfile( const theName, thePath : string; const IsDB : boolean );
begin
  fOrigName := theName;
  fOrigPath := thePath;
  SetupLabels( theName, thePath );
  if IsDB then
    fIsSqlDB := 1
  else fIsSqlDB := 0;

  IsChanged := false;

end;

procedure TfrmManageProfile.HandleFormSettings( const TheType : TSettingsDirective );
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
        FormSettings.AddSetting( fLastPath );

        FormSettings.SaveFormSettings( Self.Name );
      end;
    sdLoad :
      While FormSettings.ReadFormSettings( Self.Name, i, theValue ) do
      case i of
        0 : Height := strtoint( theValue );
        1 : Width := strtoint( theValue );
        2 : fLastPath := theValue;
      end;
  end;

end;

procedure TfrmManageProfile.SetIsChanged( const AValue : boolean );
begin
  if fIsChanged = AValue then Exit;
  fIsChanged := AValue;
end;

procedure TfrmManageProfile.SetMode( AValue : TMPType );
begin
  if fMode = AValue then Exit;
  fMode := AValue;
end;


procedure TfrmManageProfile.FormClose(Sender : TObject; var CloseAction : TCloseAction);
begin
  HandleFormSettings( sdSave );
end;

procedure TfrmManageProfile.bntOKClick(Sender : TObject);
begin
  if rgType.ItemIndex = -1 then
  begin
    Showmessage( cmpChooseDBType );
    exit;
  end;

  IsChanged := ( edtProfileName.Text <> fOrigName ) or ( edtProfilePath.Text <> fOrigPath );

  if fIsSqlDB = -1 then
  begin
    if rgType.ItemIndex = 0 then
      fIsSqlDB := 1
    else fIsSqlDB := 0;
  end;

  FCanClose := true;
  Modalresult := mrOK;
end;

procedure TfrmManageProfile.btnCancelClick(Sender : TObject);
begin
  FCanClose := true;
  Modalresult := mrCancel;
end;

procedure TfrmManageProfile.btnChangeNameClick( Sender : TObject );
var
  Str : string;
begin
  Str := edtProfileName.Text;
  if not TfrmProfiles( Owner ).NameAccepted( format( ccapGenericEdit, [ ccapGenericProfile ] ), Str ) then
    exit;
  edtProfileName.Text := Str;
end;

procedure TfrmManageProfile.btnChangePathClick( Sender : TObject );
begin
  if not DoSingleInput( format( ccapGenericChoose, [ ccapGenericFolder ] ), fLastPath, simDir, self, false ) then
    exit;
  edtProfilePath.Text := IncludeTrailingPathDelimiter( fLastPath );
end;

procedure TfrmManageProfile.btnDefaultPathClick( Sender : TObject );
begin
  edtProfilePath.Text := constDefaultPathDisplay;
end;

procedure TfrmManageProfile.FormActivate(Sender : TObject);
begin
  if not FIsInitialized then
  begin
    FIsInitialized := true;
  end;
end;

procedure TfrmManageProfile.AdjustDBType( const DBNameIdx, DBTextIdx : integer );
begin
//users can use the same Profile name for both text and sql, this removes the choice for the one already used to
//enforce unique profile names as long as they are different DB types.
  if ( DBNameIdx > - 1 ) or ( DBTextIdx > - 1 ) then
  begin
    fIsSqlDB := 1;
    if DBNameIdx > -1 then
    begin
      rgType.Items.Delete( 0 );
      fIsSqlDB := 0;
    end
    else rgType.Items.Delete( 1 );
    rgType.ItemIndex := 0;
  end;

end;

procedure TfrmManageProfile.ApplyTranslation;
begin
  rgType.Items.Text := ccapdbTypes;
//was a bad translation
  if rgType.Items.Count < 2 then
    raise EErrorDevelopment.Create( 'TfrmManageProfile.FormShow: Bad Translation, not enough items.' );
end;

procedure TfrmManageProfile.FormCloseQuery(Sender : TObject; var CanClose : boolean);
begin
  CanClose := FCanClose;
end;

procedure TfrmManageProfile.FormCreate(Sender : TObject);
begin
  ApplyChangeFont( Self );
  FHasShown := false;
  FIsInitialized := false;
  fIsSqlDB := -1;
  ShowPoint.X := 0;
end;

end.

