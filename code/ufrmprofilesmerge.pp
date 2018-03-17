unit ufrmprofilesmerge;

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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs//, HintFrame
  , lcltype {THis is needed for key up keyboard constants}
  , unitsharedobj, Buttons
  , unitGlobForm, StdCtrls
  , ufrmProfiles
  ;

type

  { TfrmProfilesMerge }

  TfrmProfilesMerge = class(TForm)
    btnCancel : TBitBtn;
    bntOK : TBitBtn;
    lblspInfo : TLabel;
    lbMergeList : TListBox;
    procedure bntOKClick(Sender : TObject);
    procedure btnCancelClick(Sender : TObject);
    procedure FormActivate(Sender : TObject);
    procedure FormClose(Sender : TObject; var CloseAction : TCloseAction);
    procedure FormCloseQuery(Sender : TObject; var CanClose : boolean);
    procedure FormCreate(Sender : TObject);
    procedure FormShow(Sender : TObject);
    procedure lbMergeListDblClick( Sender : TObject );
  private
    { private declarations }
    FCanClose : boolean;
    FHasShown : boolean;
    FIsInitialized : boolean;
    fResultProfile : TObject;
    fSelectedDest : string;
    fSelectedIsSQL : boolean;
    procedure HandleFormSettings( const TheType : TSettingsDirective );
    procedure SetResultProfile( AValue : TObject );
    procedure SetSelectedDest( AValue : string );
    procedure SetSelectedIsSQL( AValue : boolean );
  public
    { public declarations }
    property ResultProfile : TObject read fResultProfile write SetResultProfile;
    property SelectedDest : string read fSelectedDest write SetSelectedDest;
    property SelectedIsSQL : boolean read fSelectedIsSQL write SetSelectedIsSQL;
  end;

var
  frmProfilesMerge : TfrmProfilesMerge;

implementation

{$R *.lfm}

uses ufrmMsgDlg
     , unitDBUtils
     //, strconst_en
     , unitglob
     ;



{ TfrmProfilesMerge }

procedure TfrmProfilesMerge.FormShow(Sender : TObject);
begin
  if not FHasShown then
  begin
    FCanClose := false;
    HandleFormSettings( sdLoad );
    FHasShown := true;
  end;
end;

procedure TfrmProfilesMerge.lbMergeListDblClick( Sender : TObject );
begin
  if ( lbMergeList.Items.Count = 0 ) or ( lbMergeList.ItemIndex < 0 ) then
    Exit;
  bntOK.Click;
end;

procedure TfrmProfilesMerge.HandleFormSettings( const TheType : TSettingsDirective );
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

procedure TfrmProfilesMerge.SetResultProfile( AValue : TObject );
begin
  if fResultProfile = AValue then Exit;
  fResultProfile := AValue;
end;

procedure TfrmProfilesMerge.SetSelectedDest( AValue : string );
begin
  if fSelectedDest = AValue then Exit;
  fSelectedDest := AValue;
end;

procedure TfrmProfilesMerge.SetSelectedIsSQL( AValue : boolean );
begin
  if fSelectedIsSQL = AValue then Exit;
  fSelectedIsSQL := AValue;
end;

procedure TfrmProfilesMerge.FormClose(Sender : TObject; var CloseAction : TCloseAction);
var
  i : Integer;
begin
  HandleFormSettings( sdSave );
  for i := 0 to lbMergeList.Items.Count - 1 do
    lbMergeList.Items.Objects[ i ] := nil;
end;

procedure TfrmProfilesMerge.bntOKClick(Sender : TObject);
var
  Exists : Boolean;
  DefPath : String;
  aPO : TProfileObj;
begin

  if lbMergeList.ItemIndex < 0 then
  begin
    showmessage( cmsgNoCommandSelected );
    exit;
  end;

  aPO := TProfileObj( lbMergeList.Items.Objects[ lbMergeList.ItemIndex ] );

  DefPath := TfrmProfiles( Owner ).ProgDefaultPath;

  if aPO.IsDB then
    Exists := FileExists( GenerateDBFilePath( aPO.GetExpandedPath( DefPath ),
                                              aPO.Name,
                                              cSqlDBExtension
                                             )
                         )
  else Exists := IniDBFilesExist( aPO.GetExpandedPath( DefPath ), aPO.Name );

  if not Exists then
  begin
    SelectedDest := lbMergeList.Items[ lbMergeList.ItemIndex ];
    SelectedIsSQL := aPO.IsDB;
  end else ResultProfile := aPO;

  FCanClose := true;
  Modalresult := mrOK;

end;

procedure TfrmProfilesMerge.btnCancelClick(Sender : TObject);
begin
  FCanClose := true;
  Modalresult := mrCancel;
end;

procedure TfrmProfilesMerge.FormActivate(Sender : TObject);
begin
  if not FIsInitialized then
  begin
    FIsInitialized := true;
  end;
end;

procedure TfrmProfilesMerge.FormCloseQuery(Sender : TObject; var CanClose : boolean);
begin
  CanClose := FCanClose;
end;

procedure TfrmProfilesMerge.FormCreate(Sender : TObject);
begin
  ApplyChangeFont( Self );
  FHasShown := false;
  FIsInitialized := false;
  fResultProfile := nil;
  fSelectedDest := '';
  fSelectedIsSQL := false;
end;

end.

