unit HintFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils
  , lcltype {THis is needed for key up keyboard constants}
  , Forms, Controls, StdCtrls;

type

  { TFrameHint }

  TFrameHint = class (TFrame)
    cbHints : TCheckBox;
    procedure cbHintsChange(Sender : TObject);
    procedure cbHintsKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
  private
    { private declarations }
  public
    { public declarations }
  end ;

implementation

{$R *.lfm}

{ TFrameHint }

procedure TFrameHint.cbHintsChange(Sender : TObject);
begin
  ShowHint := cbHints.Checked;
  GetParentForm( self ).ShowHint := ShowHint;
end;

procedure TFrameHint.cbHintsKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin
  if key = vk_return then
    key := vk_unknown;
end;

end .

