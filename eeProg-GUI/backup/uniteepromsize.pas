unit UnitEepromSize;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormEeprom }

  TFormEeprom = class(TForm)
    BtnOk: TButton;
    BtnCancel: TButton;
    EditSize: TEdit;
    Label1: TLabel;
    procedure BtnOkClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure EditSizeKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
  private

  public
     eepromSize:integer;
     confirm:boolean;
  end;

var
  FormEeprom: TFormEeprom;

implementation

{$R *.lfm}

{ TFormEeprom }

procedure TFormEeprom.BtnOkClick(Sender: TObject);
begin
    eepromSize:=StrToInt(EditSize.text)*1024;
    confirm:=true;
    close;
end;

procedure TFormEeprom.BtnCancelClick(Sender: TObject);
begin
  confirm:=false;
  close;
end;

procedure TFormEeprom.EditSizeKeyPress(Sender: TObject; var Key: char);
begin
  if key=#13 then BtnOkClick(sender);
end;

procedure TFormEeprom.FormShow(Sender: TObject);
begin
  EditSize.Focused:=true;
end;

end.

