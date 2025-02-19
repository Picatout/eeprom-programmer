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

end.

