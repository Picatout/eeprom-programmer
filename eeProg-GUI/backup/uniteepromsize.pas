unit UnitEepromSize;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormEeeprom }

  TFormEeeprom = class(TForm)
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
  FormEeeprom: TFormEeeprom;

implementation

{$R *.lfm}

{ TFormEeeprom }

procedure TFormEeeprom.BtnOkClick(Sender: TObject);
begin
    eepromSize:=StrToInt(EditSize.text)*1024;
    confirm:=true;
    close;
end;

procedure TFormEeeprom.BtnCancelClick(Sender: TObject);
begin
  confirm:=false;
  close;
end;

end.

