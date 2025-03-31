unit UnitEepromSize;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type
  {EEPROM TYPE }
  EnumEEtype = (AT28=0,SST39=1);
  {EEPROM operating voltage}
  EnumVcc = (JP3_3V=0,JP3_5V=1);

  {EEPROM INFO RECORD}
  Teeprom=record
    name: string;
    vcc: EnumVcc;    // operating voltage
    size: integer; // in KB
    eeType: EnumEEtype;
  end;


  { TFormEeprom }

  TFormEeprom = class(TForm)
    BtnOk: TButton;
    BtnCancel: TButton;
    CBeeprom: TComboBox;
    EditeeType: TEdit;
    EditSize: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    lblJP3: TLabel;
    RGJP3: TRadioGroup;
    procedure CBeepromChange(Sender: TObject);
    procedure BtnOkClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public
     eepromSize:integer;
     confirm:boolean;
  end;

var
  FormEeprom: TFormEeprom;

  const eeprom_list: array [0..12] of Teeprom=(
  (name:'AT28BV256 - 32K 3.3V EEPROM';vcc:JP3_3V;size:32;eeType:AT28),
  (name:'AT28BV64B - 8K 3.3V EEPROM';vcc:JP3_3V;size:8;eeType:AT28),
  (name:'AT28C256 -   32K 5V EEPROM';vcc:JP3_5V;size:32;eeType:AT28),
  (name:'AT28C64B -   8K 5V EEPROM';vcc:JP3_5V;size:8;eeType:AT28),
  (name:'SST39LF010 - 128K 3.3V FLASH';vcc:JP3_3V;size:128;eeType:SST39),
  (name:'SST39LF020 -  256K 3.3V  FLASH';vcc:JP3_3V;size:256;eeType:SST39),
  (name:'SST39LF040 -  256K 3.3V FLASH';vcc:JP3_3V;size:512;eeType:SST39),
  (name:'SST39SF010 - 128K 5V FLASH';vcc:JP3_5V;size:128;eeType:SST39),
  (name:'SST39SF020 - 128K 5V FLASH';vcc:JP3_5V;size:256;eeType:SST39),
  (name:'SST39SF040 - 128K 5V FLASH';vcc:JP3_5V;size:512;eeType:SST39),
  (name:'SST39VF010 - 128K 3.3V FLASH';vcc:JP3_3V;size:128;eeType:SST39),
  (name:'SST39VF020 - 256K 3.3V FLASH';vcc:JP3_3V;size:256;eeType:SST39),
  (name:'SST39VF040 - 256K 3.3V FLASH';vcc:JP3_3V;size:512;eeType:SST39)
  );

implementation

{$R *.lfm}

uses eeProgCmd,UnitMain;

const
  eeTypeName: array[0..1] of string=(
  'AT28',
  'SST39'
  );

procedure TFormEeprom.CBeepromChange(Sender: TObject);
begin
  EditSize.text:=intToStr(eeprom_list[CBEeprom.itemIndex].size);
  RGJP3.itemIndex:=integer(eeprom_list[CBEeprom.itemIndex].vcc);
  editEEType.text:=eeTypeName[integer(eeprom_list[CBEeprom.itemIndex].eeType)];
end;

{ TFormEeprom }

var
  cmdStr: string;

procedure TFormEeprom.BtnOkClick(Sender: TObject);
begin
    eepromSize:=StrToInt(EditSize.text)*1024;
    confirm:=true;
    cmdStr:=IntToHex(integer(eeprom_list[CBEeprom.itemIndex].eeType),1)+
    'T'+IntToHex(eepromSize,5)+'S';
    eeProgCmd.eeProgCmd(cmdStr);
    eeProgCmd.receiveData(FormMain.memoConsole);
    close;
end;

procedure TFormEeprom.BtnCancelClick(Sender: TObject);
begin
  confirm:=false;
  close;
end;


procedure TFormEeprom.FormCreate(Sender: TObject);
var
  i:integer;

begin
  with CBeeprom do
  begin
  for i:= 0 to length(eeprom_list)-1 do
  begin
     items.Add(eeprom_list[i].name);
  end;
  end;
end;

procedure TFormEeprom.FormShow(Sender: TObject);
begin
  EditSize.SetFocus;
end;

end.

