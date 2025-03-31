unit unitPortCfg;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormPortCfg }

  TFormPortCfg = class(TForm)
    BtnOk: TButton;
    BtnCancel: TButton;
    CBDeviceList: TComboBox;
    Label1: TLabel;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnOkClick(Sender: TObject);
    procedure BtnOkEnter(Sender: TObject);
    procedure CBDeviceListSelect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public
    CommPortName:AnsiString;
  end;

var
  FormPortCfg: TFormPortCfg;

implementation

{$R *.lfm}


uses serial
{$IFDEF LINUX}
;
{$ENDIF}
{$IFDEF WINDOWS}
,registry;
{$ENDIF}



{ TFormPortCfg }

procedure TFormPortCfg.FormCreate(Sender: TObject);
var
   i:integer;
{$IFDEF LINUX}
  rst: TSearchRec;
  error: LongInt ;

begin
  CBDeviceList.Items.Clear;
  error:= FindFirst('/dev/ttyACM*',faAnyFile,rst);
  while error=0 do
  begin
       CBDeviceList.Items.Append(rst.Name);
       error:=FindNext(rst);
  end;
  FindClose(rst);
{$ENDIF}
{$IFDEF WINDOWS}
// code from: https://patotech.blogspot.com/2012/04/enumerate-com-ports-in-windows-with.html
  reg: TRegistry;
  l, v: TStringList;
  n: integer;
begin
    l := TStringList.Create;
    reg := TRegistry.Create;
    try
  {$IFNDEF VER100}
      reg.Access := KEY_READ;
  {$ENDIF}
      reg.RootKey := HKEY_LOCAL_MACHINE;
      reg.OpenKeyReadOnly('HARDWARE\DEVICEMAP\SERIALCOMM');//, false);
      reg.GetValueNames(l);
      for n := 0 to l.Count - 1 do
        CBDeviceList.Items.Append(reg.ReadString(l[n]));
    finally
      reg.Free;
      l.Free;
    end;
{$ENDIF}
end;

procedure TFormPortCfg.FormShow(Sender: TObject);
begin
  with CBDeviceList do
  begin
       CBDeviceList.SetFocus;
       if (ItemIndex<0) and (Items.Count>0) then ItemIndex:=0;
  end;
end;


procedure TFormPortCfg.BtnOkClick(Sender: TObject);
begin
{$IFDEF LINUX}
  CommPortName:='/dev/'+CBDeviceList.Items[CBDeviceList.ItemIndex];
{$ENDIF}
{$IFDEF WINDOWS}
  CommPortName:=CBDeviceList.Items[CBDeviceList.ItemIndex];
{$ENDIF}
 Close;
end;

procedure TFormPortCfg.BtnOkEnter(Sender: TObject);
begin
end;

procedure TFormPortCfg.CBDeviceListSelect(Sender: TObject);
begin
   BtnOkClick(Sender);
end;



procedure TFormPortCfg.BtnCancelClick(Sender: TObject);
begin
  CommPortName:='';
  Close;
end;


end.

