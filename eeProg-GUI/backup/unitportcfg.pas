unit unitPortCfg;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Lazserial;

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



{ TFormPortCfg }

procedure TFormPortCfg.FormCreate(Sender: TObject);
{$IFDEF LINUX}
var
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
end;
{$ENDIF}
{$IFDEF WINDOWS}
// code from: https://patotech.blogspot.com/2012/04/enumerate-com-ports-in-windows-with.html
var

begin

end;
{$ENDIF}

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
  CommPortName:='/dev/'+CBDeviceList.Items[CBDeviceList.ItemIndex];
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

