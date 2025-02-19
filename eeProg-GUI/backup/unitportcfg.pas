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
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnOkClick(Sender: TObject);
    procedure BtnOkEnter(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public
    CommPortName:AnsiString;
    LineDelay:integer;
  end;

var
  FormPortCfg: TFormPortCfg;

implementation

{$R *.lfm}



{ TFormPortCfg }

procedure TFormPortCfg.FormCreate(Sender: TObject);
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

procedure TFormPortCfg.FormShow(Sender: TObject);
begin
    Edit2.Text:=IntToStr(LineDelay);
end;

procedure TFormPortCfg.BtnOkClick(Sender: TObject);
begin
  CommPortName:='/dev/'+CBDeviceList.Items[CBDeviceList.ItemIndex];
  LineDelay:=StrToInt(Edit2.Text);
  Close;
end;

procedure TFormPortCfg.BtnOkEnter(Sender: TObject);
begin
end;



procedure TFormPortCfg.BtnCancelClick(Sender: TObject);
begin
  Close;
end;


end.

