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
    Edit1: TEdit;
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
begin

end;

procedure TFormPortCfg.FormShow(Sender: TObject);
begin
    Edit1.Text:=CommPortName;
    Edit2.Text:=IntToStr(LineDelay);
end;

procedure TFormPortCfg.BtnOkClick(Sender: TObject);
begin
  CommPortName:=Edit1.Text;
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

