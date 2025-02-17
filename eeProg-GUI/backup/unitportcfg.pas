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
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  FormPortCfg: TFormPortCfg;

implementation

{$R *.lfm}

{ TFormPortCfg }

procedure TFormPortCfg.FormCreate(Sender: TObject);
begin

end;

procedure TFormPortCfg.BtnOkClick(Sender: TObject);
begin
  formPortCfg.Close;
end;

procedure TFormPortCfg.BtnCancelClick(Sender: TObject);
begin
  formPortCfg.Close;
end;

end.

