unit CommError;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormCommError }

  TFormCommError = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  FormCommError: TFormCommError;

implementation

{$R *.lfm}

{ TFormCommError }

procedure TFormCommError.Button1Click(Sender: TObject);
begin
  Close;
end;

end.

