unit unitAbout;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormAbout }

  TFormAbout = class(TForm)
    BtnClose: TButton;
    MemoAbout: TMemo;
    procedure BtnCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
  public

  end;

var
  FormAbout: TFormAbout;

implementation

{$R *.lfm}
const VERSTR='eeProg_GUI V1.0R2';
const COPYRIGHT='Copyright Jacques Deschênes, 2025' ;

{ TFormAbout }


procedure TFormAbout.BtnCloseClick(Sender: TObject);
begin
  close;
end;


procedure TFormAbout.FormShow(Sender: TObject);
var
  tf:TextFile;
  line:string;
begin
  with memoAbout do
  begin
    lines.clear;
    lines.append(VERSTR);
    lines.append(COPYRIGHT);
    lines.append('');
    AssignFile(tf,'LICENSE.TXT');
    try
      reset(tf);
      while not EOF(TF) do
      begin
        ReadLn(tf,line);
        lines.append(line);
      end;
    finally
      CloseFile(tf);
    end;
    CaretPos := Point(0,0);
    end;
  BtnClose.SetFocus;
end;

end.

