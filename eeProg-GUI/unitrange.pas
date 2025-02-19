unit unitRange;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TFormRange }

  TFormRange = class(TForm)
    BtnOk: TButton;
    BtnCancel: TButton;
    EditStart: TEdit;
    EditEnd: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    RBDecimal: TRadioButton;
    RBHexadecimal: TRadioButton;
    RBbase: TRadioGroup;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnOkClick(Sender: TObject);
    procedure EditEndKeyPress(Sender: TObject; var Key: char);
    procedure EditStartKeyPress(Sender: TObject; var Key: char);
  private

  public
     Confirm:boolean;
     StartAddr:integer;
     EndAddr:integer;
     StartHex:string;
     EndHex:string;
  end;

var
  FormRange: TFormRange;

implementation

{$R *.lfm}

function IntToHexStr(n:integer):string;
var
   hexStr:string;
   digit:integer;
begin
   hexStr:='';
   While n>0 do
   begin
      digit:=n mod 16;
      n:= n div 16;
      digit:= digit+byte('0');
      if digit>byte('9') then digit := digit+7;
      hexStr := char(digit)+hexStr;
   end;
   if hexStr.length=0 then hexStr:='0';
   result:=hexStr;
end;


function StrToHex(s:string):integer;
var
  i,c:integer;
  val:integer;
begin
  val:=0;
  for i:=1 to s.length do
  begin
       c:=byte(s[i])-byte('0');
       if c>9 then c:=c-7;
       if (c>15) or (c<0) then
       begin
          ShowMessage('Not an hexadecimal digit<'+s[i]+'> C='+IntToStr(c));
          result:=-1;
          exit;
       end;
       val:= val*16+c;
  end;
  result:=val;
end;

{ TFormRange }
procedure SetRange;
Var
  base:integer;

begin
  with FormRange do
  begin
    if RBHexadecimal.Checked then
       base:=16
    else base:=10;

    if base=10 then
    begin
       StartAddr:=StrToInt(EditStart.text);
       endAddr:=StrToInt(EditEnd.text);
    end
    else
    begin
         StartAddr:=StrToHex(EditStart.text);
         EndAddr:=StrToHex(EditEnd.text);
    end;
    StartHex:=IntToHexStr(startAddr);
    Endhex:=IntToHexStr(EndAddr);
  end;
end;



procedure TFormRange.BtnOkClick(Sender: TObject);
begin
  SetRange;
  if (StartAddr>0) and (EndAddr>=StartAddr) then
  begin
     FormRange.confirm:=true;
     close;
  end
  else
  begin
       ShowMessage('Invalid range');
  end;
end;

procedure TFormRange.EditEndKeyPress(Sender: TObject; var Key: char);
begin
  Key:=Upcase(key);
end;

procedure TFormRange.EditStartKeyPress(Sender: TObject; var Key: char);
begin
  key:=Upcase(key);
end;

procedure TFormRange.BtnCancelClick(Sender: TObject);
begin
  confirm:=false;
  close;
end;

end.

