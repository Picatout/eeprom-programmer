unit eeProgCmd;
{
 communication with the programmer
 using serial port
}
{$mode ObjFPC}{$H+}

interface
uses
  Classes, SysUtils,StdCtrls,UnitPortCfg;

const
     HASH:byte=35;   // programmer command line prompt
     CTRL_C:byte=3;  // used to cancel operation
     CTRL_X:byte=24; //used to reboot programmer
     CR:byte=13;    // carriage return (end of line).
     XON:byte=17;
     XOFF:byte=19;
     ACK:byte=6;   // programmation réussie
     NAK:byte=21;  // échec de la programmation

var
   prog_ok:boolean;

{
function OpenComm(ComPortName:String):LongInt;
Open serial port
input:
   ComPortName is serial port name
output:
   serial port handle
}
function OpenComm(ComPortName:String):LongInt;

{
procedure CloseComm;
Close opened serial port
use: serialHandle local variable
}
procedure CloseComm();

{
procedure eeProgCmd(cmd:String);
Send command to programmer and wait for answer.
input:
      cmd:  command string
}
procedure eeProgCmd(cmd:AnsiString);

{
function SerReadLn:integer;
read caracters from serial port until it receive CR or '#' character.
}
function SerReadLn:ansiString;

{
procedure receiveData(answer:Tmemo);
Receive data from programmer after
sending a command.
}
procedure receiveData(answer:Tmemo);

implementation
uses
  Forms,serial;


var
   serHandle:longint;
{
function OpenComm(ComPortName:String;baud:integer):LongInt;
 Open serial port
 input:
    ComPortName is serial port name
    baud is constant (B9600..B460800) defined in UnitPortCfg
 output:
    serial port handle
}
function OpenComm(ComPortName:String):LongInt;
var
  Flags        : TSerialFlags; { set of (RtsCtsFlowControl); }
  s: array[0..1] of byte;

begin
  CloseComm; // in case a port is already open
  serhandle := SerOpen(ComPortName);
  if (serHandle>0) then
  begin
     Flags:= []; // none
     SerSetParams(serhandle, 460800, 8, NoneParity, 1,Flags);
     s[0]:=CTRL_X; //CTRL_X reboot programmer
     SerWrite(serHandle,s[0],1);
     //SerSync(serhandle);
  end;
  result:= serHandle;
end;

{
procedure CloseComm;
Close opened serial port
use: serialHandle local variable
}
procedure CloseComm;
begin
  if (serHandle>0) then
  begin
  SerDrain(serhandle); // flush out any remaining before closure
  SerFlushOutput(serhandle); // discard any remaining output
  SerClose(serhandle);
  serHandle:=-1;
  end;
end;

{
procedure eeProgCmd(cmd:String);
Send command to programmer and wait for answer.
input:
      cmd:  command string
}
procedure eeProgCmd(cmd:AnsiString);
var
  s : AnsiString;
  writecount   : Integer;
  status       : LongInt;


begin
  if serHandle<=0 then exit;
  s:= cmd; // use the input text
  s:= s+char(CR);
  writecount:= s.length;
  WriteCount:= SerWrite(serhandle, s[1], writecount);
  //SerSync(serhandle);
  end;

{
function SerReadLn:AnsiString;
read caracters from serial port until it receive CR or '#' character.
}
function SerReadLn:AnsiString;
var
  s:ansiString;
  c: array[0..2] of byte;
  ReadCount:integer;
begin
  c[0]:=0;
  s:='';
  while true do
  begin
    ReadCount:=SerRead(serHandle,c,1);
    if (ReadCount>0) then
    begin
       s := s + char(c[0]);
       if (c[0]=HASH) or (c[0]=CR) then break;
    end;

  end; //while
  result:=S;
end; // SerReadLn:AnsiString;

{
procedure receiveData(answer:Tmemo);
Receive data from programmer after
sending a command.
}
procedure receiveData(answer:Tmemo);
var
   s:ansiString;


begin //receiveData(answer:Tmemo):string;
    s:=serReadln;
    while s.length > 0 do
    begin
        if (s.length=1) and (s[1]=char(HASH)) then break;
        if (s[1]=char(NAK)) then
           prog_ok:=false
        else
            answer.lines.Append(s);
        Application.ProcessMessages;
        s:=serReadLn;
    end;
end; // receiveData(answer:Tmemo);


end.


