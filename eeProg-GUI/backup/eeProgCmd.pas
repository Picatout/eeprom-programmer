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

     INIT_BAUD:integer=B115200;

var
     serialhandle : LongInt;

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
procedure eeProgCmd(cmd:String;answer:Tmemo);
Send command to programmer and wait for answer.
input:
      cmd:  command string
      answer: Tmemo control collecting programmer answer.
}
procedure eeProgCmd(cmd:String;answer:Tmemo);

implementation
uses
  Forms,serial;




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

begin
  CloseComm; // in case a port is already open
  serialhandle := SerOpen(ComPortName);
  if (serialHandle>0) then
  begin
     Flags:= []; // none
     SerSetParams(serialhandle, 460800, 8, NoneParity, 1,Flags);
  end;
  result:= serialHandle;
end;

{
procedure CloseComm;
Close opened serial port
use: serialHandle local variable
}
procedure CloseComm;
begin
  if (serialHandle>0) then
  begin
  SerSync(serialhandle); // flush out any remaining before closure
  SerFlushOutput(serialhandle); // discard any remaining output
  SerClose(serialhandle);
  serialHandle:=-1;
  end;
end;

{
procedure eeProgCmd(cmd:String;answer:Tmemo);
Send command to programmer and wait for answer.
input:
      cmd:  command string
      answer: Tmemo control collecting programmer answer.
}
procedure eeProgCmd(cmd:String;answer:Tmemo);
var
  s : AnsiString;
  b: array [0..1] of byte;
  ComIn        : integer;
  writecount   : Integer;
  readCount    : Integer;
  status       : LongInt;
  ErrorCode    : Integer;

{
function SerReadLn:integer;
read caracters from serial port until it receive CR character.
}
function SerReadLn:integer;
var
  c : array[0..2] of byte;
  llen:integer;
begin
  c[0]:=0;
  s:='';
  llen:=0;
  while (c[0]<>13) do
  begin
    ReadCount:=SerRead(serialHandle,c,1);
    if (ReadCount>0) then
    begin
         if ((c[0]>31) and (c[0]<127) and (llen<127)) then
         begin
              s := s + char(c[0]);
              inc(llen);
         end;
    end;

  end;
  result:=llen;
end;

begin
  if serialHandle<=0 then exit;
  s:= cmd; // use the input text
  s:= s+#13; // CR
  writecount:= s.length;
  status:= SerWrite(serialhandle, s[1], writecount);
  SerSync(serialhandle);
  ComIn := WriteCount;
  if ComIn > 0 then
  begin
    while (ComIn>0)do
    begin
      ComIn:=serReadLn;
      if (ComIn>0) then
      begin
            answer.lines.Append(s);
            Application.ProcessMessages;
            if (s.length=1) and (s[1]=#35) then ComIn:=0;

      end;
      end;
  end
  else
    writeln('Error: unable to send');
  end;
end.

