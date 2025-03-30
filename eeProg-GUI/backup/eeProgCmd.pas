unit eeProgCmd;
{
 communication with the programmer
 using serial port
}
{$mode ObjFPC}{$H+}

interface
uses
  Classes, SysUtils,StdCtrls,UnitPortCfg;


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
procedure eeProgCmd(cmd:String);

{
procedure receiveData(answer:Tmemo);
Receive data from programmer after
sending a command.
}
procedure receiveData(answer:Tmemo);

implementation
uses
  Forms,serial;

const
     HASH:byte=35;   // programmer command line prompt
     CTRL_C:byte=3;  // used to cancel operation
     CTRL_R:byte=18; //used to reboot programmer
     CR:byte=13;    // carriage return (end of line).

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
     s[1]:=CTRL_X; //CTRL_X reboot programmer
     SerWrite(serHandle,s[1],1);
     SerDrain(serhandle);
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
procedure eeProgCmd(cmd:String);
var
  s : AnsiString;
  writecount   : Integer;
  status       : LongInt;


begin
  if serHandle<=0 then exit;
  s:= cmd; // use the input text
  s:= s+char(CR);
  writecount:= s.length;
  status:= SerWrite(serhandle, s[1], writecount);
  SerDrain(serhandle);
  end;

{
procedure receiveData(answer:Tmemo);
Receive data from programmer after
sending a command.
}
procedure receiveData(answer:Tmemo);
var
   s:ansiString;
   rxCount: integer;

   {
   function SerReadLn:integer;
   read caracters from serial port until it receive CR or '#' character.
   }
   function SerReadLn:integer;
   var
    // c : array[0..1] of byte;
      c: array[0..1] of byte;
      readCount,
     llen:integer;
   begin
     c[0]:=0;
     s:='';
     llen:=0;
     while (c[0]<>CR) do
     begin
       ReadCount:=SerRead(serHandle,c,1);
       if (ReadCount>0) then
       begin
            if ((c[0]>31) and (c[0]<127) and (llen<127)) then
            begin
                 s := s + char(c[0]);
                inc(llen);
            end;
            if c[0]=HASH then c[0]:=CR;
       end;

     end;
     result:=llen;
   end;

begin
    rxCount:=serReadln;
    while rxCount > 0 do
    begin
        answer.lines.Append(s);
        Application.ProcessMessages;
        if (s.length=1) and (s[1]=char(HASH)) then break;
        rxCount:=serReadLn;
    end;
end;


end.


