unit eeProgCmd;

{$mode ObjFPC}{$H+}

interface
uses
  Classes, SysUtils,StdCtrls;

procedure eeProgCmd(cmd:String;answer:Tmemo);

implementation
{
  Usage:
  TestSerialPortCom
    Uses port COM3.
    print '0.F<CR>'
    read result and display it.
    and leave
}
uses
  serial, crt;

procedure eeProgCmd(cmd:String;answer:Tmemo);
var
  serialhandle : LongInt;
  ComPortName  : String;
  s,tmpstr,txt : AnsiString;
  ComIn        : integer;
  ComPortNr    : Integer;
  writecount   : Integer;
  readCount    : Integer;
  status       : LongInt;
  Flags        : TSerialFlags; { set of (RtsCtsFlowControl); }
  ErrorCode    : Integer;


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
  ComPortNr:= 3;
  tmpstr:= '';
  txt:= '0.F';

  str(ComPortNr, tmpstr);

  ComPortName:= 'COM'+tmpstr+':';

  serialhandle := SerOpen(ComPortName);
  Flags:= []; // none
  SerSetParams(serialhandle, 115200, 8, NoneParity, 1,Flags);

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
            if (s[1]=#35) then ComIn:=0;

      end;
    end;
  end
  else
    writeln('Error: unable to send');

  SerSync(serialhandle); // flush out any remaining before closure

  SerFlushOutput(serialhandle); // discard any remaining output

  SerClose(serialhandle);
  end;

end.

