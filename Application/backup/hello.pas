unit hello;

{$mode ObjFPC}{$H+}

interface
uses
  Classes, SysUtils,StdCtrls;

procedure read_eeprom(cmd:String;answer:Tmemo);

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

procedure read_eeprom(cmd:String;answer:Tmemo);
var
  serialhandle : LongInt;
  ComPortName  : String;
  s,tmpstr,txt : String;
  c:char;
  ComIn        : String;
  ComPortNr    : Integer;
  writecount   : Integer;
  readCount    : Integer;
  status       : LongInt;
  Flags        : TSerialFlags; { set of (RtsCtsFlowControl); }
  ErrorCode    : Integer;

procedure SerReadLn;
var
  llen:LongInt;
begin
  c:=#0;
  llen:=length(AnsiString(s));
  while (c<>#13) do
  begin
    status:=SerRead(serialHandle,c,1);
    if (c=#35) then
      status:= -1 // # => end serial read
    else if (status>=0) then
    begin
         if (c=#8) then s := s[1..length(s)-1]
         else if ((c>#31) and (c<#127) and (llen<127)) then
         begin
              s := s + c;
              inc(llen);
         end;
    end;
    if ((status>=0) and (c=#13)) then status:=SerRead(serialHandle,c,1); // discard LF
  end;
end;

begin
  ComPortNr:= 3;
  tmpstr:= '';
  txt:= '0.F';

  str(ComPortNr, tmpstr);

  ComPortName:= 'COM'+tmpstr+':';

  serialhandle := SerOpen(ComPortName);
  Flags:= []; // none
  SerSetParams(serialhandle, 115200, 8, NoneParity, 1, Flags);

  s:= cmd; // use the input text
  s:= s+#13+#10; // CR
  writecount:= length(s);

  status:= SerWrite(serialhandle, s[1], writecount);
  SerSync(serialhandle);
  if status > 0 then
  begin
    s:= '';
    while (status>=0)do
    begin
      SerReadln();
      if (status>=0) then answer.lines.Append(s);
    end;
  end
  else
    writeln('Error: unable to send');

  SerSync(serialhandle); // flush out any remaining before closure

  SerFlushOutput(serialhandle); // discard any remaining output

  SerClose(serialhandle);
  end;

end.

