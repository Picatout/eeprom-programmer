unit unitMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  StdCtrls, ExtCtrls,UnitEepromSize;

type

  { TFormMain }

  TFormMain = class(TForm)
    EditCmd: TEdit;
    Label1: TLabel;
    mItemEeprom: TMenuItem;
    mItemFiles: TMainMenu;
    MemoConsole: TMemo;
    MenuItem1: TMenuItem;
    mItemConfig: TMenuItem;
    mItemView: TMenuItem;
    MItemPortCfg: TMenuItem;
    MItemProg: TMenuItem;
    mItemDump: TMenuItem;
    MItemQuit: TMenuItem;
    mItemAbout: TMenuItem;
    mItemErase: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    Separator1: TMenuItem;
    procedure EditCmdChange(Sender: TObject);
    procedure EditCmdEditingDone(Sender: TObject);
    procedure EditCmdExit(Sender: TObject);
    procedure EditCmdKeyPress(Sender: TObject; var Key: char);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MemoConsoleChange(Sender: TObject);
    procedure mItemAboutClick(Sender: TObject);
    procedure mItemEepromClick(Sender: TObject);
    procedure mItemViewClick(Sender: TObject);
    procedure MItemProgClick(Sender: TObject);
    procedure mItemDumpClick(Sender: TObject);
    procedure MItemPortCfgClick(Sender: TObject);
    procedure MItemQuitClick(Sender: TObject);
    procedure mItemEraseClick(Sender: TObject);
  private
  public
    maxAddr:integer; // last address of eeprom
  end;

var
  FormMain: TFormMain;

implementation
uses
  unitPortCfg, eeProgCmd,CommError,FileUtil,unitRange,unitAbout;

var
DlgPortCfg:TFormPortCfg;

{$R *.lfm}

{ TFormMain }

procedure TFormMain.MItemQuitClick(Sender: TObject);
begin
  close ;
end;

procedure TFormMain.mItemEraseClick(Sender: TObject);

procedure EraseRange;
var
   cmd:string;
begin
  with FormRange do
  begin
    memoConsole.lines.clear;
    cmd:=StartHex+'X'+EndHex;
    eeProgCmd.eeProgCmd(cmd,MemoConsole);
    cmd:=StartHex+'.'+EndHex;
    eeProgCmd.eeProgCmd(cmd,memoConsole);
  end;

end;

begin
  FormRange.ShowModal;
  if FormRange.confirm then
  begin
       EraseRange;
  end;
end;

procedure TFormMain.MItemPortCfgClick(Sender: TObject);
var
 serHandle:longInt;
begin
  with DlgPortCfg do
  begin
       ShowModal;
       if length(CommPortName)>0 then
       begin
            MemoConsole.lines.append(DlgPortCfg.CommPortname);
            serhandle:=OpenComm(DlgPortCfg.CommPortname);
            if  serHandle<0 then FormCommError.show
            else
            begin
                 MemoConsole.lines.append(' opended');
            end;

       end;
  end;
end;

procedure TFormMain.mItemDumpClick(Sender: TObject);
var
  cmd:string;

procedure DumpAsHexFile(FileName:string);
var
  i:integer;
  hexFile:TextFile;
begin
   assignFile(HexFile,FileName);
   try
      rewrite(HexFile);
     for i:=1 to MemoConsole.Lines.count do
     begin
          Writeln(HexFile,memoConsole.lines[i]);
     end;
     CloseFile(hexFile);
   except
       on E: EInOutError do
        ShowMessage('File error: '+ E.Message);
   end;

end;

procedure DumpAsBinFile(FileName:string);

var
  i,j:integer;
  BinFile:file of byte;
  buffer: array[0..15] of byte;
//  bufferIdx:integer;
  line:string;


function is_hex_digit(h:char):boolean;
var
  b:byte;
begin
   result:=false;
   b:=ord(h)-ord('0');
   if b>9 then b:= b-7;
   if (b<16) then result:=true;
end;

function scan(s:string;c:char; i:integer):integer;
{scan s from indice i and stop at c if in s else at end.}
begin
   while (i<=length(s)) and (s[i]<>c) do inc(i);
   result:=i;
end;

function skip(s:string;c:char;i:integer):integer;
{scan s form indice i and stop after c or at end of string}
begin
   while (i<=length(s)) and (s[i]=c) do inc(i);
   result:=i;
end;

function next_hex(s:string;i:integer):integer;
{
scan s from indice i stop at non hex digit
return indice after hex token}
begin
   while (i<=length(s)) and is_hex_digit(s[i]) do inc(i);
   result:=i;
end;

procedure ParseLine(const line:string);
{
 split line in token, each token should be a 2 letters HEX number string.
}
var
  k,i,after:integer;
  token:string;
begin
   k:=1;
   i:=0;
   while k<line.Length do
   begin
      k:=skip(line,' ',k);
      after:=next_hex(line,k);
      if (after-k=2) then
      begin
           token:=line[k..after-1];
           buffer[i]:=byte(UnitRange.StrToHex(token));
           inc(i);
      end;
      if line[k]=';' then k:= line.length else k:=after+1;
      k:=after+1;
   end;
end;

begin
  assignFile(BinFile,FileName);
  try
    rewrite(BinFile);
    for i:=1 to MemoConsole.Lines.count do
    begin
         if length(memoConsole.lines[i])>7 then
         begin
              line:=memoConsole.lines[i][7..length(memoConsole.Lines[i])];
              ParseLine(line);
              for j:=0 to 15 do Write(BinFile,buffer[j]);
         end;
    end;

     CloseFile(BinFile);
  except
      on E: EInOutError do
      ShowMessage('Fie error: '+E.Message);
  end;
end;

begin
  with FormRange do
  begin
       RGFileFormat.Enabled:=true;
       showModal;
       if confirm then
       begin
            cmd:=StartHex+'.'+EndHex;
            MemoConsole.lines.Clear;
            eeprogCmd.eeprogCmd(cmd,MemoConsole);
            if SaveDialog.Execute then
              if RBHexFile.Checked then
              begin
                   DumpAsHexFile(SaveDialog.FileName);
              end
              else
              begin
                   DumpAsBinFile(SaveDialog.FileName);
              end;
       end;
  end;
end;

procedure TFormMain.EditCmdChange(Sender: TObject);
begin

end;


procedure TFormMain.EditCmdEditingDone(Sender: TObject);
begin
end;

procedure TFormMain.EditCmdExit(Sender: TObject);
begin
  MemoConsole.lines.Clear;
  eeProgCmd.eeProgCmd(EditCmd.Text,MemoConsole);
  MemoConsole.SetFocus;
end;

procedure TFormMain.EditCmdKeyPress(Sender: TObject; var Key: char);
begin
  key:=UpCase(key);
   if (key=#13) then FormMain.EditCmdExit(self)
end;

procedure TFormMain.FormActivate(Sender: TObject);
begin

end;

procedure TFormMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseComm;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  DlgPortCfg:=TFormPortCfg.Create(FormMain);
  DlgPortCfg.CommPortName:='/dev/ttyACM0'; // default serial port
  MaxAddr:=8*1024; // default to 8KB EEPROM.
  MItemPortCfgClick(self);
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
end;

procedure TFormMain.MemoConsoleChange(Sender: TObject);
begin

end;

procedure TFormMain.mItemAboutClick(Sender: TObject);
begin
  FormAbout.showModal;
end;

procedure TFormMain.mItemEepromClick(Sender: TObject);
begin
  with FormEeprom do
  begin
       ShowModal;
       if confirm then MaxAddr:=EepromSize-1;
  end;
end;

procedure TFormMain.mItemViewClick(Sender: TObject);
var
    cmd:string;
begin
  with FormRange do
  begin
       ShowModal;
       if Confirm then
       begin
           cmd:=StartHex+'.'+EndHex;
           MemoConsole.lines.Clear;
           eeprogCmd.eeprogCmd(cmd,MemoConsole);
       end;
  end;
end;

procedure TFormMain.MItemProgClick(Sender: TObject);

var
   fExt:string; // file extension

procedure ProgBinFile(FileName:string);
{program eeprom from raw binary file
 start address take from range Dialog
}
var
   BinFile:TFileStream;
   row:array[0..15] of byte;
   line:string;
   count:integer;

procedure convert;
// convert row byte array in hexadecimal string
var
  i:integer;
begin
   line:=': ';
   i:=0;
   while i<count do
   begin
        line:= line+IntToHex(row[i],2)+' ';
        inc(i);
   end;
end;

begin
   BinFile := TFileStream.Create(FileName,fmOpenRead);
   try
   try
      BinFile.position:=0;
      line:=FormRange.StartHex+':';
      eeProgCmd.eeProgCmd(line,MemoConsole);
      repeat
      begin
         count:=BinFile.Read(row[0],16);
         if count>0 then
         begin
            convert;
            eeProgCmd.eeProgCmd(line,MemoConsole);
         end;
      end;
      until count=0;
   finally
           BinFile.free;
   end;
   except
           on E: EInOutError do
             ShowMessage('File error: '+ E.Message);

   end;
end;

procedure ProgHexFile(FileName:string);
var
   hexFile:TextFile;
   line:string;
begin
  AssignFile(hexFile,FileName);
  try
    reset(hexFile);
    while not EOF(HexFile) do
    begin
         Readln(HexFile,line);
         eeProgCmd.eeProgCmd(line,MemoConsole);
    end;
  except
      on E: EInOutError do
       ShowMessage('File error: '+ E.Message);
  end;
  CloseFile(HexFile);

end;

begin
  MemoConsole.lines.Clear;
  With OpenDialog do
  begin
  Title:='program file';
  Filter:='.hex,.txt,.bin';
  FileName:='';
  InitialDir:='.';
  DefaultExt:='.hex';
  if Execute then
  begin
      MemoConsole.lines.append(FileName);
      fext:=ExtractFileExt(FileName);
       if (fext='.bin') then
       begin
          FormRange.RBBinaryFile.checked:=true;
          FormRange.showModal;
          if FormRange.Confirm then
          begin
             ProgBinFile(FileName);
          end;
       end
       else
           ProgHexFile(FileName);
       end;
  end;

end;

end.

