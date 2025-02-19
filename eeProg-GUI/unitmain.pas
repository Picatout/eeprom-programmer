unit unitMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  StdCtrls, ExtCtrls;

type

  { TFormMain }

  TFormMain = class(TForm)
    EditCmd: TEdit;
    Label1: TLabel;
    mItemFiles: TMainMenu;
    Memo1: TMemo;
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
    StatusBar1: TStatusBar;
    procedure EditCmdChange(Sender: TObject);
    procedure EditCmdEditingDone(Sender: TObject);
    procedure EditCmdExit(Sender: TObject);
    procedure EditCmdKeyPress(Sender: TObject; var Key: char);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure mItemViewClick(Sender: TObject);
    procedure MItemProgClick(Sender: TObject);
    procedure mItemDumpClick(Sender: TObject);
    procedure MItemPortCfgClick(Sender: TObject);
    procedure MItemQuitClick(Sender: TObject);
    procedure mItemEraseClick(Sender: TObject);
  private
  public
  end;

var
  FormMain: TFormMain;

implementation
uses
  unitPortCfg, eeProgCmd,CommError,FileUtil,unitRange;

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
    cmd:=StartHex+'X'+EndHex;
    eeProgCmd.eeProgCmd(cmd,memo1);
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
            memo1.lines.append(DlgPortCfg.CommPortname);
            serhandle:=OpenComm(DlgPortCfg.CommPortname);
            if  serHandle<0 then FormCommError.show
            else
            begin
                 memo1.lines.append(' opended');
            end;

       end;
  end;
end;

procedure TFormMain.mItemDumpClick(Sender: TObject);
begin

end;

procedure TFormMain.EditCmdChange(Sender: TObject);
begin

end;


procedure TFormMain.EditCmdEditingDone(Sender: TObject);
begin
end;

procedure TFormMain.EditCmdExit(Sender: TObject);
begin
  memo1.lines.Clear;
  eeProgCmd.eeProgCmd(EditCmd.Text,memo1);
  memo1.SetFocus;
  EditCmd.SetFocus;
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
  DlgPortCfg.CommPortName:='/dev/ttyACM0';
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
end;

procedure TFormMain.Memo1Change(Sender: TObject);
begin

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
           eeprogCmd.eeprogCmd(cmd,memo1);
       end;
  end;
end;

procedure TFormMain.MItemProgClick(Sender: TObject);


procedure ProgHexFile(FileName:string);
var
   hexFile:TextFile;
   line:string;
begin
  memo1.lines.append(FileName);
  AssignFile(hexFile,FileName);
  try
    reset(hexFile);
    while not EOF(HexFile) do
    begin
         Readln(HexFile,line);
         eeProgCmd.eeProgCmd(line,memo1);
    end;
  except
      ShowMessage('Error reading file');

  end;
  CloseFile(HexFile);

end;

begin
  memo1.lines.Clear;
  With OpenDialog do
  begin
  Title:='program hexadecimal file';
  Filter:='.hex,.txt';
  FileName:='';
  InitialDir:='.';
  DefaultExt:='.hex';
  if Execute then
  begin
       ProgHexFile(FileName);
  end;

  end;
end;

end.

