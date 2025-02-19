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
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItemPort: TMenuItem;
    MenuItemSendHex: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    Separator1: TMenuItem;
    StatusBar1: TStatusBar;
    procedure delayTimer(Sender: TObject);
    procedure EditCmdChange(Sender: TObject);
    procedure EditCmdEditingDone(Sender: TObject);
    procedure EditCmdExit(Sender: TObject);
    procedure EditCmdKeyPress(Sender: TObject; var Key: char);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure MenuItemSendHexClick(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItemPortClick(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
  private
  public

  end;

var
  FormMain: TFormMain;

implementation
uses
  unitPortCfg, eeProgCmd,CommError,FileUtil;

var
DlgPortCfg:TFormPortCfg;

{$R *.lfm}

{ TFormMain }

procedure TFormMain.MenuItem8Click(Sender: TObject);
begin
  close ;
end;

procedure TFormMain.MenuItemPortClick(Sender: TObject);
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

procedure TFormMain.MenuItem6Click(Sender: TObject);
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
  DlgPortCfg.LineDelay:=15;
  DlgPortCfg.CommPortName:='/dev/ttyACM0';
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
end;

procedure TFormMain.Memo1Change(Sender: TObject);
begin

end;

procedure TFormMain.MenuItemSendHexClick(Sender: TObject);


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

