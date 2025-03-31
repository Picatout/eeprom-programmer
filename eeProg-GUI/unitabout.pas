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

uses vinfo;

{$R *.lfm}
{$IFDEF LINUX}
const VERSTR='eeProg for Linux, version ';
{$ENDIF}
{$IFDEF WINDOWS}
const VERSTR='eeProg for Windows, version ';
{$ENDIF}

const COPYRIGHT='Copyright Jacques Deschênes, 2025' ;
const LICENSE='LICENSE GPL V3';

{ TFormAbout }


procedure TFormAbout.BtnCloseClick(Sender: TObject);
begin
  close;
end;


procedure TFormAbout.FormShow(Sender: TObject);
var
  tf:TextFile;
  line:string;
  VerNum : String;
  Info: TVersionInfo;

  {
  L'unité vinfo et le code de la procédure VersionInfo proviennent de
  //https://forum.lazarus.freepascal.org/index.php?topic=12435.0
  }
  procedure VersionInfo;
  // initialize a bunch of stuff for this app when the form is first opened

       // [0] = Major version, [1] = Minor ver, [3] = Revision, [4] = Build Number
       // The above values can be found in the menu: Project > Project Options > Version Info

  begin
    Info := TVersionInfo.Create;
    Info.Load(HINSTANCE);
    // grab just the Build Number
    VerNum := IntToStr(Info.FixedInfo.FileVersion[0])+'.'+
                IntToStr(Info.FixedInfo.FileVersion[1])+'.'+
                IntToStr(Info.FixedInfo.FileVersion[2]);
    Info.Free;
  end;

begin
  VersionInfo;
  with memoAbout do
  begin
    lines.clear;
    lines.append(VERSTR+VerNum);
    lines.append(COPYRIGHT);
    lines.append(LICENSE);
   end;
    BtnClose.SetFocus;
end;

end.

