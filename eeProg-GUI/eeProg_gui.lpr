program eeProg_gui;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, unitMain, unitPortCfg, eeProgCmd, CommError, unitRange
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormPortCfg, FormPortCfg);
  Application.CreateForm(TFormCommError, FormCommError);
  Application.CreateForm(TFormRange, FormRange);
  Application.Run;
end.

