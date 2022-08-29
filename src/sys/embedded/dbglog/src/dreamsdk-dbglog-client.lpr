program DreamSDKDbgLogClient;

{$mode objfpc}{$H+}

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  SysUtils,
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  Main;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Title:='DreamSDK Debug Log Client';
  Application.Scaled:=True;
  Application.Initialize;
  frmMain := TfrmMain.Create(Application);
  frmMain.Caption := Application.Title;
  frmMain.Show;
  Application.Run;
end.

