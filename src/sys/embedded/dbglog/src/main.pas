unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    memDebugLog: TMemo;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

var
  frmMain: TfrmMain;


implementation

{$R *.lfm}

uses
  Windows;

var
  PrevWndProc: WNDPROC;

{ TfrmMain }

function WndCallback(hwnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  Data: CopyDataStruct;
  Message: string;

begin
  Result := Default(LRESULT);
  if (uMsg = WM_COPYDATA) then
  begin
     Data := {%H-}PCopyDataStruct(lParam)^;
     SetString(Message, PChar(Data.lpData), Data.cbData);
     if not SameText(Message, EmptyStr) then
      frmMain.memDebugLog.Lines.Add(Format('[%s] %s', [
        FormatDateTime('YYYY-MM-DD hh:nn:ss', Now()),
        Message
      ]));
  end
  else
    Result := CallWindowProc(PrevWndProc, hwnd, uMsg, WParam, LParam);
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Application.Terminate;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  PrevWndProc := {%H-}Windows.WNDPROC(
    SetWindowLong(Handle, GWL_WNDPROC, {%H-}PtrInt(@WndCallback))
  );
end;

end.

