unit DbgLog;

{$mode ObjFPC}{$H+}

interface

function DebugLog(const Message: string): Boolean;

implementation

{$R embedded/dbglog.rc}

uses
{$IFDEF Windows}
  Windows,
{$ENDIF}
  SysUtils,
  Classes,
{$IFDEF GUI}
  Interfaces,
  Forms,
{$ENDIF}
  Process,
{$IF Defined(Unix) OR Defined(Darwin)}
  , UTF8Process,
{$ENDIF}
  SysTools,
  UITools,
  FSTools;

const
  EMBEDDED_RESOURCE_DBGLOG = 'DBGLOG';
  EMBEDDED_FILENAME_DBGLOG = 'dreamsdk-dbglog-client.exe';

var
  ClientFileName: TFileName;
  ClientProcessId: LongWord;
  ClientWindowHandle: THandle;

procedure InitClient;
begin
  ClientFileName := EmptyStr;
  ClientProcessId := 0;
  ClientWindowHandle := 0;
end;

function CreateClient: Boolean;
var
  WindowHandles: TList;
  ClientTitle,
  ProgramName: string;

  function ExtractClient: Boolean;
  begin
    if not FileExists(ClientFileName) then
      ClientFileName := ExtractEmbeddedFileToWorkingPath(
        EMBEDDED_RESOURCE_DBGLOG, EMBEDDED_FILENAME_DBGLOG);
    Result := FileExists(ClientFileName);
  end;

  procedure RunClient;
  var
    OurProcess: {$IFDEF Windows}TProcess{$ELSE}TProcessUTF8{$ENDIF};

  begin
    OurProcess := {$IFDEF Windows}TProcess{$ELSE}TProcessUTF8{$ENDIF}.Create(nil);
    try
      OurProcess.Executable := ClientFileName;
      OurProcess.ShowWindow := swoShowNormal;
      OurProcess.Execute;
      ClientProcessId := OurProcess.ProcessID;
      WaitForWindowCreationForProcessId(ClientProcessId);
    finally
      OurProcess.Free;
    end;
  end;

begin
  Result := False;

  if ExtractClient then
  begin
    RunClient;

    WindowHandles := TList.Create;
    try
      if FindProcessWindows(ClientProcessId, WindowHandles) then
        if (WindowHandles.Count > 0) then
          // Only one window for our client
          ClientWindowHandle := THandle(WindowHandles[0]);
    finally
      WindowHandles.Free;
    end;

    Result := IsWindow(ClientWindowHandle);

    if Result then
    begin
      ProgramName := ParamStr(0);
{$IFDEF GUI}
      ProgramName := Application.Title;
{$ENDIF}
      ClientTitle := Format('%s :: %s (%d)', [
        GetWindowTitle(ClientWindowHandle),
        ProgramName,
        GetCurrentProcessId
      ]);
      SetWindowTitle(ClientWindowHandle, ClientTitle);
    end;
  end;
end;

procedure FreeClient;
begin
  if IsWindow(ClientWindowHandle) then
    SendMessage(ClientWindowHandle, WM_CLOSE, 0, 0);
  WaitForProcessId(ClientProcessId);
  KillFile(ClientFileName);
end;

function DebugLog(const Message: string): Boolean;
var
  Data: TCopyDataStruct;

begin
  Result := False;

  // Run the DbgLog client if necessary
  if (ClientProcessId = 0) or (not IsWindow(ClientWindowHandle)) then
    CreateClient;

  // Send the string to the client
  if IsWindow(ClientWindowHandle) then
  begin
    Data.dwData := 0;
    Data.cbData := Length(PChar(Message)) + 1; // Need to transfer terminating #0 as well
    Data.lpData := PChar(Message);
    SendMessage(ClientWindowHandle, WM_COPYDATA, 0, {%H-}LongInt(@Data));
    Result := True;
  end;
end;

initialization
  InitClient;

finalization
  FreeClient;

end.

