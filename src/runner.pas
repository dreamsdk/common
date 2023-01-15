unit Runner;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Settings,
  RunCmdEx,
  SysTools;

const
  UNKNOWN_EXIT_CODE = -1;

type
  { TDreamcastSoftwareDevelopmentKitRunner }
  TDreamcastSoftwareDevelopmentKitRunner = class(TObject)
  private
    fInteractiveShell: Boolean;
    fShellRunnerClientExitCodeTempFileName: TFileName;
    fShellCommand: TRunCommandEx;
    fExecutableMinTTY: TFileName;
    fExecutableShell: TFileName;
    fEnvironmentVariables: TStringList;
    fSettings: TDreamcastSoftwareDevelopmentSettings;
    fWorkingDirectory: TFileName;
    fShellProcessID: LongWord;
    procedure InitializeEnvironment;
    function GetHealthy: Boolean;
    procedure RetrieveEnvironmentVariables;
    procedure HandleNewLine(Sender: TObject; NewLine: string);
    procedure HandleTerminate(Sender: TObject);
    procedure SetWorkingDirectory(AValue: TFileName);
  protected
    function GetClientExitCode: Integer;
    procedure ExecuteThreadWatchShellWindowTitle(const AShellWindowProcessId: LongWord);
    property Settings: TDreamcastSoftwareDevelopmentSettings read fSettings;
  public
    constructor Create;
    destructor Destroy; override;
    function CheckHealty: Boolean;
    procedure StartShell;
    function StartShellCommand(const CommandLine: string): Integer;
    property Healthy: Boolean read GetHealthy;
    property InteractiveShell: Boolean read fInteractiveShell write fInteractiveShell;
    property WorkingDirectory: TFileName read fWorkingDirectory write SetWorkingDirectory;
  end;

implementation

uses
{$IFDEF Windows}
  Windows,
{$ENDIF}
  Process,
{$IF Defined(Unix) OR Defined(Darwin)}
  UTF8Process,
{$ENDIF}
{$ifdef Unix}
  CThreads,
  CMem,
{$ENDIF}
  Interfaces,
  RefBase,
  Version,
  FSTools,
  UITools;

type
  { TShellWindowTitleWatchdogThread }
  TShellWindowTitleWatchdogThread = class(TThread)
  private
    fWindowHandles: TList;
    fWindowTextMap: TIntegerStringMap;
    fShellProcessID: LongWord;
    function UpdateShellWindowTitleText: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    property ShellProcessID: LongWord
      read fShellProcessID write fShellProcessID;
  end;

resourcestring
  MSYSShellNotFound             = 'MinGW/MSYS is not properly installed.';
  ErrorTitle                    = 'Error';

{ TShellWindowTitleWatchdogThread }

function TShellWindowTitleWatchdogThread.UpdateShellWindowTitleText: Boolean;
const
  OLD_TAG = 'MINGW32';
  NEW_TAG = 'DreamSDK';

var
  WinHandle: THandle;
  i, j: Integer;
  CurrentWinText,
  SavedWinText,
  NewWinText: string;

begin
  Result := True;
  for i := 0 to fWindowHandles.Count - 1 do
  begin
    WinHandle := THandle(fWindowHandles[i]);

    SavedWinText := EmptyStr;
    CurrentWinText := GetWindowTitle(WinHandle);
    Result := not IsEmpty(CurrentWinText);

    if Result then
    begin
      j := fWindowTextMap.IndexOf(WinHandle);
      if j = -1 then
        j := fWindowTextMap.Add(WinHandle, CurrentWinText)
      else
        SavedWinText := fWindowTextMap.Data[j];

      if not SameText(CurrentWinText, SavedWinText) then
      begin
        NewWinText := StringReplace(CurrentWinText, OLD_TAG, NEW_TAG, []);

{$IFDEF DEBUG}
        DebugLog(Format('Updating Shell Window (handle: %d) title is required. SavedWinText: "%s", CurrentWinText: "%s", NewWinText: "%s"', [
          WinHandle,
          SavedWinText,
          CurrentWinText,
          NewWinText
        ]));
{$ENDIF}

        fWindowTextMap.Data[j] := NewWinText;
        Result := Result and SetWindowTitle(WinHandle, NewWinText);
        SetWindowIconForProcessId(fShellProcessID);
      end;
    end;
  end;
end;

procedure TShellWindowTitleWatchdogThread.Execute;
var
  ProcessAlive: Boolean;
{$IFDEF DEBUG}
  i: Integer;
  WinHandle: Integer;
{$ENDIF}

begin
{$IFDEF DEBUG}
  DebugLog('Shell Window Process Id: ' + IntToStr(ShellProcessId));
{$ENDIF}

  WaitForWindowCreationForProcessId(ShellProcessId);

{$IFDEF DEBUG}
  DebugLog('Setting up watchdog on Shell Window.');
{$ENDIF}

  // Getting all visible window handles
  FindProcessWindows(fShellProcessID, fWindowHandles);

{$IFDEF DEBUG}
  DebugLog(Format('%d window(s) detected for PID %d:', [
    fWindowHandles.Count,
    fShellProcessID
  ]));

  for i := 0 to fWindowHandles.Count - 1 do
  begin
    WinHandle := Integer(fWindowHandles.Items[i]);
    DebugLog(Format('  Window handle: %d (0x%x)', [
      WinHandle,
      WinHandle
    ]));
  end;
{$ENDIF}

  // Do the watchdog
  ProcessAlive := True;
  while (not Terminated) and (ProcessAlive) do
  begin
    ProcessAlive := UpdateShellWindowTitleText;
    Sleep(1);
  end;

  // Cleaning up stored window handles
  fWindowHandles.Clear;
end;

constructor TShellWindowTitleWatchdogThread.Create;
begin
  inherited Create(True);
  fWindowHandles := TList.Create;
  fWindowTextMap := TIntegerStringMap.Create;
end;

destructor TShellWindowTitleWatchdogThread.Destroy;
begin
  fWindowTextMap.Free;
  fWindowHandles.Free;
  inherited Destroy;
end;

{ TDreamcastSoftwareDevelopmentKitRunner }

procedure TDreamcastSoftwareDevelopmentKitRunner.InitializeEnvironment;
const
  BINARY_DIRECTORY = MSYS_BASE_DIRECTORY + 'bin\';

begin
  fExecutableMinTTY := Settings.InstallPath + BINARY_DIRECTORY + 'mintty.exe';
  fExecutableShell := Settings.InstallPath + BINARY_DIRECTORY + 'sh.exe';
end;

function TDreamcastSoftwareDevelopmentKitRunner.GetHealthy: Boolean;
begin
  Result := FileExists(fExecutableShell)
    and FileExists(fExecutableMinTTY);
end;

procedure TDreamcastSoftwareDevelopmentKitRunner.HandleNewLine(Sender: TObject;
  NewLine: string);
begin
  WriteLn(NewLine);
end;

procedure TDreamcastSoftwareDevelopmentKitRunner.HandleTerminate(Sender: TObject);
begin
{$IFDEF DEBUG}
  DebugLog('*** END ***');
{$ENDIF}
end;

procedure TDreamcastSoftwareDevelopmentKitRunner.SetWorkingDirectory(
  AValue: TFileName);
var
  Temp: TFileName;

begin
  if fWorkingDirectory <> AValue then
  begin
    Temp := IncludeTrailingPathDelimiter(ExpandFileName(
      SysTools.ExpandEnvironmentStrings( Trim( AValue ) ) ));
    if DirectoryExists(Temp) then
      fWorkingDirectory := Temp;
  end;
end;

function TDreamcastSoftwareDevelopmentKitRunner.GetClientExitCode: Integer;
var
  Buffer: TStringList;

begin
  Result := UNKNOWN_EXIT_CODE;
  if FileExists(fShellRunnerClientExitCodeTempFileName) then
  begin
    Buffer := TStringList.Create;
    try
      Buffer.LoadFromFile(fShellRunnerClientExitCodeTempFileName);
      Result := StrToIntDef(Trim(Buffer.Text), UNKNOWN_EXIT_CODE);
    finally
      Buffer.Free;
      SysUtils.DeleteFile(fShellRunnerClientExitCodeTempFileName);
    end;
  end;
end;

procedure TDreamcastSoftwareDevelopmentKitRunner.ExecuteThreadWatchShellWindowTitle(
  const AShellWindowProcessId: LongWord);
var
  WatchShellWindowTitleThread: TShellWindowTitleWatchdogThread;

begin
  // Executing the watchdog thread!
  WatchShellWindowTitleThread := TShellWindowTitleWatchdogThread.Create;
  with WatchShellWindowTitleThread do
  begin
    ShellProcessID := AShellWindowProcessId;
    Start;
  end;

  // Cleaning up
  WatchShellWindowTitleThread.WaitFor;
  FreeAndNil(WatchShellWindowTitleThread);
end;

function TDreamcastSoftwareDevelopmentKitRunner.StartShellCommand(
  const CommandLine: string): Integer;
var
  ClientExitCodeUnixFileName: TFileName;

begin
{$IFDEF DEBUG}
  DebugLog('CommandLine: ' + CommandLine);
{$ENDIF}

  Result := UNKNOWN_EXIT_CODE;
  fShellRunnerClientExitCodeTempFileName := GetTemporaryFileName;

  FreeAndNil(fShellCommand);
  fShellCommand := TRunCommandEx.Create(True);

  // Handle working directory
  if (WorkingDirectory <> EmptyStr) then
  begin
    fShellCommand.WorkingDirectory := WorkingDirectory;
  end;

  fShellCommand.Executable := fExecutableShell;
  fShellCommand.Parameters.Add('--login');
  fShellCommand.Parameters.Add('-i');

  ClientExitCodeUnixFileName := SystemToUnixPath(fShellRunnerClientExitCodeTempFileName);
  with fShellCommand.Environment do
  begin
    Add('_EXTERNAL_COMMAND=' + CommandLine);
    Add('_EXITCODE=' + ClientExitCodeUnixFileName);
  end;

//  SetConsoleTitle(PChar(GetFileDescription));

  fShellCommand.OnNewLine := @HandleNewLine;
  fShellCommand.OnTerminate := @HandleTerminate;

  fShellCommand.Start;

{$IFDEF DEBUG}
  DebugLog('WaitFor is starting...');
{$ENDIF}

  fShellCommand.WaitFor;

{$IFDEF DEBUG}
  DebugLog('WaitFor is done!');
{$ENDIF}

  if (fShellCommand.ExitCode = 0) then
    Result := GetClientExitCode;
end;

procedure TDreamcastSoftwareDevelopmentKitRunner.StartShell;
var
  OurProcess: {$IFDEF Windows}TProcess{$ELSE}TProcessUTF8{$ENDIF};
  ProcessId: LongWord;

begin
  RetrieveEnvironmentVariables;

  OurProcess := {$IFDEF Windows}TProcess{$ELSE}TProcessUTF8{$ENDIF}.Create(nil);
  try
    // Initialize Environment context
    HandleLogonServerVariable(fEnvironmentVariables);
    OurProcess.Environment.AddStrings(fEnvironmentVariables);

    // Handle working directory
    if WorkingDirectory <> EmptyStr then
    begin
      OurProcess.CurrentDirectory := WorkingDirectory;
      OurProcess.Environment.Add('_WORKING_DIRECTORY=' + WorkingDirectory);
    end;

    // Extracted from msys.bat
    if Settings.UseMinTTY then
    begin
      OurProcess.Executable := fExecutableMinTTY;
      // Setting up DreamSDK icon for MinTTY...
      if InteractiveShell then
        OurProcess.Parameters.Add(Format('-i "%s"', [ParamStr(0)]));
      OurProcess.Parameters.Add('/bin/bash');
      OurProcess.Parameters.Add('-l');
    end
    else
    begin
      OurProcess.Executable := fExecutableShell;
      OurProcess.Parameters.Add('--login');
      OurProcess.Parameters.Add('-i');
    end;

    // Execute our process
    OurProcess.Execute;

    // Setting up DreamSDK icon and title window...
    if InteractiveShell then
    begin
      if Settings.UseMinTTY then
      begin
        // MinTTY
        OurProcess.WaitOnExit;
        ProcessId := GetProcessIdFromParentProcessId(OurProcess.ProcessID);
        ExecuteThreadWatchShellWindowTitle(ProcessId);
        WaitForProcessId(ProcessId);
      end
      else
      begin
        // Windows Terminal
        ProcessId := OurProcess.ProcessID;
        ExecuteThreadWatchShellWindowTitle(ProcessId);
        OurProcess.WaitOnExit;
      end;
    end;
  finally
    OurProcess.Free;
  end;
end;

procedure TDreamcastSoftwareDevelopmentKitRunner.RetrieveEnvironmentVariables;
var
  i: Integer;

begin
  for i := 1 to GetEnvironmentVariableCount do
    fEnvironmentVariables.Add(GetEnvironmentString(i));
end;

constructor TDreamcastSoftwareDevelopmentKitRunner.Create;
begin
  fShellProcessID := 0;
  fInteractiveShell := False;
  fEnvironmentVariables := TStringList.Create;
  fSettings := TDreamcastSoftwareDevelopmentSettings.Create;
  Settings.LoadConfiguration;
  InitializeEnvironment;
end;

destructor TDreamcastSoftwareDevelopmentKitRunner.Destroy;
begin
  FreeAndNil(fShellCommand);
  fEnvironmentVariables.Free;
  fSettings.Free;
  inherited Destroy;
end;

function TDreamcastSoftwareDevelopmentKitRunner.CheckHealty: Boolean;
begin
  Result := Healthy;
  if not Healthy then
    MessageBox(0, PChar(MSYSShellNotFound), PChar(ErrorTitle), MB_ICONERROR);
end;

end.

