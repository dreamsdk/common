unit Runner;

{$mode objfpc}{$H+}

// {$DEFINE RUNNER_DETAILED_DEBUG}

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
    fCurrentCommandLine: string;
    fDebugEnabled: Boolean;
    fEmbeddedMode: Boolean;
    fShellCommandOutputBuffer: TStringList;
    fInteractiveShell: Boolean;
    fShellRunnerClientExitCodeTempFileName: TFileName;
    fShellCommand: TRunCommandEx;
    fExecutableMinTTY: TFileName;
    fExecutableShell: TFileName;
    fEnvironmentVariables: TStringList;
    fSettings: TDreamcastSoftwareDevelopmentSettings;
    fWorkingDirectory: TFileName;
    fShellProcessID: LongWord;

    function GetHealthy: Boolean;
    procedure HandleNewLine(Sender: TObject; NewLine: string);
    procedure HandleTerminate(Sender: TObject);
    procedure Initialize(const EmbeddedMode: Boolean);
    procedure InitializeEnvironment;
    procedure RetrieveEnvironmentVariables;
    procedure SetWorkingDirectory(AValue: TFileName);
  protected
    function GetClientExitCode: Integer;
    function GetShellCommandOutputBuffer: string;
    procedure ExecuteShellWindowWatchdogThread(const AShellWindowProcessId: LongWord);
    property Settings: TDreamcastSoftwareDevelopmentSettings read fSettings;
  public
    constructor Create; overload;
    constructor Create(const EmbeddedMode: Boolean); overload;
    destructor Destroy; override;

    function CheckHealty: Boolean;
    procedure StartShell;
    function StartShellCommand(const CommandLine: string): Integer; overload;
    function StartShellCommand(const CommandLine: string;
      var OutputBuffer: string): Integer; overload;

    property Healthy: Boolean
      read GetHealthy;

    (* Activate this flag to debug the command executed in the Shell. *)
    property DebugEnabled: Boolean
      read fDebugEnabled write fDebugEnabled;

    (* Use this to launch an interactive/user-friendly Shell. *)
    property InteractiveShell: Boolean
      read fInteractiveShell write fInteractiveShell;

    (* Defines the working/current directory for the executed command. *)
    property WorkingDirectory: TFileName
      read fWorkingDirectory write SetWorkingDirectory;
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
  { TShellWindowWatchdogThread }
  TShellWindowWatchdogThread = class(TThread)
  private
    fShellProcessID: LongWord;
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

procedure TShellWindowWatchdogThread.Execute;
begin
{$IFDEF DEBUG}
{$IFDEF RUNNER_DETAILED_DEBUG}
  DebugLog('Shell Window Process Id: ' + IntToStr(ShellProcessId));
{$ENDIF}
{$ENDIF}

  WaitForWindowCreationForProcessId(ShellProcessId);

  if IsProcessRunning(ShellProcessID) then
    SetWindowIconForProcessId(ShellProcessID);
end;

constructor TShellWindowWatchdogThread.Create;
begin
  inherited Create(True);
end;

destructor TShellWindowWatchdogThread.Destroy;
begin
  inherited Destroy;
end;

{ TDreamcastSoftwareDevelopmentKitRunner }

procedure TDreamcastSoftwareDevelopmentKitRunner.InitializeEnvironment;
begin
  fExecutableMinTTY := GetUserBinariesBaseDirectory + 'mintty.exe';
  fExecutableShell := GetUserBinariesBaseDirectory + 'sh.exe';
end;

function TDreamcastSoftwareDevelopmentKitRunner.GetHealthy: Boolean;
begin
  Result := FileExists(fExecutableShell)
    and FileExists(fExecutableMinTTY);
end;

procedure TDreamcastSoftwareDevelopmentKitRunner.HandleNewLine(Sender: TObject;
  NewLine: string);
begin
  if not fEmbeddedMode then
    WriteLn(NewLine);
  fShellCommandOutputBuffer.Add(NewLine);
end;

procedure TDreamcastSoftwareDevelopmentKitRunner.HandleTerminate(Sender: TObject);
begin
{$IFDEF DEBUG}
{$IFDEF RUNNER_DETAILED_DEBUG}
  DebugLog('*** END ***');
{$ENDIF}
{$ENDIF}
end;

procedure TDreamcastSoftwareDevelopmentKitRunner.Initialize(
  const EmbeddedMode: Boolean);
begin
  fCurrentCommandLine := EmptyStr;
  fEmbeddedMode := EmbeddedMode;
  fShellCommandOutputBuffer := TStringList.Create;
  fShellProcessID := 0;
  fDebugEnabled := False;
  fInteractiveShell := False;
  fEnvironmentVariables := TStringList.Create;
  fSettings := TDreamcastSoftwareDevelopmentSettings.Create;
  Settings.LoadConfiguration;
  InitializeEnvironment;
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

function TDreamcastSoftwareDevelopmentKitRunner.GetShellCommandOutputBuffer: string;
{$IFDEF DEBUG}
{$IFDEF RUNNER_DETAILED_DEBUG}
var
  DebugTag: string;
{$ENDIF}
{$ENDIF}

begin
  Result := fShellCommandOutputBuffer.Text;
{$IFDEF DEBUG}
{$IFDEF RUNNER_DETAILED_DEBUG}
  DebugTag := Format('GetShellCommandOutputBuffer ["%s"]', [fCurrentCommandLine]);
  DebugLog('START :: ' + DebugTag + ' >>>' + sLineBreak
    + '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~' + sLineBreak
    + sLineBreak
    + Result + sLineBreak
    + sLineBreak
    + '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~' + sLineBreak
    + '<<< END :: ' + DebugTag + sLineBreak
    + sLineBreak
  );
{$ENDIF}
{$ENDIF}
  fShellCommandOutputBuffer.Clear;
end;

procedure TDreamcastSoftwareDevelopmentKitRunner.ExecuteShellWindowWatchdogThread(
  const AShellWindowProcessId: LongWord);
var
  ShellWindowWatchdogThread: TShellWindowWatchdogThread;

begin
  // Executing the watchdog thread!
  ShellWindowWatchdogThread := TShellWindowWatchdogThread.Create;
  with ShellWindowWatchdogThread do
  begin
    ShellProcessID := AShellWindowProcessId;
    Start;
  end;

  // Cleaning up
  ShellWindowWatchdogThread.WaitFor;
  FreeAndNil(ShellWindowWatchdogThread);
end;

function TDreamcastSoftwareDevelopmentKitRunner.StartShellCommand(
  const CommandLine: string; var OutputBuffer: string): Integer; overload;
var
  ClientExitCodeUnixFileName: TFileName;

begin
{$IFDEF DEBUG}
{$IFDEF RUNNER_DETAILED_DEBUG}
  DebugLog('CommandLine: ' + CommandLine);
{$ENDIF}
{$ENDIF}

  Result := UNKNOWN_EXIT_CODE;
  fShellRunnerClientExitCodeTempFileName := GetTemporaryFileName;

  FreeAndNil(fShellCommand);
  fShellCommand := TRunCommandEx.Create(True);

  fShellCommand.Executable := fExecutableShell;
  fShellCommand.Parameters.Add('--login');
  if DebugEnabled then
    fShellCommand.Parameters.Add('-x');
  fShellCommand.WorkingDirectory := Self.WorkingDirectory;

  fCurrentCommandLine := CommandLine;
  ClientExitCodeUnixFileName := SystemToUnixPath(fShellRunnerClientExitCodeTempFileName);
  with fShellCommand.Environment do
  begin
    Add('_EXTERNAL_COMMAND=' + fCurrentCommandLine);
    Add('_EXITCODE=' + ClientExitCodeUnixFileName);
  end;

  fShellCommand.OnNewLine := @HandleNewLine;
  fShellCommand.OnTerminate := @HandleTerminate;

  fShellCommand.Start;

{$IFDEF DEBUG}
{$IFDEF RUNNER_DETAILED_DEBUG}
  DebugLog('WaitFor is starting...');
{$ENDIF}
{$ENDIF}

  fShellCommand.WaitFor;

{$IFDEF DEBUG}
{$IFDEF RUNNER_DETAILED_DEBUG}
  DebugLog('WaitFor is done!');
{$ENDIF}
{$ENDIF}

  if (fShellCommand.ExitCode = 0) then
    Result := GetClientExitCode;

  OutputBuffer := GetShellCommandOutputBuffer;
end;

function TDreamcastSoftwareDevelopmentKitRunner.StartShellCommand(
  const CommandLine: string): Integer; overload;
var
  Dummy: string;

begin
  Dummy := EmptyStr;
  Result := StartShellCommand(CommandLine, Dummy);
end;

procedure TDreamcastSoftwareDevelopmentKitRunner.StartShell;
var
  OurProcess: {$IFDEF Windows}TProcess{$ELSE}TProcessUTF8{$ENDIF};
  ProcessId: LongWord;

begin
  RetrieveEnvironmentVariables;

  // We use here standard TProcess (just for launching the Shell)
  OurProcess := {$IFDEF Windows}TProcess{$ELSE}TProcessUTF8{$ENDIF}.Create(nil);
  try
    // Initialize Environment context
    HandleLogonServerVariable(fEnvironmentVariables);
    OurProcess.Environment.AddStrings(fEnvironmentVariables);

    // Handle working directory
    if WorkingDirectory <> EmptyStr then
    begin
      OurProcess.CurrentDirectory := WorkingDirectory;
      OurProcess.Environment.Add('_WORKING_DIRECTORY='
        + SystemToDreamSdkPath(WorkingDirectory));
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
      if InteractiveShell then
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
        WaitForProcessId(ProcessId);
      end
      else
      begin
        // Windows Terminal
        ProcessId := OurProcess.ProcessID;
        ExecuteShellWindowWatchdogThread(ProcessId);
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
  Initialize(False);
end;

constructor TDreamcastSoftwareDevelopmentKitRunner.Create(
  const EmbeddedMode: Boolean);
begin
  Initialize(EmbeddedMode);
end;

destructor TDreamcastSoftwareDevelopmentKitRunner.Destroy;
begin
  if Assigned(fShellCommandOutputBuffer) then
    fShellCommandOutputBuffer.Free;
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

