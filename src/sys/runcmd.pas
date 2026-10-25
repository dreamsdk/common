unit RunCmd;

{$mode objfpc}{$H+}

{$DEFINE DUMP_ALL_ENVIRONMENT}
// {$DEFINE DUMP_PROCESS_PIPE}

interface

uses
  Classes,
  SysUtils,
  {$IFDEF Windows}Process{$ELSE}ProcessUTF8{$ENDIF};

type
  TNewLineEvent = procedure(Sender: TObject; NewLine: string) of object;

  { TRunCommand }
  TRunCommand = class(TThread)
  private
    fAbortRequested: Boolean;
    fRootSystemPath: string;
    fPipeOpened: Boolean;
    fProcessEnd: Boolean;
    fPartialLine: string;
    fBufferOutput: TStringList;
    fProcess: {$IFDEF Windows}TProcess{$ELSE}TProcessUTF8{$ENDIF};
    fEnvironment: TStringList;
    fNewLineBuffer: string;
    fExecutable: TFileName;
    fNewLine: TNewLineEvent;
    fParameters: TStringList;
    fWorkingDirectory: TFileName;
    fInheritConsole: Boolean;
    fKeepEscapeSequences: Boolean;
    fForwardInput: Boolean;
    fKillProcessesOnExit: Boolean;
    fInterrupted: Boolean;
{$IFDEF Windows}
    fInputForwarder: TThread;
    fJobHandle: THandle;
    fConsoleOutputMode: LongWord;
    fConsoleOutputModeChanged: Boolean;
    procedure AssignProcessToJob;
    procedure TerminateJob;
    procedure Interrupt;
    procedure StartInputForwarding;
    procedure StopInputForwarding;
{$ENDIF}
    procedure BeginConsoleSharing;
    procedure EndConsoleSharing;
    function GetExitCode: Integer;
    function GetTargetCommandLine: string;
    function GetTargetProcessId: LongWord;
    procedure InitializeProcess;
    function IsValidNewLine(const NewLine: string): Boolean;
    procedure SyncSendNewLineEvent;
    procedure SendNewLine(const NewLine: string; ProcessEnd: Boolean);
  protected
    procedure Execute; override;
    procedure KillRunningProcess;
  public
    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;
    procedure Abort;
    procedure Pause;
    procedure Resume;
    property BufferOutput: TStringList read fBufferOutput;
    property Environment: TStringList read fEnvironment;
    property Executable: TFileName read fExecutable write fExecutable;
    property ExitCode: Integer read GetExitCode;
    property Parameters: TStringList read fParameters;
    property TargetProcessId: LongWord read GetTargetProcessId;
    property TargetCommandLine: string read GetTargetCommandLine;
    property RootSystemPath: string
      read fRootSystemPath write fRootSystemPath;
    property WorkingDirectory: TFileName read fWorkingDirectory
      write fWorkingDirectory;
    (* The process shares our console and standard handles instead of using
       pipes, so programs see a real terminal (colors, stdin, CTRL+C...).
       OnNewLine is never triggered in that mode. *)
    property InheritConsole: Boolean read fInheritConsole write fInheritConsole;
    (* By default, escape sequences (e.g., colors) are removed from the
       lines sent to OnNewLine. *)
    property KeepEscapeSequences: Boolean read fKeepEscapeSequences
      write fKeepEscapeSequences;
    (* When pipes are used (i.e., InheritConsole is False), our standard input
       is forwarded to the process, until EOF. Otherwise, the process reads an
       empty pipe that is never closed. *)
    property ForwardInput: Boolean read fForwardInput write fForwardInput;
    (* All the processes started are terminated when we exit, even if we are
       killed (e.g., by an IDE). When pipes are used, they are terminated on
       CTRL+C/CTRL+BREAK too, as they don't share our console: Interrupted is
       then True. *)
    property KillProcessesOnExit: Boolean read fKillProcessesOnExit
      write fKillProcessesOnExit;
    property Interrupted: Boolean read fInterrupted;
    property OnNewLine: TNewLineEvent read fNewLine write fNewLine;
  end;

implementation

uses
{$IFDEF Windows}
  JwaWinBase,
  JwaWinCon,
  JwaWinNT,
{$ENDIF}
  SysTools,
  StrTools;

{ TRunCommand }

{$IFDEF Windows}
(* Put the process in a job object: every process started from it (e.g., make,
   gcc) belongs to that job too, even with MSYS2, where the parent process IDs
   can't be used for that (fork emulation). *)
procedure TRunCommand.AssignProcessToJob;
var
  LimitInformation: TJobObjectExtendedLimitInformation;

begin
  fJobHandle := CreateJobObject(nil, nil);
  if fJobHandle = 0 then
    Exit;

  // The job is closed when we exit (even if killed), then the processes too
  if KillProcessesOnExit then
  begin
    LimitInformation := Default(TJobObjectExtendedLimitInformation);
    LimitInformation.BasicLimitInformation.LimitFlags :=
      JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
    SetInformationJobObject(fJobHandle, JobObjectExtendedLimitInformation,
      @LimitInformation, SizeOf(LimitInformation));
  end;

  if not AssignProcessToJobObject(fJobHandle, fProcess.ProcessHandle) then
  begin
    CloseHandle(fJobHandle);
    fJobHandle := 0;
  end;
end;

(* Terminate all the processes of the job. This is needed when the CTRL+BREAK
   signal couldn't be sent to the process group (e.g., when our process has
   already a console, like DreamSDK Manager in Debug mode, AttachConsole fails),
   as the children would continue to run and keep the output pipe opened. *)
procedure TRunCommand.TerminateJob;
var
  LogContext: TLogMessageContext;

begin
  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%}, ClassName);
  try
    if fJobHandle <> 0 then
      LogMessage(LogContext, Format('TerminateJobObject: %s', [
        BoolToStr(TerminateJobObject(fJobHandle, 1), True)
      ]));
  finally
    LogMessageExit(LogContext);
  end;
end;

const
  ENABLE_VIRTUAL_TERMINAL_PROCESSING = $0004;

(* When the console is shared, CTRL+C/CTRL+BREAK are received by the process
   too: let it handle them and just wait for it. *)
function IgnoreConsoleCtrlHandler(CtrlType: DWORD): LongBool; stdcall;
begin
  Result := (CtrlType = CTRL_C_EVENT) or (CtrlType = CTRL_BREAK_EVENT);
end;

var
  InterruptibleRunCommand: TRunCommand = nil;

(* When pipes are used, CTRL+C/CTRL+BREAK can't reach the processes, as they
   don't share our console: terminate them instead of exiting right now. *)
function InterruptConsoleCtrlHandler(CtrlType: DWORD): LongBool; stdcall;
begin
  Result := (CtrlType = CTRL_C_EVENT) or (CtrlType = CTRL_BREAK_EVENT);
  if Result and Assigned(InterruptibleRunCommand) then
    InterruptibleRunCommand.Interrupt;
end;

procedure TRunCommand.Interrupt;
begin
  fInterrupted := True;
  TerminateJob;
end;

type
  { TInputForwarderThread }
  (* Copy our standard input to the input pipe of the process, until EOF.
     Only its own handles are used, so it can outlive the TRunCommand. *)
  TInputForwarderThread = class(TThread)
  private
    fSourceHandle: THandle;
    fTargetHandle: THandle;
  protected
    procedure Execute; override;
  public
    constructor Create(const ASourceHandle, ATargetHandle: THandle);
  end;

  TCancelSynchronousIo = function(hThread: THandle): LongBool; stdcall;

constructor TInputForwarderThread.Create(const ASourceHandle,
  ATargetHandle: THandle);
begin
  fSourceHandle := ASourceHandle;
  fTargetHandle := ATargetHandle;
  inherited Create(False);
end;

procedure TInputForwarderThread.Execute;
const
  BUF_SIZE = 4096;

var
  Buffer: array[0..BUF_SIZE - 1] of Byte;
  BytesRead,
  BytesWritten,
  Offset: DWORD;
  Done: Boolean;

begin
  try
    Done := False;
    repeat
      BytesRead := 0;
      // Failure or nothing read: EOF (e.g., pipe closed, CTRL+Z on a console)
      if (not ReadFile(fSourceHandle, @Buffer[0], BUF_SIZE, @BytesRead, nil))
        or (BytesRead = 0) then
        Done := True;

      Offset := 0;
      while (not Done) and (Offset < BytesRead) do
      begin
        BytesWritten := 0;
        // Failure: the process doesn't read its input anymore
        if WriteFile(fTargetHandle, @Buffer[Offset], BytesRead - Offset,
          @BytesWritten, nil) then
          Inc(Offset, BytesWritten)
        else
          Done := True;
      end;
    until Done;
  finally
    // The process receives EOF
    CloseHandle(fTargetHandle);
  end;
end;

(* Our own duplicate of the input pipe is used by the forwarder thread, so the
   TProcess one is closed. If there is nothing to forward, the process just
   receives EOF. *)
procedure TRunCommand.StartInputForwarding;
var
  SourceHandle,
  TargetHandle: THandle;

begin
  SourceHandle := GetStdHandle(STD_INPUT_HANDLE);
  TargetHandle := 0;
  if (SourceHandle <> 0) and (SourceHandle <> INVALID_HANDLE_VALUE)
    and DuplicateHandle(GetCurrentProcess, fProcess.Input.Handle,
      GetCurrentProcess, @TargetHandle, 0, False, DUPLICATE_SAME_ACCESS) then
    fInputForwarder := TInputForwarderThread.Create(SourceHandle, TargetHandle);
  fProcess.CloseInput;
end;

(* The process has exited, but the forwarder may still wait on our standard
   input (e.g., a pipe that is never closed): unblock it if possible. If it's
   still running, it's left behind, as freeing it would wait for it. *)
procedure TRunCommand.StopInputForwarding;
const
  STOP_ATTEMPTS = 10;
  STOP_ATTEMPT_TIMEOUT = 50;

var
  CancelSynchronousIo: TCancelSynchronousIo;
  i: Integer;

begin
  if not Assigned(fInputForwarder) then
    Exit;

  // Not available on Windows XP
  CancelSynchronousIo := TCancelSynchronousIo(
    GetProcAddress(GetModuleHandle('kernel32.dll'), 'CancelSynchronousIo'));

  i := 0;
  while (not fInputForwarder.Finished) and (i < STOP_ATTEMPTS) do
  begin
    if Assigned(CancelSynchronousIo) then
      CancelSynchronousIo(fInputForwarder.Handle);
    WaitForSingleObject(fInputForwarder.Handle, STOP_ATTEMPT_TIMEOUT);
    Inc(i);
  end;

  if fInputForwarder.Finished then
    FreeAndNil(fInputForwarder)
  else
    fInputForwarder := nil;
end;
{$ENDIF}

procedure TRunCommand.BeginConsoleSharing;
{$IFDEF Windows}
var
  OutputHandle: THandle;
  OutputMode: DWORD;

begin
  SetConsoleCtrlHandler(@IgnoreConsoleCtrlHandler, True);

  // Allow ANSI escape sequences (e.g., colors) on legacy consoles
  OutputMode := 0;
  OutputHandle := GetStdHandle(STD_OUTPUT_HANDLE);
  fConsoleOutputModeChanged := GetConsoleMode(OutputHandle, OutputMode)
    and SetConsoleMode(OutputHandle,
      OutputMode or ENABLE_VIRTUAL_TERMINAL_PROCESSING);
  fConsoleOutputMode := OutputMode;
end;
{$ELSE}
begin
end;
{$ENDIF}

procedure TRunCommand.EndConsoleSharing;
begin
{$IFDEF Windows}
  if fConsoleOutputModeChanged then
    SetConsoleMode(GetStdHandle(STD_OUTPUT_HANDLE), fConsoleOutputMode);
  SetConsoleCtrlHandler(@IgnoreConsoleCtrlHandler, False);
{$ENDIF}
end;

procedure TRunCommand.InitializeProcess;
var
  LogContext: TLogMessageContext;
  i: Integer;

begin
  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%}, ClassName);
  try

    fAbortRequested := False;
    fInheritConsole := False;
    fKeepEscapeSequences := False;
    fForwardInput := False;
    fKillProcessesOnExit := False;
    fInterrupted := False;
{$IFDEF Windows}
    fInputForwarder := nil;
    fJobHandle := 0;
    fConsoleOutputMode := 0;
    fConsoleOutputModeChanged := False;
{$ENDIF}
    fProcess := {$IFDEF WINDOWS}TProcess{$ELSE}TProcessUTF8{$ENDIF}.Create(nil);
    for i := 1 to GetEnvironmentVariableCount do
      fProcess.Environment.Add(GetEnvironmentString(i));

  finally
    LogMessageExit(LogContext);
  end;
end;

function TRunCommand.IsValidNewLine(const NewLine: string): Boolean;
begin
  Result := (Trim(NewLine) <> EmptyStr);
end;

function TRunCommand.GetExitCode: Integer;
begin
  Result := -1;
  if Assigned(fProcess) then
    Result := fProcess.ExitCode;
end;

function TRunCommand.GetTargetProcessId: LongWord;
begin
  Result := Default(LongWord);
  if Assigned(fProcess) and IsProcessRunning(fProcess.ProcessID) then
    Result := fProcess.ProcessID;
end;

procedure TRunCommand.SyncSendNewLineEvent;
var
  i, LinesCount: Integer;

  procedure SendLine(Line: string);
  begin
    if not KeepEscapeSequences then
      Line := StripEscapeSequences(Line);
    fBufferOutput.Add(Line);
    fNewLine(Self, Line);
    fPartialLine := '';
  end;

begin
  if Assigned(fNewLine) then
  begin
    if not fProcessEnd then
    begin
      LinesCount := GetSubStrCount(sLineBreak, fNewLineBuffer);
      if LinesCount = 0 then
        fPartialLine := fPartialLine + fNewLineBuffer
      else
      begin
        fPartialLine := fPartialLine + LeftNRight(sLineBreak, fNewLineBuffer, 0);
        SendLine(fPartialLine);
        for i := 1 to LinesCount - 1 do
          SendLine(LeftNRight(sLineBreak, fNewLineBuffer, i));
        fPartialLine := LeftNRight(sLineBreak, fNewLineBuffer, LinesCount);
      end;
    end
    else
      SendLine(fNewLineBuffer);
  end;
end;

procedure TRunCommand.SendNewLine(const NewLine: string; ProcessEnd: Boolean);
begin
  fNewLineBuffer := AdjustLineBreaks(NewLine);
  if (not fPipeOpened) then
    fNewLineBuffer := Left(EscapeStr, fNewLineBuffer);
  fProcessEnd := ProcessEnd;
  Synchronize(@SyncSendNewLineEvent);
end;

procedure TRunCommand.Execute;
const
  BUF_SIZE = 2048;

var
  LogContext: TLogMessageContext;
  Buffer: array[0..BUF_SIZE - 1] of AnsiChar;
  BytesRead: Integer;
  NewLine,
  OriginalProcessPath: string;
{$IFDEF DEBUG}
  i: Integer;
  TempBufferString: string;
{$ENDIF}

begin
  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%}, ClassName);
  try

    fAbortRequested := False;
    fInterrupted := False;
    Buffer[0] := #0;

{$IFDEF DEBUG}
    DebugLog('### RunCommand Invoke ###');
    DebugLog('  Executable: ' + Executable);
    TempBufferString := StringListToString(Parameters, WhiteSpaceStr);
    DebugLog('  Parameters: ' + TempBufferString);
{$ENDIF}

    fProcess.Executable := Executable;
    fProcess.Parameters.AddStrings(Parameters);
    HandleLogonServerVariable(fEnvironment);
    fProcess.Environment.AddStrings(fEnvironment);

    // Handle RootSystemPath, for DreamSDK Runner
    // Injecting the path to avoid circular loop of execution!
    if not IsEmpty(RootSystemPath) then
    begin
      OriginalProcessPath := fProcess.Environment.Values['PATH'];
      fProcess.Environment.Values['PATH'] := RootSystemPath
        + ';' + OriginalProcessPath;
      LogMessage(LogContext, Format('InjectDirectory: [%s]', [
        fProcess.Environment.Values['PATH']
      ]));
    end;

{$IF DEFINED(DEBUG) AND DEFINED(DUMP_ALL_ENVIRONMENT)}
    DebugLog('  Environment:');
    for i := 0 to fEnvironment.Count - 1 do
      DebugLog('    ' + fEnvironment[i]);
{$ENDIF}

    if InheritConsole then
      // The process uses our console and standard handles directly
      fProcess.Options := []
    else
      // NoConsole / NewProcessGroup are used for handling CTRL+BREAK signals without exiting our app
      // UsePipes / StdErrToOutput for getting stream in real time
      fProcess.Options := [
        poNoConsole,
        poNewProcessGroup,
        poUsePipes,
        poStdErrToOutput
      ];
{$IFDEF Windows}
    // Suspended until the process is in the job (see AssignProcessToJob)
    fProcess.Options := fProcess.Options + [poRunSuspended];
{$ENDIF}

{$IFDEF RELEASE}
    fProcess.ShowWindow := swoHide;
{$ELSE}
    fProcess.ShowWindow := swoShowDefault;
{$ENDIF}

    fProcess.CurrentDirectory := WorkingDirectory;

{$IFDEF DEBUG}
    DebugLog('  WorkingDirectory: ' + WorkingDirectory);
{$ENDIF}

    if InheritConsole then
      BeginConsoleSharing;

    fPipeOpened := True;
    fProcess.Execute;
{$IFDEF Windows}
    AssignProcessToJob;
    if KillProcessesOnExit and (not InheritConsole) then
    begin
      InterruptibleRunCommand := Self;
      SetConsoleCtrlHandler(@InterruptConsoleCtrlHandler, True);
    end;
    if ForwardInput and (not InheritConsole) then
      StartInputForwarding;
    fProcess.Resume;
{$ENDIF}

    LogMessage(LogContext, Format('ThreadId: %d, ExecProcessId: %d, ExecCommandLine: "%s", Environment: [%s], ThreadEnvironmentPath: [%s]', [
      ThreadID,
      fProcess.ProcessID,
      GetTargetCommandLine,
      StringListToString(fEnvironment, ArraySeparator),
      StringReplace(fProcess.Environment.Values['PATH'], sLineBreak, ArraySeparator, [rfReplaceAll])
    ]));

{$IFDEF DEBUG}
    DebugLog('  Process ID: ' + IntToStr(fProcess.ProcessID));
{$IFDEF DUMP_PROCESS_PIPE}
    i := 0;
{$ENDIF}
{$ENDIF}

    // No pipes in that mode: the output is already displayed in the console
    if InheritConsole then
      fProcess.WaitOnExit;

    repeat
      BytesRead := 0;
      FillByte(Buffer, BUF_SIZE, $00);
      if Assigned(fProcess.Output) then
      begin
        BytesRead := fProcess.Output.Read(Buffer, BUF_SIZE);
{$IF DEFINED(DEBUG) AND DEFINED(DUMP_PROCESS_PIPE)}
        if BytesRead > 0 then
        begin
          DumpCharArrayToFile(Buffer, Format('dump_%d_%d.bin', [fProcess.ProcessID, i]));
          Inc(i);
        end;
{$ENDIF}
        // Every chunk must be sent, even blank ones, as they may terminate
        // the current partial line
        SetString(NewLine, PChar(@Buffer[0]), BytesRead);
        if BytesRead > 0 then
          SendNewLine(NewLine, False);
      end;
    until (BytesRead = 0);

    if IsValidNewLine(fPartialLine) then
      SendNewLine(fPartialLine, True);

  finally
{$IFDEF Windows}
    StopInputForwarding;
    if InterruptibleRunCommand = Self then
    begin
      SetConsoleCtrlHandler(@InterruptConsoleCtrlHandler, False);
      InterruptibleRunCommand := nil;
    end;
{$ENDIF}
    if InheritConsole then
      EndConsoleSharing;
    LogMessageExit(LogContext);
  end;
end;

function TRunCommand.GetTargetCommandLine: string;
var
  i: Integer;
  ProcessParams: string;

begin
  Result := Default(string);
  if Assigned(fProcess) then
  begin
    ProcessParams := Default(string);
    for i := 0 to fProcess.Parameters.Count - 1 do
      ProcessParams := Format('%s ', [fProcess.Parameters[i]]);
    ProcessParams := Trim(ProcessParams);
    Result := Format('%s %s', [fProcess.Executable, ProcessParams]);
  end;
end;

procedure TRunCommand.KillRunningProcess;
const
  KILL_PROCESS_TIMEOUT = 2000;

var
  LogContext: TLogMessageContext;
  AExitCode: Integer;
  ProcessIdToKill: LongWord;

begin
  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%}, ClassName);
  try

    AExitCode := -1;
    if Assigned(fProcess) and (fProcess.Running) then
    begin
      fPipeOpened := False;

      // Kill the child/real process!
      if TargetProcessId <> 0 then
      begin
        ProcessIdToKill := TargetProcessId; // Save the PID, as TargetProcessId returns value only if PID is active
        LogMessage(LogContext, Format('ProcessId to kill: %d ["%s"]', [ProcessIdToKill, GetTargetCommandLine]));
        if KillProcessByProcessId(TargetProcessId, KILL_PROCESS_TIMEOUT) then
          LogMessage(LogContext, Format('Process %d successfully terminated', [ProcessIdToKill]))
        else
          LogMessage(LogContext, Format('Failed to terminate process %d', [ProcessIdToKill]));

        // Terminate now the process properly, for the next processes...
        LogMessage(LogContext, Format('Calling Terminate with exit code: %d', [AExitCode]));
        fProcess.Terminate(AExitCode);
        LogMessage(LogContext, 'Terminate process ended.');
      end
      else
        LogMessage(LogContext, 'Process has been already killed');
    end;

{$IFDEF Windows}
    // Terminate the remaining processes (e.g., if CTRL+BREAK wasn't sent)
    TerminateJob;
{$ENDIF}

  finally
    LogMessageExit(LogContext);
  end;
end;

constructor TRunCommand.Create(CreateSuspended: Boolean);
var
  LogContext: TLogMessageContext;

begin
  inherited Create(CreateSuspended);

  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%}, ClassName);
  try

    LogMessage(LogContext, Format('ThreadId: %d', [ThreadID]));
    fBufferOutput := TStringList.Create;
    fParameters := TStringList.Create;
    fEnvironment := TStringList.Create;
    InitializeProcess;

  finally
    LogMessageExit(LogContext);
  end;
end;

destructor TRunCommand.Destroy;
begin
  fBufferOutput.Free;
  fProcess.Free;
{$IFDEF Windows}
  if fJobHandle <> 0 then
    CloseHandle(fJobHandle);
{$ENDIF}
  fEnvironment.Free;
  fParameters.Free;
  inherited Destroy;
end;

procedure TRunCommand.Abort;
begin
  KillRunningProcess;
  if (not fAbortRequested) then
  begin
    fAbortRequested := True;
    Terminate;
  end;
end;

procedure TRunCommand.Pause;
begin
  if Assigned(fProcess) then
    fProcess.Suspend;
end;

procedure TRunCommand.Resume;
begin
  if Assigned(fProcess) then
    fProcess.Resume;
end;

end.

