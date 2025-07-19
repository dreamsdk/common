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
    property OnNewLine: TNewLineEvent read fNewLine write fNewLine;
  end;

implementation

uses
{$IFDEF RELEASE}
{$IFDEF Windows}
  JwaWinBase,
  JwaWinNT,
  JwaWinCon,
{$ENDIF}
{$ENDIF}
  SysTools,
  StrTools;

{ TRunCommand }

procedure TRunCommand.InitializeProcess;
var
  LogContext: TLogMessageContext;
  i: Integer;

begin
  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%}, ClassName);
  try

    fAbortRequested := False;
    fProcess := {$IFDEF WINDOWS}TProcess{$ELSE}TProcessUTF8{$ENDIF}.Create(nil);
    for i := 1 to GetEnvironmentVariableCount do
      fProcess.Environment.Add(GetEnvironmentString(i));

  finally
    LogMessageExit(LogContext);
  end;
end;

function TRunCommand.IsValidNewLine(const NewLine: string): Boolean;
begin
  Result := (Trim(NewLine) <> EmptyStr) and (not StartsWith(#$1B, NewLine));
end;

function TRunCommand.GetExitCode: Integer;
begin
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

  procedure SendLine(const Line: string);
  begin
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

    // NoConsole / NewProcessGroup are used for handling CTRL+BREAK signals without exiting our app
    // UsePipes / StdErrToOutput for getting stream in real time
    fProcess.Options := [
      poNoConsole,
      poNewProcessGroup,
      poUsePipes,
      poStdErrToOutput
    ];

{$IFDEF RELEASE}
    fProcess.ShowWindow := swoHide;
{$ELSE}
    fProcess.ShowWindow := swoShowDefault;
{$ENDIF}

    fProcess.CurrentDirectory := WorkingDirectory;

{$IFDEF DEBUG}
    DebugLog('  WorkingDirectory: ' + WorkingDirectory);
{$ENDIF}

    fPipeOpened := True;
    fProcess.Execute;

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
        SetString(NewLine, PChar(@Buffer[0]), BytesRead);
        if IsValidNewLine(NewLine) then
          SendNewLine(NewLine, False);
      end;
    until (BytesRead = 0);

    if IsValidNewLine(fPartialLine) then
      SendNewLine(fPartialLine, True);

  finally
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

