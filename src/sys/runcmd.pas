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
    function GetCommandProcessId: LongWord;
    procedure InitializeProcess;
    function IsValidNewLine(const NewLine: string): Boolean;
    procedure SyncSendNewLineEvent;
    procedure SendNewLine(const NewLine: string; ProcessEnd: Boolean);
  protected
    procedure Execute; override;
    function GetProcessCommandLine: string;
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
    property CommandProcessId: LongWord read GetCommandProcessId;
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
  i: Integer;

begin
  LogMessageEnter('TRunCommand.InitializeProcess');
  try
    try

      fAbortRequested := False;
      fProcess := {$IFDEF WINDOWS}TProcess{$ELSE}TProcessUTF8{$ENDIF}.Create(nil);
      for i := 1 to GetEnvironmentVariableCount do
        fProcess.Environment.Add(GetEnvironmentString(i));

    except
      raise;
    end;
  finally
    LogMessageExit('TRunCommand.InitializeProcess');
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

function TRunCommand.GetCommandProcessId: LongWord;
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
  Buffer: array[0..BUF_SIZE - 1] of AnsiChar;
  BytesRead: Integer;
  NewLine: string;
{$IFDEF DEBUG}
  i: Integer;
  TempBufferString: string;
{$ENDIF}

begin
  LogMessageEnter('TRunCommand.Execute');
  try
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

      LogMessage(Format('TRunCommand.Execute::ThreadId: %d, ExecProcessId: %d, ExecCommandLine: "%s", Environment: [%s]', [
        ThreadID,
        fProcess.ProcessID,
        GetProcessCommandLine,
        StringListToString(fEnvironment, '|')
      ]));

 {$IFDEF DEBUG}
      DebugLog('  Process ID: ' + IntToStr(fProcess.ProcessID));
 {$IFDEF DUMP_PROCESS_PIPE}
      i := 0;
 {$ENDIF}
 {$ENDIF}

      repeat
        FillByte(Buffer, BUF_SIZE, $00);
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
      until (BytesRead = 0);

      if IsValidNewLine(fPartialLine) then
        SendNewLine(fPartialLine, True);

    except
      raise;
    end;
  finally
    LogMessageExit('TRunCommand.Execute');
  end;
end;

function TRunCommand.GetProcessCommandLine: string;
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
{$IF DEFINED(RELEASE) AND DEFINED(WINDOWS)}
const
  CTRL_BREAK_EVENT_TIMEOUT = 1000;
{$ENDIF}

var
  AExitCode: Integer;
{$IF DEFINED(RELEASE) AND DEFINED(WINDOWS)}
  ProcessHandle: THandle;
  ProcessId: LongWord;
  WaitForOutput: LongWord;
{$ENDIF}

begin
  LogMessageEnter('TRunCommand.KillRunningProcess');
  try
    try
      AExitCode := -1;
      if Assigned(fProcess) and (fProcess.Running) then
      begin
{$IF DEFINED(RELEASE) AND DEFINED(WINDOWS)}
        {
          This code is only working on RELEASE mode.
          Indeed, the SIGINT signal sent destroy the console created in DEBUG mode.
          Since the DEBUG mode is not intended to be distributed, it's safe to do
          this only in RELEASE mode.
          See: https://docs.microsoft.com/en-us/windows/console/ctrl-c-and-ctrl-break-signals
        }
        ProcessId := fProcess.ProcessID;
        LogMessage(Format('ProcessId to kill: %d', [ProcessId]));
        ProcessHandle := OpenProcess(JwaWinNT.SYNCHRONIZE, False, CommandProcessId);
	      if ProcessHandle <> INVALID_HANDLE_VALUE then
        begin
          LogMessage(Format('ProcessHandle to kill: %d', [ProcessHandle]));
          if AttachConsole(CommandProcessId) then
          begin
            LogMessage('AttachConsole successfully made');
            if SetConsoleCtrlHandler(nil, True) then
            begin
              LogMessage('SetConsoleCtrlHandler success');
              // Send CTRL+BREAK event to all children processes
              if GenerateConsoleCtrlEvent(CTRL_BREAK_EVENT, CommandProcessId) then
              begin
                LogMessage('GenerateConsoleCtrlEvent success, entering WaitForSingleObject...');
                WaitForOutput := WaitForSingleObject(ProcessHandle, CTRL_BREAK_EVENT_TIMEOUT);
                LogMessage(Format('WaitForSingleObject returned: %d', [WaitForOutput]));
              end
              else
                LogMessage('GenerateConsoleCtrlEvent failed.');
            end
            else
              LogMessage('SetConsoleCtrlHandler failed.');

            LogMessage('FreeConsole called.');
            FreeConsole;
          end
          else
            LogMessage('Unable to AttachConsole...');

          LogMessage('Closing handle.');
          CloseHandle(ProcessHandle);
        end;
{$ENDIF}
        fPipeOpened := False;
        LogMessage(Format('Calling Terminate with exit code: %d', [AExitCode]));
        fProcess.Terminate(AExitCode);
        LogMessage('Terminate ended.');
      end;

    except
      raise;
    end;
  finally
    LogMessageExit('TRunCommand.KillRunningProcess');
  end;
end;

constructor TRunCommand.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  LogMessageEnter('TRunCommand.Create');
  try
    try

      LogMessage(Format('TRunCommand.ThreadId: %d', [ThreadID]));
      fBufferOutput := TStringList.Create;
      fParameters := TStringList.Create;
      fEnvironment := TStringList.Create;
      InitializeProcess;

    except
      raise;
    end;
  finally
    LogMessageExit('TRunCommand.Create');
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

