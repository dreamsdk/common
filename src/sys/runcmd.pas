unit RunCmd;

{$mode objfpc}{$H+}

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
  SysTools;

{ TRunCommand }

procedure TRunCommand.InitializeProcess;
var
  i: Integer;

begin
  fAbortRequested := False;
  fProcess := {$IFDEF Windows}TProcess{$ELSE}TProcessUTF8{$ENDIF}.Create(nil);
  for i := 1 to GetEnvironmentVariableCount do
    fProcess.Environment.Add(GetEnvironmentString(i));
end;

function TRunCommand.IsValidNewLine(const NewLine: string): Boolean;
begin
  Result := (Trim(NewLine) <> EmptyStr);
end;

function TRunCommand.GetExitCode: Integer;
begin
  Result := fProcess.ExitCode;
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
  fAbortRequested := False;
  Buffer[0] := #0;

{$IFDEF DEBUG}
  DebugLog('  Executable: ' + Executable);
  TempBufferString := StringListToString(Parameters, WhiteSpaceStr);
  DebugLog('  Parameters: ' + TempBufferString);
{$ENDIF}

  fProcess.Executable := Executable;
  fProcess.Parameters.AddStrings(Parameters);
  HandleLogonServerVariable(fEnvironment);
  fProcess.Environment.AddStrings(fEnvironment);

{$IFDEF DEBUG}
  DebugLog('  Environment:');
  for i := 0 to fEnvironment.Count - 1 do
    DebugLog('    ' + fEnvironment[i]);
{$ENDIF}

  // NoConsole / NewProcessGroup are used for handling CTRL+BREAK signals without exiting our app
  // UsePipes / StdErrToOutput for getting stream in real time
  fProcess.Options := [poNoConsole, poNewProcessGroup, poUsePipes, poStdErrToOutput];

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

{$IFDEF DEBUG}
  DebugLog('  Process ID: ' + IntToStr(fProcess.ProcessID));
{$IFDEF DUMP_PROCESS_PIPE}
  i := 0;
{$ENDIF}
{$ENDIF}

  repeat
    FillByte(Buffer, BUF_SIZE, $00);
    BytesRead := fProcess.Output.Read(Buffer, BUF_SIZE);
{$IFDEF DEBUG}
{$IFDEF DUMP_PROCESS_PIPE}
    if BytesRead > 0 then
    begin
      DumpCharArrayToFile(Buffer, Format('dump_%d_%d.bin', [fProcess.ProcessID, i]));
      Inc(i);
    end;
{$ENDIF}
{$ENDIF}
    SetString(NewLine, PChar(@Buffer[0]), BytesRead);
    if IsValidNewLine(NewLine) then
      SendNewLine(NewLine, False);
  until (BytesRead = 0);

  if IsValidNewLine(fPartialLine) then
    SendNewLine(fPartialLine, True);
end;

procedure TRunCommand.KillRunningProcess;
{$IFDEF RELEASE}
{$IFDEF Windows}
const
  CTRL_BREAK_EVENT_TIMEOUT = 1000;
{$ENDIF}
{$ENDIF}

var
  AExitCode: Integer;
{$IFDEF RELEASE}
{$IFDEF Windows}
  ProcessHandle: THandle;
  ProcessID: LongWord;
{$ENDIF}
{$ENDIF}

begin
  AExitCode := -1;
  if Assigned(fProcess) and (fProcess.Running) then
  begin
{$IFDEF RELEASE}
{$IFDEF Windows}
    {
      This code is only working on RELEASE mode.
      Indeed, the SIGINT signal sent destroy the console created in DEBUG mode.
      Since the DEBUG mode is not intended to be distributed, it's safe to do
      this only in RELEASE mode.
      See: https://docs.microsoft.com/en-us/windows/console/ctrl-c-and-ctrl-break-signals
    }
    ProcessID := fProcess.ProcessID;
    ProcessHandle := OpenProcess(JwaWinNT.SYNCHRONIZE, False, ProcessID);
	  if ProcessHandle <> INVALID_HANDLE_VALUE then
    begin
      if AttachConsole(ProcessID) then
      begin
        SetConsoleCtrlHandler(nil, True);
        // Send CTRL+BREAK event to all children processes
        GenerateConsoleCtrlEvent(CTRL_BREAK_EVENT, ProcessID);
        WaitForSingleObject(ProcessHandle, CTRL_BREAK_EVENT_TIMEOUT);
        FreeConsole;
      end;
      CloseHandle(ProcessHandle);
    end;
{$ENDIF}
{$ENDIF}
    fPipeOpened := False;
    fProcess.Terminate(AExitCode);
  end;
end;

constructor TRunCommand.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  fBufferOutput := TStringList.Create;
  fParameters := TStringList.Create;
  fEnvironment := TStringList.Create;
  InitializeProcess;
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

