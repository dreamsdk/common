unit RunCmd;

{$mode objfpc}{$H+}

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
  SysTools;

{ TRunCommand }

procedure TRunCommand.InitializeProcess;
var
  i: Integer;

begin
  fProcess := {$IFDEF Windows}TProcess{$ELSE}TProcessUTF8{$ENDIF}.Create(nil);
  for i := 1 to GetEnvironmentVariableCount do
    fProcess.Environment.Add(GetEnvironmentString(i));
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

procedure TRunCommand.SendNewLine(const NewLine: string;
  ProcessEnd: Boolean);
begin
  fNewLineBuffer := AdjustLineBreaks(NewLine);
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
{$ENDIF}

begin
  Buffer[0] := #0;

  fProcess.Executable := Executable;
  fProcess.Parameters.AddStrings(Parameters);
  fProcess.Environment.AddStrings(fEnvironment);
  fProcess.Options := [poUsePipes, poStderrToOutPut];
  fProcess.ShowWindow := swoHide;
  fProcess.CurrentDirectory := WorkingDirectory;
  fProcess.Execute;

{$IFDEF DEBUG}
  i := 0;
  WriteLn('PID: ', fProcess.ProcessID);
{$ENDIF}

  repeat
    FillByte(Buffer, BUF_SIZE, $00);
    BytesRead := fProcess.Output.Read(Buffer, BUF_SIZE);
{$IFDEF DEBUG}
    if BytesRead > 0 then
    begin
      DumpCharArrayToFile(Buffer, Format('dump_%d_%d.bin', [fProcess.ProcessID, i]));
      Inc(i);
    end;
{$ENDIF}
    SetString(NewLine, PChar(@Buffer[0]), BytesRead);
    if Trim(NewLine) <> EmptyStr then
      SendNewLine(NewLine, False);
  until (BytesRead = 0);

  if Trim(fPartialLine) <> EmptyStr then
    SendNewLine(fPartialLine, True);
end;

procedure TRunCommand.KillRunningProcess;
var
  AExitCode: Integer;

begin
  AExitCode := -1;
  if Assigned(fProcess) then
  begin
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
  Terminate;
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

