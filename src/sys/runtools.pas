unit RunTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  ERunFromThreadSyncError = class(Exception);

function Run(Executable: string; RefreshUI: Boolean = True): string; overload;
function Run(Executable, CommandLine: string;
  RefreshUI: Boolean = True): string; overload;
function Run(Executable, CommandLine: string; WorkingDirectory: TFileName;
  RefreshUI: Boolean = True): string; overload;
function Run(Executable, CommandLine: string; var ProcessId: Integer;
  RefreshUI: Boolean = True): string; overload;
function Run(Executable, CommandLine: string; var ProcessId: Integer;
  var ProcessExitCode: Integer; RefreshUI: Boolean = True): string; overload;
function RunAndWait(Executable: string): Boolean;
function RunAndWait(Executable, CommandLine: string): Boolean;
function RunNoWait(Executable: string): Boolean; overload;
function RunNoWait(Executable, CommandLine: string): Boolean; overload;
function RunShellExecute(Executable: string): Boolean; overload;
function RunShellExecute(Executable: string; ShowWindow: Boolean): Boolean;
function RunShellExecute(Executable, CommandLine: string): Boolean; overload;
function RunShellExecute(Executable, CommandLine: string; ShowWindow: Boolean): Boolean; overload;
function RunSingleCommand(const FullCommandLine: string): Boolean; overload;
function RunSingleCommand(const FullCommandLine: string;
  var OutputBuffer: string; var ProcessExitCode: Integer): Boolean; overload;
function RunSingleCommandUTF16(const FullCommandLine: string;
  var OutputBuffer: string; var ProcessExitCode: Integer): Boolean;
function RunWmic(CommandLine: string; var OutputBuffer: string): Boolean;

implementation

uses
  StrUtils,
  RegExpr,
  LazUTF8,
  LConvEncoding,
{$IFDEF Windows}
  Windows,
  ShellApi,
{$ENDIF}
{$IFDEF GUI}
  Forms,
{$ENDIF}
  Process,
{$IF Defined(Unix) OR Defined(Darwin)}
  , UTF8Process,
{$ENDIF}
  FSTools;

function ParseProcessParameters(const CommandLine: string): string;
begin
  Result := EmptyStr;
  if Length(CommandLine) > 0 then
    Result := StringReplace(CommandLine, ' ', sLineBreak, [rfReplaceAll]);
end;

function RunProgram(Executable, CommandLine: string; SyncExec: Boolean): Boolean;
var
  OurProcess: {$IFDEF Windows}TProcess{$ELSE}TProcessUTF8{$ENDIF};

begin
  OurProcess := {$IFDEF Windows}TProcess{$ELSE}TProcessUTF8{$ENDIF}.Create(nil);
  try
    OurProcess.Executable := Executable;
    OurProcess.Parameters.Text := ParseProcessParameters(CommandLine);
    OurProcess.ShowWindow := swoHide;
    if SyncExec then
      OurProcess.Options := [poWaitOnExit];
    OurProcess.Execute;
    Result := (OurProcess.ExitCode = 0);
  finally
    OurProcess.Free;
  end;
end;

function RunNoWait(Executable: string): Boolean;
begin
  Result := RunNoWait(Executable, EmptyStr);
end;

function RunNoWait(Executable, CommandLine: string): Boolean;
begin
  Result := RunProgram(Executable, CommandLine, False);
end;

function RunAndWait(Executable: string): Boolean;
begin
  Result := RunAndWait(Executable, EmptyStr);
end;

function RunAndWait(Executable, CommandLine: string): Boolean;
begin
  Result := RunProgram(Executable, CommandLine, True);
end;

function Run(Executable: string; RefreshUI: Boolean): string;
begin
  Result := Run(Executable, EmptyStr, RefreshUI);
end;

function Run(Executable, CommandLine: string; RefreshUI: Boolean): string;
var
  UselessProcessId: Integer;

begin
  UselessProcessId := Default(Integer);
  Result := Run(Executable, CommandLine, UselessProcessId, RefreshUI);
end;

function Run(Executable, CommandLine: string; WorkingDirectory: TFileName;
  RefreshUI: Boolean): string;
var
  SavedDirectory: TFileName;

begin
  SavedDirectory := GetCurrentDir;
  SetCurrentDir(WorkingDirectory);
  Result := Run(Executable, CommandLine, RefreshUI);
  SetCurrentDir(SavedDirectory);
end;

function Run(Executable, CommandLine: string; var ProcessId: Integer;
  RefreshUI: Boolean): string;
var
  UselessProcessExitCode: Integer;

begin
  UselessProcessExitCode := Default(Integer);
  Result := Run(Executable, CommandLine, ProcessId, UselessProcessExitCode,
    RefreshUI);
end;

// Thanks to Marc Weustink and contributors
// http://wiki.freepascal.org/Executing_External_Programs
function Run(Executable, CommandLine: string; var ProcessId: Integer;
  var ProcessExitCode: Integer; RefreshUI: Boolean = True): string;
const
  READ_BYTES = 2048;

var
  OutputLines: TStringList;
  MemStream: TMemoryStream;
{$IFDEF Windows}
  OurProcess: TProcess;
{$ELSE}
  OurProcess: TProcessUTF8;
{$ENDIF}
  NumBytes: LongInt;
  BytesRead: LongInt;

begin
  Result := EmptyStr;

{$IFNDEF GUI}
  if RefreshUI then
    Result := EmptyStr; // don't let the compiler complains about this
{$ENDIF}

  // A temp Memorystream is used to buffer the output
  MemStream := TMemoryStream.Create;
  try
    BytesRead := 0;

{$IFDEF Windows}
    OurProcess := TProcess.Create(nil);
{$ELSE}
    OurProcess := TProcessUTF8.Create(nil);
{$ENDIF}
    try
      OurProcess.Executable := Executable;
      OurProcess.Parameters.Text := ParseProcessParameters(CommandLine);

      { We cannot use poWaitOnExit here since we don't know the size of the output.
        On Linux the size of the output pipe is 2 kB; if the output data is more, we
        need to read the data. This isn't possible since we are waiting.
        So we get a deadlock here if we use poWaitOnExit. }
      OurProcess.Options := [poUsePipes, poStderrToOutput];
      OurProcess.ShowWindow := swoHide;
      OurProcess.Execute;

      ProcessId := OurProcess.ProcessID;

      while True do
      begin
{$IFDEF GUI}
        // Refresh the GUI
        if RefreshUI then
          try
            Application.ProcessMessages;
          except
            raise ERunFromThreadSyncError
              .Create('You are running that Run instruction from a Thread, please disable RefreshUI');
          end;
{$ENDIF}

        // make sure we have room
        MemStream.SetSize(BytesRead + READ_BYTES);

        // try reading it
        NumBytes := OurProcess.Output.Read((MemStream.Memory + BytesRead)^, READ_BYTES);
        if NumBytes > 0 then
        begin
          Inc(BytesRead, NumBytes);
        end
        else
          Break;
      end;

      MemStream.SetSize(BytesRead);

      OutputLines := TStringList.Create;
      try
         OutputLines.LoadFromStream(MemStream);
         Result := OutputLines.Text;
      finally
        OutputLines.Free;
      end;

      ProcessExitCode := OurProcess.ExitStatus;
    finally
      OurProcess.Free;
    end;

  finally
    MemStream.Free;
  end;
end;

function RunShellExecute(Executable: string): Boolean;
begin
  Result := RunShellExecute(Executable, EmptyStr);
end;

function RunShellExecute(Executable: string; ShowWindow: Boolean): Boolean;
begin
  Result := RunShellExecute(Executable, EmptyStr, ShowWindow);
end;

function RunShellExecute(Executable, CommandLine: string): Boolean;
begin
  Result := RunShellExecute(Executable, CommandLine, True);
end;

function RunShellExecute(Executable, CommandLine: string; ShowWindow: Boolean): Boolean;
var
  Flag: Integer;

begin
  Result := False;
{$IFDEF Windows}
  Flag := SW_SHOWNORMAL;
  if not ShowWindow then
    Flag := SW_HIDE;

  Result := ShellExecute(0, 'open', PChar(Executable), PChar(CommandLine),
    PChar(ExtractFilePath(Executable)), Flag) > 32;
{$ENDIF}
end;

type
  TLoadFileToFileFunction = function(const FileName: TFileName): string;

function RunSingleCommandInternal(const FullCommandLine: string;
  var ProcessExitCode: Integer; var OutputBuffer: string;
  LoadFileToStringFunction: TLoadFileToFileFunction): Boolean;
var
  Buffer: TStringList;
  BatchFileName,
  OutputFileName,
  ErrorLevelFileName: TFileName;

begin
  Result := False;
  ProcessExitCode := -1;
  OutputBuffer := EmptyStr;

  BatchFileName := ChangeFileExt(SysUtils.GetTempFileName, '.cmd');
  ErrorLevelFileName := ChangeFileExt(SysUtils.GetTempFileName, '.err');
  OutputFileName := ChangeFileExt(SysUtils.GetTempFileName, '.out');

  Buffer := TStringList.Create;
  try
    Buffer.Add('@echo off');
    Buffer.Add(Format('%s > "%s" 2>&1', [FullCommandLine, OutputFileName]));
    Buffer.Add(Format('echo %%errorlevel%% > "%s"', [ErrorLevelFileName]));
    Buffer.SaveToFile(BatchFileName);
  finally
    Buffer.Free;
  end;

  if RunAndWait(BatchFileName) then
  begin
    ProcessExitCode := StrToIntDef(LoadFileToString(ErrorLevelFileName), -1);
    Result := (ProcessExitCode = 0);
    OutputBuffer := LoadFileToStringFunction(OutputFileName);
  end;

  KillFile(BatchFileName);
  KillFile(ErrorLevelFileName);
  KillFile(OutputFileName);
end;

function RunSingleCommand(const FullCommandLine: string): Boolean;
var
  UselessProcessExitCode: Integer;
  UselessOutputBuffer: string;

begin
  UselessProcessExitCode := Default(Integer);
  UselessOutputBuffer := EmptyStr;
  Result := RunSingleCommandInternal(FullCommandLine, UselessProcessExitCode,
    UselessOutputBuffer, @LoadFileToString);
end;

function RunSingleCommand(const FullCommandLine: string;
  var OutputBuffer: string; var ProcessExitCode: Integer): Boolean;
begin
  Result := RunSingleCommandInternal(FullCommandLine, ProcessExitCode,
    OutputBuffer, @LoadFileToString);
end;

function RunSingleCommandUTF16(const FullCommandLine: string;
  var OutputBuffer: string; var ProcessExitCode: Integer): Boolean;
begin
  Result := RunSingleCommandInternal(FullCommandLine, ProcessExitCode,
    OutputBuffer, @LoadUTF16FileToString);
end;

function RunWmic(CommandLine: string; var OutputBuffer: string): Boolean;
var
  Buffer: TStringList;
  ProcessExitCode: Integer;
  TempBuffer: string;

begin
  Result := False;
  Buffer := TStringList.Create;
  try
    TempBuffer := EmptyStr;
    ProcessExitCode := Default(Integer);

    Result := RunSingleCommandUTF16(Format('wmic %s', [CommandLine]),
      TempBuffer, ProcessExitCode);

    if Result then
    begin
      Buffer.Text := TempBuffer;
      if Buffer.Count > 0 then
        Buffer.Delete(0); // remove header
    end;

    OutputBuffer := Buffer.Text;
  finally
    Buffer.Free;
  end;
end;

end.

