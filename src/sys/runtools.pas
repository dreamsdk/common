unit RunTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function Run(Executable: string): string; overload;
function Run(Executable, CommandLine: string): string; overload;
function Run(Executable, CommandLine: string; var ProcessId: Integer): string; overload;
function RunAndWait(Executable: string): Boolean;
function RunAndWait(Executable, CommandLine: string): Boolean;
function RunNoWait(Executable: string): Boolean; overload;
function RunNoWait(Executable, CommandLine: string): Boolean; overload;
function RunShellExecute(Executable: string): Boolean; overload;
function RunShellExecute(Executable: string; ShowWindow: Boolean): Boolean;
function RunShellExecute(Executable, CommandLine: string): Boolean; overload;
function RunShellExecute(Executable, CommandLine: string; ShowWindow: Boolean): Boolean; overload;

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
  Process
{$IF Defined(Unix) OR Defined(Darwin)}
  , UTF8Process
{$ENDIF}
  ;

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

function Run(Executable: string): string;
begin
  Result := Run(Executable, '');
end;

function Run(Executable, CommandLine: string): string;
var
  UselessProcessId: Integer;

begin
  UselessProcessId := 0;
  Result := Run(Executable, CommandLine, UselessProcessId);
end;

// Thanks to Marc Weustink and contributors
// http://wiki.freepascal.org/Executing_External_Programs
function Run(Executable, CommandLine: string; var ProcessId: Integer): string;
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
        Application.ProcessMessages;
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

end.

