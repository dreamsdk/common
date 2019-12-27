unit SysTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FGL;

const
  WhiteSpaceStr = ' ';
  ArraySeparator = '|';
  STRING_DATE_FORMAT = 'YYYY-MM-DD @ HH:mm:ss';

type
  TIntegerList = specialize TFPGList<Integer>;
  TStringIntegerMap = specialize TFPGMap<string, Integer>;

{$IFDEF DEBUG}procedure DebugLog(const Message: string);{$ENDIF}
{$IFDEF GUI}procedure Delay(Milliseconds: Integer);{$ENDIF}
{$IFDEF DEBUG}procedure DumpCharArrayToFile(A: array of Char; const FileName: TFileName);{$ENDIF}
function EndsWith(const SubStr, S: string): Boolean;
function ExpandEnvironmentStrings(const InputString: string): string;
function ExtractStr(LeftSubStr, RightSubStr, S: string): string;
function ExtremeRight(SubStr: string; S: string): string;
function GetApplicationPath: TFileName;
function GetSubStrCount(SubStr, S: string): Integer;
function IsInString(const SubStr, S: string): Boolean;
function IsRegExMatch(const InputValue, RegEx: string): Boolean;
function KillFile(const FileName: TFileName): Boolean;
function Left(SubStr: string; S: string): string;
function LeftNRight(SubStr, S: string; N: Integer): string;
function LoadFileToString(FileName: TFileName): string;
function LoadUTF16FileToString(const FileName: TFileName): string;
function PatchTextFile(const FileName: TFileName; OldValue, NewValue: string): Boolean;
function Right(SubStr: string; S: string): string;
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
function UnixPathToSystem(const PathName: TFileName): TFileName;
procedure SaveStringToFile(const InString: string; FileName: TFileName);
function StartsWith(const SubStr, S: string): Boolean;
procedure StringToStringList(const S, Delimiter: string; SL: TStringList);
function StringListToString(SL: TStringList; const Delimiter: string): string;
function StringListSubstringIndexOf(SL: TStringList; const SubStr: string): Integer;
function SuppressUselessWhiteSpaces(const S: string): string;
function SystemToUnixPath(const UnixPathName: TFileName): TFileName;
function UncompressZipFile(const FileName, OutputDirectory: TFileName): Boolean;

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
  Zipper,
  Process
{$IF Defined(Unix) OR Defined(Darwin)}
  , UTF8Process
{$ENDIF}
  ;

var
  ApplicationPath: TFileName = '';

// Thanks Michel (Phidels.com)
function GetSubStrCount(SubStr, S: string): Integer;
begin
  result:=0;
  while pos(substr,s)<>0 do
  begin
    S:=Right(substr,s);
    inc(result);
  end;
end;

// Thanks Michel (Phidels.com)
function LeftNRight(SubStr, S: string; N: Integer): string;
var i:integer;
begin
  S:=S+substr;
  for i:=1 to n do
  begin
    S:=copy(s, pos(substr, s)+length(substr), length(s)-pos(substr, s)+length(substr));
  end;
  result:=copy(s, 1, pos(substr, s)-1);
end;

// Thanks Michel (Phidels.com)
function Right(SubStr: string; S: string): string;
begin
  if pos(substr,s)=0 then result:='' else
    result:=copy(s, pos(substr, s)+length(substr), length(s)-pos(substr, s)+length(substr));
end;

// Thanks Michel (Phidels.com)
function Left(SubStr: string; S: string): string;
begin
  result:=copy(s, 1, pos(substr, s)-1);
end;

// Thanks Michel (Phidels.com)
function ExtractStr(LeftSubStr, RightSubStr, S: string): string;
begin
  Result := Left(RightSubStr, Right(LeftSubStr, S));
end;

// Thanks Michel (Phidels.com)
function ExtremeRight(SubStr: string; S: string): string;
begin
  Repeat
    S:= Right(substr,s);
  until pos(substr,s)=0;
  result:=S;
end;

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

function IsInString(const SubStr, S: string): Boolean;
begin
  Result := Pos(LowerCase(SubStr), LowerCase(S)) > 0;
end;

function SuppressUselessWhiteSpaces(const S: string): string;
var
  Buffer: TStringList;
  i: Integer;
  Entry, Separator: string;

begin
  Result := EmptyStr;
  Buffer := TStringList.Create;
  try
    Buffer.Text := StringReplace(Trim(S), WhiteSpaceStr, sLineBreak, [rfReplaceAll]);
    Separator := EmptyStr;
    for i := 0 to Buffer.Count - 1 do
    begin
      Entry := Buffer[i];
      if not SameText(Entry, EmptyStr) then
        Result := Result + Separator + Entry;
      Separator := WhiteSpaceStr;
    end;
    Result := Trim(Result);
  finally
    Buffer.Free;
  end;
end;

function IsRegExMatch(const InputValue, RegEx: string): Boolean;
var
  RegexObj: TRegExpr;

begin
  RegexObj := TRegExpr.Create;
  try
    RegexObj.Expression := RegEx;
    Result := RegexObj.Exec(InputValue);
  finally
    RegexObj.Free;
  end;
end;

function GetApplicationPath: TFileName;
var
  Path: TFileName;
{$IFDEF Darwin}
  i: Integer;
{$ENDIF}

begin
  if (ApplicationPath = EmptyStr) then
  begin
    Path := ExtractFilePath(ParamStr(0));
{$IFDEF Darwin}
    i := Pos('.app', Path);
    if i > 0 then
    begin
      i := LastDelimiter('/', Copy(Path, 1, i));
      Path := Copy(Path, 1, i);
    end;
{$ENDIF}
    ApplicationPath := IncludeTrailingPathDelimiter(Path);
  end;
  Result := ApplicationPath;
end;

function UnixPathToSystem(const PathName: TFileName): TFileName;
begin
  Result := StringReplace(PathName, '/', DirectorySeparator, [rfReplaceAll]);
  Result := IncludeTrailingPathDelimiter(Copy(Result, 2, Length(Result) - 1));
end;

function SystemToUnixPath(const UnixPathName: TFileName): TFileName;
begin
  Result := StringReplace(UnixPathName, DirectorySeparator, '/', [rfReplaceAll]);
  Result := '/' + StringReplace(Result, ':', EmptyStr, [rfReplaceAll]);
end;

function EndsWith(const SubStr, S: string): Boolean;
begin
  if SubStr = EmptyStr then
    Result := True
  else
    Result := AnsiEndsStr(SubStr, S);
end;

function StartsWith(const SubStr, S: string): Boolean;
begin
  Result := AnsiStartsStr(SubStr, S);
end;

{$IFDEF DEBUG}
procedure DebugLog(const Message: string);
begin
{$IFDEF CONSOLE}
  WriteLn(Message);
{$ENDIF}
end;
{$ENDIF}

function StringListToString(SL: TStringList; const Delimiter: string): string;
begin
  Result := EmptyStr;
  if Assigned(SL) then
    Result := Trim(StringReplace(Trim(SL.Text), sLineBreak, Delimiter, [rfReplaceAll]));
end;

procedure StringToStringList(const S, Delimiter: string; SL: TStringList);
begin
  if Assigned(SL) then
    SL.Text := StringReplace(Trim(S), Delimiter, sLineBreak, [rfReplaceAll]);
end;

function LoadFileToString(FileName: TFileName): string;
var
  Buffer: TStringList;

begin
  Result := EmptyStr;
  Buffer := TStringList.Create;
  try
    Buffer.LoadFromFile(FileName);
    Result := Trim(Buffer.Text);
  finally
    Buffer.Free;
  end;
end;

procedure SaveStringToFile(const InString: string; FileName: TFileName);
var
  Buffer: TStringList;

begin
  Buffer := TStringList.Create;
  try
    Buffer.Add(InString);
    Buffer.SaveToFile(FileName);
  finally
    Buffer.Free;
  end;
end;

function StringListSubstringIndexOf(SL: TStringList; const SubStr: string): Integer;
var
  i: Integer;

begin
  Result := -1;
  if Assigned(SL) then
  begin
    i := 0;
    while (i < SL.Count) and (Result = -1) do
    begin
      if IsInString(SubStr, SL[i]) then
        Result := i;
      Inc(i);
    end;
  end;
end;

function UncompressZipFile(const FileName, OutputDirectory: TFileName): Boolean;
var
  UnZipper: TUnZipper;

begin
  Result := False;
  if FileExists(FileName) then
  begin
    UnZipper := TUnZipper.Create;
    try
      UnZipper.FileName := FileName;
      UnZipper.OutputPath := OutputDirectory;
      UnZipper.Examine;
      UnZipper.UnZipAllFiles;
      Result := True;
    finally
      UnZipper.Free;
    end;
  end;
end;

function PatchTextFile(const FileName: TFileName; OldValue, NewValue: string): Boolean;
var
  Buffer: TStringList;

begin
  Result := False;
  if FileExists(FileName) then
  begin
    Buffer := TStringList.Create;
    try
      Buffer.LoadFromFile(FileName);
      if IsInString(OldValue, Buffer.Text) then
      begin
        Buffer.Text := StringReplace(Buffer.Text, OldValue, NewValue, [rfReplaceAll]);
        Buffer.SaveToFile(FileName);
        Result := True;
      end;
    finally
      Buffer.Free;
    end;
  end;
end;

// Thanks to: GetMem
// https://forum.lazarus.freepascal.org/index.php?topic=30553.0
function LoadUTF16FileToString(const FileName: TFileName): string;
var
  MS: TMemoryStream;
  S: string;

begin
  MS := TMemoryStream.Create;
  try
    MS.LoadFromFile(FileName);
    MS.Position := 0;

    // UTF-16 to UTF-8 with BOM
    S := UTF16ToUTF8(PWideChar(MS.Memory), MS.Size div SizeOf(WideChar));

    // UTF-8 without BOM
    Result := string(UTF8BOMToUTF8(S));
  finally
    MS.Free;
  end;
end;

function KillFile(const FileName: TFileName): Boolean;
begin
  if FileExists(FileName) then
    Result := SysUtils.DeleteFile(FileName);
end;

function ExpandEnvironmentStrings(const InputString: string): string;
{$IFDEF WINDOWS}
const
  MAXSIZE = 32768;

begin
  SetLength(Result, MAXSIZE);
  SetLength(Result, Windows.ExpandEnvironmentStrings(PChar(InputString), @Result[1], Length(Result)) - 1);
{$ELSE}
begin
  Result := InputString;
{$ENDIF}
end;

{$IFDEF GUI}
procedure Delay(Milliseconds: Integer);
var
  PastTime: LongInt;

begin
  PastTime := GetTickCount;
  repeat
    Application.ProcessMessages;
  until (GetTickCount - PastTime) >= LongInt(Milliseconds);
end;
{$ENDIF}

{$IFDEF DEBUG}
procedure DumpCharArrayToFile(A: array of Char; const FileName: TFileName);
var
  F: file;

begin
  AssignFile(F, FileName);
  ReWrite(F, SizeOf(A));
  BlockWrite(F, A[0], 1);
  CloseFile(F);
end;
{$ENDIF}

end.

