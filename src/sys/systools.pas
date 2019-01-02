unit SysTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  STRING_DATE_FORMAT = 'YYYY-MM-DD @ HH:mm:ss';

function ExtractStr(LeftSubStr, RightSubStr, S: string): string;
function ExtremeRight(SubStr: string ; S: string): string;
function GetApplicationPath: TFileName;
function GetSubStrCount(SubStr, S: string): Integer;
function IsInString(const SubStr, S: string): Boolean;
function IsValidInternetProtocolAddress(InternetProtocolAddress: string): Boolean;
function IsValidMediaAccessControlAddress(MediaAccessControlAddress: string): Boolean;
function Left(SubStr: string; S: string): string;
function LeftNRight(SubStr, S: string; N: Integer): string;
function Right(SubStr: string; S: string): string;
function Run(Executable: string): string; overload;
function Run(Executable, CommandLine: string): string; overload;
function Run(Executable, CommandLine: string; var ProcessId: Integer): string; overload;
function RunNoWait(Executable: string): Boolean; overload;
function RunNoWait(Executable, CommandLine: string): Boolean; overload;
function RunShellExecute(Executable, CommandLine: string): Boolean; overload;
function RunShellExecute(Executable: string): Boolean; overload;
function UnixPathToSystem(const PathName: TFileName): TFileName;
function SystemToUnixPath(const UnixPathName: TFileName): TFileName;

implementation

uses
  RegExpr,
{$IFDEF Windows}
  ShellApi,
{$ENDIF}
{$IFDEF GUI}
  Forms,
{$ENDIF}
{$IFDEF DEBUG}
  Dialogs,
{$ENDIF}
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

function RunNoWait(Executable: string): Boolean;
begin
  Result := RunNoWait(Executable, '');
end;

function RunNoWait(Executable, CommandLine: string): Boolean;
var
  OurProcess: {$IFDEF Windows}TProcess{$ELSE}TProcessUTF8{$ENDIF};

begin
  OurProcess := {$IFDEF Windows}TProcess{$ELSE}TProcessUTF8{$ENDIF}.Create(nil);
  try
    OurProcess.Executable := Executable;
    if CommandLine <> '' then
      OurProcess.Parameters.Text := StringReplace(CommandLine, ' ', sLineBreak, [rfReplaceAll]);
    OurProcess.ShowWindow := swoHide;
    OurProcess.Execute;
    Result := (OurProcess.ExitCode = 0);
  finally
    OurProcess.Free;
  end;
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

      if Length(CommandLine) > 0 then
        OurProcess.Parameters.Text := StringReplace(CommandLine, ' ', sLineBreak, [rfReplaceAll]);

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

function IsInString(const SubStr, S: string): Boolean;
begin
  Result := Pos(LowerCase(SubStr), LowerCase(S)) > 0;
end;

// Thanks to: http://docwiki.embarcadero.com/CodeExamples/Tokyo/en/EditMask_(Delphi)
function IsValidInternetProtocolAddress(InternetProtocolAddress: string): Boolean;
var
  net1, net2, host1, host2: Integer;
  InvalidAddress: Boolean;

begin
  try
    net1 := StrToInt(TrimRight(Copy(InternetProtocolAddress, 0, 3)));
    net2 := StrToInt(TrimRight(Copy(InternetProtocolAddress, 5, 3)));
    host1 := StrToInt(TrimRight(Copy(InternetProtocolAddress, 9, 3)));
    host2 := StrToInt(TrimRight(Copy(InternetProtocolAddress, 13, 3)));
    InvalidAddress := (
      (net1 < 0) or
      (net1 > 255) or
      (net2 < 0) or
      (net2 > 255) or
      (host1 < 0) or
      (host1 > 255) or
      (host2 < 0) or
      (host2 > 255)
    );
    Result := not InvalidAddress;
  except
    Result := False;
  end;
end;

// Thanks to: https://stackoverflow.com/a/4260512/3726096
function IsValidMediaAccessControlAddress(MediaAccessControlAddress: string): Boolean;
var
  RegexObj: TRegExpr;

begin
  RegexObj := TRegExpr.Create;
  try
    RegexObj.Expression := '^([0-9A-Fa-f]{2}[:-]){5}([0-9A-Fa-f]{2})$';
    Result := RegexObj.Exec(MediaAccessControlAddress);
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
  if (ApplicationPath = '') then
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

function RunShellExecute(Executable: string): Boolean;
begin
  Result := RunShellExecute(Executable, '');
end;

function RunShellExecute(Executable, CommandLine: string): Boolean;
begin
{$IFDEF Windows}
  Result := ShellExecute(0, 'open', PChar(Executable), PChar(CommandLine),
    PChar(ExtractFilePath(Executable)), 1) > 32;
{$ELSE}
  Result := False;
{$ENDIF}
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

end.

