unit SysTools;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  FGL
  {$IFDEF Windows}
  , Windows,
  JwaTlHelp32
  {$ENDIF} ;

const
  TabStr = #9;
  WhiteSpaceStr = ' ';
  ArraySeparator = '|';

  sError = 'Error';
  sDebugLogTitle = 'DebugLog';

  STRING_DATE_FORMAT = 'YYYY-MM-DD @ HH:mm:ss';
  ACL_RIGHT_FULL = 'F';

type
  TIntegerList = specialize TFPGList<Integer>;
  TStringIntegerMap = specialize TFPGMap<string, Integer>;

{$IFDEF DEBUG}procedure DebugLog(const Message: string);{$ENDIF}
{$IFDEF GUI}procedure Delay(Milliseconds: Integer);{$ENDIF}
{$IFDEF DEBUG}procedure DumpCharArrayToFile(A: array of Char; const FileName: TFileName);{$ENDIF}
function EndsWith(const SubStr, S: string): Boolean;
function ExpandEnvironmentStrings(const InputString: string): string;
function ExtractEmbeddedResourceToFile(const ResourceName: string;
  const FileName: TFileName): Boolean;
function ExtractStr(LeftSubStr, RightSubStr, S: string): string;
function ExtremeRight(SubStr: string; S: string): string;
function GetSubStrCount(SubStr, S: string): Integer;
function GetEveryoneName: string;
function GetFriendlyUserName(const UserName: string): string;
function GetUserList(var UserList: TStringList): Boolean;
function GetUserFullNameFromUserName(const UserName: string): string;
procedure HandleLogonServerVariable(EnvironmentVariables: TStringList);
function IsEmpty(const S: string): Boolean;
function IsInString(const SubStr, S: string): Boolean;
function IsProcessRunning(FileName: TFileName): Boolean;
function IsRegExMatch(const InputValue, RegEx: string): Boolean;
function KillProcessByName(const FileName: TFileName): Boolean;
function Left(SubStr: string; S: string): string;
function LeftNRight(SubStr, S: string; N: Integer): string;
function Right(SubStr: string; S: string): string;
function StartsWith(const SubStr, S: string): Boolean;
procedure StringToStringList(const S, Delimiter: string; SL: TStringList);
function StringListToString(SL: TStringList; const Delimiter: string): string;
function StringListSubstringIndexOf(SL: TStringList; const SubStr: string): Integer;
function SuppressUselessWhiteSpaces(const S: string): string;
function SetDirectoryRights(const DirectoryFullPath: TFileName;
  const UserName, Rights: string): Boolean;

implementation

uses
  StrUtils,
  RegExpr,
  LazUTF8,
  LConvEncoding,
{$IFDEF GUI}
  Forms,
{$ENDIF}
  RunTools,
  Version;

function SetDirectoryRights(const DirectoryFullPath: TFileName;
  const UserName, Rights: string): Boolean;
var
  CommandLine: string;

begin
{$IFDEF Windows}
  Result := False;

  CommandLine := 'echo Y | cacls "%s" /E /G "%s":%s';
  if IsWindowsVistaOrGreater then
    CommandLine := 'icacls "%s" /q /c /t /grant "%s":%s';

  CommandLine := Format(CommandLine, [
    ExcludeTrailingPathDelimiter(DirectoryFullPath),
    UserName,
    Rights
  ]);

{$IFDEF DEBUG}
  DebugLog('SetDirectoryRights: ' + CommandLine);
{$ENDIF}

  Result := RunSingleCommand(CommandLine);
{$ELSE}
  Result := False;
{$IFDEF DEBUG}
  WriteLn('SetDirectoryRights: Not implemented');
{$ENDIF}
{$ENDIF}
end;

function GetUserList(var UserList: TStringList): Boolean;
var
  i: Integer;
  OutputBuffer: string;

begin
  Result := False;
  if Assigned(UserList) then
  begin
    OutputBuffer := EmptyStr;
    Result := RunWmic('UserAccount where "LocalAccount = True and Disabled = False" get Name',
      OutputBuffer);

    if Result then
    begin
      UserList.Text := OutputBuffer;
      for i := 0 to UserList.Count - 1 do
        UserList[i] := Trim(UserList[i]);
    end;
  end;
end;

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

function KillProcessByName(const FileName: TFileName): Boolean;
begin
  Result := False;
  if FileExists(FileName) then
    Result := RunSingleCommand(Format('taskkill /im "%s" /f', [
      ExtractFileName(FileName)
    ]));
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
{$ELSE}
{$IFDEF GUI}
  MessageBox(Application.Handle, PChar(Trim(Message)), sDebugLogTitle,
    MB_ICONINFORMATION + MB_OK);
{$ENDIF}
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

// See: https://wiki.freepascal.org/Lazarus_Resources
function ExtractEmbeddedResourceToFile(const ResourceName: string;
  const FileName: TFileName): Boolean;
var
  S: TResourceStream;
  F: TFileStream;

begin
  // create a resource stream which points to our resource
  S := TResourceStream.Create(HInstance, ResourceName, RT_RCDATA);
  // Replace RT_RCDATA with ??? with what?
  // Please ensure you write the enclosing apostrophes around MYDATA,
  // otherwise no data will be extracted.
  try
    // create a file mydata.dat in the application directory
    F := TFileStream.Create(FileName, fmCreate);
    try
      F.CopyFrom(S, S.Size); // copy data from the resource stream to file stream
    finally
      F.Free; // destroy the file stream
    end;
  finally
    S.Free; // destroy the resource stream
  end;

  Result := FileExists(FileName);
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

{$IFDEF Windows}

// Thanks fbalien
// See: https://www.developpez.net/forums/d1736505/environnements-developpement/delphi/api-com-sdks/probleme-getnamedsecurityinfo-sous-tokyo-10-2-a/
function ConvertSidToString(RequestSID : PSID): string;

  function ConvertSidToStringSid(RequestSID: PSID): string;
  type
    TAdvApiConvertSidToStringSidA = function (Sid: PSID; var StringSid: LPSTR): BOOL; stdcall;

  const
    ADVAPI_LIBRARY_NAME = 'advapi32.dll';
    ADVAPI_CONVERT_SID_TO_STRING_SID = 'ConvertSidToStringSidA';

  var
    AdvApiConvertStringSidToSid: TAdvApiConvertSidToStringSidA;
    LibraryHandle: THandle;
    SidStringResult: PAnsiChar;

  begin
    Result := EmptyStr;
    LibraryHandle := LoadLibrary(ADVAPI_LIBRARY_NAME);
    if LibraryHandle <> 0 then
    try
      AdvApiConvertStringSidToSid := TAdvApiConvertSidToStringSidA(
        GetProcAddress(LibraryHandle, ADVAPI_CONVERT_SID_TO_STRING_SID));

      if @AdvApiConvertStringSidToSid <> nil then
      begin
        SidStringResult := Default(PAnsiChar);
        if AdvApiConvertStringSidToSid(RequestSID, SidStringResult) then
          Result := Format('[%s]', [SidStringResult]);
        FreeSID(RequestSID);
      end;
    finally
      FreeLibrary(LibraryHandle);
    end;
  end;

  function LegacyConvertSidToStringSid(RequestSID: PSID): string;
  const
    SID_REVISION  = 1;

  var
    Psia: TSIDIdentifierAuthority;
    i, SubAuthCount: LongWord;

  begin
    Psia := GetSidIdentifierAuthority(RequestSID)^;
    SubAuthCount := GetSidSubAuthorityCount(RequestSID)^;
    Result := Format('[S-%u-', [SID_REVISION]);
    if ((Psia.Value[0] <> 0) or (Psia.Value[1] <> 0)) then
      Result := Result + Format ('%.2x%.2x%.2x%.2x%.2x%.2x', [Psia.Value[0],
        Psia.Value[1], Psia.Value[2], Psia.Value[3], Psia.Value[4],
        Psia.Value[5]])
    else
      Result := Result + Format ('%u', [LongWord(Psia.Value[5]) +
        LongWord(Psia.Value[4] shl 8) + LongWord(Psia.Value[3] shl 16) +
        LongWord(Psia.Value[2] shl 24)]);
    for i := 0 to SubAuthCount - 1 do
      Result := Result + Format ('-%u', [GetSidSubAuthority(RequestSID, i)^]);
    Result := Result + ']';
  end;

begin
  Result := EmptyStr;
  if IsValidSid(RequestSID) then
  begin
    if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 5) then
      Result := ConvertSidToStringSid(RequestSID) // XP and newer
    else
      Result := LegacyConvertSidToStringSid(RequestSID); // older Windows
  end;
end;

function GetUserFromSid(OwnerSID: PSID): string;
type
  TWindowsString = array[0..MAX_PATH] of Char;

var
  OwnerName,
  DomainName: TWindowsString;
  SidSeparator: string;
  cbSize: DWORD;
  OwnerType: SID_NAME_USE;

begin
  Result := EmptyStr;
  OwnerName := Default(TWindowsString);
  DomainName := Default(TWindowsString);
  OwnerType := Default(SID_NAME_USE);
  cbSize := SizeOf(OwnerName);

  if LookupAccountSid(nil, OwnerSID, OwnerName, cbSize, DomainName, cbSize, OwnerType) then
  begin
    SidSeparator := DirectorySeparator;
    if SameText(DomainName, EmptyStr) then
      SidSeparator := EmptyStr;
    Result := Format('%s%s%s', [DomainName, SidSeparator, OwnerName]);
  end
  else
    Result := ConvertSidToString(OwnerSID);
end;

function CreateEveryoneSid: PSID;
const
  SECURITY_WORLD_SID_AUTHORITY: SID_IDENTIFIER_AUTHORITY = (
    Value: (0, 0, 0, 0, 0, 1)
  );

var
  IdentifierAuthority: SID_IDENTIFIER_AUTHORITY;
  EveryoneSID: PSID;
  fSuccess: Boolean;

begin
  Result := nil;

  IdentifierAuthority := SECURITY_WORLD_SID_AUTHORITY;

  EveryoneSID := nil;
  fSuccess := AllocateAndInitializeSid(IdentifierAuthority, 1,
    SECURITY_WORLD_RID, 0, 0, 0, 0, 0, 0, 0, EveryoneSID);

  if fSuccess then
    Result := EveryoneSID;
end;

{$ENDIF}

function GetEveryoneName: string;
{$IFDEF Windows}
var
  EveryoneSID: PSID;

begin
  Result := EmptyStr;
  EveryoneSID := CreateEveryoneSid;
  try
    Result := GetUserFromSid(EveryoneSID);
  finally
    FreeSid(EveryoneSID);
  end;
{$ELSE}
begin
  Result := EmptyStr;
{$IFDEF DEBUG}
  DebugLog('GetEveryoneName: Not implemented');
{$ENDIF}
{$ENDIF}
end;

function GetUserFullNameFromUserName(const UserName: string): string;
var
  OutputBuffer: string;

begin
  Result := EmptyStr;
  OutputBuffer := EmptyStr;
  if RunWmic(Format('UserAccount where Name=''%s'' get FullName', [UserName]), OutputBuffer) then
    Result := Trim(OutputBuffer);
end;

function GetFriendlyUserName(const UserName: string): string;
var
  CurrentUserFullName: string;

begin
  Result := UserName;
  CurrentUserFullName := GetUserFullNameFromUserName(UserName);
  if not IsEmpty(CurrentUserFullName) then
    Result := Format('%s (%s)', [CurrentUserFullName, UserName]);
end;

function IsEmpty(const S: string): Boolean;
begin
  Result := SameText(S, EmptyStr);
end;

function IsProcessRunning(FileName: TFileName): Boolean;
{$IFDEF Windows}
var
  ContinueLoop: Boolean;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
  
begin
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
  Result := False;
  while ContinueLoop do
  begin
    if ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile)) =
      UpperCase(FileName)) or (UpperCase(FProcessEntry32.szExeFile) =
      UpperCase(FileName))) then
    begin
      Result := True;
    end;
    ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);
{$ELSE}
begin
  raise EAbstractError.Create('IsProcessRunning is not implemented for this OS');
{$ENDIF}
end;

procedure HandleLogonServerVariable(EnvironmentVariables: TStringList);
const
  COMPUTERNAME_ENVIRONMENT_VARIABLE = 'COMPUTERNAME';
  LOGONSERVER_ENVIRONMENT_VARIABLE = 'LOGONSERVER';

var
  LogonServerIndex: Integer;
  LogonServerValue,
  Temp: string;

begin
{$IFDEF DEBUG}
  DebugLog('### HandleLogonServerVariable ###');
{$ENDIF}
  if not Assigned(EnvironmentVariables) then
    Exit;

  LogonServerValue := '\\' +
    SysUtils.GetEnvironmentVariable(COMPUTERNAME_ENVIRONMENT_VARIABLE);
  LogonServerIndex := StringListSubstringIndexOf(EnvironmentVariables,
    LOGONSERVER_ENVIRONMENT_VARIABLE + '=');

  if LogonServerIndex <> -1 then
  begin
    Temp := Trim(Right('=', EnvironmentVariables[LogonServerIndex]));
    if not IsEmpty(Temp) then
    begin
{$IFDEF DEBUG}
      DebugLog(LOGONSERVER_ENVIRONMENT_VARIABLE + ' is OK: ' + Temp);
{$ENDIF}
      Exit; // Nothing to do
    end;
  end;

{$IFDEF DEBUG}
  DebugLog(LOGONSERVER_ENVIRONMENT_VARIABLE + ' is NOT OK, setting it to '
    + LogonServerValue);
{$ENDIF}

  EnvironmentVariables.Add(
    Format('%s=%s', [LOGONSERVER_ENVIRONMENT_VARIABLE, LogonServerValue]));
end;

initialization
  Randomize;

end.

