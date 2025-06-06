(*
================================================================================
  ____                         _____  ____   _____
 |    .  ___  ___  ___  _____ |   __||    . |  |  |
 |  |  ||  _|| -_|| .'||     ||__   ||  |  ||    -|
 |____/ |_|  |___||__,||_|_|_||_____||____/ |__|__|

================================================================================
DreamSDK Common Library - Operating System Tools
================================================================================

This file is part of DreamSDK.

DreamSDK is free software: you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

DreamSDK is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
DreamSDK. If not, see <https://www.gnu.org/licenses/>.
*)
unit SysTools;

{$mode objfpc}{$H+}

(* Use this if you want to debug the LogonServerVariable addition. *)
// {$DEFINE SYSTOOLS_DETAILED_DEBUG_HANDLELOGONSERVERVARIABLE}

interface

uses
{$IFDEF WINDOWS}
  Windows,
  JwaTlHelp32,
{$ENDIF}
  Classes,
  SysUtils,
  FGL;

const
  ACL_RIGHT_FULL = 'F';

  TabStr = #9;
  EscapeStr = #27;
  WhiteSpaceStr = ' ';
  ArraySeparator = '|';

  sError = 'Error';
  sWarning = 'Warning';

{$IFDEF DEBUG}
  sDebugLogTitle = 'DebugLog';
{$ENDIF}

  STRING_DATE_FORMAT = 'YYYY-MM-DD @ HH:mm:ss';
  STRING_DATE_FORMAT_SHORT = 'YYYY-MM-DD';

type
  TIntegerList = specialize TFPGList<Integer>;
  TStringIntegerMap = specialize TFPGMap<string, Integer>;
  TIntegerStringMap = specialize TFPGMap<Integer, string>;

  EOperatingSystemTools = class(Exception);

  (* Used to store all the information from the Windows users *)
  TWindowsUserAccountInformation = record
    SID: string;
    UserName: string;
    FullName: string;
    FriendlyName: string;
    ProfilePath: TFileName;
    LocalAppDataPath: TFileName;
    RoamingAppDataPath: TFileName;
  end;
  TWindowsUserAccountInformationArray = array of TWindowsUserAccountInformation;

  (* Used for LogMessage functions, helpers for debugging / using DbgView *)
  TLogMessageContext = record
    FileName: string;
    ClassName: string;
    MethodName: string;
  end;

(* Extract a embedded file from the current executed program *)
function ExtractEmbeddedResourceToFile(const ResourceName: string;
  const FileName: TFileName): Boolean;

(* Expands environment-variable strings and replaces them with the values
   defined for the current user.
   See: https://learn.microsoft.com/en-us/windows/win32/api/processenv/nf-processenv-expandenvironmentstringsa *)
function ExpandEnvironmentStrings(const InputString: string): string;

(* Get the username of the Everyone Windows account on this system *)
function GetEveryoneName: string;

(* Search for a specific file in PATH *)
function GetFileLocationsInSystemPath(const FileName: TFileName;
  Output: TStringList): Boolean;

(* Get parent PID from another PID - reverse of GetProcessIdFromParentProcessId *)
function GetParentProcessIdFromProcessId(const ProcessId: LongWord): LongWord;

(* Get PID from parent PID - reverse of GetParentProcessIdFromProcessId *)
function GetProcessIdFromParentProcessId(const ParentProcessId: LongWord): LongWord;

(* Returns the list of all users from this system with various information *)
function GetUserList(out UsersList: TWindowsUserAccountInformationArray): Boolean;

(* Returns the Windows Users directory, typically "C:\Users\" *)
function GetUsersRootDirectory: TFileName;

(* Add a mandatory variable in TProcess environment when used with Bash. *)
procedure HandleLogonServerVariable(EnvironmentVariables: TStringList);

(* Checks if a process is running using image file name *)
function IsProcessRunning(FileName: TFileName): Boolean;

(* Checks if a process is running using a PID *)
function IsProcessRunning(const ProcessId: LongWord): Boolean;

(* Kill a running process by image file name *)
function KillProcessByName(const FileName: TFileName): Boolean;

(* Kill a running process by process ID (PID) *)
function KillProcessByProcessId(ProcessId: LongWord;
  GracePeriodMs: LongWord = 2000): Boolean;

(* Log a message while entering a procedure/function. Typically, you may use the
   following statement:

   LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%}, ClassName);

   Of course, remove the 'ClassName' parameter if you are not in a class.
*)
function LogMessageEnter(const SourceFileName: TFileName;
  const RoutineName: string; const ClassName: string = ''): TLogMessageContext;

(* Log a message to the user. This can be used for a Debug log statement but
   for the end-user and not for the programmers of DreamSDK. You will need
   a LogContext (TLogMessageContext) object to use this function. *)
procedure LogMessage(Context: TLogMessageContext; const Args: array of const);

(* Log a message to the user. This can be used for a Debug log statement but
   for the end-user and not for the programmers of DreamSDK. You will need a
   LogContext (TLogMessageContext) object to use this function. *)
procedure LogMessage(Context: TLogMessageContext; const Message: string);

(* Log a message while exiting a procedure/function. You will need a LogContext
   (TLogMessageContext) object to use this function. *)
procedure LogMessageExit(Context: TLogMessageContext);

(* Waits for the ends of execution of a specific PID *)
procedure WaitForProcessId(const ProcessId: LongWord);

// =============================================================================
// Graphical User Interface (GUI) Utilities
// =============================================================================

{$IFDEF GUI}

(* Wait for some milliseconds, it's Sleep function but better for GUI apps *)
procedure Delay(Milliseconds: Integer);

{$ENDIF}

// =============================================================================
// Debug Utilities
// =============================================================================

{$IFDEF DEBUG}

(* Similar to BoolToStr but for Debug console print. *)
function DebugBoolToStr(const Value: Boolean): string;

(* Print a message in the Console. Only works in Debug build. *)
procedure DebugLog(const Message: string);

(* Dump an array of Char to a file. *)
procedure DumpCharArrayToFile(A: array of Char; const FileName: TFileName);

(* Give a name to a thread. This is useful in "Debug View > Thread" in the IDE. *)
procedure SetThreadName(const ThreadName: string; ThreadHandle: THandle = 0);

{$ENDIF}

implementation

uses
  Variants,
  RegExpr,
  LazUTF8,
  LConvEncoding,
{$IFDEF WINDOWS}
  ActiveX,
{$ENDIF}
{$IFDEF GUI}
  Interfaces,
  Forms,
{$ENDIF}
{$IFDEF DEBUG}
{$IFDEF GUI}
  DbgLog,
{$ENDIF}
{$ENDIF}
  FSTools,
  RunTools,
  StrTools,
  UtilWMI,
  Version;

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

function GetUsersRootDirectory: TFileName;

  function TrySHGetKnownFolderPath(out Folder: WideString): Boolean;
  const
    // See: https://learn.microsoft.com/en-us/windows/win32/shell/knownfolderid
    FOLDERID_UserProfiles: TGUID = '{0762D272-C50A-4BB0-A382-697DCD729B80}';

  type
    TSHGetKnownFolderPath = function(const rfid: TGUID; dwFlags: DWord;
      hToken: THandle; out ppszPath: PWideChar): HResult; stdcall;

  var
    Shell32Handle: HMODULE;
    SHGetKnownFolderPath: TSHGetKnownFolderPath;
    Path: PWideChar;
    Res: HResult;

  begin
    Result := False;
    Shell32Handle := LoadLibrary('shell32.dll');
    if Shell32Handle <> 0 then
    begin
      Pointer(SHGetKnownFolderPath) := GetProcAddress(Shell32Handle, 'SHGetKnownFolderPath');
      if Assigned(SHGetKnownFolderPath) then
      begin
        Res := SHGetKnownFolderPath(FOLDERID_UserProfiles, 0, 0, Path);
        if Succeeded(Res) then
        begin
          Folder := WideCharToString(Path);
          CoTaskMemFree(Path);
          Result := True;
        end;
      end;
      FreeLibrary(Shell32Handle);
    end;
  end;

const
  WINXP_USERS_DIRECTORY = 'Documents and Settings';

var
  UserPath: WideString;
  WinDir: array[0..MAX_PATH - 1] of Char;

begin
{$IFDEF DEBUG}
  DebugLog('GetUsersDirectory');
{$ENDIF}

  CoInitialize(nil);
  try
    UserPath := Default(WideString);
    if TrySHGetKnownFolderPath(UserPath) then
      Result := string(UserPath)
    else
    begin
      GetWindowsDirectory(WinDir, MAX_PATH);
      Result := ExtractFileDrive(WinDir) + DirectorySeparator
        + WINXP_USERS_DIRECTORY;
    end;
    Result := IncludeTrailingPathDelimiter(Result);
  finally
    CoUninitialize;
  end;

{$IFDEF DEBUG}
  DebugLog(Format('  GetUsersDirectory::Result = "%s"', [Result]));
{$ENDIF}

  if not DirectoryExists(Result) then
    raise EOperatingSystemTools
      .CreateFmt('GetUsersDirectory returned an invalid directory: "%s"', [Result]);
end;

function GetUserList(out UsersList: TWindowsUserAccountInformationArray): Boolean;
type
  TAppDataKind = (
    adkRoaming,
    adkLocal
  );

var
  i: Integer;
  UserAccounts,
  UserProfileInfo: TWindowsManagementInstrumentationQueryResult;
  Buffer,
  LocalAppDataTemplate,
  RoamingAppDataTemplate: string;
  CurrentUser: TWindowsUserAccountInformation;

  function GetAppDataTemplate(const AppDataKind: TAppDataKind): TFileName;
  var
    AppDataTemplate,
    CurrentUserProfilePath: TFileName;
    AppDataVariable: string;

  begin
    AppDataVariable := '%AppData%';
    if AppDataKind = adkLocal then
      AppDataVariable := '%LocalAppData%';

    AppDataTemplate := IncludeTrailingPathDelimiter(
      ParseInputFileSystemObject(AppDataVariable));
    CurrentUserProfilePath := IncludeTrailingPathDelimiter(
      SysUtils.GetEnvironmentVariable('USERPROFILE'));

    // Returns something like: "%sAppData\Roaming" if adkRoaming is asked
    // This is not hardcoded as we want to get this from %AppData% variables
    // %s will be replaced by LocalPath for the user. The "\" is included in "%s".
    Result := '%s' + Right(CurrentUserProfilePath, AppDataTemplate);
  end;

begin
  Result := False;
  UsersList := Default(TWindowsUserAccountInformationArray);

  LocalAppDataTemplate := GetAppDataTemplate(adkLocal);
  RoamingAppDataTemplate := GetAppDataTemplate(adkRoaming);

  UserAccounts := QueryWindowsManagementInstrumentation('Win32_UserAccount',
    ['SID', 'Name', 'FullName'], 'LocalAccount = TRUE and Disabled = FALSE and SIDType = 1 and not Name like ''%$''');

  SetLength(UsersList, Length(UserAccounts));

  for i := Low(UserAccounts) to High(UserAccounts) do
  begin
    CurrentUser := Default(TWindowsUserAccountInformation);

    // Grab SID
    if GetWindowsManagementInstrumentationSingleValueByPropertyName(UserAccounts, 'SID', i, Buffer) then
    begin
      CurrentUser.SID := Buffer;

      // Grab the folder name of the current profile
      UserProfileInfo := QueryWindowsManagementInstrumentation('Win32_UserProfile', ['LocalPath'],
        Format('SID = ''%s''', [Buffer]));
      if GetWindowsManagementInstrumentationSingleValueByPropertyName(UserProfileInfo, 'LocalPath', 0, Buffer) then
        CurrentUser.ProfilePath := IncludeTrailingPathDelimiter(Buffer);
    end;

    // Grab UserName
    if GetWindowsManagementInstrumentationSingleValueByPropertyName(UserAccounts, 'Name', i, Buffer) then
      CurrentUser.UserName := Buffer;

    // Grab FullName
    if GetWindowsManagementInstrumentationSingleValueByPropertyName(UserAccounts, 'FullName', i, Buffer) then
      CurrentUser.FullName := Buffer;

    // Grab Local App Data
    CurrentUser.LocalAppDataPath := IncludeTrailingPathDelimiter(
      Format(LocalAppDataTemplate, [CurrentUser.ProfilePath]));

    // Grab Roaming App Data
    CurrentUser.RoamingAppDataPath := IncludeTrailingPathDelimiter(
      Format(RoamingAppDataTemplate, [CurrentUser.ProfilePath]));

    CurrentUser.FriendlyName := CurrentUser.UserName;
    if not IsEmpty(CurrentUser.FullName) then
      CurrentUser.FriendlyName := Format('%s (%s)', [CurrentUser.FullName, CurrentUser.UserName]);

    // Adding item to the array
    UsersList[i] := CurrentUser;
  end;

{$IFDEF DEBUG}
  DebugLog('GetUserList:');
  for i := Low(UsersList) to High(UsersList) do
  begin
    DebugLog(Format('  User #%d:', [i]));
    DebugLog(
      Format('   SID: "%s"', [UsersList[i].SID]) + sLineBreak +
      Format('   UserName: "%s"', [UsersList[i].UserName]) + sLineBreak +
      Format('   FullName: "%s"', [UsersList[i].FullName]) + sLineBreak +
      Format('   ProfilePath: "%s"', [UsersList[i].ProfilePath]) + sLineBreak +
      Format('   LocalAppDataPath: "%s"', [UsersList[i].LocalAppDataPath]) + sLineBreak +
      Format('   RoamingAppDataPath: "%s"', [UsersList[i].RoamingAppDataPath])
    );
  end;
{$ENDIF}

  Result := Length(UsersList) > 0;
end;

function KillProcessByName(const FileName: TFileName): Boolean;
begin
  Result := False;
  if FileExists(FileName) then
    Result := RunSingleCommand(Format('taskkill /im "%s" /f', [
      ExtractFileName(FileName)
    ]));
end;

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
  OwnerType := SidTypeUnknown;
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
{$IFDEF SYSTOOLS_DETAILED_DEBUG_HANDLELOGONSERVERVARIABLE}
  DebugLog('### HandleLogonServerVariable ###');
{$ENDIF}
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
{$IFDEF SYSTOOLS_DETAILED_DEBUG_HANDLELOGONSERVERVARIABLE}
      DebugLog(LOGONSERVER_ENVIRONMENT_VARIABLE + ' is OK: ' + Temp);
{$ENDIF}
{$ENDIF}
      Exit; // Nothing to do
    end;
  end;

{$IFDEF DEBUG}
{$IFDEF SYSTOOLS_DETAILED_DEBUG_HANDLELOGONSERVERVARIABLE}
  DebugLog(LOGONSERVER_ENVIRONMENT_VARIABLE + ' is NOT OK, setting it to '
    + LogonServerValue);
{$ENDIF}
{$ENDIF}

  EnvironmentVariables.Add(
    Format('%s=%s', [LOGONSERVER_ENVIRONMENT_VARIABLE, LogonServerValue]));
end;

function ExpandEnvironmentStrings(const InputString: string): string;
{$IFDEF WINDOWS}
const
  MAXSIZE = 32768;

begin
  Result := EmptyStr;
  SetLength(Result, MAXSIZE);
  SetLength(Result, Windows.ExpandEnvironmentStrings(PChar(InputString), @Result[1], Length(Result)) - 1);
{$ELSE}
begin
  Result := InputString;
{$ENDIF}
end;


function GetParentProcessIdFromProcessId(const ProcessId: LongWord): LongWord;
var
  ContinueLoop: Boolean;
  SnapshotHandle: THandle;
  ProcessEntry32: TProcessEntry32;

begin
  Result := 0;

  SnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  ProcessEntry32.dwSize := SizeOf(ProcessEntry32);

  ContinueLoop := Process32First(SnapshotHandle, ProcessEntry32);
  while ContinueLoop do
  begin
    if (ProcessEntry32.th32ProcessID = ProcessId) then
    begin
      Result := ProcessEntry32.th32ParentProcessID;
    end;
    ContinueLoop := (Result = 0) and Process32Next(SnapshotHandle, ProcessEntry32);
  end;

  CloseHandle(SnapshotHandle);
end;

function GetProcessIdFromParentProcessId(const ParentProcessId: LongWord): LongWord;
var
  ContinueLoop: Boolean;
  SnapshotHandle: THandle;
  ProcessEntry32: TProcessEntry32;

begin
  Result := 0;

  SnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  ProcessEntry32.dwSize := SizeOf(ProcessEntry32);

  ContinueLoop := Process32First(SnapshotHandle, ProcessEntry32);
  while ContinueLoop do
  begin
    if (ProcessEntry32.th32ParentProcessID = ParentProcessId) then
    begin
      Result := ProcessEntry32.th32ProcessID;
    end;
    ContinueLoop := (Result = 0) and Process32Next(SnapshotHandle, ProcessEntry32);
  end;

  CloseHandle(SnapshotHandle);
end;

procedure WaitForProcessId(const ProcessId: LongWord);
var
  ProcessHandle: THandle;

begin
  ProcessHandle := OpenProcess(SYNCHRONIZE, False, ProcessId);
  if ProcessHandle <> INVALID_HANDLE_VALUE then
  begin
    WaitForSingleObject(ProcessHandle, INFINITE);
    CloseHandle(ProcessHandle);
  end;
end;

function IsProcessRunning(const ProcessId: LongWord): Boolean;
var
  ProcessHandle: THandle;
  DesiredAccess,
  ProcessExitCode: LongWord;

begin
  Result := Default(Boolean);
  ProcessHandle := Default(THandle);
  ProcessExitCode := Default(LongWord);
{$IFDEF WINDOWS}
  DesiredAccess := PROCESS_QUERY_INFORMATION;
  if IsWindowsVistaOrGreater then
    DesiredAccess := PROCESS_QUERY_LIMITED_INFORMATION;
  ProcessHandle := OpenProcess(DesiredAccess, False, ProcessId);
  if ProcessHandle <> INVALID_HANDLE_VALUE then
  begin
    Result := GetExitCodeProcess(ProcessHandle, @ProcessExitCode)
      and (ProcessExitCode = STILL_ACTIVE);
    CloseHandle(ProcessHandle);
  end;
{$ELSE}
  raise EAbstractError.Create('IsProcessRunning: Not Implemented');
{$ENDIF}
end;

function KillProcessByProcessId(ProcessId: LongWord;
  GracePeriodMs: LongWord = 2000): Boolean;
const
  PROCESS_TERMINATE = $0001;
  SYNCHRONIZE = $00100000;

var
  ProcessHandle: THandle;
  WaitResult: DWORD;
  ControlHandlerSet: Boolean;
  ExitCode: DWORD;

begin
  Result := False;

  // Check if the process exists
  ProcessHandle := OpenProcess(SYNCHRONIZE or PROCESS_TERMINATE, False, ProcessId);
  if (ProcessHandle = 0) or (ProcessHandle = INVALID_HANDLE_VALUE) then
  begin
    // Process doesn't exist or no access rights
    Exit;
  end;

  try
    // Check if the process is already terminated
    if GetExitCodeProcess(ProcessHandle, ExitCode) and (ExitCode <> STILL_ACTIVE) then
    begin
      Result := True; // Process is already terminated
      Exit;
    end;

    // First attempt: clean termination with CTRL_BREAK_EVENT
    ControlHandlerSet := False;

    // Try to attach to the process console
    if AttachConsole(ProcessId) then
    begin
      try
        // Temporarily disable the control handler to prevent our own process from being affected
        // See: https://docs.microsoft.com/en-us/windows/console/ctrl-c-and-ctrl-break-signals
        ControlHandlerSet := SetConsoleCtrlHandler(nil, True);

        // Send CTRL+BREAK signal
        if GenerateConsoleCtrlEvent(CTRL_BREAK_EVENT, ProcessId) then
        begin
          // Wait for the process to terminate for GracePeriodMs
          WaitResult := WaitForSingleObject(ProcessHandle, GracePeriodMs);

          if WaitResult = WAIT_OBJECT_0 then
          begin
            Result := True; // Process terminated cleanly
          end;
        end;
      finally
        // Restore the control handler and free the console
        if ControlHandlerSet then
          SetConsoleCtrlHandler(nil, False);
        FreeConsole;
      end;
    end;

    // Check again if the process is still active
    if GetExitCodeProcess(ProcessHandle, ExitCode) and (ExitCode = STILL_ACTIVE) then
    begin
      // Second attempt: forced termination with TerminateProcess
      if TerminateProcess(ProcessHandle, 1) then
      begin
        // Wait for confirmation that the process is terminated
        WaitResult := WaitForSingleObject(ProcessHandle, GracePeriodMs);
        Result := (WaitResult = WAIT_OBJECT_0);
      end;
    end
    else
    begin
      Result := True; // Process terminated in the meantime
    end;

  finally
    // Always close the handle
    CloseHandle(ProcessHandle);
  end;
end;

function GetFileLocationsInSystemPath(const FileName: TFileName;
  Output: TStringList): Boolean;
const
  PATHEXT_ENV_VAR = 'PATHEXT';
  PATH_ENV_VAR = 'PATH';
  PATH_SEPARATOR = ';';

var
  FullFileName,
  FullFileNameWithExtension: TFileName;
  Buffer,
  Extensions: TStringList;
  i, j: Integer;

begin
  Result := False;
  if Assigned(Output) then
  begin
    Buffer := TStringList.Create;
    Extensions := TStringList.Create;
    try
      StringToStringList(SysUtils.GetEnvironmentVariable(PATH_ENV_VAR),
        PATH_SEPARATOR, Buffer);
      StringToStringList(SysUtils.GetEnvironmentVariable(PATHEXT_ENV_VAR),
        PATH_SEPARATOR, Extensions);

      for i := 0 to Buffer.Count - 1 do
      begin
        FullFileName := IncludeTrailingPathDelimiter(Buffer[i]) + FileName;

        if FileExists(FullFileName) then
          Output.Add(FullFileName)
        else
          for j := 0 to Extensions.Count - 1 do
          begin
            FullFileNameWithExtension := FullFileName + LowerCase(Extensions[j]);
            if FileExists(FullFileNameWithExtension) then
            begin
              Output.Add(FullFileNameWithExtension);
              Break;
            end;
          end;
      end;

      Result := (Output.Count > 0);
    finally
      Extensions.Free;
      Buffer.Free;
    end;
  end;
end;

// =============================================================================
// Graphical User Interface (GUI) Utilities
// =============================================================================

{$IFDEF GUI}

procedure Delay(Milliseconds: Integer);
var
  PastTime: LongInt;

begin
  PastTime := GetTickCount64;
  repeat
    Application.ProcessMessages;
  until (GetTickCount64 - PastTime) >= LongInt(Milliseconds);
end;

{$ENDIF}

// =============================================================================
// Debug Utilities
// =============================================================================

function GenerateLogMessage(const Args: array of const): string;
var
  i: Integer;
  Builder: TStringBuilder;

begin
  Result := Default(string);

  Builder := TStringBuilder.Create;
  try
    for i := Low(Args) to High(Args) do
    begin
      {if i > Low(Args) then
        Builder.Append(' ');}

      case Args[i].VType of
        vtInteger:
          Builder.Append(Args[i].VInteger);
        vtBoolean:
          Builder.Append(BoolToStr(Args[i].VBoolean, True));
        vtChar:
          Builder.Append(Args[i].VChar);
{$IFNDEF FPUNONE}
        vtExtended:
          Builder.Append(FloatToStr(Args[i].VExtended^));
{$ENDIF}
        vtString:
          Builder.Append(string(Args[i].VString));
        vtPointer:
          Builder.Append(Format('%p', [Args[i].VPointer]));
        vtPChar:
          Builder.Append(string(Args[i].VPChar));
        vtObject:
          Builder.Append('[Object: "' + TObject(Args[i].VObject).ClassName + '"]');
        vtClass:
          Builder.Append('[ClassName: "' + Args[i].VClass.ClassName + '"]');
        vtWideChar:
          Builder.Append(string(Args[i].VWideChar));
        vtPWideChar:
          Builder.Append(string(Args[i].VPWideChar));
        vtAnsiString:
          Builder.Append(string(Args[i].VAnsiString));
        vtCurrency:
          Builder.Append(Format('%m', [Args[i].VCurrency]));
        vtVariant:
          Builder.Append(VarToStr(Args[i].VVariant^));
        vtInterface:
          Builder.Append(Format('[Interface: %p]', [Args[i].VInterface]));
        vtWideString:
          Builder.Append(string(Args[i].VWideString));
        vtInt64:
          Builder.Append(IntToStr(Args[i].VInt64^));
        vtQWord:
          Builder.Append(UIntToStr(Args[i].VQWord^));
        vtUnicodeString:
          Builder.Append(string(Args[i].VUnicodeString));
      else
        Builder.Append('[UnknownType: ' + IntToStr(Args[i].VType) + ']');
      end;
    end;

    Result := Builder.ToString();

  finally
    Builder.Free;
  end;
end;

function GetLogMessageContext(Context: TLogMessageContext): string;
begin
  Result := Context.MethodName;
  if not IsEmpty(Context.ClassName) then
    Result := Concat(Context.ClassName, '.', Context.MethodName);
end;

procedure DoLogMessage(const Message: string);
begin
{$IFDEF WINDOWS}
  OutputDebugString(PChar(Message));
{$ENDIF}
end;

function LogMessageEnter(const SourceFileName: TFileName;
  const RoutineName: string; const ClassName: string = ''): TLogMessageContext;
begin
  Result.FileName := SourceFileName;
  Result.ClassName := ClassName;
  Result.MethodName := RoutineName;
  DoLogMessage(Concat(GetLogMessageContext(Result), '::Entering'));
end;

procedure LogMessage(Context: TLogMessageContext; const Args: array of const);
begin
  LogMessage(Context, GenerateLogMessage(Args));
end;

procedure LogMessage(Context: TLogMessageContext; const Message: string);
begin
  DoLogMessage(Concat(GetLogMessageContext(Context), ': ', Message));
end;

procedure LogMessageExit(Context: TLogMessageContext);
begin
  DoLogMessage(Concat(GetLogMessageContext(Context), '::Exiting'));
end;

{$IFDEF DEBUG}

procedure SetThreadName(const ThreadName: string; ThreadHandle: THandle = 0);
{$IFDEF WINDOWS}
const
  SVL_THREAD_NAMING_EXCEPTION = $406D1388;

type
  TThreadNameInfo = record
    dwType     : DWORD;      // = 0x1000
    szName     : PAnsiChar;  // Thread name
    dwThreadID : DWORD;      // Thread ID (-1 = current thread)
    dwFlags    : DWORD;      // Reserved, must be zero
  end;

  TSetThreadDescription = function(hThread: THandle; lpThreadDescription: PWideChar): HRESULT; stdcall;

var
  KernelHandle: THandle;
  SetThreadDesc: TSetThreadDescription;
  WideName: WideString;
  info: TThreadNameInfo;

begin
  if ThreadHandle = 0 then
    ThreadHandle := GetCurrentThread();

  // Try SetThreadDescription if available (Windows 10+)
  KernelHandle := GetModuleHandle('kernel32.dll');
  if KernelHandle <> 0 then
  begin
    SetThreadDesc := TSetThreadDescription(GetProcAddress(KernelHandle, 'SetThreadDescription'));
    if Assigned(SetThreadDesc) then
    begin
      WideName := WideString(ThreadName);
      SetThreadDesc(ThreadHandle, PWideChar(WideName));
      Exit;
    end;
  end;

  // Fallback: Raise exception 0x406D1388 for old debuggers (Visual Studio, etc.)
  info.dwType     := $1000;
  info.szName     := PAnsiChar(AnsiString(ThreadName));
  info.dwThreadID := GetCurrentThreadId;
  info.dwFlags    := 0;

  try
    RaiseException(SVL_THREAD_NAMING_EXCEPTION, 0,
      SizeOf(info) div SizeOf(DWORD), PDWORD(@info));
  except
    // Ignore this specific exception
  end;
{$ELSE}
begin
  raise ENotImplemented.Create('NameThread is not implemented for your system');
{$ENDIF}
end;

procedure DebugLog(const Message: string);
{$IFNDEF CONSOLE}
var
  MsgHandle: THandle;
{$ENDIF}
begin
{$IFDEF CONSOLE}
{$IFNDEF DEBUG_DISABLE_CONSOLE_PRINT}
  WriteLn(Message);
{$ENDIF}
{$ELSE}
  MsgHandle := 0;
{$IFDEF GUI}
  MsgHandle := Application.Handle;
{$ENDIF}
  if not DbgLog.DebugLog(Message) then
    MessageBox(MsgHandle, PChar(Trim(Message)), sDebugLogTitle,
      MB_ICONINFORMATION + MB_OK);
{$ENDIF}
{$IFDEF WINDOWS}
  OutputDebugString(PChar(Message));
{$ENDIF}
end;

procedure DumpCharArrayToFile(A: array of Char; const FileName: TFileName);
var
  F: file;

begin
  AssignFile(F, FileName);
  ReWrite(F, SizeOf(A));
  BlockWrite(F, A[0], 1);
  CloseFile(F);
end;

function DebugBoolToStr(const Value: Boolean): string;
begin
  Result := BoolToStr(Value, 'TRUE', 'FALSE');
end;

{$ENDIF}

initialization
  Randomize;

end.

