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

(* Extract a embedded file from the current executed program *)
function ExtractEmbeddedResourceToFile(const ResourceName: string;
  const FileName: TFileName): Boolean;

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

procedure HandleLogonServerVariable(EnvironmentVariables: TStringList);

(* Checks if a process is running using image file name *)
function IsProcessRunning(FileName: TFileName): Boolean;

(* Checks if a process is running using a PID *)
function IsProcessRunning(const ProcessId: LongWord): Boolean;

(* Kill a running process by image file name *)
function KillProcessByName(const FileName: TFileName): Boolean;

(* Waits for the ends of execution of a specific PID *)
procedure WaitForProcessId(const ProcessId: LongWord);

// =============================================================================
// Graphical User Interface (GUI) Utilities
// =============================================================================

{$IFDEF GUI}

(* Wait for some milliseconds, it's Sleep but better way for GUI apps *)
procedure Delay(Milliseconds: Integer);

{$ENDIF}

// =============================================================================
// Debug Utilities
// =============================================================================

{$IFDEF DEBUG}

function DebugBoolToStr(const Value: Boolean): string;

procedure DebugLog(const Message: string);

procedure DumpCharArrayToFile(A: array of Char; const FileName: TFileName);

{$ENDIF}

implementation

uses
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

(*
function GetUserFromAppDataDirectory(const AppDataDirectory: TFileName;
  out UserAccount: TWindowsUserAccountInformation): Boolean;
var
  AppDataTemplate,
  LocalPath: TFileName;
  TempLeft,
  TempRight: string;
  UsersList: TWindowsUserAccountInformationArray;
  i: Integer;

begin
  Result := False;
  UserAccount := Default(TWindowsUserAccountInformation);

  if GetUserList(UsersList) then
  begin
    AppDataTemplate := GetAppDataTemplate(adkRoaming);
    TempLeft := Left('%s', AppDataTemplate);
    TempRight := Right('%s', AppDataTemplate);
    LocalPath := Right(TempLeft, AppDataDirectory); // ExtractStr(TempLeft, TempRight, AppDataDirectory);

    raise Exception.create ('TODO');
    i := 0;
    while i < Count(UsersList) - 1 do
    begin
      if UsersList[i].LocalPath
    end;
  end;
end;
*)

// {$DEFINE DEBUG_USER_APP_DATA_LIST}
(*function GetAppDataListFromUsers(out UserAppDataList: TStringArray;
  const AppDataKind: TAppDataKind = adkRoaming): Boolean;
var
  AppDataTemplate: TFileName;
  i: Integer;
  UsersList: TWindowsUserAccountInformationArray;

begin
{$IFDEF DEBUG_USER_APP_DATA_LIST}
{$IFDEF DEBUG}
    DebugLog('UserAppDataList:');
{$ENDIF}
{$ENDIF}

  Result := False;
  UserAppDataList := Default(TStringArray);

  if GetUserList(UsersList) then
  begin
    SetLength(UserAppDataList, Length(UsersList));
    AppDataTemplate := GetAppDataTemplate(AppDataKind);
    for i := 0 to Length(UsersList) - 1 do
    begin
      UserAppDataList[i] := Format(AppDataTemplate, [
        UsersList[i].LocalPath
      ]);

{$IFDEF DEBUG_USER_APP_DATA_LIST}
{$IFDEF DEBUG}
      DebugLog('  ' + UserAppDataList[i]);
{$ENDIF}
{$ENDIF}
    end;
  end;
end;

function GetUserFullNameFromUserName(const UserName: string): string;
var
  UserAccount: TWindowsManagementInstrumentationQueryResult;
  UserAccountFullName: string;

begin
  Result := EmptyStr;
  UserAccount := QueryWindowsManagementInstrumentation(
    'Win32_UserAccount', ['FullName'], Format('Name = ''%s''', [UserName]));
  if GetWindowsManagementInstrumentationSingleValueByPropertyName(UserAccount, 'FullName', 0, UserAccountFullName) then
    Result := Trim(UserAccountFullName);
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
*)

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
  PastTime := GetTickCount;
  repeat
    Application.ProcessMessages;
  until (GetTickCount - PastTime) >= LongInt(Milliseconds);
end;

{$ENDIF}

// =============================================================================
// Debug Utilities
// =============================================================================

{$IFDEF DEBUG}

procedure DebugLog(const Message: string);
{$IFNDEF CONSOLE}
var
  MsgHandle: THandle;
{$ENDIF}
begin
{$IFDEF CONSOLE}
  WriteLn(Message);
{$ELSE}
  MsgHandle := 0;
{$IFDEF GUI}
  MsgHandle := Application.Handle;
{$ENDIF}
  if not DbgLog.DebugLog(Message) then
    MessageBox(MsgHandle, PChar(Trim(Message)), sDebugLogTitle,
      MB_ICONINFORMATION + MB_OK);
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

