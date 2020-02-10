unit Elevate;

{$WARN SYMBOL_PLATFORM OFF}
{$R+}

// Elevate Helper for Free Pascal/Lazarus

// Thanks Alex
// https://stackoverflow.com/a/19986365/3726096

interface

uses
  Windows;

type
  TElevatedProc = function(const ATaskName, AParameters: string; SourceHandle: THandle): Cardinal;
  TProcessMessagesMethod = procedure of object;

var
  // Warning: this function will be executed in external process.
  // Do not use any global variables inside this routine!
  // Use only supplied AParameters.
  OnElevateProc: TElevatedProc;

// Call this routine after you have assigned OnElevateProc
procedure CheckForElevatedTask;

// Runs OnElevateProc under full administrator rights
function RunElevated(
  const ATaskName, AParameters: string;
  const ASourceWindowHandle: THandle = INVALID_HANDLE_VALUE;
  const AProcessMessages: TProcessMessagesMethod = nil): Cardinal; overload;

function IsAdministrator: Boolean;
function IsAdministratorAccount: Boolean;
function IsUACEnabled: Boolean;
function IsElevated: Boolean;
function IsElevatedTaskRequested: Boolean;
procedure SetButtonElevated(const AButtonHandle: THandle);
function IsRealOSError(ALastOSError: Cardinal): Boolean;

implementation

uses
  SysUtils, Registry, ShellAPI, ComObj;

const
  RunElevatedTaskSwitch = '0C3B68CC5F5B900B64D50CB7DF000FFC'; // some unique value, just a GUID with removed '[', ']', and '-'

function CheckTokenMembership(
  TokenHandle: THandle;
  SidToCheck: Pointer;
  var IsMember: Boolean): Boolean; stdcall; external advapi32 name 'CheckTokenMembership';

function IsElevatedTaskRequested: Boolean;
begin
  Result := FindCmdLineSwitch(RunElevatedTaskSwitch);
end;

function IsRealOSError(ALastOSError: Cardinal): Boolean;
begin
  Result := (ALastOSError <> ERROR_SUCCESS) and (ALastOSError <> ERROR_CANCELLED);
end;

function RunElevated(
  const ATaskName, AParameters: string;
  const ASourceWindowHandle: THandle = INVALID_HANDLE_VALUE;
  const AProcessMessages: TProcessMessagesMethod = nil): Cardinal; overload;

var
  LocalLastOSError: Integer;
  SEI: TShellExecuteInfo;
  Host: String;
  Args: String;

  function LocalShellExecuteEx: Boolean;
  begin
    Result := False;
{$IFDEF FPC}
{$IFDEF UNICODE}
    Result := ShellExecuteExW(@SEI)
{$ELSE}
    Result := ShellExecuteExA(@SEI)
{$ENDIF}
{$ELSE}
    Result := ShellExecuteEx(@SEI)
{$ENDIF}
  end;

begin
  Assert(Assigned(OnElevateProc), 'OnElevateProc must be assigned before calling RunElevated');

{$IFDEF FPC}
  SEI := Default(TShellExecuteInfo);
{$ENDIF}

  if IsElevated then
  begin
    if Assigned(OnElevateProc) then
      Result := OnElevateProc(ATaskName, AParameters, ASourceWindowHandle)
    else
      Result := ERROR_PROC_NOT_FOUND;
    Exit;
  end;

  Host := ParamStr(0);
  Args := Format('/%s %s %d %s', [
    RunElevatedTaskSwitch,
    ATaskName,
    ASourceWindowHandle,
    AParameters
  ]);

  FillChar(SEI, SizeOf(SEI), 0);
  SEI.cbSize := SizeOf(SEI);
  SEI.fMask := SEE_MASK_NOCLOSEPROCESS;
{$IFDEF UNICODE}
  SEI.fMask := SEI.fMask or SEE_MASK_UNICODE;
{$ENDIF}
  SEI.Wnd := ASourceWindowHandle;
  SEI.lpVerb := 'runas';
  SEI.lpFile := PChar(Host);
  SEI.lpParameters := PChar(Args);
  SEI.nShow := SW_NORMAL;

  if not LocalShellExecuteEx then
  begin
    LocalLastOSError := GetLastOSError;
{$IFDEF DEBUG}
    WriteLn('LocalLastOSError: ', LocalLastOSError);
{$ENDIF}
    if IsRealOSError(LocalLastOSError) then
    begin
{$IFDEF DEBUG}
      WriteLn('RaiseLastOSError needed!');
{$ENDIF}
      RaiseLastOSError(LocalLastOSError)
    end
    else
    begin
{$IFDEF DEBUG}
      WriteLn('Elevated operation cancelled!');
{$ENDIF}
      Result := ERROR_CANCELLED;
      Exit; // user cancelled the operation
    end;
  end;

  try
    Result := ERROR_GEN_FAILURE;
    if Assigned(AProcessMessages) then
    begin
      repeat
        if not GetExitCodeProcess(SEI.hProcess, Result) then
          Result := ERROR_GEN_FAILURE;
        AProcessMessages;
      until Result <> STILL_ACTIVE;
    end
    else
    begin
      if WaitForSingleObject(SEI.hProcess, INFINITE) <> WAIT_OBJECT_0 then
        if not GetExitCodeProcess(SEI.hProcess, Result) then
          Result := ERROR_GEN_FAILURE;
    end;
  finally
    CloseHandle(SEI.hProcess);
  end;
end;

function IsAdministrator: Boolean;
var
  psidAdmin: Pointer;
  B: Boolean;

const
  SECURITY_NT_AUTHORITY: TSidIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5));
  SECURITY_BUILTIN_DOMAIN_RID  = $00000020;
  DOMAIN_ALIAS_RID_ADMINS      = $00000220;
//  SE_GROUP_USE_FOR_DENY_ONLY  = $00000010;

begin
  Result := False;
  B := False;
  psidAdmin := nil;
  try
    // Создаём SID группы админов для проверки
    Win32Check(AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2,
      SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS, 0, 0, 0, 0, 0, 0,
      psidAdmin));

    // Проверяем, входим ли мы в группу админов (с учётов всех проверок на disabled SID)
    if CheckTokenMembership(0, psidAdmin, B) then
      Result := B;
  finally
    if psidAdmin <> nil then
      FreeSid(psidAdmin);
  end;
end;

{$R-}

function IsAdministratorAccount: Boolean;
var
  psidAdmin: Pointer;
  Token: THandle;
  Count: DWORD;
  TokenInfo: PTokenGroups;
  HaveToken: Boolean;
  I: Integer;

const
  SECURITY_NT_AUTHORITY: TSidIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5));
  SECURITY_BUILTIN_DOMAIN_RID  = $00000020;
  DOMAIN_ALIAS_RID_ADMINS      = $00000220;
//  SE_GROUP_USE_FOR_DENY_ONLY  = $00000010;

begin
  Result := Win32Platform <> VER_PLATFORM_WIN32_NT;
  if Result then
    Exit;

  psidAdmin := nil;
  TokenInfo := nil;
  HaveToken := False;
  try
    Token := 0;
    HaveToken := OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True, Token);
    if (not HaveToken) and (GetLastError = ERROR_NO_TOKEN) then
      HaveToken := OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, Token);
    if HaveToken then
    begin
      Win32Check(AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2,
        SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS, 0, 0, 0, 0, 0, 0,
        psidAdmin));
{$IFDEF FPC}
      Count := Default(DWord);
{$ENDIF}
      if GetTokenInformation(Token, TokenGroups, nil, 0, Count) or
         (GetLastError <> ERROR_INSUFFICIENT_BUFFER) then
        RaiseLastOSError;
      TokenInfo := PTokenGroups(AllocMem(Count));
      Win32Check(GetTokenInformation(Token, TokenGroups, TokenInfo, Count, Count));
      for I := 0 to TokenInfo^.GroupCount - 1 do
      begin
        Result := EqualSid(psidAdmin, TokenInfo^.Groups[I].Sid);
        if Result then
          Break;
      end;
    end;
  finally
    if TokenInfo <> nil then
      FreeMem(TokenInfo);
    if HaveToken then
      CloseHandle(Token);
    if psidAdmin <> nil then
      FreeSid(psidAdmin);
  end;
end;

{$R+}

function IsUACEnabled: Boolean;
var
  Reg: TRegistry;

begin
  Result := CheckWin32Version(6, 0);
  if Result then
  begin
    Reg := TRegistry.Create(KEY_READ);
    try
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      if Reg.OpenKey('\Software\Microsoft\Windows\CurrentVersion\Policies\System', False) then
        if Reg.ValueExists('EnableLUA') then
          Result := (Reg.ReadInteger('EnableLUA') <> 0)
        else
          Result := False
      else
        Result := False;
    finally
      FreeAndNil(Reg);
    end;
  end;
end;

function IsElevated: Boolean;
const
  TokenElevation = TTokenInformationClass(20);

type
  TOKEN_ELEVATION = record
    TokenIsElevated: DWORD;
  end;

var
  TokenHandle: THandle;
  ResultLength: Cardinal;
  ATokenElevation: TOKEN_ELEVATION;
  HaveToken: Boolean;

begin
  if CheckWin32Version(6, 0) then
  begin
    TokenHandle := 0;
    HaveToken := OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True, TokenHandle);
    if (not HaveToken) and (GetLastError = ERROR_NO_TOKEN) then
      HaveToken := OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, TokenHandle);
    if HaveToken then
    begin
      try
        ResultLength := 0;
        if GetTokenInformation(TokenHandle, TokenElevation, @ATokenElevation, SizeOf(ATokenElevation), ResultLength) then
          Result := ATokenElevation.TokenIsElevated <> 0
        else
          Result := False;
      finally
        CloseHandle(TokenHandle);
      end;
    end
    else
      Result := False;
  end
  else
    Result := IsAdministrator;
end;

procedure SetButtonElevated(const AButtonHandle: THandle);
const
  BCM_SETSHIELD = $160C;

var
  Required: Boolean;

begin
  if not CheckWin32Version(6, 0) then
    Exit;
  if IsElevated then
    Exit;

  Required := True;
  SendMessage(AButtonHandle, BCM_SETSHIELD, 0, LPARAM(Required));
end;

procedure CheckForElevatedTask;

  function GetTaskName: string;
  begin
    Result := ParamStr(2);
  end;

  function GetSourceHandle: THandle;
  begin
    if not TryStrToDWord(ParamStr(3), Result) then
      Result := INVALID_HANDLE_VALUE;
  end;

  function GetArgsForElevatedTask: string;

    function PrepareParam(const ParamNo: Integer): String;
    begin
      Result := ParamStr(ParamNo);
      if Pos(' ', Result) > 0 then
        Result := AnsiQuotedStr(Result, '"');
    end;

  var
    X: Integer;

  begin
    Result := EmptyStr;
    for X := 4 to ParamCount do
      Result := Result + PrepareParam(X) + ' ';
    Result := Trim(Result);
  end;

var
  ExitCode: Cardinal;

begin
  if not IsElevatedTaskRequested then
    Exit;

  ExitCode := ERROR_GEN_FAILURE;
  try
    if not IsElevated then
      ExitCode := ERROR_ACCESS_DENIED
    else
    if Assigned(OnElevateProc) then
      ExitCode := OnElevateProc(GetTaskName, GetArgsForElevatedTask, GetSourceHandle)
    else
      ExitCode := ERROR_PROC_NOT_FOUND;
  except
    on E: Exception do
    begin
      if E is EAbort then
        ExitCode := ERROR_CANCELLED
      else
      if E is EOleSysError then
        ExitCode := Cardinal(EOleSysError(E).ErrorCode)
      else
      if E is EOSError then
      else
        ExitCode := ERROR_GEN_FAILURE;
    end;
  end;

  if ExitCode = STILL_ACTIVE then
    ExitCode := ERROR_GEN_FAILURE;
  TerminateProcess(GetCurrentProcess, ExitCode);
end;

end.
