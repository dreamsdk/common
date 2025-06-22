(*
  Elevate Helper for Free Pascal/Lazarus
  Thanks Alex [https://stackoverflow.com/users/92713/alex]
  See: https://stackoverflow.com/a/19986365/3726096
*)
unit Elevate;

{$WARN SYMBOL_PLATFORM OFF}
{$R+}

interface

uses
  Windows,
  Classes;

type
  TElevatedProc = function(const ATaskName: string; AParameters: TStringList;
    SourceHandle: THandle): Cardinal;
  TProcessMessagesMethod = procedure of object;

var
  // Warning: this function will be executed in external process.
  // Do not use any global variables inside this routine!
  // Use only supplied AParameters.
  OnElevateProc: TElevatedProc;

// Call this routine after you have assigned OnElevateProc
procedure CheckForElevatedTask;

procedure DecodeParameters(EncodedParameters: string;
  DecodedParameters: TStringList);
function EncodeParameters(DecodedParameters: TStringList): string;

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
  SysUtils,
  Registry,
  ShellAPI,
  ComObj,
  SysTools,
  StrTools;

const
  RunElevatedTaskSwitch = '0C3B68CC5F5B900B64D50CB7DF000FFC'; // some unique value, just a GUID with removed '[', ']', and '-'

function CheckTokenMembership(
  TokenHandle: THandle;
  SidToCheck: Pointer;
  var IsMember: Boolean): Boolean; stdcall; external advapi32 name 'CheckTokenMembership';

procedure DecodeParameters(EncodedParameters: string;
  DecodedParameters: TStringList);
var
  LogContext: TLogMessageContext;

begin
  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%});
  try
    if Assigned(DecodedParameters) then
      StringToStringList(EncodedParameters, ArraySeparator, DecodedParameters);
    LogMessage(LogContext, Format('Decoded parameters: "%s"', [DecodedParameters.Text]));
  finally
    LogMessageExit(LogContext);
  end;
end;

function EncodeParameters(DecodedParameters: TStringList): string;
var
  LogContext: TLogMessageContext;

begin
  Result := EmptyStr;
  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%});
  try
    if Assigned(DecodedParameters) then
      Result := StringListToString(DecodedParameters, ArraySeparator);
    LogMessage(LogContext, Format('Result of encoded parameters: "%s"', [Result]));
  finally
    LogMessageExit(LogContext);
  end;
end;

function IsElevatedTaskRequested: Boolean;
begin
  Result := FindCmdLineSwitch(RunElevatedTaskSwitch);
end;

function IsRealOSError(ALastOSError: Cardinal): Boolean;
var
  LogContext: TLogMessageContext;

begin
  Result := False;
  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%});
  try
    Result := (ALastOSError <> ERROR_SUCCESS)
      and (ALastOSError <> ERROR_CANCELLED);
    LogMessage(LogContext, Format('IsRealOSError result: "%s"', [
      BoolToStr(Result, True)
    ]));
  finally
    LogMessageExit(LogContext);
  end;
end;

function RunElevated(
  const ATaskName, AParameters: string;
  const ASourceWindowHandle: THandle = INVALID_HANDLE_VALUE;
  const AProcessMessages: TProcessMessagesMethod = nil): Cardinal; overload;

var
  LogContext: TLogMessageContext;
  LocalLastOSError: Integer;
  SEI: TShellExecuteInfo;
  Host, Args: string;
  DecodedParameters: TStringList;

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
  Result := ERROR_NOT_READY;
  Assert(Assigned(OnElevateProc), 'OnElevateProc must be assigned before calling RunElevated');

  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%});
  try
{$IFDEF FPC}
    SEI := Default(TShellExecuteInfo);
{$ENDIF}

    LogMessage(LogContext, Format('IsElevated: %s', [
      BoolToStr(IsElevated, True)]));

    if IsElevated then
    begin
      if Assigned(OnElevateProc) then
      begin
        DecodedParameters := TStringList.Create;
        try
          DecodeParameters(AParameters, DecodedParameters);
          LogMessage(LogContext, Format('Calling OnElevateProc as we are already elevated; ATaskName: "%s", DecodedParameters: "%s", ASourceWindowHandle: "%d"', [
            ATaskName,
            DecodedParameters.Text,
            ASourceWindowHandle
          ]));
          Result := OnElevateProc(ATaskName, DecodedParameters, ASourceWindowHandle);
          LogMessage(LogContext, Format('Result of OnElevateProc: %d', [
            Result
          ]));
        finally
          DecodedParameters.Free;
        end;
      end
      else
      begin
        Result := ERROR_PROC_NOT_FOUND;
        LogMessage(LogContext, 'Unable to call OnElevateProc (impossible case)');
      end;
      LogMessage(LogContext, 'Exiting block IsElevated...');
      Exit;
    end;

    Host := ParamStr(0);
    Args := Format('/%s %s %d %s', [
      RunElevatedTaskSwitch,
      ATaskName,
      ASourceWindowHandle,
      AParameters
    ]);

    LogMessage(LogContext, Format('Host: "%s", Args: "%s"', [
      Host,
      Args
    ]));

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

    if LocalShellExecuteEx then
      LogMessage(LogContext, 'ShellExecuteEx successful run!')
    else
    begin
      LocalLastOSError := GetLastOSError;
      LogMessage(LogContext, Format('ShellExecuteEx failed, LocalLastOSError: %d', [
        LocalLastOSError
      ]));

      if IsRealOSError(LocalLastOSError) then
      begin
        LogMessage(LogContext, 'Real error, RaiseLastOSError needed!');
        RaiseLastOSError(LocalLastOSError)
      end
      else
      begin
        LogMessage(LogContext, 'Elevated operation cancelled by the user!');
        Result := ERROR_CANCELLED;
        Exit; // user cancelled the operation
      end;
    end;

    try
      LogMessage(LogContext, 'Entering the block to wait the child process to finish');
      Result := ERROR_GEN_FAILURE;
      if Assigned(AProcessMessages) then
      begin
        LogMessage(LogContext, 'Wait with UI');
        repeat
          if not GetExitCodeProcess(SEI.hProcess, Result) then
            Result := ERROR_GEN_FAILURE;
          AProcessMessages;
        until Result <> STILL_ACTIVE;
      end
      else
      begin
        LogMessage(LogContext, 'Wait without UI');
        if WaitForSingleObject(SEI.hProcess, INFINITE) <> WAIT_OBJECT_0 then
          if not GetExitCodeProcess(SEI.hProcess, Result) then
            Result := ERROR_GEN_FAILURE;
      end;
    finally
      CloseHandle(SEI.hProcess);
    end;

  finally
    LogMessageExit(LogContext);
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
    // Create SID of admin group for verification
    Win32Check(AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2,
      SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS, 0, 0, 0, 0, 0, 0,
      psidAdmin));

    // We check whether we are in the admin group (taking into account all checks for disabled SID)
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
var
  DecodedParameters: TStringList;
  LogContext: TLogMessageContext;
  ExitCode: Cardinal;

  function GetTaskName: string;
  begin
    Result := ParamStr(2);
  end;

  function GetSourceHandle: THandle;
  begin
{$IFDEF CPU64}
    if not TryStrToQWord(ParamStr(3), Result) then
      Result := INVALID_HANDLE_VALUE;
{$ELSE}
    if not TryStrToDWord(ParamStr(3), Result) then
      Result := INVALID_HANDLE_VALUE;
{$ENDIF}
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

begin
  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%});
  try
    LogMessage(LogContext, Format('IsElevatedTaskRequested: "%s"', [
      BoolToStr(IsElevatedTaskRequested, True)
    ]));

    if not IsElevatedTaskRequested then
      Exit;

    ExitCode := ERROR_GEN_FAILURE;
    try
      if not IsElevated then
        ExitCode := ERROR_ACCESS_DENIED
      else if Assigned(OnElevateProc) then
      begin
        DecodedParameters := TStringList.Create;
        try
          DecodeParameters(GetArgsForElevatedTask, DecodedParameters);
          LogMessage(LogContext, Format('Calling OnElevateProc as we are already elevated; ATaskName: "%s", DecodedParameters: "%s", ASourceWindowHandle: "%d"', [
            GetTaskName,
            DecodedParameters.Text,
            GetSourceHandle
          ]));
          ExitCode := OnElevateProc(GetTaskName, DecodedParameters, GetSourceHandle);
          LogMessage(LogContext, Format('Result of OnElevateProc: %d', [
            ExitCode
          ]));
        finally
          DecodedParameters.Free;
        end;
      end
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

    LogMessage(LogContext, Format('ExitCode: "%d"', [
      ExitCode
    ]));

{$IFDEF DEBUG}
    DebugLog('*** STRIKE <ENTER> TO EXIT ***');
    ReadLn;
{$ENDIF}

    TerminateProcess(GetCurrentProcess, ExitCode);

  finally
    LogMessageExit(LogContext);
  end;
end;

end.
