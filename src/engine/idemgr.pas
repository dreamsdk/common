unit IDEMgr;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Environ,
  FSTools,
  Settings;

type
  TIntegratedDevelopmentEnvironment = class;

  { TCodeBlocksIntegratedDevelopmentEnvironment }
  TCodeBlocksIntegratedDevelopmentEnvironment = class(TObject)
  private
    fSettings: TDreamcastSoftwareDevelopmentSettingsCodeBlocks;
    fIntegratedDevelopmentEnvironment: TIntegratedDevelopmentEnvironment;
    fLastErrorMessage: string;
    fLastOperationSuccess: Boolean;
    fPatchState: string;
    fAvailableUsers: TStringList;
    fInstalledUsers: TStringList;
    function GetInstallationDirectory: TFileName;
    function GetInstalled: Boolean;
    function GetAvailableUsers: TStringList;
    function GetInstalledUsers: TStringList;
    procedure RetrievePatchState;
    function RunCodeBlocksPatcher(const Operation: string): string;
  public
    constructor Create(AIntegratedDevelopmentEnvironment: TIntegratedDevelopmentEnvironment);
    destructor Destroy; override;

    function InitializeProfiles: Boolean;
    function Install(const CodeBlocksInstallationDirectory: TFileName): Boolean;
    function Uninstall: Boolean;
    function Reinstall: Boolean;
    procedure Refresh; overload;
    procedure Refresh(const Force: Boolean);
    procedure UpdateStateFromElevatedTask(const SwapExchangeFileName: TFileName);

    property AvailableUsers: TStringList read GetAvailableUsers;
    property Installed: Boolean read GetInstalled;
    property InstallationDirectory: TFileName read GetInstallationDirectory;
    property InstalledUsers: TStringList read GetInstalledUsers;
    property LastOperationSuccess: Boolean read fLastOperationSuccess;
    property LastErrorMessage: string read fLastErrorMessage;
    property Settings: TDreamcastSoftwareDevelopmentSettingsCodeBlocks
      read fSettings;
  end;

  { TVisualStudioCodeIntegratedDevelopmentEnvironment }
  // Visual Studio Code or one of its forks (e.g. VSCodium): see VSCODE_EDITORS
  TVisualStudioCodeIntegratedDevelopmentEnvironment = class(TObject)
  private
    fIntegratedDevelopmentEnvironment: TIntegratedDevelopmentEnvironment;
    fEditorId: string;
    fName: string;
    fRefreshed: Boolean;
    fAvailableUsers: TStringList;
    fInstalledUsers: TStringList;
    fDetected: Boolean;
    fVersion: string;
    fCommandLineInterface: TFileName;
    fInstallationDirectory: TFileName;
    fLastErrorMessage: string;
    fLastOperationSuccess: Boolean;
    function GetExtensionFileName: TFileName;
    function GetInstalled: Boolean;
  public
    constructor Create(AIntegratedDevelopmentEnvironment: TIntegratedDevelopmentEnvironment;
      const AEditorId, AName: string);
    destructor Destroy; override;

    // Install (or update) the extension for all the users
    function Install: Boolean;
    // Remove the extension from all the users
    function Uninstall: Boolean;
    // Retrieve the Visual Studio Code installation and the users state
    procedure Refresh;
    procedure UpdateStateFromElevatedTask(const SwapExchangeFileName: TFileName);

    property AvailableUsers: TStringList read fAvailableUsers;
    property CommandLineInterface: TFileName read fCommandLineInterface;
    property InstallationDirectory: TFileName read fInstallationDirectory;
    property Detected: Boolean read fDetected;
    property EditorId: string read fEditorId;
    property ExtensionFileName: TFileName read GetExtensionFileName;
    property Installed: Boolean read GetInstalled;
    property InstalledUsers: TStringList read fInstalledUsers;
    property LastErrorMessage: string read fLastErrorMessage;
    property LastOperationSuccess: Boolean read fLastOperationSuccess;
    property Name: string read fName;
    // True when Refresh was called at least once
    property Refreshed: Boolean read fRefreshed;
    property Version: string read fVersion;
  end;
  TVisualStudioCodeIntegratedDevelopmentEnvironmentArray =
    array of TVisualStudioCodeIntegratedDevelopmentEnvironment;

  { TIntegratedDevelopmentEnvironment }
  TIntegratedDevelopmentEnvironment = class(TObject)
  private
    fCodeBlocks: TCodeBlocksIntegratedDevelopmentEnvironment;
    fEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
    fVisualStudioCodeEditors: TVisualStudioCodeIntegratedDevelopmentEnvironmentArray;
    function GetVisualStudioCode: TVisualStudioCodeIntegratedDevelopmentEnvironment;
  protected
    property Environment: TDreamcastSoftwareDevelopmentEnvironment
      read fEnvironment;
  public
    constructor Create(AEnvironment: TDreamcastSoftwareDevelopmentEnvironment);
    destructor Destroy; override;
    property CodeBlocks: TCodeBlocksIntegratedDevelopmentEnvironment
      read fCodeBlocks;
    // Retrieve an editor from its identifier or its name (nil if unknown)
    function GetVisualStudioCodeEditor(const EditorId: string):
      TVisualStudioCodeIntegratedDevelopmentEnvironment;
    function GetVisualStudioCodeEditorByName(const Name: string):
      TVisualStudioCodeIntegratedDevelopmentEnvironment;
    // Visual Studio Code itself
    property VisualStudioCode: TVisualStudioCodeIntegratedDevelopmentEnvironment
      read GetVisualStudioCode;
    // Visual Studio Code and its forks
    property VisualStudioCodeEditors: TVisualStudioCodeIntegratedDevelopmentEnvironmentArray
      read fVisualStudioCodeEditors;
  end;

implementation

uses
  SysTools,
  StrTools,
  RunTools,
  RefBase,
  CBTools,
  VSCTools;

const
  CODEBLOCKS_SUCCESSFUL_STATE = 'Code::Blocks is now';

{ TCodeBlocksIntegratedDevelopmentEnvironment }

function TCodeBlocksIntegratedDevelopmentEnvironment.GetAvailableUsers: TStringList;
begin
  Result := fAvailableUsers;
end;

procedure TCodeBlocksIntegratedDevelopmentEnvironment.RetrievePatchState;
begin
  fPatchState := RunCodeBlocksPatcher('internal-refresh');
end;

function TCodeBlocksIntegratedDevelopmentEnvironment.GetInstallationDirectory: TFileName;
begin
  Result := Settings.InstallationDirectory;
end;

function TCodeBlocksIntegratedDevelopmentEnvironment.GetInstalled: Boolean;
begin
  Result := Settings.Installed;
end;

function TCodeBlocksIntegratedDevelopmentEnvironment.GetInstalledUsers: TStringList;
begin
  Result := fInstalledUsers;
end;

function TCodeBlocksIntegratedDevelopmentEnvironment.RunCodeBlocksPatcher(
  const Operation: string): string;
var
  CmdLine: string;

begin
  CmdLine := Format('--operation=%s --home-dir="%s" --internal-integration', [
    Operation, GetBaseInstallationHomeDirectory]);
{$IFDEF DEBUG}
  DebugLog('RunCodeBlocksPatcher: ' + CmdLine);
{$ENDIF}
  Result := Run(fIntegratedDevelopmentEnvironment.Environment.FileSystem.Shell
    .CodeBlocksPatcherExecutable, CmdLine);

  fLastErrorMessage := Right(CODEBLOCKS_PATCHER_ERROR_TAG, Result);
  fLastErrorMessage := Trim(StringReplace(fLastErrorMessage,
    CODEBLOCKS_PATCHER_ERROR_SEPARATOR, sLineBreak, [rfReplaceAll]));
  fLastOperationSuccess := IsEmpty(fLastErrorMessage);
end;

procedure TCodeBlocksIntegratedDevelopmentEnvironment.Refresh;
begin
  Refresh(False);
end;

procedure TCodeBlocksIntegratedDevelopmentEnvironment.Refresh(
  const Force: Boolean);
var
  LogContext: TLogMessageContext;

begin
  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%}, ClassName);
  try

    if Force then
    begin
      RetrievePatchState;
      Settings.ClearCachedAvailableUsers;
    end;

    LogMessage(LogContext, Format('Force: "%s"', [
      BoolToStr(Force, True)
    ]));

    if Settings.LoadConfiguration(Force) then
    begin
      // Save updated configuration
      if Force then
        Settings.SaveConfiguration;

      // Available Users
      fAvailableUsers.Clear;
      fAvailableUsers.Assign(Settings.AvailableUsers);

      // Installed Users
      fInstalledUsers.Clear;
      if Settings.Installed then
        fInstalledUsers.Assign(Settings.InstalledUsers);
    end;

  finally
    LogMessageExit(LogContext);
  end;
end;

procedure TCodeBlocksIntegratedDevelopmentEnvironment.UpdateStateFromElevatedTask(
  const SwapExchangeFileName: TFileName);
begin
  fLastOperationSuccess := not FileExists(SwapExchangeFileName);
  if not LastOperationSuccess then
    fLastErrorMessage := LoadFileToString(SwapExchangeFileName);
end;

constructor TCodeBlocksIntegratedDevelopmentEnvironment.Create(
  AIntegratedDevelopmentEnvironment: TIntegratedDevelopmentEnvironment);
begin
  fIntegratedDevelopmentEnvironment := AIntegratedDevelopmentEnvironment;
  fAvailableUsers := TStringList.Create;
  fInstalledUsers := TStringList.Create;
  fSettings := TDreamcastSoftwareDevelopmentSettingsCodeBlocks.Create;
end;

destructor TCodeBlocksIntegratedDevelopmentEnvironment.Destroy;
begin
  fSettings.Free;
  fInstalledUsers.Free;
  fAvailableUsers.Free;
  inherited Destroy;
end;

function TCodeBlocksIntegratedDevelopmentEnvironment.InitializeProfiles: Boolean;
const
  SUCCESS = 'Profiles initialization finished!';

var
  ProfilesInitializationState: string;

begin
  ProfilesInitializationState := RunCodeBlocksPatcher('internal-initialize-profiles');
  Result := IsInString(SUCCESS, ProfilesInitializationState);
  Refresh(True);
end;

function TCodeBlocksIntegratedDevelopmentEnvironment.Install(
  const CodeBlocksInstallationDirectory: TFileName): Boolean;
begin
  Result := IsInString(CODEBLOCKS_SUCCESSFUL_STATE,
    RunCodeBlocksPatcher(Format('install --install-dir="%s" ', [
      CodeBlocksInstallationDirectory])));
  Refresh;
end;

function TCodeBlocksIntegratedDevelopmentEnvironment.Uninstall: Boolean;
begin
  Result := IsInString(CODEBLOCKS_SUCCESSFUL_STATE,
    RunCodeBlocksPatcher('uninstall'));
  Refresh;
end;

function TCodeBlocksIntegratedDevelopmentEnvironment.Reinstall: Boolean;
begin
  Result := IsInString(CODEBLOCKS_SUCCESSFUL_STATE,
    RunCodeBlocksPatcher('reinstall'));
  Refresh;
end;

{ TVisualStudioCodeIntegratedDevelopmentEnvironment }

function TVisualStudioCodeIntegratedDevelopmentEnvironment.GetExtensionFileName: TFileName;
begin
  Result := fIntegratedDevelopmentEnvironment.Environment.FileSystem.Shell
    .VisualStudioCodeExtensionFileName;
end;

function TVisualStudioCodeIntegratedDevelopmentEnvironment.GetInstalled: Boolean;
begin
  Result := fInstalledUsers.Count > 0;
end;

constructor TVisualStudioCodeIntegratedDevelopmentEnvironment.Create(
  AIntegratedDevelopmentEnvironment: TIntegratedDevelopmentEnvironment;
  const AEditorId, AName: string);
begin
  fIntegratedDevelopmentEnvironment := AIntegratedDevelopmentEnvironment;
  fEditorId := AEditorId;
  fName := AName;
  fAvailableUsers := TStringList.Create;
  fInstalledUsers := TStringList.Create;
  fLastOperationSuccess := True;
end;

destructor TVisualStudioCodeIntegratedDevelopmentEnvironment.Destroy;
begin
  fInstalledUsers.Free;
  fAvailableUsers.Free;
  inherited Destroy;
end;

function TVisualStudioCodeIntegratedDevelopmentEnvironment.Install: Boolean;
begin
  Result := VisualStudioCodeInstallExtension(EditorId, ExtensionFileName,
    fLastErrorMessage);
  fLastOperationSuccess := Result;
  Refresh;
end;

function TVisualStudioCodeIntegratedDevelopmentEnvironment.Uninstall: Boolean;
begin
  Result := VisualStudioCodeUninstallExtension(EditorId,
    VSCODE_DREAMSDK_EXTENSION_ID, fLastErrorMessage);
  fLastOperationSuccess := Result;
  Refresh;
end;

procedure TVisualStudioCodeIntegratedDevelopmentEnvironment.Refresh;
var
  LogContext: TLogMessageContext;
  Installation: TVisualStudioCodeInstallation;
  Users: TWindowsUserAccountInformationArray;
  i: Integer;

begin
  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%}, ClassName);
  try
    fDetected := GetVisualStudioCodeInstallation(EditorId, Installation);
    fVersion := Installation.Version;
    fCommandLineInterface := Installation.CommandLineInterface;
    fInstallationDirectory := Installation.InstallationDirectory;

    fAvailableUsers.Clear;
    if GetVisualStudioCodeAvailableUsers(Users) then
      for i := Low(Users) to High(Users) do
        fAvailableUsers.Add(Users[i].FriendlyName);

    // The readable profiles are checked directly; for the other ones (when
    // not elevated), the state saved in ide.conf by the elevated operations
    // is used. ide.conf is updated when all the profiles could be read.
    RefreshVisualStudioCodeInstalledUsers(EditorId, VSCODE_DREAMSDK_EXTENSION_ID,
      fIntegratedDevelopmentEnvironment.Environment.FileSystem.Shell
        .IntegratedDevelopmentEnvironmentConfigurationFile, fInstalledUsers);

    fRefreshed := True;

    LogMessage(LogContext, Format('%s: Detected: %s, Version: "%s", Installed users: %d', [
      EditorId, BoolToStr(fDetected, True), fVersion, fInstalledUsers.Count]));
  finally
    LogMessageExit(LogContext);
  end;
end;

procedure TVisualStudioCodeIntegratedDevelopmentEnvironment.UpdateStateFromElevatedTask(
  const SwapExchangeFileName: TFileName);
begin
  fLastOperationSuccess := not FileExists(SwapExchangeFileName);
  if not LastOperationSuccess then
    fLastErrorMessage := LoadFileToString(SwapExchangeFileName);
end;

{ TIntegratedDevelopmentEnvironment }

function TIntegratedDevelopmentEnvironment.GetVisualStudioCode:
  TVisualStudioCodeIntegratedDevelopmentEnvironment;
begin
  Result := GetVisualStudioCodeEditor(VSCODE_EDITOR_VSCODE);
end;

function TIntegratedDevelopmentEnvironment.GetVisualStudioCodeEditor(
  const EditorId: string): TVisualStudioCodeIntegratedDevelopmentEnvironment;
var
  i: Integer;

begin
  Result := nil;
  for i := Low(fVisualStudioCodeEditors) to High(fVisualStudioCodeEditors) do
    if SameText(fVisualStudioCodeEditors[i].EditorId, EditorId) then
      Exit(fVisualStudioCodeEditors[i]);
end;

function TIntegratedDevelopmentEnvironment.GetVisualStudioCodeEditorByName(
  const Name: string): TVisualStudioCodeIntegratedDevelopmentEnvironment;
var
  i: Integer;

begin
  Result := nil;
  for i := Low(fVisualStudioCodeEditors) to High(fVisualStudioCodeEditors) do
    if SameText(fVisualStudioCodeEditors[i].Name, Name) then
      Exit(fVisualStudioCodeEditors[i]);
end;

constructor TIntegratedDevelopmentEnvironment.Create(
  AEnvironment: TDreamcastSoftwareDevelopmentEnvironment);
var
  i: Integer;

begin
  fCodeBlocks := TCodeBlocksIntegratedDevelopmentEnvironment.Create(Self);
  SetLength(fVisualStudioCodeEditors, Length(VSCODE_EDITORS));
  for i := Low(VSCODE_EDITORS) to High(VSCODE_EDITORS) do
    fVisualStudioCodeEditors[i - Low(VSCODE_EDITORS)] :=
      TVisualStudioCodeIntegratedDevelopmentEnvironment.Create(Self,
        VSCODE_EDITORS[i].Id, VSCODE_EDITORS[i].Name);
  fEnvironment := AEnvironment;
end;

destructor TIntegratedDevelopmentEnvironment.Destroy;
var
  i: Integer;

begin
  for i := Low(fVisualStudioCodeEditors) to High(fVisualStudioCodeEditors) do
    fVisualStudioCodeEditors[i].Free;
  fCodeBlocks.Free;
  inherited Destroy;
end;

end.

