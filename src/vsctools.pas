unit VSCTools;

(*
  Visual Studio Code integration for DreamSDK.

  The DreamSDK extension for Visual Studio Code is distributed as a .vsix file
  and installed in the extensions directory of every Windows user profile
  (like the Code::Blocks integration). Visual Studio Code doesn't have a
  machine-wide extensions directory, but its command-line interface (CLI) can
  target an explicit directory with "--extensions-dir": this keeps the
  "extensions.json" registry of that directory up-to-date, which isn't the case
  if the extension is copied by hand.

  As the target directory is explicit, this works from an elevated process
  (Setup, DreamSDK Manager elevated tasks) for all the users.

  The same extension is installed in Visual Studio Code and in its forks (e.g.
  VSCodium): each supported editor is described in the VSCODE_EDITORS table.
*)

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  SysTools;

const
  VSCODE_DREAMSDK_EXTENSION_ID = 'dreamsdk.dreamsdk';

  // Identifiers of the supported editors (see VSCODE_EDITORS)
  VSCODE_EDITOR_VSCODE = 'vscode';
  VSCODE_EDITOR_VSCODIUM = 'vscodium';

type
  (* A supported editor: Visual Studio Code or one of its forks. *)
  TVisualStudioCodeEditor = record
    // Identifier, used by DreamSDK Setup and DreamSDK Manager
    Id: string;
    // Name displayed to the user
    Name: string;
    // Names of the editor in the "Uninstall" registry key, separated by '|'
    // (system and user installations)
    DisplayNames: string;
    // Default user installation directory, relative to the user profile
    UserInstallationDirectory: string;
    // Command-line interface, relative to the installation directory
    CommandLineInterface: string;
    // Extensions directory, relative to the user profile
    ExtensionsDirectory: string;
    // Section in the DreamSDK IDE configuration file (ide.conf)
    ConfigurationSection: string;
  end;
  TVisualStudioCodeEditorArray = array of TVisualStudioCodeEditor;

  TVisualStudioCodeInstallationScope = (vcisSystem, vcisUser);

  TVisualStudioCodeInstallation = record
    Scope: TVisualStudioCodeInstallationScope;
    Version: string;
    InstallationDirectory: TFileName;
    CommandLineInterface: TFileName;
  end;
  TVisualStudioCodeInstallationArray = array of TVisualStudioCodeInstallation;

const
  VSCODE_EDITORS: array[0..1] of TVisualStudioCodeEditor = (
    (
      Id: VSCODE_EDITOR_VSCODE;
      Name: 'Visual Studio Code';
      DisplayNames: 'Microsoft Visual Studio Code|Microsoft Visual Studio Code (User)';
      UserInstallationDirectory: 'AppData\Local\Programs\Microsoft VS Code\';
      CommandLineInterface: 'bin\code.cmd';
      ExtensionsDirectory: '.vscode\extensions';
      ConfigurationSection: 'VisualStudioCode'
    ),
    (
      Id: VSCODE_EDITOR_VSCODIUM;
      Name: 'VSCodium';
      DisplayNames: 'VSCodium|VSCodium (User)';
      UserInstallationDirectory: 'AppData\Local\Programs\VSCodium\';
      CommandLineInterface: 'bin\codium.cmd';
      ExtensionsDirectory: '.vscode-oss\extensions';
      ConfigurationSection: 'VSCodium'
    )
  );

(* Retrieve a supported editor from its identifier. *)
function GetVisualStudioCodeEditor(const EditorId: string;
  out Editor: TVisualStudioCodeEditor): Boolean;

(* Retrieve all the installations of an editor on this computer. *)
function GetVisualStudioCodeInstallations(const EditorId: string;
  out Installations: TVisualStudioCodeInstallationArray): Boolean;

(* Retrieve the most recent installation of an editor. Any installation can
   install the extension for any user, as the CLI only unpacks the .vsix. *)
function GetVisualStudioCodeInstallation(const EditorId: string;
  out Installation: TVisualStudioCodeInstallation): Boolean;

(* Retrieve all Windows users that can use the editors, i.e. all the users with
   a profile directory (like Code::Blocks). *)
function GetVisualStudioCodeAvailableUsers(
  out AvailableUsers: TWindowsUserAccountInformationArray): Boolean;

(* Retrieve all Windows users that have the extension installed in an editor. *)
function GetVisualStudioCodeInstalledUsers(const EditorId, ExtensionId: string;
  out InstalledUsers: TWindowsUserAccountInformationArray): Boolean;

(* Retrieve the extensions directory of an editor for an user. *)
function GetVisualStudioCodeExtensionsDirectory(const EditorId: string;
  const User: TWindowsUserAccountInformation): TFileName;

(* Check if the extension is installed in an editor for an user. *)
function IsVisualStudioCodeExtensionInstalled(const EditorId: string;
  const User: TWindowsUserAccountInformation;
  const ExtensionId: string): Boolean;

(* Retrieve the names of the users that have the extension installed in an
   editor. The profiles of the other users can't be read without elevation:
   for them, the list saved in the configuration file (ide.conf) is used.
   That list is saved when all the profiles could be read (i.e., elevated:
   DreamSDK Setup, DreamSDK Manager elevated tasks). Returns True if all the
   profiles could be read. *)
function RefreshVisualStudioCodeInstalledUsers(const EditorId,
  ExtensionId: string; const ConfigurationFileName: TFileName;
  InstalledUsers: TStringList): Boolean;

(* Install the extension (.vsix) in an editor for all the available users, or
   for the specified users. *)
function VisualStudioCodeInstallExtension(const EditorId: string;
  const VsixFileName: TFileName; out ErrorMessage: string): Boolean; overload;
function VisualStudioCodeInstallExtension(const EditorId: string;
  const VsixFileName: TFileName;
  const Users: TWindowsUserAccountInformationArray;
  out ErrorMessage: string): Boolean; overload;

(* Remove the extension from an editor for all the available users, or for the
   specified users. This works even if the editor was uninstalled. *)
function VisualStudioCodeUninstallExtension(const EditorId,
  ExtensionId: string; out ErrorMessage: string): Boolean; overload;
function VisualStudioCodeUninstallExtension(const EditorId,
  ExtensionId: string; const Users: TWindowsUserAccountInformationArray;
  out ErrorMessage: string): Boolean; overload;

implementation

uses
  Windows,
  Registry,
  IniFiles,
  FPJson,
  JSONParser,
  FSTools,
  StrTools,
  RunTools,
  Version;

const
  UNINSTALL_KEY = 'Software\Microsoft\Windows\CurrentVersion\Uninstall';
  EXTENSIONS_REGISTRY = 'extensions.json';
  CLI_NOT_INSTALLED_TAG = 'is not installed';
  CONFIGURATION_INSTALLED_USERS = 'InstalledUsers';
  CONFIGURATION_USERS_SEPARATOR = '|';

// =============================================================================
// Utilities
// =============================================================================

(* Run a batch file command line (the CLI is "code.cmd") through cmd.exe and
   wait for it. RunTools.Run uses TProcess (CreateProcessW, hidden window in
   Release mode), so profile paths with non-ASCII characters are supported. *)
function RunBatchCommand(const CommandLine: string; out ProcessExitCode: Integer;
  out OutputBuffer: string): Boolean;
var
  ProcessId: Integer;

begin
  ProcessId := 0;
  ProcessExitCode := -1;

  // "/s": the outer quotes are removed and the rest is run as is
  OutputBuffer := Trim(Run(SysUtils.GetEnvironmentVariable('ComSpec'),
    Format('/d /s /c "%s"', [CommandLine]), ProcessId, ProcessExitCode, False));
  Result := (ProcessExitCode = 0);

{$IFDEF DEBUG}
  DebugLog(Format('RunBatchCommand: [%s], exit code: %d', [CommandLine, ProcessExitCode]));
{$ENDIF}
end;

(* Parse a JSON file, returning nil if it's not possible. *)
function LoadJSONFile(const FileName: TFileName): TJSONData;
begin
  Result := nil;
  if FileExists(FileName) then
    try
      Result := GetJSON(LoadFileToString(FileName));
    except
      Result := nil;
    end;
end;

(* Retrieve the editor version from its "package.json" file. The "resources"
   directory is directly in the installation directory, or in a sub-directory
   named after the commit hash for recent versions. *)
function GetVersionFromInstallationDirectory(
  const InstallationDirectory: TFileName): string;
const
  PRODUCT_FILE = 'resources\app\package.json';

var
  SearchRec: TSearchRec;
  JSONData: TJSONData;
  PackageFileName: TFileName;

  function _ReadVersion(const FileName: TFileName): string;
  begin
    Result := EmptyStr;
    JSONData := LoadJSONFile(FileName);
    if Assigned(JSONData) then
      try
        if JSONData is TJSONObject then
          Result := TJSONObject(JSONData).Get('version', EmptyStr);
      finally
        JSONData.Free;
      end;
  end;

begin
  Result := _ReadVersion(InstallationDirectory + PRODUCT_FILE);
  if not IsEmpty(Result) then
    Exit;

  if FindFirst(InstallationDirectory + '*', faDirectory, SearchRec) = 0 then
    try
      repeat
        if ((SearchRec.Attr and faDirectory) <> 0)
          and (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        begin
          PackageFileName := InstallationDirectory + SearchRec.Name
            + DirectorySeparator + PRODUCT_FILE;
          Result := _ReadVersion(PackageFileName);
          if not IsEmpty(Result) then
            Break;
        end;
      until FindNext(SearchRec) <> 0;
    finally
      SysUtils.FindClose(SearchRec);
    end;
end;

(* Add an installation to the list, if valid and not already in the list. *)
procedure AddInstallation(var Installations: TVisualStudioCodeInstallationArray;
  const Editor: TVisualStudioCodeEditor; InstallationDirectory: TFileName;
  const Scope: TVisualStudioCodeInstallationScope; Version: string);
var
  i: Integer;
  Installation: TVisualStudioCodeInstallation;

begin
  if IsEmpty(InstallationDirectory) then
    Exit;
  InstallationDirectory := IncludeTrailingPathDelimiter(InstallationDirectory);
  if not FileExists(InstallationDirectory + Editor.CommandLineInterface) then
    Exit;

  for i := Low(Installations) to High(Installations) do
    if SameText(Installations[i].InstallationDirectory, InstallationDirectory) then
      Exit;

  if IsEmpty(Version) then
    Version := GetVersionFromInstallationDirectory(InstallationDirectory);

  Installation := Default(TVisualStudioCodeInstallation);
  Installation.Scope := Scope;
  Installation.Version := Version;
  Installation.InstallationDirectory := InstallationDirectory;
  Installation.CommandLineInterface := InstallationDirectory + Editor.CommandLineInterface;
  Insert(Installation, Installations, MaxInt);

{$IFDEF DEBUG}
  DebugLog(Format('%s %s found: "%s"', [Editor.Name, Version, InstallationDirectory]));
{$ENDIF}
end;

(* Check if a registry "DisplayName" is one of the editor names. *)
function IsEditorDisplayName(const Editor: TVisualStudioCodeEditor;
  const DisplayName: string): Boolean;
var
  DisplayNames: TStringList;
  i: Integer;

begin
  Result := False;
  DisplayNames := TStringList.Create;
  try
    StringToStringList(Editor.DisplayNames, '|', DisplayNames);
    for i := 0 to DisplayNames.Count - 1 do
      if SameText(DisplayName, DisplayNames[i]) then
        Exit(True);
  finally
    DisplayNames.Free;
  end;
end;

(* Search the editor in the "Uninstall" key of a registry hive. *)
procedure ScanUninstallKey(var Installations: TVisualStudioCodeInstallationArray;
  const Editor: TVisualStudioCodeEditor; const RootKey: HKEY;
  const KeyPrefix: string; const AccessFlags: LongWord;
  const Scope: TVisualStudioCodeInstallationScope);
var
  Registry: TRegistry;
  SubKeys: TStringList;
  i: Integer;
  BaseKey, DisplayName, InstallLocation, DisplayVersion: string;

begin
  BaseKey := KeyPrefix + UNINSTALL_KEY;
  Registry := TRegistry.Create(KEY_READ or AccessFlags);
  SubKeys := TStringList.Create;
  try
    Registry.RootKey := RootKey;
    if not Registry.OpenKeyReadOnly(BaseKey) then
      Exit;
    Registry.GetKeyNames(SubKeys);
    Registry.CloseKey;

    for i := 0 to SubKeys.Count - 1 do
      if Registry.OpenKeyReadOnly(BaseKey + '\' + SubKeys[i]) then
        try
          DisplayName := EmptyStr;
          if Registry.ValueExists('DisplayName') then
            DisplayName := Registry.ReadString('DisplayName');
          if IsEditorDisplayName(Editor, DisplayName) then
          begin
            InstallLocation := EmptyStr;
            DisplayVersion := EmptyStr;
            if Registry.ValueExists('InstallLocation') then
              InstallLocation := Registry.ReadString('InstallLocation');
            if Registry.ValueExists('DisplayVersion') then
              DisplayVersion := Registry.ReadString('DisplayVersion');
            AddInstallation(Installations, Editor, InstallLocation, Scope,
              DisplayVersion);
          end;
        finally
          Registry.CloseKey;
        end;
  finally
    SubKeys.Free;
    Registry.Free;
  end;
end;

(* Remove the extension entries from "extensions.json" and delete the extension
   directories. Used after the CLI uninstall (which leaves the directories until
   the next editor start) or when the CLI is not available. *)
function CleanupExtension(const ExtensionsDirectory: TFileName;
  const ExtensionId: string): Boolean;
var
  RegistryFileName: TFileName;
  JSONData: TJSONData;
  Extensions: TJSONArray;
  Extension: TJSONData;
  i: Integer;
  IsModified: Boolean;
  SearchRec: TSearchRec;

begin
  Result := True;

  // Remove the entries from the registry
  RegistryFileName := ExtensionsDirectory + EXTENSIONS_REGISTRY;
  JSONData := LoadJSONFile(RegistryFileName);
  if Assigned(JSONData) then
    try
      if JSONData is TJSONArray then
      begin
        Extensions := TJSONArray(JSONData);
        IsModified := False;
        for i := Extensions.Count - 1 downto 0 do
        begin
          Extension := Extensions[i].FindPath('identifier.id');
          if Assigned(Extension) and SameText(Extension.AsString, ExtensionId) then
          begin
            Extensions.Delete(i);
            IsModified := True;
          end;
        end;
        if IsModified then
          SaveStringToFile(Extensions.AsJSON, RegistryFileName);
      end;
    finally
      JSONData.Free;
    end;

  // Remove the "<publisher>.<name>-<version>" directories
  if FindFirst(ExtensionsDirectory + ExtensionId + '-*', faDirectory, SearchRec) = 0 then
    try
      repeat
        if (SearchRec.Attr and faDirectory) <> 0 then
          Result := KillDirectory(ExtensionsDirectory + SearchRec.Name) and Result;
      until FindNext(SearchRec) <> 0;
    finally
      SysUtils.FindClose(SearchRec);
    end;
end;

(* Run the editor CLI for an extensions directory. A temporary user data
   directory is used, so the profile of the user running the CLI (e.g. the
   administrator running Setup) isn't modified. *)
function RunCommandLineInterface(const Installation: TVisualStudioCodeInstallation;
  const Arguments: string; const ExtensionsDirectory: TFileName;
  out OutputBuffer: string): Boolean;
var
  UserDataDirectory: TFileName;
  ProcessExitCode: Integer;

begin
  UserDataDirectory := ChangeFileExt(GetTemporaryFileName, '.vscode');
  try
    Result := RunBatchCommand(Format('"%s" %s --extensions-dir "%s" --user-data-dir "%s"', [
      Installation.CommandLineInterface,
      Arguments,
      ExcludeTrailingPathDelimiter(ExtensionsDirectory),
      UserDataDirectory
    ]), ProcessExitCode, OutputBuffer);
  finally
    if DirectoryExists(UserDataDirectory) then
      KillDirectory(UserDataDirectory);
  end;
end;

(* Check if the profile of an user can be read (the profiles of the other users
   can't be read without elevation). *)
function IsUserProfileReadable(const User: TWindowsUserAccountInformation): Boolean;
var
  SearchRec: TSearchRec;

begin
  Result := FindFirst(IncludeTrailingPathDelimiter(User.ProfilePath) + '*',
    faAnyFile, SearchRec) = 0;
  if Result then
    SysUtils.FindClose(SearchRec);
end;

(* Format an error message for an user. *)
function FormatUserError(const User: TWindowsUserAccountInformation;
  const OutputBuffer: string): string;
begin
  Result := Format('%s: %s', [User.FriendlyName, OutputBuffer]);
end;

// =============================================================================
// Public functions
// =============================================================================

function GetVisualStudioCodeEditor(const EditorId: string;
  out Editor: TVisualStudioCodeEditor): Boolean;
var
  i: Integer;

begin
  Result := False;
  Editor := Default(TVisualStudioCodeEditor);
  for i := Low(VSCODE_EDITORS) to High(VSCODE_EDITORS) do
    if SameText(VSCODE_EDITORS[i].Id, EditorId) then
    begin
      Editor := VSCODE_EDITORS[i];
      Exit(True);
    end;
end;

function GetVisualStudioCodeInstallations(const EditorId: string;
  out Installations: TVisualStudioCodeInstallationArray): Boolean;
var
  Editor: TVisualStudioCodeEditor;
  UsersList: TWindowsUserAccountInformationArray;
  i: Integer;

begin
  Installations := Default(TVisualStudioCodeInstallationArray);
  if not GetVisualStudioCodeEditor(EditorId, Editor) then
    Exit(False);

  // System installations (64-bit and 32-bit views)
  ScanUninstallKey(Installations, Editor, HKEY_LOCAL_MACHINE, EmptyStr,
    KEY_WOW64_64KEY, vcisSystem);
  ScanUninstallKey(Installations, Editor, HKEY_LOCAL_MACHINE, EmptyStr,
    KEY_WOW64_32KEY, vcisSystem);

  // User installations: registry of the loaded hives, then the default
  // installation directory (as the hives of the other users are not loaded)
  if GetUserList(UsersList) then
    for i := Low(UsersList) to High(UsersList) do
    begin
      if not IsEmpty(UsersList[i].SID) then
        ScanUninstallKey(Installations, Editor, HKEY_USERS,
          UsersList[i].SID + '\', 0, vcisUser);
      if not IsEmpty(UsersList[i].ProfilePath) then
        AddInstallation(Installations, Editor,
          IncludeTrailingPathDelimiter(UsersList[i].ProfilePath)
          + Editor.UserInstallationDirectory, vcisUser, EmptyStr);
    end;

  Result := Length(Installations) > 0;
end;

function GetVisualStudioCodeInstallation(const EditorId: string;
  out Installation: TVisualStudioCodeInstallation): Boolean;
var
  Installations: TVisualStudioCodeInstallationArray;
  i: Integer;

begin
  Installation := Default(TVisualStudioCodeInstallation);
  Result := GetVisualStudioCodeInstallations(EditorId, Installations);
  if Result then
  begin
    Installation := Installations[0];
    // CompareVersion returns a positive value if the second version is newer
    for i := 1 to High(Installations) do
      if CompareVersion(Installation.Version, Installations[i].Version) > 0 then
        Installation := Installations[i];
  end;
end;

function GetVisualStudioCodeAvailableUsers(
  out AvailableUsers: TWindowsUserAccountInformationArray): Boolean;
var
  UsersList: TWindowsUserAccountInformationArray;
  i: Integer;

begin
  AvailableUsers := Default(TWindowsUserAccountInformationArray);
  if GetUserList(UsersList) then
    for i := Low(UsersList) to High(UsersList) do
      if (not IsEmpty(UsersList[i].ProfilePath))
        and DirectoryExists(UsersList[i].ProfilePath) then
        Insert(UsersList[i], AvailableUsers, MaxInt);
  Result := Length(AvailableUsers) > 0;
end;

function GetVisualStudioCodeInstalledUsers(const EditorId, ExtensionId: string;
  out InstalledUsers: TWindowsUserAccountInformationArray): Boolean;
var
  AvailableUsers: TWindowsUserAccountInformationArray;
  i: Integer;

begin
  InstalledUsers := Default(TWindowsUserAccountInformationArray);
  if GetVisualStudioCodeAvailableUsers(AvailableUsers) then
    for i := Low(AvailableUsers) to High(AvailableUsers) do
      if IsVisualStudioCodeExtensionInstalled(EditorId, AvailableUsers[i], ExtensionId) then
        Insert(AvailableUsers[i], InstalledUsers, MaxInt);
  Result := Length(InstalledUsers) > 0;
end;

function GetVisualStudioCodeExtensionsDirectory(const EditorId: string;
  const User: TWindowsUserAccountInformation): TFileName;
var
  Editor: TVisualStudioCodeEditor;

begin
  Result := EmptyStr;
  if GetVisualStudioCodeEditor(EditorId, Editor) then
    Result := IncludeTrailingPathDelimiter(
      IncludeTrailingPathDelimiter(User.ProfilePath) + Editor.ExtensionsDirectory);
end;

function IsVisualStudioCodeExtensionInstalled(const EditorId: string;
  const User: TWindowsUserAccountInformation;
  const ExtensionId: string): Boolean;
var
  ExtensionsDirectory: TFileName;
  JSONData: TJSONData;
  Extension: TJSONData;
  i: Integer;

begin
  Result := False;
  ExtensionsDirectory := GetVisualStudioCodeExtensionsDirectory(EditorId, User);
  if IsEmpty(ExtensionsDirectory) then
    Exit;
  JSONData := LoadJSONFile(ExtensionsDirectory + EXTENSIONS_REGISTRY);
  if Assigned(JSONData) then
    try
      if JSONData is TJSONArray then
        for i := 0 to JSONData.Count - 1 do
        begin
          Extension := JSONData.Items[i].FindPath('identifier.id');
          if Assigned(Extension) and SameText(Extension.AsString, ExtensionId) then
            Exit(True);
        end;
    finally
      JSONData.Free;
    end;
end;

function VisualStudioCodeInstallExtension(const EditorId: string;
  const VsixFileName: TFileName; out ErrorMessage: string): Boolean;
var
  AvailableUsers: TWindowsUserAccountInformationArray;

begin
  if not GetVisualStudioCodeAvailableUsers(AvailableUsers) then
  begin
    ErrorMessage := 'No Windows user profiles are available.';
    Exit(False);
  end;
  Result := VisualStudioCodeInstallExtension(EditorId, VsixFileName,
    AvailableUsers, ErrorMessage);
end;

function VisualStudioCodeInstallExtension(const EditorId: string;
  const VsixFileName: TFileName;
  const Users: TWindowsUserAccountInformationArray;
  out ErrorMessage: string): Boolean;
var
  Editor: TVisualStudioCodeEditor;
  Installation: TVisualStudioCodeInstallation;
  Errors: TStringList;
  i: Integer;
  ExtensionsDirectory: TFileName;
  OutputBuffer: string;

begin
  Result := False;
  ErrorMessage := EmptyStr;

  if not GetVisualStudioCodeEditor(EditorId, Editor) then
  begin
    ErrorMessage := Format('Unknown editor: "%s"', [EditorId]);
    Exit;
  end;

  if not FileExists(VsixFileName) then
  begin
    ErrorMessage := Format('The extension package was not found: "%s"', [VsixFileName]);
    Exit;
  end;

  if not GetVisualStudioCodeInstallation(EditorId, Installation) then
  begin
    ErrorMessage := Format('%s is not installed.', [Editor.Name]);
    Exit;
  end;

  Errors := TStringList.Create;
  try
    for i := Low(Users) to High(Users) do
    begin
      // Creating the directory from here keeps the user's rights (inherited
      // from the profile directory)
      ExtensionsDirectory := GetVisualStudioCodeExtensionsDirectory(EditorId, Users[i]);
      ForceDirectories(ExtensionsDirectory);

      // "--force" replaces the previous version, e.g. when DreamSDK is updated
      if not RunCommandLineInterface(Installation,
        Format('--install-extension "%s" --force', [VsixFileName]),
        ExtensionsDirectory, OutputBuffer) then
        Errors.Add(FormatUserError(Users[i], OutputBuffer));
    end;

    Result := Errors.Count = 0;
    ErrorMessage := Trim(Errors.Text);
  finally
    Errors.Free;
  end;
end;

function VisualStudioCodeUninstallExtension(const EditorId,
  ExtensionId: string; out ErrorMessage: string): Boolean;
var
  AvailableUsers: TWindowsUserAccountInformationArray;

begin
  GetVisualStudioCodeAvailableUsers(AvailableUsers);
  Result := VisualStudioCodeUninstallExtension(EditorId, ExtensionId,
    AvailableUsers, ErrorMessage);
end;

function VisualStudioCodeUninstallExtension(const EditorId,
  ExtensionId: string; const Users: TWindowsUserAccountInformationArray;
  out ErrorMessage: string): Boolean;
var
  Editor: TVisualStudioCodeEditor;
  Installation: TVisualStudioCodeInstallation;
  IsCommandLineInterfaceAvailable: Boolean;
  Errors: TStringList;
  i: Integer;
  ExtensionsDirectory: TFileName;
  OutputBuffer: string;

begin
  ErrorMessage := EmptyStr;
  if not GetVisualStudioCodeEditor(EditorId, Editor) then
  begin
    ErrorMessage := Format('Unknown editor: "%s"', [EditorId]);
    Exit(False);
  end;
  IsCommandLineInterfaceAvailable := GetVisualStudioCodeInstallation(EditorId,
    Installation);

  Errors := TStringList.Create;
  try
    for i := Low(Users) to High(Users) do
    begin
      ExtensionsDirectory := GetVisualStudioCodeExtensionsDirectory(EditorId, Users[i]);
      if not DirectoryExists(ExtensionsDirectory) then
        Continue;

      // Use the CLI if possible; "not installed" isn't an error
      if IsCommandLineInterfaceAvailable
        and IsVisualStudioCodeExtensionInstalled(EditorId, Users[i], ExtensionId)
        and (not RunCommandLineInterface(Installation,
          Format('--uninstall-extension %s', [ExtensionId]),
          ExtensionsDirectory, OutputBuffer))
        and (Pos(CLI_NOT_INSTALLED_TAG, OutputBuffer) = 0) then
        Errors.Add(FormatUserError(Users[i], OutputBuffer));

      // Remove what's left (or everything, if the CLI isn't available)
      if not CleanupExtension(ExtensionsDirectory, ExtensionId) then
        Errors.Add(FormatUserError(Users[i],
          'Unable to remove the extension directory.'));
    end;

    Result := Errors.Count = 0;
    ErrorMessage := Trim(Errors.Text);
  finally
    Errors.Free;
  end;
end;

function RefreshVisualStudioCodeInstalledUsers(const EditorId,
  ExtensionId: string; const ConfigurationFileName: TFileName;
  InstalledUsers: TStringList): Boolean;
var
  Editor: TVisualStudioCodeEditor;
  AvailableUsers: TWindowsUserAccountInformationArray;
  SavedUsers: TStringList;
  IniFile: TIniFile;
  i: Integer;

begin
  Result := True;
  InstalledUsers.Clear;
  if not GetVisualStudioCodeEditor(EditorId, Editor) then
    Exit(False);

  SavedUsers := TStringList.Create;
  IniFile := TIniFile.Create(ConfigurationFileName);
  try
    StringToStringList(IniFile.ReadString(Editor.ConfigurationSection,
      CONFIGURATION_INSTALLED_USERS, EmptyStr), CONFIGURATION_USERS_SEPARATOR,
      SavedUsers);

    if GetVisualStudioCodeAvailableUsers(AvailableUsers) then
      for i := Low(AvailableUsers) to High(AvailableUsers) do
        if IsUserProfileReadable(AvailableUsers[i]) then
        begin
          if IsVisualStudioCodeExtensionInstalled(EditorId, AvailableUsers[i],
            ExtensionId) then
            InstalledUsers.Add(AvailableUsers[i].FriendlyName);
        end
        else
        begin
          // This profile can't be read: use the saved state
          Result := False;
          if SavedUsers.IndexOf(AvailableUsers[i].FriendlyName) <> -1 then
            InstalledUsers.Add(AvailableUsers[i].FriendlyName);
        end;

    // All the profiles were read: save the state for the next time
    if Result then
      IniFile.WriteString(Editor.ConfigurationSection,
        CONFIGURATION_INSTALLED_USERS,
        StringListToString(InstalledUsers, CONFIGURATION_USERS_SEPARATOR));
  finally
    IniFile.Free;
    SavedUsers.Free;
  end;
end;

end.
