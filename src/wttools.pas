// Windows Terminal Integration Tools for DreamSDK
unit WtTools;

(* Change this to simulate the installation of Windows Terminal
   without installing it *)
// {$DEFINE SIMULATE_WINDOWS_TERMINAL}

(* Change this to simulate the installation of the DreamSDK profile for
   Windows Terminal *)
// {$DEFINE SIMULATE_WINDOWS_TERMINAL_PROFILE_INSTALLED}

interface

// Check if Windows Terminal is installed on this computer
function IsWindowsTerminalInstalled: Boolean;

// Indicates if Windows Terminal DreamSDK integration is installed
// This will check only for the current user
function IsWindowsTerminalIntegrationInstalled: Boolean;

// Install DreamSDK integration for all Windows Terminal
function InstallWindowsTerminalIntegration: Boolean;

// Uninstall DreamSDK integration for all Windows Terminal
function UninstallWindowsTerminalIntegration: Boolean;

implementation

uses
  SysUtils,
  Classes,
  FPJson,
  JsonParser,
  SysTools,
  FSTools,
  Settings,
  RefBase;

type
  TWindowsTerminalSettingsOperation = (
    wtsoUndefined,
    wtsoStatus,
    wtsoInstall,
    wtsoUninstall
  );

var
  Cache_IsWindowsTerminalInstalled: Boolean =
{$IFDEF DEBUG}
  {$IFDEF SIMULATE_WINDOWS_TERMINAL}
    True
  {$ELSE}
    False
  {$ENDIF}
{$ELSE}
  False
{$ENDIF};

function GetWindowsTerminalResourceDirectory: TFileName;
const
  RESOURCE_DIR_PATH = '/opt/dreamsdk/helpers';

begin
  Result := IncludeTrailingPathDelimiter(
    DreamSdkPathToSystem(RESOURCE_DIR_PATH)
  );
end;

function IsWindowsTerminalInstalled: Boolean;
const
  WINDOWS_TERMINAL_CODE_IN_PATH = 'Microsoft\WindowsApps\wt.exe';

var
  PathFileNames: TStringList;
  i: Integer;
  WindowsTerminalFileName: TFileName;

begin
{$IFDEF DEBUG}
  DebugLog('IsWindowsTerminalInstalled');
{$ENDIF}
  if not Cache_IsWindowsTerminalInstalled then
  begin
    PathFileNames := TStringList.Create;
    try
      if GetAppDataListFromUsers(PathFileNames, adkLocal) then
        for i := 0 to PathFileNames.Count - 1 do
        begin
          WindowsTerminalFileName := PathFileNames[i] + WINDOWS_TERMINAL_CODE_IN_PATH;
{$IFDEF DEBUG}
          DebugLog('  Processing: "' + WindowsTerminalFileName + '"');
{$ENDIF}
          Cache_IsWindowsTerminalInstalled := FileExists(WindowsTerminalFileName);
          if Cache_IsWindowsTerminalInstalled then
            Break; // Don't continue, it isn't needed. Not so optimized but...
        end;
    finally
      PathFileNames.Free;
    end;
  end;
  Result := Cache_IsWindowsTerminalInstalled;
{$IFDEF DEBUG}
  DebugLog('  IsWindowsTerminalInstalled RESULT: '
    + DebugBoolToStr(Result));
{$ENDIF}
end;

function UpdateWindowsTerminalSettingsFiles(
  const Operation: TWindowsTerminalSettingsOperation): Boolean;
const
  WINDOWS_TERMINAL_SETTINGS_FILEPATH = 'Packages\Microsoft.WindowsTerminal_8wekyb3d8bbwe\LocalState\settings.json';

var
  Settings: TDreamcastSoftwareDevelopmentSettings;
  WindowsTerminalSettingsData,
  Profile: TJSONObject;
  Profiles: TJSONArray;
  ProfilesEnum: TJSONEnum;
  ProfileFound: Boolean;
  MemoryStream: TMemoryStream;
  LocalAppDataDirectories: TStringList;
  WindowsTerminalSettingsFileName: TFileName;
  i: Integer;

begin
  Result := False;
  if Operation = wtsoUndefined then
    Exit;

  if IsWindowsTerminalInstalled then
  begin
    Settings := TDreamcastSoftwareDevelopmentSettings.Create;
    LocalAppDataDirectories := TStringList.Create;
    try
      if GetAppDataListFromUsers(LocalAppDataDirectories, adkLocal) then
        for i := 0 to LocalAppDataDirectories.Count - 1 do
        begin
          WindowsTerminalSettingsFileName := IncludeTrailingPathDelimiter(LocalAppDataDirectories[i])
            + WINDOWS_TERMINAL_SETTINGS_FILEPATH;
          if FileExists(WindowsTerminalSettingsFileName) then
          begin
{$IFDEF DEBUG}
            DebugLog('Processing: "' +  WindowsTerminalSettingsFileName + '"');
{$ENDIF}
            WindowsTerminalSettingsData := TJSONObject(GetJSON(LoadFileToString(WindowsTerminalSettingsFileName)));
            MemoryStream := TMemoryStream.Create;
            try
              Settings.LoadConfiguration;
              Profiles := TJSONArray(WindowsTerminalSettingsData.FindPath('profiles.list'));

              // Search for DreamSDK profile in "settings.json"
              Profile := nil;
              ProfileFound := False;
              for ProfilesEnum in Profiles do
              begin
                Profile := TJSONObject(ProfilesEnum.Value);
                if (Profile.FindPath('guid').AsString = Settings.WindowsTerminalProfileGuid) then
                begin
                  ProfileFound := True;
                  Break;
                end;
              end;

              // Status only
              if (Operation = wtsoStatus) then
              begin
                Result := ProfileFound;
                Exit;
              end;

              // Update operation: Install/Uninstall
              if (Operation = wtsoInstall) or (Operation = wtsoUninstall) then
              begin
                // Destroy the DreamSDK profile if needed
                // This is done in all cases, if uninstalling AND installing.
                // If installing, we will recreate it after deleting the old one.
                // This is because the "hidden" flag is written as string when
                // modifiying it... Looks like a bug in FPJson but we don't care.
                if ProfileFound then
                  Profiles.Remove(Profile);

                // Recreate the profile if needed (if we are in Install mode).
                if Operation = wtsoInstall then
                begin
                  Profile := TJSONObject.Create;
                  Profile.Add('commandline', GetUserBinariesBaseDirectory + 'sh.exe --login -i');
                  Profile.Add('guid', Settings.WindowsTerminalProfileGuid);
                  Profile.Add('hidden', False);
                  Profile.Add('icon', GetWindowsTerminalResourceDirectory + 'wt.ico');
                  Profile.Add('name', 'DreamSDK');
                  Profiles.Add(Profile);
                end;

                // Save the new JSON file.
                WindowsTerminalSettingsData.DumpJSON(MemoryStream);
                MemoryStream.SaveToFile(WindowsTerminalSettingsFileName);
                Result := FileExists(WindowsTerminalSettingsFileName);
              end; // wtsoInstall+wtsoUninstall
            finally
              WindowsTerminalSettingsData.Free;
              MemoryStream.Free;
            end;
          end; // Check for WindowsTerminalSettingsFileName
        end; // for
    finally
      LocalAppDataDirectories.Free;
      Settings.Free;
    end;
  end;
end;

function IsWindowsTerminalIntegrationInstalled: Boolean;
begin
  Result := UpdateWindowsTerminalSettingsFiles(wtsoStatus);
{$IFDEF DEBUG}
  {$IFDEF SIMULATE_WINDOWS_TERMINAL_PROFILE_INSTALLED}
  Result := True;
  {$ENDIF}
  DebugLog('IsWindowsTerminalIntegrationInstalled: ' + DebugBoolToStr(Result));
{$ENDIF}
end;

function InstallWindowsTerminalIntegration: Boolean;
begin
  Result := UpdateWindowsTerminalSettingsFiles(wtsoInstall);
{$IFDEF DEBUG}
  DebugLog('InstallWindowsTerminalIntegration: ' + DebugBoolToStr(Result));
{$ENDIF}
end;

function UninstallWindowsTerminalIntegration: Boolean;
begin
  Result := UpdateWindowsTerminalSettingsFiles(wtsoUninstall);
{$IFDEF DEBUG}
  DebugLog('UninstallWindowsTerminalIntegration: ' + DebugBoolToStr(Result));
{$ENDIF}
end;

end.

