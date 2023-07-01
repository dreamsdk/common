unit WtTools;

interface

type
  TWindowsTerminalSettingsOperation = (
    wtsoUndefined,
    wtsoStatus, // TODO
    wtsoInstall,
    wtsoUninstall
  );

function IsWindowsTerminalInstalled: Boolean;
function UpdateWindowsTerminalSettingsFiles(
  const Operation: TWindowsTerminalSettingsOperation): Boolean;

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

var
  Cache_IsWindowsTerminalInstalled: Boolean = False;

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
            Break; // Don't continue, it isn't needed
        end;
    finally
      PathFileNames.Free;
    end;
  end;
  Result := Cache_IsWindowsTerminalInstalled;
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
                  Profile.Add('commandline', GetMSysBaseDirectory + 'bin\sh.exe --login -i');
                  Profile.Add('guid', Settings.WindowsTerminalProfileGuid);
                  Profile.Add('hidden', False);
                  Profile.Add('icon', GetMSysBaseDirectory + 'dreamsdk-wt.ico');
                  Profile.Add('name', 'DreamSDK');
                  Profiles.Add(Profile);
                end;

                // Save the new JSON file.
                WindowsTerminalSettingsData.DumpJSON(MemoryStream);
                MemoryStream.SaveToFile(WindowsTerminalSettingsFileName);
              end;
              Result := True;
            finally
              WindowsTerminalSettingsData.Free;
              MemoryStream.Free;
            end;
          end;
        end;
    finally
      LocalAppDataDirectories.Free;
      Settings.Free;
    end;
  end;
end;

end.

