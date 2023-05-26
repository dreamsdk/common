unit WtTools;

interface

type
  TWindowsTerminalSettingsOperation = (wtsoInstall, wtsoUninstall);

function UpdateWindowsTerminalSettingsFile(
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

const
  WINDOWS_TERMINAL_SETTINGS_FILE = '%LocalAppData%\Packages\Microsoft.WindowsTerminal_8wekyb3d8bbwe\LocalState\settings.json';

var
  WindowsTerminalSettingsFileName: TFileName;

function UpdateWindowsTerminalSettingsFile(
  const Operation: TWindowsTerminalSettingsOperation): Boolean;
var
  Settings: TDreamcastSoftwareDevelopmentSettings;
  WindowsTerminalSettingsData,
  Profile: TJSONObject;
  Profiles: TJSONArray;
  ProfilesEnum: TJSONEnum;
  ProfileFound: Boolean;
  MemoryStream: TMemoryStream;

begin
  Result := False;
  if FileExists(WindowsTerminalSettingsFileName) then
  begin
    Settings := TDreamcastSoftwareDevelopmentSettings.Create;
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

      // Destroy the DreamSDK profile if needed
      // This is done in all cases, if uninstalling AND installing.
      // If installing, we will recreate it after deleting the old one.
      if ProfileFound then
        Profiles.Remove(Profile);

      // Recreate the profile if needed (if we are in Install mode).
      if Operation = wtsoInstall then
      begin
        Profile := TJSONObject.Create;
        Profile.Add('commandline', GetMSysBaseDirectory + 'bin\sh.exe --login -i');
        Profile.Add('guid', Settings.WindowsTerminalProfileGuid);
        Profile.Add('hidden', False);
        Profile.Add('icon', GetMSysBaseDirectory + 'msys.ico');
        Profile.Add('name', 'DreamSDK');
        Profiles.Add(Profile);
      end;

      WindowsTerminalSettingsData.DumpJSON(MemoryStream);
      MemoryStream.SaveToFile(WindowsTerminalSettingsFileName);
      Result := True;
    finally
      WindowsTerminalSettingsData.Free;
      Settings.Free;
      MemoryStream.Free;
    end;
  end;
end;

initialization
  WindowsTerminalSettingsFileName := ExpandEnvironmentStrings(
    WINDOWS_TERMINAL_SETTINGS_FILE
  );

end.

