unit Settings;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  IniFiles,
  FSTools;

const
  SETTINGS_FILE_NAME = 'dreamsdk.conf';

  // Default repositories URLs (can be overriden in DreamSDK Manager)
  DEFAULT_KALLISTI_URL = 'https://git.code.sf.net/p/cadcdev/kallistios';
  DEFAULT_KALLISTI_PORTS_URL = 'https://git.code.sf.net/p/cadcdev/kos-ports';
  DEFAULT_DREAMCAST_TOOL_SERIAL_URL = 'https://gitlab.com/kallistios/dcload-serial.git';
  DEFAULT_DREAMCAST_TOOL_INTERNET_PROTOCOL_URL = 'https://gitlab.com/kallistios/dcload-ip.git';
  DEFAULT_RUBY_URL = 'https://github.com/mruby/mruby.git';

  // Dreamcast Tool
  DREAMCAST_TOOL_DEFAULT_KIND = 0;
  DREAMCAST_TOOL_DEFAULT_ATTACH_CONSOLE_FILESERVER = True;
  DREAMCAST_TOOL_DEFAULT_CLEAR_SCREEN_BEFORE_DOWNLOAD = True;
  DREAMCAST_TOOL_DEFAULT_INTERNET_PROTOCOL_ADDRESS = '000.000.000.000';
  DREAMCAST_TOOL_DEFAULT_MEDIA_ACCESS_CONTROL_ENABLED = False;
  DREAMCAST_TOOL_DEFAULT_MEDIA_ACCESS_CONTROL_ADDRESS = '00-00-00-00-00-00';
  DREAMCAST_TOOL_DEFAULT_SERIAL_DUMB_TERMINAL = False;
  DREAMCAST_TOOL_DEFAULT_SERIAL_EXTERNAL_CLOCK = False;
  DREAMCAST_TOOL_DEFAULT_SERIAL_BAUDRATE = 7;
  DREAMCAST_TOOL_DEFAULT_SERIAL_BAUDRATE_ALTERNATE = False;
  DREAMCAST_TOOL_DEFAULT_SERIAL_PORT = 0;
  DREAMCAST_TOOL_DEFAULT_CUSTOM_EXECUTABLE = '';
  DREAMCAST_TOOL_DEFAULT_CUSTOM_ARGUMENTS = '';

type  
  TDreamcastToolKind = (
    dtkUndefined,
    dtkSerial,
    dtkInternetProtocol,
    dtkCustom
  );

  TDreamcastToolSerialPort = (
    dtpCOM1,
    dtpCOM2,
    dtpCOM3,
    dtpCOM4,
    dtpCOM5,
    dtpCOM6,
    dtpCOM7,
    dtpCOM8,
    dtpCOM9
  );

  TDreamcastToolSerialBaudrate = (
    dtb300,
    dtb1200,
    dtb2400,
    dtb4800,
    dtb9600,
    dtb19200,
    dtb38400,
    dtb57600,
    dtb115200,
    dtb500000,
    dtb1500000
  );

  TDreamcastSoftwareDevelopmentSettings = class;

  { TDreamcastSoftwareDevelopmentSettingsDreamcastTool }
  TDreamcastSoftwareDevelopmentSettingsDreamcastTool = class(TObject)
  private
    fAttachConsoleFileserver: Boolean;
    fClearScreenBeforeDownload: Boolean;
    fCustomArguments: string;
    fCustomExecutable: TFileName;
    fDreamcastToolKind: TDreamcastToolKind;
    fInternetProtocolAddress: string;
    fMediaAccessControlAddress: string;
    fMediaAccessControlEnabled: Boolean;
    fHostMediaAccessControlAddress: string;
    fSerialDumbTerminal: Boolean;
    fSerialExternalClock: Boolean;
    fSerialBaudrate: TDreamcastToolSerialBaudrate;
    fSerialBaudrateAlternate: Boolean;
    fSerialPort: TDreamcastToolSerialPort;
  protected
    procedure LoadConfiguration(IniFile: TIniFile);
    procedure SaveConfiguration(IniFile: TIniFile);
  public
    property Kind: TDreamcastToolKind
      read fDreamcastToolKind write fDreamcastToolKind;
    property AttachConsoleFileserver: Boolean
      read fAttachConsoleFileserver write fAttachConsoleFileserver;
    property ClearScreenBeforeDownload: Boolean
      read fClearScreenBeforeDownload write fClearScreenBeforeDownload;
    property SerialPort: TDreamcastToolSerialPort
      read fSerialPort write fSerialPort;
    property SerialBaudrate: TDreamcastToolSerialBaudrate
      read fSerialBaudrate write fSerialBaudrate;
    property SerialBaudrateAlternate: Boolean
      read fSerialBaudrateAlternate write fSerialBaudrateAlternate;
    property SerialExternalClock: Boolean
      read fSerialExternalClock write fSerialExternalClock;
    property SerialDumbTerminal: Boolean
      read fSerialDumbTerminal write fSerialDumbTerminal;
    property InternetProtocolAddress: string
      read fInternetProtocolAddress write fInternetProtocolAddress;
    property MediaAccessControlEnabled: Boolean
      read fMediaAccessControlEnabled write fMediaAccessControlEnabled;
    property MediaAccessControlAddress: string
      read fMediaAccessControlAddress write fMediaAccessControlAddress;
    property HostMediaAccessControlAddress: string
      read fHostMediaAccessControlAddress write fHostMediaAccessControlAddress;
    property CustomExecutable: TFileName
      read fCustomExecutable write fCustomExecutable;
    property CustomArguments: string
      read fCustomArguments write fCustomArguments;
  end;

  { TDreamcastSoftwareDevelopmentSettingsRepositories }
  TDreamcastSoftwareDevelopmentSettingsRepositories = class(TObject)
  private
    fKallistiPortsURL: string;
    fKallistiURL: string;
    fDreamcastToolSerialURL: string;
    fDreamcastToolInternetProtocolURL: string;
    fRubyURL: string;
    function GetDreamcastToolInternetProtocolURL: string;
    function GetDreamcastToolSerialURL: string;
    function GetKallistiPortsURL: string;
    function GetKallistiURL: string;
    function GetRubyURL: string;
  protected
    function GetURL(const FieldValue, DefaultFieldValue: string): string;
  public
    property KallistiURL: string
      read GetKallistiURL write fKallistiURL;
    property KallistiPortsURL: string
      read GetKallistiPortsURL write fKallistiPortsURL;
    property DreamcastToolSerialURL: string
      read GetDreamcastToolSerialURL write fDreamcastToolSerialURL;
    property DreamcastToolInternetProtocolURL: string
      read GetDreamcastToolInternetProtocolURL write fDreamcastToolInternetProtocolURL;
    property RubyURL: string read GetRubyURL write fRubyURL;
  end;

  { TDreamcastSoftwareDevelopmentSettingsCodeBlocks }
  TDreamcastSoftwareDevelopmentSettingsCodeBlocks = class(TObject)
  private
    fAvailableUsers: TStringList;
    fExportLibraryInformation: Boolean;
    fExportLibraryInformationPath: TFileName;
    fInstalledUsers: TStringList;
    fBackupDirectory: TFileName;
    fConfigurationFileNames: TFileList;
    fAvailableConfigurationFileNames: TFileList;
    fInstallationDirectory: TFileName;
    function GetHomeDirectory: TFileName;
    function GetInstalled: Boolean;
    function GetRegistryFileName: TFileName;
    procedure SetBackupDirectory(AValue: TFileName);
    procedure SetExportLibraryInformation(AValue: Boolean);
    procedure SetHomeDirectory(AValue: TFileName);
    procedure SetInstallationDirectory(AValue: TFileName);
  protected
    fInstalled: Boolean;
    fHomeDirectory: TFileName;
    procedure HandleDynamicDirectories;
  public
    constructor Create;
    destructor Destroy; override;

    function LoadConfiguration: Boolean;
    procedure SaveConfiguration;

    // Code::Blocks Available Users on this system (i.e. users that can use C::B)
    property AvailableUsers: TStringList
      read fAvailableUsers;

    // Code::Blocks Configuration Files (e.g. 'default.conf' files)
    property ConfigurationFileNames: TFileList
      read fConfigurationFileNames;

    // Code::Blocks Installed Users (if any)
    property InstalledUsers: TStringList
      read fInstalledUsers;

    // Code::Blocks Installation Directory
    property InstallationDirectory: TFileName
      read fInstallationDirectory
      write SetInstallationDirectory;

    // Backup Directory for Code::Blocks files (in DreamSDK tree)
    property BackupDirectory: TFileName
      read fBackupDirectory
      write SetBackupDirectory;

    // DreamSDK Configuration File
    property RegistryFileName: TFileName
      read GetRegistryFileName;

    // Used for C::B plugin
    property ExportLibraryInformation: Boolean
      read fExportLibraryInformation
      write SetExportLibraryInformation;

    // Used for C::B plugin
    property ExportLibraryInformationPath: TFileName
      read fExportLibraryInformationPath;

    // DreamSDK Home
    property HomeDirectory: TFileName
      read GetHomeDirectory
      write SetHomeDirectory;

    (* This is equals to True if the patch for C::B has been installed.
       This property is overriden to False if the installation is now invalid.
       This could happens for example if the patch has been installed and C::B
       has been uninstalled after the patch installation. *)
    property Installed: Boolean
      read GetInstalled;

    (* This is equals to True if ther patch for C::B has been installed in the past.
       This property is not overriden. *)
    property MarkedAsInstalled: Boolean
      read fInstalled;
  end;

  { TDreamcastSoftwareDevelopmentSettingsCodeBlocksPatcher }
  TDreamcastSoftwareDevelopmentSettingsCodeBlocksPatcher
    = class(TDreamcastSoftwareDevelopmentSettingsCodeBlocks)
  private
    procedure InitializeCodeBlocksInstallationDirectory;
    procedure InitializeDefaults(const AutoLoad: Boolean);
    procedure WriteRegistry;
  public
    constructor Create; overload;
    constructor Create(const AutoLoad: Boolean); overload;
    procedure Assign(
      ASource: TDreamcastSoftwareDevelopmentSettingsCodeBlocksPatcher);
    procedure Save(const AInstalled: Boolean);
    function Refresh: Boolean;
  end;

  { TDreamcastSoftwareDevelopmentSettingsRuby }
  TDreamcastSoftwareDevelopmentSettingsRuby = class(TObject)
  private
    fOwner: TDreamcastSoftwareDevelopmentSettings;
    fEnabled: Boolean;
    procedure SetEnabled(AValue: Boolean);
  protected
    procedure LoadConfiguration(IniFile: TIniFile);
    procedure SaveConfiguration(IniFile: TIniFile);
  public
    constructor Create(AOwner: TDreamcastSoftwareDevelopmentSettings);
    property Enabled: Boolean read fEnabled write SetEnabled;
  end;

  { TDreamcastSoftwareDevelopmentSettings }
  TDreamcastSoftwareDevelopmentSettings = class(TObject)
  private
    fConfigurationFileName: TFileName;
    fInstallPath: TFileName;
    fProgressWindowAutoClose: Boolean;
    fRepositories: TDreamcastSoftwareDevelopmentSettingsRepositories;
    fRuby: TDreamcastSoftwareDevelopmentSettingsRuby;
    fUseMinTTY: Boolean;
    fDreamcastTool: TDreamcastSoftwareDevelopmentSettingsDreamcastTool;
  protected
    function GetConfigurationFileName: TFileName;
  public
    constructor Create;
    destructor Destroy; override;

    function LoadConfiguration: Boolean;
    procedure SaveConfiguration;

    property DreamcastTool: TDreamcastSoftwareDevelopmentSettingsDreamcastTool read
      fDreamcastTool;
    property FileName: TFileName read GetConfigurationFileName;
    property InstallPath: TFileName read fInstallPath;
    property UseMinTTY: Boolean read fUseMinTTY write fUseMinTTY;
    property ProgressWindowAutoClose: Boolean read fProgressWindowAutoClose
      write fProgressWindowAutoClose;
    property Repositories: TDreamcastSoftwareDevelopmentSettingsRepositories
      read fRepositories;
    property Ruby: TDreamcastSoftwareDevelopmentSettingsRuby read fRuby;
  end;

function GetDefaultCodeBlocksBackupDirectory: TFileName;
function SerialPortToString(SerialPort: TDreamcastToolSerialPort): string;
function SerialBaudrateToString(SerialBaudrate: TDreamcastToolSerialBaudrate): string;

implementation

uses
  RefBase,
  SysTools,
  Version,
  CBTools;

const
  // Code::Blocks Backup Directory (for previous install)
  DEFAULT_CODEBLOCKS_BACKUP_DIR = '%s\support\ide\codeblocks\';

  // Export Library Path for DreamSDK Wizard for Code::Blocks
  EXPORT_LIBRARY_INFORMATION_DIR = '%s\msys\1.0\etc\dreamsdk\ide\codeblocks\';

  // dreamsdk.conf: sections
  CONFIG_DREAMSDK_SECTION_SETTINGS = 'Settings';
  CONFIG_DREAMSDK_SECTION_DREAMCAST_TOOL = 'DreamcastTool';

  // dreamsdk.conf: section Ruby
  CONFIG_DREAMSDK_SECTION_RUBY = 'Ruby';
  CONFIG_DREAMSDK_SECTION_RUBY_KEY_ENABLED = 'Enabled';

  // ide.conf: section Global
  CONFIG_IDE_SECTION_GLOBAL = 'Global';
  CONFIG_IDE_SECTION_GLOBAL_KEY_CODEBLOCKS = 'CodeBlocks';

  // ide.conf: section Code::Blocks
  CONFIG_IDE_SECTION_CODEBLOCKS = 'CodeBlocks';
  CONFIG_IDE_SECTION_CODEBLOCKS_KEY_EXPORT_LIBRARY_INFORMATION_ENABLED = 'ExportLibraryInformation';
  CONFIG_IDE_SECTION_CODEBLOCKS_KEY_EXPORT_LIBRARY_INFORMATION_PATH = 'ExportLibraryInformationPath';
  CONFIG_IDE_SECTION_CODEBLOCKS_KEY_CONFIGURATION_FILENAMES = 'ConfigurationFileNames';
  CONFIG_IDE_SECTION_CODEBLOCKS_KEY_INSTALLATION_PATH = 'InstallationPath';
  CONFIG_IDE_SECTION_CODEBLOCKS_KEY_BACKUP_PATH = 'BackupPath';
  CONFIG_IDE_SECTION_CODEBLOCKS_KEY_USERS_INSTALLED = 'InstalledUsers';
  CONFIG_IDE_SECTION_CODEBLOCKS_KEY_USERS_AVAILABLE = 'AvailableUsers';

function SerialPortToString(SerialPort: TDreamcastToolSerialPort): string;
var
  SerialPortNumber: Integer;

begin
  SerialPortNumber := Integer(SerialPort) + 1;
  Result := 'COM' + IntToStr(SerialPortNumber);
end;

function SerialBaudrateToString(SerialBaudrate: TDreamcastToolSerialBaudrate): string;
begin
  Result := '';
  case SerialBaudrate of
    dtb300: Result := '300';
    dtb1200: Result := '1200';
    dtb2400: Result := '2400';
    dtb4800: Result := '4800';
    dtb9600: Result := '9600';
    dtb19200: Result := '19200';
    dtb38400: Result := '38400';
    dtb57600: Result := '57600';
    dtb115200: Result := '115200';
    dtb500000: Result := '500000';
    dtb1500000: Result := '1500000';
  end;
end;

function GetDefaultCodeBlocksBackupDirectory: TFileName;
begin
  Result := Format(DEFAULT_CODEBLOCKS_BACKUP_DIR, [
      '%' + GetBaseEnvironmentVariableName + '%']);
end;

{ TDreamcastSoftwareDevelopmentSettingsRuby }

procedure TDreamcastSoftwareDevelopmentSettingsRuby.SetEnabled(AValue: Boolean);
begin
  if fEnabled <> AValue then
  begin
    fEnabled := AValue;
    fOwner.SaveConfiguration;
  end;
end;

procedure TDreamcastSoftwareDevelopmentSettingsRuby.LoadConfiguration(
  IniFile: TIniFile);
begin
  fEnabled := IniFile.ReadBool(CONFIG_DREAMSDK_SECTION_RUBY,
    CONFIG_DREAMSDK_SECTION_RUBY_KEY_ENABLED, fEnabled);
end;

procedure TDreamcastSoftwareDevelopmentSettingsRuby.SaveConfiguration(
  IniFile: TIniFile);
begin
  IniFile.WriteBool(CONFIG_DREAMSDK_SECTION_RUBY,
    CONFIG_DREAMSDK_SECTION_RUBY_KEY_ENABLED, fEnabled);
end;

constructor TDreamcastSoftwareDevelopmentSettingsRuby.Create(
  AOwner: TDreamcastSoftwareDevelopmentSettings);
begin
  fOwner := AOwner;
end;

{ TDreamcastSoftwareDevelopmentSettingsRepositories }

function TDreamcastSoftwareDevelopmentSettingsRepositories.GetDreamcastToolInternetProtocolURL: string;
begin
  Result := GetURL(fDreamcastToolInternetProtocolURL, DEFAULT_DREAMCAST_TOOL_INTERNET_PROTOCOL_URL);
end;

function TDreamcastSoftwareDevelopmentSettingsRepositories.GetDreamcastToolSerialURL: string;
begin
  Result := GetURL(fDreamcastToolSerialURL, DEFAULT_DREAMCAST_TOOL_SERIAL_URL);
end;

function TDreamcastSoftwareDevelopmentSettingsRepositories.GetKallistiPortsURL: string;
begin
  Result := GetURL(fKallistiPortsURL, DEFAULT_KALLISTI_PORTS_URL);
end;

function TDreamcastSoftwareDevelopmentSettingsRepositories.GetKallistiURL: string;
begin
  Result := GetURL(fKallistiURL, DEFAULT_KALLISTI_URL);
end;

function TDreamcastSoftwareDevelopmentSettingsRepositories.GetRubyURL: string;
begin
  Result := GetURL(fRubyURL, DEFAULT_RUBY_URL);
end;

function TDreamcastSoftwareDevelopmentSettingsRepositories.GetURL(
  const FieldValue, DefaultFieldValue: string): string;
begin
  Result := FieldValue;
  if IsEmpty(FieldValue) then
    Result := DefaultFieldValue;
end;

{ TDreamcastSoftwareDevelopmentSettingsCodeBlocksPatcher }

procedure TDreamcastSoftwareDevelopmentSettingsCodeBlocksPatcher
  .InitializeCodeBlocksInstallationDirectory;
begin
  InstallationDirectory := GetCodeBlocksDefaultInstallationDirectory;
end;

// Define this to debug this procedure
// {$DEFINE DEBUG_INITIALIZE_DEFAULTS}
procedure TDreamcastSoftwareDevelopmentSettingsCodeBlocksPatcher
  .InitializeDefaults(const AutoLoad: Boolean);
begin
  // Code::Blocks Installation Directory
  InitializeCodeBlocksInstallationDirectory;

  // Code::Blocks Configuration Files
  GetCodeBlocksAvailableConfigurationFileNames(ConfigurationFileNames);
  fAvailableConfigurationFileNames.Assign(ConfigurationFileNames);

  // Code::Blocks Backup and Export Directories
  HandleDynamicDirectories;

  if AutoLoad then
    LoadConfiguration;
end;

constructor TDreamcastSoftwareDevelopmentSettingsCodeBlocksPatcher.Create;
begin
  inherited Create;
  InitializeDefaults(True);
end;

constructor TDreamcastSoftwareDevelopmentSettingsCodeBlocksPatcher.Create(
  const AutoLoad: Boolean);
begin
  inherited Create;
  InitializeDefaults(AutoLoad);
end;

procedure TDreamcastSoftwareDevelopmentSettingsCodeBlocksPatcher.Assign(
  ASource: TDreamcastSoftwareDevelopmentSettingsCodeBlocksPatcher);
begin
  fAvailableConfigurationFileNames.Assign(ASource.fAvailableConfigurationFileNames);
  fAvailableUsers.Assign(ASource.AvailableUsers);
  fBackupDirectory := ASource.BackupDirectory;
  fConfigurationFileNames.Assign(ASource.ConfigurationFileNames);
  fExportLibraryInformation := ASource.ExportLibraryInformation;
  fExportLibraryInformationPath := ASource.ExportLibraryInformationPath;
  fHomeDirectory := ASource.HomeDirectory;
  fInstallationDirectory := ASource.InstallationDirectory;
end;

procedure TDreamcastSoftwareDevelopmentSettingsCodeBlocksPatcher.Save(
  const AInstalled: Boolean);

  procedure SetInstalledUsers;
  var
    i: Integer;
    UserName: string;

  begin
    fInstalledUsers.Clear;
    if Installed then
      for i := 0 to ConfigurationFileNames.Count - 1 do
      begin
        UserName := GetUserFromAppDataDirectory(ConfigurationFileNames[i]);
        if not IsEmpty(UserName) then
          UserName := GetFriendlyUserName(UserName)
        else
          UserName := Format('<%s>', [ExtractFileName(ConfigurationFileNames[i])]);
        fInstalledUsers.Add(UserName);
      end;
  end;

begin
  fInstalled := AInstalled;
  ExportLibraryInformation := AInstalled;

  if AInstalled then
    SetInstalledUsers
  else
  begin
    fExportLibraryInformationPath := EmptyStr;
    fBackupDirectory := EmptyStr;
    fConfigurationFileNames.Clear;
    fInstalledUsers.Clear;
    InitializeCodeBlocksInstallationDirectory;
  end;

  WriteRegistry;
end;

function TDreamcastSoftwareDevelopmentSettingsCodeBlocksPatcher.Refresh: Boolean;
begin
  LoadConfiguration;
  if not Installed then
    InitializeCodeBlocksInstallationDirectory;
  WriteRegistry;
  Result := LoadConfiguration;
end;

procedure TDreamcastSoftwareDevelopmentSettingsCodeBlocksPatcher.WriteRegistry;
begin
  ConvertCodeBlocksConfigurationFileNamesToUsers(
    fAvailableConfigurationFileNames, fAvailableUsers);
  SaveConfiguration;
end;

{ TDreamcastSoftwareDevelopmentSettingsCodeBlocks }

function TDreamcastSoftwareDevelopmentSettingsCodeBlocks
  .GetRegistryFileName: TFileName;
const
  IDE_CONFIGURATION_FILE = 'msys\1.0\etc\dreamsdk\ide.conf';

begin
  Result := IncludeTrailingPathDelimiter(HomeDirectory)
    + IDE_CONFIGURATION_FILE;
end;

function TDreamcastSoftwareDevelopmentSettingsCodeBlocks.GetHomeDirectory: TFileName;
begin
  Result := fHomeDirectory;
{$IFDEF DEBUG}
//  DebugLog('GetHomeDirectory: ' + Result);
{$ENDIF}
end;

function TDreamcastSoftwareDevelopmentSettingsCodeBlocks.GetInstalled: Boolean;
begin
  Result := fInstalled and DirectoryExists(InstallationDirectory)
    and (GetCodeBlocksVersion(InstallationDirectory) <> cbvUndefined);
end;

procedure TDreamcastSoftwareDevelopmentSettingsCodeBlocks
  .SetBackupDirectory(AValue: TFileName);
begin
  if (fBackupDirectory <> AValue) then
    fBackupDirectory := IncludeTrailingPathDelimiter(
      ParseInputFileSystemObject(AValue));
end;

procedure TDreamcastSoftwareDevelopmentSettingsCodeBlocks
  .SetExportLibraryInformation(AValue: Boolean);
begin
  if (fExportLibraryInformation <> AValue) then
    fExportLibraryInformation := AValue;

  if fExportLibraryInformation then
  begin
    if not DirectoryExists(ExportLibraryInformationPath) then
      ForceDirectories(ExportLibraryInformationPath);
    SetDirectoryRights(ExportLibraryInformationPath, GetEveryoneName,
      ACL_RIGHT_FULL);
  end;
end;

procedure TDreamcastSoftwareDevelopmentSettingsCodeBlocks
  .SetHomeDirectory(AValue: TFileName);
begin
  if fHomeDirectory <> AValue then
  begin
    fHomeDirectory := ExcludeTrailingPathDelimiter(
      ParseInputFileSystemObject(AValue));
    HandleDynamicDirectories;
  end;
end;

procedure TDreamcastSoftwareDevelopmentSettingsCodeBlocks
  .SetInstallationDirectory(AValue: TFileName);
begin
  if fInstallationDirectory <> AValue then
  begin
    fInstallationDirectory := IncludeTrailingPathDelimiter(
      ParseInputFileSystemObject(AValue));
  end;
end;

procedure TDreamcastSoftwareDevelopmentSettingsCodeBlocks.HandleDynamicDirectories;
begin
  BackupDirectory := Format(DEFAULT_CODEBLOCKS_BACKUP_DIR, [HomeDirectory]);
  fExportLibraryInformationPath := Format(EXPORT_LIBRARY_INFORMATION_DIR, [HomeDirectory]);
end;

constructor TDreamcastSoftwareDevelopmentSettingsCodeBlocks.Create;
begin
  fConfigurationFileNames := TFileList.Create;
  fAvailableConfigurationFileNames := TFileList.Create;
  fAvailableUsers := TStringList.Create;
  fInstalledUsers := TStringList.Create;
  HomeDirectory := GetInstallationBaseDirectory;
  HandleDynamicDirectories;
end;

destructor TDreamcastSoftwareDevelopmentSettingsCodeBlocks.Destroy;
begin
  fAvailableConfigurationFileNames.Free;
  fInstalledUsers.Free;
  fAvailableUsers.Free;
  fConfigurationFileNames.Free;
  inherited Destroy;
end;

function TDreamcastSoftwareDevelopmentSettingsCodeBlocks
  .LoadConfiguration: Boolean;
var
  IniFile: TIniFile; // ide.conf
  Temp: string;

begin
  Result := FileExists(RegistryFileName);

{$IFDEF DEBUG}
  DebugLog('LoadConfiguration: ' + BoolToStr(Result));
  DebugLog('  RegistryFileName: ' + RegistryFileName);
{$ENDIF}

  if Result then
  begin
    IniFile := TIniFile.Create(GetRegistryFileName);
    try
      // Global C::B flag to know if patcher was installed
      fInstalled := IniFile.ReadBool(CONFIG_IDE_SECTION_GLOBAL,
        CONFIG_IDE_SECTION_GLOBAL_KEY_CODEBLOCKS, fInstalled);

      // Export Library Information
      fExportLibraryInformation := IniFile.ReadBool(CONFIG_IDE_SECTION_CODEBLOCKS,
        CONFIG_IDE_SECTION_CODEBLOCKS_KEY_EXPORT_LIBRARY_INFORMATION_ENABLED,
          ExportLibraryInformation);

      // Export Library Information Path
      fExportLibraryInformationPath := IniFile.ReadString(
        CONFIG_IDE_SECTION_CODEBLOCKS,
        CONFIG_IDE_SECTION_CODEBLOCKS_KEY_EXPORT_LIBRARY_INFORMATION_PATH,
        ExportLibraryInformationPath);

      // Installation Directory
      InstallationDirectory :=
        IniFile.ReadString(CONFIG_IDE_SECTION_CODEBLOCKS,
          CONFIG_IDE_SECTION_CODEBLOCKS_KEY_INSTALLATION_PATH,
            InstallationDirectory);

      // Backup Directory
      BackupDirectory :=
        IniFile.ReadString(CONFIG_IDE_SECTION_CODEBLOCKS,
          CONFIG_IDE_SECTION_CODEBLOCKS_KEY_BACKUP_PATH, BackupDirectory);

      // Users Available
      Temp := IniFile.ReadString(CONFIG_IDE_SECTION_CODEBLOCKS,
        CONFIG_IDE_SECTION_CODEBLOCKS_KEY_USERS_AVAILABLE, EmptyStr);
      if not IsEmpty(Temp) then
       StringToStringList(Temp, ArraySeparator, AvailableUsers);

      // Users Installed
      Temp := IniFile.ReadString(CONFIG_IDE_SECTION_CODEBLOCKS,
        CONFIG_IDE_SECTION_CODEBLOCKS_KEY_USERS_INSTALLED, EmptyStr);
      if not IsEmpty(Temp) then
       StringToStringList(Temp, ArraySeparator, InstalledUsers);

      // Configuration File Names
      Temp := IniFile.ReadString(CONFIG_IDE_SECTION_CODEBLOCKS,
        CONFIG_IDE_SECTION_CODEBLOCKS_KEY_CONFIGURATION_FILENAMES, EmptyStr);
      if not IsEmpty(Temp) then
        ConfigurationFileNames.SetItems(Temp, ArraySeparator);
    finally
      IniFile.Free;
    end;
  end;
end;

procedure TDreamcastSoftwareDevelopmentSettingsCodeBlocks.SaveConfiguration;
var
  IniFile: TIniFile; // ide.conf

  procedure WriteString(const Section, Key, Value: string);
  begin
    if not IsEmpty(Value) then
      IniFile.WriteString(Section, Key, Value)
    else
      IniFile.DeleteKey(Section, Key);
  end;

begin
  IniFile := TIniFile.Create(RegistryFileName);
  try
    // Global C::B
    IniFile.WriteBool(CONFIG_IDE_SECTION_GLOBAL,
      CONFIG_IDE_SECTION_GLOBAL_KEY_CODEBLOCKS, Installed);

    // Settings for C::B
    IniFile.WriteBool(CONFIG_IDE_SECTION_CODEBLOCKS,
      CONFIG_IDE_SECTION_CODEBLOCKS_KEY_EXPORT_LIBRARY_INFORMATION_ENABLED,
        ExportLibraryInformation);

    WriteString(CONFIG_IDE_SECTION_CODEBLOCKS,
      CONFIG_IDE_SECTION_CODEBLOCKS_KEY_EXPORT_LIBRARY_INFORMATION_PATH,
        ExportLibraryInformationPath);

    WriteString(CONFIG_IDE_SECTION_CODEBLOCKS,
      CONFIG_IDE_SECTION_CODEBLOCKS_KEY_INSTALLATION_PATH,
        InstallationDirectory);

    WriteString(CONFIG_IDE_SECTION_CODEBLOCKS,
      CONFIG_IDE_SECTION_CODEBLOCKS_KEY_BACKUP_PATH,
        BackupDirectory);

    WriteString(CONFIG_IDE_SECTION_CODEBLOCKS,
      CONFIG_IDE_SECTION_CODEBLOCKS_KEY_CONFIGURATION_FILENAMES,
        ConfigurationFileNames.GetItems(ArraySeparator));

    WriteString(CONFIG_IDE_SECTION_CODEBLOCKS,
      CONFIG_IDE_SECTION_CODEBLOCKS_KEY_USERS_AVAILABLE,
        StringListToString(AvailableUsers, ArraySeparator));

    WriteString(CONFIG_IDE_SECTION_CODEBLOCKS,
      CONFIG_IDE_SECTION_CODEBLOCKS_KEY_USERS_INSTALLED,
        StringListToString(InstalledUsers, ArraySeparator));
  finally
    IniFile.Free;
  end;
end;

{ TDreamcastSoftwareDevelopmentSettingsDreamcastTool }

procedure TDreamcastSoftwareDevelopmentSettingsDreamcastTool.LoadConfiguration(
  IniFile: TIniFile);
begin
  fDreamcastToolKind := TDreamcastToolKind(IniFile.ReadInteger(
    CONFIG_DREAMSDK_SECTION_DREAMCAST_TOOL,
    'Kind',
    DREAMCAST_TOOL_DEFAULT_KIND
  ));
  fAttachConsoleFileserver := IniFile.ReadBool(
    CONFIG_DREAMSDK_SECTION_DREAMCAST_TOOL,
    'AttachConsoleFileserver',
    DREAMCAST_TOOL_DEFAULT_ATTACH_CONSOLE_FILESERVER
  );
  fClearScreenBeforeDownload := IniFile.ReadBool(
    CONFIG_DREAMSDK_SECTION_DREAMCAST_TOOL,
    'ClearScreenBeforeDownload',
    DREAMCAST_TOOL_DEFAULT_CLEAR_SCREEN_BEFORE_DOWNLOAD
  );
  fInternetProtocolAddress := IniFile.ReadString(
    CONFIG_DREAMSDK_SECTION_DREAMCAST_TOOL,
    'InternetProtocolAddress',
    DREAMCAST_TOOL_DEFAULT_INTERNET_PROTOCOL_ADDRESS
  );
  fMediaAccessControlEnabled := IniFile.ReadBool(
    CONFIG_DREAMSDK_SECTION_DREAMCAST_TOOL,
    'MediaAccessControlEnabled',
    DREAMCAST_TOOL_DEFAULT_MEDIA_ACCESS_CONTROL_ENABLED
  );
  fMediaAccessControlAddress := IniFile.ReadString(
    CONFIG_DREAMSDK_SECTION_DREAMCAST_TOOL,
    'MediaAccessControlAddress',
    DREAMCAST_TOOL_DEFAULT_MEDIA_ACCESS_CONTROL_ADDRESS
  );
  fHostMediaAccessControlAddress := IniFile.ReadString(
    CONFIG_DREAMSDK_SECTION_DREAMCAST_TOOL,
    'HostMediaAccessControlAddress',
    DREAMCAST_TOOL_DEFAULT_MEDIA_ACCESS_CONTROL_ADDRESS
  );
  fSerialDumbTerminal := IniFile.ReadBool(
    CONFIG_DREAMSDK_SECTION_DREAMCAST_TOOL,
    'SerialDumbTerminal',
    DREAMCAST_TOOL_DEFAULT_SERIAL_DUMB_TERMINAL
  );
  fSerialExternalClock := IniFile.ReadBool(
    CONFIG_DREAMSDK_SECTION_DREAMCAST_TOOL,
    'SerialExternalClock',
    DREAMCAST_TOOL_DEFAULT_SERIAL_EXTERNAL_CLOCK
  );
  fSerialBaudrate := TDreamcastToolSerialBaudrate(IniFile.ReadInteger(
    CONFIG_DREAMSDK_SECTION_DREAMCAST_TOOL,
    'SerialBaudrate',
    DREAMCAST_TOOL_DEFAULT_SERIAL_BAUDRATE
  ));
  fSerialBaudrateAlternate := IniFile.ReadBool(
    CONFIG_DREAMSDK_SECTION_DREAMCAST_TOOL,
    'SerialBaudrateAlternate',
    DREAMCAST_TOOL_DEFAULT_SERIAL_BAUDRATE_ALTERNATE
  );
  fSerialPort := TDreamcastToolSerialPort(IniFile.ReadInteger(
    CONFIG_DREAMSDK_SECTION_DREAMCAST_TOOL,
    'SerialPort',
    DREAMCAST_TOOL_DEFAULT_SERIAL_PORT
  ));
  fCustomExecutable := IniFile.ReadString(
    CONFIG_DREAMSDK_SECTION_DREAMCAST_TOOL,
    'CustomExecutable',
    DREAMCAST_TOOL_DEFAULT_CUSTOM_EXECUTABLE
  );
  fCustomArguments := IniFile.ReadString(
    CONFIG_DREAMSDK_SECTION_DREAMCAST_TOOL,
    'CustomArguments',
    DREAMCAST_TOOL_DEFAULT_CUSTOM_ARGUMENTS
  );
end;

procedure TDreamcastSoftwareDevelopmentSettingsDreamcastTool.SaveConfiguration(
  IniFile: TIniFile);
begin
  IniFile.WriteInteger(
    CONFIG_DREAMSDK_SECTION_DREAMCAST_TOOL,
    'Kind',
    Integer(fDreamcastToolKind)
  );
  IniFile.WriteBool(
    CONFIG_DREAMSDK_SECTION_DREAMCAST_TOOL,
    'AttachConsoleFileserver',
    fAttachConsoleFileserver
  );
  IniFile.WriteBool(
    CONFIG_DREAMSDK_SECTION_DREAMCAST_TOOL,
    'ClearScreenBeforeDownload',
    fClearScreenBeforeDownload
  );
  IniFile.WriteString(
    CONFIG_DREAMSDK_SECTION_DREAMCAST_TOOL,
    'InternetProtocolAddress',
    fInternetProtocolAddress
  );
  IniFile.WriteBool(
    CONFIG_DREAMSDK_SECTION_DREAMCAST_TOOL,
    'MediaAccessControlEnabled',
    fMediaAccessControlEnabled
  );
  IniFile.WriteString(
    CONFIG_DREAMSDK_SECTION_DREAMCAST_TOOL,
    'MediaAccessControlAddress',
    fMediaAccessControlAddress
  );
  IniFile.WriteString(
    CONFIG_DREAMSDK_SECTION_DREAMCAST_TOOL,
    'HostMediaAccessControlAddress',
    fHostMediaAccessControlAddress
  );
  IniFile.WriteBool(
    CONFIG_DREAMSDK_SECTION_DREAMCAST_TOOL,
    'SerialDumbTerminal',
    fSerialDumbTerminal
  );
  IniFile.WriteBool(
    CONFIG_DREAMSDK_SECTION_DREAMCAST_TOOL,
    'SerialExternalClock',
    fSerialExternalClock
  );
  IniFile.WriteInteger(
    CONFIG_DREAMSDK_SECTION_DREAMCAST_TOOL,
    'SerialBaudrate',
    Integer(fSerialBaudrate)
  );
  IniFile.WriteBool(
    CONFIG_DREAMSDK_SECTION_DREAMCAST_TOOL,
    'SerialBaudrateAlternate',
    fSerialBaudrateAlternate
  );
  IniFile.WriteInteger(
    CONFIG_DREAMSDK_SECTION_DREAMCAST_TOOL,
    'SerialPort',
    Integer(fSerialPort)
  );
  IniFile.WriteString(
    CONFIG_DREAMSDK_SECTION_DREAMCAST_TOOL,
    'CustomExecutable',
    fCustomExecutable
  );
  IniFile.WriteString(
    CONFIG_DREAMSDK_SECTION_DREAMCAST_TOOL,
    'CustomArguments',
    fCustomArguments
  );
end;

{ TDreamcastSoftwareDevelopmentSettingsDreamcastTool }

function TDreamcastSoftwareDevelopmentSettings.GetConfigurationFileName: TFileName;
begin
  if (fConfigurationFileName = '') then
   begin
     fConfigurationFileName := GetConfigurationDirectory + SETTINGS_FILE_NAME;
{$IFDEF DEBUG}
     if not FileExists(fConfigurationFileName) then
       fConfigurationFileName := GetApplicationPath + SETTINGS_FILE_NAME;
{$ENDIF}
   end;
   Result := fConfigurationFileName;
end;

constructor TDreamcastSoftwareDevelopmentSettings.Create;
begin
  fRuby := TDreamcastSoftwareDevelopmentSettingsRuby.Create(Self);
  fRepositories := TDreamcastSoftwareDevelopmentSettingsRepositories.Create;
  fDreamcastTool := TDreamcastSoftwareDevelopmentSettingsDreamcastTool.Create;
end;

destructor TDreamcastSoftwareDevelopmentSettings.Destroy;
begin
  fRepositories.Free;
  fDreamcastTool.Free;
  fRuby.Free;
  inherited Destroy;
end;

function TDreamcastSoftwareDevelopmentSettings.LoadConfiguration: Boolean;
var
  IniFile: TIniFile;
  DefaultInstallationPath: TFileName;

begin
  Result := FileExists(FileName);
  IniFile := TIniFile.Create(FileName);
  try
    // Settings
    DefaultInstallationPath := GetInstallationBaseDirectory;
    fInstallPath := IncludeTrailingPathDelimiter(
      IniFile.ReadString(
        CONFIG_DREAMSDK_SECTION_SETTINGS,
        'InstallPath',
        DefaultInstallationPath
      )
    );

    fUseMintty := IniFile.ReadBool(
      CONFIG_DREAMSDK_SECTION_SETTINGS,
      'UseMinTTY',
      False
    );

    fProgressWindowAutoClose := IniFile.ReadBool(
      CONFIG_DREAMSDK_SECTION_SETTINGS,
      'ProgressWindowAutoClose',
      True
    );

    DreamcastTool.LoadConfiguration(IniFile);

    Ruby.LoadConfiguration(IniFile);

    Result := True;
  finally
    IniFile.Free;
  end;
end;

procedure TDreamcastSoftwareDevelopmentSettings.SaveConfiguration;
var
  IniFile: TIniFile;

begin
  IniFile := TIniFile.Create(FileName);
  try
    // Settings
    IniFile.WriteString(
      CONFIG_DREAMSDK_SECTION_SETTINGS,
      'InstallPath',
      fInstallPath
    );
    IniFile.WriteBool(
      CONFIG_DREAMSDK_SECTION_SETTINGS,
      'UseMinTTY',
      fUseMintty
    );
    IniFile.WriteBool(
      CONFIG_DREAMSDK_SECTION_SETTINGS,
      'ProgressWindowAutoClose',
      fProgressWindowAutoClose
    );

    DreamcastTool.SaveConfiguration(IniFile);

    Ruby.SaveConfiguration(IniFile);
  finally
    IniFile.Free;
  end;
end;

end.

