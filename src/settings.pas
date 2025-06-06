unit Settings;

// {$DEFINE DEBUG_SETTINGS_RECOMPUTE_DYNAMIC_DIRECTORIES}

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  IniFiles,
  SysTools,
  FSTools;

const
  // DreamSDK Application GUID (coming from "dreamsdk.iss", i.e., DreamSDK Setup)
  DREAMSDK_APPLICATION_ID = '{df847892-5d85-4ffa-8603-e71750d81602}';

  // Name of main configuration file for DreamSDK
  SETTINGS_FILE_NAME = 'dreamsdk.conf';

  // Dreamcast Tool
  DREAMCAST_TOOL_DEFAULT_SERIAL_BAUDRATE_MINIMAL_ALLOWED = 300;
  DREAMCAST_TOOL_DEFAULT_SERIAL_BAUDRATE_ALTERNATE_ALLOWED = 115200;

  DREAMCAST_TOOL_DEFAULT_KIND = 0;
  DREAMCAST_TOOL_DEFAULT_ATTACH_CONSOLE_FILESERVER = True;
  DREAMCAST_TOOL_DEFAULT_CLEAR_SCREEN_BEFORE_DOWNLOAD = True;
  DREAMCAST_TOOL_DEFAULT_INTERNET_PROTOCOL_ADDRESS = '000.000.000.000';
  DREAMCAST_TOOL_DEFAULT_MEDIA_ACCESS_CONTROL_ENABLED = False;
  DREAMCAST_TOOL_DEFAULT_MEDIA_ACCESS_CONTROL_ADDRESS = '00-00-00-00-00-00';
  DREAMCAST_TOOL_DEFAULT_SERIAL_DUMB_TERMINAL = False;
  DREAMCAST_TOOL_DEFAULT_SERIAL_EXTERNAL_CLOCK = False;
  DREAMCAST_TOOL_DEFAULT_SERIAL_BAUDRATE = DREAMCAST_TOOL_DEFAULT_SERIAL_BAUDRATE_ALTERNATE_ALLOWED;
  DREAMCAST_TOOL_DEFAULT_SERIAL_BAUDRATE_ALTERNATE = False;
  DREAMCAST_TOOL_DEFAULT_SERIAL_PORT = 1;
  DREAMCAST_TOOL_DEFAULT_CUSTOM_EXECUTABLE = '';
  DREAMCAST_TOOL_DEFAULT_CUSTOM_ARGUMENTS = '';

type  
  TDreamcastToolKind = (
    dtkUndefined,
    dtkSerial,
    dtkInternetProtocol,
    dtkCustom
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
    fSerialBaudrate: Integer;
    fSerialBaudrateAlternate: Boolean;
    fSerialPort: Integer;
    function IsAlternateBaudrateAllowed: Boolean;
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
    property SerialPort: Integer
      read fSerialPort write fSerialPort;
    property SerialBaudrate: Integer
      read fSerialBaudrate write fSerialBaudrate;
    property SerialBaudrateAlternate: Boolean
      read fSerialBaudrateAlternate write fSerialBaudrateAlternate;
    property SerialBaudrateAlternateAllowed: Boolean
      read IsAlternateBaudrateAllowed;
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
    fConfiguredUsers: TWindowsUserAccountInformationArray;
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
{$IFDEF DISABLE_REFBASE_HOME_DIR_AUTO_OVERRIDE}
    procedure SetHomeDirectory(AValue: TFileName);
{$ENDIF}
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
{$IFDEF DISABLE_REFBASE_HOME_DIR_AUTO_OVERRIDE}
      write SetHomeDirectory
{$ENDIF};

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
  TDreamcastSoftwareDevelopmentSettingsCodeBlocksPatcher = class(
    TDreamcastSoftwareDevelopmentSettingsCodeBlocks
  )
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
    fWindowsTerminalProfileGuid: string;
    fWindowsTerminalProfileGuidGenerated: Boolean;
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
    property WindowsTerminalProfileGuid: string
      read fWindowsTerminalProfileGuid;
  end;

function GetDefaultCodeBlocksBackupDirectory: TFileName;

function GetDefaultUrlKallisti: string;
function GetDefaultUrlKallistiPorts: string;
function GetDefaultUrlDreamcastToolSerial: string;
function GetDefaultUrlDreamcastToolInternetProtocol: string;
function GetDefaultUrlRuby: string;

function SerialPortToString(SerialPortIndex: Integer): string;

implementation

uses
  TypInfo,
{$IFDEF GUI}
  Interfaces,
  Dialogs,
  MsgDlg,
{$ENDIF}
  RefBase,
  StrTools,
  Version,
  CBTools,
  RunTools;

const
  // Code::Blocks Backup Directory (for previous install)
  DEFAULT_CODEBLOCKS_BACKUP_DIR = '%s\support\ide\codeblocks\';

  // Export Library Path for DreamSDK Wizard for Code::Blocks
  EXPORT_LIBRARY_INFORMATION_DIR = '%s\ide\codeblocks\';

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

  // Handling default repositories (mainly used while installing DreamSDK)
  REPOSITORIES_DEFAULT_FILE_NAME = 'repositories-default.conf';

type
  { TDefaultRepositoryItem }
  TDefaultRepositoryItem = class(TObject)
  private
    fPrimary: string;
    fSecondary: string;
  public
    property Primary: string read fPrimary;
    property Secondary: string read fSecondary;
  end;

  { TDefaultRepository }
  TDefaultRepository = class(TObject)
  private
    fDreamcastToolInternetProtocol: TDefaultRepositoryItem;
    fDreamcastToolSerial: TDefaultRepositoryItem;
    fKallisti: TDefaultRepositoryItem;
    fKallistiPorts: TDefaultRepositoryItem;
    fRuby: TDefaultRepositoryItem;
    procedure Initialize(const AutoLoad: Boolean);
    function IsRepositoryAvailable(const Url: string): Boolean;
    function GetOptimalRepositoryUrl(Item: TDefaultRepositoryItem): string;
  protected
    function GetKallisti: string;
    function GetKallistiPorts: string;
    function GetDreamcastToolSerial: string;
    function GetDreamcastToolInternetProtocol: string;
    function GetRuby: string;
  public
    constructor Create;
    constructor Create(const AutoLoad: Boolean); overload;
    destructor Destroy; override;

    function Load(const FileName: TFileName): Boolean;
    function Load: Boolean; overload;

    property Kallisti: string read GetKallisti;
    property KallistiPorts: string read GetKallistiPorts;
    property DreamcastToolSerial: string read GetDreamcastToolSerial;
    property DreamcastToolInternetProtocol: string read GetDreamcastToolInternetProtocol;
    property Ruby: string read GetRuby;
  end;

var
  DefaultRepository: TDefaultRepository;

{$IFDEF DEBUG}
{$IFDEF DEBUG_SETTINGS_RECOMPUTE_DYNAMIC_DIRECTORIES}
resourcestring
  RecomputeDynamicDirectoriesTriggered = 'Recompute Dynamic Directories Triggered: "%s"';
{$ENDIF}
{$ENDIF}

function SerialPortToString(SerialPortIndex: Integer): string;
begin
  Result := Format('COM%d', [SerialPortIndex]);
end;

function GetDefaultCodeBlocksBackupDirectory: TFileName;
begin
  Result := Format(DEFAULT_CODEBLOCKS_BACKUP_DIR, [
      '%' + GetBaseEnvironmentVariableName + '%']);
end;

function GetDefaultUrlKallisti: string;
begin
  Result := DefaultRepository.Kallisti;
end;

function GetDefaultUrlKallistiPorts: string;
begin
  Result := DefaultRepository.KallistiPorts;
end;

function GetDefaultUrlDreamcastToolSerial: string;
begin
  Result := DefaultRepository.DreamcastToolSerial;
end;

function GetDefaultUrlDreamcastToolInternetProtocol: string;
begin
  Result := DefaultRepository.DreamcastToolInternetProtocol;
end;

function GetDefaultUrlRuby: string;
begin
  Result := DefaultRepository.Ruby;
end;

{ TDefaultRepository }

constructor TDefaultRepository.Create;
begin
  Initialize(True);
end;

constructor TDefaultRepository.Create(const AutoLoad: Boolean);
begin
  Initialize(AutoLoad);
end;

destructor TDefaultRepository.Destroy;
begin
  fRuby.Free;
  fDreamcastToolInternetProtocol.Free;
  fDreamcastToolSerial.Free;
  fKallistiPorts.Free;
  fKallisti.Free;

  inherited Destroy;
end;

procedure TDefaultRepository.Initialize(const AutoLoad: Boolean);
begin
  fKallisti := TDefaultRepositoryItem.Create;
  fKallistiPorts := TDefaultRepositoryItem.Create;
  fDreamcastToolSerial := TDefaultRepositoryItem.Create;
  fDreamcastToolInternetProtocol := TDefaultRepositoryItem.Create;
  fRuby := TDefaultRepositoryItem.Create;

  if AutoLoad then
    Load;
end;

function TDefaultRepository.IsRepositoryAvailable(const Url: string): Boolean;

  function ExtractHostFromUrl(const Url: string): string;
  begin
    Result := ExtractStr('://', '/', Url);
    if IsEmpty(Result) then
      Result := Right('://', Url);
  end;

  function IsHostReachable(const Host: string): Boolean;
  const
    HOSTS_STATUS_FILE_NAME = 'hosts-status.conf';
    GIT_VALID_KEYWORD = #$09'HEAD'#$0D#$0A;
    GIT_CHECK_CMD = 'where git > nul 2>&1 && git ls-remote "%s"';

  type
    THostReachableStatus = (hrsNotReachable, hrsReachable, hrsUndefined);

  var
    Buffer: string;
    ProcessExitCode: Integer;
    HostsStatusFile: TIniFile;
    HostReachableStatus: THostReachableStatus;

  begin
    Result := False;
    Buffer := Default(string);
    ProcessExitCode := Default(Integer);

    HostsStatusFile := TIniFile.Create(GetConfigurationDirectory + HOSTS_STATUS_FILE_NAME);
    try
      HostReachableStatus := THostReachableStatus(
        HostsStatusFile.ReadInteger('Status', Host, Integer(hrsUndefined))
      );

      if (HostReachableStatus = hrsUndefined) then
      begin
        if RunSingleCommand(Format(GIT_CHECK_CMD, [Url]), Buffer, ProcessExitCode) then
          if (ProcessExitCode = 0) then
            Result := IsInString(GIT_VALID_KEYWORD, Buffer);
        HostsStatusFile.WriteBool('Status', Host, Result);
      end;
    finally
      HostsStatusFile.Free;
    end;
  end;

var
  Host: string;

begin
  Host := ExtractHostFromUrl(Url);
  Result := IsHostReachable(Host);
end;

function TDefaultRepository.GetOptimalRepositoryUrl(
  Item: TDefaultRepositoryItem): string;
begin
  Result := EmptyStr;
  if Assigned(Item) then
  begin
    Result := Item.Primary;
    if not IsRepositoryAvailable(Result) then
      Result := Item.Secondary;
  end;
end;

function TDefaultRepository.GetKallisti: string;
begin
  Result := GetOptimalRepositoryUrl(fKallisti);
end;

function TDefaultRepository.GetKallistiPorts: string;
begin
  Result := GetOptimalRepositoryUrl(fKallistiPorts);
end;

function TDefaultRepository.GetDreamcastToolSerial: string;
begin
  Result := GetOptimalRepositoryUrl(fDreamcastToolSerial);
end;

function TDefaultRepository.GetDreamcastToolInternetProtocol: string;
begin
  Result := GetOptimalRepositoryUrl(fDreamcastToolInternetProtocol);
end;

function TDefaultRepository.GetRuby: string;
begin
  Result := GetOptimalRepositoryUrl(fRuby);
end;

function TDefaultRepository.Load(const FileName: TFileName): Boolean;
var
  IniFile: TIniFile;

begin
  Result := False;
  if FileExists(FileName) then
  begin
    IniFile := TIniFile.Create(FileName);
    try
      fKallisti.fPrimary := IniFile.ReadString('Kallisti', 'Primary', EmptyStr);
      fKallisti.fSecondary := IniFile.ReadString('Kallisti', 'Secondary', EmptyStr);

      fKallistiPorts.fPrimary := IniFile.ReadString('KallistiPorts', 'Primary', EmptyStr);
      fKallistiPorts.fSecondary := IniFile.ReadString('KallistiPorts', 'Secondary', EmptyStr);

      fDreamcastToolSerial.fPrimary := IniFile.ReadString('DreamcastToolSerial', 'Primary', EmptyStr);
      fDreamcastToolSerial.fSecondary := IniFile.ReadString('DreamcastToolSerial', 'Secondary', EmptyStr);

      fDreamcastToolInternetProtocol.fPrimary := IniFile.ReadString('DreamcastToolInternetProtocol', 'Primary', EmptyStr);
      fDreamcastToolInternetProtocol.fSecondary := IniFile.ReadString('DreamcastToolInternetProtocol', 'Secondary', EmptyStr);

      fRuby.fPrimary := IniFile.ReadString('Ruby', 'Primary', EmptyStr);
      fRuby.fSecondary := IniFile.ReadString('Ruby', 'Secondary', EmptyStr);

      Result := True;
    finally
      IniFile.Free;
    end;
  end;
end;

function TDefaultRepository.Load: Boolean;
begin
  Result := Load(GetConfigurationDirectory + REPOSITORIES_DEFAULT_FILE_NAME);
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
  Result := GetURL(fDreamcastToolInternetProtocolURL, GetDefaultUrlDreamcastToolInternetProtocol);
end;

function TDreamcastSoftwareDevelopmentSettingsRepositories.GetDreamcastToolSerialURL: string;
begin
  Result := GetURL(fDreamcastToolSerialURL, GetDefaultUrlDreamcastToolSerial);
end;

function TDreamcastSoftwareDevelopmentSettingsRepositories.GetKallistiPortsURL: string;
begin
  Result := GetURL(fKallistiPortsURL, GetDefaultUrlKallistiPorts);
end;

function TDreamcastSoftwareDevelopmentSettingsRepositories.GetKallistiURL: string;
begin
  Result := GetURL(fKallistiURL, GetDefaultUrlKallisti);
end;

function TDreamcastSoftwareDevelopmentSettingsRepositories.GetRubyURL: string;
begin
  Result := GetURL(fRubyURL, GetDefaultUrlRuby);
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
  GetCodeBlocksAvailableConfigurationFileNames(fConfiguredUsers,
    ConfigurationFileNames);
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
        UserName := fConfiguredUsers[i].FriendlyName;
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
  SaveConfiguration;
end;

{ TDreamcastSoftwareDevelopmentSettingsCodeBlocks }

function TDreamcastSoftwareDevelopmentSettingsCodeBlocks
  .GetRegistryFileName: TFileName;
const
  IDE_CONFIGURATION_FILE = 'ide.conf';

begin
  Result := IncludeTrailingPathDelimiter(HomeDirectory)
    + GetConfigurationPartialPath + IDE_CONFIGURATION_FILE;
{$IFDEF DEBUG}
  DebugLog('Settings Code::Blocks GetRegistryFileName: "' + Result + '"');
{$ENDIF}
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

{$IFDEF DISABLE_REFBASE_HOME_DIR_AUTO_OVERRIDE}

procedure TDreamcastSoftwareDevelopmentSettingsCodeBlocks
  .SetHomeDirectory(AValue: TFileName);
begin
  if fHomeDirectory <> AValue then
  begin
    fHomeDirectory := AValue;
    SetBaseInstallationHomeDirectory(fHomeDirectory);
    HandleDynamicDirectories;
  end;
end;

{$ENDIF}

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
  fExportLibraryInformationPath := Format(EXPORT_LIBRARY_INFORMATION_DIR, [
    ExcludeTrailingPathDelimiter(GetConfigurationDirectory)]);
{$IFDEF DEBUG}
  DebugLog('Settings Code::Blocks HandleDynamicDirectories: "'
    + fExportLibraryInformationPath + '"');
{$ENDIF}
end;

constructor TDreamcastSoftwareDevelopmentSettingsCodeBlocks.Create;
begin
  fConfigurationFileNames := TFileList.Create;
  fAvailableConfigurationFileNames := TFileList.Create;
  fAvailableUsers := TStringList.Create;
  fInstalledUsers := TStringList.Create;
  fHomeDirectory := GetBaseInstallationHomeDirectory;
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
  i: Integer;
  BufferAvailableUsers: TWindowsUserAccountInformationArray;
  IniFile: TIniFile; // ide.conf
  Temp: string;
  RecomputeDynamicDirectories: Boolean;

  procedure CheckIfRecomputeDynamicDirectories(const Directory: TFileName);
{$IF DEFINED(DEBUG) AND DEFINED(DEBUG_SETTINGS_RECOMPUTE_DYNAMIC_DIRECTORIES)}
  var
    MessageRecomputeDynamicDirectoriesTriggered: string;
{$ENDIF}
  begin
    RecomputeDynamicDirectories := RecomputeDynamicDirectories
      or IsEmpty(Directory) or (not DirectoryExists(Directory));

{$IF DEFINED(DEBUG) AND DEFINED(DEBUG_SETTINGS_RECOMPUTE_DYNAMIC_DIRECTORIES)}
    if RecomputeDynamicDirectories then
    begin
      MessageRecomputeDynamicDirectoriesTriggered := Format(
        RecomputeDynamicDirectoriesTriggered, [Directory]);
{$IFDEF GUI}
      MsgBoxDlg(0, sWarning, PChar(MessageRecomputeDynamicDirectoriesTriggered),
        mtWarning, [mbOk]);
{$ELSE}
      DebugLog(MessageRecomputeDynamicDirectoriesTriggered);
{$ENDIF}
    end;
{$ENDIF}
  end;

begin
  Result := FileExists(RegistryFileName);
  RecomputeDynamicDirectories := False;

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
      CheckIfRecomputeDynamicDirectories(fExportLibraryInformationPath);

      // Installation Directory
      InstallationDirectory :=
        IniFile.ReadString(CONFIG_IDE_SECTION_CODEBLOCKS,
          CONFIG_IDE_SECTION_CODEBLOCKS_KEY_INSTALLATION_PATH,
            InstallationDirectory);

      // Backup Directory
      BackupDirectory :=
        IniFile.ReadString(CONFIG_IDE_SECTION_CODEBLOCKS,
          CONFIG_IDE_SECTION_CODEBLOCKS_KEY_BACKUP_PATH, BackupDirectory);
      CheckIfRecomputeDynamicDirectories(BackupDirectory);

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

      // Available Users
      AvailableUsers.Clear;
      GetCodeBlocksAvailableUsers(BufferAvailableUsers);
      for i := Low(BufferAvailableUsers) to High(BufferAvailableUsers) do
        AvailableUsers.Add(BufferAvailableUsers[i].FriendlyName);

      // Recompute directories if empty or not exists
      if RecomputeDynamicDirectories then
      begin
{$IFDEF DEBUG}
        DebugLog('* RecomputeDynamicDirectories after LoadConfiguration');
{$ENDIF}
        HandleDynamicDirectories;
      end;
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
      CONFIG_IDE_SECTION_CODEBLOCKS_KEY_USERS_INSTALLED,
        StringListToString(InstalledUsers, ArraySeparator));
  finally
    IniFile.Free;
  end;
end;

{ TDreamcastSoftwareDevelopmentSettingsDreamcastTool }

function TDreamcastSoftwareDevelopmentSettingsDreamcastTool.IsAlternateBaudrateAllowed: Boolean;
begin
  Result := (SerialBaudrate >= DREAMCAST_TOOL_DEFAULT_SERIAL_BAUDRATE_ALTERNATE_ALLOWED);
end;

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
  fSerialBaudrate := IniFile.ReadInteger(
    CONFIG_DREAMSDK_SECTION_DREAMCAST_TOOL,
    'SerialBaudrate',
    DREAMCAST_TOOL_DEFAULT_SERIAL_BAUDRATE
  );
  fSerialBaudrateAlternate := IniFile.ReadBool(
    CONFIG_DREAMSDK_SECTION_DREAMCAST_TOOL,
    'SerialBaudrateAlternate',
    DREAMCAST_TOOL_DEFAULT_SERIAL_BAUDRATE_ALTERNATE
  );
  fSerialPort := IniFile.ReadInteger(
    CONFIG_DREAMSDK_SECTION_DREAMCAST_TOOL,
    'SerialPort',
    DREAMCAST_TOOL_DEFAULT_SERIAL_PORT
  );
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
  if fWindowsTerminalProfileGuidGenerated then
   SaveConfiguration;
  fRepositories.Free;
  fDreamcastTool.Free;
  fRuby.Free;
  inherited Destroy;
end;

function TDreamcastSoftwareDevelopmentSettings.LoadConfiguration: Boolean;
var
  IniFile: TIniFile;
  DefaultInstallationPath: TFileName;
  RealGuid: TGUID;

begin
  Result := FileExists(FileName);
  IniFile := TIniFile.Create(FileName);
  try
    // Settings
    DefaultInstallationPath := GetBaseInstallationHomeDirectory;
    fInstallPath := IncludeTrailingPathDelimiter(
      IniFile.ReadString(
        CONFIG_DREAMSDK_SECTION_SETTINGS,
        'InstallPath',
        DefaultInstallationPath
      )
    );

    // Fail-safe check
    // This part will be removed at some point.
    if (not SameText(fInstallPath, GetBaseInstallationHomeDirectory)) then
    begin
{$IFDEF DEBUG}
      DebugLog('*** ERROR: Incorrect InstallPath detected...');
{$ENDIF}
      fInstallPath := GetBaseInstallationHomeDirectory;
    end;

    fUseMintty := IniFile.ReadBool(
      CONFIG_DREAMSDK_SECTION_SETTINGS,
      'UseMinTTY',
      True // Use MinTTY by default (much better)
    );

    fProgressWindowAutoClose := IniFile.ReadBool(
      CONFIG_DREAMSDK_SECTION_SETTINGS,
      'ProgressWindowAutoClose',
      True
    );

    // Handle GUID for Windows Terminal
    fWindowsTerminalProfileGuidGenerated := False;
    fWindowsTerminalProfileGuid := IniFile.ReadString(
      CONFIG_DREAMSDK_SECTION_SETTINGS,
      'WindowsTerminalProfileGuid',
      EmptyStr
    );
    if not TryStringToGUID(fWindowsTerminalProfileGuid, RealGuid) then
    begin
      fWindowsTerminalProfileGuid := LowerCase(DREAMSDK_APPLICATION_ID);
      fWindowsTerminalProfileGuidGenerated := True;
    end;

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
    IniFile.WriteString(
      CONFIG_DREAMSDK_SECTION_SETTINGS,
      'WindowsTerminalProfileGuid',
      fWindowsTerminalProfileGuid
    );

    DreamcastTool.SaveConfiguration(IniFile);

    Ruby.SaveConfiguration(IniFile);
  finally
    IniFile.Free;
  end;
end;

initialization
  DefaultRepository := TDefaultRepository.Create;

finalization
  DefaultRepository.Free;

end.

