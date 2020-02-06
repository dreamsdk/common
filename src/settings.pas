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

  // Repositories
  DEFAULT_KALLISTI_URL = 'https://gitlab.com/simulant/community/kallistios-nitro.git';
  DEFAULT_KALLISTI_PORTS_URL = 'https://gitlab.com/simulant/community/kallistios-ports-nitro.git';
  DEFAULT_DREAMCAST_TOOL_SERIAL_URL = 'https://gitlab.com/simulant/community/dcload-serial-nitro.git';
  DEFAULT_DREAMCAST_TOOL_INTERNET_PROTOCOL_URL = 'https://gitlab.com/simulant/community/dcload-ip-nitro.git';

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
    dtb115200
  );

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
  public
    property KallistiURL: string
      read fKallistiURL write fKallistiURL;
    property KallistiPortsURL: string
      read fKallistiPortsURL write fKallistiPortsURL;
    property DreamcastToolSerialURL: string
      read fDreamcastToolSerialURL write fDreamcastToolSerialURL;
    property DreamcastToolInternetProtocolURL: string
      read fDreamcastToolInternetProtocolURL write fDreamcastToolInternetProtocolURL;
  end;

  { TDreamcastSoftwareDevelopmentCodeBlocksSettings }
  TDreamcastSoftwareDevelopmentCodeBlocksSettings = class(TObject)
  private
    fAvailableUsers: TStringList;
    fExportLibraryInformation: Boolean;
    fExportLibraryInformationPath: TFileName;
    fInstalledUsers: TStringList;
    fBackupDirectory: TFileName;
    fConfigurationFileNames: TFileList;
    fAvailableConfigurationFileNames: TFileList;
    fInstallationDirectory: TFileName;
    function GetAvailableUsers: TStringList;
    function GetInstalledUsers: TStringList;
    function GetRegistryFileName: TFileName;
    procedure SetBackupDirectory(AValue: TFileName);
    procedure SetExportLibraryInformation(AValue: Boolean);
    procedure SetHomeDirectory(AValue: TFileName);
    procedure SetInstallationDirectory(AValue: TFileName);
  protected
    fInstalled: Boolean;
    fHomeDirectory: TFileName;
  public
    constructor Create;
    destructor Destroy; override;

    function LoadConfiguration: Boolean;
    procedure SaveConfiguration;

    // Code::Blocks Configuration Files (default.conf)
    property ConfigurationFileNames: TFileList
      read fConfigurationFileNames;

    // Code::Blocks Available Users
    property AvailableUsers: TStringList
      read GetAvailableUsers;

    // Code::Blocks Installed Users (if any)
    property InstalledUsers: TStringList
      read GetInstalledUsers;

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

    property ExportLibraryInformation: Boolean
      read fExportLibraryInformation
      write SetExportLibraryInformation;

    property ExportLibraryInformationPath: TFileName
      read fExportLibraryInformationPath;

    // DreamSDK Home
    property HomeDirectory: TFileName
      read fHomeDirectory
      write SetHomeDirectory;

    property Installed: Boolean
      read fInstalled;
  end;

  { TDreamcastSoftwareDevelopmentCodeBlocksPatcherSettings }

  TDreamcastSoftwareDevelopmentCodeBlocksPatcherSettings
    = class(TDreamcastSoftwareDevelopmentCodeBlocksSettings)
  private
    procedure InitializeDefaults;
  public
    constructor Create;
    procedure SaveInstall;
    procedure SaveUninstall;
  end;

  { TDreamcastSoftwareDevelopmentSettings }
  TDreamcastSoftwareDevelopmentSettings = class(TObject)
  private
    fConfigurationFileName: TFileName;
    fInstallPath: TFileName;
    fProgressWindowAutoClose: Boolean;
    fRepositories: TDreamcastSoftwareDevelopmentSettingsRepositories;
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
  end;

function GetDefaultCodeBlocksBackupDirectory: TFileName;
function SerialPortToString(SerialPort: TDreamcastToolSerialPort): string;
function SerialBaudrateToString(SerialBaudrate: TDreamcastToolSerialBaudrate): string;

implementation

uses
  RefBase, SysTools, Version;

const
  CONFIG_SETTINGS_SECTION_NAME = 'Settings';
  CONFIG_DREAMCAST_TOOL_SECTION_NAME = 'DreamcastTool';

  CONFIG_SECTION_GLOBAL = 'Global';
  CONFIG_SECTION_GLOBAL_KEY_CODEBLOCKS = 'CodeBlocks';

  CONFIG_SECTION_CODEBLOCKS = 'CodeBlocks';
  CONFIG_SECTION_CODEBLOCKS_KEY_EXPORT_LIBRARY_INFORMATION_ENABLED = 'ExportLibraryInformation';
  CONFIG_SECTION_CODEBLOCKS_KEY_EXPORT_LIBRARY_INFORMATION_PATH = 'ExportLibraryInformationPath';
  CONFIG_SECTION_CODEBLOCKS_KEY_CONFIGURATION_FILENAMES = 'ConfigurationFileNames';
  CONFIG_SECTION_CODEBLOCKS_KEY_INSTALLATION_PATH = 'InstallationPath';
  CONFIG_SECTION_CODEBLOCKS_KEY_BACKUP_PATH = 'BackupPath';
  CONFIG_SECTION_CODEBLOCKS_KEY_USERS_INSTALLED = 'InstalledUsers';
  CONFIG_SECTION_CODEBLOCKS_KEY_USERS_AVAILABLE = 'AvailableUsers';

  DEFAULT_CODEBLOCKS_DIR_64 = '%ProgramFiles(x86)%\CodeBlocks';
  DEFAULT_CODEBLOCKS_DIR_32 = '%ProgramFiles%\CodeBlocks';
  DEFAULT_CODEBLOCKS_CONFIGURATION_FILE = '%sCodeBlocks\default.conf';
  DEFAULT_CODEBLOCKS_BACKUP_DIR = '%s\support\ide\codeblocks\';

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
  end;
end;

function GetDefaultCodeBlocksBackupDirectory: TFileName;
begin
  Result := Format(DEFAULT_CODEBLOCKS_BACKUP_DIR, [
      '%' + GetBaseEnvironmentVariableName + '%']);
end;

{ TDreamcastSoftwareDevelopmentCodeBlocksPatcherSettings }

// Define this to debug this procedure
// {$DEFINE DEBUG_INITIALIZE_DEFAULTS}
procedure TDreamcastSoftwareDevelopmentCodeBlocksPatcherSettings.InitializeDefaults;
var
  UsersAppData: TStringList;
  i: Integer;
  CodeBlocksConfigurationFileName: TFileName;

begin
  // Code::Blocks Installation Directory
  InstallationDirectory := DEFAULT_CODEBLOCKS_DIR_32;
  if IsWindows64 then
    InstallationDirectory := DEFAULT_CODEBLOCKS_DIR_64;

  // Code::Blocks Configuration Files
  UsersAppData := TStringList.Create;
  try
    GetAppDataListFromUsers(UsersAppData);

    for i := 0 to UsersAppData.Count - 1 do
    begin
      CodeBlocksConfigurationFileName := Format(
        DEFAULT_CODEBLOCKS_CONFIGURATION_FILE,
        [ IncludeTrailingPathDelimiter(UsersAppData[i]) ]
      );
{$IFDEF DEBUG}
{$IFDEF DEBUG_INITIALIZE_DEFAULTS}
      Write(CodeBlocksConfigurationFileName, ' ... ');
{$ENDIF}
{$ENDIF}
      if FileExists(CodeBlocksConfigurationFileName) then
      begin
{$IFDEF DEBUG}
{$IFDEF DEBUG_INITIALIZE_DEFAULTS}
        WriteLn('exist!');
{$ENDIF}
{$ENDIF}
        ConfigurationFileNames.Add(CodeBlocksConfigurationFileName);
        fAvailableConfigurationFileNames.Add(CodeBlocksConfigurationFileName);
      end
{$IFDEF DEBUG}
{$IFDEF DEBUG_INITIALIZE_DEFAULTS}
      else
        WriteLn('doesn''t exist!')
{$ENDIF}
{$ENDIF};
    end;
  finally
    UsersAppData.Free;
  end;
end;

constructor TDreamcastSoftwareDevelopmentCodeBlocksPatcherSettings.Create;
begin
  inherited Create;
  InitializeDefaults;
end;

procedure TDreamcastSoftwareDevelopmentCodeBlocksPatcherSettings.SaveInstall;
begin
  fInstalled := True;
  SaveConfiguration;
end;

procedure TDreamcastSoftwareDevelopmentCodeBlocksPatcherSettings.SaveUninstall;
begin
  fInstalled := False;
  ExportLibraryInformation := False;
  InstallationDirectory := EmptyStr;
  BackupDirectory := EmptyStr;
  SaveConfiguration;
end;

{ TDreamcastSoftwareDevelopmentCodeBlocksSettings }

function TDreamcastSoftwareDevelopmentCodeBlocksSettings
  .GetAvailableUsers: TStringList;
var
  i: Integer;
  UsersDirectory: TFileName;
  CurrentUserName: string;

begin
  fAvailableUsers.Clear;
  UsersDirectory := GetUsersDirectory;
  for i := 0 to fAvailableConfigurationFileNames.Count - 1 do
  begin
    CurrentUserName := ExtractStr(UsersDirectory, DirectorySeparator,
      fAvailableConfigurationFileNames[i]);
    fAvailableUsers.Add(GetFriendlyUserName(CurrentUserName));
  end;
  Result := fAvailableUsers;
end;

function TDreamcastSoftwareDevelopmentCodeBlocksSettings
  .GetInstalledUsers: TStringList;
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
  Result := fInstalledUsers;
end;

function TDreamcastSoftwareDevelopmentCodeBlocksSettings
  .GetRegistryFileName: TFileName;
const
  IDE_CONFIGURATION_FILE = 'msys\1.0\etc\dreamsdk\ide.conf';

begin
  Result := IncludeTrailingPathDelimiter(fHomeDirectory)
    + IDE_CONFIGURATION_FILE;
end;

procedure TDreamcastSoftwareDevelopmentCodeBlocksSettings
  .SetBackupDirectory(AValue: TFileName);
begin
  if (fBackupDirectory <> AValue) then
    fBackupDirectory := IncludeTrailingPathDelimiter(
      ParseInputFileSystemObject(AValue));
end;

procedure TDreamcastSoftwareDevelopmentCodeBlocksSettings
  .SetExportLibraryInformation(AValue: Boolean);
begin
  if (fExportLibraryInformation <> AValue) then
  begin
    fExportLibraryInformation := AValue;
    if AValue then
    begin
      if not DirectoryExists(ExportLibraryInformationPath) then
        ForceDirectories(ExportLibraryInformationPath);
      SetDirectoryRights(ExportLibraryInformationPath, GetEveryoneName,
        ACL_RIGHT_FULL);
    end;
  end;
end;

procedure TDreamcastSoftwareDevelopmentCodeBlocksSettings
  .SetHomeDirectory(AValue: TFileName);
begin
  if fHomeDirectory <> AValue then
  begin
    fHomeDirectory := ExcludeTrailingPathDelimiter(
      ParseInputFileSystemObject(AValue));

    // Code::Blocks Backup Directory
    if IsEmpty(fBackupDirectory) then
      BackupDirectory := Format(DEFAULT_CODEBLOCKS_BACKUP_DIR, [fHomeDirectory]);
  end;
end;

procedure TDreamcastSoftwareDevelopmentCodeBlocksSettings
  .SetInstallationDirectory(AValue: TFileName);
const
  IDE_EXPORT_LIB_INFO_DIR = 'share\CodeBlocks\templates\wizard\dc\libinfo\';

begin
  if fInstallationDirectory <> AValue then
  begin
    fInstallationDirectory := IncludeTrailingPathDelimiter(
      ParseInputFileSystemObject(AValue));

    // Export Library Information
    if not IsEmpty(fInstallationDirectory) then
      fExportLibraryInformationPath := fInstallationDirectory
        + IDE_EXPORT_LIB_INFO_DIR
    else
      fExportLibraryInformationPath := EmptyStr;
  end;
end;

constructor TDreamcastSoftwareDevelopmentCodeBlocksSettings.Create;
begin
  fConfigurationFileNames := TFileList.Create;
  fAvailableConfigurationFileNames := TFileList.Create;
  fAvailableUsers := TStringList.Create;
  fInstalledUsers := TStringList.Create;
  HomeDirectory := GetInstallationBaseDirectory;
end;

destructor TDreamcastSoftwareDevelopmentCodeBlocksSettings.Destroy;
begin
  fAvailableConfigurationFileNames.Free;
  fInstalledUsers.Free;
  fAvailableUsers.Free;
  fConfigurationFileNames.Free;
  inherited Destroy;
end;

function TDreamcastSoftwareDevelopmentCodeBlocksSettings
  .LoadConfiguration: Boolean;
var
  IniFile: TIniFile;
  Temp: string;

begin
  Result := FileExists(RegistryFileName);
  if Result then
  begin
    IniFile := TIniFile.Create(GetRegistryFileName);
    try
      // Global C::B
      fInstalled := IniFile.ReadBool(CONFIG_SECTION_GLOBAL,
        CONFIG_SECTION_GLOBAL_KEY_CODEBLOCKS, fInstalled);

      // Export Library Information
      IniFile.ReadBool(CONFIG_SECTION_CODEBLOCKS,
        CONFIG_SECTION_CODEBLOCKS_KEY_EXPORT_LIBRARY_INFORMATION_ENABLED,
          ExportLibraryInformation);

      // Export Library Information Path
      fExportLibraryInformationPath := IniFile.ReadString(
        CONFIG_SECTION_CODEBLOCKS,
        CONFIG_SECTION_CODEBLOCKS_KEY_EXPORT_LIBRARY_INFORMATION_PATH,
        ExportLibraryInformationPath);

      // Installation Directory
      InstallationDirectory :=
        IniFile.ReadString(CONFIG_SECTION_CODEBLOCKS,
          CONFIG_SECTION_CODEBLOCKS_KEY_INSTALLATION_PATH,
            InstallationDirectory);

      // Backup Directory
      BackupDirectory :=
        IniFile.ReadString(CONFIG_SECTION_CODEBLOCKS,
          CONFIG_SECTION_CODEBLOCKS_KEY_BACKUP_PATH, BackupDirectory);

      // Users Available
      Temp := IniFile.ReadString(CONFIG_SECTION_CODEBLOCKS,
        CONFIG_SECTION_CODEBLOCKS_KEY_USERS_AVAILABLE, EmptyStr);
      if not IsEmpty(Temp) then
       StringListToString(AvailableUsers, ArraySeparator);

      // Users Installed
      Temp := IniFile.ReadString(CONFIG_SECTION_CODEBLOCKS,
        CONFIG_SECTION_CODEBLOCKS_KEY_USERS_INSTALLED, EmptyStr);
      if not IsEmpty(Temp) then
       StringListToString(InstalledUsers, ArraySeparator);

      // Configuration File Names
      Temp := IniFile.ReadString(CONFIG_SECTION_CODEBLOCKS,
        CONFIG_SECTION_CODEBLOCKS_KEY_CONFIGURATION_FILENAMES, EmptyStr);
      if not IsEmpty(Temp) then
        ConfigurationFileNames.SetItems(Temp, ArraySeparator);
    finally
      IniFile.Free;
    end;
  end;
end;

procedure TDreamcastSoftwareDevelopmentCodeBlocksSettings.SaveConfiguration;
var
  IniFile: TIniFile;

begin
  IniFile := TIniFile.Create(RegistryFileName);
  try
    // Global C::B
    IniFile.WriteBool(CONFIG_SECTION_GLOBAL,
      CONFIG_SECTION_GLOBAL_KEY_CODEBLOCKS, Installed);

    // Settings for C::B
    IniFile.WriteBool(CONFIG_SECTION_CODEBLOCKS,
      CONFIG_SECTION_CODEBLOCKS_KEY_EXPORT_LIBRARY_INFORMATION_ENABLED,
        ExportLibraryInformation);

    IniFile.WriteString(CONFIG_SECTION_CODEBLOCKS,
      CONFIG_SECTION_CODEBLOCKS_KEY_EXPORT_LIBRARY_INFORMATION_PATH,
        ExportLibraryInformationPath);

    IniFile.WriteString(CONFIG_SECTION_CODEBLOCKS,
      CONFIG_SECTION_CODEBLOCKS_KEY_INSTALLATION_PATH,
        InstallationDirectory);

    IniFile.WriteString(CONFIG_SECTION_CODEBLOCKS,
      CONFIG_SECTION_CODEBLOCKS_KEY_BACKUP_PATH,
        BackupDirectory);

    IniFile.WriteString(CONFIG_SECTION_CODEBLOCKS,
      CONFIG_SECTION_CODEBLOCKS_KEY_CONFIGURATION_FILENAMES,
        ConfigurationFileNames.GetItems(ArraySeparator));

    IniFile.WriteString(CONFIG_SECTION_CODEBLOCKS,
      CONFIG_SECTION_CODEBLOCKS_KEY_USERS_AVAILABLE,
        StringListToString(AvailableUsers, ArraySeparator));

    IniFile.WriteString(CONFIG_SECTION_CODEBLOCKS,
      CONFIG_SECTION_CODEBLOCKS_KEY_USERS_INSTALLED,
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
    CONFIG_DREAMCAST_TOOL_SECTION_NAME,
    'Kind',
    DREAMCAST_TOOL_DEFAULT_KIND
  ));
  fAttachConsoleFileserver := IniFile.ReadBool(
    CONFIG_DREAMCAST_TOOL_SECTION_NAME,
    'AttachConsoleFileserver',
    DREAMCAST_TOOL_DEFAULT_ATTACH_CONSOLE_FILESERVER
  );
  fClearScreenBeforeDownload := IniFile.ReadBool(
    CONFIG_DREAMCAST_TOOL_SECTION_NAME,
    'ClearScreenBeforeDownload',
    DREAMCAST_TOOL_DEFAULT_CLEAR_SCREEN_BEFORE_DOWNLOAD
  );
  fInternetProtocolAddress := IniFile.ReadString(
    CONFIG_DREAMCAST_TOOL_SECTION_NAME,
    'InternetProtocolAddress',
    DREAMCAST_TOOL_DEFAULT_INTERNET_PROTOCOL_ADDRESS
  );
  fMediaAccessControlEnabled := IniFile.ReadBool(
    CONFIG_DREAMCAST_TOOL_SECTION_NAME,
    'MediaAccessControlEnabled',
    DREAMCAST_TOOL_DEFAULT_MEDIA_ACCESS_CONTROL_ENABLED
  );
  fMediaAccessControlAddress := IniFile.ReadString(
    CONFIG_DREAMCAST_TOOL_SECTION_NAME,
    'MediaAccessControlAddress',
    DREAMCAST_TOOL_DEFAULT_MEDIA_ACCESS_CONTROL_ADDRESS
  );
  fHostMediaAccessControlAddress := IniFile.ReadString(
    CONFIG_DREAMCAST_TOOL_SECTION_NAME,
    'HostMediaAccessControlAddress',
    DREAMCAST_TOOL_DEFAULT_MEDIA_ACCESS_CONTROL_ADDRESS
  );
  fSerialDumbTerminal := IniFile.ReadBool(
    CONFIG_DREAMCAST_TOOL_SECTION_NAME,
    'SerialDumbTerminal',
    DREAMCAST_TOOL_DEFAULT_SERIAL_DUMB_TERMINAL
  );
  fSerialExternalClock := IniFile.ReadBool(
    CONFIG_DREAMCAST_TOOL_SECTION_NAME,
    'SerialExternalClock',
    DREAMCAST_TOOL_DEFAULT_SERIAL_EXTERNAL_CLOCK
  );
  fSerialBaudrate := TDreamcastToolSerialBaudrate(IniFile.ReadInteger(
    CONFIG_DREAMCAST_TOOL_SECTION_NAME,
    'SerialBaudrate',
    DREAMCAST_TOOL_DEFAULT_SERIAL_BAUDRATE
  ));
  fSerialBaudrateAlternate := IniFile.ReadBool(
    CONFIG_DREAMCAST_TOOL_SECTION_NAME,
    'SerialBaudrateAlternate',
    DREAMCAST_TOOL_DEFAULT_SERIAL_BAUDRATE_ALTERNATE
  );
  fSerialPort := TDreamcastToolSerialPort(IniFile.ReadInteger(
    CONFIG_DREAMCAST_TOOL_SECTION_NAME,
    'SerialPort',
    DREAMCAST_TOOL_DEFAULT_SERIAL_PORT
  ));
  fCustomExecutable := IniFile.ReadString(
    CONFIG_DREAMCAST_TOOL_SECTION_NAME,
    'CustomExecutable',
    DREAMCAST_TOOL_DEFAULT_CUSTOM_EXECUTABLE
  );
  fCustomArguments := IniFile.ReadString(
    CONFIG_DREAMCAST_TOOL_SECTION_NAME,
    'CustomArguments',
    DREAMCAST_TOOL_DEFAULT_CUSTOM_ARGUMENTS
  );
end;

procedure TDreamcastSoftwareDevelopmentSettingsDreamcastTool.SaveConfiguration(
  IniFile: TIniFile);
begin
  IniFile.WriteInteger(
    CONFIG_DREAMCAST_TOOL_SECTION_NAME,
    'Kind',
    Integer(fDreamcastToolKind)
  );
  IniFile.WriteBool(
    CONFIG_DREAMCAST_TOOL_SECTION_NAME,
    'AttachConsoleFileserver',
    fAttachConsoleFileserver
  );
  IniFile.WriteBool(
    CONFIG_DREAMCAST_TOOL_SECTION_NAME,
    'ClearScreenBeforeDownload',
    fClearScreenBeforeDownload
  );
  IniFile.WriteString(
    CONFIG_DREAMCAST_TOOL_SECTION_NAME,
    'InternetProtocolAddress',
    fInternetProtocolAddress
  );
  IniFile.WriteBool(
    CONFIG_DREAMCAST_TOOL_SECTION_NAME,
    'MediaAccessControlEnabled',
    fMediaAccessControlEnabled
  );
  IniFile.WriteString(
    CONFIG_DREAMCAST_TOOL_SECTION_NAME,
    'MediaAccessControlAddress',
    fMediaAccessControlAddress
  );
  IniFile.WriteString(
    CONFIG_DREAMCAST_TOOL_SECTION_NAME,
    'HostMediaAccessControlAddress',
    fHostMediaAccessControlAddress
  );
  IniFile.WriteBool(
    CONFIG_DREAMCAST_TOOL_SECTION_NAME,
    'SerialDumbTerminal',
    fSerialDumbTerminal
  );
  IniFile.WriteBool(
    CONFIG_DREAMCAST_TOOL_SECTION_NAME,
    'SerialExternalClock',
    fSerialExternalClock
  );
  IniFile.WriteInteger(
    CONFIG_DREAMCAST_TOOL_SECTION_NAME,
    'SerialBaudrate',
    Integer(fSerialBaudrate)
  );
  IniFile.WriteBool(
    CONFIG_DREAMCAST_TOOL_SECTION_NAME,
    'SerialBaudrateAlternate',
    fSerialBaudrateAlternate
  );
  IniFile.WriteInteger(
    CONFIG_DREAMCAST_TOOL_SECTION_NAME,
    'SerialPort',
    Integer(fSerialPort)
  );
  IniFile.WriteString(
    CONFIG_DREAMCAST_TOOL_SECTION_NAME,
    'CustomExecutable',
    fCustomExecutable
  );
  IniFile.WriteString(
    CONFIG_DREAMCAST_TOOL_SECTION_NAME,
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
  fRepositories := TDreamcastSoftwareDevelopmentSettingsRepositories.Create;
  fDreamcastTool := TDreamcastSoftwareDevelopmentSettingsDreamcastTool.Create;
end;

destructor TDreamcastSoftwareDevelopmentSettings.Destroy;
begin
  fRepositories.Free;
  fDreamcastTool.Free;
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
    DefaultInstallationPath := ExpandFileName(GetApplicationPath + '..\..\..\..\');
    fInstallPath := IncludeTrailingPathDelimiter(
      IniFile.ReadString(
        CONFIG_SETTINGS_SECTION_NAME,
        'InstallPath',
        DefaultInstallationPath
      )
    );

    fUseMintty := IniFile.ReadBool(
      CONFIG_SETTINGS_SECTION_NAME,
      'UseMinTTY',
      False
    );

    fProgressWindowAutoClose := IniFile.ReadBool(
      CONFIG_SETTINGS_SECTION_NAME,
      'ProgressWindowAutoClose',
      True
    );

    DreamcastTool.LoadConfiguration(IniFile);

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
      CONFIG_SETTINGS_SECTION_NAME,
      'InstallPath',
      fInstallPath
    );
    IniFile.WriteBool(
      CONFIG_SETTINGS_SECTION_NAME,
      'UseMinTTY',
      fUseMintty
    );
    IniFile.WriteBool(
      CONFIG_SETTINGS_SECTION_NAME,
      'ProgressWindowAutoClose',
      fProgressWindowAutoClose
    );

    DreamcastTool.SaveConfiguration(IniFile);
  finally
    IniFile.Free;
  end;
end;

end.

