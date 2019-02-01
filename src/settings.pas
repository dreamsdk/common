unit Settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

const
  SETTINGS_FILE_NAME = 'dreamsdk.conf';
  SETTINGS_DIRECTORY = '/etc/dreamsdk/';

  // Repositories
  DEFAULT_KALLISTI_URL = 'https://git.code.sf.net/p/cadcdev/kallistios';
  DEFAULT_KALLISTI_PORTS_URL = 'https://git.code.sf.net/p/cadcdev/kos-ports';
  DEFAULT_DREAMCAST_TOOL_SERIAL_URL = 'https://github.com/dreamsdk/dcload-serial.git';
  DEFAULT_DREAMCAST_TOOL_INTERNET_PROTOCOL_URL = 'https://github.com/dreamsdk/dcload-ip.git';

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
  protected
    procedure LoadConfiguration(IniFile: TIniFile);
    procedure SaveConfiguration(IniFile: TIniFile);
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

  { TDreamcastSoftwareDevelopmentSettings }
  TDreamcastSoftwareDevelopmentSettings = class(TObject)
  private
    fConfigurationFileName: TFileName;
    fInstallPath: TFileName;
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
    property Repositories: TDreamcastSoftwareDevelopmentSettingsRepositories
      read fRepositories;
  end;

function GetConfigurationDirectory: TFileName;
function SerialPortToString(SerialPort: TDreamcastToolSerialPort): string;
function SerialBaudrateToString(SerialBaudrate: TDreamcastToolSerialBaudrate): string;

implementation

uses
  SysTools;

const
  CONFIG_SETTINGS_SECTION_NAME = 'Settings';
  CONFIG_DREAMCAST_TOOL_SECTION_NAME = 'DreamcastTool';
  CONFIG_REPOSITORIES_SECTION_NAME = 'Repositories';

function GetConfigurationDirectory: TFileName;
begin
  Result := IncludeTrailingPathDelimiter(GetApplicationPath + '..\..\'
    + UnixPathToSystem(SETTINGS_DIRECTORY));
end;

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

{ TDreamcastSoftwareDevelopmentSettingsRepositories }

procedure TDreamcastSoftwareDevelopmentSettingsRepositories.LoadConfiguration(
  IniFile: TIniFile);
begin
  fKallistiURL := IniFile.ReadString(
    CONFIG_REPOSITORIES_SECTION_NAME,
    'KallistiOS',
    DEFAULT_KALLISTI_URL
  );
  fKallistiPortsURL := IniFile.ReadString(
    CONFIG_REPOSITORIES_SECTION_NAME,
    'KallistiPorts',
    DEFAULT_KALLISTI_PORTS_URL
  );
  fDreamcastToolSerialURL := IniFile.ReadString(
    CONFIG_REPOSITORIES_SECTION_NAME,
    'DreamcastToolSerial',
    DEFAULT_DREAMCAST_TOOL_SERIAL_URL
  );
  fDreamcastToolInternetProtocolURL := IniFile.ReadString(
    CONFIG_REPOSITORIES_SECTION_NAME,
    'DreamcastToolInternetProtocol',
    DEFAULT_DREAMCAST_TOOL_INTERNET_PROTOCOL_URL
  );
end;

procedure TDreamcastSoftwareDevelopmentSettingsRepositories.SaveConfiguration(
  IniFile: TIniFile);
begin
  IniFile.WriteString(
    CONFIG_REPOSITORIES_SECTION_NAME,
    'KallistiOS',
    fKallistiURL
  );
  IniFile.WriteString(
    CONFIG_REPOSITORIES_SECTION_NAME,
    'KallistiPorts',
    fKallistiPortsURL
  );
  IniFile.WriteString(
    CONFIG_REPOSITORIES_SECTION_NAME,
    'DreamcastToolSerial',
    fDreamcastToolSerialURL
  );
  IniFile.WriteString(
    CONFIG_REPOSITORIES_SECTION_NAME,
    'DreamcastToolInternetProtocol',
    fDreamcastToolInternetProtocolURL
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

    DreamcastTool.LoadConfiguration(IniFile);
    Repositories.LoadConfiguration(IniFile);

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

    DreamcastTool.SaveConfiguration(IniFile);
    Repositories.SaveConfiguration(IniFile);
  finally
    IniFile.Free;
  end;
end;

end.

