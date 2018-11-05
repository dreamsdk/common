unit Settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

const
  SETTINGS_FILE_NAME = 'dreamsdk.conf';
  SETTINGS_DIRECTORY = '/etc/dreamsdk/';
  SETTINGS_FULL_FILE_PATH = SETTINGS_DIRECTORY + SETTINGS_FILE_NAME;

  DEFAULT_KALLISTI_URL = 'https://git.code.sf.net/p/cadcdev/kallistios';
  DEFAULT_KALLISTI_PORTS_URL = 'https://git.code.sf.net/p/cadcdev/kos-ports';
  DEFAULT_DREAMCAST_TOOL_SERIAL_URL = 'https://github.com/sizious/dcload-serial.git';
  DEFAULT_DREAMCAST_TOOL_INTERNET_PROTOCOL_URL = 'https://github.com/sizious/dcload-ip.git';

type
  TDreamcastToolKind = (
    dtkUndefined,
    dtkSerial,
    dtkInternetProtocol
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
    fAlwaysStartDebugger: Boolean;
    fAttachConsoleFileserver: Boolean;
    fClearScreenBeforeDownload: Boolean;
    fDreamcastToolKind: TDreamcastToolKind;
    fInternetProtocolAddress: string;
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
    property AlwaysStartDebugger: Boolean
      read fAlwaysStartDebugger write fAlwaysStartDebugger;
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
    fApplicationPath: TFileName;
    fConfigurationFileName: TFileName;
    fInstallPath: TFileName;
    fRepositories: TDreamcastSoftwareDevelopmentSettingsRepositories;
    fUseMinTTY: Boolean;
    fDreamcastTool: TDreamcastSoftwareDevelopmentSettingsDreamcastTool;
  protected
    function GetApplicationPath: TFileName;
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

function SerialPortToString(SerialPort: TDreamcastToolSerialPort): string;
function SerialBaudrateToString(SerialBaudrate: TDreamcastToolSerialBaudrate): string;

implementation

const
  CONFIG_SETTINGS_SECTION_NAME = 'Settings';
  CONFIG_DREAMCAST_TOOL_SECTION_NAME = 'DreamcastTool';
  CONFIG_REPOSITORIES_SECTION_NAME = 'Repositories';

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
    0
  ));
  fAlwaysStartDebugger := IniFile.ReadBool(
    CONFIG_DREAMCAST_TOOL_SECTION_NAME,
    'AlwaysStartDebugger',
    False
  );
  fAttachConsoleFileserver := IniFile.ReadBool(
    CONFIG_DREAMCAST_TOOL_SECTION_NAME,
    'AttachConsoleFileserver',
    True
  );
  fClearScreenBeforeDownload := IniFile.ReadBool(
    CONFIG_DREAMCAST_TOOL_SECTION_NAME,
    'ClearScreenBeforeDownload',
    True
  );
  fInternetProtocolAddress := IniFile.ReadString(
    CONFIG_DREAMCAST_TOOL_SECTION_NAME,
    'InternetProtocolAddress',
    '000.000.000.000'
  );
  fSerialDumbTerminal := IniFile.ReadBool(
    CONFIG_DREAMCAST_TOOL_SECTION_NAME,
    'SerialDumbTerminal',
    False
  );
  fSerialExternalClock := IniFile.ReadBool(
    CONFIG_DREAMCAST_TOOL_SECTION_NAME,
    'SerialExternalClock',
    False
  );
  fSerialBaudrate := TDreamcastToolSerialBaudrate(IniFile.ReadInteger(
    CONFIG_DREAMCAST_TOOL_SECTION_NAME,
    'SerialBaudrate',
    0
  ));
  fSerialBaudrateAlternate := IniFile.ReadBool(
    CONFIG_DREAMCAST_TOOL_SECTION_NAME,
    'SerialBaudrateAlternate',
    False
  );
  fSerialPort := TDreamcastToolSerialPort(IniFile.ReadInteger(
    CONFIG_DREAMCAST_TOOL_SECTION_NAME,
    'SerialPort',
    0
  ));
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
    'AlwaysStartDebugger',
    fAlwaysStartDebugger
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

function TDreamcastSoftwareDevelopmentSettings.GetApplicationPath: TFileName;
var
  Path: TFileName;
{$IFDEF Darwin}
  i: Integer;
{$ENDIF}

begin
  if (fApplicationPath = '') then
  begin
    Path := ExtractFilePath(ParamStr(0));
{$IFDEF Darwin}
    i := Pos('.app', Path);
    if i > 0 then
    begin
      i := LastDelimiter('/', Copy(Path, 1, i));
      Path := Copy(Path, 1, i);
    end;
{$ENDIF}
    fApplicationPath := IncludeTrailingPathDelimiter(Path);
  end;
  Result := fApplicationPath;
end;

function TDreamcastSoftwareDevelopmentSettings.GetConfigurationFileName: TFileName;
begin
  if (fConfigurationFileName = '') then
  begin
    fConfigurationFileName := GetApplicationPath + SETTINGS_FULL_FILE_PATH;
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

