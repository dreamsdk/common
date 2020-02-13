unit Runner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Settings, RunCmd;

const
  UNKNOWN_EXIT_CODE = -1;

type
  { TDreamcastSoftwareDevelopmentKitRunner }
  TDreamcastSoftwareDevelopmentKitRunner = class(TObject)
  private
    fShellRunnerClientExitCodeTempFileName: TFileName;
    fShellCommand: TRunCommand;
    fExecutableMinTTY: TFileName;
    fExecutableShell: TFileName;
    fEnvironmentVariables: TStringList;
    fSettings: TDreamcastSoftwareDevelopmentSettings;
    fWorkingDirectory: TFileName;
    procedure InitializeEnvironment;
    function GetHealthy: Boolean;
    procedure RetrieveEnvironmentVariables;
    procedure HandleNewLine(Sender: TObject; NewLine: string);
    procedure HandleTerminate(Sender: TObject);
    procedure SetWorkingDirectory(AValue: TFileName);
  protected
    function GetClientExitCode: Integer;
    property Settings: TDreamcastSoftwareDevelopmentSettings read fSettings;
  public
    constructor Create;
    destructor Destroy; override;
    function CheckHealty: Boolean;
    procedure StartShell;
    function StartShellCommand(const CommandLine: string): Integer;
    property Healthy: Boolean read GetHealthy;
    property WorkingDirectory: TFileName read fWorkingDirectory write SetWorkingDirectory;
  end;

implementation

uses
  Version,
{$IFDEF Windows}
  Windows,
{$ENDIF}
  SysTools,
  Process,
{$IF Defined(Unix) OR Defined(Darwin)}
  , UTF8Process,
{$ENDIF}
  FSTools;

resourcestring
  MSYSShellNotFound             = 'MinGW/MSYS is not properly installed.';
  ErrorTitle                    = 'Error';

{ TDreamcastSoftwareDevelopmentKitRunner }

procedure TDreamcastSoftwareDevelopmentKitRunner.InitializeEnvironment;
const
  BINARY_DIRECTORY = 'msys\1.0\bin\';

begin
  fExecutableMinTTY := Settings.InstallPath + BINARY_DIRECTORY + 'mintty.exe';
  fExecutableShell := Settings.InstallPath + BINARY_DIRECTORY + 'sh.exe';
end;

function TDreamcastSoftwareDevelopmentKitRunner.GetHealthy: Boolean;
begin
  Result := FileExists(fExecutableShell)
    and FileExists(fExecutableMinTTY);
end;

procedure TDreamcastSoftwareDevelopmentKitRunner.HandleNewLine(Sender: TObject;
  NewLine: string);
begin
  WriteLn(NewLine);
end;

procedure TDreamcastSoftwareDevelopmentKitRunner.HandleTerminate(Sender: TObject);
begin
{$IFDEF DEBUG}
  WriteLn('*** END ***');
{$ENDIF}
end;

procedure TDreamcastSoftwareDevelopmentKitRunner.SetWorkingDirectory(
  AValue: TFileName);
var
  Temp: TFileName;

begin
  if fWorkingDirectory <> AValue then
  begin
    Temp := IncludeTrailingPathDelimiter(ExpandFileName( ExpandEnvironmentStrings( Trim( AValue ) ) ));
    if DirectoryExists(Temp) then
      fWorkingDirectory := Temp;
  end;
end;

function TDreamcastSoftwareDevelopmentKitRunner.GetClientExitCode: Integer;
var
  Buffer: TStringList;

begin
  Result := UNKNOWN_EXIT_CODE;
  if FileExists(fShellRunnerClientExitCodeTempFileName) then
  begin
    Buffer := TStringList.Create;
    try
      Buffer.LoadFromFile(fShellRunnerClientExitCodeTempFileName);
      Result := StrToIntDef(Trim(Buffer.Text), UNKNOWN_EXIT_CODE);
    finally
      Buffer.Free;
      SysUtils.DeleteFile(fShellRunnerClientExitCodeTempFileName);
    end;
  end;
end;

function TDreamcastSoftwareDevelopmentKitRunner.StartShellCommand(
  const CommandLine: string): Integer;
var
  ClientExitCodeUnixFileName: TFileName;

begin
{$IFDEF DEBUG}
  WriteLn('CommandLine: ', CommandLine);
{$ENDIF}

  Result := UNKNOWN_EXIT_CODE;
  fShellRunnerClientExitCodeTempFileName := GetTemporaryFileName;

  FreeAndNil(fShellCommand);
  fShellCommand := TRunCommand.Create(True);

  // Handle working directory
  if (WorkingDirectory <> EmptyStr) then
  begin
    fShellCommand.WorkingDirectory := WorkingDirectory;
  end;

  fShellCommand.Executable := fExecutableShell;
  fShellCommand.Parameters.Add('--login');
  fShellCommand.Parameters.Add('-i');

  ClientExitCodeUnixFileName := SystemToUnixPath(fShellRunnerClientExitCodeTempFileName);
  with fShellCommand.Environment do
  begin
    Add('_EXTERNAL_COMMAND=' + CommandLine);
    Add('_EXITCODE=' + ClientExitCodeUnixFileName);
  end;

//  SetConsoleTitle(PChar(GetFileDescription));

  fShellCommand.OnNewLine := @HandleNewLine;
  fShellCommand.OnTerminate := @HandleTerminate;

  fShellCommand.Start;

{$IFDEF DEBUG}
  WriteLn('WaitFor is starting...');
{$ENDIF}

  fShellCommand.WaitFor;

{$IFDEF DEBUG}
  WriteLn('WaitFor is done!');
{$ENDIF}

  if (fShellCommand.ExitCode = 0) then
    Result := GetClientExitCode;
end;

procedure TDreamcastSoftwareDevelopmentKitRunner.StartShell;
var
  OurProcess: {$IFDEF Windows}TProcess{$ELSE}TProcessUTF8{$ENDIF};

begin
  RetrieveEnvironmentVariables;

  OurProcess := {$IFDEF Windows}TProcess{$ELSE}TProcessUTF8{$ENDIF}.Create(nil);
  try
    // Initialize Environment context
    HandleLogonServerVariable(fEnvironmentVariables);
    OurProcess.Environment.AddStrings(fEnvironmentVariables);

    // Handle working directory
    if WorkingDirectory <> EmptyStr then
    begin
      OurProcess.CurrentDirectory := WorkingDirectory;
      OurProcess.Environment.Add('_WORKING_DIRECTORY=' + WorkingDirectory);
    end;

    // Extracted from msys.bat
    if Settings.UseMinTTY then
    begin
      OurProcess.Executable := fExecutableMinTTY;
      OurProcess.Parameters.Add('/bin/bash');
      OurProcess.Parameters.Add('-l');
    end
    else
    begin
      OurProcess.Executable := fExecutableShell;
      OurProcess.Parameters.Add('--login');
      OurProcess.Parameters.Add('-i');
    end;

    // Execute our process
    OurProcess.Execute;
  finally
    OurProcess.Free;
  end;
end;

procedure TDreamcastSoftwareDevelopmentKitRunner.RetrieveEnvironmentVariables;
var
  i: Integer;

begin
  for i := 1 to GetEnvironmentVariableCount do
    fEnvironmentVariables.Add(GetEnvironmentString(i));
end;

constructor TDreamcastSoftwareDevelopmentKitRunner.Create;
begin
  fEnvironmentVariables := TStringList.Create;
  fSettings := TDreamcastSoftwareDevelopmentSettings.Create;
  Settings.LoadConfiguration;
  InitializeEnvironment;
end;

destructor TDreamcastSoftwareDevelopmentKitRunner.Destroy;
begin
  FreeAndNil(fShellCommand);
  fEnvironmentVariables.Free;
  fSettings.Free;
  inherited Destroy;
end;

function TDreamcastSoftwareDevelopmentKitRunner.CheckHealty: Boolean;
begin
  Result := Healthy;
  if not Healthy then
    MessageBox(0, PChar(MSYSShellNotFound), PChar(ErrorTitle), MB_ICONERROR);
end;

end.

