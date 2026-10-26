unit PortMgr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, SysTools, Environ, IDEMgr, GetVer, Settings;

type
  TLibraryLanguageKind = (llkAll, llkC, llkCPP);
  TOverridenInformationKind = (oikLibrary, oikInclude, oikIncludeDirectory);
  TKallistiPortHostTool = (phtCMake, phtRuby, phtPython);
  TKallistiPortHostTools = set of TKallistiPortHostTool;

  TKallistiPortManager = class;

  { TKallistiPortItem }
  TKallistiPortItem = class(TObject)
  private
    fIncludeDirectory: TFileName;
    fOwner: TKallistiPortManager;
    fUsableWithinIDE: Boolean;
    fRequiredHostTools: TKallistiPortHostTools;
    fVirtualAddon: Boolean;
    fListIndex: Integer;
    fSourceDirectory: TFileName;
    fInstallDirectory: TFileName;
    fDescription: string;
    fLicense: string;
    fMaintainer: string;
    fName: string;
    fShortDescription: string;
    fURL: string;
    fVersion: string;
    fSelfIncludes: string;
    fSelfLibraries: string;
    fDeclaredDependenciesNameList: TStringList;
    fFullComputedDependenciesNameList: TStringList;
    fFullComputedDependenciesLibraries: string;
    fFullComputedDependenciesLibraryWeights: string;
    fLibraryLanguageKind: TLibraryLanguageKind;
    function GetEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
    function GetLibraries: string;
    function GetMissingHostTools: TKallistiPortHostTools;
    function IsPortInstalled: Boolean;
    function DeleteInstallPortDirectoryIfNeeded: Boolean;
  protected
    procedure DetectUsability;
    function DoInstallOrUpdate: string;
    function ExecuteShellCommand(const CommandLine: string): string;
    function IsShellCommandOK(const SuccessTag, BufferOutput: string): Boolean;
    property Environment: TDreamcastSoftwareDevelopmentEnvironment
      read GetEnvironment;
  public
    constructor Create(AOwner: TKallistiPortManager);
    destructor Destroy; override;

    function Install(var BufferOutput: string): Boolean;
    function Uninstall(var BufferOutput: string): Boolean;
    function Update(var BufferOutput: string): TUpdateOperationState;

    property Name: string read fName;
    property Description: string read fDescription;
    property SourceDirectory: TFileName read fSourceDirectory;
    property Includes: string read fSelfIncludes;
    property IncludeDirectory: TFileName read fIncludeDirectory;
    property InstallDirectory: TFileName read fInstallDirectory;
    property Installed: Boolean read IsPortInstalled;
    property Maintainer: string read fMaintainer;
    property Libraries: string read GetLibraries;
    property License: string read fLicense;
    property ShortDescription: string read fShortDescription;
    property URL: string read fURL;
    property Version: string read fVersion;
    property Hidden: Boolean read fVirtualAddon;
    property LibraryWeights: string read fFullComputedDependenciesLibraryWeights;
    property RequiredHostTools: TKallistiPortHostTools read fRequiredHostTools;
    property MissingHostTools: TKallistiPortHostTools read GetMissingHostTools;
    property UsableWithinIDE: Boolean read fUsableWithinIDE;
    property LibraryLanguageKind: TLibraryLanguageKind read fLibraryLanguageKind;
  end;

  { TKallistiPortManager }
  TKallistiPortManager = class(TObject)
  private
    fComponentVersion: TComponentVersion;
    fOnlyVisibleListCount: Integer;
    fList: TList;
    fPortsMap: TStringIntegerMap;
    fPortsWithDependencies: TIntegerList;
    fPortsWithoutDependencies: TIntegerList;
    fEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
    fIntegratedDevelopmentEnvironment: TIntegratedDevelopmentEnvironment;
    fKallistiLibraryInformation: TIniFile;
    fProcessDependenciesCurrentPortInfo: TKallistiPortItem;
    fProcessDependenciesCallCount: LongWord;
    fAddonsId: TStringList;
    fAddonsIncludes: TStringList;
    fAddonsLibraries: TStringList;
    fRepository: TDreamcastSoftwareDevelopmentRepository;
    fRepositoryOffline: Boolean;
    fRepositoryOfflineVersion: string;
    function GetCount: Integer;
    function GetCountInstalled: Integer;
    function GetCountVisibleInstalled: Integer;
    function GetInstalled: Boolean;
    function GetItem(Index: Integer): TKallistiPortItem;
    function Add: TKallistiPortItem;
    procedure Clear;
    function GetInstalledPorts(const VisibleOnly: Boolean): Integer;
    function GetRepositoryReady: Boolean;
    function GetRepositoryVersion: string;
    function GetUtilityDirectory: TFileName;
    procedure ProcessPort(const PortDirectoryBaseName: TFileName);
    procedure ProcessPortsDependencies;
    procedure ProcessPortsWithoutDependencies;
    function GetPortLibrary(const PortName: string): string;
    function GetPortWeight(const PortName: string): string;
    function GetPortLanguageKind(const PortName: string): TLibraryLanguageKind;
    function GetOverridenInformation(InformationKind: TOverridenInformationKind;
      PortName: string): string;
    procedure ProcessDependenciesInitialize(PortInfo: TKallistiPortItem);
    function ProcessPortDependencies(PortInfo: TKallistiPortItem): string;
    function GenerateIncludeHeader(const IncludeFiles: string;
      const IncludeDirectory: TFileName; const IsOverriden: Boolean): string;
    function GetPortIndex(const PortName: string): Integer;
    procedure HandleLibrary(OutputBuffer: TStringList; PortName: string);
    procedure SetPortAdditionalInformation(PortInfo: TKallistiPortItem;
      ProcessDependencies: Boolean);
  protected
    procedure GenerateIntegratedDevelopmentEnvironmentLibraryInformation(
      const LibraryLanguageKind: TLibraryLanguageKind);
    procedure GenerateLibraryInformationFile;
    function GetAddonIncludes(const AddonIndex: Integer): string;
    function GetAddonIndex(const AddonName: string): Integer;
    procedure RetrieveAvailableAddons;
    property UtilityDirectory: TFileName read GetUtilityDirectory;
    property Environment: TDreamcastSoftwareDevelopmentEnvironment
      read fEnvironment;
    property Versions: TComponentVersion read fComponentVersion;
    property IntegratedDevelopmentEnvironment: TIntegratedDevelopmentEnvironment
      read fIntegratedDevelopmentEnvironment;
  public
    constructor Create(AEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
      AIntegratedDevelopmentEnvironment: TIntegratedDevelopmentEnvironment;
      AComponentVersion: TComponentVersion;
      const AutoLoad: Boolean);
    destructor Destroy; override;
    function CloneRepository(var BufferOutput: string): Boolean;
    procedure GenerateIntegratedDevelopmentEnvironmentLibraryInformation;
    function UpdateRepository(var BufferOutput: string): TUpdateOperationState;
    function InitializeEnvironment: Boolean;
    function Install(var OutputBuffer: string): Boolean;
    function Uninstall(var OutputBuffer: string): Boolean;
    procedure RetrieveAvailablePorts;

    property Count: Integer read GetCount;
    property CountInstalled: Integer read GetCountInstalled;
    property CountVisible: Integer read fOnlyVisibleListCount;
    property CountVisibleInstalled: Integer read GetCountVisibleInstalled;
    property Installed: Boolean read GetInstalled;
    property Items[Index: Integer]: TKallistiPortItem read GetItem; default;
    property Repository: TDreamcastSoftwareDevelopmentRepository
      read fRepository;
  end;

function HostToolsToString(const HostTools: TKallistiPortHostTools): string;

implementation

uses
  FileUtil,
  FPJson,
  FSTools,
  StrTools,
  Version;

const
  KALLISTI_PORTS_PACKAGE_DESCRIPTION = 'pkg-descr';
  KALLISTI_PORTS_DIRECTORY = '/opt/toolchains/dc/kos-ports/';
  KALLISTI_PORTS_UTILS_DIRECTORY = KALLISTI_PORTS_DIRECTORY + 'utils/';
  KALLISTI_PORTS_ALL_BUILD = KALLISTI_PORTS_UTILS_DIRECTORY + 'build-all.sh';
  KALLISTI_PORTS_ALL_CLEAN = KALLISTI_PORTS_UTILS_DIRECTORY + 'clean-all.sh';
  KALLISTI_PORTS_ALL_UNINSTALL = KALLISTI_PORTS_UTILS_DIRECTORY + 'uninstall-all.sh';
  KALLISTI_PORTS_RESET = DREAMSDK_MSYS_INSTALL_HELPERS_DIRECTORY + 'kos-ports-reset';

  PROCESS_DEPENDENCIES_MAX_DEPTH = 128;
  PROCESS_DEPENDENCIES_CIRCULAR_REFERENCE_ERROR = '#CIRCULAR_REFERENCE_ERROR#';

function HostToolsToString(const HostTools: TKallistiPortHostTools): string;
const
  HOST_TOOL_NAMES: array[TKallistiPortHostTool] of string = (
    'CMake', 'Ruby', 'Python'
  );

var
  HostTool: TKallistiPortHostTool;

begin
  Result := EmptyStr;
  for HostTool in HostTools do
  begin
    if not IsEmpty(Result) then
      Result := Result + ', ';
    Result := Result + HOST_TOOL_NAMES[HostTool];
  end;
end;

// Host tools needed by some ports, as they aren't declared in the port Makefile
function GetPortExtraHostTools(const PortName: string): TKallistiPortHostTools;
begin
  Result := [];
  if SameText(PortName, 'mruby') then
    Result := [phtRuby]
  else if SameText(PortName, 'micropython') then
    Result := [phtPython];
end;

{ TKallistiPortItem }

function TKallistiPortItem.IsPortInstalled: Boolean;
begin
  Result := FileExists(fSourceDirectory + '..\lib\.kos-ports\' + Name)
    or fVirtualAddon;
end;

function TKallistiPortItem.DeleteInstallPortDirectoryIfNeeded: Boolean;
var
  ExamplePortDirectory: TFileName;

begin
  Result := True; // By default, everything is OK

  // Clean-up the kos-ports/lib/<lib> directory if needed
  if not IsEmpty(InstallDirectory) and DirectoryExists(InstallDirectory) then
    Result := KillDirectory(InstallDirectory);

  // Clean-up the kos-ports/examples/<lib> directory if needed
  ExamplePortDirectory := Environment.FileSystem.Kallisti.KallistiPortsDirectory
    + 'examples' + DirectorySeparator + Name + DirectorySeparator;
  KillDirectory(ExamplePortDirectory);
end;

procedure TKallistiPortItem.DetectUsability;
begin
  // For getting a port to work: Name, Includes, Libraries and LibraryWeights should be set
  fUsableWithinIDE := (not IsEmpty(Name)) and (not IsEmpty(Includes))
    and (not IsEmpty(Libraries)) and (not IsEmpty(LibraryWeights));
end;

function TKallistiPortItem.DoInstallOrUpdate: string;
var
  InstallExtraParameters: string;

begin
  fOwner.InitializeEnvironment;
  InstallExtraParameters := fOwner.fKallistiLibraryInformation
    .ReadString('Install', fName, EmptyStr);
  Result := ExecuteShellCommand(Format('make install %s', [InstallExtraParameters]));
  ExecuteShellCommand('make clean');
end;

function TKallistiPortItem.ExecuteShellCommand(const CommandLine: string): string;
begin
  Result := Environment.ExecuteShellCommand(CommandLine, SourceDirectory);
end;

function TKallistiPortItem.IsShellCommandOK(const SuccessTag,
  BufferOutput: string): Boolean;
begin
  Result := IsInString(SuccessTag, BufferOutput);
//    and (not Environment.ShellCommandError);
end;

function TKallistiPortItem.GetEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
begin
  Result := fOwner.fEnvironment;
end;

function TKallistiPortItem.GetLibraries: string;
begin
  // Use the sorted list whenever it was computed, so Libraries and
  // LibraryWeights are always in the same order (even without dependencies)
  Result := fSelfLibraries;
  if not IsEmpty(fFullComputedDependenciesLibraries) then
    Result := fFullComputedDependenciesLibraries;
end;

function TKallistiPortItem.GetMissingHostTools: TKallistiPortHostTools;
begin
  Result := [];
  with fOwner.Versions do
  begin
    if (phtCMake in fRequiredHostTools) and (not CMakeInstalled) then
      Include(Result, phtCMake);
    if (phtRuby in fRequiredHostTools) and (not RubyInstalled) then
      Include(Result, phtRuby);
    if (phtPython in fRequiredHostTools) and (not PythonInstalled) then
      Include(Result, phtPython);
  end;
end;

constructor TKallistiPortItem.Create(AOwner: TKallistiPortManager);
begin
  fOwner := AOwner;
  fDeclaredDependenciesNameList := TStringList.Create;
  fFullComputedDependenciesNameList := TStringList.Create;
end;

destructor TKallistiPortItem.Destroy;
begin
  fDeclaredDependenciesNameList.Free;
  fFullComputedDependenciesNameList.Free;
  inherited Destroy;
end;

function TKallistiPortItem.Install(var BufferOutput: string): Boolean;
const
  SUCCESS_TAG = 'Marking %s %s as installed.';

var
  SuccessTag: string;

begin
  DeleteInstallPortDirectoryIfNeeded;
  SuccessTag := Format(SUCCESS_TAG, [Name, Version]);
  BufferOutput := DoInstallOrUpdate;
  Result := IsShellCommandOK(SuccessTag, BufferOutput);
end;

function TKallistiPortItem.Uninstall(var BufferOutput: string): Boolean;
const
  SUCCESS_TAG = 'Uninstalled %s.';

var
  SuccessTag: string;

begin
  SuccessTag := Format(SUCCESS_TAG, [Name]);
  BufferOutput := ExecuteShellCommand('make uninstall');
  Result := IsShellCommandOK(SuccessTag, BufferOutput);
  DeleteInstallPortDirectoryIfNeeded;
end;

function TKallistiPortItem.Update(var BufferOutput: string): TUpdateOperationState;
const
  USELESS_TAG = '%s is already installed';
  SUCCESS_TAG = 'Marking %s %s as installed.';

var
  SuccessTag,
  UselessTag: string;

begin
  Result := uosUndefined;

  SuccessTag := Format(SUCCESS_TAG, [Name, Version]);
  UselessTag := Format(USELESS_TAG, [Name]);

  BufferOutput := DoInstallOrUpdate;
  if IsShellCommandOK(SuccessTag, BufferOutput) then
    Result := uosUpdateSuccess
  else
    if IsShellCommandOK(UselessTag, BufferOutput) then
      Result := uosUpdateUseless
    else
      Result := uosUpdateFailed;
end;

{ TKallistiPortManager }

procedure TKallistiPortManager.RetrieveAvailablePorts;
var
  PortsAvailable: TStringList;
  i: Integer;

begin
  Clear;
  RetrieveAvailableAddons;

  PortsAvailable := TStringList.Create;
  try
    FindAllFiles(PortsAvailable, Environment.FileSystem.Kallisti.KallistiPortsDirectory,
      KALLISTI_PORTS_PACKAGE_DESCRIPTION, True);

    // Keeping only PortName information...
    for i := 0 to PortsAvailable.Count - 1 do
      PortsAvailable[i] :=
        ExtractStr(
          Environment.FileSystem.Kallisti.KallistiPortsDirectory,
          DirectorySeparator + KALLISTI_PORTS_PACKAGE_DESCRIPTION,
          PortsAvailable[i]
        );

    // Adding addons
    PortsAvailable.AddStrings(fAddonsId);

    // Sorting everything
    PortsAvailable.Sort;

{$IFDEF DEBUG}
    DebugLog(StringListToString(PortsAvailable, sLineBreak));
{$ENDIF}

    for i := 0 to PortsAvailable.Count - 1 do
      ProcessPort(PortsAvailable[i]);

    ProcessPortsDependencies;

    ProcessPortsWithoutDependencies;

    // Check if ports are usable
    for i := 0 to Count - 1 do
      Items[i].DetectUsability;
  finally
    PortsAvailable.Free;
  end;
end;

function TKallistiPortManager.GetItem(Index: Integer): TKallistiPortItem;
begin
  Result := TKallistiPortItem(fList[Index]);
end;

function TKallistiPortManager.GetCount: Integer;
begin
  Result := fList.Count;
end;

function TKallistiPortManager.GetCountInstalled: Integer;
begin
  Result := GetInstalledPorts(False);
end;

function TKallistiPortManager.GetCountVisibleInstalled: Integer;
begin
  Result := GetInstalledPorts(True);
end;

function TKallistiPortManager.GetInstalled: Boolean;
begin
  Result := Environment.IsComponentInstalled(Environment.FileSystem.Kallisti
    .KallistiPortsDirectory);
end;

function TKallistiPortManager.Add: TKallistiPortItem;
begin
  Result := TKallistiPortItem.Create(Self);
  Result.fListIndex := fList.Add(Result);
end;

procedure TKallistiPortManager.Clear;
var
  i: Integer;

begin
  for i := 0 to fList.Count - 1 do
    TKallistiPortItem(fList[i]).Free;
  fList.Clear;
  fPortsWithDependencies.Clear;
  fPortsWithoutDependencies.Clear;
  fPortsMap.Clear;
  fOnlyVisibleListCount := 0;
end;

function TKallistiPortManager.GetInstalledPorts(const VisibleOnly: Boolean): Integer;
var
  i: Integer;

begin
  Result := 0;
  for i := 0 to Count - 1 do
    if Items[i].Installed and (not VisibleOnly or (VisibleOnly and not Items[i].Hidden)) then
      Inc(Result);
end;

function TKallistiPortManager.GetRepositoryReady: Boolean;
begin
  Result := Environment.IsRepositoryReady(
    Environment.FileSystem.Kallisti.KallistiPortsDirectory);
end;

function TKallistiPortManager.GetRepositoryVersion: string;
begin
  Result := EmptyStr;
  if fRepositoryOffline then
    Result := fRepositoryOfflineVersion
  else
    Result := Environment.GetRepositoryVersion(
      Environment.FileSystem.Kallisti.KallistiPortsDirectory);
end;

function TKallistiPortManager.GetUtilityDirectory: TFileName;
begin
  Result := Environment.FileSystem.Kallisti.KallistiPortsDirectory + 'utils\';
end;

procedure TKallistiPortManager.ProcessPort(const PortDirectoryBaseName: TFileName);
const
  MAKEFILE_FILE_NAME = 'Makefile';

var
  MakefileContent: TStringList;
  PortName, MakefileContentText, ExtractedURL: string;
  PortDirectory, Makefile, PortDescriptionFileName: TFileName;
  AddonIndex: Integer;

  function GetPackageString(const Key: string): string;
  var
    S, Line: string;
    i: Integer;
    Buffer: TStringList;

  begin
    Result := EmptyStr;
    S := Right(Key, MakefileContentText);
    if S <> EmptyStr then
    begin
      // Handle simple case
      Result := Trim(ExtractStr('=', #10, S));

      // Handle multilines ('\')
      if EndsWith('\', Result) then
      begin
        Buffer := TStringList.Create;
        try
          Buffer.Text := Trim(Right(Result, MakefileContentText));
          Result := Trim(Left('\', Result));
          for i := 0 to Buffer.Count - 1 do
          begin
            Line := Buffer[i];
            if EndsWith('\', Line) then
              // There is another lines to proceed
              Result := Result + ' ' + Trim(Left('\', Line))
            else
            begin
              // This is the last line, so break
              Result := Result + ' ' + Trim(Line);
              Break;
            end;
          end;
        finally
          Buffer.Free;
        end;
      end;
    end;
  end;

  function SanitizeText(const Text: string): string;
  begin
    Result := Text;
    Result := StringReplace(Result, #13#10, #10, [rfReplaceAll]);
    Result := StringReplace(Result, #13, #10, [rfReplaceAll]);
    Result := StringReplace(Result, #9, '', [rfReplaceAll]);
  end;

  function GetPackageDescription(var URL: string): string;
  const
    URL_TAG = ' URL: ';

  var
    DescriptionContent: TStringList;

  begin
    Result := '';
    DescriptionContent := TStringList.Create;
    try
      PortDescriptionFileName := PortDirectory + KALLISTI_PORTS_PACKAGE_DESCRIPTION;
      if FileExists(PortDescriptionFileName) then
      begin
        DescriptionContent.LoadFromFile(PortDescriptionFileName);
        Result := SanitizeText(DescriptionContent.Text);
        Result := StringReplace(Result, #10, ' ', [rfReplaceAll]);
        Result := StringReplace(Result, '  ', ' ', [rfReplaceAll]);
        if IsInString(URL_TAG, Result) then
        begin
          URL := Trim(Right(URL_TAG, Result));
          Result := Left(URL_TAG, Result);
        end;
        Result := Trim(Result);
      end;
    finally
      DescriptionContent.Free;
    end;
  end;

  function GetInstallDirectory: TFileName;
  begin
    Result := GetPackageString('HDR_INSTDIR');
    if IsEmpty(Result) then
      Result := GetPackageString('HDR_COMDIR');
    if IsEmpty(Result) then
      Result := GetPackageString('HDR_FULLDIR');
    if IsEmpty(Result) then
      Result := PortName; // by default, the directory is the same as the PortName
    Result := PortDirectory + '..\include\' + Result;
  end;

  function GetPackageIncludes: string;
  var
    IncludeDirectory, IncludeFiles: string;
    IsOverriden: Boolean;

  begin
    Result := EmptyStr;

    IncludeDirectory := EmptyStr;
    IncludeFiles := GetOverridenInformation(oikInclude, PortName);
    IsOverriden := not SameText(IncludeFiles, EmptyStr);

    if not IsOverriden then
    begin
      // Get the include directory
      IncludeDirectory := GetPackageString('HDR_INSTDIR');
      if SameText(IncludeDirectory, EmptyStr) then
      begin
        IncludeDirectory := GetPackageString('HDR_COMDIR');
        if SameText(IncludeDirectory, EmptyStr) then
          IncludeDirectory := PortName;
      end;
      IncludeDirectory := IncludeDirectory + '/';

      // Get all includes files
      IncludeFiles := Trim(StringReplace(GetPackageString('INSTALLED_HDRS'),
        'include/' + IncludeDirectory, EmptyStr, [rfReplaceAll]));
      IncludeFiles := Trim(StringReplace(IncludeFiles, 'include/', EmptyStr,
        [rfReplaceAll]));
      IncludeFiles := Trim(StringReplace(IncludeFiles, 'src/', EmptyStr,
        [rfReplaceAll]));
    end;

    // Combine everything
    Result := GenerateIncludeHeader(IncludeFiles, IncludeDirectory,
      IsOverriden);
  end;

  function GetPackageIncludeDirectory: TFileName;
  begin
    Result := GetOverridenInformation(oikIncludeDirectory, PortName);
    if SameText(Result, EmptyStr) then
      Result := GetPackageString('HDR_FULLDIR');
  end;

  function GetPackageLibraries: string;
  var
    Targets: TStringList;
    i: Integer;
    LibraryName: string;

  begin
    Result := GetOverridenInformation(oikLibrary, PortName);
    if SameText(Result, EmptyStr) then
    begin
      // TARGET may declare several libraries (e.g. "libGL.a libGLU.a") and
      // may use the PORTNAME variable (e.g. "lib$(PORTNAME).a")
      Result := GetPackageString('TARGET');
      Result := StringReplace(Result, '$(PORTNAME)', PortName, [rfReplaceAll]);
      Result := StringReplace(Result, '${PORTNAME}', PortName, [rfReplaceAll]);

      Targets := TStringList.Create;
      try
        StringToStringList(SuppressUselessWhiteSpaces(Result), WhiteSpaceStr, Targets);
        for i := 0 to Targets.Count - 1 do
        begin
          // "libGL.a" => "GL"
          LibraryName := ChangeFileExt(Trim(Targets[i]), EmptyStr);
          if StartsWith('lib', LibraryName) then
            LibraryName := Copy(LibraryName, 4, Length(LibraryName));
          Targets[i] := LibraryName;
        end;
        Result := StringListToString(Targets, ArraySeparator);
      finally
        Targets.Free;
      end;
    end
    else
      Result := StringReplace(Trim(Result), ' ', ArraySeparator, [rfReplaceAll]);
  end;

  function GetPackageDependencies: string;
  begin
    Result := GetPackageString('DEPENDENCIES');
    if not SameText(Result, EmptyStr) then
      Result := StringReplace(SuppressUselessWhiteSpaces(Result), WhiteSpaceStr,
        sLineBreak, [rfReplaceAll]);
  end;

  function SanitizeMakefileContent(const RawMakefileContentText: string): string;
  const
    DUMMY_TAG = '_USELESS_';
    CLEANUP_TAGS: array[0..0] of string = (
      'NOCOPY_TARGET'
    );

  var
    i: Integer;

  begin
    Result := SanitizeText(RawMakefileContentText);
    for i := Low(CLEANUP_TAGS) to High(CLEANUP_TAGS) do
      Result := StringReplace(Result, CLEANUP_TAGS[i], DUMMY_TAG, [rfReplaceAll]);
  end;

begin
  PortDirectory := IncludeTrailingPathDelimiter(
    Environment.FileSystem.Kallisti.KallistiPortsDirectory + PortDirectoryBaseName);
  Makefile := PortDirectory + MAKEFILE_FILE_NAME;
  if FileExists(Makefile) then
  begin
    // Normal KallistiOS Port
    MakefileContent := TStringList.Create;
    try
      MakefileContent.LoadFromFile(Makefile);
      MakefileContentText := SanitizeMakefileContent(MakefileContent.Text);

      ExtractedURL := EmptyStr;
      PortName := GetPackageString('PORTNAME');

      with Add do
      begin
        fSourceDirectory := PortDirectory;
        fName := PortName;
        fVersion := GetPackageString('PORTVERSION');
        fMaintainer := GetPackageString('MAINTAINER');
        fLicense := GetPackageString('LICENSE');
        fShortDescription := GetPackageString('SHORT_DESC');
        fDescription := GetPackageDescription(ExtractedURL);
        fURL := ExtractedURL;
        fIncludeDirectory := GetPackageIncludeDirectory;
        fInstallDirectory := GetInstallDirectory;
        fSelfIncludes := GetPackageIncludes;
        fSelfLibraries := GetPackageLibraries;
        fDeclaredDependenciesNameList.Text := GetPackageDependencies;
        if not SameText(fDeclaredDependenciesNameList.Text, EmptyStr) then
          fPortsWithDependencies.Add(fListIndex)
        else
          fPortsWithoutDependencies.Add(fListIndex);
        fPortsMap.Add(PortName, fListIndex);
        fLibraryLanguageKind := GetPortLanguageKind(PortName);
        fVirtualAddon := False;
        fRequiredHostTools := GetPortExtraHostTools(PortName);
        if SameText(GetPackageString('PORT_BUILD'), 'cmake') then
          Include(fRequiredHostTools, phtCMake);
      end;

      Inc(fOnlyVisibleListCount);
    finally
      MakefileContent.Free;
    end;
  end
  else
  begin
    // KallistiOS Addon
    AddonIndex := GetAddonIndex(PortDirectoryBaseName);
    with Add do
    begin
      fName := PortDirectoryBaseName;
      fSelfIncludes := GetAddonIncludes(AddonIndex);
      fSelfLibraries := fAddonsLibraries[AddonIndex];
      fFullComputedDependenciesLibraryWeights := GetPortWeight(fName);
      fVirtualAddon := True;
    end;
  end;
end;

procedure TKallistiPortManager.ProcessPortsDependencies;
var
  i: Integer;
  PortInfo: TKallistiPortItem;
  PortDependenciesStr: string;

begin
{$IFDEF DEBUG}
  DebugLog('*** Process Ports Dependencies: Start ***');
{$ENDIF}

  for i := 0 to fPortsWithDependencies.Count - 1 do
  begin
    PortInfo := Items[fPortsWithDependencies[i]];
{$IFDEF DEBUG}
    DebugLog('  Processing: ' + PortInfo.Name);
{$ENDIF}
    ProcessDependenciesInitialize(PortInfo);
    PortDependenciesStr := ProcessPortDependencies(PortInfo);
    if not IsInString(PROCESS_DEPENDENCIES_CIRCULAR_REFERENCE_ERROR, PortDependenciesStr) then
    begin
      PortInfo.fFullComputedDependenciesNameList.Text := PortDependenciesStr;
      StringListRemoveDuplicates(PortInfo.fFullComputedDependenciesNameList);
      SetPortAdditionalInformation(PortInfo, True);
    end;
  end;

{$IFDEF DEBUG}
  DebugLog(sLineBreak + 'Results:');
  for i := 0 to fPortsWithDependencies.Count - 1 do
  begin
    PortInfo := Items[fPortsWithDependencies[i]];
    DebugLog('  ' + PortInfo.Name + ': "' +
      StringListToString(PortInfo.fFullComputedDependenciesNameList, ' + ') + '"' +
      ' (vs. "' + StringListToString(PortInfo.fDeclaredDependenciesNameList, ' + ') + '")');
  end;

  DebugLog('*** Process Ports Dependencies: End ***');
{$ENDIF}
end;

procedure TKallistiPortManager.ProcessPortsWithoutDependencies;
var
  i: Integer;
  PortInfo: TKallistiPortItem;

begin
  for i := 0 to fPortsWithoutDependencies.Count - 1 do
  begin
    PortInfo := Items[fPortsWithoutDependencies[i]];
    SetPortAdditionalInformation(PortInfo, False);
  end;
end;

function TKallistiPortManager.GetPortLibrary(const PortName: string): string;
var
  Index: Integer;

begin
  Result := EmptyStr;
  Index := GetPortIndex(PortName);
  if Index <> -1 then
    Result := Items[Index].fSelfLibraries;
end;

function TKallistiPortManager.GetPortWeight(const PortName: string): string;
var
  Weight: Integer;

  function LibraryNameToPortName(const LibraryName: string): string;
  begin
    Result := fKallistiLibraryInformation.ReadString('Reverse', LibraryName, LibraryName);
  end;

begin
  Weight := fKallistiLibraryInformation.ReadInteger('Weights',
    LibraryNameToPortName(PortName), 0);

  if Weight = 0 then
    Weight := fKallistiLibraryInformation.ReadInteger('Weights',
      LibraryNameToPortName('lib' + PortName), 0);

  Result := Format('%0.8d', [Weight]);
end;

function TKallistiPortManager.GetPortLanguageKind(const PortName: string
  ): TLibraryLanguageKind;
begin
  Result := TLibraryLanguageKind(
    fKallistiLibraryInformation.ReadInteger('Languages', PortName, 0));
end;

function TKallistiPortManager.GetOverridenInformation(
  InformationKind: TOverridenInformationKind; PortName: string): string;
var
  SectionName: string;

begin
  case InformationKind of
    oikInclude:
      SectionName := 'Includes';
    oikIncludeDirectory:
      SectionName := 'IncludeDirectories';
    else
      SectionName := 'Libraries';
  end;
  Result := fKallistiLibraryInformation.ReadString(SectionName, PortName, EmptyStr);
end;

procedure TKallistiPortManager.ProcessDependenciesInitialize(
  PortInfo: TKallistiPortItem);
begin
  fProcessDependenciesCurrentPortInfo := PortInfo;
  fProcessDependenciesCallCount := 0;
end;

function TKallistiPortManager.ProcessPortDependencies(PortInfo: TKallistiPortItem): string;
var
  j, DependencyPortIndex: Integer;
  SubPortInfo: TKallistiPortItem;

begin
  // Fail-safe for circular references detection...
  Inc(fProcessDependenciesCallCount);
{$IFDEF DEBUG}
  DebugLog(Format('ProcessPortDependencies for %s: %d',
    [fProcessDependenciesCurrentPortInfo.Name, fProcessDependenciesCallCount]
  ));
{$ENDIF}
  if fProcessDependenciesCallCount >= PROCESS_DEPENDENCIES_MAX_DEPTH then
  begin
    Result := PROCESS_DEPENDENCIES_CIRCULAR_REFERENCE_ERROR;
    Exit;
  end;

  // Recursive code
  if PortInfo.fDeclaredDependenciesNameList.Count = 0 then
  begin
    Result := PortInfo.Name;
  end
  else
  begin
    Result := PortInfo.Name;
    for j := 0 to PortInfo.fDeclaredDependenciesNameList.Count - 1 do
    begin
      DependencyPortIndex := GetPortIndex(PortInfo.fDeclaredDependenciesNameList[j]);
      if DependencyPortIndex <> -1 then
      begin
        SubPortInfo := Items[DependencyPortIndex];
        Result := Result + sLineBreak + ProcessPortDependencies(SubPortInfo);
      end;
    end;
  end;
end;

function TKallistiPortManager.GenerateIncludeHeader(const IncludeFiles: string;
  const IncludeDirectory: TFileName; const IsOverriden: Boolean): string;
const
  INCLUDE_SEPARATORS: array[0..1] of Char = ('|', ' ');

var
  InputBuffer,
  OutputBuffer: TStringList;
  i: Integer;
  Buffer, IncludeFile: string;

begin
  InputBuffer := TStringList.Create;
  OutputBuffer := TStringList.Create;
  try
    Buffer := IncludeFiles;
    for i := Low(INCLUDE_SEPARATORS) to High(INCLUDE_SEPARATORS) do
      Buffer := StringReplace(Buffer, INCLUDE_SEPARATORS[i], sLineBreak, [rfReplaceAll]);
    InputBuffer.Text := Buffer;
    for i := 0 to InputBuffer.Count - 1 do
    begin
      IncludeFile := Trim(InputBuffer[i]);
      if not SameText(IncludeFile, EmptyStr) then
      begin
        IncludeFile := IncludeDirectory + IncludeFile;
        if not IsOverriden then
          IncludeFile := '<' + IncludeFile + '>';
        if not SameText(IncludeFile, EmptyStr) then
          OutputBuffer.Add(IncludeFile);
      end;
    end;
    Result := StringListToString(OutputBuffer, ArraySeparator);
  finally
    OutputBuffer.Free;
    InputBuffer.Free;
  end;
end;

function TKallistiPortManager.GetPortIndex(const PortName: string): Integer;
var
  KeyIndex: Integer;

begin
  Result := -1;
  KeyIndex := fPortsMap.IndexOf(PortName);
  if KeyIndex <> -1 then
    Result := fPortsMap.Data[KeyIndex];
end;

procedure TKallistiPortManager.HandleLibrary(OutputBuffer: TStringList;
  PortName: string);
var
  DependencyPortLibrary: string;

  function GenerateLine(const LibraryCode: string): string;
  begin
    Result := Format('%s:%s', [GetPortWeight(LibraryCode), LibraryCode]);
  end;

  function HandleMultipleLibrary(OutputBuffer: TStringList;
    MultipleLibraryStr: string): Boolean;
  var
    Buffer: TStringList;
    k: Integer;
    SinglePortName: string;

  begin
    Result := False;
    Buffer := TStringList.Create;
    try
      StringToStringList(MultipleLibraryStr, ArraySeparator, Buffer);
      for k := 0 to Buffer.Count - 1 do
      begin
        SinglePortName := Trim(Buffer[k]);
        if not IsEmpty(SinglePortName) then
          OutputBuffer.Add(GenerateLine(SinglePortName));
      end;
      Result := True;
    finally
      Buffer.Free;
    end;
  end;

begin
  if Assigned(OutputBuffer) then
  begin
    DependencyPortLibrary := GetPortLibrary(PortName);
    if IsInString(ArraySeparator, DependencyPortLibrary) then
      HandleMultipleLibrary(OutputBuffer, DependencyPortLibrary)
    else if not IsEmpty(DependencyPortLibrary) then
    begin
      // An unknown dependency (not a port) has no library: don't add an empty entry
      OutputBuffer.Add(GenerateLine(DependencyPortLibrary));
    end;
  end;
end;

procedure TKallistiPortManager.SetPortAdditionalInformation(
  PortInfo: TKallistiPortItem; ProcessDependencies: Boolean);
var
  j: Integer;
  DependencyPortName, DependencyPortLibrary, DependencyPortWeight: string;
  PortOrderingBuffer, WeightsBuffer: TStringList;

  // Sort "weight:library" entries on the weight only, with a stable insertion
  // sort: libraries with the same weight keep their declared order, which is
  // the link order required by the port (e.g. mbedtls mbedx509 mbedcrypto).
  // TStringList.Sort can't be used: it sorts the whole string (so it falls
  // back to alphabetical order on equal weights) and isn't stable.
  procedure StableSortByWeight(Buffer: TStringList);
  var
    i, k: Integer;
    Entry, EntryWeight: string;

  begin
    for i := 1 to Buffer.Count - 1 do
    begin
      Entry := Buffer[i];
      EntryWeight := Left(':', Entry);
      k := i - 1;
      while (k >= 0) and (CompareStr(Left(':', Buffer[k]), EntryWeight) > 0) do
      begin
        Buffer[k + 1] := Buffer[k];
        Dec(k);
      end;
      Buffer[k + 1] := Entry;
    end;
  end;

begin
  PortOrderingBuffer := TStringList.Create;
  try
    // Populate PortOrderingBuffer to sort by weight
    if ProcessDependencies then
    begin
      for j := 0 to PortInfo.fFullComputedDependenciesNameList.Count - 1 do
      begin
        DependencyPortName := PortInfo.fFullComputedDependenciesNameList[j];
        HandleLibrary(PortOrderingBuffer, DependencyPortName);
      end;
    end
    else
      HandleLibrary(PortOrderingBuffer, PortInfo.Name);

    // Sort by Port weights
    StableSortByWeight(PortOrderingBuffer);

{$IFDEF DEBUG}
    DebugLog(StringListToString(PortOrderingBuffer, ' + '));
{$ENDIF}

    // Remove all weight information from the libraries names
    // Save the weight in a separate list
    WeightsBuffer := TStringList.Create;
    try
      for j := 0 to PortOrderingBuffer.Count - 1 do
      begin
        DependencyPortLibrary := Right(':', PortOrderingBuffer[j]);
        DependencyPortWeight := Left(':', PortOrderingBuffer[j]);

        // Saving only library name (without weight)
        PortOrderingBuffer[j] := DependencyPortLibrary;

        // Saving the libraries weight
        WeightsBuffer.Add(DependencyPortWeight);
      end;

      // Save the weights!
      PortInfo.fFullComputedDependenciesLibraryWeights :=
        StringListToString(WeightsBuffer, ArraySeparator);
    finally
      WeightsBuffer.Free;
    end;

    // Store all the dependencies libraries, ordered in the good way!
    // This is the final result of all of this...
    PortInfo.fFullComputedDependenciesLibraries :=
      StringListToString(PortOrderingBuffer, ArraySeparator);
  finally
    PortOrderingBuffer.Free;
  end;
end;

procedure TKallistiPortManager.GenerateIntegratedDevelopmentEnvironmentLibraryInformation
  (const LibraryLanguageKind: TLibraryLanguageKind);
const
  LIBINFO_PATH_C = 'c';
  LIBINFO_PATH_CPP = 'cpp';

  LIBINFO_ID = 'id.dat';
  LIBINFO_INC = 'inc.dat';
  LIBINFO_INCDIR = 'incdir.dat';
  LIBINFO_LIB = 'lib.dat';
  LIBINFO_SORT = 'sort.dat';

var
  OutputDirectory: TFileName;
  BufferId,
  BufferIncludes,
  BufferIncludeDirectories,
  BufferLibraries,
  BufferSort: TStringList;
  i: Integer;
  PortInfo: TKallistiPortItem;
  PortName: string;

  function LibraryLanguageKindToDirectory: TFileName;
  begin
    Result := LIBINFO_PATH_C;
    if LibraryLanguageKind = llkCPP then
      Result := LIBINFO_PATH_CPP;
  end;

  function IsValidPort: Boolean;
  begin
    Result := (PortInfo.fLibraryLanguageKind = llkAll)
      or (PortInfo.fLibraryLanguageKind = LibraryLanguageKind);
  end;

  function SanitizeInfo(const Value: string): string;
  begin
    Result := Value;
    if IsEmpty(Value) then
      Result := Chr($0C);
  end;

begin
{$IFDEF DEBUG}
  DebugLog('> GenerateIntegratedDevelopmentEnvironmentLibraryInformation: '
    + LibraryLanguageKindToDirectory);
{$ENDIF}
  with IntegratedDevelopmentEnvironment.CodeBlocks do
  begin
    if Settings.ExportLibraryInformation then
    begin
      OutputDirectory := Settings.ExportLibraryInformationPath
        + LibraryLanguageKindToDirectory + DirectorySeparator;
      ForceDirectories(OutputDirectory);

{$IFDEF DEBUG}
      DebugLog('  OutputDirectory: "' + OutputDirectory + '"');
{$ENDIF}

      BufferId := TStringList.Create;
      BufferIncludes := TStringList.Create;
      BufferIncludeDirectories := TStringList.Create;
      BufferLibraries := TStringList.Create;
      BufferSort := TStringList.Create;
      try
{$IFDEF DEBUG}
        DebugLog('  Ports Count: ' + IntToStr(Count));
{$ENDIF}
        for i := 0 to Count - 1 do
        begin
          PortInfo := Items[i];
{$IFDEF DEBUG}
          DebugLog('    * Handling "' + PortInfo.Name + '"');
{$ENDIF}
          if PortInfo.Installed and IsValidPort then
          begin
{$IFDEF DEBUG}
            DebugLog('      > Installed and Valid (Usable Within IDE: '
              + BoolToStr(PortInfo.UsableWithinIDE, 'Yes', 'No') + ')');
{$ENDIF}
            PortName := PortInfo.Name;
            BufferIncludes.Add(SanitizeInfo(PortInfo.Includes));
            BufferIncludeDirectories.Add(SanitizeInfo(PortInfo.IncludeDirectory));
            BufferLibraries.Add(SanitizeInfo(PortInfo.Libraries));
            BufferSort.Add(SanitizeInfo(PortInfo.LibraryWeights));
            if PortInfo.UsableWithinIDE then
              BufferId.Add(PortName)
            else
              BufferId.Add(PortName + ' (!)');
          end;
        end;

        SaveStringToFile(StringListToString(BufferId, ';', False), OutputDirectory + LIBINFO_ID);
        SaveStringToFile(StringListToString(BufferIncludes, ';', False), OutputDirectory + LIBINFO_INC);
        SaveStringToFile(StringListToString(BufferIncludeDirectories, ';', False), OutputDirectory + LIBINFO_INCDIR);
        SaveStringToFile(StringListToString(BufferLibraries, ';', False), OutputDirectory + LIBINFO_LIB);
        SaveStringToFile(StringListToString(BufferSort, ';', False), OutputDirectory + LIBINFO_SORT);
      finally
        BufferId.Free;
        BufferIncludes.Free;
        BufferIncludeDirectories.Free;
        BufferLibraries.Free;
        BufferSort.Free;
      end;
    end;
  end;
end;

// Generates the IDE-neutral library information file (JSON), used by IDEs
// that can't use the Code::Blocks files (e.g. Visual Studio Code).
// It contains all the ports and addons, whatever the IDE settings are.
procedure TKallistiPortManager.GenerateLibraryInformationFile;
const
  LIBINFO_DIRECTORY = 'ide';
  LIBINFO_FILE = 'libraries.json';
  LIBINFO_FORMAT_VERSION = 1;
  LANGUAGE_KIND_NAMES: array[TLibraryLanguageKind] of string = ('all', 'c', 'cpp');

var
  OutputDirectory, OutputFileName, TempFileName: TFileName;
  Root, PortObject: TJSONObject;
  PortsArray: TJSONArray;
  i: Integer;
  PortInfo: TKallistiPortItem;

  function ToJSONArray(const Value: string): TJSONArray;
  var
    Buffer: TStringList;
    j: Integer;
    Item: string;

  begin
    Result := TJSONArray.Create;
    Buffer := TStringList.Create;
    try
      StringToStringList(Value, ArraySeparator, Buffer);
      for j := 0 to Buffer.Count - 1 do
      begin
        Item := Trim(Buffer[j]);
        if not IsEmpty(Item) then
          Result.Add(Item);
      end;
    finally
      Buffer.Free;
    end;
  end;

begin
  OutputDirectory := Environment.FileSystem.Shell.ConfigurationDirectory
    + LIBINFO_DIRECTORY + DirectorySeparator;
  OutputFileName := OutputDirectory + LIBINFO_FILE;
  TempFileName := OutputFileName + '.tmp';

{$IFDEF DEBUG}
  DebugLog('> GenerateLibraryInformationFile: "' + OutputFileName + '"');
{$ENDIF}

  Root := TJSONObject.Create;
  try
    try
      PortsArray := TJSONArray.Create;
      Root.Add('version', LIBINFO_FORMAT_VERSION);
      Root.Add('libraries', PortsArray);

      for i := 0 to Count - 1 do
      begin
        PortInfo := Items[i];
        PortObject := TJSONObject.Create;
        PortObject.Add('name', PortInfo.Name);
        PortObject.Add('version', PortInfo.Version);
        PortObject.Add('description', PortInfo.ShortDescription);
        PortObject.Add('addon', PortInfo.Hidden);
        PortObject.Add('installed', PortInfo.Installed);
        PortObject.Add('usable', PortInfo.UsableWithinIDE);
        PortObject.Add('language', LANGUAGE_KIND_NAMES[PortInfo.LibraryLanguageKind]);
        PortObject.Add('includes', ToJSONArray(PortInfo.Includes));
        PortObject.Add('includeDirectories', ToJSONArray(PortInfo.IncludeDirectory));
        PortObject.Add('libraries', ToJSONArray(PortInfo.Libraries));
        PortObject.Add('weights', ToJSONArray(PortInfo.LibraryWeights));
        PortsArray.Add(PortObject);
      end;

      // Write to a temporary file first, so readers never see a partial file
      ForceDirectories(OutputDirectory);
      SaveStringToFile(Root.FormatJSON, TempFileName);
      if FileExists(OutputFileName) then
        DeleteFile(OutputFileName);
      RenameFile(TempFileName, OutputFileName);
    except
      // Never fail the caller (e.g. DreamSDK Manager on exit) for this file
      on E: Exception do
      begin
{$IFDEF DEBUG}
        DebugLog('  GenerateLibraryInformationFile failed: ' + E.Message);
{$ENDIF}
      end;
    end;
  finally
    Root.Free;
  end;
end;

function TKallistiPortManager.GetAddonIncludes(const AddonIndex: Integer): string;
var
  IncludeFiles: string;

begin
  IncludeFiles := StringReplace(fAddonsIncludes[AddonIndex],
    ArraySeparator, WhiteSpaceStr, [rfReplaceAll]);
  Result := GenerateIncludeHeader(IncludeFiles , EmptyStr, True);
end;

function TKallistiPortManager.GetAddonIndex(const AddonName: string): Integer;
begin
  Result := fAddonsId.IndexOf(AddonName);
end;

procedure TKallistiPortManager.RetrieveAvailableAddons;
begin
  StringToStringList(fKallistiLibraryInformation.ReadString('Addons', 'Id', EmptyStr), ';', fAddonsId);
  StringToStringList(fKallistiLibraryInformation.ReadString('Addons', 'Includes', EmptyStr), ';', fAddonsIncludes);
  StringToStringList(fKallistiLibraryInformation.ReadString('Addons', 'Libraries', EmptyStr), ';', fAddonsLibraries);
end;

constructor TKallistiPortManager.Create(
  AEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
  AIntegratedDevelopmentEnvironment: TIntegratedDevelopmentEnvironment;
  AComponentVersion: TComponentVersion;
  const AutoLoad: Boolean);
begin
  fEnvironment := AEnvironment;
  fComponentVersion := AComponentVersion;
  fIntegratedDevelopmentEnvironment := AIntegratedDevelopmentEnvironment;
  fList := TList.Create;
  fPortsWithDependencies := TIntegerList.Create;
  fPortsWithoutDependencies := TIntegerList.Create;
  fPortsMap := TStringIntegerMap.Create;
  fKallistiLibraryInformation := TIniFile.Create(Environment.FileSystem.Kallisti.KallistiPortsLibraryInformationFile);
  fAddonsId := TStringList.Create;
  fAddonsIncludes := TStringList.Create;
  fAddonsLibraries := TStringList.Create;
  fRepository := TDreamcastSoftwareDevelopmentRepository.Create(Environment,
    Environment.FileSystem.Kallisti.KallistiPortsDirectory);
  if AutoLoad then
    RetrieveAvailablePorts;
end;

destructor TKallistiPortManager.Destroy;
begin
  Clear;
  fRepository.Free;
  fPortsWithDependencies.Free;
  fPortsWithoutDependencies.Free;
  fList.Free;
  fPortsMap.Free;
  fKallistiLibraryInformation.Free;
  fAddonsId.Free;
  fAddonsIncludes.Free;
  fAddonsLibraries.Free;
  inherited Destroy;
end;

function TKallistiPortManager.CloneRepository(var BufferOutput: string): Boolean;
const
  KALLISTI_PORTS_INSTALLATION_DIRECTORY = 'kos-ports';

begin
  if not DirectoryExists(Environment.FileSystem.ToolchainBase) then
    ForceDirectories(Environment.FileSystem.ToolchainBase);
  Result := Environment.CloneRepository(Environment.Settings.Repositories.KallistiPortsURL,
    KALLISTI_PORTS_INSTALLATION_DIRECTORY,
    Environment.FileSystem.ToolchainBase, BufferOutput,
    Environment.Settings.Repositories.KallistiPortsRef);
end;

procedure TKallistiPortManager.GenerateIntegratedDevelopmentEnvironmentLibraryInformation;
begin
  // IDE-neutral library information file, always generated
  GenerateLibraryInformationFile;

  // Code::Blocks library information files
  if IntegratedDevelopmentEnvironment.CodeBlocks.Settings.ExportLibraryInformation then
  begin
    GenerateIntegratedDevelopmentEnvironmentLibraryInformation(llkC);
    GenerateIntegratedDevelopmentEnvironmentLibraryInformation(llkCPP);
  end;
end;

function TKallistiPortManager.UpdateRepository(var BufferOutput: string): TUpdateOperationState;
begin
  Result := Environment.UpdateRepository(
    Environment.FileSystem.Kallisti.KallistiPortsDirectory, BufferOutput);
end;

function TKallistiPortManager.InitializeEnvironment: Boolean;
const
  CONFIG_MAKEFILE = 'config.mk';
  BUILD_MAKEFILE = 'scripts\build.mk';
  DOWNLOAD_MAKEFILE = 'scripts\download.mk';
  GIT_CLONE_FILTER_MINIMAL_VERSION = '2.20.0';

var
  ConfigFileName,
  BuildFileName,
  DownloadFileName: TFileName;
  PythonValidateDistFilesOld,
  PythonValidateDistFilesNew: string;

begin
  ConfigFileName := Environment.FileSystem.Kallisti.KallistiPortsDirectory + CONFIG_MAKEFILE;
  BuildFileName := Environment.FileSystem.Kallisti.KallistiPortsDirectory + BUILD_MAKEFILE;
  DownloadFileName := Environment.FileSystem.Kallisti.KallistiPortsDirectory + DOWNLOAD_MAKEFILE;

  Result := FileExists(ConfigFileName) and FileExists(BuildFileName);
  if Result then
  begin
    PatchTextFile(
      ConfigFileName,
      '#FETCH_CMD = wget',
      'FETCH_CMD = wget --no-check-certificate',
      ptfbPatchWithWatermark
    );

    PatchTextFile(
      ConfigFileName,
      'FETCH_CMD = curl',
      '#FETCH_CMD = curl',
      ptfbPatchWithWatermark
    );

    // ln doesn't work very well under MinGW/MSYS...
    PatchTextFile(
      BuildFileName,
      'ln -s',
      'cp -r',
      ptfbPatchWithWatermark
    );

    // Python 3 on Windows is always "python"
    PatchTextFile(
      ConfigFileName,
      'PYTHON_CMD = python3',
      'PYTHON_CMD = python',
      ptfbPatchWithWatermark
    );

    // Handle Python "Validate DistFiles"
    PythonValidateDistFilesOld := 'true';
    PythonValidateDistFilesNew := 'false';
    if Versions.PythonInstalled then
    begin
      PythonValidateDistFilesOld := 'false';
      PythonValidateDistFilesNew := 'true';
    end;

    PatchTextFile(
      ConfigFileName,
      Format('VALIDATE_DISTFILES = %s', [PythonValidateDistFilesOld]),
      Format('VALIDATE_DISTFILES = %s', [PythonValidateDistFilesNew]),
      ptfbAlwaysPatch
    );

    // "git clone --filter=tree:0" isn't supported by older Git (e.g., on Windows XP)
    if Versions.GitInstalled
      and (CompareVersion(Versions.Git, GIT_CLONE_FILTER_MINIMAL_VERSION) = 1) then
      PatchTextFile(
        DownloadFileName,
        'git clone --filter=tree:0 ',
        'git clone ',
        ptfbPatchWithWatermark
      );
  end;
end;

function TKallistiPortManager.Install(var OutputBuffer: string): Boolean;
const
  FAILED_TAG = 'Bailing out.';

begin
  Result := False;
  if Installed then
  begin
    OutputBuffer := Environment.ExecuteShellCommand(KALLISTI_PORTS_ALL_BUILD, UtilityDirectory);
    Environment.ExecuteShellCommand(KALLISTI_PORTS_ALL_CLEAN, UtilityDirectory);
    Result := not IsInString(FAILED_TAG, OutputBuffer);
  end;
end;

function TKallistiPortManager.Uninstall(var OutputBuffer: string): Boolean;
begin
  if Installed then
  begin
    OutputBuffer := Environment.ExecuteShellCommand(KALLISTI_PORTS_ALL_UNINSTALL, UtilityDirectory);
    Environment.ExecuteShellCommand(KALLISTI_PORTS_RESET, UtilityDirectory);
  end;
  Result := Installed;
end;

end.

