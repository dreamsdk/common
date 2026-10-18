unit KOSInst;

(*
  KallistiOS, KallistiOS Ports and Dreamcast Tool installer.

  This unit contains the process used for installing/updating KallistiOS,
  KallistiOS Ports and Dreamcast Tool (and unpacking the toolchains if needed),
  mainly used as the post-install step of the DreamSDK Setup.

  This is used by DreamSDK Manager (through the ShellThread) and by the
  DreamSDK Setup post-install helper library, so it *MUST* stay GUI-agnostic.

  The installer runs synchronously in the caller's thread; it should be called
  from a worker thread. OnProgressText is triggered in the caller's thread,
  OnNewLine and OnUnpackTerminate are triggered by the Package Manager (i.e.,
  through Synchronize, so in the main thread).
*)

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DCSDKMgr,
  Environ,
  PkgMgr;

resourcestring
  KallistiText = 'KallistiOS';
  KallistiPortsText = 'KallistiOS Ports';
  DreamcastToolText = 'Dreamcast Tool';
  FinalizingPostInstall = 'Executing post-install steps...';
  InstallationProblem =
    'Your installation have problems!!!' + sLineBreak +
    'No required binaries were found in the InstallPath directory ("%s").' + sLineBreak +
    'Please check the configuration file ("%s") and modify the InstallPath key if needed.';
  KallistiOperationNothingNeededText = 'KallistiOS is already installed and up-to-date.';
  CloningOperation = 'Creating %s repository...';
  UpdatingOperation = 'Updating %s repository...';
  KallistiInitializeText = 'Initialize KallistiOS environment...';
  KallistiBuildText = 'Building KallistiOS library...';
  KallistiFixNewlibText = 'Fixing SuperH Newlib...';
  DreamcastToolInitializeText = 'Initialize Dreamcast Tool environment...';
  DreamcastToolBuildText = 'Building Dreamcast Tool components...';
  DreamcastToolInstallText = 'Installing Dreamcast Tool...';
  ToolchainsUnpackingText = 'Unpacking toolchains packages...';
  ToolchainsUnpackingPickupText = 'Processing: %s';
  ToolchainsUnpackingProgressText = 'Package Manager: %d%% / Progress Event';

type
  TKallistiInstallerResponse = (
    kirNothing,
    kirInstall,
    kirUpdate
  );

  TKallistiInstallerTextEvent = procedure(Sender: TObject;
    const Text: string) of object;
  TKallistiInstallerUnpackTerminateEvent = procedure(Sender: TObject;
    const Success: Boolean; const Aborted: Boolean) of object;

  { TKallistiInstaller }
  TKallistiInstaller = class(TObject)
  private
    fAborted: Boolean;
    fManager: TDreamcastSoftwareDevelopmentKitManager;
    fPackageManager: TPackageManager;
    fOperationSuccess: Boolean;
    fOperationResultOutput: string;
    fResponse: TKallistiInstallerResponse;
    fOperationUpdateState: TUpdateOperationState;
    fNewLine: TKallistiInstallerTextEvent;
    fProgressText: TKallistiInstallerTextEvent;
    fUnpackTerminate: TKallistiInstallerUnpackTerminateEvent;
    function GetShellAvailable: Boolean;
    procedure HandlePackageManagerStart(Sender: TObject);
    procedure HandlePackagePickup(Sender: TObject;
      const SourceFileName, OutputDirectory: TFileName);
    procedure HandlePackageManagerProgress(Sender: TObject;
      const CurrentValue: Integer; const TotalValue: Integer);
    procedure HandlePackageManagerTerminate(Sender: TObject;
      const Success: Boolean; const Aborted: Boolean);
  protected
    function TaskSuccess: Boolean;
    function CanContinue: Boolean;
    procedure AddNewLine(const NewLine: string);
    procedure SetOperationSuccess(const Success: Boolean);
    procedure UpdateProgressText(const Text: string);
  public
    constructor Create(AManager: TDreamcastSoftwareDevelopmentKitManager);
    destructor Destroy; override;

    (* Abort the running process (this can be called from another thread). *)
    procedure Abort;
    procedure Pause;
    procedure Resume;

    (* Run all the steps: Prepare then ProcessKallisti. *)
    function Execute: Boolean;

    (* Execute the shell once if in post-install mode (as on MinGW64/MSYS2
       there is some additional setup done the first time the shell is run)
       then unpack the toolchains (if required). *)
    procedure Prepare;

    (* Install or update KallistiOS, KallistiOS Ports and Dreamcast Tool. *)
    function ProcessKallisti: Boolean;

    property Aborted: Boolean read fAborted write fAborted;
    property Manager: TDreamcastSoftwareDevelopmentKitManager read fManager;
    property PackageManager: TPackageManager read fPackageManager;
    property Response: TKallistiInstallerResponse read fResponse;
    property ResultOutput: string read fOperationResultOutput;
    property ShellAvailable: Boolean read GetShellAvailable;
    property Success: Boolean read fOperationSuccess;
    property UpdateState: TUpdateOperationState read fOperationUpdateState;

    property OnNewLine: TKallistiInstallerTextEvent
      read fNewLine write fNewLine;
    property OnProgressText: TKallistiInstallerTextEvent
      read fProgressText write fProgressText;
    property OnUnpackTerminate: TKallistiInstallerUnpackTerminateEvent
      read fUnpackTerminate write fUnpackTerminate;
  end;

implementation

uses
  TypInfo,
  SysTools,
  FSTools,
  InstMode;

{ TKallistiInstaller }

constructor TKallistiInstaller.Create(
  AManager: TDreamcastSoftwareDevelopmentKitManager);
begin
  fManager := AManager;
  fAborted := False;
  fOperationSuccess := True;
  fOperationResultOutput := EmptyStr;
  fResponse := kirNothing;
  fOperationUpdateState := uosUndefined;

  // This will be used for installing toolchains/debugger after installing DreamSDK (mainly)
  fPackageManager := TPackageManager.Create(fManager, False);
  with fPackageManager do
  begin
    EnableBusyWaitingOnExecution := True; // Active wait, as we should be on a thread
    Operation := pmrAutoDetectDebuggerToolchain; // Detect if DebuggerProfileKey/ToolchainProfileKey need to be replaced
    OnStart := @HandlePackageManagerStart;
    OnPackagePickup := @HandlePackagePickup;
    OnProgress := @HandlePackageManagerProgress;
    OnTerminate := @HandlePackageManagerTerminate;
  end;
end;

destructor TKallistiInstaller.Destroy;
begin
  fPackageManager.Free;
  inherited Destroy;
end;

function TKallistiInstaller.GetShellAvailable: Boolean;
begin
  Result := FileExists(Manager.Environment.FileSystem.Shell.ShellExecutable);
end;

procedure TKallistiInstaller.HandlePackageManagerStart(Sender: TObject);
begin
  UpdateProgressText(ToolchainsUnpackingText);
end;

procedure TKallistiInstaller.HandlePackagePickup(Sender: TObject;
  const SourceFileName, OutputDirectory: TFileName);
begin
  AddNewLine(Format(ToolchainsUnpackingPickupText, [
    ExtractFileName(SourceFileName)
  ]));
end;

procedure TKallistiInstaller.HandlePackageManagerProgress(Sender: TObject;
  const CurrentValue: Integer; const TotalValue: Integer);
begin
{$IFDEF DEBUG}
  DebugLog(Format('HandlePackageManagerProgress: %d%% (%d%%)', [
    CurrentValue,
    TotalValue
  ]));
{$ENDIF}

  AddNewLine(Format(ToolchainsUnpackingProgressText, [CurrentValue]));
end;

procedure TKallistiInstaller.HandlePackageManagerTerminate(Sender: TObject;
  const Success: Boolean; const Aborted: Boolean);
var
  LogContext: TLogMessageContext;

begin
  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%}, ClassName);
  try

    LogMessage(LogContext, Format('Success=%s, Aborted=%s', [
      BoolToStr(Success, True),
      BoolToStr(Aborted, True)
    ]));

    // Updating success status of the whole process
    fOperationSuccess := fOperationSuccess and Success;

    if Assigned(fUnpackTerminate) then
      fUnpackTerminate(Self, Success, Aborted);

  finally
    LogMessageExit(LogContext);
  end;
end;

function TKallistiInstaller.TaskSuccess: Boolean;
begin
  Result := fOperationSuccess and (not Aborted);
end;

function TKallistiInstaller.CanContinue: Boolean;
begin
  Result := (not IsPostInstallMode and TaskSuccess) or
    (IsPostInstallMode and (not Aborted));
end;

procedure TKallistiInstaller.AddNewLine(const NewLine: string);
begin
  if Assigned(fNewLine) then
    fNewLine(Self, NewLine);
end;

procedure TKallistiInstaller.SetOperationSuccess(const Success: Boolean);
begin
  fOperationSuccess := TaskSuccess and Success;
end;

procedure TKallistiInstaller.UpdateProgressText(const Text: string);
begin
  if Assigned(fProgressText) then
    fProgressText(Self, Text);
end;

procedure TKallistiInstaller.Abort;
begin
  fAborted := True;
  if Assigned(fPackageManager) and fPackageManager.Running then
    fPackageManager.Abort;
  Manager.Environment.AbortShellCommand;
end;

procedure TKallistiInstaller.Pause;
begin
  Manager.Environment.PauseShellCommand;
  if Assigned(fPackageManager) and fPackageManager.Running then
    fPackageManager.Pause;
end;

procedure TKallistiInstaller.Resume;
begin
  Manager.Environment.ResumeShellCommand;
  if Assigned(fPackageManager) and fPackageManager.Running then
    fPackageManager.Resume;
end;

function TKallistiInstaller.Execute: Boolean;
var
  LogContext: TLogMessageContext;

begin
  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%}, ClassName);
  try

    Prepare;

    if ShellAvailable then
      ProcessKallisti
    else
    begin
      // This should never happens...
      fOperationSuccess := False;
      fOperationResultOutput := Format(InstallationProblem, [
        Manager.Environment.Settings.InstallPath,
        Manager.Environment.Settings.FileName
      ]);
      UpdateProgressText(fOperationResultOutput);
    end;

    Result := TaskSuccess;

  finally
    LogMessageExit(LogContext);
  end;
end;

procedure TKallistiInstaller.Prepare;
begin
  // If we installed DreamSDK, execute the shell once, as on MinGW64/MSYS2
  // there is some additional setup done the first time the shell is run
  if IsPostInstallMode then
  begin
    UpdateProgressText(FinalizingPostInstall);
    Manager.Environment.ExecuteShellCommand('exit', GetCurrentDir);
  end;

  // Process Unpacking Toolchains (if required)
  // This is usually used while installing DreamSDK only.
  fPackageManager.Execute;
end;

function TKallistiInstaller.ProcessKallisti: Boolean;
type
  TRepositoryOperation = (roNothing, roClone, roUpdate);

var
  LogContext: TLogMessageContext;
  OutputBuffer: string;
  IsModifiedKallisti,
  IsModifiedKallistiPorts,
  IsModifiedDreamcastTool,
  IsEnvironShellScriptUpdated: Boolean;

  procedure CombineOutputBuffer(const InputBuffer: string);
  var
    Separator: string;

  begin
    Separator := '';
    if OutputBuffer <> '' then
      Separator := sLineBreak;
    OutputBuffer := OutputBuffer + Separator + InputBuffer;
  end;

  function RepositoryKindToString(RepositoryKind: TRepositoryKind): string;
  begin
    Result := KallistiText;
    case RepositoryKind of
      rkKallistiPorts:
        Result := KallistiPortsText;
      rkDreamcastTool:
        Result := DreamcastToolText;
    end;
  end;

  function HandleRepository(Installed: Boolean; RepositoryKind: TRepositoryKind;
    var UpdateState: TUpdateOperationState): TRepositoryOperation;
  var
    LogSubContext: TLogMessageContext;
    RepositoryName,
    TempBuffer: string;
    IsSuccess: Boolean;
    WorkDirectory: TFileName;
    RenameBackupResult: TRenameFileOrDirectoryAsBackupResult;

  begin
    IsSuccess := False;
    Result := roNothing;
    TempBuffer := EmptyStr;

    LogSubContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%}, ClassName);
    try
      RepositoryName := RepositoryKindToString(RepositoryKind);

      (* Specific process: we will force the cloning in that case, so remove old
         directories in that case *)
      if IsPostInstallMode then
      begin
        case RepositoryKind of
          rkKallisti:
            WorkDirectory := Manager.Environment.FileSystem.Kallisti.KallistiDirectory;
          rkKallistiPorts:
            WorkDirectory := Manager.Environment.FileSystem.Kallisti.KallistiPortsDirectory;
          rkDreamcastTool:
            WorkDirectory := Manager.Environment.FileSystem.DreamcastTool.BaseDirectory;
        end;
        RenameBackupResult := RenameFileOrDirectoryAsBackup(WorkDirectory);
        IsSuccess := (RenameBackupResult = rfdbSuccess);
        LogMessage(LogSubContext, Format('PostInstall Mode, renaming directory [IsSuccess: "%s", Reason: "%s"]: "%s"', [
          BoolToStr(IsSuccess, True),
          GetEnumName(TypeInfo(TRenameFileOrDirectoryAsBackupResult), Ord(RenameBackupResult)),
          WorkDirectory
        ]));
        IsSuccess := False;
      end;

      if CanContinue then
      begin
        if not Installed then
        begin
          // Install (Clone)
          Result := roClone;
          UpdateProgressText(Format(CloningOperation, [RepositoryName]));
          case RepositoryKind of
            rkKallisti:
              IsSuccess := Manager.KallistiOS.CloneRepository(TempBuffer);
            rkKallistiPorts:
              IsSuccess := Manager.KallistiPorts.CloneRepository(TempBuffer);
            rkDreamcastTool:
              IsSuccess := Manager.DreamcastTool.CloneRepository(TempBuffer);
          end;
          SetOperationSuccess(IsSuccess);
        end
        else
        begin
          // Update
          Result := roUpdate;
          UpdateProgressText(Format(UpdatingOperation, [RepositoryName]));
          case RepositoryKind of
            rkKallisti:
              UpdateState := Manager.KallistiOS.UpdateRepository(TempBuffer);
            rkKallistiPorts:
              UpdateState := Manager.KallistiPorts.UpdateRepository(TempBuffer);
            rkDreamcastTool:
              UpdateState := Manager.DreamcastTool.UpdateRepository(TempBuffer);
          end;
          SetOperationSuccess(UpdateState <> uosUpdateFailed);
        end;
        CombineOutputBuffer(TempBuffer);
      end;

      LogMessage(LogSubContext, Format('RepositoryKind: "%s", ' +
        'RepositoryName: "%s", Result: "%s", UpdateState: "%s", ' +
        'Installed: "%s", IsSuccess: "%s", CanContinue: "%s"', [
        GetEnumName(TypeInfo(TRepositoryKind), Ord(RepositoryKind)),
        RepositoryName,
        GetEnumName(TypeInfo(TRepositoryOperation), Ord(Result)),
        GetEnumName(TypeInfo(TUpdateOperationState), Ord(UpdateState)),
        BoolToStr(Installed, True),
        BoolToStr(IsSuccess, True),
        BoolToStr(CanContinue, True)
      ]));

      LogMessage(LogSubContext, Format('TempBuffer: "%s"', [
        TempBuffer
      ]));
    finally
      LogMessageExit(LogSubContext);
    end;
  end;

  function HandleResponse(RepositoryKind: TRepositoryKind;
    RepositoryOperation: TRepositoryOperation; UpdateState: TUpdateOperationState;
    ComponentBuilt: Boolean): Boolean;
  var
    RepositoryInstalled,
    RepositoryUpdated,
    BuildNecessary: Boolean;

  begin
    RepositoryInstalled := (RepositoryOperation = roClone);
    RepositoryUpdated := ((RepositoryOperation = roUpdate)
      and (UpdateState = uosUpdateSuccess));
    BuildNecessary := (not ComponentBuilt);

    if (RepositoryKind = rkKallisti) then
    begin
      // Kallisti has priority
      if RepositoryInstalled then
        fResponse := kirInstall
      else if RepositoryUpdated then
      begin
        fResponse := kirUpdate;
        fOperationUpdateState := UpdateState;
      end
      else if BuildNecessary then
      begin
        fResponse := kirUpdate;
        fOperationUpdateState := uosUpdateSuccess;
      end;
    end
    else if (fResponse = kirNothing) then
    begin
      // Kallisti Ports and Dreamcast Tool in second place...
      if (RepositoryInstalled or RepositoryUpdated or BuildNecessary) then
      begin
        fResponse := kirUpdate;
        fOperationUpdateState := uosUpdateSuccess;
      end;
    end;

    Result := RepositoryInstalled or BuildNecessary;
  end;

  function HandleKallisti: Boolean;
  var
    TempBuffer: string;
    RepositoryOperation: TRepositoryOperation;
    UpdateState: TUpdateOperationState;

  begin
    // Handle KallistiOS Repository
    TempBuffer := EmptyStr;
    UpdateState := uosUndefined;

    RepositoryOperation := HandleRepository(Manager.KallistiOS.Installed,
      rkKallisti, UpdateState);

    // Handle all the cases where Kallisti need to be compiled
    Result := HandleResponse(rkKallisti, RepositoryOperation, UpdateState,
      Manager.KallistiOS.Built);

    // Determine if we need to do something
    if Result then
    begin
      // Generate environ.sh file, unpacking genromfs and patching config.mk in kos-ports
      if CanContinue then
      begin
        UpdateProgressText(KallistiInitializeText);
        SetOperationSuccess(Manager.KallistiOS.InitializeEnvironment);
        IsEnvironShellScriptUpdated := True;
      end;

      // Making KallistiOS library
      if CanContinue then
      begin
        UpdateProgressText(KallistiBuildText);
        SetOperationSuccess(Manager.KallistiOS.Build(TempBuffer));
      end;

      // Fixing-up SH-4 Newlib
      if CanContinue then
      begin
        UpdateProgressText(KallistiFixNewlibText);
        SetOperationSuccess(Manager.KallistiOS.FixupHitachiNewlib(TempBuffer));
      end;
    end;
    CombineOutputBuffer(TempBuffer);
  end;

  function HandleKallistiPorts: Boolean;
  var
    RepositoryOperation: TRepositoryOperation;
    UpdateState: TUpdateOperationState;

  begin
    // Handle Kallisti Ports Repository
    UpdateState := uosUndefined;
    RepositoryOperation := HandleRepository(Manager.KallistiPorts.Installed,
      rkKallistiPorts, UpdateState);
    Result := HandleResponse(rkKallistiPorts, RepositoryOperation, UpdateState, True);

    // Determine if we need to do something
    if Result then
      Manager.KallistiPorts.InitializeEnvironment;
  end;

  function HandleDreamcastTool: Boolean;
  var
    TempBuffer: string;
    RepositoryOperation: TRepositoryOperation;
    UpdateState: TUpdateOperationState;

  begin
    // Handle Dreamcast Tool(dc-tool) repository
    TempBuffer := EmptyStr;
    UpdateState := uosUndefined;

    RepositoryOperation := HandleRepository(Manager.DreamcastTool.Installed,
      rkDreamcastTool, UpdateState);

    // Determine if we need to do something
    Result := HandleResponse(rkDreamcastTool, RepositoryOperation, UpdateState,
      Manager.DreamcastTool.Built) or IsEnvironShellScriptUpdated;

    if Result then
    begin
      // Preparing Makefile.cfg
      if CanContinue then
      begin
        UpdateProgressText(DreamcastToolInitializeText);
        SetOperationSuccess(Manager.DreamcastTool.InitializeEnvironment);
      end;

      // Making Dreamcast Tool binaries
      if CanContinue then
      begin
        UpdateProgressText(DreamcastToolBuildText);
        SetOperationSuccess(Manager.DreamcastTool.Build(TempBuffer));
      end;

      // Copying the right Dreamcast Tool binary in place
      if CanContinue then
      begin
        UpdateProgressText(DreamcastToolInstallText);
        SetOperationSuccess(Manager.DreamcastTool.Install);
      end;
    end;
    CombineOutputBuffer(TempBuffer);
  end;

begin
  OutputBuffer := EmptyStr;
  IsEnvironShellScriptUpdated := False;

  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%}, ClassName);
  try
    IsModifiedKallisti := HandleKallisti;
    LogMessage(LogContext, Format('IsModifiedKallisti: "%s"', [
      BoolToStr(IsModifiedKallisti, True)
    ]));

    IsModifiedKallistiPorts := HandleKallistiPorts;
    LogMessage(LogContext, Format('IsModifiedKallistiPorts: "%s"', [
      BoolToStr(IsModifiedKallistiPorts, True)
    ]));

    IsModifiedDreamcastTool := HandleDreamcastTool;
    LogMessage(LogContext, Format('IsModifiedDreamcastTool: "%s"', [
      BoolToStr(IsModifiedDreamcastTool, True)
    ]));

    if (CanContinue) and (not IsModifiedKallisti) and (not IsModifiedKallistiPorts)
      and (not IsModifiedDreamcastTool) then
        UpdateProgressText(KallistiOperationNothingNeededText);
    LogMessage(LogContext, Format('CanContinue: "%s"', [
      BoolToStr(CanContinue, True)
    ]));

    fOperationResultOutput := OutputBuffer;
    LogMessage(LogContext, Format('Result: "%s"', [
      fOperationResultOutput
    ]));

    Result := TaskSuccess;
  finally
    LogMessageExit(LogContext);
  end;
end;

end.
