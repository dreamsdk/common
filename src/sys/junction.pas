unit Junction;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

function CreateJunction(const SourceDirectory,
  TargetDirectory: TFileName): Boolean;
function IsJunction(const DirectoryName: string): Boolean;
function RemoveJunction(const TargetDirectory: TFileName): Boolean;

implementation

{$R embedded/junction.rc}

uses
  Windows,
  Registry,
  SysTools,
  FSTools,
  RunTools,
  Version;

const
  EMBEDDED_RESOURCE_JUNCTION  = 'JUNCTION';
  EMBEDDED_FILENAME_JUNCTION  = 'junction.exe';

  JUNCTION_REGISTRY_KEY       = 'Software\Sysinternals\Junction';
  JUNCTION_REGISTRY_VALUE     = 'EulaAccepted';

{$IFDEF DEBUG}
  // Set this to TRUE if you want to force the usage of Sysinternals Junction
  // DEBUG only of course.
  JUNCTION_FORCE_UTILITY      = False;
{$ENDIF}

var
  JunctionAcceptEulaExists,
  JunctionAcceptEulaValue: Boolean;
  JunctionFileName: TFileName = '';

function IsNativeJunctionUtility: Boolean;
begin
  Result := IsWindowsVistaOrGreater;
{$IFDEF DEBUG}
  if JUNCTION_FORCE_UTILITY then
    Result := True;
{$ENDIF}
end;

// Thanks to: Sertac Akyuz
// https://stackoverflow.com/a/13384920
function IsJunction(const DirectoryName: string): Boolean;
const
  IO_REPARSE_TAG_MOUNT_POINT = $A0000003;

var
  FindHandle: THandle;
  FindData: TWin32FindData;

begin
  Result := False;
  FindData := Default(TWin32FindData);

{$IFDEF DEBUG}
  DebugLog(Format('IsJunction: "%s"', [DirectoryName]));
{$ENDIF}

  FindHandle := FindFirstFile(PChar(DirectoryName), FindData);
  if FindHandle <> INVALID_HANDLE_VALUE then
  begin
    Result := (Bool(FindData.dwFileAttributes and FILE_ATTRIBUTE_REPARSE_POINT))
              and (FindData.dwReserved0 = IO_REPARSE_TAG_MOUNT_POINT);

{$IFDEF DEBUG}
    DebugLog(Format('  IsJunction ["%s"]: %s', [DirectoryName, DebugBoolToStr(Result)]));
{$ENDIF}

    FindClose(FindHandle);
  end
  else
    RaiseLastOSError;
end;

function CreateJunction(const SourceDirectory,
  TargetDirectory: TFileName): Boolean;
var
  Executable,
  Parameters: string;

begin
  Result := False;

{$IFDEF DEBUG}
  DebugLog('CreateJunction' + sLineBreak
    + '  SourceDirectory: "' + SourceDirectory + '"' + sLineBreak
    + '  TargetDirectory: "' + TargetDirectory + '"'
  );
{$ENDIF}

  // If nothing to do, exit
  if DirectoryExists(TargetDirectory) and IsJunction(TargetDirectory) then
  begin
{$IFDEF DEBUG}
    DebugLog('  Nothing to do, exiting');
{$ENDIF}
    Result := True;
    Exit;
  end;

  // Source is not available
  if not DirectoryExists(SourceDirectory) then
    Exit;

  // Target is already here (and not Junction)
  if DirectoryExists(TargetDirectory) then
    Exit;

  // For Windows XP until Vista, we need to use Junction from SysInternals
  Executable := JunctionFileName;
  Parameters := Format('/accepteula "%s" "%s"', [
    TargetDirectory,
    SourceDirectory
  ]);

  // For Vista and greater, we use mklink
  if IsNativeJunctionUtility then
  begin
    Executable := ExpandEnvironmentStrings('%ComSpec%');
    Parameters := Format('/C "mklink /J "%s" "%s""', [
      TargetDirectory,
      SourceDirectory
    ]);
  end;

  // Make that Junction
  try
    Run(Executable, Parameters);
    Result := IsJunction(TargetDirectory);
  except
    Result := False;
  end;
end;

function RemoveJunction(const TargetDirectory: TFileName): Boolean;
var
  Executable,
  Parameters: string;

begin
  Result := False;

{$IFDEF DEBUG}
  DebugLog('RemoveJunction' + sLineBreak
    + '  TargetDirectory: "' + TargetDirectory + '"'
  );
{$ENDIF}

  // Target is not available
  if not DirectoryExists(TargetDirectory) then
  begin
{$IFDEF DEBUG}
    DebugLog('  Nothing to do, exiting');
{$ENDIF}
    Result := True;
    Exit;
  end;

  // Target is not junction
  if not IsJunction(TargetDirectory) then
    Exit;

  // For Windows XP until Vista, we need to use Junction from SysInternals
  Executable := JunctionFileName;
  Parameters := Format('/accepteula -d "%s"', [
    TargetDirectory
  ]);

  // For Vista and greater, we use rmdir
  // See: https://superuser.com/a/285596
  if IsNativeJunctionUtility then
  begin
    Executable := ExpandEnvironmentStrings('%ComSpec%');
    Parameters := Format('/C "rmdir "%s""', [
      TargetDirectory
    ]);
  end;

  // Remove that Junction
  try
    Run(Executable, Parameters);
    Result := not DirectoryExists(TargetDirectory);
  except
    Result := False;
  end;
end;

procedure BackupJunctionAcceptEula;
var
  JunctionRegistryInstance: TRegistry;

begin
{$IFDEF DEBUG}
  DebugLog('BackupJunctionAcceptEula');
{$ENDIF}
  JunctionRegistryInstance := TRegistry.Create;
  try
    JunctionAcceptEulaExists := False;
    JunctionRegistryInstance.RootKey := HKEY_CURRENT_USER;
    if JunctionRegistryInstance.OpenKey(JUNCTION_REGISTRY_KEY, False) then
      if JunctionRegistryInstance.ValueExists(JUNCTION_REGISTRY_VALUE) then
      begin
        JunctionAcceptEulaExists := True;
        JunctionAcceptEulaValue := JunctionRegistryInstance.ReadBool(JUNCTION_REGISTRY_VALUE);
{$IFDEF DEBUG}
        DebugLog('  ' + JUNCTION_REGISTRY_VALUE + ' = ' + DebugBoolToStr(JunctionAcceptEulaValue));
{$ENDIF}
        // Delete value to avoid EulaAccepted not working if it was FALSE
        JunctionRegistryInstance.DeleteValue(JUNCTION_REGISTRY_VALUE);
      end;
  finally
    JunctionRegistryInstance.Free;
  end;
end;

procedure RestoreJunctionAcceptEula;
var
  JunctionRegistryInstance: TRegistry;

begin
{$IFDEF DEBUG}
  DebugLog('RestoreJunctionAcceptEula');
{$ENDIF}
  JunctionRegistryInstance := TRegistry.Create;
  try
    JunctionRegistryInstance.RootKey := HKEY_CURRENT_USER;
    if JunctionRegistryInstance.OpenKey(JUNCTION_REGISTRY_KEY, False) then
    begin
      if JunctionAcceptEulaExists then
      begin
        JunctionRegistryInstance.WriteBool(JUNCTION_REGISTRY_VALUE, JunctionAcceptEulaValue);
{$IFDEF DEBUG}
        DebugLog('  ' + JUNCTION_REGISTRY_VALUE + ' = ' + DebugBoolToStr(JunctionAcceptEulaValue));
{$ENDIF}
      end
      else
      begin
        JunctionRegistryInstance.DeleteValue(JUNCTION_REGISTRY_VALUE);
{$IFDEF DEBUG}
        DebugLog('  ' + JUNCTION_REGISTRY_VALUE + ' was DELETED.');
{$ENDIF}
      end;
    end;
  finally
    JunctionRegistryInstance.Free;
  end;
end;

initialization
  // Initialize Registry interface for Junction EULA state
  JunctionAcceptEulaValue := Default(Boolean);
  JunctionAcceptEulaExists := Default(Boolean);
  BackupJunctionAcceptEula;

  // Extract SysInternals Junction utility for Windows XP
  if not FileExists(JunctionFileName) then
    JunctionFileName := ExtractEmbeddedFileToWorkingPath(
      EMBEDDED_RESOURCE_JUNCTION, EMBEDDED_FILENAME_JUNCTION);

finalization
  KillFile(JunctionFileName);

  // Destroy Registry interface for Junction EULA state
  RestoreJunctionAcceptEula;
end.

