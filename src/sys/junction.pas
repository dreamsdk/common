unit Junction;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

function CreateJunction(const SourceDirectory,
  TargetDirectory: TFileName): Boolean;

implementation

{$R embedded/junction.rc}

uses
  Windows,
  SysTools,
  FSTools,
  RunTools,
  Version;

const
  EMBEDDED_RESOURCE_JUNCTION = 'JUNCTION';
  EMBEDDED_FILENAME_JUNCTION = 'junction.exe';

var
  JunctionFileName: TFileName = '';

// Thanks to: Sertac Akyuz
// https://stackoverflow.com/a/13384920
function IsJunction(const FileName: string): Boolean;
//  IO_REPARSE_TAG_MOUNT_POINT = $A0000003;
var
  FindHandle: THandle;
  FindData: TWin32FindData;
begin
  Result := False;
  FindData := Default(TWin32FindData);
  FindHandle := FindFirstFile(PChar(FileName), FindData);
  if FindHandle <> INVALID_HANDLE_VALUE then begin
    Result := (Bool(FindData.dwFileAttributes and FILE_ATTRIBUTE_REPARSE_POINT))
              and Bool(FindData.dwReserved0 and $80000000) // MS bit
              and Bool(FindData.dwReserved0 and $20000000) // name surrogate bit
              and (LoWord(FindData.dwReserved0) = 3); // mount point value
    FindClose(FindHandle);
  end else
    RaiseLastOSError;
end;

function CreateJunction(const SourceDirectory,
  TargetDirectory: TFileName): Boolean;
var
  Executable,
  Parameters: string;

begin
  Result := False;

  // If nothing to do, exit
  if DirectoryExists(TargetDirectory) and IsJunction(TargetDirectory) then
  begin
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
  if IsWindowsVistaOrGreater then
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

initialization
  if not FileExists(JunctionFileName) then
    JunctionFileName := ExtractEmbeddedFileToWorkingPath(
      EMBEDDED_RESOURCE_JUNCTION, EMBEDDED_FILENAME_JUNCTION);

finalization
  KillFile(JunctionFileName);

end.

