unit FSTools; // File System Tools

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

type
  EFileSystemTools = class(Exception);
  ERenameFileOrDirectoryAsBackupException = class(EFileSystemTools);

  { TParseInputFileSystemObjectBehaviour }
  TParseInputFileSystemObjectBehaviour = (
    pifsobNoAlteration,
    pifsobIncludeTrailingPathDelimiter,
    pifsobExcludeTrailingPathDelimiter
  );

  { TPatchTextFileBehaviour }
  TPatchTextFileBehaviour = (
    ptfbAlwaysPatch,
    ptfbPatchWithWatermark
  );

  { TFileListItem }
  TFileListItem = class(TObject)
  private
    fFileName: TFileName;
  public
    constructor Create(const AFileName: TFileName);
    property FileName: TFileName read fFileName;
  end;

  { TFileList }
  TFileList = class(TObject)
  private
    fList: TList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TFileName;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(const FileName: TFileName);
    procedure Add(const FileNames, Delimiter: string);
    procedure Assign(ASource: TFileList);
    function GetItems(const Delimiter: string): string;
    procedure SetItems(const Values, Delimiter: string);
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TFileName read GetItem; default;
  end;

function ExtractDirectoryName(const DirectoryName: string): string;
function ExtractEmbeddedFileToWorkingPath(const ResourceName: string;
  const FileName: TFileName): TFileName;
function ExtractFileFromAr(const ArchiveFileName,
  FileNameToExtract: TFileName; var ADestination: TByteArray): Boolean;
function GetApplicationPath: TFileName;
function GetFileHash(const FileName: TFileName): string;
function GetProgramName: string;
function GetWorkingPath: TFileName;
function GetFileDate(const FileName: TFileName): TDateTime;
function GetTemporaryFileName: TFileName;
function IsCorrectFileHash(const FileName: TFileName; const Hash: string): Boolean;
function KillDirectory(const DirectoryName: TFileName): Boolean;
function KillFile(const FileName: TFileName): Boolean;
function LoadFileToString(const FileName: TFileName): string;
function LoadUTF16FileToString(const FileName: TFileName): string;
function ParseInputFileSystemObject(const Parameter: TFileName;
  const Behaviour: TParseInputFileSystemObjectBehaviour = pifsobNoAlteration): TFileName;
function PatchTextFile(const FileName: TFileName; OldValue, NewValue: string;
  const Behaviour: TPatchTextFileBehaviour): Boolean; overload;
function PatchTextFile(const FileName: TFileName; OldValue, NewValue: string;
  const Behaviour: TPatchTextFileBehaviour;
  const WatermarkPrefix: string): Boolean; overload;
function PatchTextFile(const FileName: TFileName; OldValue, NewValue: string;
  const Behaviour: TPatchTextFileBehaviour;
  const WatermarkPrefix, WatermarkSuffix: string): Boolean; overload;
(* Renames the passed file or directory in parameter, with a ".old" suffix *)
function RenameFileOrDirectoryAsBackup(const TargetFileOrDirectory: TFileName): Boolean; overload;
function RenameFileOrDirectoryAsBackup(const TargetFileOrDirectory: TFileName;
  var NewTargetPath: TFileName): Boolean; overload;
procedure SaveStringToFile(const InString: string; FileName: TFileName;
  const Append: Boolean = False);
function SetDirectoryRights(const DirectoryFullPath: TFileName;
  const UserName, Rights: string): Boolean;
function SystemToUnixPath(const SystemPathName: TFileName): TFileName;
function UncompressZipFile(const FileName, OutputDirectory: TFileName): Boolean;
function UnixPathToSystem(const UnixPathName: TFileName): TFileName;

implementation

uses
  Windows,
  ShellApi,
  StrUtils,
  RegExpr,
  LazUTF8,
  LConvEncoding,
  LazFileUtils,
  FileUtil,
  MD5,
{$IFDEF GUI}
  Interfaces,
  Forms,
{$ENDIF}
  SysTools,
  StrTools,
  Zipper,
  RunTools,
  Version;

var
  ApplicationPath: TFileName = Default(TFileName);
  WorkingPath: TFileName = Default(TFileName);

function GetFileHash(const FileName: TFileName): string;
begin
  Result := MD5Print(MD5File(FileName));
end;

function IsCorrectFileHash(const FileName: TFileName; const Hash: string): Boolean;
begin
  Result := SameText(GetFileHash(FileName), Hash);
end;

function ExtractDirectoryName(const DirectoryName: string): string;
begin
  Result := ExtremeRight(DirectorySeparator,
    ExcludeTrailingPathDelimiter(DirectoryName));
end;

function GetProgramName: string;
begin
  Result := ExtractFileNameOnly(ParamStr(0));
end;

function ParseInputFileSystemObject(const Parameter: TFileName;
  const Behaviour: TParseInputFileSystemObjectBehaviour = pifsobNoAlteration): TFileName;
begin
  Result := ExpandFileName(ExpandEnvironmentStrings(Parameter));
  case Behaviour of
    pifsobIncludeTrailingPathDelimiter:
      Result := IncludeTrailingPathDelimiter(Result);
    pifsobExcludeTrailingPathDelimiter:
      Result := ExcludeTrailingPathDelimiter(Result);
  end;
end;

function KillFile(const FileName: TFileName): Boolean;
begin
  Result := False;
  if FileExists(FileName) then
    Result := SysUtils.DeleteFile(FileName);
end;

// See: https://forum.lazarus.freepascal.org/index.php/topic,16093.msg87124.html#msg87124
function KillDirectory(const DirectoryName: TFileName): Boolean;
// Lazarus fileutil.DeleteDirectory on steroids, works like
// deltree <directory>, rmdir /s /q <directory> or rm -rf <directory>
// - removes read-only files/directories (DeleteDirectory doesn't)
// - removes directory itself
// Adapted from fileutil.DeleteDirectory, thanks to Paweł Dmitruk
var
  FileInfo: TSearchRec;
  CurSrcDir,
  CurFileName: string;

begin
  Result := False;

  // Fail-safe: don't continue if DirectoryName is empty!
  if IsEmpty(DirectoryName) then
    Exit;

  CurSrcDir := CleanAndExpandDirectory(DirectoryName);
  if DirectoryExists(CurSrcDir) then
  begin
    if FindFirstUTF8(CurSrcDir + GetAllFilesMask, faAnyFile, FileInfo) = 0 then
    begin
      repeat
        // Ignore directories and files without name:
        if (FileInfo.Name <> '.') and (FileInfo.Name <> '..') and (FileInfo.Name <> '') then
        begin
          // Remove all files and directories in this directory:
          CurFileName := CurSrcDir + FileInfo.Name;
          // Remove read-only file attribute so we can delete it:
          if (FileInfo.Attr and faReadOnly) > 0 then
            FileSetAttrUTF8(CurFileName, FileInfo.Attr - faReadOnly);
          if (FileInfo.Attr and faDirectory) > 0 then
          begin
            // Directory; exit with failure on error
            if not KillDirectory(CurFileName) then Exit;
          end
          else
          begin
            // File; exit with failure on error
            if not DeleteFileUTF8(CurFileName) then Exit;
          end;
        end;
      until FindNextUTF8(FileInfo) <> 0;
    end;
    FindCloseUTF8(FileInfo);

    // Remove "root" directory
    RemoveDirUTF8(DirectoryName);

    // Verify if the required directory exists
    Result := not DirectoryExists(DirectoryName);
  end;
end;

// Thanks to: GetMem
// https://forum.lazarus.freepascal.org/index.php?topic=30553.0
function LoadUTF16FileToString(const FileName: TFileName): string;
var
  MS: TMemoryStream;
  S: string;

begin
  Result := #0;
  if FileExists(FileName) then
  begin
    MS := TMemoryStream.Create;
    try
      MS.LoadFromFile(FileName);
      MS.Position := 0;

      // UTF-16 to UTF-8 with BOM
      S := UTF16ToUTF8(PWideChar(MS.Memory), MS.Size div SizeOf(WideChar));

      // UTF-8 without BOM
      Result := string(UTF8BOMToUTF8(S));
    finally
      MS.Free;
    end;
  end;
end;

function PatchTextFile(const FileName: TFileName; OldValue, NewValue: string;
  const Behaviour: TPatchTextFileBehaviour;
  const WatermarkPrefix, WatermarkSuffix: string): Boolean; overload;
const
  WATERMARK_HEADER  = 'This file has been patched by DreamSDK Manager';
  WATERMARK_STAMP   = 'dreamsdk-patch';

var
  Buffer: TStringList;
  PatchWatermark,
  PatchWatermarkHeader: string;
  HeaderAdded: Boolean;

begin
  Result := False;
  HeaderAdded := False;
  if FileExists(FileName) then
  begin
    Buffer := TStringList.Create;
    try
      Buffer.LoadFromFile(FileName);
      if IsInString(OldValue, Buffer.Text) then
      begin
        // Do something only if the old value has been found

        // Compute unique watermark code for specific instruction
        PatchWatermark := MD5Print(MD5String(OldValue + NewValue));

        // Patch if always required (ptfbAlwaysPatch) or if the PatchWatermark is not found
        if (Behaviour = ptfbAlwaysPatch)
          or ((Behaviour = ptfbPatchWithWatermark) and (not IsInString(PatchWatermark, Buffer.Text))) then
        begin
          // This is just used for adding a 'warning' in the file beginning
          if (Behaviour = ptfbPatchWithWatermark) and (not IsInString(WATERMARK_HEADER, Buffer.Text)) then
          begin
            Buffer.Insert(0, Format('%s %s%s', [WatermarkPrefix, WATERMARK_HEADER, sLineBreak]));
            HeaderAdded := True;
          end;

          // Apply the patch for real
          Buffer.Text := StringReplace(Buffer.Text, OldValue, NewValue, [rfReplaceAll]);

          // Save the watermark code in the file
          if (Behaviour = ptfbPatchWithWatermark) then
          begin
            PatchWatermarkHeader := EmptyStr;
            if HeaderAdded then
              PatchWatermarkHeader := sLineBreak;
            Buffer.Add(Format('%s%s %s: %s%s', [
              PatchWatermarkHeader, // sLineBreak or EmptyStr
              WatermarkPrefix,      // # or <!--
              WATERMARK_STAMP,      // Fixed value (e.g., "code")
              PatchWatermark,       // Watermark value
              WatermarkSuffix       // EmptyStr or -->
            ]));
          end;

          // Save the patched file
          Buffer.SaveToFile(FileName);
        end;

        Result := True;
      end;
    finally
      Buffer.Free;
    end;
  end;
end;

function PatchTextFile(const FileName: TFileName; OldValue, NewValue: string;
  const Behaviour: TPatchTextFileBehaviour): Boolean; overload;
begin
  Result := PatchTextFile(FileName, OldValue, NewValue, Behaviour, '#',
    EmptyStr);
end;

function PatchTextFile(const FileName: TFileName; OldValue, NewValue: string;
  const Behaviour: TPatchTextFileBehaviour;
  const WatermarkPrefix: string): Boolean; overload;
begin
  Result := PatchTextFile(FileName, OldValue, NewValue, Behaviour,
    WatermarkPrefix, EmptyStr);
end;

function UncompressZipFile(const FileName, OutputDirectory: TFileName): Boolean;
var
  UnZipper: TUnZipper;

begin
  Result := False;
  if FileExists(FileName) then
  begin
    UnZipper := TUnZipper.Create;
    try
      UnZipper.FileName := FileName;
      UnZipper.OutputPath := OutputDirectory;
      UnZipper.Examine;
      UnZipper.UnZipAllFiles;
      Result := True;
    finally
      UnZipper.Free;
    end;
  end;
end;

function LoadFileToString(const FileName: TFileName): string;
var
  Buffer: TStringList;

begin
  Result := EmptyStr;
  if FileExists(FileName) then
  begin
    Buffer := TStringList.Create;
    try
      Buffer.LoadFromFile(FileName);
      Result := Trim(Buffer.Text);
    finally
      Buffer.Free;
    end;
  end;
end;

procedure SaveStringToFile(const InString: string; FileName: TFileName; const Append: Boolean = False);
var
  Buffer: TStringList;

begin
  Buffer := TStringList.Create;
  try
    if Append and FileExists(FileName) then
      Buffer.LoadFromFile(FileName);
    Buffer.Add(InString);
    Buffer.SaveToFile(FileName);
  finally
    Buffer.Free;
  end;
end;

function GetApplicationPath: TFileName;
var
  Path: TFileName;
{$IFDEF Darwin}
  i: Integer;
{$ENDIF}

begin
  if (ApplicationPath = EmptyStr) then
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
    ApplicationPath := IncludeTrailingPathDelimiter(Path);
  end;
  Result := ApplicationPath;
end;

function ExtractEmbeddedFileToWorkingPath(const ResourceName: string;
  const FileName: TFileName): TFileName;
begin
  Result := GetWorkingPath + FileName;
  ExtractEmbeddedResourceToFile(ResourceName, Result);
end;

function UnixPathToSystem(const UnixPathName: TFileName): TFileName;
var
  AttemptDriveLetter,
  AttemptPartPath,
  AttemptFullPath: TFileName;

begin
  Result := StringReplace(UnixPathName, '/', DirectorySeparator, [rfReplaceAll]);
  Result := IncludeTrailingPathDelimiter(Copy(Result, 2, Length(Result) - 1));
  if not DirectoryExists(Result) then
  begin
    AttemptDriveLetter := Left(DirectorySeparator, Result);
    AttemptPartPath := Right(DirectorySeparator, Result);
    AttemptFullPath := AttemptDriveLetter + ':' +  DirectorySeparator + AttemptPartPath;
    if DirectoryExists(AttemptFullPath) then
      Result := AttemptFullPath;
  end;
end;

function SystemToUnixPath(const SystemPathName: TFileName): TFileName;
var
  WorkingPath: string;
  IsAbsolutePath: Boolean;
  PrefixPart: string;
  PathPart: string;
  i: Integer;

begin
  Result := SystemPathName;

  // Check if there are any directory separators to process
  if not IsInString(DirectorySeparator, SystemPathName) then
    Exit;

  WorkingPath := SystemPathName;
  PrefixPart := '';
  PathPart := WorkingPath;

  // Check for argument prefixes (like -T, -I, -L, etc.)
  if (Length(WorkingPath) > 1) and (WorkingPath[1] = '-') then
  begin
    // Find where the path starts (look for drive letter pattern)
    for i := 2 to Length(WorkingPath) - 1 do
    begin
      if (WorkingPath[i] in ['A'..'Z', 'a'..'z']) and
         (i < Length(WorkingPath)) and (WorkingPath[i + 1] = ':') then
      begin
        PrefixPart := Copy(WorkingPath, 1, i - 1);
        PathPart := Copy(WorkingPath, i, Length(WorkingPath));
        Break;
      end;
    end;
  end;

  // Determine if the path part is an absolute path
  // Absolute paths on Windows start with drive letter followed by colon
  // or UNC paths starting with \\
  IsAbsolutePath := (Length(PathPart) >= 2) and
                   (((PathPart[1] in ['A'..'Z', 'a'..'z']) and (PathPart[2] = ':')) or
                    (LeftStr(PathPart, 2) = '\\'));

  // Replace all directory separators with forward slashes in the path part
  PathPart := StringReplace(PathPart, DirectorySeparator, '/', [rfReplaceAll]);

  // Handle absolute paths
  if IsAbsolutePath then
  begin
    // For drive letter paths (C:\path), convert to /c/path format
    if (Length(PathPart) >= 2) and (PathPart[2] = ':') then
    begin
      PathPart := '/' + LowerCase(PathPart[1]) + Copy(PathPart, 3, Length(PathPart));
    end
    // For UNC paths (\\server\share), convert to //server/share
    else if LeftStr(PathPart, 2) = '//' then
    begin
      // UNC paths are already in correct format after separator replacement
      // Just ensure it starts with // (which it should)
    end;
  end;
  // Relative paths remain unchanged (no leading slash added)

  // Combine prefix and converted path
  Result := PrefixPart + PathPart;
end;

function GetWorkingPath: TFileName;
begin
  Result := WorkingPath;
end;

// {$DEFINE DEBUG_GET_TEMPORARY_FILE_NAME}
function GetTemporaryFileName: TFileName;
begin
  Result := ChangeFileExt(SysUtils.GetTempFileName, Format('-%s-%d-dreamsdk.tmp', [
    GetProgramName,
    GetProcessID
  ]));
{$IFDEF DEBUG}
{$IFDEF DEBUG_TEMPORARY_FILE_GENERATION}
  DebugLog('GetTemporaryFileName: "' + Result + '"');
{$ENDIF}
{$ENDIF}
end;

function GetFileDate(const FileName: TFileName): TDateTime;
var
  BuildDate: Integer;

begin
  Result := Default(TDateTime);
  if FileExists(FileName) then
  begin
    BuildDate := FileAge(FileName);
    if BuildDate <> -1 then
      Result := FileDateToDateTime(BuildDate);
  end;
end;

function SetDirectoryRights(const DirectoryFullPath: TFileName;
  const UserName, Rights: string): Boolean;
var
  CommandLine: string;

begin
{$IFDEF Windows}
  Result := False;

  CommandLine := 'echo Y | cacls "%s" /E /G "%s":%s';
  if IsWindowsVistaOrGreater then
    CommandLine := 'icacls "%s" /q /c /t /grant "%s":%s';

  CommandLine := Format(CommandLine, [
    ExcludeTrailingPathDelimiter(DirectoryFullPath),
    UserName,
    Rights
  ]);

{$IFDEF DEBUG}
  DebugLog('SetDirectoryRights: ' + CommandLine);
{$ENDIF}

  Result := RunSingleCommand(CommandLine);
{$ELSE}
  Result := False;
{$IFDEF DEBUG}
  WriteLn('SetDirectoryRights: Not implemented');
{$ENDIF}
{$ENDIF}

{$IFDEF DEBUG}
  DebugLog('SetDirectoryRights: Result = ' + BoolToStr(Result, True));
{$ENDIF}
end;

procedure InitializeWorkingPath;
var
  TempVariable: TFileName;

begin
  TempVariable := LowerCase(ChangeFileExt(GetTemporaryFileName, EmptyStr));
  WorkingPath := IncludeTrailingPathDelimiter(TempVariable);
  ForceDirectories(WorkingPath);
end;

function ExtractFileFromAr(const ArchiveFileName,
  FileNameToExtract: TFileName; var ADestination: TByteArray): Boolean;
type
  TArFileSignature = array[0..7] of Char;
  TArFileHeader = packed record
    Name: array[0..15] of Char;
    ModTime: array[0..11] of Char;
    OwnerID: array[0..5] of Char;
    GroupID: array[0..5] of Char;
    Mode: array[0..7] of Char;
    Size: array[0..9] of Char;
    Magic: array[0..1] of Char;
  end;

const
  FILE_SIGNATURE = '!<arch>'#$0A;

var
  Signature: TArFileSignature;
  ArchiveFile: TFileStream;
  Header: TArFileHeader;
  FileName: TFileName;
  FileSize: Int64;
  FileData: TMemoryStream;
  Found: Boolean;

begin
  Result := False;
  Signature := Default(TArFileSignature);

  if FileExists(ArchiveFileName) then
  begin
    ArchiveFile := TFileStream.Create(ArchiveFileName, fmOpenRead or fmShareDenyNone);
    try
      // Check if we have a correct ar file
      ArchiveFile.Read(Signature, SizeOf(TArFileSignature));
      if not SameText(FILE_SIGNATURE, Signature) then
        Exit;

      // Check every files in the ar file
      Found := False;
      Header := Default(TArFileHeader);
      while ArchiveFile.Position < ArchiveFile.Size do
      begin
        // Read the header
        ArchiveFile.Read(Header, SizeOf(Header));

        // Check magic number
        if (Header.Magic[0] <> '`') or (Header.Magic[1] <> #10) then
          Break;

        // Extract the name and the size of the current entry
        FileName := LowerCase(ExcludeTrailingPathDelimiter(TrimRight(string(Header.Name))));
        FileSize := StrToInt64Def(TrimRight(string(Header.Size)), 0);

        // Check if it's the file we want
        if SameText(LowerCase(FileNameToExtract), LowerCase(FileName)) then
        begin
          Found := True;
          Break;
        end;

        // Next file
        ArchiveFile.Seek(FileSize, soFromCurrent);

        // Align if needed (padding could be necessary)
        if (ArchiveFile.Position mod 2) <> 0 then
          ArchiveFile.Seek(1, soFromCurrent);
      end;

      // Extract the found file from the ar file
      if Found then
      begin
        FileData := TMemoryStream.Create;
        try
          FileData.CopyFrom(ArchiveFile, FileSize);
          FileData.Seek(0, soFromBeginning);
          FileData.Read(ADestination, FileSize);
          Result := True;
        finally
          FileData.Free;
        end;
      end;
    finally
      ArchiveFile.Free;
    end;
  end;
end;

function RenameFileOrDirectoryAsBackup(const TargetFileOrDirectory: TFileName): Boolean; overload;
var
  NewTargetPath: TFileName;

begin
  NewTargetPath := Default(string);
  Result := RenameFileOrDirectoryAsBackup(TargetFileOrDirectory, NewTargetPath);
end;

(* This function rename the object (file or directory) passed parameter with the
 * ".old" suffix in order to keep a backup, instead of just deleting the object.
 * This is used in the DreamSDK Setup and DreamSDK Manager.
 *)
function RenameFileOrDirectoryAsBackup(const TargetFileOrDirectory: TFileName;
  var NewTargetPath: TFileName): Boolean;
const
  MAX_TRIES = 999;

var
  Count: Integer;

  SourcePath,
  CleanTargetPath,
  OldObjectName,
  NewObjectName: TFileName;

  IsTargetFile,
  IsTargetDirectory,
  ShouldContinue: Boolean;

  function _RenameWithMoveFileEx(const OldName, NewName: TFileName): Boolean;
  begin
    Result := MoveFileEx(PChar(OldName), PChar(NewName), MOVEFILE_COPY_ALLOWED);
  end;

  function _RenameWithShell(const OldName, NewName: TFileName): Boolean;
  var
    shOp: TSHFileOpStruct;
    fromBuf, toBuf: array[0..MAX_PATH] of Char;

  begin
    ZeroMemory(@shOp, SizeOf(shOp));

    StrPCopy(fromBuf, OldName + #0#0); // double null-terminated
    StrPCopy(toBuf,   NewName + #0#0);

    shOp.Wnd := 0;
    shOp.wFunc := FO_RENAME;
    shOp.pFrom := @fromBuf[0];
    shOp.pTo   := @toBuf[0];
    shOp.fFlags := FOF_NOCONFIRMATION or FOF_SILENT;

    Result := (SHFileOperation(shOp) = 0);
  end;

begin
  Result := False;

  // Cleanup input param by removing trailing slashes
  CleanTargetPath := ExcludeTrailingPathDelimiter(TargetFileOrDirectory);

  // If we have an empty string or just a drive reference we stop
  if (CleanTargetPath = EmptyStr) or (Length(CleanTargetPath) <= 3) then
    Exit;

  IsTargetFile := FileExists(CleanTargetPath);
  IsTargetDirectory := DirectoryExists(CleanTargetPath);

{$IFDEF DEBUG}
  DebugLog(Format('RenameFileOrDirectoryAsBackup [IsFile: %s, IsDir: %s]: "%s" (cleaned: "%s")', [
    BoolToStr(IsTargetFile, True),
    BoolToStr(IsTargetDirectory, True),
    TargetFileOrDirectory,
    CleanTargetPath
  ]));
{$ENDIF}

  if not IsTargetFile and not IsTargetDirectory then
    Exit;

  Count := 0;
  OldObjectName := ExtractFileName(CleanTargetPath);

  // Additional check: if ExtractFileName returns an empty string, we have an issue
  if OldObjectName = EmptyStr then
  begin
{$IFDEF DEBUG}
    DebugLog(Format('ExtractFileName returned empty string for: "%s"', [CleanTargetPath]));
{$ENDIF}
    Exit;
  end;

  SourcePath := IncludeTrailingPathDelimiter(ExtractFilePath(CleanTargetPath));

  // Find the new name
  repeat
    if (Count = 0) then
      NewObjectName := Format('%s.old', [OldObjectName])
    else
      NewObjectName := Format('%s.old.%.3d', [OldObjectName, Count]);

    NewTargetPath := SourcePath + NewObjectName;

    ShouldContinue := FileExists(NewTargetPath) or DirectoryExists(NewTargetPath);

{$IFDEF DEBUG}
    DebugLog(Format('  [ShouldContinue: %s, Counter: %d] "%s"', [
      BoolToStr(ShouldContinue, True),
      Count,
      NewTargetPath
    ]));
{$ENDIF}

    // Fail-safe
    if (Count > MAX_TRIES) then
      raise ERenameFileOrDirectoryAsBackupException.CreateFmt('Unable to rename the object: "%s"', [
        CleanTargetPath]);

    // Next try (if needed)
    Inc(Count);
  until (not ShouldContinue);

{$IFDEF DEBUG}
  DebugLog('  RenameFileOrDirectoryAsBackup defined the new name:');
  DebugLog(Format('    Old: "%s"', [
    CleanTargetPath
  ]));
  DebugLog(Format('    New: "%s"', [
    NewTargetPath
  ]));
{$ENDIF}

  // The new name has been found, in NewObjectName (full path in NewTargetPath)
  Result := RenameFile(CleanTargetPath, NewTargetPath);

  // Fail-safes for stranges cases where RenameFile gets Access Denied #5...
  if not Result then
    Result := _RenameWithMoveFileEx(CleanTargetPath, NewTargetPath);
  if not Result then
    Result := _RenameWithShell(CleanTargetPath, NewTargetPath);

{$IFDEF DEBUG}
  DebugLog(Format('  RenameFileOrDirectoryAsBackup Result: "%s"', [
    BoolToStr(Result, True)
  ]));
  DebugLog(Format('Error #%d: %s', [
    GetLastOSError,
    SysErrorMessage(GetLastOSError)
  ]));
{$ENDIF}
end;

{ TFileListItem }

constructor TFileListItem.Create(const AFileName: TFileName);
begin
  fFileName := ParseInputFileSystemObject(AFileName);
end;

{ TFileList }

function TFileList.GetItem(Index: Integer): TFileName;
var
  Item: TFileListItem;

begin
  Result := EmptyStr;
  Item := TFileListItem(fList[Index]);
  if Assigned(Item) then
    Result := Item.FileName;
end;

function TFileList.GetCount: Integer;
begin
  Result := fList.Count;
end;

constructor TFileList.Create;
begin
  fList := TList.Create;
end;

destructor TFileList.Destroy;
begin
  Clear;
  fList.Free;
  inherited Destroy;
end;

procedure TFileList.Clear;
var
  i: Integer;

begin
  for i := 0 to fList.Count - 1 do
    TFileListItem(fList[i]).Free;
  fList.Clear;
end;

procedure TFileList.Add(const FileName: TFileName);
var
  Item: TFileListItem;

begin
  Item := TFileListItem.Create(FileName);
  fList.Add(Item);
end;

procedure TFileList.Add(const FileNames, Delimiter: string);
var
  Buffer: TStringList;
  i: Integer;

begin
  Buffer := TStringList.Create;
  try
    StringToStringList(FileNames, Delimiter, Buffer);
    for i := 0 to Buffer.Count - 1 do
      Add(Buffer[i]);
  finally
    Buffer.Free;
  end;
end;

procedure TFileList.Assign(ASource: TFileList);
begin
  Clear;
  SetItems(ASource.GetItems(ArraySeparator), ArraySeparator);
end;

function TFileList.GetItems(const Delimiter: string): string;
var
  i: Integer;
  Separator: string;

begin
  Result := EmptyStr;
  Separator := EmptyStr;
  for i := 0 to Count - 1 do
  begin
    Result := Result + Separator + Items[i];
    Separator := Delimiter;
  end;
end;

procedure TFileList.SetItems(const Values, Delimiter: string);
begin
  Clear;
  Add(Values, Delimiter);
end;

initialization
  InitializeWorkingPath;

finalization
  KillDirectory(WorkingPath);

end.

