unit FSTools; // File System Tools

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

type
  { TParseInputFileSystemObjectBehaviour }
  TParseInputFileSystemObjectBehaviour = (
    pifsobNoAlteration,
    pifsobIncludeTrailingPathDelimiter,
    pifsobExcludeTrailingPathDelimiter
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
function GetApplicationPath: TFileName;
function GetFileHash(const FileName: TFileName): string;
function GetProgramName: string;
function GetWorkingPath: TFileName;
function GetUsersDirectory: TFileName;
function GetAppDataListFromUsers(var UserAppDataList: TStringList): Boolean;
function GetFileDate(const FileName: TFileName): TDateTime;
function GetTemporaryFileName: TFileName;
function GetUserFromAppDataDirectory(const AppDataDirectory: TFileName): string;
function IsCorrectFileHash(const FileName: TFileName; const Hash: string): Boolean;
function KillDirectory(const DirectoryName: TFileName): Boolean;
function KillFile(const FileName: TFileName): Boolean;
function LoadFileToString(const FileName: TFileName): string;
function LoadUTF16FileToString(const FileName: TFileName): string;
function ParseInputFileSystemObject(const Parameter: TFileName;
  const Behaviour: TParseInputFileSystemObjectBehaviour = pifsobNoAlteration): TFileName;
function PatchTextFile(const FileName: TFileName; OldValue, NewValue: string): Boolean;
procedure SaveStringToFile(const InString: string; FileName: TFileName); overload;
procedure SaveStringToFile(const InString: string; FileName: TFileName; const Append: Boolean); overload;
function SetDirectoryRights(const DirectoryFullPath: TFileName;
  const UserName, Rights: string): Boolean;
function SystemToUnixPath(const SystemPathName: TFileName): TFileName;
function UncompressZipFile(const FileName, OutputDirectory: TFileName): Boolean;
function UnixPathToSystem(const UnixPathName: TFileName): TFileName;

implementation

uses
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
  Zipper,
  SysTools,
  RunTools,
  Version;

var
  ApplicationPath: TFileName = '';
  WorkingPath: TFileName = '';

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

function GetUsersDirectory: TFileName;
var
  CurrentUserName: string;

begin
  CurrentUserName := GetEnvironmentVariable('USERNAME');
  Result := IncludeTrailingPathDelimiter(Left(DirectorySeparator
    + CurrentUserName, GetEnvironmentVariable('APPDATA')));
end;

function GetAppDataTemplate: TFileName;
var
  AppDataTemplate: TFileName;
  CurrentUserName: string;

begin
  AppDataTemplate := IncludeTrailingPathDelimiter(
    ParseInputFileSystemObject('%AppData%'));
  CurrentUserName := GetEnvironmentVariable('USERNAME');

  AppDataTemplate := StringReplace(
    AppDataTemplate,
    DirectorySeparator + CurrentUserName + DirectorySeparator,
    DirectorySeparator + '%s' + DirectorySeparator,
    [rfIgnoreCase]
  );

  Result := AppDataTemplate;
end;

// {$DEFINE DEBUG_USER_APP_DATA_LIST}
function GetAppDataListFromUsers(var UserAppDataList: TStringList): Boolean;
var
  AppDataTemplate: TFileName;
  i: Integer;

begin
  Result := GetUserList(UserAppDataList);
  if Result then
  begin
{$IFDEF DEBUG_USER_APP_DATA_LIST}
{$IFDEF DEBUG}
    DebugLog('UserAppDataList:');
{$ENDIF}
{$ENDIF}

    AppDataTemplate := GetAppDataTemplate;
    for i := 0 to UserAppDataList.Count - 1 do
    begin
      UserAppDataList[i] := Format(AppDataTemplate, [UserAppDataList[i]]);
{$IFDEF DEBUG_USER_APP_DATA_LIST}
{$IFDEF DEBUG}
      DebugLog('  ' + UserAppDataList[i]);
{$ENDIF}
{$ENDIF}
    end;
  end;
end;

function GetUserFromAppDataDirectory(const AppDataDirectory: TFileName): string;
var
  AppDataTemplate: TFileName;
  TempLeft, TempRight: string;

begin
  AppDataTemplate := GetAppDataTemplate;
  TempLeft := Left('%s', AppDataTemplate);
  TempRight := Right('%s', AppDataTemplate);
  Result := Trim(ExtractStr(TempLeft, TempRight, AppDataDirectory));
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

function PatchTextFile(const FileName: TFileName; OldValue, NewValue: string): Boolean;
var
  Buffer: TStringList;

begin
  Result := False;
  if FileExists(FileName) then
  begin
    Buffer := TStringList.Create;
    try
      Buffer.LoadFromFile(FileName);
      if IsInString(OldValue, Buffer.Text) then
      begin
        Buffer.Text := StringReplace(Buffer.Text, OldValue, NewValue, [rfReplaceAll]);
        Buffer.SaveToFile(FileName);
        Result := True;
      end;
    finally
      Buffer.Free;
    end;
  end;
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
  Buffer := TStringList.Create;
  try
    Buffer.LoadFromFile(FileName);
    Result := Trim(Buffer.Text);
  finally
    Buffer.Free;
  end;
end;

procedure SaveStringToFile(const InString: string; FileName: TFileName; const Append: Boolean);
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

procedure SaveStringToFile(const InString: string; FileName: TFileName); overload;
begin
  SaveStringToFile(InString, FileName, False);
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
begin
  Result := StringReplace(SystemPathName, DirectorySeparator, '/', [rfReplaceAll]);
  Result := '/' + StringReplace(Result, ':', EmptyStr, [rfReplaceAll]);
end;

function GetWorkingPath: TFileName;
begin
  Result := WorkingPath;
end;

function GetTemporaryFileName: TFileName;
begin
  Result := ChangeFileExt(SysUtils.GetTempFileName, Format('-%s-%d-dreamsdk.tmp', [
    GetProgramName,
    GetProcessID
  ]));
{$IFDEF DEBUG}
  DebugLog('GetTemporaryFileName: "' + Result + '"');
{$ENDIF}
end;

function GetFileDate(const FileName: TFileName): TDateTime;
const
  c_UnassignedDate = -693594;

var
  BuildDate: Integer;

begin
  Result := c_UnassignedDate;
  BuildDate := FileAge(FileName);
  if BuildDate <> -1 then
    Result := FileDateToDateTime(BuildDate);
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
end;

procedure InitializeWorkingPath;
var
  TempVariable: TFileName;

begin
  TempVariable := LowerCase(ChangeFileExt(GetTemporaryFileName, EmptyStr));
  WorkingPath := IncludeTrailingPathDelimiter(TempVariable);
  ForceDirectories(WorkingPath);
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

