unit FSTools; // File System Tools

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
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
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TFileName read GetItem; default;
  end;

function GetApplicationPath: TFileName;
function GetProgramName: string;
function KillFile(const FileName: TFileName): Boolean;
function LoadFileToString(FileName: TFileName): string;
function LoadUTF16FileToString(const FileName: TFileName): string;
function ParseInputFileSystemObject(const Parameter: TFileName): TFileName;
function PatchTextFile(const FileName: TFileName; OldValue, NewValue: string): Boolean;
procedure SaveStringToFile(const InString: string; FileName: TFileName);
function SystemToUnixPath(const UnixPathName: TFileName): TFileName;
function UncompressZipFile(const FileName, OutputDirectory: TFileName): Boolean;
function UnixPathToSystem(const PathName: TFileName): TFileName;

function GetUserAppDataList(var UserAppDataList: TStringList): Boolean;

implementation

uses
  StrUtils,
  RegExpr,
  LazUTF8,
  LConvEncoding,
  LazFileUtils,
{$IFDEF GUI}
  Forms,
{$ENDIF}
  Zipper,
  SysTools;

var
  ApplicationPath: TFileName = '';

function GetUserAppDataList(var UserAppDataList: TStringList): Boolean;
var
  AppDataTemplate: TFileName;
  CurrentUserName: string;
  i: Integer;

begin
  AppDataTemplate := ParseInputFileSystemObject('%AppData%');
  CurrentUserName := GetEnvironmentVariable('USERNAME');

  AppDataTemplate := StringReplace(
    AppDataTemplate,
    DirectorySeparator + CurrentUserName + DirectorySeparator,
    DirectorySeparator + '%s' + DirectorySeparator,
    [rfIgnoreCase]
  );

  Result := GetUserList(UserAppDataList);

  for i := 0 to UserAppDataList.Count - 1 do
    UserAppDataList[i] := Format(AppDataTemplate, [UserAppDataList[i]]);
end;

function GetProgramName: string;
begin
  Result := ExtractFileNameOnly(ParamStr(0));
end;

function ParseInputFileSystemObject(const Parameter: TFileName): TFileName;
begin
  Result := ExpandFileName(ExpandEnvironmentStrings(Parameter));
end;

function KillFile(const FileName: TFileName): Boolean;
begin
  Result := False;
  if FileExists(FileName) then
    Result := SysUtils.DeleteFile(FileName);
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

function LoadFileToString(FileName: TFileName): string;
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

procedure SaveStringToFile(const InString: string; FileName: TFileName);
var
  Buffer: TStringList;

begin
  Buffer := TStringList.Create;
  try
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

function UnixPathToSystem(const PathName: TFileName): TFileName;
begin
  Result := StringReplace(PathName, '/', DirectorySeparator, [rfReplaceAll]);
  Result := IncludeTrailingPathDelimiter(Copy(Result, 2, Length(Result) - 1));
end;

function SystemToUnixPath(const UnixPathName: TFileName): TFileName;
begin
  Result := StringReplace(UnixPathName, DirectorySeparator, '/', [rfReplaceAll]);
  Result := '/' + StringReplace(Result, ':', EmptyStr, [rfReplaceAll]);
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

end.

