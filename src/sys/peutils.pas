unit PEUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

type
  TPortableExecutableBitness = (pebUnknown, peb16, peb32, peb64);

function GetPortableExecutableBitness(const APath: WideString): TPortableExecutableBitness;
function GetPortableExecutableBitness(const APath: TFileName): TPortableExecutableBitness;
function GetPortableExecutableModules(const FileName: TFileName;
  var ModulesList: TStringList): Boolean;

implementation

uses
  Windows,
  JwaWinNT,
  JwaImageHlp;

// Thanks ASerge
// See: https://forum.lazarus.freepascal.org/index.php?topic=36834.0
function GetPortableExecutableBitness(const APath: WideString): TPortableExecutableBitness;
const
  IMAGE_NT_OPTIONAL_HDR32_MAGIC = $10b;
  IMAGE_NT_OPTIONAL_HDR64_MAGIC = $20b;

var
  HFile, HFileMap: THandle;
  PMapView: Pointer;
  PIDH: Windows.PImageDosHeader;
  PINTH: PImageNtHeaders;

begin
  Result := pebUnknown;
  HFile := CreateFileW(PWideChar(APath), GENERIC_READ, FILE_SHARE_READ, nil,
    OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if HFile = INVALID_HANDLE_VALUE then
    Exit;
  HFileMap  := CreateFileMapping(HFile, nil, PAGE_READONLY, 0, 0, nil);
  CloseHandle(HFile);
  if HFileMap = 0 then
    Exit;
  PMapView := MapViewOfFile(HFileMap, FILE_MAP_READ, 0, 0, 0);
  CloseHandle(HFileMap);
  if PMapView = nil then
    Exit;
  PIDH := Windows.PImageDosHeader(PMapView);
  if PIDH^.e_magic = IMAGE_DOS_SIGNATURE then
  begin
    PINTH := PImageNtHeaders(PAnsiChar(PMapView) + PIDH^._lfanew);
    if PINTH^.Signature <> IMAGE_NT_SIGNATURE then
      Result := peb16
    else
      case PINTH^.OptionalHeader.Magic of
        IMAGE_NT_OPTIONAL_HDR32_MAGIC:
          Result := peb32;
        IMAGE_NT_OPTIONAL_HDR64_MAGIC:
          Result := peb64;
      end;
  end;
  UnmapViewOfFile(PMapView);
end;

function GetPortableExecutableBitness(const APath: TFileName): TPortableExecutableBitness;
begin
  Result := GetPortableExecutableBitness(WideString(APath));
end;

function RVAToPointer(RVA: DWORD; const Image : TLoadedImage): Pointer;
var
  Dummy: PImageSectionHeader;

begin
  Dummy := nil;
  Result := ImageRVAToVA(Image.FileHeader, Image.MappedAddress, RVA, Dummy);
  if not Assigned(Result) then
    RaiseLastWin32Error;
end;

function RVAToPChar(RVA: DWORD; const Image: TLoadedImage): PChar;
begin
  Result := RVAToPointer(RVA, Image);
end;

// Thanks to CyprUS and menjaraz
// https://stackoverflow.com/questions/10241901/how-to-read-import-directory-table-by-imagedirectoryentrytodata-in-delphi-7
function GetPortableExecutableModules(const FileName: TFileName;
  var ModulesList: TStringList): Boolean;
var
  Image: TLoadedImage;
  ImportDirectory: PImageImportDecriptor;
  Dummy: LongWord;
  ModuleName: string;

begin
  Result := False;
  Dummy := 0;
  Image := Default(TLoadedImage);
  if Assigned(ModulesList) and FileExists(FileName)
    and MapAndLoad(PAnsiChar(filename), nil, Image, True, True) then
  begin
    try
      ImportDirectory := ImageDirectoryEntryToData(Image.MappedAddress,
        False, IMAGE_DIRECTORY_ENTRY_IMPORT, Dummy);
      if Assigned(ImportDirectory) then
      begin
        while ImportDirectory^.Name <> 0 do
        begin
          ModuleName := RVAToPChar(DWord(ImportDirectory^.Name), Image);
          if ModulesList.IndexOf(ModuleName) = -1 then
            ModulesList.Add(ModuleName);
          Inc(ImportDirectory);
        end;
        ModulesList.Sort;
        Result := True;
      end;
    finally
      UnMapAndLoad(Image);
    end
  end;
end;

end.

