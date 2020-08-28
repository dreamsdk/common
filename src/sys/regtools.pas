unit RegTools;

interface

uses
  Classes, SysUtils, Registry;

function RegistryReadMultiSzToSingle(Registry: TRegistry; const KeyName: string): string;

implementation

uses
  Windows;

// Thanks to Thomas Stutz
// https://www.swissdelphicenter.ch/en/showcode.php?id=1431
procedure ReadREG_MULTI_SZ(const CurrentKey: HKey; const Subkey, ValueName: string;
  Strings: TStrings);
var
  valueType: DWORD;
  valueLen: DWORD;
  p, buffer: PChar;
  key: HKEY;

begin
  Key := Default(HKEY);

  // Clear TStrings
  // TStrings leeren
  Strings.Clear;
  // open the specified key
  // CurrentKey Schlüssel öffnen
  if RegOpenKeyEx(CurrentKey,
                  PChar(Subkey),
                  0, KEY_READ, key) = ERROR_SUCCESS then
  begin
    // retrieve the type and data for a specified value name
    // Den Typ und Wert des Eintrags Ermitteln.
    SetLastError(RegQueryValueEx(key,
                 PChar(ValueName),
                 nil,
                 @valueType,
                 nil,
                 @valueLen));
    if GetLastError = ERROR_SUCCESS then
      if valueType = REG_MULTI_SZ then
      begin
        GetMem(buffer, valueLen);
        try
          // receive the value's data (in an array).
          // Ein Array von Null-terminierten Strings
          // wird zurückgegeben
          RegQueryValueEx(key,
                          PChar(ValueName),
                          nil,
                          nil,
                          PBYTE(buffer),
                          @valueLen);
          // Add values to stringlist
          // Werte in String Liste einfügen
          p := buffer;
          while p^ <> #0 do
          begin
            Strings.Add(p);
            Inc(p, lstrlen(p) + 1)
          end
        finally
          FreeMem(buffer)
        end
      end
      else
        raise ERegistryException.Create('StringList expected')
    else
      raise ERegistryException.Create('Cannot read REG_MULTI_SZ value');
  end;
end;

function RegistryReadMultiSzToSingle(Registry: TRegistry; const KeyName: string): string;
var
  Buffer: TStringList;

begin
  Buffer := TStringList.Create;
  try
    ReadREG_MULTI_SZ(Registry.RootKey, Registry.CurrentPath, KeyName, Buffer);
    Result := Trim(Buffer.Text);
  finally
    Buffer.Free;
  end;
end;
end.
