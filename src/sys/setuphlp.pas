unit SetupHlp;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils,
  Classes;

function WriteSharedAnsiString(const Source: string;
  Destination: PAnsiChar; const MaxLength: UInt32): UInt32;
function WriteSharedWideString(const Source: string;
  Destination: PWideChar; const MaxLength: UInt32): UInt32;

implementation

uses
  LazUTF8;

function WriteSharedAnsiString(const Source: string;
  Destination: PAnsiChar; const MaxLength: UInt32): UInt32;

begin
  Result := 0;
  if Assigned(Destination) and (MaxLength > 0) then
  begin
    StrPLCopy(Destination, Source, MaxLength);
    Result := Length(Destination);
  end;
end;

function WriteSharedWideString(const Source: string;
  Destination: PWideChar; const MaxLength: UInt32): UInt32;
var
  Buffer: UnicodeString;

begin
  Result := 0;
  if Assigned(Destination) and (MaxLength > 0) then
  begin
    Buffer := UnicodeString(Source);
    StrPLCopy(Destination, Buffer, MaxLength);
    Result := Length(Destination);
  end;
end;

end.

