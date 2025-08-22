(*
================================================================================
  ____                         _____  ____   _____
 |    .  ___  ___  ___  _____ |   __||    . |  |  |
 |  |  ||  _|| -_|| .'||     ||__   ||  |  ||    -|
 |____/ |_|  |___||__,||_|_|_||_____||____/ |__|__|

================================================================================
DreamSDK Common Library - String/StringList
================================================================================

This file is part of DreamSDK.

DreamSDK is free software: you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

DreamSDK is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
DreamSDK. If not, see <https://www.gnu.org/licenses/>.
*)
unit StrTools;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  FGL;

(* Checks if S finishes with SubStr - reverse of StartsWith *)
function EndsWith(const SubStr, S: string): Boolean;

(* Extract a part from S, between LeftSubStr and RightSubStr *)
function ExtractStr(LeftSubStr, RightSubStr, S: string): string;

(* Extract the latest part from SubStr from S *)
function ExtremeRight(SubStr: string; S: string): string;

(* Count the number of occurences of SubStr in S *)
function GetSubStrCount(SubStr, S: string): Integer;

(* Check if S is empty *)
function IsEmpty(const S: string): Boolean;

(* Check if S is in Array *)
function IsInArray(A: TStringArray; const S: string): Boolean;

(* Check if SubStr is part of S *)
function IsInString(const SubStr, S: string): Boolean;

(* Check if RegEx applies to InputValue *)
function IsRegExMatch(const InputValue, RegEx: string): Boolean;

(* Extract the left part from S, using SubStr as delimiter - reverse of Right *)
function Left(SubStr: string; S: string): string;

(* Extract the N-part of S using SubStr *)
function LeftNRight(SubStr, S: string; N: Integer): string;

(* Extract the right part from S, using SubStr as delimiter - reverse of Left *)
function Right(SubStr: string; S: string): string;

(* Checks if S starts with SubStr - reverse of EndsWith *)
function StartsWith(const SubStr, S: string): Boolean;

(* Explode S into SL, using Delimiter - reverse of StringListToString *)
procedure StringToStringList(const S, Delimiter: string; SL: TStringList);

(* Merge SL into a single string, using Delimiter - reverse of StringToStringList *)
function StringListToString(SL: TStringList; const Delimiter: string;
  const IgnoreBlank: Boolean = True): string;

(* Seek SubStr into SL and returns the index *)
function StringListSubstringIndexOf(SL: TStringList; SubStr: string): Integer; overload;

(* Seek SubStr into SL and returns the index; case sensitive option available *)
function StringListSubstringIndexOf(SL: TStringList; SubStr: string;
  CaseSensitive: Boolean): Integer; overload;

(* Remove duplicates strings from SL *)
procedure StringListRemoveDuplicates(SL: TStringList; ProcessFromEnd: Boolean = False);

(* Super trim function for S *)
function SuppressUselessWhiteSpaces(const S: string): string;

implementation

uses
  StrUtils,
  RegExpr,
  LazUTF8,
  LConvEncoding,
{$IFDEF GUI}
  Interfaces,
  Forms,
{$ENDIF}
  SysTools,
  Version;

// =============================================================================
// String Utilities
// =============================================================================

// Thanks Michel (Phidels.com)
function GetSubStrCount(SubStr, S: string): Integer;
begin
  result:=0;
  while pos(substr,s)<>0 do
  begin
    S:=Right(substr,s);
    inc(result);
  end;
end;

// Thanks Michel (Phidels.com)
function LeftNRight(SubStr, S: string; N: Integer): string;
var i:integer;
begin
  S:=S+substr;
  for i:=1 to n do
  begin
    S:=copy(s, pos(substr, s)+length(substr), length(s)-pos(substr, s)+length(substr));
  end;
  result:=copy(s, 1, pos(substr, s)-1);
end;

// Thanks Michel (Phidels.com)
function Right(SubStr: string; S: string): string;
begin
  if pos(substr,s)=0 then result:='' else
    result:=copy(s, pos(substr, s)+length(substr), length(s)-pos(substr, s)+length(substr));
end;

// Thanks Michel (Phidels.com)
function Left(SubStr: string; S: string): string;
begin
  result:=copy(s, 1, pos(substr, s)-1);
end;

// Thanks Michel (Phidels.com)
function ExtractStr(LeftSubStr, RightSubStr, S: string): string;
begin
  Result := Left(RightSubStr, Right(LeftSubStr, S));
end;

// Thanks Michel (Phidels.com)
function ExtremeRight(SubStr: string; S: string): string;
begin
  Repeat
    S:= Right(substr,s);
  until pos(substr,s)=0;
  result:=S;
end;

function IsInString(const SubStr, S: string): Boolean;
begin
  Result := Pos(LowerCase(SubStr), LowerCase(S)) > 0;
end;

function SuppressUselessWhiteSpaces(const S: string): string;
var
  Buffer: TStringList;
  i: Integer;
  Entry, Separator: string;

begin
  Result := EmptyStr;
  Buffer := TStringList.Create;
  try
    Buffer.Text := StringReplace(Trim(S), WhiteSpaceStr, sLineBreak, [rfReplaceAll]);
    Separator := EmptyStr;
    for i := 0 to Buffer.Count - 1 do
    begin
      Entry := Buffer[i];
      if not SameText(Entry, EmptyStr) then
        Result := Result + Separator + Entry;
      Separator := WhiteSpaceStr;
    end;
    Result := Trim(Result);
  finally
    Buffer.Free;
  end;
end;

function IsRegExMatch(const InputValue, RegEx: string): Boolean;
var
  RegexObj: TRegExpr;

begin
  RegexObj := TRegExpr.Create;
  try
    RegexObj.Expression := RegEx;
    Result := RegexObj.Exec(InputValue);
  finally
    RegexObj.Free;
  end;
end;

function EndsWith(const SubStr, S: string): Boolean;
begin
  if SubStr = EmptyStr then
    Result := True
  else
    Result := AnsiEndsStr(SubStr, S);
end;

function StartsWith(const SubStr, S: string): Boolean;
begin
  Result := AnsiStartsStr(SubStr, S);
end;

function IsEmpty(const S: string): Boolean;
begin
  Result := SameText(S, EmptyStr);
end;


function IsInArray(A: TStringArray; const S: string): Boolean;
var
  Item: string;

begin
  Result := False;
  for Item in A do
    if (Item = S) then
      Exit(True);
end;

// =============================================================================
// StringList Utilities
// =============================================================================

procedure StringToStringList(const S, Delimiter: string; SL: TStringList);
begin
  if Assigned(SL) then
    SL.Text := StringReplace(Trim(S), Delimiter, sLineBreak, [rfReplaceAll]);
end;

function StringListToString(SL: TStringList; const Delimiter: string;
  const IgnoreBlank: Boolean = True): string;
var
  Buffer: string;

begin
  Result := EmptyStr;
  if Assigned(SL) then
  begin
    Buffer := SL.Text;
    if IgnoreBlank then
      Buffer := Trim(Buffer);
    Result := StringReplace(Buffer, sLineBreak, Delimiter, [rfReplaceAll]);
    if IgnoreBlank then
      Result := Trim(Result);
  end;
end;

function StringListSubstringIndexOf(SL: TStringList;
  SubStr: string): Integer; overload;
begin
  Result := StringListSubstringIndexOf(SL, SubStr, True);
end;

function StringListSubstringIndexOf(SL: TStringList; SubStr: string;
  CaseSensitive: Boolean): Integer; overload;
var
  i: Integer;
  Str: string;

begin
  Result := -1;

  if not CaseSensitive then
    SubStr := LowerCase(SubStr);

  if Assigned(SL) then
  begin
    i := 0;
    while (i < SL.Count) and (Result = -1) do
    begin
      Str := SL[i];
      if not CaseSensitive then
        Str := LowerCase(Str);
      if IsInString(SubStr, Str) then
        Result := i;
      Inc(i);
    end;
  end;
end;

{$PUSH}
{$WARN 6058 OFF}
procedure StringListRemoveDuplicates(SL: TStringList;
  ProcessFromEnd: Boolean = False);
var
  Dict: TStringIntegerMap;
  i: Integer;
  Indexes: TIntegerList;

  function ProcessItem(const ItemIndex: Integer): Integer;
  var
    Value: string;
    Dummy: Integer;

  begin
    Result := -1;
    Value := SL[ItemIndex];
    if not Dict.Find(Value, Dummy) then
      Dict.Add(Value, 0)
    else
      Result := ItemIndex;
  end;

begin
  Dict := TStringIntegerMap.Create;
  try
    Dict.Sorted := True;
    if ProcessFromEnd then
    begin
      for i := SL.Count - 1 downto 0 do
        if ProcessItem(i) <> -1 then
          SL.Delete(i);
    end
    else
    begin
      Indexes := TIntegerList.Create;
      try
        for i := 0 to SL.Count - 1 do
          if ProcessItem(i) <> -1 then
            Indexes.Add(i);
        for i := Indexes.Count - 1 downto 0 do
          SL.Delete(Indexes[i]);
      finally
        Indexes.Free;
      end;
    end;
  finally
    Dict.Free;
  end;
end;
{$POP}

end.
