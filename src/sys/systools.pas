unit SysTools;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  FGL;

const
  WhiteSpaceStr = ' ';
  ArraySeparator = '|';
  STRING_DATE_FORMAT = 'YYYY-MM-DD @ HH:mm:ss';

type
  TIntegerList = specialize TFPGList<Integer>;
  TStringIntegerMap = specialize TFPGMap<string, Integer>;

{$IFDEF DEBUG}procedure DebugLog(const Message: string);{$ENDIF}
{$IFDEF GUI}procedure Delay(Milliseconds: Integer);{$ENDIF}
{$IFDEF DEBUG}procedure DumpCharArrayToFile(A: array of Char; const FileName: TFileName);{$ENDIF}
function EndsWith(const SubStr, S: string): Boolean;
function ExpandEnvironmentStrings(const InputString: string): string;
function ExtractStr(LeftSubStr, RightSubStr, S: string): string;
function ExtremeRight(SubStr: string; S: string): string;
function GetSubStrCount(SubStr, S: string): Integer;
function IsInString(const SubStr, S: string): Boolean;
function IsRegExMatch(const InputValue, RegEx: string): Boolean;
function Left(SubStr: string; S: string): string;
function LeftNRight(SubStr, S: string; N: Integer): string;
function Right(SubStr: string; S: string): string;
function StartsWith(const SubStr, S: string): Boolean;
procedure StringToStringList(const S, Delimiter: string; SL: TStringList);
function StringListToString(SL: TStringList; const Delimiter: string): string;
function StringListSubstringIndexOf(SL: TStringList; const SubStr: string): Integer;
function SuppressUselessWhiteSpaces(const S: string): string;

function GetUserList(var UserList: TStringList): Boolean;

implementation

uses
  StrUtils,
  RegExpr,
  LazUTF8,
  LConvEncoding,
{$IFDEF Windows}
  Windows
{$ENDIF}
{$IFDEF GUI}
  , Forms,
{$ELSE}
  ,
{$ENDIF}
  RunTools,
  FSTools;

function GetUserList(var UserList: TStringList): Boolean;
var
  BatchFileName,
  OutputFileName: TFileName;
  Buffer: TStringList;
  i: Integer;

begin
  if Assigned(UserList) then
  begin
    BatchFileName := ChangeFileExt(SysUtils.GetTempFileName, '.bat');
    OutputFileName := SysUtils.GetTempFileName;

    Buffer := TStringList.Create;
    try
      Buffer.Add('@echo off');
      Buffer.Add(Format('wmic UserAccount where "LocalAccount = True and Disabled = False" get Name > "%s"', [OutputFileName]));
      Buffer.SaveToFile(BatchFileName);
    finally
      Buffer.Free;
    end;

    Result := RunAndWait(BatchFileName);

    if FileExists(OutputFileName) then
    begin
      UserList.Text := LoadUTF16FileToString(OutputFileName);
      UserList.Delete(0); // Remove the 'Name' header
      for i := 0 to UserList.Count - 1 do
        UserList[i] := Trim(UserList[i]);
    end;

    KillFile(OutputFileName);
    KillFile(BatchFileName);
  end;
end;

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

{$IFDEF DEBUG}
procedure DebugLog(const Message: string);
begin
{$IFDEF CONSOLE}
  WriteLn(Message);
{$ENDIF}
end;
{$ENDIF}

function StringListToString(SL: TStringList; const Delimiter: string): string;
begin
  Result := EmptyStr;
  if Assigned(SL) then
    Result := Trim(StringReplace(Trim(SL.Text), sLineBreak, Delimiter, [rfReplaceAll]));
end;

procedure StringToStringList(const S, Delimiter: string; SL: TStringList);
begin
  if Assigned(SL) then
    SL.Text := StringReplace(Trim(S), Delimiter, sLineBreak, [rfReplaceAll]);
end;

function StringListSubstringIndexOf(SL: TStringList; const SubStr: string): Integer;
var
  i: Integer;

begin
  Result := -1;
  if Assigned(SL) then
  begin
    i := 0;
    while (i < SL.Count) and (Result = -1) do
    begin
      if IsInString(SubStr, SL[i]) then
        Result := i;
      Inc(i);
    end;
  end;
end;

function ExpandEnvironmentStrings(const InputString: string): string;
{$IFDEF WINDOWS}
const
  MAXSIZE = 32768;

begin
  SetLength(Result, MAXSIZE);
  SetLength(Result, Windows.ExpandEnvironmentStrings(PChar(InputString), @Result[1], Length(Result)) - 1);
{$ELSE}
begin
  Result := InputString;
{$ENDIF}
end;

{$IFDEF GUI}
procedure Delay(Milliseconds: Integer);
var
  PastTime: LongInt;

begin
  PastTime := GetTickCount;
  repeat
    Application.ProcessMessages;
  until (GetTickCount - PastTime) >= LongInt(Milliseconds);
end;
{$ENDIF}

{$IFDEF DEBUG}
procedure DumpCharArrayToFile(A: array of Char; const FileName: TFileName);
var
  F: file;

begin
  AssignFile(F, FileName);
  ReWrite(F, SizeOf(A));
  BlockWrite(F, A[0], 1);
  CloseFile(F);
end;
{$ENDIF}

end.

