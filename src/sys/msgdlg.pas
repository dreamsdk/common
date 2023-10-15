unit MsgDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, Controls;

function MsgBoxDlg(Handle: THandle; const aCaption: string; const aMsg: string;
  DlgType: TMsgDlgType; Buttons: TMsgDlgButtons;
  DefaultButton: TMsgDlgBtn): TModalResult; overload;
function MsgBoxDlg(Handle: THandle; const aCaption: string; const aMsg: string;
  DlgType: TMsgDlgType; Buttons: TMsgDlgButtons): TModalResult; overload;
function MsgBoxDlgTranslateString(const AText: string): string;
function MsgBoxDlgWrapStr: string;

implementation

uses
  Windows, SysTools, Version;

function DlgTypeToMsgBox(DlgType: TMsgDlgType): Integer;
begin
  Result := 0;
  case DlgType of
    mtWarning: Result := MB_ICONWARNING;
    mtError: Result := MB_ICONERROR;
    mtInformation: Result := MB_ICONINFORMATION;
    mtConfirmation: Result := MB_ICONQUESTION;
  end;
end;

function MessageBoxResultToModalResult(OutputResult: Integer): TModalResult;
begin
  Result := mrNone;
  case OutputResult of
    IDABORT: Result := mrAbort;
    IDCANCEL: Result := mrCancel;
    IDIGNORE: Result := mrIgnore;
    IDNO: Result := mrNo;
    IDOK: Result := mrOK;
    IDRETRY: Result := mrRetry;
    IDYES: Result := mrYes;
  end;
end;

function ButtonsToMsgDlg(Buttons: TMsgDlgButtons): Integer;
begin
  Result := MB_OK;
  if (mbYes in Buttons) or (mbNo in Buttons) then
    Result := MB_YESNO;
  if (mbOk in Buttons) and (mbCancel in Buttons) then
    Result := MB_OKCANCEL;
end;

function DefaultButtonToMsgDlg(DefaultButton: TMsgDlgBtn): Integer;
begin
  Result := 0;
  if DefaultButton = mbNo then
    Result := MB_DEFBUTTON2;
end;

function MsgBoxDlg(Handle: THandle; const aCaption: string; const aMsg: string;
  DlgType: TMsgDlgType; Buttons: TMsgDlgButtons;
  DefaultButton: TMsgDlgBtn): TModalResult; overload;
var
  Flags: Integer;
  OutputResult: Integer;

begin
  Flags := DlgTypeToMsgBox(DlgType) + ButtonsToMsgDlg(Buttons) + DefaultButtonToMsgDlg(DefaultButton);
  OutputResult := MessageBox(Handle, PChar(aMsg), PChar(aCaption), Flags);
  Result := MessageBoxResultToModalResult(OutputResult);
end;

function MsgBoxDlg(Handle: THandle; const aCaption: string; const aMsg: string;
  DlgType: TMsgDlgType; Buttons: TMsgDlgButtons): TModalResult; overload;
begin
  Result := MsgBoxDlg(Handle, aCaption, aMsg, DlgType, Buttons, mbClose);
end;

function MsgBoxDlgWrapStr: string;
begin
  Result := WhiteSpaceStr;
  if not IsWindowsVistaOrGreater then
    Result := sLineBreak;
end;

function MsgBoxDlgTranslateString(const AText: string): string;
begin
  Result := StringReplace(AText, '\n', MsgBoxDlgWrapStr, [rfReplaceAll]);
end;

end.

