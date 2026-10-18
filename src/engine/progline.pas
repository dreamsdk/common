unit ProgLine;

(*
  Detect the percentages in the output lines of the processes (Git, Wget,
  Package Manager, CMake...), so they can be displayed in a progress bar.
  This is used by DreamSDK Manager (progress window) and by the DreamSDK Setup
  post-install helper library.
*)

{$mode objfpc}{$H+}

interface

const
  (* Values returned by TProgressLineParser.Parse in the Position parameter *)
  PROGRESS_POSITION_MARQUEE = -1;   // No percentage: use a marquee progress bar
  PROGRESS_POSITION_UNCHANGED = -2; // Keep the progress bar as it is

type
  TProgressType = (ptNoPercentage, ptPercentageSingle, ptPercentagePersistent);

  { TProgressLineParser }
  TProgressLineParser = class(TObject)
  private
    fProgressType: TProgressType;
  protected
    function IsProgressLine(const Message: string): Boolean;
    function ExtractValueFromLine(const Message: string): Integer;
  public
    constructor Create;
    procedure Reset;

    (* Parse a new output line. Position is the new progress bar position (0 to
       100), PROGRESS_POSITION_MARQUEE or PROGRESS_POSITION_UNCHANGED. The
       result indicates if the line should be displayed, i.e., lines with a
       percentage (e.g., Git progress) are not displayed. *)
    function Parse(const Message: string; out Position: Integer): Boolean;

    property ProgressType: TProgressType read fProgressType;
  end;

implementation

uses
  SysUtils,
  StrTools;

const
  PROGRESS_LINE1 = '% ';
  PROGRESS_LINE2 = '%] ';
  FULL = '100%';
  MAX_PERCENT = 100;

{ TProgressLineParser }

constructor TProgressLineParser.Create;
begin
  Reset;
end;

procedure TProgressLineParser.Reset;
begin
  fProgressType := ptNoPercentage;
end;

function TProgressLineParser.IsProgressLine(const Message: string): Boolean;
begin
  Result := (IsInString(PROGRESS_LINE1, Message))
    or (IsInString(PROGRESS_LINE2, Message));
end;

function TProgressLineParser.ExtractValueFromLine(const Message: string): Integer;

  function ParseValue(LeftStr, RightStr, Str: string): Integer;
  var
    StrExtractedValue: string;

  begin
    StrExtractedValue := Trim(ExtractStr(LeftStr, RightStr, Str));
    Result := StrToIntDef(StrExtractedValue, -1);
  end;

var
  i: Integer;
  Buffer: string;
  IsPersistentPercentageMode: Boolean;

begin
  Result := -1;
  IsPersistentPercentageMode := False;
  Buffer := Default(string);

  // For Git
  Result := ParseValue(':', PROGRESS_LINE1 + ' (', Message);

  // For Wget and Package Manager
  if (Result = -1) then
  begin
    i := Pos(PROGRESS_LINE1, Message);
    Buffer := Copy(Message, i - 4, i - 1);
    Result := ParseValue(' ', PROGRESS_LINE1, Buffer);
  end;

  // If we detect "100%" then the value is 100...
  if (Result = -1) and (IsInString(FULL, Buffer)) then
    Result := MAX_PERCENT;

  // For CMake
  if (Result = -1) then
  begin
    Result := ParseValue('[', PROGRESS_LINE2, Message);
    IsPersistentPercentageMode := (Result <> -1);
  end;

  // There is a value here to display in the progress bar...
  if (Result <> -1) then
  begin
    fProgressType := ptPercentageSingle;
    if IsPersistentPercentageMode and (Result < MAX_PERCENT) then
      fProgressType := ptPercentagePersistent;
  end;
end;

function TProgressLineParser.Parse(const Message: string;
  out Position: Integer): Boolean;
var
  Value: Integer;

begin
  Position := PROGRESS_POSITION_UNCHANGED;

  // Handle percent steps (0% to 100%)
  case IsProgressLine(Message) of
    // The current line contains a percentage. We will extract it and handle it
    True:
      begin
        Value := ExtractValueFromLine(Message);
        if (Value <> -1) then
          Position := Value;
      end;
    // The current line don't have any percentage.
    False:
      begin
        if (fProgressType <> ptPercentagePersistent) then
          // We don't have any percentage currently, so reset the flag.
          fProgressType := ptNoPercentage;
      end;
  end;

  // Display the line
  Result := (fProgressType <> ptPercentageSingle);

  if (fProgressType = ptNoPercentage) then
    Position := PROGRESS_POSITION_MARQUEE;
end;

end.
