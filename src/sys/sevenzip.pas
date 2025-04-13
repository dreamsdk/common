unit SevenZip;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  RunCmd;

type
  TSevenZipOperationPickupEvent = procedure(Sender: TObject;
    const SourceFileName, OutputDirectory: TFileName) of object;
  TSevenZipProgressValueEvent = procedure(Sender: TObject; const CurrentValue: Integer;
    const TotalValue: Integer) of object;
  TSevenZipProgressRecordEvent = procedure(Sender: TObject; const RecordNode: string) of object;
  TSevenZipTerminateEvent = procedure(Sender: TObject; const Success: Boolean) of object;

  { TSevenZipCommanderOperationItem }
  TSevenZipCommanderOperationItem = class(TObject)
  private
    fOutputDirectory: TFileName;
    fSourceFileName: TFileName;
  public
    property SourceFileName: TFileName
      read fSourceFileName write fSourceFileName;
    property OutputDirectory: TFileName
      read fOutputDirectory write fOutputDirectory;
  end;

  { TSevenZipCommanderOperationList }
  TSevenZipCommanderOperationList = class(TObject)
  private
    fList: TList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TSevenZipCommanderOperationItem;
  public
    constructor Create;
    destructor Destroy; override;
    function Add: TSevenZipCommanderOperationItem;
    procedure Clear;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TSevenZipCommanderOperationItem
      read GetItem; default;
  end;

  { TSevenZipCommander }
  TSevenZipCommander = class(TObject)
  private
    fOperationPickup: TSevenZipOperationPickupEvent;
    fProgressRecord: TSevenZipProgressRecordEvent;
    fSevenZipProcess: TRunCommand;
    fCurrentTaskIndex: Integer;
    fOperationSuccess: Boolean;
    fOldCurrentValue: Integer;
    fTotalValue: Integer;
    fOperationList: TSevenZipCommanderOperationList;
    fProgress: TSevenZipProgressValueEvent;
    fTerminate: TSevenZipTerminateEvent;
    function GetActive: Boolean;
    function GetCurrentOperation: TSevenZipCommanderOperationItem;
    function GetOperationTerminated: Boolean;
    procedure HandleNewLine(Sender: TObject; NewLine: string);
    procedure HandleTerminate(Sender: TObject);
    procedure SendProgressEvent(const Value: Integer);
    procedure SendProgressRecord(RecordNode: string);
  protected
    procedure CreateTask;
    property CurrentOperation: TSevenZipCommanderOperationItem
      read GetCurrentOperation;
    property OperationTerminated: Boolean
      read GetOperationTerminated;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Abort;
    procedure Execute;
    procedure Pause;
    procedure Resume;

    property Active: Boolean read GetActive;
    property Operations: TSevenZipCommanderOperationList read fOperationList;

    property OnOperationPickup: TSevenZipOperationPickupEvent
      read fOperationPickup write fOperationPickup;
    property OnProgress: TSevenZipProgressValueEvent read fProgress write fProgress;
    property OnProgressRecord: TSevenZipProgressRecordEvent
      read fProgressRecord write fProgressRecord;
    property OnTerminate: TSevenZipTerminateEvent read fTerminate write fTerminate;
  end;

function UncompressLzmaFile(const FileName, OutputDirectory: TFileName): Boolean;

implementation

{$R embedded/sevenzip.rc}

uses
  SysTools,
  StrTools,
  FSTools,
  RunTools;

const
  EMBEDDED_RESOURCE_SEVENZIP = 'SEVENZIP';
  EMBEDDED_FILENAME_SEVENZIP = '7za.exe';
  SUCCESS_MESSAGE = 'Everything is Ok';
  RECORD_NODE_TERMINATED = '#|#TERMINATED#|#';
  PERCENT_MAX_VALUE = 100;

var
  SevenZipFileName: TFileName = '';

function UncompressLzmaFile(const FileName, OutputDirectory: TFileName): Boolean;
var
  Buffer: string;

begin
  Result := False;
  try
    Buffer := Run(SevenZipFileName, Format('x "%s" -o"%s" -y',
      [FileName, OutputDirectory]));
    Result := IsInString(SUCCESS_MESSAGE, Buffer);
  except
    Result := False;
  end;
end;

{ TSevenZipCommanderOperationList }

function TSevenZipCommanderOperationList.GetCount: Integer;
begin
  Result := fList.Count;
end;

function TSevenZipCommanderOperationList.GetItem(Index: Integer): TSevenZipCommanderOperationItem;
begin
  Result := TSevenZipCommanderOperationItem(fList[Index]);
end;

constructor TSevenZipCommanderOperationList.Create;
begin
  fList := TList.Create;
end;

destructor TSevenZipCommanderOperationList.Destroy;
begin
  Clear;
  fList.Free;
  inherited Destroy;
end;

function TSevenZipCommanderOperationList.Add: TSevenZipCommanderOperationItem;
var
  Index: Integer;

begin
  Index := fList.Add(TSevenZipCommanderOperationItem.Create);
  Result := Items[Index];
end;

procedure TSevenZipCommanderOperationList.Clear;
var
  i: Integer;

begin
  for i := 0 to fList.Count - 1 do
    Items[i].Free;
  fList.Clear;
end;

{ TSevenZipCommander }

procedure TSevenZipCommander.HandleNewLine(Sender: TObject; NewLine: string);
const
  PROGRESS_LINE = '% ';

var
  Value: Integer;
  CurrentFile: string;

  function ParseValue(LeftStr, RightStr, Str: string): Integer;
  var
    StrExtractedValue: string;

  begin
    StrExtractedValue := Trim(ExtractStr(LeftStr, RightStr, Str));
    Result := StrToIntDef(StrExtractedValue, -1);
  end;

  function ExtractValue: Integer;
  const
    FULL = '100%';

  begin
    Result := ParseValue(' ', PROGRESS_LINE, NewLine);
    if (Result = -1) and (IsInString(FULL, NewLine)) then
      Result := PERCENT_MAX_VALUE;
  end;

  function ExtractCurrentFile: string;
  begin
    Result := Trim(Right('-', NewLine));
  end;

begin
  if IsInString(PROGRESS_LINE, NewLine)
    and (not IsInString(SUCCESS_MESSAGE, NewLine)) then
  begin
    // Send progress event
    Value := ExtractValue;
    CurrentFile := ExtractCurrentFile;
    SendProgressEvent(Value);
    SendProgressRecord(CurrentFile);
  end;
end;

function TSevenZipCommander.GetCurrentOperation: TSevenZipCommanderOperationItem;
begin
  Result := nil;
  if fCurrentTaskIndex <= (Operations.Count - 1) then
    Result := Operations[fCurrentTaskIndex];
end;

function TSevenZipCommander.GetActive: Boolean;
begin
  Result := fCurrentTaskIndex <> -1;
end;

function TSevenZipCommander.GetOperationTerminated: Boolean;
begin
  Result := (fCurrentTaskIndex > (Operations.Count - 1))
    or (fCurrentTaskIndex = -1);
end;

procedure TSevenZipCommander.HandleTerminate(Sender: TObject);
var
  TaskSuccess: Boolean;

begin
{$IFDEF DEBUG}
  WriteLn('SevenZip ExitCode: ', fSevenZipProcess.ExitCode);
{$ENDIF}

  SendProgressEvent(PERCENT_MAX_VALUE);

  TaskSuccess := (fSevenZipProcess.ExitCode = 0);
  fOperationSuccess := fOperationSuccess and TaskSuccess;

{$IFDEF DEBUG}
  WriteLn('SevenZip Success: ', fOperationSuccess, ' (Task: ', TaskSuccess, ')');
{$ENDIF}

  if (not OperationTerminated) then
    CreateTask
  else if Assigned(fTerminate) then
  begin
    fCurrentTaskIndex := -1;
    fTerminate(Self, fOperationSuccess);
  end;
end;

procedure TSevenZipCommander.SendProgressEvent(const Value: Integer);
var
  NewTotalValue: Integer;

begin
  if Assigned(fProgress) and (fOldCurrentValue <> Value) then
  begin
{$IFDEF DEBUG}
    WriteLn('SevenZip Progress: ', Value, '%');
{$ENDIF}
    NewTotalValue := (Value div Operations.Count);
    if (Value >= PERCENT_MAX_VALUE) then
    begin
      Inc(fTotalValue, NewTotalValue);
      NewTotalValue := 0;
    end;
    fProgress(Self, Value, fTotalValue + NewTotalValue);
    fOldCurrentValue := Value;
  end;
end;

procedure TSevenZipCommander.SendProgressRecord(RecordNode: string);
begin
  if Assigned(fProgressRecord) then
  begin
    if RecordNode = RECORD_NODE_TERMINATED then
      RecordNode := EmptyStr;
    fProgressRecord(Self, RecordNode);
  end;
end;

procedure TSevenZipCommander.CreateTask;
begin
  FreeAndNil(fSevenZipProcess);

  fOldCurrentValue := 0;

  fSevenZipProcess := TRunCommand.Create(True);

  fSevenZipProcess.Executable := SevenZipFileName;
  fSevenZipProcess.Parameters.Add('x');
  fSevenZipProcess.Parameters.Add(Format('"%s"', [CurrentOperation.SourceFileName]));
  fSevenZipProcess.Parameters.Add(Format('-o"%s"', [CurrentOperation.OutputDirectory]));
  fSevenZipProcess.Parameters.Add('-y');
  fSevenZipProcess.Parameters.Add('-bsp1');

  fSevenZipProcess.OnNewLine := @HandleNewLine;
  fSevenZipProcess.OnTerminate := @HandleTerminate;

  fSevenZipProcess.Start;

  if Assigned(fOperationPickup) then
    fOperationPickup(
      Self,
      CurrentOperation.SourceFileName,
      CurrentOperation.OutputDirectory
    );

  Inc(fCurrentTaskIndex);
end;

constructor TSevenZipCommander.Create;
begin
  fOperationList := TSevenZipCommanderOperationList.Create;
end;

destructor TSevenZipCommander.Destroy;
begin
  Abort;
  fOperationList.Free;
  inherited Destroy;
end;

procedure TSevenZipCommander.Abort;
begin
  LogMessageEnter('SevenZipCommander.Abort');
  try
    try

      fCurrentTaskIndex := -1;
      fOperationSuccess := False;
      if Assigned(fSevenZipProcess) then
      begin
        LogMessage('Aborting SevenZipProcess...');
        fSevenZipProcess.Abort;
{$IFDEF DEBUG}
        DebugLog('SevenZipProcess::WaitFor::Start');
{$ENDIF}
        LogMessage('SevenZipProcess::WaitFor::Start...');
        fSevenZipProcess.WaitFor;
        LogMessage('SevenZipProcess::WaitFor::Exit...');
{$IFDEF DEBUG}
        DebugLog('SevenZipProcess::WaitFor::End');
{$ENDIF}
        FreeAndNil(fSevenZipProcess);
      end;

    except
      raise;
    end;
  finally
    LogMessageExit('SevenZipCommander.Abort');
  end;
end;

procedure TSevenZipCommander.Execute;
begin
  Abort;

  fTotalValue := 0;
  fCurrentTaskIndex := 0;
  fOperationSuccess := True;

  if fOperationList.Count > 0 then
    CreateTask;
end;

procedure TSevenZipCommander.Pause;
begin
  if Assigned(fSevenZipProcess) then
    fSevenZipProcess.Pause;
end;

procedure TSevenZipCommander.Resume;
begin
  if Assigned(fSevenZipProcess) then
    fSevenZipProcess.Resume;
end;

initialization
  if not FileExists(SevenZipFileName) then
    SevenZipFileName := ExtractEmbeddedFileToWorkingPath(
      EMBEDDED_RESOURCE_SEVENZIP, EMBEDDED_FILENAME_SEVENZIP);

finalization
  KillFile(SevenZipFileName);

end.

