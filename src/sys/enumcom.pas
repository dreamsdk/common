unit EnumCom;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TSerialPort = packed record
    PortIndex: Integer;
    DevicePath: string;
    Name: string;
    FriendlyName: string;
    IsUSBDevice: Boolean;
    Description: string;
  end;

  TSerialPortList = array of TSerialPort;

function GetSerialPortList(var SerialPortList: TSerialPortList): Boolean;

implementation

{$R embedded/enumcom.rc}

uses
  SysTools,
  FSTools,
  RunTools,
  Version,
  CSVDocument;

const
  EMBEDDED_RESOURCE_ENUMCOM  = 'ENUMCOM';
  EMBEDDED_FILENAME_ENUMCOM  = 'enumcom.exe';

var
  EnumComFileName: TFileName = '';

function ParseResultFile(Buffer: string;
  var SerialPortList: TSerialPortList): Boolean;
var 
  CSVDoc: TCSVDocument;
  i: Integer;
  SerialPort: TSerialPort;

begin
  Result := False;
  CSVDoc := TCSVDocument.Create;
  try
    CSVDoc.Delimiter := '|';
    CSVDoc.CSVText := Buffer;

    // Allocate the required memory
    SetLength(SerialPortList, CSVDoc.RowCount - 1);

    // Looping on all CSV rows extracted from EnumCom
    for i := 1 to CSVDoc.RowCount - 1 do
    begin
      // Parsing the row
      SerialPort.PortIndex := StrToInt(CSVDoc.Cells[0, i]);
      SerialPort.DevicePath := CSVDoc.Cells[1, i];
      SerialPort.Name := CSVDoc.Cells[2, i];
      SerialPort.FriendlyName := CSVDoc.Cells[3, i];
      SerialPort.IsUSBDevice := StrToBool(CSVDoc.Cells[4, i]);
      SerialPort.Description := CSVDoc.Cells[5, i];

      // Adding the COM Port to the final Array
      SerialPortList[i - 1] := SerialPort;
    end;

    Result := (High(SerialPortList) + 1) = (CSVDoc.RowCount - 1);
  finally
    CSVDoc.Free;
  end;
end;

function GetSerialPortList(var SerialPortList: TSerialPortList): Boolean;
var
  OutputBuffer: string;
  ProcessExitCode: Integer;

begin
  Result := False;
  try
    OutputBuffer := Default(string);
    ProcessExitCode := Default(Integer);
    if RunSingleCommand(EnumComFileName, OutputBuffer, ProcessExitCode) then
      Result := ParseResultFile(OutputBuffer, SerialPortList);
  except
    Result := False;
  end;
end;

initialization
  if not FileExists(EnumComFileName) then
    EnumComFileName := ExtractEmbeddedFileToWorkingPath(
      EMBEDDED_RESOURCE_ENUMCOM, EMBEDDED_FILENAME_ENUMCOM);

finalization
  KillFile(EnumComFileName);
  
end.

