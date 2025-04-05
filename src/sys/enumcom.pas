unit EnumCom;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils;

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
  Windows,
  SysTools,
  FSTools,
  Version;

const
{$IFDEF CPU64}
  EMBEDDED_RESOURCE_ENUMCOM64   = 'ENUMCOM64';
{$ELSE}
  EMBEDDED_RESOURCE_ENUMCOM32   = 'ENUMCOM32';
{$ENDIF}
  EMBEDDED_FILENAME_ENUMCOM     = 'enumcom.dll';

var
  EnumComFileName: TFileName = '';

function GetSerialPortList(var SerialPortList: TSerialPortList): Boolean;
const
  BUFFERSIZE = 1024;

type
  TSharedSerialPortInformation = packed record
    intPortIndex: Integer;
    bUsbDevice: Integer;                                    // Provided through a USB connection?
    strDevPath: array[0..BUFFERSIZE - 1] of AnsiChar;       // Device path for use with CreateFile()
    strPortName: array[0..BUFFERSIZE - 1] of AnsiChar;      // Simple name (i.e. COM1)
    strFriendlyName: array[0..BUFFERSIZE - 1] of AnsiChar;  // Full name to be displayed to a user
    strPortDesc: array[0..BUFFERSIZE - 1] of AnsiChar;      // friendly name without the COMx*)
  end;
  PSharedSerialPortInformation = ^TSharedSerialPortInformation;
  TSharedSerialPortInformationArray = array of TSharedSerialPortInformation;

  TGetSerialPorts = function(outArray: PSharedSerialPortInformation; maxCount: Integer): Integer; stdcall;
  TGetSerialPortsCount = function(): Integer; stdcall;

var
  DLLHandle: THandle;
  GetSerialPorts: TGetSerialPorts;
  GetSerialPortsCount: TGetSerialPortsCount;
  SerialPortInformationArray: TSharedSerialPortInformationArray;
  i,
  SerialPortsCount,
  EffectiveSerialPortsCount: Integer;
  SerialPort: TSerialPort;
  CurrentItem: TSharedSerialPortInformation;

begin
  Result := False;
{$IFDEF DEBUG}
  DebugLog('GetSerialPortList');
{$ENDIF}

  SerialPortInformationArray := Default(TSharedSerialPortInformationArray);
  DLLHandle := LoadLibrary(PChar(EnumComFileName));
  if DLLHandle <> 0 then
  begin
    try
      GetSerialPorts := TGetSerialPorts(GetProcAddress(DLLHandle, 'GetSerialPorts'));
      GetSerialPortsCount := TGetSerialPortsCount(GetProcAddress(DLLHandle, 'GetSerialPortsCount'));

      if Assigned(GetSerialPorts) and Assigned(GetSerialPortsCount) then
      begin
        SerialPortsCount := GetSerialPortsCount();

        SetLength(SerialPortInformationArray, SerialPortsCount);
        SetLength(SerialPortList, SerialPortsCount);

        if Assigned(SerialPortInformationArray) then
        begin
          EffectiveSerialPortsCount := GetSerialPorts(@SerialPortInformationArray[0], SerialPortsCount);
          for i := 0 to SerialPortsCount - 1 do
          begin
            CurrentItem := SerialPortInformationArray[i];

            // Assigning the values
            SerialPort.PortIndex := CurrentItem.intPortIndex;
            SerialPort.DevicePath := CurrentItem.strDevPath;
            SerialPort.Name := CurrentItem.strPortName;
            SerialPort.FriendlyName := CurrentItem.strFriendlyName;
            SerialPort.IsUSBDevice := (CurrentItem.bUsbDevice = 1);
            SerialPort.Description := CurrentItem.strPortDesc;

            // Adding the COM Port to the final Array
            SerialPortList[i] := SerialPort;
          end;

          Result := (Length(SerialPortList) = SerialPortsCount) and
            (EffectiveSerialPortsCount = SerialPortsCount);
        end;
      end
{$IFDEF DEBUG}
      else
        DebugLog('  Error: SerialPortInformationArray not assigned');
{$ELSE}
      ;
{$ENDIF}
    finally
      FreeLibrary(DLLHandle);
    end;
  end
{$IFDEF DEBUG}
  else
    DebugLog('  Error: Library not loaded');
{$ELSE}
  ;
{$ENDIF}
end;

initialization
  EnumComFileName := ExtractEmbeddedFileToWorkingPath(
    {$IFDEF CPU64}EMBEDDED_RESOURCE_ENUMCOM64{$ELSE}EMBEDDED_RESOURCE_ENUMCOM32{$ENDIF},
    EMBEDDED_FILENAME_ENUMCOM);

finalization
  KillFile(EnumComFileName);
  
end.

