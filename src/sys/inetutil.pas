unit InetUtil;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

type
  TNetworkCardAdapter = record
    NetworkCardName: string;
    MacAddress: string;
    IpAddresses: array of string;
  end;

  TNetworkCardAdapterList = array of TNetworkCardAdapter;

function GetNetworkCardAdapterList(var ANetworkAdapterCardList: TNetworkCardAdapterList): Boolean;
function IsValidInternetProtocolAddress(InternetProtocolAddress: string): Boolean;
function IsValidMediaAccessControlAddress(MediaAccessControlAddress: string): Boolean;
function ParseInternetProtocolAddress(const InputValue: string): string;

implementation

uses
  LazUTF8,
  SysTools;

function ParseInternetProtocolAddress(const InputValue: string): string;
var
  Buffer: TStringList;
  i: Integer;

begin
  Result := EmptyStr;
  if IsValidInternetProtocolAddress(InputValue) then
  begin
    Buffer := TStringList.Create;
    try
      StringToStringList(InputValue, '.', Buffer);
      for i := 0 to Buffer.Count - 1 do
        Buffer[i] := IntToStr(StrToInt(Buffer[i]));
      Result := StringListToString(Buffer, '.');
    finally
      Buffer.Free;
    end;
  end;
end;

// Thanks to: https://stackoverflow.com/a/5284410/3726096
function IsValidInternetProtocolAddress(InternetProtocolAddress: string): Boolean;
begin
  Result := IsRegExMatch(InternetProtocolAddress, '\b((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)(\.|$)){4}\b');
end;

// Thanks to: https://stackoverflow.com/a/4260512/3726096
function IsValidMediaAccessControlAddress(MediaAccessControlAddress: string): Boolean;
begin
  Result := IsRegExMatch(MediaAccessControlAddress, '^([0-9A-Fa-f]{2}[:-]){5}([0-9A-Fa-f]{2})$');
end;

// For debugging this complex function:
// {$DEFINE DEBUG_GET_NETWORK_CARD_ADAPTER}
function GetNetworkCardAdapterList(
  var ANetworkAdapterCardList: TNetworkCardAdapterList): Boolean;
var
  IpToMacStringBuffer,
  MacToAdapterNameStringBuffer: string;
  IpToMacBuffer,
  MacToAdapterNameBuffer: TStringList;

  function RunWmic: Boolean;
  var
    Buffer: TStringList;
    BatchFileName,
    IpToMacFileName,
    MacToAdapterNameFileName: TFileName;

  begin
    BatchFileName := ChangeFileExt(SysUtils.GetTempFileName, '.bat');
    IpToMacFileName := ChangeFileExt(SysUtils.GetTempFileName, '-ip2mac.tmp');
    MacToAdapterNameFileName := ChangeFileExt(SysUtils.GetTempFileName, '-mac2name.tmp');

    Buffer := TStringList.Create;
    try
      Buffer.Add('@echo off');
      Buffer.Add('wmic OS get Version > nul');
      Buffer.Add(Format('wmic NICCONFIG get IPAddress,MACAddress /FORMAT:CSV > "%s"', [IpToMacFileName]));
      Buffer.Add(Format('wmic NIC where "NetConnectionID like ''%%%%''" get MACAddress,NetConnectionID /FORMAT:CSV > "%s"', [MacToAdapterNameFileName]));
      Buffer.Add(':check_files');
      Buffer.Add(Format('if not exist "%s" goto check_files', [IpToMacFileName]));
      Buffer.Add(Format('if not exist "%s" goto check_files', [MacToAdapterNameFileName]));
      Buffer.SaveToFile(BatchFileName);
    finally
      Buffer.Free;
    end;

    Result := RunAndWait(BatchFileName);

{$IFDEF DEBUG}
{$IFDEF DEBUG_GET_NETWORK_CARD_ADAPTER}
    WriteLn('RunAndWait: ', Result);
{$ENDIF}
{$ENDIF}

    if Result then
    begin
      IpToMacStringBuffer := EmptyStr;
      if FileExists(IpToMacFileName) then
      begin
{$IFDEF DEBUG}
{$IFDEF DEBUG_GET_NETWORK_CARD_ADAPTER}
        WriteLn('IpToMacFileName Loaded');
{$ENDIF}
{$ENDIF}
        IpToMacStringBuffer := LoadUTF16FileToString(IpToMacFileName);
      end;

      MacToAdapterNameStringBuffer := EmptyStr;
      if FileExists(MacToAdapterNameFileName) then
      begin
{$IFDEF DEBUG}
{$IFDEF DEBUG_GET_NETWORK_CARD_ADAPTER}
        WriteLn('MacToAdapterNameFileName Loaded');
{$ENDIF}
{$ENDIF}
        MacToAdapterNameStringBuffer := LoadUTF16FileToString(MacToAdapterNameFileName);
      end;
    end;

    KillFile(IpToMacFileName);
    KillFile(MacToAdapterNameFileName);
    KillFile(BatchFileName);
  end;

  procedure ParseMacToAdapterName;
  const
    HEADER = 'Node,MACAddress,NetConnectionID';

  var
    i, j, StartIndex: Integer;
    Buffer: TStringList;
    MacAddress: string;

  begin
{$IFDEF DEBUG}
{$IFDEF DEBUG_GET_NETWORK_CARD_ADAPTER}
    WriteLn('ParseMacToAdapterName');
{$ENDIF}
{$ENDIF}
    Buffer := TStringList.Create;
    try
      j := 0;
      StartIndex := StringListSubstringIndexOf(MacToAdapterNameBuffer, HEADER) + 1;
      for i := StartIndex to MacToAdapterNameBuffer.Count - 1 do
      begin
        StringToStringList(MacToAdapterNameBuffer[i], ',', Buffer);

        if Buffer.Count = 3 then
        begin
          MacAddress := Buffer[1];
          if MacAddress <> EmptyStr then
          begin
            SetLength(ANetworkAdapterCardList, j + 1);
            ANetworkAdapterCardList[j].NetworkCardName := Buffer[2];
            ANetworkAdapterCardList[j].MacAddress := MacAddress;
{$IFDEF DEBUG}
{$IFDEF DEBUG_GET_NETWORK_CARD_ADAPTER}
            WriteLn('  MacAddress: ', ANetworkAdapterCardList[j].MacAddress,
              ', NetworkCardName: ', ANetworkAdapterCardList[j].NetworkCardName);
{$ENDIF}
{$ENDIF}
            Inc(j);
          end;
        end;
      end;
    finally
      Buffer.Free;
    end;
  end;

  function FindMac(const MacAddress: string): Integer;
  var
    i: Integer;

  begin
    Result := -1;
    for i := Low(ANetworkAdapterCardList) to High(ANetworkAdapterCardList) do
      if SameText(ANetworkAdapterCardList[i].MacAddress, MacAddress) then
      begin
        Result := i;
        Break;
      end;
  end;

  procedure HandleIpAddresses(const AIpIndex: Integer; const AIpAddresses: string);
  var
    i, IpEntryIndex: Integer;
    ExtractedIpAddresses, CurrentIpAddress: string;
    Buffer: TStringList;

  begin
    ExtractedIpAddresses := ExtractStr('{', '}', AIpAddresses);
    if ExtractedIpAddresses <> EmptyStr then
    begin
      Buffer := TStringList.Create;
      try
        StringToStringList(ExtractedIpAddresses, ';', Buffer);

        with ANetworkAdapterCardList[AIpIndex] do
        begin
          for i := 0 to Buffer.Count - 1 do
          begin
            CurrentIpAddress := Buffer[i];
{$IFDEF DEBUG}
{$IFDEF DEBUG_GET_NETWORK_CARD_ADAPTER}
            WriteLn('    ', CurrentIpAddress);
{$ENDIF}
{$ENDIF}
            if Trim(CurrentIpAddress) <> EmptyStr then
            begin
              IpEntryIndex := Length(IpAddresses);
              SetLength(IpAddresses, IpEntryIndex + 1);
              IpAddresses[IpEntryIndex] := CurrentIpAddress;
            end;
          end;
        end;

      finally
        Buffer.Free;
      end;
    end;
  end;

  procedure ParseIpToMac;
  const
    HEADER = 'Node,IPAddress,MACAddress';

  var
    i, j, StartIndex: Integer;
    Buffer: TStringList;

  begin
    Buffer := TStringList.Create;
    try
      StartIndex := StringListSubstringIndexOf(IpToMacBuffer, HEADER) + 1;
      for i := StartIndex to IpToMacBuffer.Count - 1 do
      begin
{$IFDEF DEBUG}
{$IFDEF DEBUG_GET_NETWORK_CARD_ADAPTER}
        WriteLn('  ', IpToMacBuffer[i]);
{$ENDIF}
{$ENDIF}
        StringToStringList(IpToMacBuffer[i], ',', Buffer);

        if Buffer.Count = 3 then
        begin
          // Only for items with MAC
          j := FindMac(Buffer[2]);
          if j <> -1 then
            HandleIpAddresses(j, Buffer[1]);
        end;
      end;
     finally
       Buffer.Free;
     end;
  end;

begin
  Result := RunWmic;

  if Result then
  begin
    IpToMacBuffer := TStringList.Create;
    MacToAdapterNameBuffer := TStringList.Create;

    try
      // Assign the buffer
      MacToAdapterNameBuffer.Text := MacToAdapterNameStringBuffer;
      IpToMacBuffer.Text := IpToMacStringBuffer;

      // Extract MAC addresses for Network Adapters
      ParseMacToAdapterName;

      // Extract IPv4 addresses for MAC addresses
      ParseIpToMac;
    finally
      IpToMacBuffer.Free;
      MacToAdapterNameBuffer.Free;
    end;
  end;
end;

end.
