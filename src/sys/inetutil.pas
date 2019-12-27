unit InetUtil;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

type
  TIpAddress = record
    Address: string;
    Subnet: string;
  end;
  PIpAddress = ^TIpAddress;

  TIpAddresses = array of TIpAddress;
  PIpAddresses = ^TIpAddresses;

  TNetworkCardAdapter = record
    NetworkCardName: string;
    MacAddress: string;
    IPv4Addresses: TIpAddresses;
    IPv6Addresses: TIpAddresses;
  end;

  TNetworkCardAdapterList = array of TNetworkCardAdapter;

function GetNetworkCardAdapterList(var ANetworkAdapterCardList: TNetworkCardAdapterList): Boolean;
function FindMediaAccessControlAddress(var ANetworkAdapterCardList: TNetworkCardAdapterList;
  MediaAccessControlAddress: string): Integer;
function IsValidInternetProtocolAddress(InternetProtocolAddress: string): Boolean;
function IsValidMediaAccessControlAddress(MediaAccessControlAddress: string): Boolean;
function ParseInternetProtocolAddress(const InputValue: string): string;
function SanitizeMediaAccessControlAddress(MediaAccessControlAddress: string): string;

implementation

uses
  LazUTF8,
  SynaIP,
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

function IsSameSubnet(const Subnet, InternetProtocolAddress1,
  InternetProtocolAddress2: string): Boolean;
var
  BufferInternetProtocolAddress1,
  BufferInternetProtocolAddress2,
  BufferSubnet: TStringList;
  i: Integer;
  SubnetValue,
  IPv4Value1,
  IPv4Value2: Integer;

begin
  Result := False;

  BufferSubnet := TStringList.Create;
  BufferInternetProtocolAddress1 := TStringList.Create;
  BufferInternetProtocolAddress2 := TStringList.Create;
  try
    StringToStringList(Subnet, '.', BufferSubnet);
    StringToStringList(InternetProtocolAddress1, '.', BufferInternetProtocolAddress1);
    StringToStringList(InternetProtocolAddress2, '.', BufferInternetProtocolAddress2);

    if (BufferSubnet.Count <> BufferInternetProtocolAddress1.Count) or
      (BufferSubnet.Count <> BufferInternetProtocolAddress2.Count) then
        Exit;

    Result := True;
    for i := 0 to BufferSubnet.Count - 1 do
    begin
      SubnetValue := StrToInt(BufferSubnet[i]);
      IPv4Value1 := StrToInt(BufferInternetProtocolAddress1[i]);
      IPv4Value2 := StrToInt(BufferInternetProtocolAddress2[i]);

      Result := Result
        and (IPv4Value1 and SubnetValue = IPv4Value2 and SubnetValue);
    end;

  finally
    BufferInternetProtocolAddress1.Free;
    BufferInternetProtocolAddress2.Free;
    BufferSubnet.Free;
  end;
end;

function IsValidInternetProtocolAddress(InternetProtocolAddress: string): Boolean;
begin
  Result := IsIP(InternetProtocolAddress);
end;

// Thanks to: https://stackoverflow.com/a/4260512/3726096
function IsValidMediaAccessControlAddress(MediaAccessControlAddress: string): Boolean;
begin
  Result := IsRegExMatch(MediaAccessControlAddress, '^([0-9A-Fa-f]{2}[:-]){5}([0-9A-Fa-f]{2})$');
end;

function SanitizeMediaAccessControlAddress(MediaAccessControlAddress: string): string;
begin
  Result := StringReplace(UpperCase(MediaAccessControlAddress), ':', '-', [rfReplaceAll]);
end;

function FindMediaAccessControlAddress(
  var ANetworkAdapterCardList: TNetworkCardAdapterList;
  MediaAccessControlAddress: string): Integer;
var
  i: Integer;

begin
  Result := -1;
  MediaAccessControlAddress :=
    SanitizeMediaAccessControlAddress(MediaAccessControlAddress);

  for i := Low(ANetworkAdapterCardList) to High(ANetworkAdapterCardList) do
    if SameText(ANetworkAdapterCardList[i].MacAddress, MediaAccessControlAddress) then
    begin
      Result := i;
      Break;
    end;
end;

// Note: for debugging this complex function, define the thing below
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
      Buffer.Add(Format('wmic NICCONFIG get IPAddress,IPSubnet,MACAddress /FORMAT:CSV > "%s"', [IpToMacFileName]));
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
    FieldCount: Integer;

  begin
{$IFDEF DEBUG}
{$IFDEF DEBUG_GET_NETWORK_CARD_ADAPTER}
    WriteLn('ParseMacToAdapterName');
{$ENDIF}
{$ENDIF}
    FieldCount := GetSubStrCount(',', HEADER) + 1;
    Buffer := TStringList.Create;
    try
      j := 0;
      StartIndex := StringListSubstringIndexOf(MacToAdapterNameBuffer, HEADER) + 1;
      for i := StartIndex to MacToAdapterNameBuffer.Count - 1 do
      begin
        StringToStringList(MacToAdapterNameBuffer[i], ',', Buffer);

        if Buffer.Count = FieldCount then
        begin
{$IFDEF DEBUG}
{$IFDEF DEBUG_GET_NETWORK_CARD_ADAPTER}
          WriteLn('  ', MacToAdapterNameBuffer[i]);
{$ENDIF}
{$ENDIF}
          MacAddress := SanitizeMediaAccessControlAddress(Buffer[1]);
          if MacAddress <> EmptyStr then
          begin
            SetLength(ANetworkAdapterCardList, j + 1);
            ANetworkAdapterCardList[j].NetworkCardName := Buffer[2];
            ANetworkAdapterCardList[j].MacAddress := MacAddress;
{$IFDEF DEBUG}
{$IFDEF DEBUG_GET_NETWORK_CARD_ADAPTER}
            WriteLn('    MacAddress: ', ANetworkAdapterCardList[j].MacAddress, sLineBreak,
              '    NetworkCardName: ', ANetworkAdapterCardList[j].NetworkCardName);
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

  procedure HandleIpAddresses(const AIpIndex: Integer;
    const AIpAddresses: string; const AIpSubnets: string);
  var
    i, IpEntryIndex: Integer;
    ExtractedIpAddresses,
    ExtractedSubnets,
    CurrentIpAddress,
    CurrentIpSubnet: string;
    IpBuffer,
    SubnetBuffer: TStringList;
    IpAddresses: PIpAddresses;

  begin
    ExtractedIpAddresses := ExtractStr('{', '}', AIpAddresses);
    ExtractedSubnets := ExtractStr('{', '}', AIpSubnets);

    if ExtractedIpAddresses <> EmptyStr then
    begin
      IpBuffer := TStringList.Create;
      SubnetBuffer := TStringList.Create;
      try
        StringToStringList(ExtractedIpAddresses, ';', IpBuffer);
        StringToStringList(ExtractedSubnets, ';', SubnetBuffer);

        with ANetworkAdapterCardList[AIpIndex] do
        begin
          for i := 0 to IpBuffer.Count - 1 do
          begin
            CurrentIpAddress := IpBuffer[i];
            CurrentIpSubnet := SubnetBuffer[i]; // no parsing needed

            if CurrentIpAddress <> EmptyStr then
            begin
{$IFDEF DEBUG}
{$IFDEF DEBUG_GET_NETWORK_CARD_ADAPTER}
              WriteLn('    ', CurrentIpAddress);
{$ENDIF}
{$ENDIF}
              // Determine if the item is an IPv4 or IPv6
              IpAddresses := @IPv4Addresses;
              if IsIP6(CurrentIpAddress) then
                IpAddresses := @IPv6Addresses
              else
                CurrentIpAddress := ParseInternetProtocolAddress(CurrentIpAddress); // Sanitize IPv4

              // Add a item to the array
              IpEntryIndex := Length(IpAddresses^);
              SetLength(IpAddresses^, IpEntryIndex + 1);

              // Assign the values to the new item
              IpAddresses^[IpEntryIndex].Address := CurrentIpAddress;
              IpAddresses^[IpEntryIndex].Subnet := CurrentIpSubnet;
            end;
          end;
        end;

      finally
        SubnetBuffer.Free;
        IpBuffer.Free;
      end;
    end;
  end;

  procedure ParseIpToMac;
  const
    HEADER = 'Node,IPAddress,IPSubnet,MACAddress';

  var
    i, j, StartIndex: Integer;
    Buffer: TStringList;
    MacAddress: string;
    FieldCount: Integer;

  begin
{$IFDEF DEBUG}
{$IFDEF DEBUG_GET_NETWORK_CARD_ADAPTER}
    WriteLn('ParseIpToMac');
{$ENDIF}
{$ENDIF}
    FieldCount := GetSubStrCount(',', HEADER) + 1;
    Buffer := TStringList.Create;
    try
      StartIndex := StringListSubstringIndexOf(IpToMacBuffer, HEADER) + 1;
      for i := StartIndex to IpToMacBuffer.Count - 1 do
      begin
        StringToStringList(IpToMacBuffer[i], ',', Buffer);

        if Buffer.Count = FieldCount then
        begin
          // Only for items with MAC
          MacAddress := SanitizeMediaAccessControlAddress(Buffer[3]);
          j := FindMediaAccessControlAddress(ANetworkAdapterCardList, MacAddress);
          if j <> -1 then
          begin
{$IFDEF DEBUG}
{$IFDEF DEBUG_GET_NETWORK_CARD_ADAPTER}
            WriteLn('  ', IpToMacBuffer[i]);
{$ENDIF}
{$ENDIF}
            HandleIpAddresses(j, Buffer[1], Buffer[2]);
          end;
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

