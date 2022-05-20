unit InetUtil;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

type
  EExternalInternetProtocolAddress = class(Exception);

  TIpAddress = record
    Address: string;
    Subnet: string;
  end;
  PIpAddress = ^TIpAddress;

  TIpAddresses = array of TIpAddress;
  PIpAddresses = ^TIpAddresses;

  TNetworkCardAdapter = record
    NetworkCardName: string;
    InterfaceIndex: Integer;
    MacAddress: string;
    IPv4Addresses: TIpAddresses;
    IPv6Addresses: TIpAddresses;
  end;

  TNetworkCardAdapterList = array of TNetworkCardAdapter;

{$IFDEF DEBUG}
procedure DumpNetworkCardAdapterList(var ANetworkAdapterCardList: TNetworkCardAdapterList);
{$ENDIF}
function GetExternalInternetProtocolAddress: string;
function GetNetworkCardAdapterList(var ANetworkAdapterCardList: TNetworkCardAdapterList): Boolean;
function FindMediaAccessControlAddress(var ANetworkAdapterCardList: TNetworkCardAdapterList;
  MediaAccessControlAddress: string): Integer;
function IsInternetConnectionAvailable: Boolean;
function IsSameSubnet(const Subnet, InternetProtocolAddress1,
  InternetProtocolAddress2: string): Boolean;
function IsValidInternetProtocolAddress(InternetProtocolAddress: string): Boolean;
function IsValidMediaAccessControlAddress(MediaAccessControlAddress: string): Boolean;
function ParseInternetProtocolAddress(const InputValue: string): string;
function SanitizeMediaAccessControlAddress(MediaAccessControlAddress: string): string;

function GetNetworkCardAdapterList_OLD(
  var ANetworkAdapterCardList: TNetworkCardAdapterList): Boolean;

implementation

uses
  Registry,
  LazUTF8,
  SynaIP,
  FPHTTPClient,
  RegExpr,
  SysTools,
  RunTools,
  FSTools,
  Version,
  RegTools,
  UtilWMI;

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
  MacAddress: string;

begin
  Result := -1;
  MacAddress := SanitizeMediaAccessControlAddress(MediaAccessControlAddress);

  for i := Low(ANetworkAdapterCardList) to High(ANetworkAdapterCardList) do
    if SameText(ANetworkAdapterCardList[i].MacAddress, MacAddress) then
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
  IsIpAddressExtractSuccess: Boolean;
  NetworkAdapterConfiguration,
  NetworkAdapter: TWindowsManagementInstrumentationQueryResult;
  WMIFieldNames: TStringArray;
  i,
  CurrentCardItemIndex: Integer;
  NetworkCardAdapter: TNetworkCardAdapter;
  MacAddress: string;
  IpAddressesHashMap: TStringIntegerMap;

  function _HandleIpAddressesSubnets(
    const ANetworkCardAdapterItemIndex: Integer;
    const AIpAddresses, AIpSubnets: TStringArray): Boolean;
  var
    i,
    IpEntryIndex: Integer;
    CurrentIpAddress,
    CurrentIpSubnet,
    IpAddressHashKey: string;
    IpAddresses: PIpAddresses;
    IsValidAddress: Boolean;

    function __IsUsableIpAddress(AnIpAddress: string): Boolean;
    const
      INVALID_IP_ADDRESS = '0.0.0.0';
    begin
      AnIpAddress := Trim(AnIpAddress);
      Result := (not IsEmpty(AnIpAddress)) and (Length(AnIpAddress) > 0)
        and (not SameText(AnIpAddress, INVALID_IP_ADDRESS));
    end;

    function __IsUsableIpAddresses(const IpAddresses: TStringArray): Boolean;
    var
      i: Integer;
    begin
      Result := False;
      for i := Low(IpAddresses) to High(IpAddresses) do
        Result := Result or __IsUsableIpAddress(IpAddresses[i]);
    end;

  begin
{$IFDEF DEBUG}
{$IFDEF DEBUG_GET_NETWORK_CARD_ADAPTER}
    DebugLog('    _HandleIpAddressesSubnets: Index: ' + IntToStr(ANetworkCardAdapterItemIndex));
{$ENDIF}
{$ENDIF}
    Result := False;

    if __IsUsableIpAddresses(AIpAddresses) then
      with ANetworkAdapterCardList[ANetworkCardAdapterItemIndex] do
      begin
{$IFDEF DEBUG}
{$IFDEF DEBUG_GET_NETWORK_CARD_ADAPTER}
        DebugLog('      Extracting IP addresses for: ' + NetworkCardName);
{$ENDIF}
{$ENDIF}
        for i := Low(AIpAddresses) to High(AIpAddresses) do
        begin
          CurrentIpAddress := AIpAddresses[i];
          CurrentIpSubnet := EmptyStr;

          // Push the address to the correct array...
          IsValidAddress := __IsUsableIpAddress(CurrentIpAddress);
          if IsValidAddress then
          begin
             CurrentIpSubnet := AIpSubnets[i]; // no parsing needed

            // Determine if the item is an IPv4 or IPv6
            IpAddresses := @IPv4Addresses;
            if IsIP6(CurrentIpAddress) then
              IpAddresses := @IPv6Addresses
            else
              CurrentIpAddress := ParseInternetProtocolAddress(CurrentIpAddress); // Sanitize IPv4
              // No sanitization available for IPv6

            // Checking if this IP address wasn't already assigned... (avoid dupes)
            IpAddressHashKey := Concat(IntToStr(ANetworkCardAdapterItemIndex), CurrentIpAddress);
            if (IpAddressesHashMap.IndexOf(IpAddressHashKey) = -1) then
            begin
              // Add a item to the array
              IpEntryIndex := Length(IpAddresses^);
              SetLength(IpAddresses^, IpEntryIndex + 1);

              // Assign the values to the new item
              IpAddresses^[IpEntryIndex].Address := CurrentIpAddress;
              IpAddresses^[IpEntryIndex].Subnet := CurrentIpSubnet;

              // Pushing the IP to the Map, to avoid duplicates
              IpAddressesHashMap.Add(IpAddressHashKey, IpEntryIndex);
            end;
          end;

          // At least one address should be parsed
          Result := Result or IsValidAddress;
        end;
      end;
  end;

  function _HandleIpAddressesSubnetsFromRegistry(
    const ANetworkCardAdapterItemIndex: Integer; const SettingID: string): Boolean;
  var
    Reg: TRegistry;
    AIpAddress,
    AIpSubnet: string;
    IpAddresses,
    IpSubnets: TStringArray;

    function __RetrieveValues(const KeyPath: string): Boolean;
    begin
      Result := False;
      if Reg.OpenKey(Format(KeyPath, [SettingID]), False) then
      begin
        if Reg.ValueExists('IPAddress') then
          AIpAddress := RegistryReadMultiSzToSingle(Reg, 'IPAddress');
        if Reg.ValueExists('SubnetMask') then
          AIpSubnet := RegistryReadMultiSzToSingle(Reg, 'SubnetMask');
      end;
      Result := (not IsEmpty(AIpAddress)) and (not IsEmpty(AIpSubnet));
    end;

  begin
{$IFDEF DEBUG}
{$IFDEF DEBUG_GET_NETWORK_CARD_ADAPTER}
    DebugLog('      Network Card GUID: ' + SettingID);
{$ENDIF}
{$ENDIF}
    Result := False;
    AIpAddress := EmptyStr;
    AIpSubnet := EmptyStr;
    Reg := TRegistry.Create(KEY_READ);
    try
      // Extract IpAddress/IpSubnet from Registry
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      Result := __RetrieveValues('\SYSTEM\CurrentControlSet\Services\%s\Parameters\Tcpip'); // Win XP
      if not Result then
        Result := __RetrieveValues('\SYSTEM\CurrentControlSet\Services\Tcpip\Parameters\Interfaces\%s'); // Win 10

      if Result then
      begin
        IpAddresses := AIpAddress.Split(sLineBreak);
        IpSubnets := AIpSubnet.Split(sLineBreak);
        Result := _HandleIpAddressesSubnets(ANetworkCardAdapterItemIndex,
          IpAddresses, IpSubnets);
      end;
    finally
      FreeAndNil(Reg);
    end;
  end;

begin
{$IFDEF DEBUG}
{$IFDEF DEBUG_GET_NETWORK_CARD_ADAPTER}
  DebugLog('*** GetNetworkCardAdapterList: Start');
{$ENDIF}
{$ENDIF}

  Result := False;
  ANetworkAdapterCardList := Default(TNetworkCardAdapterList);
  SetLength(ANetworkAdapterCardList, 0);

  // Querying: Win32_NetworkAdapterConfiguration
  NetworkAdapterConfiguration := QueryWindowsManagementInstrumentation(
    'Win32_NetworkAdapterConfiguration', [
        'IPAddress',
        'IPSubnet',
        'MACAddress',
        'SettingID'
      ], 'MACAddress is not null');

  // Querying: Win32_NetworkAdapter
  WMIFieldNames := ['MACAddress', 'NetConnectionID'];
  if IsWindowsVistaOrGreater then
    Insert('InterfaceIndex', WMIFieldNames, MaxInt);
  NetworkAdapter := QueryWindowsManagementInstrumentation(
    'Win32_NetworkAdapter', WMIFieldNames,
    'NetConnectionID is not null');

{$IFDEF DEBUG}
{$IFDEF DEBUG_GET_NETWORK_CARD_ADAPTER}
  DebugLog('### NetworkAdapterConfiguration:');
  DumpWindowsManagementInstrumentation(NetworkAdapterConfiguration);

  DebugLog('### NetworkAdapter:');
  DumpWindowsManagementInstrumentation(NetworkAdapter);
{$ENDIF}
{$ENDIF}

  IpAddressesHashMap := TStringIntegerMap.Create;
  try
    // Analyzing NetworkAdapter
    for i := Low(NetworkAdapter) to High(NetworkAdapter) do
    begin
      // Retrieving MAC Address
      MacAddress := SanitizeMediaAccessControlAddress(
        GetWindowsManagementInstrumentationSingleValueByPropertyName(NetworkAdapter, 'MACAddress', i));

{$IFDEF DEBUG}
{$IFDEF DEBUG_GET_NETWORK_CARD_ADAPTER}
      DebugLog('  Processing NetworkAdapter: ' + MacAddress);
{$ENDIF}
{$ENDIF}

      if IsValidMediaAccessControlAddress(MacAddress) then
      begin
        // Valid MAC Address, handle this Network Adapter
        NetworkCardAdapter.MacAddress := MacAddress;

        // InterfaceIndex
        // In WinXP there is no InterfaceIndex field
        if IsWindowsVistaOrGreater then
        begin
          NetworkCardAdapter.InterfaceIndex :=
            StrToInt(GetWindowsManagementInstrumentationSingleValueByPropertyName(NetworkAdapter, 'InterfaceIndex', i));
        end;

        // NetworkCardName
        NetworkCardAdapter.NetworkCardName :=
          GetWindowsManagementInstrumentationSingleValueByPropertyName(NetworkAdapter, 'NetConnectionID', i);

        // Pushing the NetworkCardAdapter
        Insert(NetworkCardAdapter, ANetworkAdapterCardList, MaxInt);
      end;
    end;

    // Analyzing NetworkAdapterConfiguration
    Result := (Length(NetworkAdapterConfiguration) > 0); // Initialization, this will be reset by the loop below
    for i := Low(NetworkAdapterConfiguration) to High(NetworkAdapterConfiguration) do
    begin
      MacAddress := SanitizeMediaAccessControlAddress(
        GetWindowsManagementInstrumentationSingleValueByPropertyName(NetworkAdapterConfiguration, 'MACAddress', i));

{$IFDEF DEBUG}
{$IFDEF DEBUG_GET_NETWORK_CARD_ADAPTER}
      DebugLog('  Processing NetworkAdapterConfiguration: ' + MacAddress);
{$ENDIF}
{$ENDIF}

      if IsValidMediaAccessControlAddress(MacAddress) then
      begin
        CurrentCardItemIndex := FindMediaAccessControlAddress(ANetworkAdapterCardList, MacAddress);
        if CurrentCardItemIndex <> -1 then
        begin
          // Parsing IP address and net mask (subnet) from WMI data
          IsIpAddressExtractSuccess := _HandleIpAddressesSubnets(
            CurrentCardItemIndex,
            GetWindowsManagementInstrumentationMultipleValuesByPropertyName(NetworkAdapterConfiguration, 'IPAddress', i),
            GetWindowsManagementInstrumentationMultipleValuesByPropertyName(NetworkAdapterConfiguration, 'IPSubnet', i)
          );

          // Fail-back: try to read from registry if possible
          if not IsIpAddressExtractSuccess then
            IsIpAddressExtractSuccess := _HandleIpAddressesSubnetsFromRegistry(
              CurrentCardItemIndex,
              GetWindowsManagementInstrumentationSingleValueByPropertyName(NetworkAdapterConfiguration, 'SettingID', i)
            );

          // Saving the result...
          Result := Result and IsIpAddressExtractSuccess;
        end;
      end;
    end;

  finally
    IpAddressesHashMap.Free;
  end;

{$IFDEF DEBUG}
{$IFDEF DEBUG_GET_NETWORK_CARD_ADAPTER}
  DebugLog(sLineBreak + '>>> Dumping ANetworkAdapterCardList' + sLineBreak);
  DumpNetworkCardAdapterList(ANetworkAdapterCardList);
  DebugLog(sLineBreak + '*** GetNetworkCardAdapterList: End [Result: '
    + BoolToStr(Result, 'TRUE', 'FALSE') + ']');
{$ENDIF}
{$ENDIF}
end;

// Note: for debugging this complex function, define the thing below
// {$DEFINE DEBUG_GET_NETWORK_CARD_ADAPTER}
function GetNetworkCardAdapterList_OLD(
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
    InterfaceIndexField: string;

  begin
    BatchFileName := ChangeFileExt(GetTemporaryFileName, '.cmd');
    IpToMacFileName := ChangeFileExt(GetTemporaryFileName, '-ip2mac.tmp');
    MacToAdapterNameFileName := ChangeFileExt(GetTemporaryFileName, '-mac2name.tmp');

    InterfaceIndexField := EmptyStr;
    if IsWindowsVistaOrGreater then
      InterfaceIndexField := 'InterfaceIndex,';

    Buffer := TStringList.Create;
    try
      Buffer.Add('@echo off');
      Buffer.Add('echo. | wmic OS get Version > nul');
      // Get-WmiObject -Class Win32_NetworkAdapterConfiguration -ComputerName . | Select @{Name='Node';Expression={$env:computername}},@{Name='IPAddress';Expression={"{" + [string]::join(";", ($_.IPAddress)) + "}"}},@{Name='IPSubnet';Expression={"{" + [string]::join(";", ($_.IPSubnet)) + "}"}},MACAddress,SettingID | ConvertTo-Csv -NoTypeInformation -Delimiter "," | % {$_ -replace '"',''}
      Buffer.Add(Format('echo. | wmic NICCONFIG get IPAddress,IPSubnet,MACAddress,SettingID /FORMAT:CSV > "%s"', [IpToMacFileName]));
      // Get-NetAdapter | Where-Object Name -like '*' | Select-Object IfIndex,MacAddress,Name | ConvertTo-Csv
      Buffer.Add(Format('echo. | wmic NIC where "NetConnectionID like ''%%%%''" get %sMACAddress,NetConnectionID /FORMAT:CSV > "%s"', [InterfaceIndexField, MacToAdapterNameFileName]));
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
    HEADER_VISTA_OR_GREATER = 'Node,InterfaceIndex,MACAddress,NetConnectionID';
    HEADER_XP_OR_OLDER = 'Node,MACAddress,NetConnectionID';

  var
    i, j, StartIndex: Integer;
    Buffer: TStringList;
    Header, MacAddress: string;
    FieldCount, FieldIndex: Integer;

  begin
{$IFDEF DEBUG}
{$IFDEF DEBUG_GET_NETWORK_CARD_ADAPTER}
    WriteLn('ParseMacToAdapterName');
{$ENDIF}
{$ENDIF}

    Header := HEADER_XP_OR_OLDER;
    if IsWindowsVistaOrGreater then
      Header := HEADER_VISTA_OR_GREATER;

    FieldCount := GetSubStrCount(',', Header) + 1;
    Buffer := TStringList.Create;
    try
      j := 0;
      StartIndex := StringListSubstringIndexOf(MacToAdapterNameBuffer, Header) + 1;
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
          // In WinXP there is no InterfaceIndex field
          FieldIndex := -1;
          if IsWindowsVistaOrGreater then
            FieldIndex := 0;

          MacAddress := SanitizeMediaAccessControlAddress(Buffer[FieldIndex + 2]);
          if MacAddress <> EmptyStr then
          begin
            SetLength(ANetworkAdapterCardList, j + 1);
            if IsWindowsVistaOrGreater then
              ANetworkAdapterCardList[j].InterfaceIndex := StrToInt(Buffer[FieldIndex + 1]);
            ANetworkAdapterCardList[j].NetworkCardName := Buffer[FieldIndex + 3];
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
  end; // ParseMacToAdapterName

  procedure HandleIpAddressesFromGuid(const GUID: string; var AIpAddress,
    AIpSubnet: string);
  var
    Reg: TRegistry;

    function RetrieveValues(const KeyPath: string): Boolean;
    begin
      Result := False;
      if Reg.OpenKey(Format(KeyPath, [GUID]), False) then
      begin
        if Reg.ValueExists('IPAddress') then
          AIpAddress := RegistryReadMultiSzToSingle(Reg, 'IPAddress');
        if Reg.ValueExists('SubnetMask') then
          AIpSubnet := RegistryReadMultiSzToSingle(Reg, 'SubnetMask');
      end;
      Result := (not IsEmpty(AIpAddress)) and (not IsEmpty(AIpSubnet));
    end;

  begin
{$IFDEF DEBUG}
{$IFDEF DEBUG_GET_NETWORK_CARD_ADAPTER}
    DebugLog('      Network Card GUID: ' + GUID);
{$ENDIF}
{$ENDIF}
    AIpAddress := EmptyStr;
    AIpSubnet := EmptyStr;
    Reg := TRegistry.Create(KEY_READ);
    try
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      if not RetrieveValues('\SYSTEM\CurrentControlSet\Services\%s\Parameters\Tcpip') then // Win XP
        RetrieveValues('\SYSTEM\CurrentControlSet\Services\Tcpip\Parameters\Interfaces\%s'); // Win 10
    finally
      FreeAndNil(Reg);
    end;
  end;

  function IsUsableIpAddresses(const IpAddress: string): Boolean;
  const
    INVALID_IP_ADDRESS = '0.0.0.0';

  begin
    Result := (not IsEmpty(IpAddress)) and (not SameText(IpAddress, INVALID_IP_ADDRESS));
  end;

  procedure HandleIpAddresses(const ANetworkCardAdapterItemIndex: Integer;
    const AIpAddresses, AIpSubnets, ANetworkCardGuid: string);

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
{$IFDEF DEBUG}
{$IFDEF DEBUG_GET_NETWORK_CARD_ADAPTER}
    WriteLn('  HandleIpAddresses ANetworkCardGuid: ', ANetworkCardGuid);
{$ENDIF}
{$ENDIF}
    ExtractedIpAddresses := ExtractStr('{', '}', AIpAddresses);
    ExtractedSubnets := ExtractStr('{', '}', AIpSubnets);

{$IFDEF DEBUG}
{$IFDEF DEBUG_GET_NETWORK_CARD_ADAPTER}
    WriteLn('    Before processing: ExtractedIpAddresses: ', ExtractedIpAddresses);
{$ENDIF}
{$ENDIF}

    if not IsUsableIpAddresses(ExtractedIpAddresses) then
      // Fail-back: try to read from registry if possible
      HandleIpAddressesFromGuid(ANetworkCardGuid, ExtractedIpAddresses, ExtractedSubnets);

{$IFDEF DEBUG}
{$IFDEF DEBUG_GET_NETWORK_CARD_ADAPTER}
    WriteLn('    After processing: ExtractedIpAddresses: ', ExtractedIpAddresses,
      ', ExtractedSubnets: ', ExtractedSubnets);
{$ENDIF}
{$ENDIF}

    if not IsEmpty(ExtractedIpAddresses) then
    begin
      IpBuffer := TStringList.Create;
      SubnetBuffer := TStringList.Create;
      try
        StringToStringList(ExtractedIpAddresses, ';', IpBuffer);
        StringToStringList(ExtractedSubnets, ';', SubnetBuffer);

        with ANetworkAdapterCardList[ANetworkCardAdapterItemIndex] do
        begin
          for i := 0 to IpBuffer.Count - 1 do
          begin
            CurrentIpAddress := IpBuffer[i];
            CurrentIpSubnet := EmptyStr;

            if IsUsableIpAddresses(CurrentIpAddress) then
            begin
              CurrentIpSubnet := SubnetBuffer[i]; // no parsing needed

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
    HEADER = 'Node,IPAddress,IPSubnet,MACAddress,SettingID';

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
            HandleIpAddresses(j, Buffer[1], Buffer[2], Buffer[4]);
          end;
        end;
      end;
     finally
       Buffer.Free;
     end;
  end;

  // Thanks http://delphi.cjcsoft.net/viewthread.php?tid=46612
  procedure DeleteElement(
    var WorkingNetworkCardAdapterList: TNetworkCardAdapterList;
    const ItemIndex: Integer
  );
  var
     ArrayLength, j : Integer;

  begin
    ArrayLength := Length(WorkingNetworkCardAdapterList);
    if (ItemIndex < Low(WorkingNetworkCardAdapterList))
      and (ItemIndex > High(WorkingNetworkCardAdapterList)) then
        Exit; // out of bounds

    // shift items if the requested item isn't the last
    if (ItemIndex <> ArrayLength - 1) then
      for j := ItemIndex to ArrayLength - 2 do
        WorkingNetworkCardAdapterList[j] := WorkingNetworkCardAdapterList[j + 1];

    // Remove one item
    SetLength(WorkingNetworkCardAdapterList, ArrayLength - 1);
  end;

  procedure CleanupMacAddresses;
  var
    i: Integer;
    Item: TNetworkCardAdapter;

  begin
    for i := High(ANetworkAdapterCardList) downto Low(ANetworkAdapterCardList) do
    begin
      Item := ANetworkAdapterCardList[i];
{$IFDEF DEBUG}
{$IFDEF DEBUG_GET_NETWORK_CARD_ADAPTER}
      DebugLog('*** CleanupMacAddresses: Item = ' + Item.NetworkCardName);
{$ENDIF}
{$ENDIF}
      if (Length(Item.IPv4Addresses) = 0) and (Length(Item.IPv6Addresses) = 0) then
      begin
{$IFDEF DEBUG}
{$IFDEF DEBUG_GET_NETWORK_CARD_ADAPTER}
        DebugLog('  Deleting ItemIndex = ' + IntToStr(i));
{$ENDIF}
{$ENDIF}
        DeleteElement(ANetworkAdapterCardList, i);
      end;
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

      // Remove useless MAC addresses
      CleanupMacAddresses;
    finally
      IpToMacBuffer.Free;
      MacToAdapterNameBuffer.Free;
    end;
  end;
end;

// See: https://wiki.lazarus.freepascal.org/fphttpclient#Get_external_IP_address
function GetExternalInternetProtocolAddress: string;
const
  TEST_URL = 'http://checkip.dyndns.org';
  INVALID_IP_ADDRESS = 'Got invalid results getting external IP address. Details: %s %s';
  GENERIC_EXCEPTION = 'Error retrieving external IP address: %s';

var
  HTTPClient: TFPHTTPClient;
  IPRegEx: TRegExpr;
  RawData: string;

begin
  try
    HTTPClient := TFPHTTPClient.Create(nil);
    IPRegEx := TRegExpr.Create;
    try
      //returns something like:
      {
<html><head><title>Current IP Check</title></head><body>Current IP Address: 111.222.333.444</body></html>
      }
      RawData := HTTPClient.Get(TEST_URL);
      // adjust for expected output; we just capture the first IP address now:
      IPRegEx.Expression := RegExprString('\b\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}\b');
      //or
      //\b(?:\d{1,3}\.){3}\d{1,3}\b
      if IPRegEx.Exec(RawData) then
        Result := IPRegEx.Match[0]
      else
        raise EExternalInternetProtocolAddress
          .CreateFmt(INVALID_IP_ADDRESS, [LineEnding, RawData]);
    except
      on E: Exception do
        raise EExternalInternetProtocolAddress
          .CreateFmt(GENERIC_EXCEPTION, [E.Message]);
    end;
  finally
    HTTPClient.Free;
    IPRegEx.Free;
  end;
end;

function IsInternetConnectionAvailable: Boolean;
begin
  Result := True;
  try
    GetExternalInternetProtocolAddress;
  except
    Result := False;
  end;
end;

{$IFDEF DEBUG}
procedure DumpNetworkCardAdapterList(
  var ANetworkAdapterCardList: TNetworkCardAdapterList);

  function DumpIpAdresses(const AIpAddresses: TIpAddresses): string;
  var
    j: Integer;
    Separator: string;

  begin
    Result := EmptyStr;
    Separator := EmptyStr;
    for j := Low(AIpAddresses) to High(AIpAddresses) do
    begin
      Result := Result + Separator + AIpAddresses[j].Address + '/' + AIpAddresses[j].Subnet;
      Separator := ', ';
    end;
  end;

var
  i: Integer;
  NetworkCardAdapter: TNetworkCardAdapter;
  Separator,
  InterfaceIndex: string;

begin
  Separator := EmptyStr;
  for i := Low(ANetworkAdapterCardList) to High(ANetworkAdapterCardList) do
  begin
    NetworkCardAdapter := ANetworkAdapterCardList[i];

    InterfaceIndex := EmptyStr;
    if IsWindowsVistaOrGreater then
      InterfaceIndex := '  InterfaceIndex: '
        + IntToStr(NetworkCardAdapter.InterfaceIndex) + sLineBreak;

    DebugLog(
        Separator +
        'Interface #' + IntToStr(i) + ':' + sLineBreak +
        '  NetworkCardName: ' + NetworkCardAdapter.NetworkCardName + sLineBreak +
        InterfaceIndex +
        '  MacAddress: ' + NetworkCardAdapter.MacAddress + sLineBreak +
        '  IPv4Addresses: ' + DumpIpAdresses(NetworkCardAdapter.IPv4Addresses) + sLineBreak +
        '  IPv6Addresses: ' + DumpIpAdresses(NetworkCardAdapter.IPv6Addresses)
      );

    Separator := sLineBreak;
  end;
end;
{$ENDIF}

end.

