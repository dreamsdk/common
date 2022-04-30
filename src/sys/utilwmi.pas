(*******************************************************************************

UtilWMI for Free Pascal/Lazarus
A very simple utility for querying Windows Management Instrumentation (WMI).
https://forum.lazarus.freepascal.org/index.php/topic,24490.0.html

Copyright (c) 2016-2022 Jurassic Pork, Molly and SiZiOUS

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:
The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

*******************************************************************************)
unit UtilWMI;
 
// 0.1 (July 2015)  Jurassic Pork: first public release
// 0.2 (January 2016) Molly: improvement: fpc 3.0 compatibility + usage of TFPObjectList
 
// Changes 2016-jan-02 (Molly)
// - updated/corrected comments
// - Introduction of variable nrValue.
// - Generic solution for variable nr, using nrValue (inc. pointer assignment)
// - re-introduction of calling ShowMessage() when exception occurs (code only
//   active when USE_DIALOG is defined) + reorganized defines
 
// 0.3  Molly  November 2016 : improvement : support for variant arrays
// Changes 2016-nov-11 (Molly)
// - Add support for variant arrays

// 0.4  SiZiOUS  April 2022: refactoring: comments
 
{$MODE OBJFPC}{$H+}{$HINTS ON}
 
{$IF FPC_FULLVERSION > 29999} // FPC > 2.9.9
  {$INFO "Use new WMI"}
{$ELSE}
  {$INFO "Use old WMI"}
  {$DEFINE USE_OLD_WMI}
{$ENDIF}
 
{
  Enable this define if wanting to use ShowMessage() to inform about an
  exception.

  Please realize that using unit Dialogs can 'clash', as both Freevision and
  Lazarus have a unit with the same name.
}
// {$DEFINE USE_DIALOG}

interface
 
uses
  SysUtils,
  Classes;

type
  TWindowsManagementInstrumentationProperty = record
    Key: string;
    Values: TStringArray;
  end;
  TWindowsManagementInstrumentationProperties = array of TWindowsManagementInstrumentationProperty;
  TWindowsManagementInstrumentationResult = array of TWindowsManagementInstrumentationProperties;

function GetWMIInfo(const WMIClass: string;
  const WMIPropertyNames: Array of String;
  const Condition: string = ''): TWindowsManagementInstrumentationResult;

implementation
 
uses
{$IFDEF USE_DIALOG}
  Dialogs,
{$ENDIF}
  Variants,
  ActiveX,
  ComObj;

function VarArrayToStr(Value: Variant): TStringArray;
var
  i, j: Integer;
  Buffer: TStringArray;

begin
  Result := Default(TStringArray);
  SetLength(Result, 0);
  for i := VarArrayLowBound(Value, 1) to VarArrayHighBound(Value, 1) do
  begin
    if not VarIsNull(Value[i]) then
    begin
      if VarIsArray(Value[i]) then
      begin
        Buffer := VarArrayToStr(Value[i]);
        for j := Low(Buffer) to High(Buffer) do
          Insert(Buffer[j], Result, MaxInt);
      end
      else
        Insert(VarToStr(Value[i]), Result, MaxInt);
    end
    else
      Insert(#0, Result, MaxInt);
  end;
end;

function CombineStringArray(const Array1, Array2: TStringArray): TStringArray;
var
  i: Integer;

begin
  Result := Array1;
  for i := Low(Array2) to High(Array2) do
    Insert(Array2[i], Result, MaxInt);
end;

function GetWMIInfo(const WMIClass: string;
  const WMIPropertyNames: Array of String;
  const Condition: string = ''): TWindowsManagementInstrumentationResult;
const
  WhereKeyword = 'WHERE';
  wbemFlagForwardOnly = $00000020;

var
  FSWbemLocator,
  objWMIService,
  colWMI: Variant;
  oEnumWMI: IEnumVariant;
  nrValue: LongWord;
{$IFDEF USE_OLD_WMI}
  objWMI: Variant;                 // FPC < 3.0 requires WMIobj to be an variant, not an OleVariant
  NR: PLongWord;                   // FPC < 3.0 requires IEnumvariant.next to supply a pointer to a longword for # returned values
{$ELSE}
  objWMI: OLEVariant;              // FPC 3.0 requires WMIobj to be an olevariant, not a variant
  NR: LongWord absolute nrValue;   // FPC 3.0 requires IEnumvariant.next to supply a longword variable for # returned values
{$ENDIF}
  WMIProperties,
  WMICondition: string;
  Request,
  PropertyName: string;
  PropertyValue: Variant;
  i: Integer;
  Properties: TWindowsManagementInstrumentationProperties;
  Buffer: TStringArray;
  SingleProperty: TWindowsManagementInstrumentationProperty;

begin
  Result := Default(TWindowsManagementInstrumentationResult);

{$IFDEF USE_OLD_WMI}
  nr := @nrValue;
{$ENDIF}

  // Prepare the search query
  WMIProperties := EmptyStr;
  for i := Low(WMIPropertyNames) to High(WMIPropertyNames) do
    WMIProperties := WMIProperties + WMIPropertyNames[i] + ',';
  Delete(WMIProperties, Length(WMIProperties), 1);

  // Let FPObjectList take care of freeing the objects
  try
    FSWbemLocator := CreateOleObject('WbemScripting.SWbemLocator');
    objWMIService := FSWbemLocator.ConnectServer('localhost', 'root\CIMV2',
      EmptyStr, EmptyStr);

    WMICondition := Condition;
    if (not SameText(WMICondition, EmptyStr)) and (not WMICondition.StartsWith(WhereKeyword, True)) then
      WMICondition := Concat(WhereKeyword, ' ', WMICondition);

    Request := Format('SELECT %s FROM %s %s', [WMIProperties, WMIClass, WMICondition]);

    // Start Request
    colWMI := objWMIService.ExecQuery(WideString(Request), 'WQL',
      wbemFlagForwardOnly);

    // Enum for requested results
    oEnumWMI := IUnknown(colWMI._NewEnum) as IEnumVariant;

    // Enumerate results from query, one by one
    while oEnumWMI.Next(1, objWMI, NR) = 0 do
    begin
      // Store all property name/value pairs for this enum
      Properties := Default(TWindowsManagementInstrumentationProperties);
      SetLength(Properties, 0);

      for i := Low(WMIPropertyNames) to High(WMIPropertyNames) do
      begin
        SingleProperty := Default(TWindowsManagementInstrumentationProperty);

        PropertyName := WMIPropertyNames[i];
        PropertyValue := objWMI.Properties_.Item(WideString(PropertyName)).Value;

        Buffer := Default(TStringArray);
        SetLength(Buffer, 0);

        if not VarIsNull(PropertyValue) then
        begin
          if VarIsArray(PropertyValue) then
          begin
            Buffer := CombineStringArray(Buffer, VarArrayToStr(PropertyValue));
          end
          else
            Insert(PropertyValue, Buffer, MaxInt);
        end
        else
          Insert(#0, Buffer, MaxInt);

        SingleProperty.Key := PropertyName;
        SingleProperty.Values := Buffer;

        Insert(SingleProperty, Properties, MaxInt);
      end;
      Insert(Properties, Result, MaxInt);
    end;
   except
{$IFDEF USE_DIALOG}
      on e: Exception do
        ShowMessage('Error WMI with ' + Request + sLineBreak + 'Error : ' + e.Message);
{$ELSE}
      // Replace Raise with more appropiate exception if wanted.
      on e: Exception do
        raise;
{$ENDIF}
  end;
end;

(*
function QueryWindowsManagementInstrumentation(const WMIClass: string;
  const WMIPropertyNames: array of string; const Condition: string = ''): TWindowsManagementInstrumentationResult;
var
  Output: TFPObjectList;
  i: Integer;

begin
  Output := GetWMIInfo(WMIClass, WMIPropertyNames, Condition);
  try
    for i := 0 to Pred(WMIResult.Count) do
    begin
      Buffer := TStringList(WMIResult[i]);
      if Assigned(Buffer) then
      begin
        WriteLn(Buffer.Text);
      end;
    end;
  finally
    Output.Free;
  end;
end;
*)

end.

