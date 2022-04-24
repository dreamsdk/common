(*******************************************************************************

UtilWMI
https://forum.lazarus.freepascal.org/index.php/topic,24490.0.html

Copyright (c) 2016 Jurassic Pork, Molly, SiZiOUS

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
 
// 0.1  Jurassic Pork July 2015
// 0.2  Molly  January 2016 : improvement : fpc 3.0 compatibility + usage of  TFPObjectList
 
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
  Classes,
  Contnrs;

function GetWMIInfo(const WMIClass: string;
  const WMIPropertyNames: array of string; const Condition: string = ''): TFPObjectList;

implementation
 
uses
{$IFDEF USE_DIALOG}
  Dialogs,
{$ENDIF}
  Variants,
  ActiveX,
  ComObj,
  SysUtils;
 
function VarArrayToStr(Value: Variant): String;
var
  i : Integer;
begin
  Result := '[';
  for i := VarArrayLowBound(Value, 1) to VarArrayHighBound(Value, 1) do
  begin
    if Result <> '[' then Result := Result + ',';
    if not VarIsNull(Value[i]) then
    begin
      if VarIsArray(Value[i]) 
      then Result := Result + VarArrayToStr(Value[i])
      else Result := Result + VartoStr(Value[i])
    end
    else Result := Result + '<null>';
  end;
  Result := Result + ']';
end;

function GetWMIInfo(const WMIClass: string;
  const WMIPropertyNames: Array of String; const Condition: string = ''): TFPObjectList;
const
  wbemFlagForwardOnly = $00000020;

var
  FSWbemLocator,
  objWMIService,
  colWMI: Variant;
  oEnumWMI: IEnumvariant;
  nrValue: LongWord;
{$IFDEF USE_OLD_WMI}
  objWMI: Variant;                 // FPC < 3.0 requires WMIobj to be an variant, not an OleVariant
  nr: PLongWord;                   // FPC < 3.0 requires IEnumvariant.next to supply a pointer to a longword for # returned values
{$ELSE}
  objWMI: OLEVariant;              // FPC 3.0 requires WMIobj to be an olevariant, not a variant
  nr: LongWord absolute nrValue;   // FPC 3.0 requires IEnumvariant.next to supply a longword variable for # returned values
{$ENDIF}
  WMIProperties: string;
  Request,
  PropertyName,
  PropertyStrVal: string;
  i: Integer;
  WMIProp: TStringList;

begin
{$IFDEF USE_OLD_WMI}
  nr := @nrValue;
{$ENDIF}

  // Prepare the search query
  WMIProperties := EmptyStr;
  for i := Low(WMIPropertyNames) to High(WMIPropertyNames) do
    WMIProperties := WMIProperties + WMIPropertyNames[i] + ',';
  Delete(WMIProperties, Length(WMIProperties), 1);

  // Let FPObjectList take care of freeing the objects
  Result := TFPObjectList.Create(True);
  try
    FSWbemLocator := CreateOleObject('WbemScripting.SWbemLocator');
    objWMIService := FSWbemLocator.ConnectServer('localhost', 'root\CIMV2',
      EmptyStr, EmptyStr);

    if Condition = EmptyStr then
      Request := Format('SELECT %s FROM %s', [WMIProperties, WMIClass])
    else
      Request := Format('SELECT %s FROM %s %s', [WMIProperties, WMIClass, Condition]);

    // Start Request
    colWMI := objWMIService.ExecQuery(WideString(Request), 'WQL',
      wbemFlagForwardOnly);

    // Enum for requested results
    oEnumWMI := IUnknown(colWMI._NewEnum) as IEnumVariant;

    // Enumerate results from query, one by one
    while oEnumWMI.Next(1, objWMI, nr) = 0 do
    begin
      // Store all property name/value pairs for this enum to TStringList.
      WMIprop := TStringList.Create;
      for i := Low(WMIPropertyNames) to High(WMIPropertyNames) do
      begin
        PropertyName := WMIPropertyNames[i];
        if not VarIsNull(objWMI.Properties_.Item(WideString(PropertyName)).value) then
        begin
          if VarIsArray(objWMI.Properties_.Item(WideString(PropertyName)).value) then
            PropertyStrVal := VarArrayToStr(objWMI.Properties_.Item(WideString(PropertyName)).value)
          else
            PropertyStrVal := VartoStr(objWMI.Properties_.Item(WideString(PropertyName)).value);
        end
        else
          PropertyStrVal := '<null>';
        WMIProp.Add(PropertyName + '=' + PropertyStrVal);
      end;
      // Add properties from this enum to FPObjectList as TStringList
      Result.Add(WMIProp);
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
 
end.

