unit RunCmdEx;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  RunCmd;

type
  { TRunCommandEx }
  TRunCommandEx = class(TRunCommand)
  private
    procedure HandleHomeVariable;
  protected
    procedure Execute; override;
  public
  end;

implementation

uses
  RefBase;

{ TRunCommandEx }

procedure TRunCommandEx.HandleHomeVariable;
begin
  // define DREAMSDK_HOME if value is not defined
  if not IsDefinedInstallationBaseDirectoryVariable then
  begin
    Environment.Add(Format('%s=%s', [
      GetBaseEnvironmentVariableName, GetInstallationBaseDirectory])
    );
  end;
end;

procedure TRunCommandEx.Execute;
begin
  HandleHomeVariable;
  inherited Execute;
end;

end.

