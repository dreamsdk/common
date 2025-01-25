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
    procedure HandleWorkingDirectoryVariable;
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

procedure TRunCommandEx.HandleWorkingDirectoryVariable;
begin
  if (WorkingDirectory <> EmptyStr) then
  begin
    Environment.Add('_WORKING_DIRECTORY='
      + SystemToDreamSdkPath(WorkingDirectory));
  end;
end;

procedure TRunCommandEx.Execute;
begin
  HandleHomeVariable;
  HandleWorkingDirectoryVariable;
  inherited Execute;
end;

end.

