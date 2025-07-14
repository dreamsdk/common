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
  RefBase,
  SysTools;

{ TRunCommandEx }

procedure TRunCommandEx.HandleHomeVariable;
begin
  // define DREAMSDK_HOME if value is not defined
  if not IsDefinedInstallationBaseDirectoryVariable then
  begin
    Environment.Add(Format('%s=%s', [
      GetBaseEnvironmentVariableName, GetBaseInstallationHomeDirectory])
    );
  end;
end;

procedure TRunCommandEx.HandleWorkingDirectoryVariable;
var
  LogContext: TLogMessageContext;
  WorkingDirectoryUnix: string;

begin
  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%}, ClassName);
  try
    LogMessage(LogContext, Format('WorkingDirectory: "%s"', [WorkingDirectory]));
    if (WorkingDirectory <> EmptyStr) then
    begin
      WorkingDirectoryUnix := SystemToDreamSdkPath(WorkingDirectory);
      LogMessage(LogContext, Format('WorkingDirectoryUnix: "%s"', [WorkingDirectoryUnix]));
      Environment.Add('_WORKING_DIRECTORY=' + WorkingDirectoryUnix);
    end;
  finally
    LogMessageExit(LogContext);
  end;
end;

procedure TRunCommandEx.Execute;
begin
  HandleHomeVariable;
  HandleWorkingDirectoryVariable;
  inherited Execute;
end;

end.

