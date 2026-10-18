unit InstMode;

{$mode objfpc}{$H+}

interface

(* Indicate if we are running the post-install steps, i.e., right after the
   DreamSDK Setup. This is set by DreamSDK Manager (--post-install switch) or by
   the DreamSDK Setup post-install helper library. *)
function IsPostInstallMode: Boolean;

procedure SetPostInstallMode(const Value: Boolean);

implementation

var
  PostInstallMode: Boolean = False;

function IsPostInstallMode: Boolean;
begin
  Result := PostInstallMode;
end;

procedure SetPostInstallMode(const Value: Boolean);
begin
  PostInstallMode := Value;
end;

end.

