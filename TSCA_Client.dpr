program TSCA_Client;

uses
  Vcl.Forms,
  MainUnit in 'MainUnit.pas' {FLogonUser},
  ExtUnit in 'ExtUnit.pas',
  System.SyncObjs;

var
  CheckEvent: TEvent;

  {$R *.res}

begin
  CheckEvent := TEvent.Create(nil, false, true, 'Client_CheckExist');
  if CheckEvent.WaitFor(10) = wrSignaled then
  begin
    Application.Initialize;
    Application.MainFormOnTaskbar := True;
    Application.CreateForm(TFLogonUser, FLogonUser);
    StartClient;
  end;
  CheckEvent.Free;
end.
