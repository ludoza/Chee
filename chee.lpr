program chee;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  tahadmin, webclient_form, mqttclient_form, main, gridframe, ezutil;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  //Application.ShowMainForm := False;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TfrmWebClient, frmWebClient);
  Application.CreateForm(TfrmMqttClient, frmMqttClient);
  Application.CreateForm(TTahForm, TahForm);
  Application.Run;
end.

