unit mqttclient_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Buttons, ActnList,
  { chee }
  mqttgate;

type

  { TfrmMqttClient }

  TfrmMqttClient = class(TForm)

    UnsubscribeFromTopic: TAction;
    ListenToTopic: TAction;
    Disconnect: TAction;
    Connect: TAction;
    ActionList1: TActionList;
    btnConnect: TBitBtn;
    btnDisconnect: TBitBtn;
    btnDisconnect1: TBitBtn;
    btnDisconnect2: TBitBtn;
    edtUri: TEdit;
    edtUri1: TEdit;
    edtTopic: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
    StatusBar1: TStatusBar;
    TreeView1: TTreeView;
    procedure btnConnectClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure ConnectExecute(Sender: TObject);
  private
    fMqtt: TMQTTGate;
  public

  end;

var
  frmMqttClient: TfrmMqttClient;

implementation

{$R *.lfm}

uses main; { BAD!1 but needed for the macro writeln hack }

{ TfrmMqttClient }

procedure TfrmMqttClient.btnConnectClick(Sender: TObject);
begin
  fMqtt := TMQTTGate.Create;
  fMqtt.Writeln:= @(form1.MemoOutput.lines.add);

  fMqtt.Topic := edtTopic.Caption;
  fMqtt.DoRun;
end;

procedure TfrmMqttClient.btnDisconnectClick(Sender: TObject);
begin
   fMqtt.DoTerminate;
end;

procedure TfrmMqttClient.ConnectExecute(Sender: TObject);
begin

end;

end.

