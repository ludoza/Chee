unit mqttclient_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Buttons, ActnList,
  { chee }
  mqttgate, mqtt;

type

  { TfrmMqttClient }

  TfrmMqttClient = class(TForm)
    edtNick: TEdit;
    Label5: TLabel;
    Send: TAction;
    btnSend: TButton;
    ChatMemo: TMemo;
    edtMessage: TEdit;
    Label4: TLabel;
    Panel2: TPanel;

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
    procedure ConnectExecute(Sender: TObject);
    procedure DisconnectExecute(Sender: TObject);
    procedure edtMessageKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ListenToTopicExecute(Sender: TObject);
    procedure OnMessage(Sender: TObject; topic, payload: TMqttString; isRetain: boolean);
    procedure SendExecute(Sender: TObject);
    procedure UnsubscribeFromTopicExecute(Sender: TObject);
  private
    fMqtt: TMQTTGate;
  public

  end;

var
  frmMqttClient: TfrmMqttClient;

implementation

{$R *.lfm}

uses
  tahadmin,
  fpjson, jsonparser;

{ TfrmMqttClient }

procedure TfrmMqttClient.ConnectExecute(Sender: TObject);
begin
  fMqtt := TMQTTGate.Create;
  fMqtt.Writeln:= @(TahForm.MemoOutput.lines.add);
  fMqtt.Topic := edtTopic.Caption;
  fMqtt.AddOnMessage(@OnMessage);
  fMqtt.DoRun;
end;

procedure TfrmMqttClient.DisconnectExecute(Sender: TObject);
begin
     fMqtt.DoTerminate;
end;

procedure TfrmMqttClient.edtMessageKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (key = 13) then SendExecute(Sender)
end;

procedure TfrmMqttClient.ListenToTopicExecute(Sender: TObject);
begin
  fMqtt.Topic := edtTopic.Caption;
  fMqtt.AddOnMessage(@OnMessage);
end;

procedure TfrmMqttClient.OnMessage(Sender: TObject; topic,
  payload: TMqttString; isRetain: boolean);
var
  jData : TJSONData;
  jObject : TJSONObject;
  n, m, c: TJSONData;
begin
  jData := GetJSON(payload);
  jObject := TJSONObject(jData);
  if jObject.Find('n', n) and jObject.Find('m', m) then
  begin
    ChatMemo.Append(n.AsString + ': ' + m.AsString);
  end
  else if jObject.Find('c', c) then ChatMemo.lines.append('counter = ' +  c.AsString)
  else ChatMemo.lines.append('unknown payload = ' + jData.AsJSON)
end;

procedure TfrmMqttClient.SendExecute(Sender: TObject);
var
  jData : TJSONData;
  jObject : TJSONObject;
begin
  jData := GetJSON('{}');
  jObject := TJSONObject(jData);
  jObject.Strings['n'] := edtNick.Text;
  jObject.Strings['m'] := edtMessage.Text;
  fMqtt.sendMessage(fMqtt.topic, jData.AsJSON);
  edtMessage.Text := '';
end;

procedure TfrmMqttClient.UnsubscribeFromTopicExecute(Sender: TObject);
begin
  { TODO }
end;

end.
