unit mqttclient_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Buttons, ActnList,
  { chee }
  mqttmodule, mqtt;

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
    MqttActionList: TActionList;
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
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListenToTopicExecute(Sender: TObject);
    procedure OnMessage(Sender: TObject; topic, payload: TMqttString; isRetain: boolean);
    procedure SendExecute(Sender: TObject);
    procedure UnsubscribeFromTopicExecute(Sender: TObject);
    function WriteDebug(const S: string): integer;
  private
    ForcedShow: Boolean;
  public

  end;

var
  frmMqttClient: TfrmMqttClient;

implementation

{$R *.lfm}

uses
  main,
  fpjson,
  jsonparser,
  ezutil;

{ TfrmMqttClient }

procedure TfrmMqttClient.ConnectExecute(Sender: TObject);
begin
  MQTTGate.OnWriteDebug :=  @(WriteDebug);
  MQTTGate.Start;

  //fMqtt.Topic := edtTopic.Caption;
  //fMqtt.AddOnMessage(@OnMessage);
  //fMqtt.DoRun;
end;

procedure TfrmMqttClient.DisconnectExecute(Sender: TObject);
begin
  MQTTGate.Stop;
end;

procedure TfrmMqttClient.edtMessageKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (key = 13) then SendExecute(Sender)
end;

procedure TfrmMqttClient.FormCreate(Sender: TObject);
var
  i: Integer;
  vItem: TDispatcherItem;
begin
  // our js emulation namespace
  vItem := TDispatcherItem(MainForm.Dispatcher.Add);
  vItem.DisplayName:= 'js:mqtt.sendMessage';
  vItem.Action := Send;

  for i := 0 to pred(MqttActionList.ActionCount) do
  begin
    vItem := TDispatcherItem(MainForm.Dispatcher.Add);
    vItem.Action := MqttActionList[i];
    vItem.DisplayName:= 'fp:mqtt.' + TAction(vItem.Action).Caption;
  end;
  ForcedShow := True;
  Show;
end;

procedure TfrmMqttClient.FormShow(Sender: TObject);
begin
  if ForcedShow then
  begin
    ForcedShow := False;
    Hide;
  end;
end;

procedure TfrmMqttClient.ListenToTopicExecute(Sender: TObject);
begin
  //fMqtt.Topic := edtTopic.Caption;
  //fMqtt.AddOnMessage(@OnMessage);
end;

procedure TfrmMqttClient.OnMessage(Sender: TObject; topic,
  payload: TMqttString; isRetain: boolean);
var
  jData : TJSONData;
  jObject : TJSONObject;
  n, m, c: TJSONData;
  jWelcomeData: TJSONData;
begin
  jData := GetJSON(payload);
  jObject := TJSONObject(jData);
  if jObject.Find('n', n) and jObject.Find('m', m) then
  begin
    ChatMemo.Append(n.AsString + ': ' + m.AsString);
    if self.CanFocus then
      self.SetFocus
    else
    begin
      self.Show;
      if self.CanFocus then
        self.SetFocus;
    end;
    if m.AsString = 'Hello' then
    begin
      jWelcomeData := GetJSON('{"n": "AutoChee", "m": "Welcome"}');
      Main.MainForm.Dispatcher.trigger('js:mqtt.sendMessage', TObject(jWelcomeData));
    end;
  end
  else if jObject.Find('c', c) then ChatMemo.lines.append('counter = ' +  c.AsString)
  else ChatMemo.lines.append('unknown payload = ' + jData.AsJSON)
end;

procedure TfrmMqttClient.SendExecute(Sender: TObject);
var
  vComp: TComponent;
  vObject : TObject;
  jData : TJSONData;
  jObject : TJSONObject;
  vTag: PtrInt;
begin
  vComp := TComponent(Sender);
  if TComponent(Sender).Tag > 0 then
  begin
    vTag := PtrInt(vComp.Tag);
    vObject := TObject(vTag);
    jObject :=  TJSONObject(vObject);
  end else
    jObject := TJSONObject(GetJSON('{}'));
  if jObject.IndexOfName('n') = -1 then
    jObject.Strings['n'] := edtNick.Text;
  if jObject.IndexOfName('m') = -1 then
    jObject.Strings['m'] := edtMessage.Text;
  MqttGate.sendMessage('World', jObject.AsJSON); // TODO remove hard coded topic
  edtMessage.Text := '';
end;

procedure TfrmMqttClient.UnsubscribeFromTopicExecute(Sender: TObject);
begin
  { TODO }
end;

function TfrmMqttClient.WriteDebug(const S: string): integer;
begin
  Result := MainForm.Memo1.lines.add(s);
end;

end.
