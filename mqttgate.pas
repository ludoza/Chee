unit mqttgate;

{ sourced from https://github.com/heX16/mqtt-free-pascal/tree/master/examples/fpcConsole thanks @heX16 }

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  MQTT,
  syncobjs, // TCriticalSection
  fptimer,
  MQTTReadThread, // TPublishEvent
  ezutil
  ;

const
  MQTT_Server = 'iot.eclipse.org';
  MQTT_Port = 1883;

type
  PPublishEvent = ^TPublishEvent;


  { TMQTTGate }

  TMQTTGate = class(TObject)
    fOnOnMessages: TList;
    // https://forum.lazarus.freepascal.org/index.php/topic,10370.msg51275.htm
    fOnMessageSender: TObject;
    fOnMessageTopic, fOnMessagePayload: TMqttString;
    fOnMessageIsRetain: boolean;
    fNick: String;
  protected

    fWriteln: TWriteDebug;
    fTopic: string;
    MQTTClient: TMQTTClient;

    SyncCode: TCriticalSection;
    TimerTick: TFPTimer;
    TimerPing: TFPTimer;
    cnt: integer;


    // Unsafe events! Called from MQTT thread (TMQTTReadThread)
    procedure OnConnAck(Sender: TObject; ReturnCode: integer);
    procedure OnPingResp(Sender: TObject);
    procedure OnSubAck(Sender: TObject; MessageID: integer; GrantedQoS: integer);
    procedure OnUnSubAck(Sender: TObject);
    procedure OnMessage(Sender: TObject; topic, payload: TMqttString; isRetain: boolean);

    procedure CallOnMessages(Sender: TObject; topic, payload: TMqttString;
      isRetain: boolean);

    procedure OnTimerTick(Sender: TObject);
    procedure OnTimerPing(Sender: TObject);
  public
    property Nick: String read fNick write fNick;
    property writeln: TWriteDebug read fwriteln write fwriteln;
    procedure AddOnMessage(Event: TPublishEvent);

    property Topic: string read fTopic write fTopic;
    procedure DoRun;
    procedure DoTerminate;
    procedure sendMessage(aTopic, aMsg: string);

    constructor Create;
    destructor Destroy; override;
  end;

{old: const
  { ^C }
  //ContrBreakSIG = ^C; // yes, its valid string! (OMG!)
  //ContrBreakSIG = #$03;
}

implementation

uses
  fpjson, jsonparser, dateutils;

function NewTimer(Intr: integer; Proc: TNotifyEvent; AEnable: boolean = False): TFPTimer;
begin
  Result := TFPTimer.Create(nil);
  Result.UseTimerThread := False;
  Result.Interval := Intr;
  Result.OnTimer := Proc;
  Result.Enabled := AEnable;
end;

{ TMQTTGate }


constructor TMQTTGate.Create;
begin
  inherited Create;

  fOnOnMessages := TList.Create;
end;

destructor TMQTTGate.Destroy;
begin
  fOnOnMessages.Free;
  inherited Destroy;
end;


procedure TMQTTGate.AddOnMessage(Event: TPublishEvent);
var
  p: PPublishEvent;
begin
  New(p);
  p^ := Event;
  fOnOnMessages.Add(p);
end;

procedure TMQTTGate.CallOnMessages(Sender: TObject; topic, payload: TMqttString;
  isRetain: boolean);
var
  i: integer;
  Event: TPublishEvent;
begin
  for i := 0 to Pred(fOnOnMessages.Count) do
  begin
    Event := TPublishEvent(fOnOnMessages.Items[i]^);
    Event(Self, topic, payload, isRetain);
  end;
end;

procedure TMQTTGate.OnConnAck(Sender: TObject; ReturnCode: integer);
begin
  SyncCode.Enter;
  writeln('ConnAck');
  MQTTClient.Subscribe(fTopic);
  writeln('MQTT Sub: ' + fTopic);
  cnt := 0;
  TimerTick := NewTimer(60 * 1000, @OnTimerTick, True);
  TimerPing := NewTimer(5000, @OnTimerPing, True);
  SyncCode.Leave;
end;

procedure TMQTTGate.OnPingResp(Sender: TObject);
begin
  SyncCode.Enter;
  //writeln('PingResp');
  SyncCode.Leave;
end;

procedure TMQTTGate.OnSubAck(Sender: TObject; MessageID: integer; GrantedQoS: integer);
begin
  SyncCode.Enter;
  writeln('SubAck');
  SyncCode.Leave;
end;

procedure TMQTTGate.OnUnSubAck(Sender: TObject);
begin
  SyncCode.Enter;
  writeln('UnSubAck');
  SyncCode.Leave;
end;

procedure TMQTTGate.OnMessage(Sender: TObject; topic, payload: TMqttString;
  isRetain: boolean);
begin
  SyncCode.Enter;
  fOnMessageSender := Sender;
  fOnMessageTopic := '' + topic;
  fOnMessagePayload := '' + payload;
  fOnMessageIsRetain := isRetain;
  SyncCode.Leave;
  writeln('Message' + ' topic=' + topic + ' payload=' + payload);
  CallOnMessages(Sender, topic, payload, isRetain);
end;

procedure TMQTTGate.OnTimerTick(Sender: TObject);
var
  jObject: TJSONObject;
begin
  SyncCode.Enter;
  cnt := cnt + 1;
  writeln('Tick. N=' + IntToStr(cnt));
  try
      jObject := TJSONObject(GetJSON('{}'));
      jObject.Integers['c'] := cnt;
      jObject.Integers['t'] := DateTimeToUnix(Now);
      jObject.Strings['n'] := Nick;
      MQTTClient.Publish(fTopic, jObject.AsJSON);
  finally
    jObject.free;
  end;
  SyncCode.Leave;
end;

procedure TMQTTGate.OnTimerPing(Sender: TObject);
begin
  SyncCode.Enter;
  MQTTClient.PingReq;
  SyncCode.Leave;
end;

procedure TMQTTGate.sendMessage(aTopic, aMsg: string);
begin
  MQTTClient.Publish(aTopic, aMsg);
end;

procedure TMQTTGate.DoRun;
var
  ErrorMsg: string;
begin
  SyncCode := TCriticalSection.Create();
  MQTTClient := TMQTTClient.Create(MQTT_Server, MQTT_Port);
  MQTTClient.OnConnAck := @OnConnAck;
  MQTTClient.OnPingResp := @OnPingResp;
  MQTTClient.OnPublish := @OnMessage;
  MQTTClient.OnSubAck := @OnSubAck;
  MQTTClient.QueueEnabled := False;
  MQTTClient.EventEnabled := True;
  MQTTClient.Connect();
end;

procedure TMQTTGate.DoTerminate;
begin
  try
    MQTTClient.Unsubscribe(fTopic);
    MQTTClient.Disconnect;
    Sleep(100);
    MQTTClient.ForceDisconnect;
  finally
    FreeAndNil(TimerTick);
    FreeAndNil(MQTTClient);
    FreeAndNil(SyncCode);
    Sleep(2000); // wait thread dies
  end;
end;

end.
