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
  MQTTReadThread // TPublishEvent
  ;

const
  MQTT_Server = 'iot.eclipse.org';
  MQTT_Port = 1883;

type

  Twriteln = function (const S: string): Integer of object;

  PPublishEvent = ^TPublishEvent;


  { TMQTTGate }

  TMQTTGate = class(TObject)
    fOnOnMessages: TList; // https://forum.lazarus.freepascal.org/index.php/topic,10370.msg51275.htm
  protected
    fWriteln: Twriteln;
    fTopic: String;
    MQTTClient: TMQTTClient;

    SyncCode:   TCriticalSection;
    TimerTick: TFPTimer;
    cnt:      integer;

    // Unsafe events! Called from MQTT thread (TMQTTReadThread)
    procedure OnConnAck(Sender: TObject; ReturnCode: integer);
    procedure OnPingResp(Sender: TObject);
    procedure OnSubAck(Sender: TObject; MessageID: integer; GrantedQoS: integer);
    procedure OnUnSubAck(Sender: TObject);
    procedure OnMessage(Sender: TObject; topic, payload: TMqttString; isRetain: boolean);

    procedure CallOnMessages(Sender: TObject; topic, payload: TMqttString;
      isRetain: boolean);

    procedure OnTimerTick(Sender: TObject);

  public
    property writeln: Twriteln read fwriteln write fwriteln;
    procedure AddOnMessage(Event: TPublishEvent);

    property Topic: String read fTopic write fTopic;
        procedure DoRun;
        procedure DoTerminate;

    procedure WriteHelp; virtual;
    procedure sendMessage(aTopic, aMsg: string);

    constructor Create;
    destructor Destroy;
  end;

{old: const
  { ^C }
  //ContrBreakSIG = ^C; // yes, its valid string! (OMG!)
  //ContrBreakSIG = #$03;
}

implementation

uses
    fpjson, jsonparser;

function NewTimer(Intr: integer; Proc: TNotifyEvent; AEnable: boolean = false): TFPTimer;
begin
  Result := TFPTimer.Create(nil);
  Result.UseTimerThread:=false;
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
  i: Integer;
  Event: TPublishEvent;
begin
  for i:=0 to Pred(fOnOnMessages.Count) do
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
  TimerTick := NewTimer(5000, @OnTimerTick, true);
  SyncCode.Leave;
end;

procedure TMQTTGate.OnPingResp(Sender: TObject);
begin
  SyncCode.Enter;
  writeln('PingResp');
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
  writeln('Message' + ' topic=' + topic + ' payload=' + payload);
  CallOnMessages(Sender, topic, payload, isRetain);
  SyncCode.Leave;
end;

procedure TMQTTGate.OnTimerTick(Sender: TObject);
var
  jData : TJSONData;
  jObject : TJSONObject;
begin
  SyncCode.Enter;
  cnt := cnt + 1;
  writeln('Tick. N='+IntToStr(cnt));
  MQTTClient.PingReq;
  jData := GetJSON('{}');
  jObject := TJSONObject(jData);
  jObject.Integers['c'] := cnt;
  MQTTClient.Publish(fTopic, jData.AsJSON);
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
  //StopOnException := True;
  SyncCode := TCriticalSection.Create();

  // quick check parameters
  (*
  ErrorMsg := CheckOptions('h', 'help');
  if ErrorMsg <> '' then
  begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;
  *)
  // begin main program
  MQTTClient := TMQTTClient.Create(MQTT_Server, MQTT_Port);
  MQTTClient.OnConnAck := @OnConnAck;
  MQTTClient.OnPingResp := @OnPingResp;
  MQTTClient.OnPublish := @OnMessage;
  MQTTClient.OnSubAck := @OnSubAck;
  MQTTClient.QueueEnabled := false;
  MQTTClient.EventEnabled := true;
  MQTTClient.Connect();
  (*
  //todo: wait 'OnConnAck'
  Sleep(1000);
  if not MQTTClient.isConnected then
  begin
    writeln('connect FAIL');
    exit;
  end;
    *)
  // mqtt subscribe to all topics
   //MQTTClient.Subscribe('#');

  (*
  try
    //while (not Terminated) and (MQTTClient.isConnected) do
    while (MQTTClient.isConnected) do
    begin
      // wait other thread
      CheckSynchronize(1000);

      //old: Check for ctrl-c
      {if KeyPressed then          //  <--- CRT function to test key press
        if ReadKey = ContrBreakSIG then      // read the key pressed
        begin
          writeln('Ctrl-C pressed.');
          Terminate;
        end;}
    end;

    MQTTClient.Unsubscribe(Topic);
    MQTTClient.Disconnect;
    Sleep(100);
    MQTTClient.ForceDisconnect;
  finally
    FreeAndNil(TimerTick);
    FreeAndNil(MQTTClient);
    FreeAndNil(SyncCode);
    Sleep(2000); // wait thread dies
  end;
  // stop program loop
  //Terminate;
  *)
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

procedure TMQTTGate.WriteHelp;
begin
  { add your help code here }
  //writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TMQTTGate;

function MyCtrlBreakHandler(CtrlBr: boolean): boolean;
begin
  writeln('CtrlBreak pressed. Terminating.');
  //Application.Terminate;
  Result := true;
end;

end.


