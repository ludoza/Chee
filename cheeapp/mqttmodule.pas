unit mqttmodule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, MQTTReadThread, MQTT, FPTimer, ezutil;

type

  { TMQTTThread }

  TMQTTThread = class(TThread)
    private
      fServer: string;
      fPort: Integer;
      fUsername: string;
      fPassword: string;

      fSyncCode: TCriticalSection;
      fMQTTClient: TMQTTClient;

      fTimerTick: TFPTimer;
      fAliveCount: Integer;
      fAliveCountDelay: Integer;
      fAliveTopic: String;

      fTopics: TStringList;

      fLastWriteDebugMessage: string;
      fOnWriteDebug: TWriteDebug;

      fAutoReconnect: Boolean;

      fLastMessage: TMQTTMessage;
      fOnMessage: TPublishEvent;
      fMessageLoopSleepTime: Integer;

      procedure SetupClient;
      procedure WriteDebugSync;
      procedure WriteDebug(aStr: string);
      procedure OnMessageSync;
      procedure OnMessageCall(aMsg: TMQTTMessage);

      // Unsafe events! Called from MQTT thread (TMQTTReadThread)
      procedure OnConnAck(Sender: TObject; ReturnCode: integer);
      procedure OnPingResp(Sender: TObject);
      procedure OnSubAck(Sender: TObject; MessageID: integer; GrantedQoS: integer);
      procedure OnUnSubAck(Sender: TObject; MessageID: integer);
      //procedure OnMessage(Sender: TObject; topic, payload: TMqttString; isRetain: boolean);

      procedure WaitForConnection;
      procedure AfterConnect;
      procedure BeforeDisconnect;
      procedure CreateAndResetAliveTimer;
      procedure SubscribeToTopics;
      procedure UnSubscribeToTopics;
      procedure OnTimerTick(Sender: TObject);
      procedure GetMessageLoop;

      procedure ConnectAndLoop;
      procedure Disconnect;
    protected
      procedure Execute; override;
    public
      Constructor Create(CreateSuspended : boolean);
      destructor Destroy; override;
      property OnWriteDebug: TWriteDebug read fOnWriteDebug write fOnWriteDebug;
      property AutoReconnect: Boolean read fAutoReconnect write fAutoReconnect;
      property OnMessage: TPublishEvent read fOnMessage write fOnMessage;
      procedure Publish(aTopic, aMsg: string);
    end;

  { TMQTTGate }

  TMQTTGate = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    fMQTTThread: TMQTTThread;
    fOnWriteDebug: TWriteDebug;
    function GetAutoReconnect: Boolean;
    function GetOnMessage: TPublishEvent;
    procedure SetAutoReconnect(AValue: Boolean);
    procedure SetOnMessage(AValue: TPublishEvent);
  public
    property AutoReconnect: Boolean read GetAutoReconnect write SetAutoReconnect;
    property OnWriteDebug: TWriteDebug read fOnWriteDebug write fOnWriteDebug;
    property OnMessage: TPublishEvent read GetOnMessage write SetOnMessage;
    procedure Start;
    procedure Stop;
    procedure SendMessage(aTopic, aMsg: string);
  end;

var
  MQTTGate: TMQTTGate;

implementation

{$R *.lfm}

uses
  DateUtils, LazSysUtils;

{ TMQTTGate }

procedure TMQTTGate.DataModuleCreate(Sender: TObject);
begin
  fMQTTThread := TMQTTThread.Create(True);
end;

procedure TMQTTGate.DataModuleDestroy(Sender: TObject);
begin
  FreeAndNil(fMQTTThread);
end;

function TMQTTGate.GetAutoReconnect: Boolean;
begin
  result := fMQTTThread.AutoReconnect;
end;

function TMQTTGate.GetOnMessage: TPublishEvent;
begin
  result := fMQTTThread.OnMessage;
end;


procedure TMQTTGate.SetAutoReconnect(AValue: Boolean);
begin
  fMQTTThread.AutoReconnect := AValue;
end;

procedure TMQTTGate.SetOnMessage(AValue: TPublishEvent);
begin
  fMQTTThread.OnMessage:= AValue;
end;


procedure TMQTTGate.Start;
begin
  if Assigned(fOnWriteDebug) then
    fMQTTThread.OnWriteDebug:= fOnWriteDebug;
  fMQTTThread.Start;
end;

procedure TMQTTGate.Stop;
begin
  fMQTTThread.Terminate;

end;

procedure TMQTTGate.SendMessage(aTopic, aMsg: string);
begin
  fMQTTThread.Publish(aTopic, aMsg);
end;

procedure TMQTTThread.SetupClient;
begin
  fMQTTClient.UserName := fUserName;
  fMQTTClient.Password := fPassword;
  fMQTTClient.OnConnAck := @OnConnAck;
  fMQTTClient.OnPingResp := @OnPingResp;
  //fMQTTClient.OnPublish := @OnMessage; // We Rather use the Queue
  fMQTTClient.OnSubAck := @OnSubAck;
  fMQTTClient.OnUnSubAck:= @OnUnSubAck;
end;

procedure TMQTTThread.WriteDebugSync;
begin
  if Assigned(fOnWriteDebug) then
  begin
    fOnWriteDebug(fLastWriteDebugMessage);
  end;
end;

procedure TMQTTThread.WriteDebug(aStr: string);
begin
  fLastWriteDebugMessage:= aStr;
  Synchronize(@WriteDebugSync);
end;

procedure TMQTTThread.OnMessageSync;
begin
  if Assigned(fOnMessage) then
  try
    fOnMessage(fMQTTClient, fLastMessage.topic, fLastMessage.payload, fLastMessage.Retain);
  finally
    FreeAndNil(fLastMessage)
  end;
end;

procedure TMQTTThread.OnMessageCall(aMsg: TMQTTMessage);
begin
  fLastMessage:= aMsg;
  Synchronize(@OnMessageSync);
end;

procedure TMQTTThread.WaitForConnection;
begin
  while not fMQTTClient.isConnected do
  begin
    WriteDebug('Waiting for connection...');
    Sleep(100);
  end;
end;

procedure TMQTTThread.OnConnAck(Sender: TObject; ReturnCode: integer);
begin
  fSyncCode.Enter;
  WriteDebug('ConnAck');
  fSyncCode.Leave;
end;

procedure TMQTTThread.OnPingResp(Sender: TObject);
begin
  fSyncCode.Enter;
  WriteDebug('PingResp');
  fSyncCode.Leave;
end;

procedure TMQTTThread.OnSubAck(Sender: TObject; MessageID: integer; GrantedQoS: integer);
begin
  fSyncCode.Enter;
  WriteDebug('SubAck');
  fSyncCode.Leave;
end;

procedure TMQTTThread.OnUnSubAck(Sender: TObject; MessageID: integer);
begin
  fSyncCode.Enter;
  WriteDebug('UnSubAck');
  fSyncCode.Leave;
end;

{
procedure TMQTTThread.OnMessage(Sender: TObject; topic, payload: TMqttString;
  isRetain: boolean);
begin
  fSyncCode.Enter;
  WriteDebug('topic:"' + topic + '", payload:"' + payload +'"');
  fSyncCode.Leave;
end;
}

procedure TMQTTThread.AfterConnect;
begin
  CreateAndResetAliveTimer;
  SubscribeToTopics;
end;

procedure TMQTTThread.BeforeDisconnect;
begin
  FreeAndNil(fTimerTick);
end;

procedure TMQTTThread.CreateAndResetAliveTimer;
begin
  fAliveCount := 0;
  if assigned(fTimerTick) then
    FreeAndNil(fTimerTick);
  fTimerTick := NewTimer(fAliveCountDelay, @OnTimerTick, true);
end;

procedure TMQTTThread.SubscribeToTopics;
var
  i: integer;
begin
  for i := 0 to pred(fTopics.Count) do
  begin
    WriteDebug('Subscribe: "' + fTopics[i] + '"');
    fMQTTClient.Subscribe(fTopics[i]);
  end;
end;

procedure TMQTTThread.UnSubscribeToTopics;
var
  i: integer;
begin
  for i := 0 to pred(fTopics.Count) do
  begin
    WriteDebug('Unsubscribe: "' + fTopics[i] + '"');
    if assigned(fMQTTClient) and fMQTTClient.isConnected then
      fMQTTClient.Unsubscribe(fTopics[i])
    else
      WriteDebug('Disconnected CANNOT Unsubscribe: "' + fTopics[i] + '"');
  end;
end;

procedure TMQTTThread.OnTimerTick(Sender: TObject);
var
  vNow: TDateTime;
  vTimeStamp: Int64;
begin
  fSyncCode.Enter;
  fAliveCount := fAliveCount + 1;
  WriteDebug('Tick. N='+IntToStr(fAliveCount));
  fMQTTClient.PingReq;
  vNow := NowUTC;
  vTimeStamp := (DateTimeToUnix(vNow) * 1000) + MilliSecondOf(vNow);
  fMQTTClient.Publish(fAliveTopic, Format('[%d,%d]', [fAliveCount, vTimeStamp]) );
  fSyncCode.Leave;
end;

procedure TMQTTThread.GetMessageLoop;
var
  vMsg: TMQTTMessage;
  vAckMsg: TMQTTMessageAck;
begin
  while (not Terminated) and (fMQTTClient.isConnected) do
  begin
   vAckMsg := fMQTTClient.getMessageAck;
   if assigned(vAckMsg) then begin
     WriteDebug('AckMsg ' + inttostr(vAckMsg.messageId) + ' ' + inttostr(vAckMsg.returnCode));
     FreeAndNil(vAckMsg);
   end;
   vMsg := fMQTTClient.getMessage;
   if assigned(vMsg) then begin
     WriteDebug('Msg ' + vMsg.Topic + ' ' + vMsg.PayLoad);
     if assigned(fOnMessage) then
     begin
       OnMessageCall(vMsg);
     end else  // free our msg because OnMessge is not going to use it
       FreeAndNil(vMsg);
   end;
   if fMessageLoopSleepTime <> -1 then // TODO http://www.paradicesoftware.com/blog/2014/02/dont-use-suspend-and-resume-but-dont-poll-either/
     Sleep(fMessageLoopSleepTime); // give the cpu some breathing space;
  end;
end;

procedure TMQTTThread.ConnectAndLoop;
begin
  fMQTTClient.Connect;
  WaitForConnection;
  AfterConnect;
  GetMessageLoop;
end;

procedure TMQTTThread.Disconnect;
begin
  BeforeDisconnect;
  UnSubscribeToTopics;
  fMQTTClient.Disconnect;
  Sleep(100); // wait for disconnect msg to be sent
  fMQTTClient.ForceDisconnect;
end;

procedure TMQTTThread.Execute;
begin
  ConnectAndLoop;
  while (not Terminated) and fAutoReconnect do
  begin
    Disconnect;
    ConnectAndLoop;

  end;
end;

constructor TMQTTThread.Create(CreateSuspended: boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := False;

  // TODO remove hard coded testing var
  fServer := 'planeteer.mooo.com';
  fPort := 1883;
  fUsername := 'rugraat';
  fPassword := 'rugraat';
  fAliveTopic:= 'alive';
  fAliveCountDelay := 5000;
  fAutoReconnect := True;
  fMessageLoopSleepTime:= 3; // milliseconds to sleep in the message handling loop.
  //fMessageLoopSleepTime:= -1; // disable sleeping in message handling loop.
  fTopics := TStringList.Create;
  fTopics.add('#'); // TODO remove listen to all

  fSyncCode := TCriticalSection.Create();
  fMQTTClient := TMQTTClient.Create(fServer, fPort);
  SetupClient;
end;

destructor TMQTTThread.Destroy;
begin
  Disconnect;
  FreeAndNil(fTimerTick);
  FreeAndNil(fMQTTClient);
  FreeAndNil(fSyncCode);


  inherited Destroy;
end;

procedure TMQTTThread.Publish(aTopic, aMsg: string);
begin
  fMQTTClient.Publish(aTopic, aMsg);
end;


end.

