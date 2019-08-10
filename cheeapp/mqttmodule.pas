unit mqttmodule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, MQTT, FPTimer;

type

  { TMQTTGate }

  TMQTTGate = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
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

    fExitMessageLoop: Boolean;

    procedure SetupClient;
    procedure WriteDebug(aStr: string);

    // Unsafe events! Called from MQTT thread (TMQTTReadThread)
    procedure OnConnAck(Sender: TObject; ReturnCode: integer);
    procedure OnPingResp(Sender: TObject);
    procedure OnSubAck(Sender: TObject; MessageID: integer; GrantedQoS: integer);
    procedure OnUnSubAck(Sender: TObject; MessageID: integer);
    procedure OnMessage(Sender: TObject; topic, payload: TMqttString; isRetain: boolean);

    procedure WaitForConnection;
    procedure OnConnected;
    procedure CreateAndResetAliveTimer;
    procedure SubscribeToTopics;
    procedure OnTimerTick(Sender: TObject);
    procedure GetMessageLoop;
  public

    procedure Connect;
  end;

var
  MQTTGate: TMQTTGate;

implementation

{$R *.lfm}

uses
  DateUtils, ezutil, LazSysUtils;

{ TMQTTGate }

procedure TMQTTGate.DataModuleCreate(Sender: TObject);
begin
  // TODO remove hard coded testing var
  fServer := 'planeteer.mooo.com';
  fPort := 1883;
  fUsername := 'rugraat';
  fPassword := 'rugraat';
  fAliveTopic:= 'alive';
  fAliveCountDelay := 5000;

  fTopics := TStringList.Create;
  fTopics.add('#'); // TODO remove listen to all

  fSyncCode := TCriticalSection.Create();
  fMQTTClient := TMQTTClient.Create(fServer, fPort);
  SetupClient;

end;

procedure TMQTTGate.DataModuleDestroy(Sender: TObject);
begin
  FreeAndNil(TimerTick);
  FreeAndNil(MQTTClient);
  FreeAndNil(SyncCode);
end;

procedure TMQTTGate.SetupClient;
begin
  fMQTTClient.UserName := fUserName;
  fMQTTClient.Password := fPassword;
  fMQTTClient.OnConnAck := @OnConnAck;
  fMQTTClient.OnPingResp := @OnPingResp;
  fMQTTClient.OnPublish := @OnMessage;
  fMQTTClient.OnSubAck := @OnSubAck;
  fMQTTClient.OnUnSubAck:= @OnUnSubAck;
end;

procedure TMQTTGate.WriteDebug(aStr: string);
begin
  // TODO
end;

procedure TMQTTGate.WaitForConnection;
begin
  while not fMQTTClient.isConnected do
  begin
    WriteDebug('Waiting for connection...');
    //Sleep(1000);
    // wait other thread
    CheckSynchronize(1000);
  end;
end;

procedure TMQTTGate.OnConnAck(Sender: TObject; ReturnCode: integer);
begin
  fSyncCode.Enter;
  WriteDebug('ConnAck');
  fSyncCode.Leave;
end;

procedure TMQTTGate.OnPingResp(Sender: TObject);
begin
  fSyncCode.Enter;
  WriteDebug('PingResp');
  fSyncCode.Leave;
end;

procedure TMQTTGate.OnSubAck(Sender: TObject; MessageID: integer; GrantedQoS: integer);
begin
  fSyncCode.Enter;
  WriteDebug('SubAck');
  fSyncCode.Leave;
end;

procedure TMQTTGate.OnUnSubAck(Sender: TObject; MessageID: integer);
begin
  fSyncCode.Enter;
  WriteDebug('UnSubAck');
  fSyncCode.Leave;
end;

procedure TMQTTGate.OnMessage(Sender: TObject; topic, payload: TMqttString;
  isRetain: boolean);
begin
  fSyncCode.Enter;
  WriteDebug('topic:"' + topic + '", payload:"' + payload +'"');
  fSyncCode.Leave;
end;

procedure TMQTTGate.OnConnected;
begin
  CreateAndResetAliveTimer;
  SubscribeToTopics;
end;

procedure TMQTTGate.CreateAndResetAliveTimer;
begin
  fAliveCount := 0;
  if assigned(fTimerTick) then
    FreeAndNil(fTimerTick);
  fTimerTick := NewTimer(fAliveCountDelay, @OnTimerTick, true);
end;

procedure TMQTTGate.SubscribeToTopics;
var
  i: integer;
begin
  for i := 0 to pred(fTopics.Count) do
  begin
    WriteDebug('Subscribe: "' + fTopics[i] + '"');
    fMQTTClient.Subscribe(fTopics[i]);
  end;
end;

procedure TMQTTGate.OnTimerTick(Sender: TObject);
var
  vNow: TDateTime;
  vTimeStamp: integer;
begin
  fSyncCode.Enter;
  fAliveCount := fAliveCount + 1;
  WriteDebug('Tick. N='+IntToStr(fAliveCount));
  fMQTTClient.PingReq;
  vNow := NowUTC;
  vTimeStamp := DateTimeToUnix(vNow) * 1000 + MilliSecondOf(vNow);
  fMQTTClient.Publish(fAliveTopic, Format('[%d,%d]', [fAliveCount, vTimeStamp]) );
  fSyncCode.Leave;
end;

procedure TMQTTGate.GetMessageLoop;
var
  vMsg: TMQTTMessage;
  vAckMsg: TMQTTMessageAck;
begin
  fExitMessageLoop := False;
  while (not fExitMessageLoop) and (fMQTTClient.isConnected) do
  begin
   // wait other thread
   CheckSynchronize;//(1000);
   vAckMsg := fMQTTClient.getMessageAck;
   if assigned(vAckMsg) then begin
     WriteDebug('AckMsg ' + inttostr(vAckMsg.messageId) + ' ' + inttostr(vAckMsg.returnCode));
     FreeAndNil(vAckMsg);
   end;
   vMsg := fMQTTClient.getMessage;
   if assigned(vMsg) then begin
     WriteDebug('Msg ' + vMsg.Topic + ' ' + vMsg.PayLoad);
     FreeAndNil(vMsg);
   end;
  end;
end;

procedure TMQTTGate.Connect;
begin
  fMQTTClient.Connect();
  WaitForConnection;
end;


end.

