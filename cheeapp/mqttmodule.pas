unit mqttmodule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, MQTT, FPTimer, ezutil;

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

      procedure SetupClient;
      procedure WriteDebugSync;
      procedure WriteDebug(aStr: string);

      // Unsafe events! Called from MQTT thread (TMQTTReadThread)
      procedure OnConnAck(Sender: TObject; ReturnCode: integer);
      procedure OnPingResp(Sender: TObject);
      procedure OnSubAck(Sender: TObject; MessageID: integer; GrantedQoS: integer);
      procedure OnUnSubAck(Sender: TObject; MessageID: integer);
      procedure OnMessage(Sender: TObject; topic, payload: TMqttString; isRetain: boolean);

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
    end;

  { TMQTTGate }

  TMQTTGate = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    fMQTTThread: TMQTTThread;
    fOnWriteDebug: TWriteDebug;
    function GetAutoReconnect: Boolean;
    procedure SetAutoReconnect(AValue: Boolean);
  public
    property AutoReconnect: Boolean read GetAutoReconnect write SetAutoReconnect;
    property OnWriteDebug: TWriteDebug read fOnWriteDebug write fOnWriteDebug;
    procedure Start;
    procedure Stop;
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


procedure TMQTTGate.SetAutoReconnect(AValue: Boolean);
begin
  fMQTTThread.AutoReconnect := AValue;
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

procedure TMQTTThread.SetupClient;
begin
  fMQTTClient.UserName := fUserName;
  fMQTTClient.Password := fPassword;
  fMQTTClient.OnConnAck := @OnConnAck;
  fMQTTClient.OnPingResp := @OnPingResp;
  fMQTTClient.OnPublish := @OnMessage;
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

procedure TMQTTThread.OnMessage(Sender: TObject; topic, payload: TMqttString;
  isRetain: boolean);
begin
  fSyncCode.Enter;
  WriteDebug('topic:"' + topic + '", payload:"' + payload +'"');
  fSyncCode.Leave;
end;

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
     FreeAndNil(vMsg);
   end;
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


end.

