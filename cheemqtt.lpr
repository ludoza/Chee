program cheemqtt;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
    {$IFDEF UseCThreads}
  CThreads,
    {$ENDIF}
  {$ENDIF}
  Classes, SysUtils, CustApp, SyncObjs, FPTimer, DateUtils,
  MQTT;



type
  { TMQTTGate }

  TMQTTGate = class(TCustomApplication)
  protected
    MQTTClient: TMQTTClient;

    SyncCode: TCriticalSection;
    TimerTick: TFPTimer;
    AliveCount: Integer;
    AliveCountDelay: Integer;
    AliveTopic: String;

    // Unsafe events! Called from MQTT thread (TMQTTReadThread)
    procedure OnConnAck(Sender: TObject; ReturnCode: integer);
    procedure OnPingResp(Sender: TObject);
    procedure OnSubAck(Sender: TObject; MessageID: integer; GrantedQoS: integer);
    procedure OnUnSubAck(Sender: TObject);
    procedure OnMessage(Sender: TObject; topic, payload: TMqttString; isRetain: boolean);
    procedure SetupClient;
    procedure SetupArgs;
    procedure WaitForConnection;

    procedure MainLoop;
    procedure OnTimerTick(Sender: TObject);
    procedure DoRun; override;
    procedure WriteDebug(aStr: string);
  public
    Topics: TStringList;
    Server: String;
    Port: Integer;
    procedure WriteHelp; virtual;
  end;

function NewTimer(Intr: integer; Proc: TNotifyEvent; AEnable: boolean = false): TFPTimer;
begin
  Result := TFPTimer.Create(nil);
  Result.UseTimerThread:=false;
  Result.Interval := Intr;
  Result.OnTimer := Proc;
  Result.Enabled := AEnable;
end;

{ TMQTTGate }

procedure TMQTTGate.OnConnAck(Sender: TObject; ReturnCode: integer);
begin
  SyncCode.Enter;
  WriteDebug('ConnAck');
  SyncCode.Leave;
end;

procedure TMQTTGate.OnPingResp(Sender: TObject);
begin
  SyncCode.Enter;
  WriteDebug('PingResp');
  SyncCode.Leave;
end;

procedure TMQTTGate.OnSubAck(Sender: TObject; MessageID: integer; GrantedQoS: integer);
begin
  SyncCode.Enter;
  WriteDebug('SubAck');
  SyncCode.Leave;
end;

procedure TMQTTGate.OnUnSubAck(Sender: TObject);
begin
  SyncCode.Enter;
  WriteDebug('UnSubAck');
  SyncCode.Leave;
end;

procedure TMQTTGate.OnMessage(Sender: TObject; topic, payload: TMqttString;
  isRetain: boolean);
begin
  SyncCode.Enter;
  WriteDebug('topic:"' + topic + '", payload:"' + payload +'"');
  SyncCode.Leave;
end;

procedure TMQTTGate.SetupClient;
begin
 MQTTClient.OnConnAck := @OnConnAck;
 MQTTClient.OnPingResp := @OnPingResp;
 MQTTClient.OnPublish := @OnMessage;
 MQTTClient.OnSubAck := @OnSubAck;
 //MQTTClient.QueueEnabled := false;
 //MQTTClient.EventEnabled := true;
end;

procedure TMQTTGate.SetupArgs;
begin
 // setup options from parameters
 AliveCountDelay := 5000;
 AliveTopic:= 'alive';
 if HasOption('s', 'server') then
   Server := GetOptionValue('s', 'server')
 else
   Server := 'iot.eclipse.org';
 if HasOption('p', 'port') then
   Port := StrToInt(GetOptionValue('p', 'port'))
 else
   Port := 1883;

 Topics := TStringList.Create;
 if HasOption('t', 'topic') then
 begin
   Topics.Delimiter:= ',';
   Topics.DelimitedText:= GetOptionValue('t', 'topic');
 end else
   Topics.Add('#');
end;

procedure TMQTTGate.WaitForConnection;
begin
  sleep(1000);
  while not MQTTClient.isConnected do
  begin
    WriteDebug('Waiting for connection...');
    Sleep(1000);
  end;
end;

procedure TMQTTGate.MainLoop;
var
  i: Integer;
  msg: TMQTTMessage;
  ackmsg: TMQTTMessageAck;
begin
  for i := 0 to pred(Topics.Count) do
 begin
   WriteDebug('Subscribe: "' + Topics[i] + '"');
   MQTTClient.Subscribe(Topics[i]);
 end;
 while (*(not Terminated) and *) (MQTTClient.isConnected) do
 begin
   // wait other thread
   CheckSynchronize;//(1000);
   ackmsg := MQTTClient.getMessageAck;
   if assigned(ackmsg) then WriteDebug('AckMsg ' + inttostr(ackmsg.messageId) + ' ' + inttostr(ackmsg.returnCode));
   msg := MQTTClient.getMessage;
   if assigned(msg) then WriteDebug( msg.Topic + ' ' + inttostr(length(msg.PayLoad)));
 end;
 WriteDebug('AliveCount:' + IntToStr(AliveCount));
 for i := 0 to pred(Topics.Count) do
 begin
   WriteDebug('Unsubscribe: "' + Topics[i] + '"');
   MQTTClient.Unsubscribe(Topics[i]);
 end;
 MQTTClient.Disconnect;
 Sleep(100);
 MQTTClient.ForceDisconnect;
end;

procedure TMQTTGate.OnTimerTick(Sender: TObject);
begin
  SyncCode.Enter;
  AliveCount := AliveCount + 1;
  WriteDebug('Tick. N='+IntToStr(AliveCount));
  MQTTClient.PingReq;
  MQTTClient.Publish(AliveTopic, '[' + IntToStr(AliveCount) + ',' + IntToStr(DateTimeToUnix(Now)) + ']');
  SyncCode.Leave;
end;

procedure TMQTTGate.DoRun;
var
  ErrorMsg: String;
begin
  //StopOnException := True;
  SyncCode := TCriticalSection.Create();

  // parse parameters
  if HasOption('h', 'help') then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  SetupArgs;

  // begin main program
  MQTTClient := TMQTTClient.Create(Server, Port);
  SetupClient;
  MQTTClient.Connect();

  // TODO OnConnAck
  WaitForConnection;

  AliveCount := 0;
  TimerTick := NewTimer(AliveCountDelay, @OnTimerTick, true);
  try
    while true do
    begin
      MainLoop;
      MQTTClient.Connect();
      WaitForConnection;
    end;
  finally
    FreeAndNil(TimerTick);
    FreeAndNil(MQTTClient);
    FreeAndNil(SyncCode);
    Sleep(2000); // wait thread dies
  end;
  // stop program loop
  WriteDebug('Done... <enter>');
  Readln;
  Terminate;
end;

procedure TMQTTGate.WriteDebug(aStr: string);
begin
  writeln(aStr);
end;

procedure TMQTTGate.WriteHelp;
begin
  WriteDebug('Usage: ' + ExeName + ' -h');
  WriteDebug('');
  WriteDebug('-s --server=<hostname or ip> default: iot.eclipse.org');
  WriteDebug('-p --port=<port> default: 1883');
  WriteDebug('-t --topic=<topic1,t2,t3,...> default: #');
end;

var
  Application: TMQTTGate;

function MyCtrlBreakHandler(CtrlBr: boolean): boolean;
begin
  //WriteDebug('CtrlBreak pressed. Terminating.');
  Application.Terminate;
  Result := true;
end;

begin
  SysSetCtrlBreakHandler(@MyCtrlBreakHandler);
  Application := TMQTTGate.Create(nil);
  Application.Run;
  Application.Free;

end.

