{
 -------------------------------------------------
  MQTT.pas -  A Library for Publishing and Subscribing to messages from an MQTT Message
  broker such as the RSMB (http://alphaworks.ibm.com/tech/rsmb).

  MQTT - http://mqtt.org/
  Spec - http://publib.boulder.ibm.com/infocenter/wmbhelp/v6r0m0/topic/com.ibm.etools.mft.doc/ac10840_.htm

  MIT License -  http://www.opensource.org/licenses/mit-license.php
  Copyright (c) 2009 Jamie Ingilby

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
  THE SOFTWARE.
   -------------------------------------------------
}

{$mode objfpc}

unit MQTT;

interface

uses
  SysUtils, blcksock, contnrs, MQTTReadThread, dateutils;

type
  //  Message type. 4 Bit unsigned.
  TMQTTMessageType = (
    Reserved0 = 0,    // Reserved
    CONNECT = 1,      // Client request to connect to Broker
    CONNACK = 2,      // Connect Acknowledgment
    // PUBLISH Control Packet is sent from a Client to a Server or from Server to a Client to transport an Application Message.
    PUBLISH = 3,      // Publish message
    PUBACK = 4,       // Publish Acknowledgment
    PUBREC = 5,       // Publish Received (assured delivery part 1)
    PUBREL = 6,       // Publish Release (assured delivery part 2)
    PUBCOMP = 7,      // Publish Complete (assured delivery part 3)
    SUBSCRIBE = 8,    // Client Subscribe request
    SUBACK = 9,       // Subscribe Acknowledgment
    UNSUBSCRIBE = 10, // Client Unsubscribe request
    UNSUBACK = 11,    // Unsubscribe Acknowledgment
    PINGREQ = 12,     // PING Request
    PINGRESP = 13,    // PING Response
    DISCONNECT = 14,  // Client is Disconnecting
    Reserved15 = 15   // Reserved
    );

  TMqttString = MQTTReadThread.TMqttString;

  // The message class definition

  { TMQTTMessage }

  TMQTTMessage = class
  private
    FTopic: TMqttString;
    FPayload: TMqttString;
    FRetain: boolean;

  public
    (*todo: Python:
    topic : String. topic that the message was published on.
    payload : String/bytes the message payload.
    qos : Integer. The message Quality of Service 0, 1 or 2.
    retain : Boolean. If true, the message is a retained message and not fresh.
    mid : Integer. The message id.
    *)
    property Topic: TMqttString read FTopic;
    property PayLoad: TMqttString read FPayload;
    property Retain: boolean read FRetain;

    constructor Create(const topic_: TMqttString; const payload_: TMqttString;
      const retain_: boolean);
  end;

  // The acknowledgement class definition

  { TMQTTMessageAck }

  TMQTTMessageAck = class
  private
    FMessageType: TMQTTMessageType;
    FMessageId: integer;
    FReturnCode: integer;
    FQos: integer;
  public
    property messageType: TMQTTMessageType read FMessageType;
    property messageId: integer read FMessageId;
    property returnCode: integer read FReturnCode;
    property qos: integer read FQos;

    constructor Create(const messageType_: TMQTTMessageType;
      const messageId_: integer; const returnCode_: integer; const qos_: integer);
  end;


  TRemainingLength = array of byte;

  //todo: remove this!
  TUTF8Text = array of byte;

  // Main object - MQTT client implementation

  { TMQTTClient }

  TMQTTClient = class(TObject)
  protected
    FClientID: TMqttString;
    FUserName: TMqttString;
    FPassword: TMqttString;
    FHostname: UTF8String;
    FPort: integer;

    // main background thread - using for reading
    FReadThread: TMQTTReadThread;

    FMessageID: integer;
    FisConnected: boolean;

    FConnAckEvent: TConnAckEvent;
    FPublishEvent: TPublishEvent;
    FPingRespEvent: TPingRespEvent;
    FSubAckEvent: TSubAckEvent;
    FUnSubAckEvent: TUnSubAckEvent;

    //todo:! rename: FCritical->FCritQueuePtr
    FCritical: TRTLCriticalSection;
    FCritThreadPtr: TRTLCriticalSection;

    FMessageAckQueue: TQueue;

    // Gets a next Message ID and increases the Message ID Increment
    function GetMessageID: TBytes;
    function VariableHeaderPublish(topic: TMqttString): TBytes;
    function VariableHeaderSubscribe: TBytes;
    function VariableHeaderUnsubscribe: TBytes;
    // Internally Write the provided data to the Socket. Wrapper function.
    function SocketWrite(Data: TBytes): boolean;

    // These are chained event handlers from the ReceiveThread. They trigger the
    // public TMQTTClient.On*** handlers.
    procedure OnRTConnAck(Sender: TObject; ReturnCode: integer);
    procedure OnRTPingResp(Sender: TObject);
    procedure OnRTSubAck(Sender: TObject; MessageID: integer; GrantedQoS: integer);
    procedure OnRTUnSubAck(Sender: TObject; MessageID: integer);
    procedure OnRTPublish(Sender: TObject; topic, payload: TMqttString;
      retain: boolean);
    procedure OnRTTerminate(Sender: TObject);

    // virtual creator for Thread class.
    // 'Virtual Constructor' (aka 'Factory Method') - you can overload the TMQTTReadThread class.
    function CreateMQTTThread: TMQTTReadThread; virtual;
    procedure TerminateThread(waitThreadEnd: boolean = False);

  public
    //todo:! move back!
    FMessageQueue: TQueue;

    EventEnabled: boolean;
    QueueEnabled: boolean;

    function isConnected: boolean;
    //todo: ? Python: host, port=1883, keepalive=60, bind_address=""
    procedure Connect;
    function Disconnect: boolean;
    procedure ForceDisconnect;
    function Publish(Topic, sPayload: TMqttString; Retain: boolean = False): boolean;
    function Subscribe(Topic: TMqttString): integer;
    function Unsubscribe(Topic: TMqttString): integer;
    function PingReq: boolean;
    function getMessage: TMQTTMessage;
    function getMessageAck: TMQTTMessageAck;

    //todo: ? Python: client_id="", clean_session=True, userdata=None
    constructor Create(Hostname: UTF8String; Port: integer); overload;
    destructor Destroy; override;

    property ClientID: TMqttString read FClientID write FClientID;
    property UserName: TMqttString read FUserName write FUserName;
    property Password: TMqttString read FPassword write FPassword;

    //
    property OnConnAck: TConnAckEvent read FConnAckEvent write FConnAckEvent;

    property OnPublish: TPublishEvent read FPublishEvent write FPublishEvent;
    property OnPingResp: TPingRespEvent read FPingRespEvent write FPingRespEvent;
    property OnSubAck: TSubAckEvent read FSubAckEvent write FSubAckEvent;
    property OnUnSubAck: TUnSubAckEvent read FUnSubAckEvent write FUnSubAckEvent;
  end;

// Message Component Build helpers
function FixedHeader(MessageType: TMQTTMessageType; Dup, Qos, Retain: byte): byte;

// Variable Header per command creation funcs
function VariableHeaderConnect(KeepAlive: word; Username: word = 0; Password: word = 0): TBytes;

// Takes a UTF8String and converts to An Array of Bytes preceded by 2 Length Bytes.
function StrToBytes(str: UTF8String; perpendLength: boolean): TUTF8Text;

procedure CopyIntoArray(var DestArray: array of byte; SourceArray: array of byte;
  StartIndex: integer);

// Byte Array Helper Functions
procedure AppendArray(var Dest: TUTF8Text; Source: array of byte);

// Helper Function - Puts the seperate component together into an Array of Bytes for transmission
function BuildCommand(FixedHead: byte; RemainL: TRemainingLength;
  VariableHead: TBytes; Payload: array of byte): TBytes;

// Calculates the Remaining Length bytes of the FixedHeader as per the spec.
function RemainingLength(MessageLength: integer): TRemainingLength;


implementation

{$IFDEF DEBUG_MQTT_LCLLOG}
uses LCLProc; // DbgOutThreadLog

procedure WRITE_DEBUG(str: String);
begin
  DbgOutThreadLog(TimeToStr(Now()) + '[' + IntToStr(GetTickCount64()) + ']' + str + LineEnding);
end;
{$ELSE}
{ ok, so this is a hack, but it works nicely. Just never use
  a multiline argument with WRITE_DEBUG! }
{$IFDEF DEBUG_MQTT}
procedure WRITE_DEBUG(str: String);
begin
  Writeln(TimeToStr(Now()) + '[' + IntToStr(GetTickCount64()) + ']' + str + LineEnding);
end;
{$ELSE}
{$MACRO ON}
{$define WRITE_DEBUG := //}// just comment out those lines
{$ENDIF}
{$ENDIF DEBUG_MQTT_LCLLOG}

constructor TMQTTMessage.Create(const topic_: TMqttString;
  const payload_: TMqttString; const retain_: boolean);
begin
  // Save the passed parameters
  FTopic := Topic_;
  FPayload := Payload_;
  FRetain := retain_;
end;

constructor TMQTTMessageAck.Create(const messageType_: TMQTTMessageType;
  const messageId_: integer; const returnCode_: integer; const qos_: integer);
begin
  FMessageType := messageType_;
  FMessageId := messageId_;
  FReturnCode := returnCode_;
  FQos := qos_;
end;

{*------------------------------------------------------------------------------
  Instructs the Client to try to connect to the server at TMQTTClient.Hostname and
  TMQTTClient.Port and then to send the initial CONNECT message as required by the
  protocol. Check for a CONACK message to verify successful connection.
------------------------------------------------------------------------------*}
procedure TMQTTClient.Connect;
var
  newThread: TMQTTReadThread;
begin
  if not isConnected then
  begin
    // Create RX thread
    newThread := CreateMQTTThread();
    newThread.OnConnAck := @OnRTConnAck;
    newThread.OnPublish := @OnRTPublish;
    newThread.OnPublish := @OnRTPublish;
    newThread.OnPingResp := @OnRTPingResp;
    newThread.OnSubAck := @OnRTSubAck;
    newThread.OnUnSubAck := @OnRTUnSubAck;
    newThread.OnTerminate := @OnRTTerminate;
    newThread.FreeOnTerminate := True;

    TerminateThread();

    try
      EnterCriticalsection(FCritThreadPtr);
      FReadThread := newThread;
      // start thread
      FReadThread.Start;
    finally
      LeaveCriticalsection(FCritThreadPtr);
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Sends the DISCONNECT packets and then Disconnects gracefully from the server
  which it is currently connected to.
  @return Returns whether the Data was written successfully to the socket.
------------------------------------------------------------------------------*}
function TMQTTClient.Disconnect: boolean;
var
  Data: TBytes;
begin
  WRITE_DEBUG('TMQTTClient.Disconnect begin');
  Result := False;

  SetLength(Data, 2);
  Data[0] := FixedHeader(MQTT.DISCONNECT, 0, 0, 0);
  Data[1] := 0;
  WRITE_DEBUG('TMQTTClient.Disconnect SocketWrite');
  if SocketWrite(Data) then
  begin
    TerminateThread();
    Result := True;
  end
  else
    Result := False;
  WRITE_DEBUG('TMQTTClient.Disconnect end');
end;

{*------------------------------------------------------------------------------
  Terminate the reader thread and close the socket forcibly.
------------------------------------------------------------------------------*}
procedure TMQTTClient.ForceDisconnect;
begin
  WRITE_DEBUG('TMQTTClient.ForceDisconnect');
  TerminateThread();
end;

{*------------------------------------------------------------------------------
  Call back for reader thread termination.
------------------------------------------------------------------------------*}
procedure TMQTTClient.OnRTTerminate(Sender: TObject);
begin
  FisConnected := False;
  EnterCriticalsection(FCritThreadPtr);
  FReadThread := nil;
  LeaveCriticalsection(FCritThreadPtr);
  WRITE_DEBUG('TMQTTClient.OnRTTerminate: Thread.Terminated.');
end;

function TMQTTClient.CreateMQTTThread: TMQTTReadThread;
begin
  Result := TMQTTReadThread.Create(FHostname, FPort);
  Result.ClientID:= FClientID;
  Result.UserName:= FUserName;
  Result.Password:= FPassword;
end;

procedure TMQTTClient.TerminateThread(waitThreadEnd: boolean = False);
var
  p: TMQTTReadThread;
begin
  WRITE_DEBUG('TMQTTClient.TerminateThread begin ');
  if not waitThreadEnd then
  begin
    // fast terminate
    try
      EnterCriticalsection(FCritThreadPtr);
      if FReadThread <> nil then
      begin
        FisConnected := False;
        FReadThread.OnTerminate := nil;
        FReadThread.Terminate;
        // call 'free' is not needed - the 'FreeOnTerminate' mode is enabled
        // just drom terminating thread
        FReadThread := nil;
      end
    finally
      LeaveCriticalsection(FCritThreadPtr);
    end;
  end
  else
  begin
    // slow terminate
    try
      p := nil;
      EnterCriticalsection(FCritThreadPtr);
      if FReadThread <> nil then
      begin
        WRITE_DEBUG('TMQTTClient.TerminateThread FReadThread.OnTerminate := nil ');
        FReadThread.OnTerminate := nil;
        FReadThread.FreeOnTerminate := False;
        // capture pointer to thread
        p := FReadThread;
        FReadThread := nil;
      end;
    finally
      LeaveCriticalsection(FCritThreadPtr);
    end;
    if p <> nil then
    begin
      WRITE_DEBUG('TMQTTClient.TerminateThread p.Terminate');
      p.Terminate;
      p.WaitFor;
      // manual free (because the 'FreeOnTerminate' is disabled)
      FreeAndNil(p);
    end;
  end;
  WRITE_DEBUG('TMQTTClient.TerminateThread end ');
end;

{*------------------------------------------------------------------------------
  Sends a PINGREQ to the server informing it that the client is alice and that it
  should send a PINGRESP back in return.
  @return Returns whether the Data was written successfully to the socket.
------------------------------------------------------------------------------*}
function TMQTTClient.PingReq: boolean;
var
  FH: byte;
  RL: byte;
  Data: TBytes;
begin
  Result := False;

  SetLength(Data, 2);
  FH := FixedHeader(MQTT.PINGREQ, 0, 0, 0);
  RL := 0;
  Data[0] := FH;
  Data[1] := RL;
  if SocketWrite(Data) then
    Result := True
  else
    Result := False;
end;

{*------------------------------------------------------------------------------
  Publishes a message sPayload to the Topic on the remote broker with the retain flag
  defined as given in the 3rd parameter.
  @param Topic   The Topic Name of your message eg /station1/temperature/
  @param sPayload   The Actual Payload of the message eg 18 degrees celcius
  @param Retain   Should this message be retained for clients connecting subsequently
  @return Returns whether the Data was written successfully to the socket.
------------------------------------------------------------------------------*}
function TMQTTClient.Publish(Topic, sPayload: TMqttString; Retain: boolean
  ): boolean;
var
  Data: TBytes;
  FH: byte;
  RL: TRemainingLength;
  VH: TBytes;
  Payload: TUTF8Text;
begin
  Result := False;

  FH := FixedHeader(MQTT.PUBLISH, 0, 0, Ord(Retain));
  VH := VariableHeaderPublish(Topic);
  SetLength(Payload, 0);
  AppendArray(Payload, StrToBytes(sPayload, False));
  RL := RemainingLength(Length(VH) + Length(Payload));
  Data := BuildCommand(FH, RL, VH, Payload);
  if SocketWrite(Data) then
    Result := True
  else
    Result := False;
end;

{*------------------------------------------------------------------------------
  Subscribe to Messages published to the topic specified. Only accepts 1 topic per
  call at this point.
  @param Topic   The Topic that you wish to Subscribe to.
  @return Returns the Message ID used to send the message for the purpose of comparing
  it to the Message ID used later in the SUBACK event handler.
------------------------------------------------------------------------------*}
function TMQTTClient.Subscribe(Topic: TMqttString): integer;
var
  Data: TBytes;
  FH: byte;
  RL: TRemainingLength;
  VH: TBytes;
  Payload: TUTF8Text;
begin
  FH := FixedHeader(MQTT.SUBSCRIBE, 0, 1, 0);
  VH := VariableHeaderSubscribe;
  Result := (FMessageID - 1);
  SetLength(Payload, 0);
  AppendArray(Payload, StrToBytes(Topic, True));
  // Append a new Byte to Add the Requested QoS Level for that Topic
  SetLength(Payload, Length(Payload) + 1);
  // Always Append Requested QoS Level 0
  Payload[Length(Payload) - 1] := $0;
  RL := RemainingLength(Length(VH) + Length(Payload));
  Data := BuildCommand(FH, RL, VH, Payload);
  SocketWrite(Data);
end;

{*------------------------------------------------------------------------------
  Unsubscribe to Messages published to the topic specified. Only accepts 1 topic per
  call at this point.
  @param Topic   The Topic that you wish to Unsubscribe to.
  @return Returns the Message ID used to send the message for the purpose of comparing
  it to the Message ID used later in the UNSUBACK event handler.
------------------------------------------------------------------------------*}
function TMQTTClient.Unsubscribe(Topic: TMqttString): integer;
var
  Data: TBytes;
  FH: byte;
  RL: TRemainingLength;
  VH: TBytes;
  Payload: TUTF8Text;
begin
  FH := FixedHeader(MQTT.UNSUBSCRIBE, 0, 0, 0);
  VH := VariableHeaderUnsubscribe;
  Result := (FMessageID - 1);
  SetLength(Payload, 0);
  AppendArray(Payload, StrToBytes(Topic, True));
  RL := RemainingLength(Length(VH) + Length(Payload));
  Data := BuildCommand(FH, RL, VH, Payload);
  SocketWrite(Data);
end;

{*------------------------------------------------------------------------------
  Not Reliable. This is a leaky abstraction. The Core Socket components can only
  tell if the connection is truly Connected if they try to read or write to the
  socket. Therefore this reflects a boolean flag which is set in the
  TMQTTClient.Connect and .Disconnect methods.
  @return Returns whether the internal connected flag is set or not.
------------------------------------------------------------------------------*}
function TMQTTClient.isConnected: boolean;
begin
  Result := FisConnected;
end;

{*------------------------------------------------------------------------------
  Component Constructor,
  @param Hostname   Hostname of the MQTT Server
  @param Port   Port of the MQTT Server
  @return Instance
------------------------------------------------------------------------------*}
constructor TMQTTClient.Create(Hostname: UTF8String; Port: integer);
begin
  inherited Create;

  EventEnabled:=false;
  QueueEnabled:=true;

  // Create a Default ClientID as a default. Can be overridden with TMQTTClient.ClientID any time before connection.
  FClientID := 'tMQTTClient' + IntToStr(DateTimeToUnix(Time));
  FUserName := '';
  FPassword := '';
  FHostname := Hostname;
  FPort := Port;
  FMessageID := 1;
  InitCriticalSection(FCritical);
  InitCriticalSection(FCritThreadPtr);
  FMessageQueue := TQueue.Create;
  FMessageAckQueue := TQueue.Create;
end;

destructor TMQTTClient.Destroy;
begin
  TerminateThread(True);
  try
    EnterCriticalsection(FCritical);
    FMessageQueue.Free;
    FMessageAckQueue.Free;
  finally
    LeaveCriticalsection(FCritical);
  end;
  DoneCriticalSection(FCritical);
  DoneCriticalSection(FCritThreadPtr);
  inherited;
end;

function FixedHeader(MessageType: TMQTTMessageType; Dup, Qos, Retain: byte): byte;
begin
{ Fixed Header Spec:
  byte 1 bits |7 6 5 4     | 3        | 2 1      | 0     |
  fields      |Message Type| DUP flag | QoS level| RETAIN|
}
  Result := byte(Ord(MessageType) shl 4) or (Dup shl 3) or (Qos shl 1) or
    (Retain shl 0);
  //todo: OLD code: Result := (Ord(MessageType) * 16) + (Dup * 8) + (Qos * 2) + (Retain * 1);
end;

function TMQTTClient.GetMessageID: TBytes;
begin
  Assert((FMessageID > Low(word)), 'Message ID too low');
  Assert((FMessageID < High(word)), 'Message ID has gotten too big');

{  FMessageID is initialised to 1 upon TMQTTClient.Create
  The Message ID is a 16-bit unsigned integer, which typically increases by exactly
  one from one message to the next, but is not required to do so.
  The two bytes of the Message ID are ordered as MSB, followed by LSB (big-endian).}
  SetLength(Result, 2);
  Result[0] := Hi(FMessageID);
  Result[1] := Lo(FMessageID);
  Inc(FMessageID);
end;

function TMQTTClient.SocketWrite(Data: TBytes): boolean;
//todo: DEL: var sentData: integer;
begin
  Result := False;
  // Returns whether the Data was successfully written to the socket.
  if isConnected then
  begin
    EnterCriticalsection(FCritThreadPtr);
    Result := FReadThread.SocketWrite(Data);
    LeaveCriticalsection(FCritThreadPtr);
  end;
end;

function StrToBytes(str: UTF8String; perpendLength: boolean): TUTF8Text;
var
  i, offset: integer;
begin
  { This is a UTF-8 hack to give 2 Bytes of Length followed by the string itself. }
  if perpendLength then
  begin
    SetLength(Result, Length(str) + 2);
    Result[0] := Length(str) div 256;
    Result[1] := Length(str) mod 256;
    offset := 1;
  end
  else
  begin
    SetLength(Result, Length(str));
    offset := -1;
  end;
  for I := 1 to Length(str) do
    Result[i + offset] := Ord(str[i]);
end;

function RemainingLength(MessageLength: integer): TRemainingLength;
var
  byteindex: integer;
  digit: integer;
begin
  SetLength(Result, 1);
  byteindex := 0;
  while (MessageLength > 0) do
  begin
    digit := MessageLength mod 128;
    MessageLength := MessageLength div 128;
    if MessageLength > 0 then
    begin
      digit := digit or $80;
    end;
    Result[byteindex] := digit;
    if MessageLength > 0 then
    begin
      Inc(byteindex);
      SetLength(Result, Length(Result) + 1);
    end;
  end;
end;

function VariableHeaderConnect(KeepAlive: word; Username: word = 0; Password: word = 0): TBytes;
const
  //todo: version update! MQIsdp->MQTT. version 4!
  MQTT_PROTOCOL = 'MQIsdp';
  MQTT_VERSION = 3;
var
  Qos, Retain: word;
{todo: connect flags support
5 Will Retain
4 Will QoS
3 Will QoS
2 Will Flag
1 Clean Session
0 Reserved }
  iByteIndex: integer;
  ProtoBytes: TUTF8Text;
begin
  // Set the Length of our variable header array.
  SetLength(Result, 12);
  iByteIndex := 0;
  // Put out Protocol string in there.
  ProtoBytes := StrToBytes(MQTT_PROTOCOL, True);
  CopyIntoArray(Result, ProtoBytes, iByteIndex);
  Inc(iByteIndex, Length(ProtoBytes));
  // Version Number = 3
  Result[iByteIndex] := MQTT_VERSION;
  Inc(iByteIndex);
  // Connect Flags
  Qos := 0;
  Retain := 0;
  Result[iByteIndex] := 0;
  Result[iByteIndex] := (Username * 128) + (Password * 64) + (Retain * 32) + (Qos * 16) + (1 * 4) + (1 * 2);
  Inc(iByteIndex);
  Result[iByteIndex] := 0;
  Inc(iByteIndex);
  Result[iByteIndex] := KeepAlive;
end;

function TMQTTClient.VariableHeaderPublish(topic: TMqttString): TBytes;
var
  BytesTopic: TUTF8Text;
begin
  BytesTopic := StrToBytes(Topic, True);
  SetLength(Result, Length(BytesTopic));
  CopyIntoArray(Result, BytesTopic, 0);
end;

function TMQTTClient.VariableHeaderSubscribe: TBytes;
begin
  Result := GetMessageID;
end;

function TMQTTClient.VariableHeaderUnsubscribe: TBytes;
begin
  Result := GetMessageID;
end;

procedure CopyIntoArray(var DestArray: array of byte; SourceArray: array of byte;
  StartIndex: integer);
begin
  Assert(StartIndex >= 0);
  // WARNING! move causes range check error if source length is zero.
  if Length(SourceArray) > 0 then
    Move(SourceArray[0], DestArray[StartIndex], Length(SourceArray));
end;

procedure AppendArray(var Dest: TUTF8Text; Source: array of byte);

var
  DestLen: integer;
begin
  // WARNING: move causes range check error if source length is zero!
  if Length(Source) > 0 then
  begin
    DestLen := Length(Dest);
    SetLength(Dest, DestLen + Length(Source));
    Move(Source, Dest[DestLen], Length(Source));
  end;
end;

function BuildCommand(FixedHead: byte; RemainL: TRemainingLength;
  VariableHead: TBytes; Payload: array of byte): TBytes;
var
  iNextIndex: integer;
begin
  // Attach Fixed Header (1 byte)
  iNextIndex := 0;
  SetLength(Result, 1);
  Result[iNextIndex] := FixedHead;

  // Attach RemainingLength (1-4 bytes)
  iNextIndex := Length(Result);
  SetLength(Result, Length(Result) + Length(RemainL));
  CopyIntoArray(Result, RemainL, iNextIndex);

  // Attach Variable Head
  iNextIndex := Length(Result);
  SetLength(Result, Length(Result) + Length(VariableHead));
  CopyIntoArray(Result, VariableHead, iNextIndex);

  // Attach Payload.
  iNextIndex := Length(Result);
  SetLength(Result, Length(Result) + Length(Payload));
  CopyIntoArray(Result, Payload, iNextIndex);
end;

procedure TMQTTClient.OnRTConnAck(Sender: TObject; ReturnCode: integer);
begin
  if ReturnCode = 0 then
  begin
    FisConnected := True;
  end;
  if Assigned(OnConnAck) and EventEnabled then
  begin
    OnConnAck(Self, ReturnCode);
  end;
  if QueueEnabled then
  begin
    // Protected code.
    EnterCriticalSection(FCritical);
    try
      FMessageAckQueue.Push(TMQTTMessageAck.Create(CONNACK, 0, ReturnCode, 0));
    finally
      LeaveCriticalSection(FCritical);
    end;
  end;
end;

procedure TMQTTClient.OnRTPingResp(Sender: TObject);
begin
  if Assigned(OnPingResp) and EventEnabled then
  begin
    OnPingResp(Self);
  end;
  if QueueEnabled then
  begin
    // Protected code.
    EnterCriticalSection(FCritical);
    try
      FMessageAckQueue.Push(TMQTTMessageAck.Create(PINGRESP, 0, 0, 0));
    finally
      LeaveCriticalSection(FCritical);
    end;
  end;
end;

procedure TMQTTClient.OnRTPublish(Sender: TObject; topic, payload: TMqttString;
  retain: boolean);
begin
  if Assigned(OnPublish) and EventEnabled then
  begin
    OnPublish(Self, topic, payload, retain);
  end;
  if QueueEnabled then
  begin
    // Protected code.
    EnterCriticalSection(FCritical);
    try
      FMessageQueue.Push(TMQTTMessage.Create(topic, payload, retain));
    finally
      LeaveCriticalSection(FCritical);
    end;
  end;
end;

procedure TMQTTClient.OnRTSubAck(Sender: TObject; MessageID: integer;
  GrantedQoS: integer);
begin
  if Assigned(OnSubAck) and EventEnabled then
  begin
    OnSubAck(Self, MessageID, GrantedQoS);
  end;
  if QueueEnabled then
  begin
    // Protected code.
    EnterCriticalSection(FCritical);
    try
      FMessageAckQueue.Push(TMQTTMessageAck.Create(SUBACK, MessageID,
        0, GrantedQos));
    finally
      LeaveCriticalSection(FCritical);
    end;
  end;
end;

procedure TMQTTClient.OnRTUnSubAck(Sender: TObject; MessageID: integer);
begin
  if Assigned(OnUnSubAck) and EventEnabled then
  begin
    OnUnSubAck(Self, MessageID);
  end;
  if QueueEnabled then
  begin
    // Protected code.
    EnterCriticalSection(FCritical);
    try
      FMessageAckQueue.Push(TMQTTMessageAck.Create(UNSUBACK, MessageID, 0, 0));
    finally
      LeaveCriticalSection(FCritical);
    end;
  end;
end;

function TMQTTClient.getMessage: TMQTTMessage;
begin
  // Protected code.
  EnterCriticalSection(FCritical);
  try
    Result := TMQTTMessage(FMessageQueue.Pop);
  finally
    LeaveCriticalSection(FCritical);
  end;
end;

function TMQTTClient.getMessageAck: TMQTTMessageAck;
begin
  // Protected code.
  EnterCriticalSection(FCritical);
  try
    Result := TMQTTMessageAck(FMessageAckQueue.Pop);
  finally
    LeaveCriticalSection(FCritical);
  end;
end;

end.
