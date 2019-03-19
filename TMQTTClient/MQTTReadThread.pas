{
 -------------------------------------------------
  MQTTReadThread.pas -  Contains the socket receiving thread that is part of the
  TMQTTClient library (MQTT.pas).

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

unit MQTTReadThread;

interface

uses
  SysUtils, Classes, blcksock, synsock;

type
  TBytes = array of byte;

  // all string is UTF-8! (see doc: mqtt-v3.1.1 part 1.5.3).
  TMqttString = UTF8String;

type
  TMQTTMessage = record
    FixedHeader: byte;
    RL: TBytes;
    Data: TBytes;
  end;

type
  TRxStates = (RX_START, RX_FIXED_HEADER, RX_LENGTH, RX_DATA, RX_ERROR);

  TRemainingLength = array of byte;

  TUTF8Text = array of byte;

  TConnAckEvent = procedure(Sender: TObject; ReturnCode: integer) of object;

  //todo: Sender: TObject; msg: TMQTTMessage; userdata: Pointer
  TPublishEvent = procedure(Sender: TObject; topic, payload: TMqttString;
    retain: boolean) of object;

  TPingRespEvent = procedure(Sender: TObject) of object;
  TSubAckEvent = procedure(Sender: TObject; MessageID: integer;
    GrantedQoS: integer) of object;
  TUnSubAckEvent = procedure(Sender: TObject; MessageID: integer) of object;

  { TMQTTReadThread }

  TMQTTReadThread = class(TThread)
  protected
    FClientID: TMqttString;
    FHostname: UTF8String;
    FPort: integer;
    CurrentMessage: TMQTTMessage;
    // Events
    FConnAckEvent: TConnAckEvent;
    FPublishEvent: TPublishEvent;
    FPingRespEvent: TPingRespEvent;
    FSubAckEvent: TSubAckEvent;
    FUnSubAckEvent: TUnSubAckEvent;

    // Takes a 2 Byte Length array and returns the length of the string it preceeds as per the spec.
    function BytesToStrLength(LengthBytes: TBytes): integer;

    // This is our data processing and event firing command.
    procedure HandleData;

    procedure Execute; override;
  public
    //todo: change Socket resurse working
    FPSocket: TTCPBlockSocket;
    function SocketWrite(Data: TBytes): boolean;

    constructor Create(Hostname: UTF8String; Port: integer);
    property OnConnAck: TConnAckEvent read FConnAckEvent write FConnAckEvent;
    property OnPublish: TPublishEvent read FPublishEvent write FPublishEvent;
    property OnPingResp: TPingRespEvent read FPingRespEvent write FPingRespEvent;
    property OnSubAck: TSubAckEvent read FSubAckEvent write FSubAckEvent;
    property OnUnSubAck: TUnSubAckEvent read FUnSubAckEvent write FUnSubAckEvent;
  end;

implementation

uses
  {$IFDEF DEBUG_MQTT_LCLLOG}
  LCLProc, // DbgOutThreadLog
  {$ENDIF DEBUG_MQTT_LCLLOG}
  MQTT;

{$IFDEF DEBUG_MQTT_LCLLOG}
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

procedure SetBit(var Value: byte; const Index: byte; const State: boolean); inline;
begin
  Value := (Value and ((byte(1) shl Index) xor High(byte))) or
    (byte(State) shl Index);
end;

function GetBit(const Value: byte; const Index: byte): boolean; inline;
begin
  Result := ((Value shr Index) and 1) = 1;
end;

{ TMQTTReadThread }

constructor TMQTTReadThread.Create(Hostname: UTF8String; Port: integer);
begin
  inherited Create(True);

  // Create a Default ClientID as a default. Can be overridden with TMQTTClient.ClientID any time before connection.
  FClientID := 'dMQTTClientx' + IntToStr(Random(1000) + 1);
  FHostname := Hostname;
  FPort := Port;
end;

procedure TMQTTReadThread.Execute;
var
  rxState: TRxStates;
  remainingLengthx: integer;
  digit: integer;
  multiplier: integer;
  Data: TBytes;
  RL: TRemainingLength;
  VH: TBytes;
  FH: byte;
  Payload: TUTF8Text;
  error: integer;
begin
  rxState := RX_START;
  try
    // Create a socket.
    FPSocket := TTCPBlockSocket.Create;
    // We really don't want sending on the socket to block our main thread.
    FPSocket.nonBlockMode := True;
    FPSocket.NonblockSendTimeout := 1;
    while not self.Terminated do
    begin
      case rxState of
        RX_START:
        begin
          WRITE_DEBUG('TMQTTReadThread: RX_START begin...');

          // Make the socket connection
          FPSocket.Connect(FHostname, IntToStr(FPort));

          //todo: check error code after FPSocket.Connect
          WRITE_DEBUG('TMQTTReadThread: FPSocket.LastErrorDesc='+FPSocket.LastErrorDesc);
          WRITE_DEBUG('TMQTTReadThread: FPSocket.LastError='+IntToStr(FPSocket.LastError));

          //  Build CONNECT message
          FH := FixedHeader(MQTT.CONNECT, 0, 0, 0);
          VH := VariableHeaderConnect(40);
          SetLength(Payload, 0);
          AppendArray(Payload, StrToBytes(FClientID, True));
          AppendArray(Payload, StrToBytes('lwt', True)); //todo: add normal "LWT" topic support
          AppendArray(Payload, StrToBytes(FClientID + ' died', True)); //todo: add normal "LWT" message support
          {todo: add support username and password (see doc: mqtt-v3.1.1 part 3.1.3.4).
          http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html#_Toc398718031
          Need just add to payload.
          "These fields, if present, MUST appear in the order Client Identifier, Will Topic, Will Message, User Name, Password [MQTT-3.1.3-1]."
          }
          RL := RemainingLength(Length(VH) + Length(Payload));
          Data := BuildCommand(FH, RL, VH, Payload);

          //sleep(1);

          // Send CONNECT message
          while not self.Terminated do
          begin
            WRITE_DEBUG('TMQTTReadThread: Send CONNECT message...');
            SocketWrite(Data);
            error := FPSocket.LastError;
            //WRITE_DEBUG('TMQTTReadThread: FPSocket.LastErrorDesc=', FPSocket.LastErrorDesc);
            //WRITE_DEBUG('TMQTTReadThread: FPSocket.LastError=', error);
            if error = 0 then
            begin
              WRITE_DEBUG('TMQTTReadThread: RX_START end.');
              rxState := RX_FIXED_HEADER;
              break;
            end
            else
            begin
              //todo: remove magic const!  (? WSANO_ERROR?, WSAWOULDBLOCK, WSAINPROGRESS )
              if error = 110 then
              begin
                continue;
              end;
              WRITE_DEBUG('TMQTTReadThread: RX_START error!');
              rxState := RX_ERROR;
              break;
            end;
          end;
        end;
        RX_FIXED_HEADER:
        begin
          multiplier := 1;
          remainingLengthx := 0;
          CurrentMessage.Data := nil;

          CurrentMessage.FixedHeader := FPSocket.RecvByte(1000);
          if (FPSocket.LastError = WSAETIMEDOUT) then
            continue;
          if (FPSocket.LastError <> 0) then
            rxState := RX_ERROR
          else
            rxState := RX_LENGTH;
        end;
        RX_LENGTH:
        begin
          digit := FPSocket.RecvByte(1000);
          if (FPSocket.LastError = WSAETIMEDOUT) then
            continue;
          if (FPSocket.LastError <> 0) then
            rxState := RX_ERROR
          else
          begin
            remainingLengthx :=
              remainingLengthx + (digit and 127) * multiplier;
            if (digit and 128) > 0 then
            begin
              multiplier := multiplier * 128;
              rxState := RX_LENGTH;
            end
            else
              rxState := RX_DATA;
          end;
        end;
        RX_DATA:
        begin
          SetLength(CurrentMessage.Data, remainingLengthx);
          FPSocket.RecvBufferEx(Pointer(CurrentMessage.Data),
            remainingLengthx, 1000);
          if (FPSocket.LastError <> 0) then
            rxState := RX_ERROR
          else
          begin
            HandleData;
            rxState := RX_FIXED_HEADER;
          end;
        end;
        RX_ERROR:
        begin
          // Quit the loop, terminating the thread.
          break;
        end;
      end;
    end;
  finally
    FPSocket.CloseSocket();
    FreeAndNil(FPSocket);
  end; // try
end;

procedure TMQTTReadThread.HandleData;
var
  MessageType: TMQTTMessageType;
  DataLen: integer;
  QoS: integer;
  Retain: boolean;
  Topic: TMqttString;
  Payload: TMqttString;
  ResponseVH: TBytes;
  ConnectReturn: integer;
begin
  MessageType := TMQTTMessageType(CurrentMessage.FixedHeader shr 4);

  case MessageType of
  MQTT.CONNACK:
    begin
      // Check if we were given a Connect Return Code.
      // Any return code except 0 is an Error
      if ((Length(CurrentMessage.Data) > 0) and
        (Length(CurrentMessage.Data) < 4)) then
      begin
        ConnectReturn := CurrentMessage.Data[1];
        if Assigned(OnConnAck) then
          OnConnAck(Self, ConnectReturn);
      end;
    end;
  MQTT.PUBLISH:
    begin
      Retain := GetBit(CurrentMessage.FixedHeader, 0);
      // Read the Length Bytes
      DataLen := BytesToStrLength(Copy(CurrentMessage.Data, 0, 2));
      // Get the Topic
      SetString(Topic, PChar(@CurrentMessage.Data[2]), DataLen);
      // Get the Payload
      if Length(CurrentMessage.Data) - 2 - DataLen <= 0 then
        Payload:='' else
        SetString(Payload, PChar(@CurrentMessage.Data[2 + DataLen]),
          (Length(CurrentMessage.Data) - 2 - DataLen));
      if Assigned(OnPublish) then
        OnPublish(Self, Topic, Payload, retain);
    end;
  MQTT.SUBACK:
    begin
      // Reading the Message ID
      ResponseVH := Copy(CurrentMessage.Data, 0, 2);
      DataLen := BytesToStrLength(ResponseVH);
      // Next Read the Granted QoS
      QoS := 0;
      if (Length(CurrentMessage.Data) - 2) > 0 then
      begin
        ResponseVH := Copy(CurrentMessage.Data, 2, 1);
        QoS := ResponseVH[0];
      end;
      if Assigned(OnSubAck) then
        OnSubAck(Self, DataLen, QoS);
    end;
  MQTT.UNSUBACK:
    begin
      // Read the Message ID for the event handler
      ResponseVH := Copy(CurrentMessage.Data, 0, 2);
      DataLen := BytesToStrLength(ResponseVH);
      if Assigned(OnUnSubAck) then
        OnUnSubAck(Self, DataLen);
    end;
  MQTT.PINGRESP:
    begin
      if Assigned(OnPingResp) then
        OnPingResp(Self);
    end;
  end;
end;

function TMQTTReadThread.BytesToStrLength(LengthBytes: TBytes): integer;
begin
  Assert(Length(LengthBytes) = 2,
    'TMQTTReadThread: UTF-8 Length Bytes preceeding the text must be 2 Bytes in Legnth');

  Result := 0;
  Result := LengthBytes[0] shl 8;
  Result := Result + LengthBytes[1];
end;

function TMQTTReadThread.SocketWrite(Data: TBytes): boolean;
var
  sentData: integer;
begin
  Result := False;
  // Returns whether the Data was successfully written to the socket.
  while not FPSocket.CanWrite(0) do
  begin
    sleep(100);
  end;

  sentData := FPSocket.SendBuffer(Pointer(Data), Length(Data));
  if sentData = Length(Data) then
    Result := True;
end;

end.
