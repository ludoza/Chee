unit webclient;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fphttpclient,
  //
  {$IFDEF UNIX}
    {$IFDEF BSD}
      {$IFDEF DARWIN}
      sslsockets, fpopenssl;
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
  {$IFDEF WINDOWS}
  // opensslsockets;
  sslsockets, fpopenssl,
  {$ENDIF}
  ezutil;
Type

  { TWebClient }

  TWebClient = Class(TObject)
    fWriteDebug: TWriteDebug;
  protected
    procedure SetupClient(aClient: TFPHTTPClient);
    procedure DoProgress(Sender: TObject; Const ContentLength, CurrentPos : Int64);
    procedure DoHeaders(Sender : TObject);
    procedure DoPassword(Sender: TObject; var RepeatRequest: Boolean);
    procedure ShowRedirect(ASender : TObject; Const ASrc : String; Var ADest : String);

  public

    uri: String;
    filename: String;
    data: TStringList;
    Function Get(Const AURL : String) : String;
    Procedure GetUriToFileName;
    Procedure Post;
    property WriteDebug: TWriteDebug read fWriteDebug write fWriteDebug;
    constructor Create;
  end;

implementation

uses
  webclient_form;

procedure TWebClient.DoHeaders(Sender : TObject);
Var
  I : Integer;
begin
  WriteDebug('Response headers received:');
  With (Sender as TFPHTTPClient) do
    For I:=0 to ResponseHeaders.Count-1 do
      WriteDebug(ResponseHeaders[i]);
end;

procedure TWebClient.SetupClient(aClient: TFPHTTPClient);
begin
  with aClient do
  begin
    AllowRedirect := True;
    OnRedirect := @ShowRedirect;
    OnPassword := @DoPassword;
    OnDataReceived := @DoProgress;
    { For when you need to sniff the Auth password.
    OnHeaders := @DoHeaders;
    }
    { Set this if you want to try a proxy.
    Proxy.Host:= '127.0.0.1';
    Proxy.Port:= 8080;
    }
  end;
end;

procedure TWebClient.DoProgress(Sender: TObject; const ContentLength, CurrentPos: Int64);
begin
  If (ContentLength=0) then
    WriteDebug('Reading headers : ' + IntToStr(CurrentPos) + ' Bytes.')
  else If (ContentLength=-1) then
    WriteDebug('Reading data (no length available) : ' + IntToStr(CurrentPos) + ' Bytes.')
  else
    WriteDebug('Reading data : ' + IntToStr(CurrentPos) + ' Bytes of ' + IntToStr(ContentLength));
end;

procedure TWebClient.DoPassword(Sender: TObject; var RepeatRequest: Boolean);
Var
  H,UN,PW : String;
  P : Integer;
begin
  With TFPHTTPClient(Sender) do
    begin
    H:=GetHeader(ResponseHeaders,'WWW-Authenticate');
    end;
  P:=Pos('realm',LowerCase(H));
  if (P>0) then
    begin
    P:=Pos('"',H);
    Delete(H,1,P);
    P:=Pos('"',H);
    H:=Copy(H,1,Pos('"',H)-1);
    end;
  {WriteDebug('Authorization required. Remote site says: ',H);
  Write('Enter username (empty quits): ');
  ReadLn(UN);
  RepeatRequest:=(UN<>'');
  if RepeatRequest then
    begin
    Write('Enter password: ');
    Readln(PW);
    TFPHTTPClient(Sender).UserName:=UN;
    TFPHTTPClient(Sender).Password:=PW;
    end;}
end;

procedure TWebClient.ShowRedirect(ASender: TObject; const ASrc: String;
  var ADest: String);
begin
  WriteDebug('Following redirect from "' + ASrc + '" to "' + ADest + '"');
end;


procedure TWebClient.GetUriToFileName;
var
  aClient: TFPHTTPClient;
begin
  aClient := TFPHTTPClient.Create(Nil);
  try
    SetupClient(aClient);
    aClient.Get(uri, filename);
  finally
    aClient.Free;
  end;
end;

function TWebClient.Get(const AURL: String): String;
var
  aClient: TFPHTTPClient;
begin
  aClient := TFPHTTPClient.Create(Nil);
  try
    SetupClient(aClient);
    result := aClient.Get(aURL);
  finally
    aClient.Free;
  end;
end;

procedure TWebClient.Post;
var
  PostStr: String;
begin

  With TFPHTTPClient.Create(Nil) do
    try
      AllowRedirect := True;
      OnRedirect := @ShowRedirect;
      OnPassword := @DoPassword;
      OnDataReceived := @DoProgress;

      //data := TStringList.Create;
      try
        //SL.Add('a=' + WriteDebug(9));
        //SL.Add('b=' + WriteDebug(6));
        try
          PostStr := SimpleFormPost(uri, data);
          WriteDebug(PostStr);
        except
          on E: exception do
            WriteDebug(E.Message);
        end;
      finally
        //SL.Free;
      end;
    finally
      Free;
    end;
end;

constructor TWebClient.Create;
begin
  fWriteDebug:= @(frmWebClient.MemoOutput.Lines.Add);
end;


end.
