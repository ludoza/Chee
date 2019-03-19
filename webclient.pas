unit webclient;

{$mode objfpc}{$H+}
{$macro on}
{$define writeln := Form1.MemoOutput.lines.add}

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
   opensslsockets;
  {$ENDIF}
Type

  { TWebClient }

  TWebClient = Class(TObject)
    uri: String;
    filename: String;
    data: TStringList;
    procedure DoProgress(Sender: TObject; Const ContentLength, CurrentPos : Int64);
    procedure DoHeaders(Sender : TObject);
    procedure DoPassword(Sender: TObject; var RepeatRequest: Boolean);
    procedure ShowRedirect(ASender : TObject; Const ASrc : String; Var ADest : String);
    Procedure GetUriToFileName;
    Procedure Post;
  end;

implementation

uses main; { BAD!1 but needed for the macro writeln hack }

procedure TWebClient.DoHeaders(Sender : TObject);
Var
  I : Integer;
begin
  Writeln('Response headers received:');
  With (Sender as TFPHTTPClient) do
    For I:=0 to ResponseHeaders.Count-1 do
      Writeln(ResponseHeaders[i]);
end;

procedure TWebClient.DoProgress(Sender: TObject; const ContentLength, CurrentPos: Int64);
begin
  If (ContentLength=0) then
    Writeln('Reading headers : ' + IntToStr(CurrentPos) + ' Bytes.')
  else If (ContentLength=-1) then
    Writeln('Reading data (no length available) : ' + IntToStr(CurrentPos) + ' Bytes.')
  else
    Writeln('Reading data : ' + IntToStr(CurrentPos) + ' Bytes of ' + IntToStr(ContentLength));
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
  {Writeln('Authorization required. Remote site says: ',H);
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
  Writeln('Following redirect from "' + ASrc + '" to "' + ADest + '"');
end;

procedure TWebClient.GetUriToFileName;
begin
  With TFPHTTPClient.Create(Nil) do
    try
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
      Get(uri, filename);
    finally
      Free;
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
        //SL.Add('a=' + IntToStr(9));
        //SL.Add('b=' + IntToStr(6));
        try
          PostStr := SimpleFormPost(uri, data);
          writeln(PostStr);
        except
          on E: exception do
            writeln(E.Message);
        end;
      finally
        //SL.Free;
      end;
    finally
      Free;
    end;
end;


end.
