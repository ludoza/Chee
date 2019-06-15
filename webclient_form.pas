unit webclient_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, Buttons, ActnList;

type

  { TfrmWebClient }

  TfrmWebClient = class(TForm)
    Post: TAction;
    Get: TAction;
    HttpActionList: TActionList;
    btnGet: TBitBtn;
    btnPost: TBitBtn;
    edtUri: TEdit;
    edtOut: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    MemoOutput: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    StatusBar1: TStatusBar;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GetExecute(Sender: TObject);
    procedure PostExecute(Sender: TObject);
    procedure btnGetClick(Sender: TObject);
  private
    ForcedShow: Boolean;
  public

  end;

var
  frmWebClient: TfrmWebClient;

implementation

{$R *.lfm}

uses
  main,
  webclient,
  fpjson,
  jsonparser,
  ezutil;

{ TfrmWebClient }

procedure TfrmWebClient.btnGetClick(Sender: TObject);
begin

end;

procedure TfrmWebClient.PostExecute(Sender: TObject);
begin

end;

procedure TfrmWebClient.FormCreate(Sender: TObject);
var
  i: Integer;
  vItem: TDispatcherItem;
begin
  for i := 0 to pred(HttpActionList.ActionCount) do
  begin
    vItem := TDispatcherItem(MainForm.Dispatcher.Add);
    vItem.Action := HttpActionList[i];
    vItem.DisplayName:= 'fp:http.' + TAction(vItem.Action).Caption;
  end;

  ForcedShow:= True;
  Show;
end;

procedure TfrmWebClient.FormShow(Sender: TObject);
begin
  if ForcedShow then begin
    ForcedShow := False;
    Hide;
  end;
end;

procedure TfrmWebClient.GetExecute(Sender: TObject);
var
  jData : TJSONData;
  jObject : TJSONObject;
begin
  with TWebClient.Create do
  try
    if TComponent(Sender).tag > 0 then
    begin
      uri := edtUri.text;
      filename := edtOut.text;
    end else begin
      uri := edtUri.text;
      filename := edtOut.text;
    end;


    WriteDebug('Download Uri: ' + uri + ' To File: ' + filename);
    GetUriToFileName();
    MemoOutput.Lines.LoadFromFile(edtOut.text);
  finally
    Free;
  end;
end;

end.

