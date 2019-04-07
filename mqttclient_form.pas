unit mqttclient_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Buttons;

type

  { TfrmMqttClient }

  TfrmMqttClient = class(TForm)
    btnConnect: TBitBtn;
    btnDisconnect: TBitBtn;
    btnDisconnect1: TBitBtn;
    btnDisconnect2: TBitBtn;
    edtUri: TEdit;
    edtUri1: TEdit;
    edtUri2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
    StatusBar1: TStatusBar;
    TreeView1: TTreeView;
  private

  public

  end;

var
  frmMqttClient: TfrmMqttClient;

implementation

{$R *.lfm}

end.

