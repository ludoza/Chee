unit webclient_form;

{$mode objfpc}{$H+}
{$macro on}
{$define writeln := frmWebClient.MemoOutput.lines.add}
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, Buttons;

type

  { TfrmWebClient }

  TfrmWebClient = class(TForm)
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
    procedure btnGetClick(Sender: TObject);
  private

  public

  end;

var
  frmWebClient: TfrmWebClient;

implementation

{$R *.lfm}

uses webclient;

{ TfrmWebClient }

procedure TfrmWebClient.btnGetClick(Sender: TObject);
begin
    With TWebClient.Create do
    try
      uri := edtUri.text;
      filename := edtOut.text;
      WriteLn('Download Uri: ' + uri + ' To File: ' + filename);
      GetUriToFileName();
      MemoOutput.Lines.LoadFromFile(edtOut.text);
    finally
      Free;
    end;
end;

end.

