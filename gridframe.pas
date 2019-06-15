unit gridframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, Buttons, StdCtrls, Grids,
  ActnList;

type

  { TfrmGrid }

  TfrmGrid = class(TFrame)
    Load: TAction;
    ActionList: TActionList;
    BitBtn1: TBitBtn;
    edtUri: TEdit;
    ImageList: TImageList;
    StringGrid1: TStringGrid;
    ToolBar1: TToolBar;
    procedure LoadExecute(Sender: TObject);
    function GetDownloadUri: String;
  private

  public

  end;

implementation

{$R *.lfm}

uses
  webclient;
{ TfrmGrid }

procedure TfrmGrid.LoadExecute(Sender: TObject);
var
  sResponse: string;
begin
  With TWebClient.Create do
  try
    uri := self.GetDownloadUri();
    WriteLn('Download Uri: ' + uri );
    sResponse := Get(uri);
  finally
    Free;
  end;
  StringGrid1.SaveOptions := StringGrid1.SaveOptions + [soDesign];
  //StringGrid1.LoadFromFile(EditGridFile.Text);
  StringGrid1.Options := StringGrid1.Options + [goEditing];
end;

function TfrmGrid.GetDownloadUri: String;
begin
  Result := 'http://planeteer.mooo.com:8088/admin/user/tah/list/xml'
end;

end.

