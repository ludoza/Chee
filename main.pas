unit main;

{$mode objfpc}{$H+}
{$macro on}
{$define writeln := MemoOutput.lines.add}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ActnList, Menus,
  ComCtrls, StdCtrls, Grids, PairSplitter, Buttons;

type

  { TForm1 }

  TForm1 = class(TForm)
    actDownloadGrid: TAction;
    actSaveCell: TAction;
    actSettings: TAction;
    actQuit: TAction;
    actSaveGrid: TAction;
    actLoadGrid: TAction;
    ActionList1: TActionList;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    EditModel: TComboBox;
    EditUri: TEdit;
    EditGridFile: TEdit;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    lblUriPostfix: TLabel;
    Label4: TLabel;
    MainMenu1: TMainMenu;
    MemoOutput: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    N2: TMenuItem;
    N1: TMenuItem;
    miAdmin: TMenuItem;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    StatusBar1: TStatusBar;
    StringGrid1: TStringGrid;
    ToolBar1: TToolBar;
    procedure actDownloadGridExecute(Sender: TObject);
    procedure actLoadGridExecute(Sender: TObject);
    procedure actQuitExecute(Sender: TObject);
    procedure actSaveGridExecute(Sender: TObject);
    procedure EditModelChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure StringGrid1EditingDone(Sender: TObject);
    procedure StringGrid1SelectEditor(Sender: TObject; aCol, aRow: Integer;
      var Editor: TWinControl);
    procedure StringGrid1SetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
  private
    fCol, fRow: Integer;
    fEditor: TWinControl;
  public
    function GetDownloadUri: String;
    function GetUpdateCellUri: String;
    function GetDownloadFilename: String;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses webclient;

{ TForm1 }

function TForm1.GetDownloadUri: String;
var
  str: string;
begin
  str := Self.EditUri.Text;
  result := str + '/' + Self.EditModel.Text + lblUriPostfix.Caption;
end;

function TForm1.GetUpdateCellUri: String;
var
  str: string;
begin
  str := Self.EditUri.Text;
  result := str + '/' + Self.EditModel.Text + '/ajax/update/';

end;

function TForm1.GetDownloadFilename: String;
begin
  result := 'tah/' + Self.EditModel.Text + '.xml';
end;

procedure TForm1.actLoadGridExecute(Sender: TObject);
begin
  StringGrid1.SaveOptions := StringGrid1.SaveOptions + [soDesign];
  StringGrid1.LoadFromFile(EditGridFile.Text);
  StringGrid1.Options := StringGrid1.Options + [goEditing];

  WriteLn('Grid Loaded From: ' + EditGridFile.Text);
end;

procedure TForm1.actQuitExecute(Sender: TObject);
begin
  WriteLn('Bye Bye!');
  Application.Terminate();
end;

procedure TForm1.actSaveGridExecute(Sender: TObject);
begin
  StringGrid1.SaveOptions := StringGrid1.SaveOptions + [soDesign];
  StringGrid1.SaveToFile(EditGridFile.Text);

  WriteLn('Grid Saved To: ' + EditGridFile.Text);
end;

procedure TForm1.EditModelChange(Sender: TObject);
begin

end;

procedure TForm1.actDownloadGridExecute(Sender: TObject);
begin
  With TWebClient.Create do
  try
    uri := self.GetDownloadUri();
    filename := EditGridFile.Caption;
    WriteLn('Download Uri: ' + uri + ' To File: ' + filename);
    GetUriToFileName();
  finally
    Free;
  end;
  Self.actLoadGridExecute(Sender);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

procedure TForm1.StringGrid1EditingDone(Sender: TObject);
var
  aRow, aCol: Integer;
  aEditor: TWinControl;
  Value: String;
begin
  aRow := fRow;
  aCol := fCol;
  aEditor := StringGrid1.EditorByStyle(cbsAuto);
  Value := TStringCellEditor(aEditor).Text;
  With TWebClient.Create do
  try
    data := TStringList.create;
    //data['list_form_pk'] := StringGrid1.Cells[0,ARow];
    //data[StringGrid1.Cells[ACol,0]] := Value
    data.Add('list_form_pk=' + StringGrid1.Cells[0,ARow]);
    data.Add(StringGrid1.Cells[ACol,0] + '=' + Value);

    uri := self.GetUpdateCellUri();
    //filename := EditGridFile.Caption;
    WriteLn('Post Uri: ' + uri);
    Post;
  finally
    data.Free;
    Free;
  end;
end;

procedure TForm1.StringGrid1SelectEditor(Sender: TObject; aCol, aRow: Integer;
  var Editor: TWinControl);
begin
  fEditor:= Editor;
  fCol:= aCol;
  fRow:= aRow;
end;

procedure TForm1.StringGrid1SetEditText(Sender: TObject; ACol, ARow: Integer;
  const Value: string);
begin

end;

end.

