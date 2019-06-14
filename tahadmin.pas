unit tahadmin;

{$mode objfpc}{$H+}
{$macro on}
{$define writeln := MemoOutput.lines.add}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ActnList, Menus,
  ComCtrls, StdCtrls, Grids, PairSplitter, Buttons, ExtCtrls, Interfaces,
  mqttgate, xmlform, webclient_form, webclient, mqttclient_form;

type

  { TTahForm }

  TTahForm = class(TForm)
    actDownloadGrid: TAction;
    actDispatcher: TAction;
    actMqttClient: TAction;
    actWebClient: TAction;
    actSaveCell: TAction;
    actSettings: TAction;
    actQuit: TAction;
    actSaveGrid: TAction;
    actLoadGrid: TAction;
    ActionList1: TActionList;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    EditGridFile: TEdit;
    EditModel: TComboBox;
    EditUri: TEdit;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lblUriPostfix: TLabel;
    MainMenu1: TMainMenu;
    MemoOutput: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    miMqttClient: TMenuItem;
    miWebClient: TMenuItem;
    N2: TMenuItem;
    N1: TMenuItem;
    miAdmin: TMenuItem;
    PairSplitter1: TPairSplitter;
    PairSplitter2: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    PairSplitterSide3: TPairSplitterSide;
    PairSplitterSide4: TPairSplitterSide;
    Panel1: TPanel;
    StatusBar1: TStatusBar;
    StringGrid1: TStringGrid;
    ToolBar1: TToolBar;
    ToolBar2: TToolBar;
    TreeView1: TTreeView;
    procedure actDispatcherExecute(Sender: TObject);
    procedure actDownloadGridExecute(Sender: TObject);
    procedure actLoadGridExecute(Sender: TObject);
    procedure actMqttClientExecute(Sender: TObject);
    procedure actQuitExecute(Sender: TObject);
    procedure actSaveGridExecute(Sender: TObject);
    procedure actWebClientExecute(Sender: TObject);
    procedure EditModelChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);

    procedure StringGrid1EditingDone(Sender: TObject);
    procedure StringGrid1SelectEditor(Sender: TObject; aCol, aRow: Integer;
      var Editor: TWinControl);
    procedure StringGrid1SetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure ToolBar1Click(Sender: TObject);
    procedure TreeView1SelectionChanged(Sender: TObject);
    procedure CreateMainMenuExecute(Sender: TObject);
  private
    fCol, fRow: Integer;
    fEditor: TWinControl;
  public
    function GetDownloadUri: String;
    function GetUpdateCellUri: String;
    function GetDownloadFilename: String;
  end;

var
  TahForm: TTahForm;

implementation

{$R *.lfm}

uses main;

{ TTahForm }

function TTahForm.GetDownloadUri: String;
var
  str: string;
begin
  str := Self.EditUri.Text;
  result := str + '/' + Self.EditModel.Text + lblUriPostfix.Caption;
end;

function TTahForm.GetUpdateCellUri: String;
var
  str: string;
begin
  str := Self.EditUri.Text;
  result := str + '/' + Self.EditModel.Text + '/ajax/update/';

end;

function TTahForm.GetDownloadFilename: String;
begin
  result := 'tah/' + Self.EditModel.Text + '.xml';
end;

procedure TTahForm.actLoadGridExecute(Sender: TObject);
begin
  StringGrid1.SaveOptions := StringGrid1.SaveOptions + [soDesign];
  StringGrid1.LoadFromFile(EditGridFile.Text);
  StringGrid1.Options := StringGrid1.Options + [goEditing];

  WriteLn('Grid Loaded From: ' + EditGridFile.Text);
end;

procedure TTahForm.actMqttClientExecute(Sender: TObject);
begin
  frmMqttClient.show();
end;

procedure TTahForm.actQuitExecute(Sender: TObject);
begin
  WriteLn('Bye Bye!');
  Application.Terminate();
end;

procedure TTahForm.actSaveGridExecute(Sender: TObject);
begin
  StringGrid1.SaveOptions := StringGrid1.SaveOptions + [soDesign];
  StringGrid1.SaveToFile(EditGridFile.Text);

  WriteLn('Grid Saved To: ' + EditGridFile.Text);
end;

procedure TTahForm.actWebClientExecute(Sender: TObject);
begin
  frmWebClient.show();
end;

procedure TTahForm.EditModelChange(Sender: TObject);
begin

end;

procedure TTahForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Application.Terminate;
end;

procedure TTahForm.actDownloadGridExecute(Sender: TObject);
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

procedure TTahForm.actDispatcherExecute(Sender: TObject);
begin
  MainForm.Show;
end;

procedure TTahForm.FormCreate(Sender: TObject);
var
  vItem: TDispatcherItem;
begin
  // TODO add beter events
  //vItem := TDispatcherItem(MainForm.Dispatcher.Add);
  //vItem.DisplayName:= 'js:mqtt.sendMessage';
  //vItem.Action := actDownloadGrid;
  //XMLForm2 := TXMLForm.create(self);
  //XMLForm2.Filename := 'tah/form.xml';
  //XMLForm2.ReadComponents;
  //XMLForm2.ShowModal();

  //XMLForm1 := TXMLForm.create(self);
  //XMLForm1.Filename := 'tah/form.xml';
  //XMLForm1.WriteComponents;
  //XMLForm1.free();
  Show();
end;

procedure TTahForm.FormShow(Sender: TObject);

  var
  i: Integer;
  vItem: TDispatcherItem;
begin
  frmMqttClient.Connect.Execute();
  for i := 0 to pred(ActionList1.ActionCount) do
  begin
    vItem := TDispatcherItem(MainForm.Dispatcher.Add);
    vItem.Action := ActionList1[i];
    vItem.DisplayName:= 'fp:Admin.' + TAction(vItem.Action).Caption;
  end;
end;

procedure TTahForm.MenuItem6Click(Sender: TObject);
begin

end;

procedure TTahForm.StringGrid1EditingDone(Sender: TObject);
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

procedure TTahForm.StringGrid1SelectEditor(Sender: TObject; aCol, aRow: Integer;
  var Editor: TWinControl);
begin
  fEditor:= Editor;
  fCol:= aCol;
  fRow:= aRow;
end;

procedure TTahForm.StringGrid1SetEditText(Sender: TObject; ACol, ARow: Integer;
  const Value: string);
begin

end;

procedure TTahForm.ToolBar1Click(Sender: TObject);
begin

end;

procedure TTahForm.TreeView1SelectionChanged(Sender: TObject);
begin

  if TreeView1.Selected.Parent <> nil then
  begin
    EditModel.Text:= TreeView1.Selected.Text;
    EditUri.Text:=  TreeView1.Selected.Parent.Text;
    actDownloadGridExecute(Sender);
  end else
  begin
    EditUri.Text:=  TreeView1.Selected.Text;
  end;
end;

procedure TTahForm.CreateMainMenuExecute(Sender: TObject);
begin

end;

end.

