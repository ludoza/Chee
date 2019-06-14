unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Menus, ActnList, ComCtrls, fpjson;

type
  { TDispatcherItem }
  TDispatcherItem = class(TCollectionItem)
  private
    fName: string;
    fAction: TBasicAction;
  protected
    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;
  public
    property Action: TBasicAction read fAction write fAction;
  end;

  { TDispatcher }
  TDispatcher = class(TCollection)
  public
    constructor Create(AItemClass: TCollectionItemClass);
    function Trigger(aDisplayName: string): Boolean;
    function Trigger(aDisplayName: string; aObj: TObject): Boolean;
  end;

  { TMainForm }

  TMainForm = class(TForm)
    edtMessage: TMemo;
    Send: TAction;
    RefreshDispatcher: TAction;
    ActionList1: TActionList;
    btnSend: TButton;
    edtEvent: TEdit;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    Panel1: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    tvEvents: TTreeView;
    procedure edtMessageKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RefreshDispatcherExecute(Sender: TObject);
    procedure SendExecute(Sender: TObject);
    procedure tvEventsSelectionChanged(Sender: TObject);
  private
    fDispatcher : TDispatcher;
  public
    property Dispatcher: TDispatcher read fDispatcher;
  end;

var
  MainForm: TMainForm;

function ObjectToTag(aObject: TObject): PtrInt;
function TagToObject(aTag: PtrInt): TObject;
function JSONToTag(aJSON: TJSONObject): PtrInt;
function TagToJSON(aTag: PtrInt): TJSONObject;

implementation

{$R *.lfm}

uses
  LCLType,
  Windows;
{ TDispatcherItem }

function TDispatcherItem.GetDisplayName: string;
begin
  inherited GetDisplayName; // do we actually want to call this?
  Result:= fName;
end;

procedure TDispatcherItem.SetDisplayName(const Value: string);
begin
  inherited SetDisplayName(Value);
  fName := Value;
end;

{ TDispatcher }

constructor TDispatcher.Create(AItemClass: TCollectionItemClass);
begin
  inherited Create(AItemClass);
end;

function TDispatcher.Trigger(aDisplayName: string): Boolean;
begin
  result := self.Trigger(aDisplayName, nil);
end;

function TDispatcher.Trigger(aDisplayName: string; aObj: TObject): Boolean;
var
  i : Integer;
  vItem: TDispatcherItem;
begin
  Result := False;
  for i:= 0 to self.Count -1 do
  begin
    vItem := TDispatcherItem(self.Items[i]);
    if aDisplayName = vItem.DisplayName then
    try
        if (aObj <> nil) then
        begin
          if vItem.Action.Tag <> 0 then raise Exception(ClassName + '.Action.Tag must be 0 to pass action arguments.');
          vItem.Action.Tag := ObjectToTag(aObj);
        end;
        if vItem.Action.Execute then
        begin
          Result := True;
        end;
    finally
      if (aObj <> nil) then
      begin
        vItem.Action.Tag := 0;
      end;
    end;
  end;
end;

function ObjectToTag(aObject: TObject): PtrInt;
begin
  Result := PtrInt(aObject)
end;

function TagToObject(aTag: PtrInt): TObject;
begin
  Result := TJSONObject(aTag)
end;

function JSONToTag(aJSON: TJSONObject): PtrInt;
begin
  Result := PtrInt(aJSON)
end;

function TagToJSON(aTag: PtrInt): TJSONObject;
begin
  Result := TJSONObject(aTag)
end;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  vItem: TDispatcherItem;
begin
  fDispatcher := TDispatcher.Create(TDispatcherItem);
  vItem := TDispatcherItem(fDispatcher.Add);
  vItem.DisplayName:= 'fp:Main.Send';
  vItem.Action := Send
end;

procedure TMainForm.edtMessageKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    Send.Execute;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  fDispatcher.Destroy;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  RefreshDispatcher.Execute;
  tvEvents.Selected := tvEvents.Items[0];
end;

procedure TMainForm.RefreshDispatcherExecute(Sender: TObject);
var
  i: Integer;
  eventNode: TTreeNode;
begin
  tvEvents.Items.Clear;
  for i:= 0 to pred(fDispatcher.Count) do
  begin
    eventNode := tvEvents.Items.add(nil, fDispatcher.Items[i].DisplayName);
    eventNode.Data := TDispatcherItem(fDispatcher.Items[i]).Action; // TODO maybe add dispatcher item instead of action
  end
end;

procedure TMainForm.SendExecute(Sender: TObject);
var
  vEventName: string;
  result : Boolean;
  jObject : TJSONObject;
begin
  vEventName := edtEvent.Text;
  if TComponent(Sender).Tag > 0 then // Was the Action triggered from the dispatcher with a Args Object attached to the tag?
  begin
    jObject := TagToJSON(TComponent(Sender).Tag);
    if jObject.IndexOfName('e') <> -1 then
    begin
      vEventName := jObject.Strings['e'];
      jObject.Remove(jObject.Find('e'));
    end;
    result := Dispatcher.trigger(vEventName, TObject(jObject));
  end else // use the input controls to create our message
  try
    if LeftStr(edtMessage.Text, 1) = '{' then
      jObject := TJSONObject(GetJSON(edtMessage.Text))
    else
    begin
      jObject := TJSONObject(GetJSON('{}'));
      jObject.Strings['m'] := edtMessage.Text;
    end;
    result := Dispatcher.trigger(vEventName, TObject(jObject));
  finally
    jObject.free;
  end;
  if result then
  begin
    edtMessage.Text := '';
    tvEventsSelectionChanged(Sender);
  end
  else
    ShowMessage('Failed to send message');
end;

procedure TMainForm.tvEventsSelectionChanged(Sender: TObject);
var
  vNode: TTreeNode;
  findPos: Integer;
begin
  if tvEvents.Selected <> nil then
  begin
    vNode := tvEvents.Selected;
    edtEvent.Text := vNode.Text;
    edtMessage.Lines.Clear;
    if (vNode.Data <> nil) and TObject(vNode.Data).InheritsFrom(TAction) then
    with edtMessage do
    begin
       Lines.Add(TAction(vNode.Data).Hint);
       findPos := Pos('m', Text);
       Perform(EM_SCROLLCARET, 0, findPos);
       SetFocus;
    end;


  end;

end;

end.

