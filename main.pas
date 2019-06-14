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
    Send: TAction;
    edtMessage: TEdit;
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
    procedure btnSendClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
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
  vComp: TComponent;
  ownComp: Boolean;
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
      // debug info here TODO
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

{ TDispatcher }


{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);


begin
  fDispatcher := TDispatcher.Create(TDispatcherItem);
end;

procedure TMainForm.btnSendClick(Sender: TObject);
begin
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin

end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  fDispatcher.Destroy;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  RefreshDispatcher.Execute;
end;

procedure TMainForm.RefreshDispatcherExecute(Sender: TObject);
var
  i: Integer;
begin
  tvEvents.Items.Clear;
  for i:= 0 to pred(fDispatcher.Count) do
  begin
    tvEvents.Items.add(nil, fDispatcher.Items[i].DisplayName)
  end
end;

procedure TMainForm.SendExecute(Sender: TObject);
var
  vEventName: string;
  vJSONData : TJSONData;
  result : Boolean;
  jObject : TJSONObject;
begin
  vEventName := edtEvent.Text;
  if TComponent(Sender).Tag > 0 then
  begin
    jObject := TagToJSON(TComponent(Sender).Tag);
    if jObject.IndexOfName('e') <> -1 then
    begin
      vEventName := jObject.Strings['e'];
      jObject.Remove(jObject.Find('e'));
    end;
  end else
  begin
    jObject := TJSONObject(GetJSON('{}'));
    jObject.Strings['m'] := edtMessage.Text;
  end;
  try
    result := Dispatcher.trigger(edtEvent.Text, TObject(jObject));
  finally
    vJSONData.free;
  end;
  if result then
    edtEvent.Text:= ''
  else
    ShowMessage('Failed to send message');
end;

procedure TMainForm.tvEventsSelectionChanged(Sender: TObject);
begin
  edtEvent.Text := tvEvents.Selected.Text
end;

end.

