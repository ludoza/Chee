unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Menus, ActnList, fpjson;

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

    function ObjectToTag(aObject: TObject): PtrInt;
    function TagToObject(aTag: PtrInt): TObject;
    function JSONToTag(aJSON: TJSONObject): PtrInt;
    function TagToJSON(aTag: PtrInt): TJSONObject;
  end;

  { TMainForm }

  TMainForm = class(TForm)
    RefreshDispatcher: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    Edit1: TEdit;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RefreshDispatcherExecute(Sender: TObject);
  private
    fDispatcher : TDispatcher;
  public
    property Dispatcher: TDispatcher read fDispatcher;
  end;

var
  MainForm: TMainForm;

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

function TDispatcher.ObjectToTag(aObject: TObject): PtrInt;
begin
  Result := PtrInt(aObject)
end;

function TDispatcher.TagToObject(aTag: PtrInt): TObject;
begin
  Result := TJSONObject(aTag)
end;

function TDispatcher.JSONToTag(aJSON: TJSONObject): PtrInt;
begin
  Result := PtrInt(aJSON)
end;

function TDispatcher.TagToJSON(aTag: PtrInt): TJSONObject;
begin
  Result := TJSONObject(aTag)
end;

{ TDispatcher }


{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);


begin
  fDispatcher := TDispatcher.Create(TDispatcherItem);
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  vJSONData : TJSONData;
  result : Boolean;
  jObject : TJSONObject;
begin
  vJSONData := GetJSON('{}');
  jObject := TJSONObject(vJSONData);
  jObject.Strings['m'] := Edit1.Text;
  try
    result := Dispatcher.trigger('js:mqtt.sendMessage', TObject(jObject));
  finally
    vJSONData.free;
  end;
  if result then
    Edit1.Text:= ''
  else
    ShowMessage('Failed to send message');
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

end;

procedure TMainForm.RefreshDispatcherExecute(Sender: TObject);
var
  i: Integer;
begin
  Memo1.Lines.Clear;
  for i:= 0 to pred(fDispatcher.Count) do
  begin
    Memo1.Lines.Add(fDispatcher.Items[i].DisplayName);
  end;
end;

end.

