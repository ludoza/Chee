unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

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
    Button1: TButton;
    Edit1: TEdit;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    fDispatcher : TDispatcher;
  public
    property Dispatcher: TDispatcher read fDispatcher;
  end;

var
  MainForm: TMainForm;

implementation

uses
  fpjson;
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
begin
  Result := False;
  for i:= 0 to self.Count -1 do
  begin
    vItem := TDispatcherItem(self.Items[i]);
    if aDisplayName = vItem.DisplayName then
    try
        if (aObj <> nil) then
        begin
          if aObj.InheritsFrom(TComponent) then
          begin
            vComp := TComponent(aObj);
            vItem.Action.InsertComponent(vComp);
          end else
          begin
            vComp := TComponent.create(vItem.Action);
            vComp.Tag := PtrInt(aObj);
          end;
        end;
        if vItem.Action.Execute then
        begin
          Result := True;
        end;
    finally
      if vComp <> nil then
        vItem.Action.RemoveComponent(vComp)
      // debug info here TODO
    end;

  end;
end;

{ TDispatcher }


{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);


begin
  fDispatcher := TDispatcher.Create(TDispatcherItem);
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  vComp : TJSONData;
  result : Boolean;
  jObject : TJSONObject;
begin
  vComp := GetJSON('{}');
  jObject := TJSONObject(vComp);
  jObject.Strings['m'] := Edit1.Text;
  try
    result := Dispatcher.trigger('js:mqtt.sendMessage', TObject(vComp));
  finally
    vComp.free;
  end;
  if result then
    Edit1.Text:= ''
  else
    ShowMessage('Failed to send message');
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  fDispatcher.Destroy;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin

end;

end.

