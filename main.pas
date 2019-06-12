unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

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
  end;

  { TMainForm }

  TMainForm = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
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

function TDispatcher.trigger(aDisplayName: string): Boolean;
var
  i : Integer;
  vItem: TDispatcherItem;
begin
  Result := False;
  for i:= 0 to self.Count -1 do
  begin
    vItem := TDispatcherItem(self.Items[i]);
    if aDisplayName = vItem.DisplayName then
    if vItem.Action.Execute then
    begin
      Result := True;
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
begin
  if Dispatcher.trigger('js:mqtt.sendMessage') then
    Edit1.Text:= 'Sent'
  else
        Edit1.Text:= 'Failed';
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  fDispatcher.Destroy;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin

end;

end.

