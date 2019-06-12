unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  { TDispatcherItem }
  TDispatcherItem = class(TCollectionItem)
  private
    fAction: TBasicAction;
  public
    property Action: TBasicAction read fAction write fAction;
  end;

  { TDispatcher }
  TDispatcher = class(TCollection)
  public
    constructor Create(AItemClass: TCollectionItemClass);
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

{ TDispatcher }

constructor TDispatcher.Create(AItemClass: TCollectionItemClass);
begin
  inherited Create(AItemClass);

end;

{ TDispatcher }


{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);


begin
  fDispatcher := TDispatcher.Create(TDispatcherItem);
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  i : Integer;
  vItem: TDispatcherItem;
begin
  for i:= 0 to Dispatcher.Count -1 do
  begin
    vItem := TDispatcherItem(Dispatcher.Items[i]);
    if vItem.Action.Execute then
    begin

    end;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  fDispatcher.Destroy;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin

end;

end.

