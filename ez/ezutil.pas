unit ezutil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, FPTimer;

type
  { TWriteDebug }
  TWriteDebug = function(const S: string): integer of object;

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


function ObjectToTag(aObject: TObject): PtrInt;
function TagToObject(aTag: PtrInt): TObject;
function JSONToTag(aJSON: TJSONObject): PtrInt;
function TagToJSON(aTag: PtrInt): TJSONObject;

function NewTimer(Intr: integer; Proc: TNotifyEvent; AEnable: boolean = false): TFPTimer;

implementation

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

function NewTimer(Intr: integer; Proc: TNotifyEvent; AEnable: boolean
  ): TFPTimer;
begin
  Result := TFPTimer.Create(nil);
  Result.UseTimerThread:=false;
  Result.Interval := Intr;
  Result.OnTimer := Proc;
  Result.Enabled := AEnable;
end;

end.

