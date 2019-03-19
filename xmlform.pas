unit xmlform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Menus, Buttons, Controls,
  ComCtrls, StdCtrls,
  LazUTF8, Laz_XMLStreaming, Laz2_DOM, Laz2_XMLCfg;

Type

{ TXMLForm }

TXMLForm = class(TForm)
  procedure FormCreate(Sender: TObject);
private
  FFilename: string;
  procedure SetFilename(const AValue: string);
public
  procedure WriteComponents;
  procedure ReadComponents;
  procedure OnFindComponentClass({%H-}Reader: TReader; const AClassName: string;
                                 var ComponentClass: TComponentClass);
  property Filename: string read FFilename write SetFilename;
end;

var
  XMLForm1, XMLForm2: TXMLForm;

function CreateXMLWriter(ADoc: TDOMDocument; const Path: string;
  Append: Boolean; var DestroyDriver: boolean): TWriter;
function CreateXMLReader(ADoc: TDOMDocument; const Path: string;
  var DestroyDriver: boolean): TReader;

procedure WriteComponentToXMLConfig(XMLConfig: TXMLConfig; const Path: string;
  AComponent: TComponent);
procedure ReadComponentFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
  var RootComponent: TComponent;
  OnFindComponentClass: TFindComponentClassEvent; TheOwner: TComponent);

implementation

{$R *.lfm}

{ TXMLForm }

procedure TXMLForm.FormCreate(Sender: TObject);
begin

end;

procedure TXMLForm.SetFilename(const AValue: string);
begin
  if FFilename=AValue then exit;
  FFilename:=AValue;
end;

procedure TXMLForm.WriteComponents;
var
  XMLConfig: TXMLConfig;
  sl: TStringList;
begin
  //DebugLn('TXMLForm.WriteComponents ',Filename);
  XMLConfig:=TXMLConfig.Create(Filename);
  try
    WriteComponentToXMLConfig(XMLConfig,'xmlcomp',Self);
    {
    WriteComponentToXMLConfig(XMLConfig,'Component',MyComponent);
    WriteComponentToXMLConfig(XMLConfig,'Component',DemoGroupBox);
    }
    XMLConfig.Flush;
  finally
    XMLConfig.Free;
  end;

  sl:=TStringList.Create;
  sl.LoadFromFile(UTF8ToSys(Filename));
  //DebugLn('TXMLForm.WriteComponents ',sl.Text);
  sl.Free;
end;

procedure TXMLForm.ReadComponents;
var
  XMLConfig: TXMLConfig;
  sl: TStringList;
  NewComponent: TComponent;
begin
  //DebugLn('TXMLForm.ReadComponents ',Filename);
  XMLConfig:=TXMLConfig.Create(Filename);
  try
    NewComponent:=nil;
    ReadComponentFromXMLConfig(XMLConfig,'xmlcomp',NewComponent,
      @OnFindComponentClass, {DestinationGroupBox} Self);
    //if NewComponent is TMyComponent then
    //  TMyComponent(NewComponent).WriteDebugReport;
    //if NewComponent is TControl then
    //  TControl(NewComponent).Parent:= {DestinationGroupBox} Self;
    XMLConfig.Flush;
  finally
    XMLConfig.Free;
  end;

  sl:=TStringList.Create;
  sl.LoadFromFile(UTF8ToSys(Filename));
  //DebugLn('TXMLForm.StreamComponents ',sl.Text);
  sl.Free;
end;

procedure TXMLForm.OnFindComponentClass(Reader: TReader;
  const AClassName: string; var ComponentClass: TComponentClass);
begin
  // most from standard tab lcl components http://wiki.freepascal.org/LCL_Components#Standard_tab
  if CompareText(AClassName,'TXMLForm') = 0 then
    ComponentClass := TXMLForm
  else if CompareText(AClassName,'TMainMenu') = 0 then
    ComponentClass := TMainMenu
  else if CompareText(AClassName,'TPopupMenu') = 0 then
    ComponentClass := TPopupMenu
  else if CompareText(AClassName,'TButton') = 0 then
    ComponentClass := TButton
  else if CompareText(AClassName,'TLabel') = 0 then
    ComponentClass := TLabel
  else if CompareText(AClassName,'TEdit') = 0 then
    ComponentClass := TEdit
  else if CompareText(AClassName,'TMemo') = 0 then
    ComponentClass := TMemo
  else if CompareText(AClassName,'TToggleBox') = 0 then
    ComponentClass := TToggleBox
  else if CompareText(AClassName,'TCheckBox') = 0 then
    ComponentClass := TCheckBox
  else if CompareText(AClassName,'TRadioButton') = 0 then
    ComponentClass := TRadioButton
  else if CompareText(AClassName,'TListBox') = 0 then
    ComponentClass := TListBox
  else if CompareText(AClassName,'TComboBox') = 0 then
    ComponentClass := TComboBox
  else if CompareText(AClassName,'TScrollBar') = 0 then
    ComponentClass := TScrollBar
  else if CompareText(AClassName,'TGroupBox') = 0 then
    ComponentClass := TGroupBox
  else if CompareText(AClassName,'TFrame') = 0 then
    ComponentClass := TFrame
  //else if CompareText(AClassName,'TMyComponent')=0 then
  //  ComponentClass:=TMyComponent;
  //else if CompareText(AClassName,'TMyGroupBox')=0 then
  //  ComponentClass:=TMyGroupBox;
  //DebugLn('TXMLForm.OnFindComponentClass ',AClassName,' ',dbgs(ComponentClass));
end;

{ XML helper methods }

function CreateXMLWriter(ADoc: TDOMDocument; const Path: string;
  Append: Boolean; var DestroyDriver: boolean): TWriter;
var
  Driver: TAbstractObjectWriter;
begin
  Driver:=TXMLObjectWriter.Create(ADoc,Path,Append);
  DestroyDriver:=true;
  Result:=TWriter.Create(Driver);
end;

function CreateXMLReader(ADoc: TDOMDocument; const Path: string;
  var DestroyDriver: boolean): TReader;
var
  p: Pointer;
  Driver: TAbstractObjectReader;
  DummyStream: TMemoryStream;
begin
  DummyStream:=TMemoryStream.Create;
  try
    Result:=TReader.Create(DummyStream,256);
    DestroyDriver:=false;
    // hack to set a write protected variable.
    // DestroyDriver:=true; TReader will free it
    Driver:=TXMLObjectReader.Create(ADoc,Path);
    p:=@Result.Driver;
    Result.Driver.Free;
    TAbstractObjectReader(p^):=Driver;
  finally
    DummyStream.Free;
  end;
end;

procedure WriteComponentToXMLConfig(XMLConfig: TXMLConfig; const Path: string;
  AComponent: TComponent);
var
  Writer: TWriter;
  DestroyDriver: boolean;
begin
  Writer:=nil;
  DestroyDriver:=false;
  try
    Writer:=CreateXMLWriter(XMLConfig.Document,Path,false,DestroyDriver);
    XMLConfig.Modified:=true;
    Writer.WriteRootComponent(AComponent);
    XMLConfig.Flush;
  finally
    if DestroyDriver and (Writer<>nil) then
      Writer.Driver.Free;
    Writer.Free;
  end;
end;

procedure ReadComponentFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
  var RootComponent: TComponent;
  OnFindComponentClass: TFindComponentClassEvent; TheOwner: TComponent);
var
  DestroyDriver: Boolean;
  Reader: TReader;
  IsInherited: Boolean;
  AClassName: String;
  AClass: TComponentClass;
begin
  Reader:=nil;
  DestroyDriver:=false;
  try
    Reader:=CreateXMLReader(XMLConfig.Document,Path,DestroyDriver);
    Reader.OnFindComponentClass:=OnFindComponentClass;

    // get root class
    AClassName:=(Reader.Driver as TXMLObjectReader).GetRootClassName(IsInherited);
    if IsInherited then begin
      // inherited is not supported by this simple function
      //DebugLn('ReadComponentFromXMLConfig WARNING: "inherited" is not supported by this simple function');
    end;
    AClass:=nil;
    OnFindComponentClass(nil,AClassName,AClass);
    if AClass=nil then
      raise EClassNotFound.CreateFmt('Class "%s" not found', [AClassName]);

    if RootComponent=nil then begin
      // create root component
      // first create the new instance and set the variable ...
      RootComponent:=AClass.NewInstance as TComponent;
      // then call the constructor
      RootComponent.Create(TheOwner);
    end else begin
      // there is a root component, check if class is compatible
      if not RootComponent.InheritsFrom(AClass) then begin
        raise EComponentError.CreateFmt('Cannot assign a %s to a %s.',
                                        [AClassName,RootComponent.ClassName]);
      end;
    end;

    Reader.ReadRootComponent(RootComponent);
  finally
    if DestroyDriver then
      Reader.Driver.Free;
    Reader.Free;
  end;
end;

end.

