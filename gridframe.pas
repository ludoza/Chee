unit gridframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, Buttons, StdCtrls, Grids,
  ActnList;

type

  { TfrmGrid }

  TfrmGrid = class(TFrame)
    Load: TAction;
    ActionList: TActionList;
    BitBtn1: TBitBtn;
    edtUri: TEdit;
    ImageList: TImageList;
    StringGrid1: TStringGrid;
    ToolBar1: TToolBar;
  private

  public

  end;

implementation

{$R *.lfm}

end.

