unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids,
  ntvGrids, ntvImgBtns, ntvPanels;

type

  { TForm1 }

  TForm1 = class(TForm)
    CloseBtn: TButton;
    ntvPanel1: TntvPanel;
    procedure CloseBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public
    Grid: TntvGrid;
    NameCol: TntvColumn;
    AddressCol: TntvColumn;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Grid:=TntvGrid.Create(Self);
  Grid.Parent := Self;
  Grid.SetBounds(10, 10 ,ClientWidth - 20, CloseBtn.Top - 10);
  Grid.Anchors := [akLeft, akRight, akTop, akBottom];
  Grid.Capacity := 10000;
  NameCol := TntvStandardColumn.Create(Grid.Columns, 'NameCol');
  NameCol.Visible := True;
  AddressCol := TntvStandardColumn.Create(Grid.Columns, 'AddressCol');
  AddressCol.Visible := True;
  Grid.TabOrder := 0;
  Grid.ActiveRow := 0;
  NameCol.AsString := 'zaher';
  AddressCol.AsString := 'syria';
  Grid.Visible := True;
  ActiveControl := Grid;
end;

end.

