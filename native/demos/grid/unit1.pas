unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids,
  ntvGrids, ntvImgBtns, ntvPanels;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    CloseBtn: TButton;
    procedure Button1Click(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ntvGrid1Click(Sender: TObject);
  private

  public
    Grid: TntvGrid;
    NameCol: TntvColumn;
    PhoneCol: TntvColumn;
    EmailCol: TntvColumn;
    MobileCol: TntvColumn;
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

procedure TForm1.Button1Click(Sender: TObject);
begin
  Grid[5, 0] := 'test1';

  Grid.BeginUpdate;
  try
    Grid.Rows[6][0].Text := 'test2';
  finally
    Grid.EndUpdate;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Grid:=TntvGrid.Create(Self);
  Grid.Parent := Self;
  Grid.SetBounds(10, 10 ,ClientWidth - 20, CloseBtn.Top - 10);
  Grid.Anchors := [akLeft, akRight, akTop, akBottom];
  Grid.Capacity := 3;
  Grid.RowHeight := 47;
  NameCol := TntvStandardColumn.Create(Grid.Columns, 'Name');
  AddressCol := TntvStandardColumn.Create(Grid.Columns, 'Address');
  PhoneCol := TntvStandardColumn.Create(Grid.Columns, 'Phone');
  EmailCol := TntvStandardColumn.Create(Grid.Columns, 'Email');
  MobileCol := TntvStandardColumn.Create(Grid.Columns, 'Mobile');
  Grid.TabOrder := 0;
  Grid.ActiveRow := 0;
  NameCol.AsString := 'zaher';
  AddressCol.AsString := 'syria';
  Grid.SettledCols := 1;
  Grid.Visible := True;
  ActiveControl := Grid;
end;

procedure TForm1.ntvGrid1Click(Sender: TObject);
begin

end;

end.

