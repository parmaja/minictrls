unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids, Menus,
  ntvGrids, ntvImgBtns, ntvPanels;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CloseBtn: TButton;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    PopupMenu1: TPopupMenu;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ntvGrid1Click(Sender: TObject);
  private
    procedure OnColClick(Sender: TntvCustomGrid; vCol: Integer);
  public
    Grid: TntvGrid;
    NameCol: TntvColumn;
    PhoneCol: TntvColumn;
    EmailCol: TntvColumn;
    MobileCol: TntvColumn;
    PayCol: TntvColumn;
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

procedure TForm1.Button2Click(Sender: TObject);
begin
  if BiDiMode = bdLeftToRight then
    BiDiMode := bdRightToLeft
  else
    BiDiMode := bdLeftToRight
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Grid := TntvGrid.Create(Self);
  Grid.Parent := Self;
  Grid.SetBounds(10, 10 ,ClientWidth - 20, CloseBtn.Top - 10);
  Grid.Anchors := [akLeft, akRight, akTop, akBottom];
  Grid.Capacity := 3;
  Grid.OnColClick := @OnColClick;
  Grid.Footer := True;
  Grid.Fringe := True;
  Grid.PopupMenu := PopupMenu1;

  //Grid.RowHeight := 47;
  NameCol := TntvStandardColumn.Create(Grid.Columns, 'Name');
  PhoneCol := TntvStandardColumn.Create(Grid.Columns, 'Phone');
  MobileCol := TntvStandardColumn.Create(Grid.Columns, 'Mobile');
  EmailCol := TntvStandardColumn.Create(Grid.Columns, 'Email');
  EmailCol.Hint := 'example: email@domain.com';
  PayCol := TntvStandardColumn.Create(Grid.Columns, 'Pay');
  PayCol.IsTotal := True;
  //PayCol.AutoSize := True;
  AddressCol := TntvStandardColumn.Create(Grid.Columns, 'Address');
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

procedure TForm1.OnColClick(Sender: TntvCustomGrid; vCol: Integer);
begin
  Grid[0, vCol] := 'Clicked';
end;

end.

