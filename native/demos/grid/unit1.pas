unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids, Menus,
  DBGrids, ntvGrids;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    CloseBtn: TButton;
    ComboBox1: TComboBox;
    Label1: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    PopupMenu1: TPopupMenu;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ntvGrid1Click(Sender: TObject);
  private
    procedure OnColClick(Sender: TntvCustomGrid; Column: TntvColumn);
  public
    Grid: TntvGrid;
    IDCol: TntvColumn;
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

procedure TForm1.Button3Click(Sender: TObject);
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
  Grid.FullHeader := true;
  Grid.AnchorCols := 2;
  Grid.BorderStyle := bsSingle;
  Grid.FixedCols := 1;

  //Grid.RowHeight := 47;
  IDCol := TntvStandardColumn.Create(Grid, 'ID');
  NameCol := TntvStandardColumn.Create(Grid, 'Name');
  PhoneCol := TntvStandardColumn.Create(Grid, 'Phone');
  MobileCol := TntvStandardColumn.Create(Grid, 'Mobile');
  EmailCol := TntvStandardColumn.Create(Grid, 'Email');
  EmailCol.Hint := 'example: email@domain.com';
  PayCol := TntvStandardColumn.Create(Grid, 'Pay');
  PayCol.IsTotal := True;
  //PayCol.AutoSize := True;
  AddressCol := TntvStandardColumn.Create(Grid, 'Address');
  Grid.TabOrder := 0;

  Grid.ActiveRow := 0;
  IDCol.AsString := '101';

  NameCol.AsString := 'zaher';
  AddressCol.AsString := 'syria';

  MobileCol.OrderIndex := 3;
  MobileCol.Width := 100;
  MobileCol.AutoFit := True;

  EmailCol.Visible := False;

  Grid.Visible := True;
  ActiveControl := Grid;
end;

procedure TForm1.Button4Click(Sender: TObject);
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
  Grid.FullHeader := true;
  Grid.AnchorCols := 1;
  Grid.BorderStyle := bsSingle;
  Grid.ColumnsCount := 3;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
end;

procedure TForm1.ntvGrid1Click(Sender: TObject);
begin

end;

procedure TForm1.OnColClick(Sender: TntvCustomGrid; Column: TntvColumn);
begin
  Grid.Values[Column.Index, 0] := 'Clicked';
end;

end.

