unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ntvGrids;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ntvGrid1: TntvGrid;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    procedure ListStyle;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  i: Integer;
begin
  ListStyle;
  for i := 0 to 15 do
    ntvGrid1.AddItem('test' + IntToStr(i));
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  i: Integer;
begin
  ListStyle;
  ntvGrid1.Columns.Count := 2;
  for i := 0 to 15 do
    ntvGrid1.AddItem(['foo' + IntToStr(i), 'boo' + IntToStr(i)]);
end;

procedure TForm1.ListStyle;
begin
  ntvGrid1.Rows.Count := 0;
  ntvGrid1.Capacity := 0;
  ntvGrid1.Columns.Count := 1;
  ntvGrid1.Columns[0].AutoFit := True;
  ntvGrid1.Gutter := False;
  ntvGrid1.Header := False;
  ntvGrid1.ScrollBars := ssAutoVertical;
  ntvGrid1.DualColor := False;
  ntvGrid1.GridLines := glNone;
  ntvGrid1.RowSelect := True;
  ntvGrid1.RowRefresh := True;
  ntvGrid1.Color := ntvGrid1.EvenColor;
end;

end.

