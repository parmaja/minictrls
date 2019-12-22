unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  FPimage, ntvPixelGrids;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public
    Grid: TntvPixelGrid;
    constructor Create(TheOwner: TComponent); override;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Grid.Dots.Width := 16;
  Grid.Dots.Height := 16;
  Grid.Dots.Clear;
  Grid.Dots.Canvas.Pen.Color := clRed;
  Grid.Dots.Canvas.Font.Assign(Grid.Font);
  Grid.Dots.Canvas.Font.Color := clGreen;
  Grid.Dots.Canvas.TextOut(0, 0, 'Z');
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Grid.Dots.Canvas.DrawPixel(2, 2, FPColor(255,0,0,150));
  Grid.Dots.SaveToFile(Application.Location+'test.png');

end;

constructor TForm1.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Grid := TntvPixelGrid.Create(Self);
  with Grid do
  begin
    Parent := Panel1;
    Align := alClient;
    Visible := True;
    Font.Size := 8;
    Font.Quality := fqNonAntialiased;
  end;
end;

end.

