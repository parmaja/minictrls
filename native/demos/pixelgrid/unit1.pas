unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ColorBox, ComCtrls, FPimage, FPCanvas, ntvPixelGrids;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button4: TButton;
    Button5: TButton;
    ColorListBox1: TColorListBox;
    Panel1: TPanel;
    AlphaTrack: TTrackBar;
    procedure AlphaTrackChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure ColorListBox1SelectionChange(Sender: TObject; User: boolean);
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
  Grid.Dots.DotSize := 4;
  Grid.Dots.Width := 16;
  Grid.Dots.Height := 16;
  Grid.Dots.Clear;
  Grid.Dots.DrawText(1, 10, 'a', clGreen);
end;

procedure TForm1.AlphaTrackChange(Sender: TObject);
begin
  Grid.CurrentAlpha := AlphaTrack.Position;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Grid.Dots.DrawPixel(2, 2, Grid.CurrentColor, 255);
  Grid.Dots.DrawPixel(2, 3, Grid.CurrentColor, 150);
  Grid.Dots.SaveToFile(Application.Location+'test.png');
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  Grid.Dots.DrawPixel(2, 3, Grid.CurrentColor, 150);
  Grid.Dots.DrawPixel(2, 4, Grid.CurrentColor, 255);
  Grid.Dots.SaveToFile(Application.Location+'test.png');
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  Grid.Dots.FloodFill(5, 5, clRed, 128);
end;

procedure TForm1.ColorListBox1SelectionChange(Sender: TObject; User: boolean);
begin
  Grid.CurrentColor := ColorListBox1.Selected;
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
//    Dots.FontName := Application.Location + 'terminus.ttf';
  end;
end;

end.

