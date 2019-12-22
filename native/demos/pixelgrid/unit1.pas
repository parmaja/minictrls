unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  FPimage, FPCanvas, ntvPixelGrids;

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
  Grid.Dots.Canvas.DrawingMode := dmOpaque;
  Grid.Dots.Canvas.Pen.Color := clFuchsia;
  Grid.Dots.Canvas.DrawPixel(2, 2, FPColor(255, 255, 0, 150));
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
{
uses
  GraphType, FpImage, intfGraphics, EasyLazFreeType, LazFreeTypeIntfDrawer;

procedure TForm1.Button1Click(Sender: TObject);
var
  png: TPortableNetworkGraphic;
  img: TLazIntfImage;
  drw: TIntfFreeTypeDrawer;
  fnt: TFreeTypeFont;
begin
  png := TPortableNetworkGraphic.Create;
  try
    img := TLazIntfImage.Create(0,0, [riqfRGB, riqfAlpha]);
    try
      img.SetSize(400, 200);
      drw := TIntfFreeTypeDrawer.Create(img);
      fnt := TFreeTypeFont.Create;
      try
        fnt.Name := 'c:\Windows\fonts\Arial.ttf';
        fnt.SizeInPixels := 36;
        fnt.Hinted := true;
        fnt.ClearType := true;
        fnt.Quality := grqHighQuality;

        // Transparent background
        drw.FillPixels(colTransparent);
        // Yellow rectangle on transparent background
        drw.FillRect(30, 30, img.Width-30, img.Height-30, colYellow);
        // Text, partially on transparent background, partially on yellow rectangle
        drw.DrawText('Lazarus', fnt, 0, 100, colRed);

        png.LoadFromIntfImage(img);
        png.saveToFile('d:\test.png');
      finally
        fnt.Free;
        drw.Free;
      end;
    finally
      img.Free;
    end;
  finally
    png.Free;
  end;
end;
}
end.

