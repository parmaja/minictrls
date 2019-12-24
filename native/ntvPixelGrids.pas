unit ntvPixelGrids;
{**
 *  This file is part of the "Mini Library"
 *
 * @url       http://www.sourceforge.net/projects/minilib
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *
 *
 * Thanks to
 *
 * https://sites.google.com/a/gorilla3d.com/fpc-docs/built-in-units/fcl-image/filling-the-circle
 * https://forum.lazarus.freepascal.org/index.php?topic=35424.msg234045
 *
 *}

{$mode objfpc}{$H+}

interface

uses
  LMessages, SysUtils, Classes, Graphics, Controls, Variants, Types,
  LCLIntf, LCLType, IntfGraphics,
  FPImage, FPCanvas, FPImgCanv, GraphType, EasyLazFreeType, LazFreeTypeIntfDrawer,
  mnUtils;

const
  cDotSize = 10;
  cDotMargin = 1;

  cForeColor = clBlack;
  cBackColor = clSilver;

  colWhiteTransparent: TFPColor = (Red: $ffff; Green: $ffff; Blue: $ffff; Alpha: alphaTransparent);
  colFuchsiaTransparent: TFPColor = (Red: $ffff; Green: $0000; Blue: $ffff; Alpha: alphaTransparent);

type
  TImageClass = TPortableNetworkGraphic;

  TPixelGridInfo = record
    BackColor: TColor;
    DotSize: Integer;
    DotMargin: Integer;
  end;

  { TntvDisplayDots }

  TntvDisplayDots = class(TPersistent)
  private
    FImage: TLazIntfImage;
    FDrawer: TIntfFreeTypeDrawer;
    FCanvas: TFPImageCanvas;
    FFont: TFreeTypeFont;
    FOffsetX: Integer;
    FOffsetY: Integer;
    FUpdateCount: Integer;

    procedure CanvasChanged(Sender: TObject);
    function GetFontName: string;

    function GetHeight: Integer;
    function GetPixel(x, y: integer): TColor;
    function GetWidth: Integer;
    procedure SetDotMargin(const AValue: Integer);
    procedure SetDotSize(const AValue: Integer);
    procedure SetFontName(AValue: string);
    procedure SetWidth(const AValue: Integer);
    procedure SetHeight(const AValue: Integer);
    procedure SetOffsetX(const AValue: Integer);
    procedure SetOffsetY(const AValue: Integer);
    function GetUpdating: Boolean;
    procedure SetPixel(x, y: integer; const AValue: TColor);
    procedure SetBackColor(const AValue: TColor);
  protected
    Matrix: TPixelGridInfo;

    procedure Changed;
    procedure Invalidate;
    procedure DoInvalidate; virtual;

    procedure PushHistory;
    procedure PopHistory;
    function VisualToIndex(x, y: Integer; out col, row: Integer): Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Paint(vCanvas: TCanvas; vRect: TRect);
    //y here is the base line of text, bottom of text
    procedure DrawText(x, y: Integer; AText: string; AColor: TColor);
    procedure DrawPixel(x, y: Integer; AColor: TColor; Alpha: Byte = 100);
    procedure FloodFill(x, y: Integer; AColor: TColor; Alpha: Byte = 100);
    procedure SetSize(const AWidth, AHeight: Integer);
    procedure SaveToFile(FileName: string);
    procedure LoadFromFile(FileName: string);
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Clear; virtual;
    procedure Reset; virtual;
    procedure Scroll(x, y: Integer);
    procedure Assign(Source: TPersistent); override;
    property Updating: Boolean read GetUpdating;
    property Image: TLazIntfImage read FImage;

    property Pixels[x, y:integer] : TColor read GetPixel write SetPixel;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    //Scroll the dots
    property OffsetX: Integer read FOffsetX write SetOffsetX;
    property OffsetY: Integer read FOffsetY write SetOffsetY;
  published
    property BackColor: TColor read Matrix.BackColor write SetBackColor default cBackColor;
    property DotSize: Integer read Matrix.DotSize write SetDotSize default cDotSize;
    property DotMargin: Integer read Matrix.DotMargin write SetDotMargin default 1;
    property FontName: string read GetFontName write SetFontName;
  end;


  TntvPixelGrid = class;

  { TntvGridDisplayDots }

  TntvGridDisplayDots = class(TntvDisplayDots)
  private
    FControl: TntvPixelGrid;
  protected
    procedure DoInvalidate; override;
  end;

  TCurrentAction = (actPixel, actMerge, actFlood, actRectangle);

  { TntvPixelGrid }

  TntvPixelGrid = class(TCustomControl)
  private
    FCurrentAction: TCurrentAction;
    FDots: TntvDisplayDots;
    procedure SetCurrentAction(AValue: TCurrentAction);
  protected
    StartPoint: TPoint;
    FCurrentColor: TColor;
    FCurrentAlpha: Byte;
    procedure Action(APosition: TPoint);
    procedure FontChanged(Sender: TObject); override;
    procedure Paint; override;
    procedure CMBidModeChanged(var Message: TLMessage); message CM_BIDIMODECHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;
    procedure Resize; override;
    procedure Loaded; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure BeginUpdate;
    procedure EndUpdate;
    property Dots: TntvDisplayDots read FDots;
    property CurrentAction: TCurrentAction read FCurrentAction write SetCurrentAction;
    property CurrentColor: TColor read FCurrentColor write FCurrentColor;
    property CurrentAlpha: Byte read FCurrentAlpha write FCurrentAlpha;
  published
    property Align;
    property Anchors;
    property Color default cBackColor;
    property BidiMode;
    property Font;
    property ParentBidiMode;
    property ParentFont;
    property BorderWidth;
    property BorderStyle;
  end;

implementation

procedure DrawDot(Canvas: TCanvas; x: integer; y: integer; PixelColor: TColor; vInfo: TPixelGridInfo);
var
  R: TRect;
begin
  R.Left := x;
  R.Top := y;

  R.Right := x + vInfo.DotSize;
  R.Bottom := Y + vInfo.DotSize;
  Canvas.Brush.Color := PixelColor;
  Canvas.FillRect(R);
end;

{ TntvGridDisplayDots }

procedure TntvGridDisplayDots.DoInvalidate;
begin
  inherited DoInvalidate;
  if FControl <> nil then
    FControl.Invalidate;
end;

function TntvDisplayDots.GetFontName: string;
begin
  Result := FFont.Name;
end;

{ TntvPixelGrid }

constructor TntvPixelGrid.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csCaptureMouse, csDoubleClicks] - [csOpaque];
  FDots := TntvGridDisplayDots.Create;
  (FDots as TntvGridDisplayDots).FControl:= Self;
  Color := cBackColor;
  FCurrentColor := clBlack;
  FCurrentAlpha := 255;
end;

destructor TntvPixelGrid.Destroy;
begin
  FreeAndNil(FDots);
  inherited;
end;

procedure TntvPixelGrid.SetCurrentAction(AValue: TCurrentAction);
begin
  if FCurrentAction = AValue then
    Exit;
  FCurrentAction :=AValue;
  Invalidate;
end;

procedure TntvPixelGrid.Action(APosition: TPoint);
var
  x, y: Integer;
begin
  if Dots.VisualToIndex(APosition.x, APosition.y, x, y) then
  begin
    case FCurrentAction of
      actPixel:
      begin
        Dots.DrawPixel(x, y, CurrentColor, CurrentAlpha);
      end;
      actFlood:
      begin
        Dots.FloodFill(x, y, CurrentColor, CurrentAlpha);
      end;
      actRectangle:
      begin
      end;
    end;
  end;
end;

procedure TntvPixelGrid.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);

end;

procedure TntvPixelGrid.Paint;
begin
  inherited;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);
  Dots.Paint(Canvas, ClientRect);
end;

procedure TntvPixelGrid.EraseBackground(DC: HDC);
begin
  //To reduce the flicker do not inherite
end;

procedure TntvPixelGrid.CMBidModeChanged(var Message: TLMessage);
begin
  BeginUpdate;
  try
    Inherited;
  finally
    EndUpdate;
  end;
end;

procedure TntvPixelGrid.Resize;
begin
  inherited;
  if not (csLoading in ComponentState) then
  begin
  end;
end;

procedure TntvPixelGrid.Loaded;
begin
  inherited Loaded;
end;

procedure TntvPixelGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if MouseCapture then
    Action(Point(x, y));
end;

procedure TntvPixelGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  StartPoint := Point(x, y);
  Action(StartPoint);
end;

procedure TntvPixelGrid.BeginUpdate;
begin
  Dots.BeginUpdate;
end;

procedure TntvPixelGrid.EndUpdate;
begin
  Dots.EndUpdate;
end;

{ TntvDisplayDots }

procedure TntvDisplayDots.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TntvDisplayDots.Paint(vCanvas: TCanvas; vRect: TRect);
var
  x, y: integer;
  ix, iy: integer;
  ox, oy: integer;
  aPixelColor: TColor;
begin
  y := vRect.Top;
  iy := 0;
  while iy < Image.Height do
  begin
    x := vRect.Left;
    ix := 0;
    while ix < Width do
    begin
      ox := ix + OffsetX;
      oy := iy + OffsetY;

      if (ox < Image.Width) and (oy < Image.Height) then
        aPixelColor := Image.TColors[ox, oy]
      else
        aPixelColor := BackColor;

      DrawDot(vCanvas, x, y, aPixelColor, Matrix);
      x := x + (Matrix.DotSize + Matrix.DotMargin);
      Inc(ix);
    end;
    Inc(iy);
    y := y + (Matrix.DotSize + Matrix.DotMargin);
  end;
end;

procedure TntvDisplayDots.DrawText(x, y: Integer; AText: string; AColor: TColor);
begin
  FDrawer.DrawText(AText, FFont, x, y, TColorToFPColor(AColor));
  Changed;
  Invalidate;
end;

procedure TntvDisplayDots.DrawPixel(x, y: Integer; AColor: TColor; Alpha: Byte);
var
  c: TFPColor;
begin
  c := TColorToFPColor(AColor);
  c.Alpha := MAXWORD * Alpha div 255;
  FDrawer.UnClippedDrawPixel(x, y, c);
  //FCanvas.FloodFill()
  Changed;
  Invalidate;
end;

procedure TntvDisplayDots.FloodFill(x, y: Integer; AColor: TColor; Alpha: Byte);
var
  c: TFPColor;
begin
  c := TColorToFPColor(AColor);
  c.Alpha := MAXWORD * Alpha div 255;
  FCanvas.Brush.FPColor := c;
  FCanvas.FloodFill(x, y);
  Changed;
  Invalidate;
end;

procedure TntvDisplayDots.SetSize(const AWidth, AHeight: Integer);
begin
  if (Width <> AWidth) or (Height <> AHeight) then
  begin
    FImage.SetSize(AWidth, AHeight);
    Invalidate;
  end;
end;

procedure TntvDisplayDots.SaveToFile(FileName: string);
var
  png: TPortableNetworkGraphic;
begin
  png := TPortableNetworkGraphic.Create;
  try
    png.LoadFromIntfImage(FImage);
    png.Transparent := True;
    //png.TransparentColor := clFuchsia;
    //png.PixelFormat := pf32bit;
    png.SaveToFile(FileName);
  finally
    png.Free;
  end;
end;

procedure TntvDisplayDots.LoadFromFile(FileName: string);
var
  png: TPortableNetworkGraphic;
begin
  png := TPortableNetworkGraphic.Create;
  try
    png.LoadFromFile(FileName);
    FImage.LoadFromBitmap(png.Handle, png.MaskHandle);
  finally
    png.Free;
  end;
end;

procedure TntvDisplayDots.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if (FUpdateCount = 0) then
      Invalidate;
  end;
end;

procedure TntvDisplayDots.Clear;
begin
  FDrawer.FillPixels(colWhiteTransparent);
  Invalidate;
end;

procedure TntvDisplayDots.Reset;
begin
  BeginUpdate;
  try
    OffsetX := 0;
    OffsetY := 0;
    Clear;
  finally
    EndUpdate;
  end;
end;

procedure TntvDisplayDots.Scroll(x, y: Integer);
begin
  BeginUpdate;
  try
    FOffsetX := FOffsetX + x;
    if Abs(FOffsetX) >= Width - 1 then
      FOffsetX := 0;
    FOffsety := FOffsety + y;
    if Abs(FOffsetY) >= Height - 1 then
      FOffsetY := 0;
  finally
    EndUpdate;
  end;
end;

procedure TntvDisplayDots.Assign(Source: TPersistent);
begin
  if Source is TntvDisplayDots then
  begin
  end
  else
    inherited;
end;

function TntvDisplayDots.GetUpdating: Boolean;
begin
  Result := FUpdateCount > 0;
end;

procedure TntvDisplayDots.Invalidate;
begin
  if not Updating then
    DoInvalidate;
end;

procedure TntvDisplayDots.DoInvalidate;
begin
end;

procedure TntvDisplayDots.PushHistory;
begin
  FImage.GetRawImage()
end;

procedure TntvDisplayDots.PopHistory;
begin

end;

function TntvDisplayDots.VisualToIndex(x, y: Integer; out col, row: Integer): Boolean;
var
  aSize: Integer;
begin
  if (x >= 0) and (y >= 0) then
  begin
    aSize := Matrix.DotSize + Matrix.DotMargin;
    col := (x + aSize) div aSize - 1;
    row := (y + aSize) div aSize - 1;
    Result := (col < Width) and (row < Height);
  end
  else
    Result := False;
end;

procedure TntvDisplayDots.SetDotMargin(const AValue: Integer);
begin
  if Matrix.DotMargin <> AValue then
  begin
    Matrix.DotMargin := AValue;
    Changed;
    Invalidate;
  end;
end;

procedure TntvDisplayDots.CanvasChanged(Sender: TObject);
begin
  Changed;
  Invalidate;
end;


function TntvDisplayDots.GetHeight: Integer;
begin
  Result := FImage.Height;
end;

function TntvDisplayDots.GetPixel(x, y: integer): TColor;
begin
  Result := Image.TColors[x, y];
end;

function TntvDisplayDots.GetWidth: Integer;
begin
  Result := FImage.Width;
end;

procedure TntvDisplayDots.SetHeight(const AValue: Integer);
begin
  if FImage.Height <> AValue then
  begin
    SetSize(Width, AValue);
  end;
end;

procedure TntvDisplayDots.SetOffsetX(const AValue: Integer);
begin
  if FOffsetX <> AValue then
  begin
    FOffsetX := AValue;
    Invalidate;
  end;
end;

procedure TntvDisplayDots.SetOffsetY(const AValue: Integer);
begin
  if FOffsetY <> AValue then
  begin
    FOffsetY := AValue;
    Invalidate;
  end;
end;

procedure TntvDisplayDots.SetPixel(x, y: integer; const AValue: TColor);
begin
  Image.TColors[x, y] := AValue;
  Invalidate;
end;

procedure TntvDisplayDots.SetWidth(const AValue: Integer);
begin
  if FImage.Width <> AValue then
  begin
    SetSize(AValue, Height);
  end;
end;

constructor TntvDisplayDots.Create;
begin
  inherited;
  Matrix.BackColor := cBackColor;
  Matrix.DotSize := cDotSize;
  Matrix.DotMargin := 1;

  FImage := TLazIntfImage.Create(10, 10, [riqfRGB, riqfPalette, riqfAlpha]);
  FDrawer := TIntfFreeTypeDrawer.Create(FImage);
  FCanvas := TFPImageCanvas.Create(FImage);
  FFont := TFreeTypeFont.Create;
  FFont.Name := 'c:\Windows\fonts\Arial.ttf';
  FFont.SizeInPixels := 9;
  FFont.Hinted := False;
  FFont.ClearType := False;
  FFont.Quality := grqLowQuality;
  SetSize(8, 8);
  Clear;

end;

procedure TntvDisplayDots.SetDotSize(const AValue: Integer);
begin
  if Matrix.DotSize <> AValue then
  begin
    Matrix.DotSize := AValue;
    Changed;
    Invalidate;
  end;
end;

procedure TntvDisplayDots.SetFontName(AValue: string);
begin
  FFont.Name := AValue;
end;

destructor TntvDisplayDots.Destroy;
begin
  FreeAndNil(FCanvas);
  FreeAndNil(FDrawer);
  FreeAndNil(FFont);
  FreeAndNil(FImage);
  inherited;
end;

procedure TntvDisplayDots.SetBackColor(const AValue: TColor);
begin
  if Matrix.BackColor <> AValue then
  begin
    Matrix.BackColor := AValue;
    Invalidate;
  end;
end;

procedure TntvDisplayDots.Changed;
begin
end;

end.

