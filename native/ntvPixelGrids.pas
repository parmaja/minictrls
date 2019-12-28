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
 * https://wiki.freepascal.org/Graphics_-_Working_with_TCanvas
 *
 *}

{$mode objfpc}{$H+}

interface

uses
  LMessages, SysUtils, Classes, Graphics, Controls, Variants, Types,
  fgl, LCLIntf, LCLType, IntfGraphics,
  FPImage, FPCanvas, FPImgCanv, GraphType, EasyLazFreeType, LazFreeTypeIntfDrawer, LazCanvas,
  mnUtils;

const
  cDotSize = 11;
  cDotPadding = 1;
  cDefaultWidth = 32;
  cDefaultHeight = 32;

  cForeColor = clBlack;
  cBackColor = clSilver;

  colWhiteTransparent: TFPColor = (Red: $ffff; Green: $ffff; Blue: $ffff; Alpha: alphaTransparent);
  colFuchsiaTransparent: TFPColor = (Red: $ffff; Green: $0000; Blue: $ffff; Alpha: alphaTransparent);

type

  { TLazIntfImageHelper }

  TLazIntfImageHelper = class helper for TLazIntfImage
  public
    procedure ScalePixels(Source: TLazIntfImage; ScaleBy: Integer = 1; WithAlphaBlend: Boolean = True); //1 = no scale
  end;

  TImageClass = TPortableNetworkGraphic;
  TntvPaintTool = class;

  TPixelGridInfo = record
    BackColor: TColor;
    DotSize: Integer;
    DotPadding: Integer;
  end;

  { TntvHistoryObject }

  TntvHistoryObject = class(TObject)
  public
    Image: TLazIntfImage;
    constructor Create(AImage: TLazIntfImage);
    destructor Destroy; override;
  end;

  TntvHistory = class (specialize TFPGObjectList<TntvHistoryObject>)
  end;

  { TntvDisplayDots }

  TntvDisplayDots = class(TPersistent)
  private
    FScrachImage: TLazIntfImage;
    FScrachCanvas: TFPImageCanvas;

    FImage: TLazIntfImage;
    FCanvas: TFPImageCanvas;
    FDrawer: TIntfFreeTypeDrawer;

    FFont: TFreeTypeFont;
    FOffsetX: Integer;
    FOffsetY: Integer;
    FUpdateCount: Integer;
    FHistory: TntvHistory;

    ScaledImage: TLazIntfImage;
    //ScaledCanvas: TFPImageCanvas;

    BackgroundImage: TLazIntfImage;
    BackgroundCanvas: TFPImageCanvas;

    procedure CanvasChanged(Sender: TObject);
    function GetFontName: string;

    function GetHeight: Integer;
    function GetPixel(x, y: integer): TColor;
    function GetWidth: Integer;
    procedure PaintBackground;
    procedure SetDotPadding(const AValue: Integer);
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
    IsChanged: Boolean;
    Matrix: TPixelGridInfo;

    procedure Changed;
    procedure Invalidate;
    procedure DoInvalidate; virtual;

    procedure UpdateSize;
    procedure PushHistory;
    procedure PopHistory;
    property Canvas: TFPImageCanvas read FCanvas;
    function VisualToReal(VistualPoint: TPoint; out RealPoint: TPoint): Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Paint(vCanvas: TCanvas; vRect: TRect; PaintTool: TntvPaintTool);
    //y here is the base line of text, bottom of text
    procedure DrawText(x, y: Integer; AText: string; AColor: TColor);
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
    property ScrachImage: TLazIntfImage read FScrachImage;
    property ScrachCanvas: TFPImageCanvas read FScrachCanvas;
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
    property DotPadding: Integer read Matrix.DotPadding write SetDotPadding default cDotPadding;
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

  TntvPixelGridInfo = record
    CurrentColor: TColor;
    CurrentAlpha: Byte;
    CurrentMerge: Boolean;
  end;

  TntvPaintToolStyle = set of (
    ptsDirect, //Direct Paint on Canvas, like Pixel and FloodFill
    ptsEnd   //Finish work after first paint (MouseDown)
  );

  { TntvPaintTool }

  TntvPaintTool = class abstract(TObject)
  private
    FControl: TntvPixelGrid;
  protected
    FStyle: TntvPaintToolStyle;
    StartPoint: TPoint;
    CurrentPoint: TPoint;
    property Control: TntvPixelGrid read FControl;
    procedure Created; virtual;
  public
    constructor Create(AStartPoint: TPoint; AControl: TntvPixelGrid); virtual;
    procedure Paint(Canvas: TFPImageCanvas); virtual; abstract;
    property Style: TntvPaintToolStyle read FStyle;
  end;

  TntvPaintToolClass = class of TntvPaintTool;

  TntvPaintTools = class (specialize TFPGObjectList<TntvPaintTool>)
  end;

  { TntvPixel }

  TntvPixel = class(TntvPaintTool)
  public
    procedure Paint(Canvas: TFPImageCanvas); override;
    procedure Created; override;
  end;

  { TntvDraw }

  TntvDraw = class(TntvPaintTool)
  public
    LastPoint: TPoint;
    procedure Paint(Canvas: TFPImageCanvas); override;
    procedure Created; override;
  end;

  { TntvLine }

  TntvLine = class(TntvPaintTool)
  public
    procedure Paint(Canvas: TFPImageCanvas); override;
  end;

  { TntvRectangle }

  TntvRectangle = class(TntvPaintTool)
  public
    procedure Paint(Canvas: TFPImageCanvas); override;
  end;

  { TntvCircle }

  TntvCircle = class(TntvPaintTool)
  public
    procedure Paint(Canvas: TFPImageCanvas); override;
  end;

  { TntvFill }

  TntvFill = class(TntvPaintTool)
  public
    procedure Created; override;
    procedure Paint(Canvas: TFPImageCanvas); override;
  end;

  { TntvReplaceColor }

  TntvReplaceColor = class(TntvPaintTool)
  public
    procedure Paint(Canvas: TFPImageCanvas); override;
  end;

  { TntvPixelGrid }

  TntvPixelGrid = class(TCustomControl)
  private
    FCurrentTool: TntvPaintTool;
    FCurrentToolClass: TntvPaintToolClass;
    FDots: TntvDisplayDots;
    Info: TntvPixelGridInfo;
  protected
    procedure FontChanged(Sender: TObject); override;
    procedure Paint; override;
    procedure CMBidModeChanged(var Message: TLMessage); message CM_BIDIMODECHANGED;
    procedure SetCurrentTool(AValue: TntvPaintTool);
    procedure StartTool(RealPoint: TPoint); virtual;
    procedure EndTool(Cancel: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;
    procedure Resize; override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Clear;
    property Dots: TntvDisplayDots read FDots;
    property CurrentToolClass: TntvPaintToolClass read FCurrentToolClass write FCurrentToolClass;
    property CurrentTool: TntvPaintTool read FCurrentTool;
    property CurrentColor: TColor read Info.CurrentColor write Info.CurrentColor;
    property CurrentAlpha: Byte read Info.CurrentAlpha write Info.CurrentAlpha;
    property CurrentMerge: Boolean read Info.CurrentMerge write Info.CurrentMerge; //TODO
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

function ToFPColor(AColor: TColor; AAlpha: Byte): TFPColor;
begin
  Result := TColorToFPColor(AColor);
  Result.Alpha := MAXWORD * AAlpha div 255;
end;

{ TLazIntfImageHelper }

procedure TLazIntfImageHelper.ScalePixels(Source: TLazIntfImage; ScaleBy: Integer; WithAlphaBlend: Boolean);
var
  x, y: Integer;
  sx, sy: Integer;
  dx, dy: integer;
begin
  //TODO check if source greater than self
  x := 0;
  sx := 0;
  while sx < Source.Width do
  begin
    dx := 0;
    while dx < ScaleBy do
    begin
      y := 0;
      sy := 0;
      while sy < Source.Width do
      begin
        dy := 0;
        while dy < ScaleBy do
        begin
          if WithAlphaBlend then
            Colors[x, y] := FPImage.AlphaBlend(Colors[x,y], Source.Colors[sx, sy])
          else
            Colors[x, y] := Source.Colors[sx, sy];
          inc(y);
          inc(dy);
        end;
        inc(sy);
      end;
      inc(x);
      inc(dx);
    end;
    inc(sx);
  end;
end;

{ TntvDraw }

procedure TntvDraw.Paint(Canvas: TFPImageCanvas);
begin
  Canvas.Pen.FPColor := ToFPColor(Control.CurrentColor, Control.CurrentAlpha);
  Canvas.Brush.Style := bsClear;
  Canvas.Line(LastPoint, CurrentPoint);
  LastPoint := CurrentPoint;
end;

procedure TntvDraw.Created;
begin
  inherited Created;
  FStyle := FStyle + [ptsDirect];
  LastPoint := CurrentPoint;
end;

{ TntvPaintTool }

procedure TntvPaintTool.Created;
begin

end;

constructor TntvPaintTool.Create(AStartPoint: TPoint; AControl: TntvPixelGrid);
begin
  inherited Create;
  FControl := AControl;
  StartPoint := AStartPoint;
  CurrentPoint := AStartPoint;
  FStyle := [];
  Created;
end;

{ TntvLine }

procedure TntvLine.Paint(Canvas: TFPImageCanvas);
begin
  Canvas.Pen.FPColor := ToFPColor(Control.CurrentColor, Control.CurrentAlpha);
  Canvas.Brush.Style := bsClear;
  Canvas.Line(StartPoint, CurrentPoint);
end;

{ TntvReplaceColor }

procedure TntvReplaceColor.Paint(Canvas: TFPImageCanvas);
begin
  //Canvas.
end;

{ TntvFill }

procedure TntvFill.Created;
begin
  inherited Created;
  FStyle := FStyle + [ptsDirect, ptsEnd];
end;

procedure TntvFill.Paint(Canvas: TFPImageCanvas);
var
  c: TFPColor;
begin
  c := ToFPColor(Control.CurrentColor, Control.CurrentAlpha);
  if Canvas.Image.Colors[CurrentPoint.X, CurrentPoint.Y] <> c then
  begin
    //Canvas.Pen.FPColor := c;
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.FPColor := c;
    Canvas.FloodFill(CurrentPoint.X, CurrentPoint.Y);
  end;
end;

{ TntvCircle }

procedure TntvCircle.Paint(Canvas: TFPImageCanvas);
begin
  Canvas.Pen.FPColor := ToFPColor(Control.CurrentColor, Control.CurrentAlpha);
  Canvas.Brush.Style := bsClear;
  Canvas.Ellipse(StartPoint.x, StartPoint.y, CurrentPoint.x, CurrentPoint.y);
end;

{ TntvRectangle }

procedure TntvRectangle.Paint(Canvas: TFPImageCanvas);
begin
  Canvas.Pen.FPColor := ToFPColor(Control.CurrentColor, Control.CurrentAlpha);
  Canvas.Brush.Style := bsClear;
  Canvas.Rectangle(StartPoint.x, StartPoint.y, CurrentPoint.x, CurrentPoint.y);
end;

{ TntvHistoryObject }

constructor TntvHistoryObject.Create(AImage: TLazIntfImage);
begin
  inherited Create;
  Image := AImage;
end;

destructor TntvHistoryObject.Destroy;
begin
  inherited Destroy;
  FreeAndNil(Image);
end;

{ TntvPixel }

procedure TntvPixel.Paint(Canvas: TFPImageCanvas);
begin
  Canvas.DrawPixel(CurrentPoint.x, CurrentPoint.y, ToFPColor(Control.CurrentColor, Control.CurrentAlpha));
end;

procedure TntvPixel.Created;
begin
  inherited Created;
  FStyle := FStyle + [ptsDirect];
end;

{ TntvPaintTool }


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
  Info.CurrentColor := clBlack;
  Info.CurrentAlpha := 255;
  CurrentToolClass := TntvPixel;
end;

destructor TntvPixelGrid.Destroy;
begin
  EndTool(True);
  FreeAndNil(FDots);
  inherited;
end;

procedure TntvPixelGrid.SetCurrentTool(AValue: TntvPaintTool);
begin
  if FCurrentTool = AValue then
    Exit;
  FCurrentTool := AValue;
  Dots.Changed;
end;

procedure TntvPixelGrid.StartTool(RealPoint: TPoint);
begin
  EndTool(True);
  FCurrentTool := FCurrentToolClass.Create(RealPoint, Self);
end;

procedure TntvPixelGrid.EndTool(Cancel: Boolean);
begin
  if FCurrentTool <> nil then
  begin
    if not Cancel then
    begin
      if not (ptsDirect in FCurrentTool.Style) then
        Dots.Image.CopyPixels(Dots.ScrachImage);
    end;
    FreeAndNil(FCurrentTool);
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
  Dots.Paint(Canvas, ClientRect, CurrentTool);
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
var
  RealPoint: TPoint;
begin
  inherited MouseMove(Shift, X, Y);
  if FCurrentTool <> nil then
    if Dots.VisualToReal(Point(X, Y), RealPoint) then
    begin
      if (RealPoint.X <> FCurrentTool.CurrentPoint.X) or (RealPoint.Y <> FCurrentTool.CurrentPoint.Y) then
      begin
        FCurrentTool.CurrentPoint := RealPoint;
        Dots.ScrachImage.CopyPixels(Dots.Image);
        FCurrentTool.Paint(Dots.ScrachCanvas);
        if ptsDirect in FCurrentTool.Style then
          Dots.Image.CopyPixels(Dots.ScrachImage);
        Dots.Changed;
      end;
    end;
end;

procedure TntvPixelGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  EndTool(False);
end;

procedure TntvPixelGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Shift = [] then
  begin
    case Key of
      VK_ESCAPE: EndTool(True);
    end;
  end
  else if Shift = [ssCtrl] then
  begin
    case Key of
      VK_Z: Dots.PopHistory;
    end;
  end;
end;

procedure TntvPixelGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  RealPoint: TPoint;
begin
  inherited MouseDown(Button, Shift, X, Y);
  SetFocus;
  if Dots.VisualToReal(Point(X, Y), RealPoint) then
  begin
    if FCurrentToolClass <> nil then
    begin
      Dots.PushHistory;
      StartTool(RealPoint);
      Dots.ScrachImage.CopyPixels(Dots.Image);
      FCurrentTool.Paint(Dots.ScrachCanvas);
      if ptsDirect in FCurrentTool.Style then
        Dots.Image.CopyPixels(Dots.ScrachImage);
      if ptsEnd in FCurrentTool.Style then
        EndTool(True);
      Dots.Changed;
    end;
  end
  else
    ReleaseCapture;
end;

procedure TntvPixelGrid.BeginUpdate;
begin
  Dots.BeginUpdate;
end;

procedure TntvPixelGrid.EndUpdate;
begin
  Dots.EndUpdate;
end;

procedure TntvPixelGrid.Clear;
begin
  Dots.Clear;
end;

{ TntvDisplayDots }

procedure TntvDisplayDots.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TntvDisplayDots.Paint(vCanvas: TCanvas; vRect: TRect; PaintTool: TntvPaintTool);
var
  Img: TBitmap;
begin
  if IsChanged then
  begin
    IsChanged := False;

    ScaledImage.CopyPixels(BackgroundImage);
    //ScaledCanvas.DrawingMode := dmAlphaBlend;
    //ScaledCanvas.Interpolation := TFPBoxInterpolation.Create; moved to after create ScaledCanvas
    //ScaledCanvas.Draw(0, 0, ScrachImage); for testing
    //ScaledCanvas.StretchDraw(0, 0, ScaledImage.Width, ScaledImage.Height, ScrachImage);
    ScaledImage.ScalePixels(ScrachImage, DotSize, True); //still slow
  end;

  //(vCanvas as TFPCustomCanvas).Draw(0, 0, ScaledImage); //very slow than loading it into bmp

  Img := TBitmap.Create; //maybe make it as cache
  try
    Img.LoadFromIntfImage(ScaledImage);
    vCanvas.Draw(0, 0, Img);
  finally
    Img.Free;
  end;
end;

procedure TntvDisplayDots.PaintBackground;
  procedure DrawIt(Canvas: TFPImageCanvas; x: integer; y: integer);
  var
    R: TRect;
    d: Integer;
  begin
    Canvas.Brush.Style := bsSolid;

    R.Left := x;
    R.Top := y;
    R.Right := R.Left + DotSize - 1;
    R.Bottom := R.Top + DotSize - 1;
    Canvas.Brush.FPColor := colBlack;
    Canvas.FillRect(R);

    R.Left := x;
    R.Top := y;
    R.Right := R.Left + DotSize - DotPadding - 1;
    R.Bottom := R.Top + DotSize - DotPadding - 1;
    Canvas.Brush.FPColor := colLtGray;
    Canvas.FillRect(R);

    Canvas.Brush.FPColor := colWhite;

    d := (DotSize - DotPadding) div 2;
    R.Left := x;
    R.Top := y;
    R.Right := R.Left + d - 1;
    R.Bottom := R.Top + d - 1 ;
    Canvas.FillRect(R);

    R.Left := x + d;
    R.Top := y + d;
    R.Right := R.Left + d - 1;
    R.Bottom := R.Top + d - 1;
    Canvas.FillRect(R);
  end;
var
  x, y: integer;
  ix, iy: integer;
begin
  y := 0;
  iy := 0;
  while iy < Height do
  begin
    x := 0;
    ix := 0;
    while ix < Width do
    begin
      DrawIt(BackgroundCanvas, x, y);
      x := x + DotSize;
      Inc(ix);
    end;
    y := y + DotSize;
    Inc(iy);
  end;
end;

procedure TntvDisplayDots.DrawText(x, y: Integer; AText: string; AColor: TColor);
begin
  FDrawer.DrawText(AText, FFont, x, y, TColorToFPColor(AColor));
  Changed;
end;

procedure TntvDisplayDots.SetSize(const AWidth, AHeight: Integer);
begin
  FImage.SetSize(AWidth, AHeight);
  UpdateSize;
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
    UpdateSize;
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
      Changed;
  end;
end;

procedure TntvDisplayDots.Clear;
begin
  FImage.FillPixels(colWhiteTransparent);
  FScrachImage.FillPixels(colWhiteTransparent);
  Changed;
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

procedure TntvDisplayDots.UpdateSize;
begin
  ScrachImage.SetSize(Width, Height);
  ScaledImage.SetSize(Width * DotSize, Height * DotSize);
  BackgroundImage.SetSize(Width * DotSize, Height * DotSize);
  PaintBackground;
  Changed;
end;

procedure TntvDisplayDots.PushHistory;
var
  AImage: TLazIntfImage;
begin
  AImage := TLazIntfImage.CreateCompatible(FImage, FImage.Width, FImage.Height);
  AImage.CopyPixels(FImage);
  FHistory.Add(TntvHistoryObject.Create(AImage));
  if FHistory.Count > 20 then
    FHistory.Delete(0);
end;

procedure TntvDisplayDots.PopHistory;
var
  AImage: TLazIntfImage;
begin
  if FHistory.Count > 0 then
  begin
    AImage := FHistory.Last.Image;
    FImage.CopyPixels(AImage);
    FHistory.Delete(FHistory.Count - 1);
    Changed;
  end;
end;

function TntvDisplayDots.VisualToReal(VistualPoint: TPoint; out RealPoint: TPoint): Boolean;
var
  aSize: Integer;
begin
  if (VistualPoint.x >= 0) and (VistualPoint.y >= 0) then
  begin
    aSize := Matrix.DotSize;
    RealPoint.x := (VistualPoint.x + aSize) div aSize - 1;
    RealPoint.y := (VistualPoint.y + aSize) div aSize - 1;
    Result := (RealPoint.x < Width) and (RealPoint.y < Height);
  end
  else
    Result := False;
end;

procedure TntvDisplayDots.SetDotPadding(const AValue: Integer);
begin
  if Matrix.DotPadding <> AValue then
  begin
    Matrix.DotPadding := AValue;
    Changed;
  end;
end;

procedure TntvDisplayDots.CanvasChanged(Sender: TObject);
begin
  Changed;
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
    Changed;
  end;
end;

procedure TntvDisplayDots.SetOffsetY(const AValue: Integer);
begin
  if FOffsetY <> AValue then
  begin
    FOffsetY := AValue;
    Changed;
  end;
end;

procedure TntvDisplayDots.SetPixel(x, y: integer; const AValue: TColor);
begin
  Image.TColors[x, y] := AValue;
  Changed;
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
  Matrix.DotPadding := cDotPadding;

  FImage := TLazIntfImage.Create(cDefaultWidth, cDefaultHeight, [riqfRGB, riqfAlpha]);
  FCanvas := TFPImageCanvas.Create(FImage);
  FDrawer := TIntfFreeTypeDrawer.Create(FImage);
  FFont := TFreeTypeFont.Create;
  FFont.Name := 'c:\Windows\fonts\Arial.ttf';
  FFont.SizeInPixels := 9;
  FFont.Hinted := False;
  FFont.ClearType := False;
  FFont.Quality := grqLowQuality;

  FScrachImage := TLazIntfImage.CreateCompatible(FImage, cDefaultWidth, cDefaultHeight);
  FScrachCanvas := TFPImageCanvas.Create(FScrachImage);

  ScaledImage := TLazIntfImage.CreateCompatible(ScrachImage, ScrachImage.Width * DotSize, ScrachImage.Height * DotSize);
  //ScaledCanvas := TFPImageCanvas.Create(ScaledImage);
  //ScaledCanvas.Interpolation := TFPBoxInterpolation.Create;

  BackgroundImage := TLazIntfImage.CreateCompatible(ScrachImage, ScrachImage.Width * DotSize, ScrachImage.Height * DotSize);
  BackgroundCanvas := TFPImageCanvas.Create(BackgroundImage);

  FHistory := TntvHistory.Create;

  SetSize(cDefaultWidth, cDefaultHeight);
  Clear;

end;

procedure TntvDisplayDots.SetDotSize(const AValue: Integer);
begin
  if Matrix.DotSize <> AValue then
  begin
    Matrix.DotSize := AValue;
    Changed;
  end;
end;

procedure TntvDisplayDots.SetFontName(AValue: string);
begin
  FFont.Name := AValue;
end;

destructor TntvDisplayDots.Destroy;
begin
  BackgroundImage.Free;
  BackgroundCanvas.Free;
  ScaledImage.Free;
  //ScaledCanvas.Interpolation.Free;
  //ScaledCanvas.Free;
  FreeAndNil(FHistory);
  FreeAndNil(FCanvas);
  FreeAndNil(FScrachCanvas);
  FreeAndNil(FDrawer);
  FreeAndNil(FFont);
  FreeAndNil(FImage);
  FreeAndNil(FScrachImage);
  inherited;
end;

procedure TntvDisplayDots.SetBackColor(const AValue: TColor);
begin
  if Matrix.BackColor <> AValue then
  begin
    Matrix.BackColor := AValue;
    Changed;
  end;
end;

procedure TntvDisplayDots.Changed;
begin
  IsChanged := True;
  Invalidate;
end;

end.

