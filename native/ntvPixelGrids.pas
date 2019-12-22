unit ntvPixelGrids;
{**
 *  This file is part of the "Mini Library"
 *
 * @url       http://www.sourceforge.net/projects/minilib
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$mode objfpc}{$H+}

interface

uses
  LMessages, SysUtils, Classes, Graphics, Controls, Variants, Types,
  LCLIntf, LCLType, IntfGraphics,
  FPimage, FPCanvas, GraphType,
  mnUtils;

const
  cDotSize = 10;
  cDotMargin = 1;

  cForeColor = clBlack;
  cBackColor = clSilver;

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
    FRawImage: TLazIntfImage;
    FBitmap: TImageClass;
    FOffsetX: Integer;
    FOffsetY: Integer;
    FRotateOffset: Boolean;
    FUpdateCount: Integer;
    FBitmapChanged: Boolean;
    FOnInvalidate: TNotifyEvent;
    procedure CanvasChanged(Sender: TObject);
    procedure DrawPixelGrid(vCanvas: TCanvas; vRect: TRect);
    function GetCanvas: TCanvas;
    function GetHeight: Integer;
    function GetPixel(x, y: integer): TColor;
    function GetWidth: Integer;
    procedure SetDotMargin(const AValue: Integer);
    procedure SetDotSize(const AValue: Integer);
    procedure SetWidth(const AValue: Integer);
    procedure SetHeight(const AValue: Integer);
    procedure SetOffsetX(const AValue: Integer);
    procedure SetOffsetY(const AValue: Integer);
    procedure SetRotateOffset(const AValue: Boolean);
    function GetUpdating: Boolean;
    procedure InternalInvalidate;
    procedure SetPixel(x, y: integer; const AValue: TColor);
    procedure SetBackColor(const AValue: TColor);
  protected
    Matrix: TPixelGridInfo;
    procedure Changed;
    procedure Invalidate;
    property Bitmap: TImageClass read FBitmap;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Draw(vCanvas: TCanvas; vRect: TRect);
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
    property RawImage: TLazIntfImage read FRawImage;
    property Canvas: TCanvas read GetCanvas;
    property Pixels[x, y:integer] : TColor read GetPixel write SetPixel;
    //For auto refresh it when draw to Canvas
    property OnInvalidate: TNotifyEvent read FOnInvalidate write FOnInvalidate;
    //Some control need to redraw bitmap after bitmap is clear
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    //Scroll the dots
    property OffsetX: Integer read FOffsetX write SetOffsetX;
    property OffsetY: Integer read FOffsetY write SetOffsetY;
    //when the dots is out of canvas take the dot from the first, Rotate the dots one time only
    property RotateOffset: Boolean read FRotateOffset write SetRotateOffset;
  published
    property BackColor: TColor read Matrix.BackColor write SetBackColor default cBackColor;
    property DotSize: Integer read Matrix.DotSize write SetDotSize default cDotSize;
    property DotMargin: Integer read Matrix.DotMargin write SetDotMargin default 1;
  end;

  { TntvPixelGrid }

  TntvPixelGrid = class(TCustomControl)
  private
    FDots: TntvDisplayDots;
  protected
    procedure FontChanged(Sender: TObject); override;
    procedure Paint; override;
    procedure CMBidModeChanged(var Message: TLMessage); message CM_BIDIMODECHANGED;
    procedure DoInvalidate(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;
    procedure Resize; override;
    procedure Loaded; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    property Dots: TntvDisplayDots read FDots;
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

  {R.Right := x + vInfo.DotSize + vInfo.DotMargin;
  R.Bottom := Y + vInfo.DotSize + vInfo.DotMargin;
  Canvas.Brush.Color := vInfo.BackColor;
  Canvas.FillRect(R);}

  R.Right := x + vInfo.DotSize;
  R.Bottom := Y + vInfo.DotSize;
  Canvas.Brush.Color := PixelColor;
  Canvas.FillRect(R);
end;

procedure TntvDisplayDots.DrawPixelGrid(vCanvas: TCanvas; vRect: TRect);
var
  x, y: integer;
  ix, iy: integer;
  ox, oy: integer;
  aPixelColor: TColor;
begin
  vCanvas.Brush.Color := Matrix.BackColor;
  vCanvas.FillRect(vRect);
  y := vRect.Top;
  iy := 0;
  while iy < RawImage.Height do
  begin
    x := vRect.Left;
    ix := 0;
    while ix < RawImage.Width do
    begin
      ox := ix + OffsetX;
      oy := iy + OffsetY;
      if RotateOffset then
      begin
        if ox >= RawImage.Width then
          ox := ox - RawImage.Width;
        if oy >= RawImage.Height then
          oy := oy - RawImage.Height;
      end;

      if (ox < RawImage.Width) and (oy < RawImage.Height) then
        aPixelColor := RawImage.TColors[ox, oy]
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

{ TntvPixelGrid }

constructor TntvPixelGrid.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csDoubleClicks] - [csOpaque];
  FDots := TntvDisplayDots.Create;
  FDots.Canvas.Font.Assign(Font);
  FDots.OnInvalidate := @DoInvalidate;
  Color := cBackColor;
end;

destructor TntvPixelGrid.Destroy;
begin
  FreeAndNil(FDots);
  inherited;
end;

procedure TntvPixelGrid.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);
  FDots.Canvas.Font.Assign(Font);
end;

procedure TntvPixelGrid.Paint;
begin
  inherited;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);
  Dots.Draw(Canvas, ClientRect);
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

procedure TntvPixelGrid.DoInvalidate(Sender: TObject);
begin
  Invalidate;
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
  FDots.Canvas.Font.Assign(Font);
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

procedure TntvDisplayDots.Draw(vCanvas: TCanvas; vRect: TRect);
begin
  if FBitmapChanged then
  begin
    RawImage.LoadFromBitmap(Bitmap.BitmapHandle, Bitmap.MaskHandle);
    FBitmapChanged := False;
  end;
  DrawPixelGrid(vCanvas, vRect);
end;

procedure TntvDisplayDots.SetSize(const AWidth, AHeight: Integer);
begin
  if (Width <> AWidth) or (Height <> AHeight) then
  begin
    Bitmap.SetSize(AWidth, AHeight);
    Invalidate;
  end;
end;

procedure TntvDisplayDots.SaveToFile(FileName: string);
begin
  Bitmap.SaveToFile(FileName);
end;

procedure TntvDisplayDots.LoadFromFile(FileName: string);
begin
  Bitmap.LoadFromFile(FileName);
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
  Bitmap.Canvas.Brush.Color := clWhite;
  Bitmap.Canvas.FillRect(0, 0, Width, Height);
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
    InternalInvalidate;
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

function TntvDisplayDots.GetCanvas: TCanvas;
begin
  Result := Bitmap.Canvas;
end;

function TntvDisplayDots.GetHeight: Integer;
begin
  Result := FBitmap.Height;
end;

function TntvDisplayDots.GetPixel(x, y: integer): TColor;
begin
  Result := RawImage.TColors[x, y];
end;

function TntvDisplayDots.GetWidth: Integer;
begin
  Result := FBitmap.Width;
end;

procedure TntvDisplayDots.InternalInvalidate;
begin
  if Assigned(OnInvalidate) then
    OnInvalidate(Self);
end;

procedure TntvDisplayDots.SetHeight(const AValue: Integer);
begin
  if FBitmap.Height <> AValue then
  begin
    FBitmap.Height := AValue;
    Invalidate;
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

procedure TntvDisplayDots.SetRotateOffset(const AValue: Boolean);
begin
  if FRotateOffset <> AValue then
  begin
    FRotateOffset :=AValue;
    Invalidate;
  end;

end;

procedure TntvDisplayDots.SetPixel(x, y: integer; const AValue: TColor);
begin
  RawImage.TColors[x, y] := AValue;
  Invalidate;
end;

procedure TntvDisplayDots.SetWidth(const AValue: Integer);
begin
  if FBitmap.Width <> AValue then
  begin
    FBitmap.Width := AValue;
    Invalidate;
  end;
end;

constructor TntvDisplayDots.Create;
begin
  inherited;
  Matrix.BackColor := cBackColor;
  Matrix.DotSize := cDotSize;
  Matrix.DotMargin := 1;

  FRawImage := TLazIntfImage.Create(10, 10, [riqfRGB, riqfAlpha]);
  FBitmap := TImageClass.Create;
  FBitmap.SetSize(10, 10);
  Clear;
  FBitmap.Canvas.OnChange := @CanvasChanged;
  Changed;
  Invalidate;
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

destructor TntvDisplayDots.Destroy;
begin
  FreeAndNil(FRawImage);
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
  FBitmapChanged := True;
end;

end.

