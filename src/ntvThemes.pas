unit ntvThemes;
{$mode objfpc}{$H+}
{**
 *  This file is part of the "Mini Library"
 *
 * @url       http://www.sourceforge.net/projects/minilib
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey
 *}
interface

uses
  Classes, Controls, SysUtils, Contnrs, Graphics, ImgList, GraphType,
  mnClasses, ColorUtils,
  LCLType, LCLIntf;

type
  TdrawState = (pdsFocused, pdsSelected, pdsActive, pdsDisabled, pdsDown, pdsMultiLine);
  TdrawStates = set of TdrawState;

  TdrawButtonKind = (kndNone, kndNext, kndLeft, kndRight, kndFirst, kndLast, kndUp, kndDown, kndEllipsis,
                     kndPin, kndPlus, kndOK, kndCheck, kndMinus, kndCross, kndStar, kndDiv, kndPoint);

  TdrawCorner = (crnTopLeft, crnTopRight, crnBottomRight, crnBottomLeft);
  TdrawCorners = set of TdrawCorner;
  TdrawSide = (sidTop, sidLeft, sidBottom, sidRight);
  TdrawSides = set of TdrawSide;

  TntvThemeEngine = class;

  TntvTheme = class;

  { TThemeAttribute }

  TThemeAttribute = class(TPersistent)
  private
    FBackground: TColor;
    FDescription: string;
    FForeground: TColor;
    FIndex: Integer;
    FParent: TntvTheme;
    FTitle: string;
  protected
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    procedure Reset;
    property Index: Integer read FIndex;
    property Title: string read FTitle write FTitle;
    property Description: string read FDescription write FDescription;
  published
    property Background: TColor read FBackground write FBackground default clNone;
    property Foreground: TColor read FForeground write FForeground default clNone;
  end;

  { TntvTheme }

  TntvTheme = class (specialize TmnObjectList<TThemeAttribute>)
  private
    FActive: TThemeAttribute;
    FButton: TThemeAttribute;
    FOdd: TThemeAttribute;
    FDefault: TThemeAttribute;
    FEngine: TntvThemeEngine;
    FHeader: TThemeAttribute;
    FHighlighted: TThemeAttribute;
    FLink: TThemeAttribute;
    FPanel: TThemeAttribute;
    FName: string;
    FSelected: TThemeAttribute;
    FSeparator: TThemeAttribute;
    FStub: TThemeAttribute;
    FTitle: TThemeAttribute;
    procedure SetName(const AValue: string);
  public
    constructor Create(AEngine: TntvThemeEngine; AName: string); virtual;
    procedure Correct;
    function GetEdgeColor: TColor;
    function GetLineColor: TColor; //for grids
    function GetUnactiveColor: TColor;

    procedure DrawText(Canvas: TCanvas; Text: string; Rect: TRect; Style: TTextStyle; UseRightToLeft: Boolean); virtual;
    procedure DrawText(Canvas: TCanvas; Text: string; Rect: TRect; UseRightToLeft: Boolean = False);
    procedure DrawRect(Canvas: TCanvas; const Rect: TRect; Color, BorderColor: TColor); virtual;
    procedure DrawButtonText(Canvas: TCanvas; Text: string; ImageWidth: Integer; Rect: TRect; States: TdrawStates; UseRightToLeft: Boolean); virtual;
    procedure DrawButtonEdge(Canvas: TCanvas; Rect: TRect; States: TdrawStates); virtual;
    procedure DrawButton(Canvas: TCanvas; Text: string; ImageWidth: Integer; Rect: TRect; States: TdrawStates; UseRightToLeft: Boolean); virtual;
    procedure DrawImage(Canvas: TCanvas; ImageList:TImageList; ImageIndex: TImageIndex; Rect: TRect; States: TdrawStates); virtual;
    property Name: string read FName write SetName;
  published
    property Default: TThemeAttribute read FDefault; //ListBox, Grid
    property Odd: TThemeAttribute read FOdd; //alternate ListBox, Grid colors
    property Panel: TThemeAttribute read FPanel; //Forms and Panels
    property Button: TThemeAttribute read FButton;
    property Title: TThemeAttribute read FTitle;
    property Header: TThemeAttribute read FHeader;
    property Stub: TThemeAttribute read FStub;
    property Separator: TThemeAttribute read FSeparator;
    property Link: TThemeAttribute read FLink;
    property Selected: TThemeAttribute read FSelected;
    property Highlighted: TThemeAttribute  read FHighlighted;
    property Active: TThemeAttribute read FActive;
  end;

  TntvThemes = class(specialize TmnNamedObjectList<TntvTheme>)
  end;

  IThemeNotify = interface(IInterface)
  ['{9F320814-3CEA-4106-8CCC-22C04DDCED53}']
    procedure InvalidateTheme;
  end;

  { TntvThemeEngine }

  TntvThemeEngine = class(TObject)
  private
    FTheme: TntvTheme;
    FThemes: TntvThemes;
    FShowButtonImages: Boolean;
    FNotifyComponents: TComponentList;
  protected
    procedure Switched;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddNotification(AComponent: TComponent);
    procedure RemoveNotification(AComponent: TComponent);
    procedure Correct;
    procedure Notify;
    //function Switch(NewPainter: TntvTheme):Boolean;// use Painter class
    //function Switch(NewPainter: string):Boolean; //use Painter name
    property Theme: TntvTheme read FTheme;
    property ShowButtonImages: Boolean read FShowButtonImages write FShowButtonImages;
  end;

function ThemeEngine: TntvThemeEngine;
function Theme: TntvTheme;

function DrawStates(Enabled, Down, Focused: Boolean): TdrawStates;
function DrawStates(ADrawStates: TdrawStates; Enabled, Down, Focused: Boolean): TdrawStates;

implementation

uses
  Types, ntvStdThemes;

var
  FThemeEngine: TntvThemeEngine = nil;

function Theme: TntvTheme;
begin
  Result := ThemeEngine.Theme;
end;

function ThemeEngine: TntvThemeEngine;
begin
  if FThemeEngine = nil then
    FThemeEngine := TntvThemeEngine.Create;
  Result := FThemeEngine;
end;

function DrawStates(Enabled, Down, Focused: Boolean): TdrawStates;
begin
  Result := DrawStates([], Enabled, Down, Focused);
end;

function DrawStates(ADrawStates: TdrawStates; Enabled, Down, Focused: Boolean): TdrawStates;
begin
  Result := ADrawStates;
  if Down then
    Result := Result + [pdsDown];
  if Focused then
    Result := Result + [pdsFocused];
  if not Enabled then
    Result := Result + [pdsDisabled];
end;

{ TThemeAttribute }

constructor TThemeAttribute.Create;
begin
  inherited;
  FBackground := clNone;
  FForeground := clNone;
end;

procedure TThemeAttribute.Assign(Source: TPersistent);
begin
  if Source is TThemeAttribute then
  begin
    Foreground := (Source as TThemeAttribute).Foreground;
    Background := (Source as TThemeAttribute).Background;
  end
  else
    inherited;
end;

procedure TThemeAttribute.Reset;
begin
  FBackground := clNone;
  FForeground := clNone;
end;

{ TntvTheme }

procedure TntvTheme.SetName(const AValue: string);
begin
  if FName =AValue then exit;
  FName :=AValue;
end;

constructor TntvTheme.Create(AEngine: TntvThemeEngine; AName: string);
  procedure Add(var Item: TThemeAttribute; Title, Description: string; Foreground, Background: TColor);
  begin
    Item := TThemeAttribute.Create;
    Item.Title := Title;
    Item.Description := Description;
    Item.Foreground := Foreground;
    Item.Background := Background;
    Item.FParent := Self;
    Item.FIndex := inherited Add(Item);
  end;
begin
  inherited Create;
  FEngine:= AEngine;
  FName := AName;

  Add(FDefault, 'Default', 'Default colors', clWindowText, clWindow);
  Add(FPanel, 'Panel', 'GUI Panel', clBtnText, clBtnFace);
  Add(FButton, 'Button', 'Buttons', clBtnText, clBtnFace);
  Add(FSelected, 'Selected', 'Selected text', clHighlightText, clHighlight);
  Add(FTitle, 'Title', 'Title', clCaptionText, clActiveCaption);
  Add(FSeparator, 'Separator', 'Seperators, between Stub of Editor and Line numbers', $00D8D8D8, $00CDCDCD);
  Add(FLink, 'Link', 'URL Links of follow defines highlight', clWhite, $002A190F);
  Add(FHighlighted, 'Highlighted', 'Highlighted important line, like running line', $00150FFF, clNone);
  Add(FActive, 'Active', 'Hover', clBlack, $008585CB);

  Add(FHeader, 'Header', 'Header', clBlack, $00E9E9E9);
  Add(FStub, 'Stub', 'Stub', clBlack, $00E9E9E9);
  Add(FOdd, 'Odd', 'Odd', clBlack, clMoneyGreen);

  Correct;
end;

procedure TntvTheme.Correct;
begin
end;

function TntvTheme.GetEdgeColor: TColor;
begin
  Result := MixColors(Default.Foreground, Theme.Panel.Background, 100);
end;

function TntvTheme.GetLineColor: TColor;
begin
  Result := MixColors(Default.Foreground, Theme.Default.Background, 100);
end;

function TntvTheme.GetUnactiveColor: TColor;
begin
  Result := MixColors(Theme.Default.Background, Theme.Panel.Background, 100);

end;

procedure TntvTheme.DrawText(Canvas: TCanvas; Text: string; Rect: TRect; Style: TTextStyle; UseRightToLeft: Boolean);
begin
  Style.RightToLeft := UseRightToLeft;
  Rect.Top := Rect.Top + 1; //idk why
  Canvas.TextRect(Rect, Rect.Left, Rect.Top, Text, Style);
end;

procedure TntvTheme.DrawText(Canvas: TCanvas; Text: string; Rect: TRect; UseRightToLeft: Boolean);
var
  TS: TTextStyle;
begin
  Initialize(TS);
  with TS do
  begin
    Alignment := BidiFlipAlignment(Alignment, UseRightToLeft);
    WordBreak := False;
    SingleLine:= True;
    Clipping := True;
    ShowPrefix := False;
    SystemFont := False;
    RightToLeft := UseRightToLeft;
    ExpandTabs := True;
  end;
  DrawText(Canvas, Text, Rect, TS, UseRightToLeft);
end;

procedure TntvTheme.DrawButtonText(Canvas: TCanvas; Text: string; ImageWidth: Integer; Rect: TRect; States: TdrawStates; UseRightToLeft: Boolean);
var
  TS: TTextStyle;
begin
  with Canvas do
  begin
    Brush.Style := bsClear;
    Initialize(TS);
    with TS do
    begin
      if ImageWidth = 0 then
        Alignment := BidiFlipAlignment(taCenter)
      else
        Alignment := BidiFlipAlignment(taLeftJustify, UseRightToLeft);
      WordBreak := False;
      SingleLine:= True;
      Clipping := True;
      ShowPrefix := False;
      SystemFont := False;
      RightToLeft := UseRightToLeft;
      ExpandTabs := True;
    end;

    if pdsDisabled in States then
      Font.Color := clBtnShadow;

    if pdsDown in States then
      OffsetRect(Rect, 1 , 1);
    DrawText(Canvas, Text, Rect, TS, UseRightToLeft);
  end;
end;

procedure TntvTheme.DrawButtonEdge(Canvas: TCanvas; Rect: TRect; States: TdrawStates);
begin
  if not (pdsDown in States) then
  begin
    Canvas.Pen.Color := Separator.Foreground;
    Canvas.MoveTo(Rect.Left, Rect.Bottom - 1);
    Canvas.LineTo(Rect.Left, Rect.Top);
    Canvas.LineTo(Rect.Right - 1, Rect.Top);
    Canvas.Pen.Color := Separator.Background;
    Canvas.LineTo(Rect.Right - 1, Rect.Bottom - 1);
    Canvas.LineTo(Rect.Left, Rect.Bottom - 1);
  end
  else
  begin
    Canvas.Pen.Color := Separator.Foreground;
    Canvas.MoveTo(Rect.Left, Rect.Bottom - 1);
    Canvas.LineTo(Rect.Left, Rect.Top);
    Canvas.LineTo(Rect.Right - 1, Rect.Top);
    Canvas.Pen.Color := Separator.Background;
    Canvas.LineTo(Rect.Right - 1, Rect.Bottom - 1);
    Canvas.LineTo(Rect.Left, Rect.Bottom - 1);
  end;
end;

procedure TntvTheme.DrawButton(Canvas: TCanvas; Text: string; ImageWidth: Integer; Rect: TRect; States: TdrawStates; UseRightToLeft: Boolean);
begin
  DrawButtonEdge(Canvas, Rect, States);
  InflateRect(Rect, -1 , -1);
  DrawButtonText(Canvas, Text, ImageWidth, Rect, States, UseRightToLeft);
end;

procedure TntvTheme.DrawImage(Canvas: TCanvas; ImageList: TImageList; ImageIndex: TImageIndex; Rect: TRect; States: TdrawStates);
var
  X, Y: integer;
begin
  x := Rect.Left + ((Rect.Right - Rect.Left) div 2) - (ImageList.Width div 2);
  y := Rect.Top + ((Rect.Bottom - Rect.Top) div 2) - (ImageList.Height div 2);
  ImageList.Draw(Canvas, x, y, ImageIndex, gdeNormal);
end;

procedure TntvTheme.DrawRect(Canvas: TCanvas; const Rect: TRect; Color, BorderColor: TColor);
begin
  Canvas.Pen.Color := BorderColor;
  Canvas.MoveTo(Rect.Left, Rect.Bottom - 1);
  Canvas.LineTo(Rect.Left, Rect.Top);
  Canvas.LineTo(Rect.Right - 1, Rect.Top);
  Canvas.LineTo(Rect.Right - 1, Rect.Bottom - 1);
  Canvas.LineTo(Rect.Left, Rect.Bottom - 1);
end;

{ TntvThemeEngine }

procedure TntvThemeEngine.Switched;
begin
  Notify;
end;

constructor TntvThemeEngine.Create;
begin
  inherited;
  FNotifyComponents := TComponentList.Create(False);
  FThemes  := TntvThemes.Create;
  FThemes.Add(TntvTheme.Create(Self, 'Standard'));
  FTheme := FThemes[0];
  Switched;
end;

destructor TntvThemeEngine.Destroy;
begin
  FreeAndNil(FThemes);
  FreeAndNil(FNotifyComponents);
  inherited Destroy;
end;

procedure TntvThemeEngine.AddNotification(AComponent: TComponent);
begin
  FNotifyComponents.Add(AComponent);
end;

procedure TntvThemeEngine.RemoveNotification(AComponent: TComponent);
begin
  FNotifyComponents.Remove(AComponent);
end;

procedure TntvThemeEngine.Correct;
begin
  Theme.Correct;
end;

procedure TntvThemeEngine.Notify;
var
  i: Integer;
begin
  for i := 0 to FNotifyComponents.Count -1 do
  begin
    if FNotifyComponents[i] is IThemeNotify then
    begin
      (FNotifyComponents[i] as IThemeNotify).InvalidateTheme;
    end;
  end;
end;

finalization
  FreeAndNil(FThemeEngine);
end.

