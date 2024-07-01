unit ntvCtrls; 

{$mode objfpc}{$H+}

interface

uses
  Classes, Messages, Controls, StdCtrls, ExtCtrls, SysUtils, Math, Contnrs, Graphics, Forms,
  LCLType, LCLIntf, LMessages, LCLProc, ntvThemes,
  ntvUtils;

type

  { TntvCustomControl }

  TntvCustomControl = class(TCustomControl)
  protected
    procedure InvalidateRect(ARect : TRect; Erase : Boolean); virtual; //this should be in Lazarus TCustomControl
  end;

  { TntvEdit }

  TntvEdit = class(TEdit, IThemeNotify)
  protected
    procedure InvalidateTheme;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Color;
  end;

procedure ExcludeClipRect(Canvas: TCanvas; Rect: TRect);

implementation

procedure ExcludeClipRect(Canvas: TCanvas; Rect: TRect);
begin
  LCLIntf.ExcludeClipRect(Canvas.Handle, Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
end;

{ TntvEdit }

procedure TntvEdit.InvalidateTheme;
begin
  Color := Theme.Default.Background;
  Font.Color := Theme.Default.Foreground;
  Invalidate;
end;

procedure TntvEdit.Loaded;
begin
  inherited Loaded;
  if ParentColor then
  begin
    Color := Theme.Default.Background;
    Font.Color := Theme.Default.Foreground;
  end;
end;

constructor TntvEdit.Create(AOwner: TComponent);
begin
  inherited;
  ThemeEngine.AddNotification(Self);
end;

destructor TntvEdit.Destroy;
begin
  ThemeEngine.RemoveNotification(Self);
  inherited;
end;

{ TntvCustomControl }

procedure TntvCustomControl.InvalidateRect(ARect: TRect; Erase: Boolean);
begin
  LCLIntf.InvalidateRect(Handle, @ARect, Erase);
end;

end.

