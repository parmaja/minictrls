unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ColorBox, ComCtrls, Menus, FPimage, FPCanvas, ntvPixelGrids;

type

  { TForm1 }

  TForm1 = class(TForm)
    AlphaTrack: TTrackBar;
    ColorListBox1: TColorListBox;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    N1: TMenuItem;
    Grid: TntvPixelGrid;
    OpenDialog1: TOpenDialog;
    Panel3: TPanel;
    SaveDialog1: TSaveDialog;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure AlphaTrackChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure ColorListBox1SelectionChange(Sender: TObject; User: boolean);
    procedure FormCreate(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    procedure ToolButton4Click(Sender: TObject);
    procedure ToolButton5Click(Sender: TObject);
    procedure ToolButton6Click(Sender: TObject);
    procedure ToolButton7Click(Sender: TObject);
    procedure ToolButton8Click(Sender: TObject);
  private
  public
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

procedure TForm1.MenuItem2Click(Sender: TObject);
begin
end;

procedure TForm1.MenuItem4Click(Sender: TObject);
begin
  Exit;
end;

procedure TForm1.ToolButton1Click(Sender: TObject);
begin
  Grid.CurrentToolClass := TntvPixel;
end;

procedure TForm1.ToolButton2Click(Sender: TObject);
begin
  Grid.CurrentToolClass := TntvRectangle;
end;

procedure TForm1.ToolButton3Click(Sender: TObject);
begin
  Grid.CurrentToolClass := TntvLine;
end;

procedure TForm1.ToolButton4Click(Sender: TObject);
begin
  Grid.CurrentToolClass := TntvFill;
end;

procedure TForm1.ToolButton5Click(Sender: TObject);
begin
  Grid.CurrentToolClass := TntvCircle;
end;

procedure TForm1.ToolButton6Click(Sender: TObject);
begin
  Grid.CurrentToolClass := TntvReplaceColor;
end;

procedure TForm1.ToolButton7Click(Sender: TObject);
begin
  Grid.Clear;
end;

procedure TForm1.ToolButton8Click(Sender: TObject);
begin
  Grid.CurrentToolClass := TntvDraw;
end;

procedure TForm1.AlphaTrackChange(Sender: TObject);
begin
  Grid.CurrentAlpha := AlphaTrack.Position;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin

end;

procedure TForm1.Button2Click(Sender: TObject);
begin

end;

procedure TForm1.Button4Click(Sender: TObject);
begin

end;

procedure TForm1.Button5Click(Sender: TObject);
begin

end;

procedure TForm1.ColorListBox1SelectionChange(Sender: TObject; User: boolean);
begin
  Grid.CurrentColor := ColorListBox1.Selected;
end;

constructor TForm1.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

end.

