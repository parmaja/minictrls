unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ColorBox, ComCtrls, Menus, Interfaces, FPimage, FPCanvas, ntvPixelGrids,
  ntvDotMatrix;

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
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    MergeBtn: TToolButton;
    ToolButton15: TToolButton;
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
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure ToolButton10Click(Sender: TObject);
    procedure ToolButton11Click(Sender: TObject);
    procedure ToolButton12Click(Sender: TObject);
    procedure ToolButton13Click(Sender: TObject);
    procedure MergeBtnClick(Sender: TObject);
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
  if OpenDialog1.Execute then
    Grid.Dots.LoadFromFile(OpenDialog1.FileName);
end;

procedure TForm1.MenuItem3Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
    Grid.Dots.SaveToFile(SaveDialog1.FileName);
end;

procedure TForm1.MenuItem4Click(Sender: TObject);
begin
  Exit;
end;

procedure TForm1.ToolButton10Click(Sender: TObject);
begin
  Grid.Dots.SetSize(64, 64);
end;

procedure TForm1.ToolButton11Click(Sender: TObject);
begin
  Grid.Dots.SetSize(128, 128);
end;

procedure TForm1.ToolButton12Click(Sender: TObject);
begin
  Grid.Dots.SetSize(32, 32);
end;

procedure TForm1.ToolButton13Click(Sender: TObject);
begin
  Grid.Dots.SetSize(16, 16);
end;

procedure TForm1.MergeBtnClick(Sender: TObject);
begin
  Grid.CurrentMerge := MergeBtn.Down;
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

