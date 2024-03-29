unit mnSynParamsHint;
{$mode objfpc}{$H+}
{**
 *  MiniLib project
 *
 *  This file is part of the "Mini Library"
 *
 * @url       http://www.sourceforge.net/projects/minilib
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey
 *}

{$DEFINE HintClickWorkaround} // Workaround for issue 21952

interface

uses
  Classes, SysUtils, Types, Character,
  // LCL
  LCLProc, LCLIntf, LCLType, LMessages, Graphics, Forms,
  Controls, StdCtrls, ExtCtrls, Menus, Themes,
  // LazUtils
  LazUTF8,
  // SynEdit
  SynEditMiscProcs, SynEditKeyCmds, SynEdit, SynEditTypes, SynEditPlugins, mnSynCompletion;

type
  TSynShowParamsHint = class;

  { TSynBaseHint }

  TSynBaseHint = class(THintWindow)
  private
    FBorderColor: TColor;
    FParamsHint: TSynShowParamsHint;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    function CalcHintRect(MaxWidth: Integer; const AHint: string; AData: pointer): TRect; override;
    procedure Paint; override;
    property BorderColor: TColor read FBorderColor write FBorderColor;
  end;

  TOnGetHintString = procedure(AEditor: TCustomSynEdit; Token: string; ParamIndex: Integer; out AHint: String) of object;
  TOnGetHintExists = procedure(AEditor: TCustomSynEdit; Token: string; FunctionsOnly: Boolean; var Exists: Boolean) of object;

  { TSynShowParamsHint }

  TSynShowParamsHint = class(TLazSynMultiEditPlugin) //experimental
  private
    FEndOfTokenChr: string;
    FHint: TSynBaseHint;
    FExecCommandID: TSynEditorCommand;
    FOnGetHintString: TOnGetHintString;
    FOnGetHintExists: TOnGetHintExists;
    FShortCut: TShortCut;
    FParenChr: string;
    FLongLineHintTime: Integer;
    FHintTimer: TTimer;
    FUsePrettyText: Boolean;
    //Cache it
    FLastToken: string;
    FLastParamIndex: Integer;
    FLastHint: string;
  protected
    function GetPreviousToken(aEditor: TCustomSynEdit; out Value: string; out Index: Integer): Boolean;

    {$IFDEF HintClickWorkaround}
    procedure HintWindowMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    {$ENDIF}
    procedure OnHintTimer(Sender: TObject);

    procedure ShowHint(AEditor: TCustomSynEdit);
    function HideHint: Boolean;

    procedure DoGetHintString(AEditor: TCustomSynEdit; Token: string; ParamIndex: Integer; out AHint: String); virtual;
    function GetHintString(AEditor: TCustomSynEdit; Token: string; ParamIndex: Integer): string;

    function DoGetHintExists(AEditor: TCustomSynEdit; Token: string): Boolean; virtual;
    function GetHintExists(AEditor: TCustomSynEdit; Token: string): Boolean;

    function FindFunction(AEditor: TCustomSynEdit; out charIndex, AIndex: Integer; out AString: string): Boolean;
    function FindToken(AEditor: TCustomSynEdit; out charIndex, AIndex: Integer; out AString: string): Boolean;


    procedure KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure StatusChange(Sender: TObject; Changes: TSynStatusChanges); virtual;

    procedure SetEditor(const Value: TCustomSynEdit); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure DoEditorAdded(AValue: TCustomSynEdit); override;
    procedure DoEditorRemoving(AValue: TCustomSynEdit); override;
    procedure SetShortCut(Value: TShortCut);
    procedure TranslateKey(Sender: TObject; Code: word; SState: TShiftState;
      var Data: pointer; var IsStartOfCombo: boolean; var Handled: boolean;
      var Command: TSynEditorCommand; FinishComboOnly: Boolean;
      var ComboKeyStrokes: TSynEditKeyStrokes);
    procedure ProcessSynCommand(Sender: TObject; AfterProcessing: boolean;
              var Handled: boolean; var Command: TSynEditorCommand;
              var AChar: TUTF8Char; Data: pointer; HandlerData: pointer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute(aEditor: TCustomSynEdit);
    property Hint: TSynBaseHint read FHint;
  published
    //use () in pair
    property ParenChr: string read FParenChr write FParenChr; //TODO
    property ShortCut: TShortCut read FShortCut write SetShortCut;
    property ExecCommandID: TSynEditorCommand read FExecCommandID write FExecCommandID;
    property Editor;
    property UsePrettyText: Boolean read FUsePrettyText write FUsePrettyText;
    property EndOfTokenChr: string read FEndOfTokenChr write FEndOfTokenChr;
    property OnGetHintString: TOnGetHintString read FOnGetHintString write FOnGetHintString;
    property OnGetHintExists: TOnGetHintExists read FOnGetHintExists write FOnGetHintExists;
  end;

implementation

{ TSynBaseHint }

constructor TSynBaseHint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Canvas.Brush.Style := bsSolid;
  BorderWidth := 1;
end;

function TSynBaseHint.CalcHintRect(MaxWidth: Integer; const AHint: string; AData: pointer): TRect;
var
  S: string;
begin
  if FParamsHint.UsePrettyText then
    S := StripFormatCommands(Hint)
  else
    S := AHint;
  Result := Rect(0, 0, Canvas.TextWidth(S) + BorderWidth * 2 + 4, Canvas.TextHeight(S) + BorderWidth * 2 + 4); //4 margines
end;

procedure TSynBaseHint.Paint;
var
  R: TRect;
begin
  //inherited;
  R := ClientRect;

  Canvas.Brush.Color := Color;

  Canvas.Pen.Color := BorderColor;
  Canvas.Pen.Width := BorderWidth;
//  Canvas.FillRect(R);
  Canvas.Rectangle(R);

  InflateRect(R, -BorderWidth, -BorderWidth);
  InflateRect(R, -2, -2);
  if FParamsHint.UsePrettyText then
    FormattedTextOut(Canvas, R, Hint, False, nil)
  else
    Canvas.TextOut(R.Left, R.Top, Hint);
end;

{ TSynShowParamsHint }

constructor TSynShowParamsHint.Create(AOwner: TComponent);
begin
  inherited;
  FParenChr := '()';
  FShortCut := Menus.ShortCut(Ord(' '), [ssCtrl, ssShift]);
  FExecCommandID := AllocatePluginKeyRange(1);
  FHint := TSynBaseHint.Create(Self);
  FHint.FParamsHint := Self;
  FHint.FormStyle := fsSystemStayOnTop;
  {$IFDEF HintClickWorkaround}
  FHint.OnMouseDown :=@HintWindowMouseDown;
  {$ENDIF}
  FHintTimer := TTimer.Create(nil);
  FHintTimer.Enabled := False;
  FHintTimer.OnTimer := @OnHintTimer;
  FHintTimer.Interval := 0;
  FLongLineHintTime := 0;
end;

procedure TSynShowParamsHint.SetShortCut(Value: TShortCut);
begin
  FShortCut := Value;
end;

destructor TSynShowParamsHint.Destroy;
begin
  FreeAndNil(FHint);
  FreeAndNil(FHintTimer);
  inherited;
end;

procedure TSynShowParamsHint.Execute(aEditor: TCustomSynEdit);
begin
  ShowHint(aEditor);
end;

procedure TSynShowParamsHint.TranslateKey(Sender: TObject; Code: word; SState: TShiftState;
  var Data: pointer; var IsStartOfCombo: boolean; var Handled: boolean;
  var Command: TSynEditorCommand; FinishComboOnly: Boolean;
  var ComboKeyStrokes: TSynEditKeyStrokes);
var
  i: integer;
  ShortCutKey: Word;
  ShortCutShift: TShiftState;
begin
  if (Code = VK_UNKNOWN) or Handled or FinishComboOnly or (FExecCommandID = ecNone) then exit;

  i := IndexOfEditor(Sender as TCustomSynEdit);
  if i >= 0 then begin
    ShortCutToKey(FShortCut, ShortCutKey, ShortCutShift);
    if (SState = ShortCutShift) and (Code = ShortCutKey) then begin
      Command := FExecCommandID;
      Handled := True;
    end;
  end;
end;

function SynParamsHintCallBack(Sender:TObject; S: string): Boolean;
begin
  Result := True;
end;

procedure TSynShowParamsHint.ProcessSynCommand(Sender: TObject; AfterProcessing: boolean;
  var Handled: boolean; var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer;
  HandlerData: pointer);
var
  i: integer;
begin
  if Handled or (Command <> FExecCommandID) then
    exit;

  i := IndexOfEditor(Sender as TCustomSynEdit);
  if i >= 0 then begin
    with sender as TCustomSynEdit do begin
      if not ReadOnly then begin
        Editor := Sender as TCustomSynEdit; // Will set Form.SetCurrentEditor
        Execute(Editor);
        Handled := True;
      end;
    end;
  end;
end;

procedure TSynShowParamsHint.DoGetHintString(AEditor: TCustomSynEdit; Token: string; ParamIndex: Integer; out AHint: String);
begin
end;

function TSynShowParamsHint.GetHintString(AEditor: TCustomSynEdit; Token: string; ParamIndex: Integer): string;
begin
  DoGetHintString(AEditor, Token, ParamIndex, Result);
  if Assigned(FOnGetHintString) then
    FOnGetHintString(AEditor, Token, ParamIndex, Result);
  if Result = '' then
    Result := Token;
end;

function TSynShowParamsHint.DoGetHintExists(AEditor: TCustomSynEdit; Token: string): Boolean;
begin
  Result := True;
end;

function TSynShowParamsHint.GetHintExists(AEditor: TCustomSynEdit; Token: string): Boolean;
begin
  Result := DoGetHintExists(AEditor, Token);
  if Assigned(FOnGetHintExists) then
    FOnGetHintExists(AEditor, Token, True, Result);
end;

//* ported from Delphi version of SynEdit example
//TODO need to bypass string " or ' with escape
function TSynShowParamsHint.FindFunction(AEditor: TCustomSynEdit; out charIndex, AIndex: Integer; out AString: string): Boolean;
var
  aLine, lookup: string;
  SavePos, X, StartX,
  ParenCounter,
  lLocation    : Integer;
  FoundMatch     : Boolean;
begin
  with Editor do
  begin
    aLine := LineText;

    //go back from the cursor and find the first open paren
    X := CaretX;
    if X > length(aLine) then
      X := length(aLine)
    else
      Dec(X);
    AIndex := 0;
    charIndex := 0;
    FoundMatch := False;
    lLocation := 0;
    while (X > 0) and not(FoundMatch) do
    begin
      if aLine[X] = ',' then
      begin
        Inc(lLocation);
        Dec(X);
      end else if aLine[X] = ')' then
      begin
        //We found a close, go till it's opening paren
        ParenCounter := 1;
        dec(X);
        while (X > 0) and (ParenCounter > 0) do
        begin
          if aLine[X] = ')' then inc(ParenCounter)
          else if aLine[X] = '(' then dec(ParenCounter);
          dec(X);
        end;
        if X > 0 then dec(X);  //eat the open paren
      end else if aLine[X] = '(' then
      begin
        //we have a valid open paren, lets see what the word before it is
        StartX := X;
        while (X > 0) and not IsIdentChar(aLine[X])do
          Dec(X);
        if X > 0 then
        begin
          SavePos := X;
          While (X > 0) and IsIdentChar(aLine[X]) do
            Dec(X);
          Inc(X);
          lookup := Copy(aLine, X, SavePos - X + 1);
          FoundMatch := GetHintExists(AEditor, Lookup);
          if FoundMatch then
          begin
            AString := lookup;
            AIndex := lLocation;
            charIndex := X;
          end
          else
          begin
            X := StartX;
            dec(X);
          end;
        end;
      end
      else
        Dec(X)
    end;
  end;
  Result := FoundMatch;
end;

function TSynShowParamsHint.FindToken(AEditor: TCustomSynEdit; out charIndex, AIndex: Integer; out AString: string): Boolean;
var
  StartX, EndX: integer;
  Line: string;
begin
  //if (agent == llGetOwner() || llGetPermiss|ions())  //* must return llGetPermissions

  AEditor.GetWordBoundsAtRowCol(AEditor.LogicalCaretXY, StartX, EndX);
  Line := AEditor.LineText;
  AString := Copy(Line, StartX, EndX - StartX);
  charIndex := StartX;
  AIndex := 0;
  Result := (AString <> '') and GetHintExists(AEditor, AString);

  if not Result then
  begin
    Result := FindFunction(AEditor, charIndex, AIndex, AString);
    //Result := AEditor.GetWordAtRowCol(AEditor.LogicalCaretXY);
    //Result := GetPreviousToken(AEditor, Astring, charIndex);
  end;
end;

procedure TSynShowParamsHint.ShowHint(AEditor: TCustomSynEdit);
var
  R: TRect;
  P: TPoint;
  charIndex, paramIndex: Integer;
  AToken: string;
begin
  if FindToken(aEditor, charIndex, paramIndex, AToken) then
  begin
    FHintTimer.Enabled := False;
    if AToken <> '' then
    begin
      if (FLastToken = AToken) and (FLastParamIndex = paramIndex) then //reduce recall huge find in list
        FHint.Hint := FLastHint
      else
      begin
        FHint.Hint := GetHintString(AEditor, AToken, paramIndex);
        FLastToken := AToken;
        FLastParamIndex := paramIndex;
        FLastHint := FHint.Hint;
      end;
      if FHint.Hint <> '' then
      begin
        FHint.Font.Assign(AEditor.Font);
        //* https://gitlab.com/freepascal.org/lazarus/lazarus/-/issues/32260
        FHint.Font.PixelsPerInch := Screen.PixelsPerInch;

        FHint.Color := AEditor.Color;
        P := Point(charIndex, AEditor.LogicalCaretXY.Y);
        P := AEditor.LogicalToPhysicalPos(P);
        //charIndex := AEditor.LogicalToPhysicalPos(Point(charIndex, AEditor.LogicalCaretXY.Y));
        P := AEditor.RowColumnToPixels(P);
        P := AEditor.ClientToScreen(P);
        P.Y := P.Y + AEditor.LineHeight + 1;
        //P := AEditor.ClientToScreen(Point(AEditor.CaretXPix, AEditor.CaretYPix + AEditor.LineHeight + 1));
        R := FHint.CalcHintRect(Application.MainForm.Monitor.Width, FHint.Hint, nil);
        OffsetRect(R, P.X, P.Y);
        //InflateRect(R, 2, 2);

        FHint.HintRect := R;

        if (not FHint.IsVisible) and (FLongLineHintTime > 0) then
          FHintTimer.Enabled := True
        else
          OnHintTimer(nil);
      end;
    end
    else
      FHint.Hide;
  end
  else
    FHint.Hide;
end;

function TSynShowParamsHint.GetPreviousToken(aEditor: TCustomSynEdit; out Value: string; out Index: Integer): Boolean;
var
  s: string;
  i: integer;
begin
  Index := -1;
  Value := '';
  Result := False;
  if aEditor <> nil then
  begin
    s := aEditor.LineText;
    i := aEditor.LogicalCaretXY.X - 1;
    if i > length(s) then
      Result := False
    else
    begin
      while (i > 0) and (s[i] > ' ') and (pos(s[i], FEndOfTokenChr) = 0) do
        Begin
          Dec(i);
        end;
      Value := Copy(s, i + 1, aEditor.LogicalCaretXY.X - i - 1);
      Index := i + 1;
      Result := True;
    end;
  end;
end;

{$IFDEF HintClickWorkaround}
procedure TSynShowParamsHint.HintWindowMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
{var
  p: TPoint;}
begin
{  p := ScreenToClient(FHint.ClientToScreen(Point(X, Y)));
   MouseDown(Button, Shift, p.X, p.Y); }
end;
{$ENDIF}

procedure TSynShowParamsHint.OnHintTimer(Sender: TObject);
begin
  FHintTimer.Enabled := False;
  FHint.ActivateHint(FHint.Hint);
  FHint.Invalidate;
end;

function TSynShowParamsHint.HideHint: Boolean;
begin
  FHintTimer.Enabled := False;
  Result := FHint.Visible = True;
  FHint.Visible := False;
end;

procedure TSynShowParamsHint.KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Handled: Boolean;
begin
  inherited;
  if Key = VK_UNKNOWN then
    exit;
  Handled:=true;
  case Key of
    VK_RETURN,
    VK_ESCAPE:
    begin
      HideHint;
      Handled := False;
    end;
    VK_UP,
    VK_DOWN:
    begin
      HideHint;
      Handled := False;
    end
    else
    begin
      if FHint.Visible then
      begin
        ShowHint(Sender as TCustomSynEdit);
      end;
      Handled := False;
    end;
  end;
  if Handled then
    Key:=VK_UNKNOWN;
end;

procedure TSynShowParamsHint.StatusChange(Sender: TObject; Changes: TSynStatusChanges);
begin
  if (scFocus in Changes) or (scCaretY in Changes) then
  begin
    HideHint;
  end;
end;

procedure TSynShowParamsHint.SetEditor(const Value: TCustomSynEdit);
begin
  if Editor <> Value then
  begin
    if Editor <> nil then
    begin
      Editor.UnregisterBeforeKeyDownHandler(@KeyDown);
      RemoveFreeNotification( Editor );
    end;
    inherited;
    if Editor <> nil then
    begin
      Editor.RegisterBeforeKeyDownHandler(@KeyDown);
      FreeNotification(Editor);
    end;
  end;
end;

procedure TSynShowParamsHint.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) then
  begin
    if Editor = AComponent then
      Editor := nil
    else if AComponent is TCustomSynEdit then
      RemoveEditor(TCustomSynEdit(AComponent));
  end;
  inherited;
end;

procedure TSynShowParamsHint.DoEditorAdded(AValue: TCustomSynEdit);
begin
  inherited DoEditorAdded(AValue);
  AValue.RegisterCommandHandler(@ProcessSynCommand, nil);
  AValue.RegisterStatusChangedHandler(@StatusChange, [scCaretY, scFocus]);
  AValue.RegisterKeyTranslationHandler(@TranslateKey);
end;

procedure TSynShowParamsHint.DoEditorRemoving(AValue: TCustomSynEdit);
begin
  inherited DoEditorRemoving(AValue);
  AValue.UnregisterCommandHandler(@ProcessSynCommand);
  AValue.UnregisterStatusChangedHandler(@StatusChange);
  AValue.UnRegisterKeyTranslationHandler(@TranslateKey);
end;

end.
