{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynCompletionProposal.pas, released 2000-04-11.
The Original Code is based on mwCompletionProposal.pas by Cyrille de Brebisson,
part of the mwEdit component suite.
Portions created by Cyrille de Brebisson are Copyright (C) 1999
Cyrille de Brebisson. All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id$

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit mnSynCompletion;

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
  SynEditMiscProcs, SynEditKeyCmds, SynEdit, SynEditTypes, SynEditPlugins;

type
  TSynBaseCompletionPaintItem =
    function(const AKey: string; ACanvas: TCanvas;
             X, Y: integer; Selected: boolean; Index: integer
            ): boolean of object;
  TSynBaseCompletionMeasureItem =
    function(const AKey: string; ACanvas: TCanvas;
      Selected: boolean; Index: integer): TPoint of object;
  TCodeCompletionEvent = procedure(var Value: string;
                                   SourceValue: string;
                                   var SourceStart, SourceEnd: TPoint;
                                   KeyChar: TUTF8Char;
                                   Shift: TShiftState) of object;
  TValidateEvent = procedure(Sender: TObject;
                             KeyChar: TUTF8Char;
                             Shift: TShiftState) of object;
  TSynBaseCompletionSearchPosition = procedure(var APosition :integer) of object;

  TSynVirtualCompletionForm = class;

  { TSynBaseCompletionHint }

  TSynBaseCompletionHint = class(THintWindow)
  private
    FCompletionForm: TSynVirtualCompletionForm;
    FIndex: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    function CalcHintRect(MaxWidth: Integer; const AHint: string;
                          AData: pointer): TRect; override;
    procedure Paint; override;
    property Index: Integer read FIndex write FIndex;
  end;

  TSynCompletionLongHintType = (sclpNone,
                                sclpExtendRightOnly,
                                sclpExtendHalfLeft,
                                sclpExtendUnlimitedLeft
                               );

  { TSynBaseCompletionFormSizeDrag }

  TSynBaseCompletionFormSizeDrag = class(TPanel)
  private
    FMouseDownPos, FMouseLastPos, FWinSize: TPoint;
  protected
    procedure DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
      const AXProportion, AYProportion: Double); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Paint; override;
  end;

  TSynBaseCompletionFormScrollBar = class(TScrollBar)
  protected
    procedure DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
      const AXProportion, AYProportion: Double); override;
  end;

  { TSynVirtualCompletionForm }

  TSynVirtualCompletionForm = class(TForm)
    procedure SDKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SDKeyPress(Sender: TObject; var Key: char);
    procedure SDUtf8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  protected
    FCurrentString: string;
    FOnKeyPress: TKeyPressEvent;
    FOnKeyDelete: TNotifyEvent;
    FOnPaintItem: TSynBaseCompletionPaintItem;
    FPosition: Integer;
    FNbLinesInWindow: Integer;
    FFontHeight: integer;
    FResizeLock: Integer;
    Scroll: TScrollBar;
    SizeDrag: TSynBaseCompletionFormSizeDrag;
    FOnValidate: TValidateEvent;
    FOnCancel: TNotifyEvent;
    FClSelect: TColor;
    FCaseSensitive: Boolean;
    FUsePrettyText: Boolean;
    FBackgroundColor: TColor;
    FDrawBorderColor: TColor;
    FOnSearchPosition: TSynBaseCompletionSearchPosition;
    FOnKeyCompletePrefix: TNotifyEvent;
    FOnKeyNextChar: TNotifyEvent;
    FOnKeyPrevChar: TNotifyEvent;
    FTextColor: TColor;
    FTextSelectedColor: TColor;
    FHint: TSynBaseCompletionHint;
    FHintTimer: TTimer;
    FLongLineHintTime: Integer;
    FLongLineHintType: TSynCompletionLongHintType;
    FMouseWheelAccumulator: Integer;
    procedure DoEditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoEditorKeyPress(Sender: TObject; var Key: char);
    procedure DoEditorUtf8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure UTF8KeyPress(var UTF8Key: TUTF8Char); override;
    procedure SetCurrentString(const Value: string);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: char); override;
    procedure AddCharAtCursor(AUtf8Char: TUTF8Char); virtual;
    procedure DeleteCharAfterCursor; virtual;
    procedure DeleteCharBeforeCursor; virtual;
    procedure Paint; override;
    procedure AppDeactivated(Sender: TObject); // Because Form.Deactivate isn't called
    procedure Deactivate; override;
    procedure SelectPrec;
    procedure SelectNext;
    procedure ScrollChange(Sender: TObject);
    procedure ScrollGetFocus(Sender: TObject);
    procedure ScrollScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure SetPosition(Value: Integer);
    procedure SetNbLinesInWindow(const Value: Integer);
    {$IFDEF HintClickWorkaround}
    procedure HintWindowMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    {$ENDIF}
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure DoOnResize; override;
    procedure SetBackgroundColor(const AValue: TColor);
    procedure FontChanged(Sender: TObject); override;
    procedure WMMouseWheel(var Msg: TLMMouseEvent); message LM_MOUSEWHEEL;
  private
    FCurrentEditor: TCustomSynEdit; // Must only be set via TSynCompletion.SetEditor
    FDoubleClickSelects: Boolean;
    FDrawBorderWidth: Integer;
    FOnDragResized: TNotifyEvent;
    FOnMeasureItem: TSynBaseCompletionMeasureItem;
    FOnPositionChanged: TNotifyEvent;
    FShowSizeDrag: Boolean;
    FHintLock: Integer;
    FSmartEdit: Boolean;
    procedure SetCurrentEditor(const AValue: TCustomSynEdit);
    procedure SetDrawBorderWidth(const AValue: Integer);
    procedure SetLongLineHintTime(const AValue: Integer);
    procedure EditorStatusChanged(Sender: TObject; Changes: TSynStatusChanges);
    procedure SetShowSizeDrag(const AValue: Boolean);
  protected
    procedure RegisterHandlers(EditOnly: Boolean = False);
    procedure UnRegisterHandlers(EditOnly: Boolean = False);
    procedure SetVisible(Value: Boolean); override;
    procedure IncHintLock;
    procedure DecHintLock;
    procedure DoOnDragResize(Sender: TObject);
    procedure ClearCurrentString;
    //Callback functions
    function GetItemText(Index: Integer): string; virtual; abstract;
    function GetItemDisplay(Index: Integer): string; virtual; abstract;
    function GetItemsCount: Integer; virtual; abstract;
  public
    constructor Create(AOwner: Tcomponent); override;
    destructor Destroy; override;
    function Focused: Boolean; override;
    procedure ShowItemHint(AIndex: Integer);
    procedure OnHintTimer(Sender: TObject);
    // Must only be set via TSynCompletion.SetEditor
    property CurrentEditor: TCustomSynEdit read FCurrentEditor;
    property InsertList[Index: Integer]: string read GetItemText;
    property ItemList[Index: Integer]: string read GetItemDisplay;
    property ItemsCount: Integer read GetItemsCount;
  published
    property CurrentString: string read FCurrentString write SetCurrentString;
    property OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
    property OnKeyDelete: TNotifyEvent read FOnKeyDelete write FOnKeyDelete;
    property OnPaintItem: TSynBaseCompletionPaintItem read FOnPaintItem
      write FOnPaintItem;
    property OnMeasureItem: TSynBaseCompletionMeasureItem read FOnMeasureItem
      write FOnMeasureItem;
    property OnValidate: TValidateEvent read FOnValidate write FOnValidate;
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
    property Position: Integer read FPosition write SetPosition;
    property NbLinesInWindow: Integer read FNbLinesInWindow write SetNbLinesInWindow;
    property ClSelect: TColor read FClSelect write FClSelect;
    property CaseSensitive: boolean read FCaseSensitive write FCaseSensitive;
    property UsePrettyText: Boolean read FUsePrettyText write FUsePrettyText;
    property FontHeight:integer read FFontHeight;
    property OnSearchPosition:TSynBaseCompletionSearchPosition
      read FOnSearchPosition write FOnSearchPosition;
    property OnKeyCompletePrefix: TNotifyEvent read FOnKeyCompletePrefix write FOnKeyCompletePrefix;// e.g. Tab
    property OnKeyNextChar: TNotifyEvent read FOnKeyNextChar write FOnKeyNextChar;// e.g. arrow right
    property OnKeyPrevChar: TNotifyEvent read FOnKeyPrevChar write FOnKeyPrevChar;// e.g. arrow left
    property OnPositionChanged: TNotifyEvent read FOnPositionChanged write FOnPositionChanged;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor;
    property DrawBorderColor: TColor read FDrawBorderColor write FDrawBorderColor;
    property DrawBorderWidth: Integer read FDrawBorderWidth write SetDrawBorderWidth;
    property TextColor: TColor read FTextColor write FTextColor;
    property TextSelectedColor: TColor
      read FTextSelectedColor write FTextSelectedColor;
    property LongLineHintTime: Integer read FLongLineHintTime
             write SetLongLineHintTime default 0;
    property LongLineHintType: TSynCompletionLongHintType read FLongLineHintType
             write FLongLineHintType default sclpExtendRightOnly;
    property DoubleClickSelects: Boolean read FDoubleClickSelects write FDoubleClickSelects default True;
    property ShowSizeDrag: Boolean read FShowSizeDrag write SetShowSizeDrag default False;
    property OnDragResized: TNotifyEvent read FOnDragResized write FOnDragResized;
    property SmartEdit: Boolean read FSmartEdit write FSmartEdit;
  end;

  TSynVirtualCompletionFormClass = class of TSynVirtualCompletionForm;

  { TSynVirtualCompletion }

  TSynVirtualCompletion = class;

  TOnBeforeExeucteFlag = (befAbort);
  TOnBeforeExeucteFlags = set of TOnBeforeExeucteFlag;

  TSynVirtualCompletion = class(TLazSynMultiEditPlugin)
  private
    FAutoUseSingleIdent: Boolean;
    Form: TSynVirtualCompletionForm;
    FAddedPersistentCaret, FChangedNoneBlink: boolean;
    FOnExecute: TNotifyEvent;
    FWidth: Integer;
    function GetCaseSensitive: boolean;
    function GetUsePrettyText: boolean;
    function GetClSelect: TColor;
    function GetDoubleClickSelects: Boolean;
    function GetLongLineHintTime: Integer;
    function GetLongLineHintType: TSynCompletionLongHintType;
    function GetOnKeyDown: TKeyEvent;
    function GetOnMeasureItem: TSynBaseCompletionMeasureItem;
    function GetOnPositionChanged: TNotifyEvent;
    function GetShowSizeDrag: Boolean;
    procedure SetCaseSensitive(const AValue: boolean);
    procedure SetUsePrettyText(const AValue: boolean);
    procedure SetClSelect(const Value: TColor);
    function GetCurrentString: string;

    function GetNbLinesInWindow: Integer;
    function GetOnCancel: TNotifyEvent;
    function GetOnKeyPress: TKeyPressEvent;
    function GetOnPaintItem: TSynBaseCompletionPaintItem;
    function GetOnValidate: TValidateEvent;
    function GetPosition: Integer;
    procedure SetCurrentString(const Value: string);
    procedure SetDoubleClickSelects(const AValue: Boolean);
    procedure SetLongLineHintTime(const AValue: Integer);
    procedure SetLongLineHintType(const AValue: TSynCompletionLongHintType);
    procedure SetNbLinesInWindow(const Value: Integer);
    procedure SetOnCancel(const Value: TNotifyEvent);
    procedure SetOnKeyDown(const AValue: TKeyEvent);
    procedure SetOnKeyPress(const Value: TKeyPressEvent);
    procedure SetOnMeasureItem(const AValue: TSynBaseCompletionMeasureItem);
    procedure SetOnPositionChanged(const AValue: TNotifyEvent);
    procedure SetOnPaintItem(const Value: TSynBaseCompletionPaintItem);
    procedure SetPosition(const Value: Integer);
    procedure SetOnValidate(const Value: TValidateEvent);
    function GetOnKeyDelete: TNotifyEvent;
    procedure SetOnKeyDelete(const Value: TNotifyEvent);
    procedure SetShowSizeDrag(const AValue: Boolean);
    procedure SetWidth(Value: Integer);
    function GetOnUTF8KeyPress: TUTF8KeyPressEvent;
    procedure SetOnUTF8KeyPress(const AValue: TUTF8KeyPressEvent);
    function GetFontHeight:integer;
    function GetOnSearchPosition:TSynBaseCompletionSearchPosition;
    procedure SetOnSearchPosition(NewValue :TSynBaseCompletionSearchPosition);
    function GetOnKeyCompletePrefix: TNotifyEvent;
    procedure SetOnKeyCompletePrefix(const AValue: TNotifyEvent);
    function GetOnKeyNextChar: TNotifyEvent;
    procedure SetOnKeyNextChar(const AValue: TNotifyEvent);
    function GetOnKeyPrevChar: TNotifyEvent;
    procedure SetOnKeyPrevChar(const AValue: TNotifyEvent);
  protected
    procedure DoBeforeExecute(var ACurrentString: String; var APosition: Integer; var AnX, AnY: Integer; var AnResult: TOnBeforeExeucteFlags); virtual;
    function GetCompletionFormClass: TSynVirtualCompletionFormClass; virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute(s: string; x, y: integer); overload;
    procedure Execute(s: string; TopLeft: TPoint); overload;
    procedure Execute(s: string; TokenRect: TRect); overload; // Excute below or above the token // may be extended to adjust left corner too
    procedure Deactivate;
    function IsActive: boolean;
    function TheForm: TSynVirtualCompletionForm;
    property OnKeyDown: TKeyEvent read GetOnKeyDown write SetOnKeyDown;
    property OnUTF8KeyPress: TUTF8KeyPressEvent read GetOnUTF8KeyPress
                                                write SetOnUTF8KeyPress;
    property OnKeyPress: TKeyPressEvent read GetOnKeyPress write SetOnKeyPress;
    property OnKeyDelete: TNotifyEvent read GetOnKeyDelete write SetOnKeyDelete;
    property OnValidate: TValidateEvent read GetOnValidate write SetOnValidate;
    property OnCancel: TNotifyEvent read GetOnCancel write SetOnCancel;
    property CurrentString: string read GetCurrentString write SetCurrentString;
    property FontHeight: integer read GetFontHeight;
    property ClSelect: TColor read GetClSelect write SetClSelect; deprecated; // use SelectedColor
    property NbLinesInWindow: Integer read GetNbLinesInWindow write SetNbLinesInWindow; deprecated;
  published
    property OnExecute: TNotifyEvent read FOnExecute write FOnExecute;
    property OnPaintItem: TSynBaseCompletionPaintItem
             read GetOnPaintItem write SetOnPaintItem;
    property OnMeasureItem: TSynBaseCompletionMeasureItem read GetOnMeasureItem
             write SetOnMeasureItem;

    property Position: Integer read GetPosition write SetPosition;
    property LinesInWindow: Integer read GetNbLinesInWindow
                                      write SetNbLinesInWindow;
    property OnSearchPosition: TSynBaseCompletionSearchPosition
                             read GetOnSearchPosition write SetOnSearchPosition;
    property OnKeyCompletePrefix: TNotifyEvent read GetOnKeyCompletePrefix
                                               write SetOnKeyCompletePrefix;// e.g. Tab
    property OnKeyNextChar: TNotifyEvent read GetOnKeyNextChar
                                         write SetOnKeyNextChar;// e.g. arrow right
    property OnKeyPrevChar: TNotifyEvent read GetOnKeyPrevChar
                                         write SetOnKeyPrevChar;// e.g. arrow left
    property OnPositionChanged: TNotifyEvent read GetOnPositionChanged
                                             write SetOnPositionChanged;
    property SelectedColor: TColor read GetClSelect write SetClSelect;
    property CaseSensitive: boolean read GetCaseSensitive write SetCaseSensitive;
    property UsePrettyText: boolean read GetUsePrettyText write SetUsePrettyText;
    property Width: Integer read FWidth write SetWidth;
    property LongLineHintTime: Integer read GetLongLineHintTime
             write SetLongLineHintTime default 0;
    property LongLineHintType: TSynCompletionLongHintType read GetLongLineHintType
             write SetLongLineHintType default sclpExtendRightOnly;
    property DoubleClickSelects: Boolean read GetDoubleClickSelects write SetDoubleClickSelects default True;
    property ShowSizeDrag: Boolean read GetShowSizeDrag write SetShowSizeDrag default False;
    property AutoUseSingleIdent: Boolean read FAutoUseSingleIdent write FAutoUseSingleIdent;
  end;

  TSynBaseCompletion = class;

  { TSynBaseCompletionForm }

  TSynBaseCompletionForm = class(TSynVirtualCompletionForm)
  private
    FItemList: TStrings;
  protected
    procedure StringListChange(Sender: TObject);

    function GetItemText(Index: Integer): string; override;
    function GetItemDisplay(Index: Integer): string; override;
    function GetItemsCount: Integer; override;

    procedure SetItemList(const Value: TStrings);
  public
    constructor Create(AOwner: Tcomponent); override;
    destructor Destroy; override;
    property ItemList: TStrings read FItemList write SetItemList;
  end;

  { TSynCompletionForm }

  TSynCompletionForm = class(TSynBaseCompletionForm)
  protected
  public
    constructor Create(AOwner: Tcomponent); override;
  end;

  TOnBeforeExecuteEvent = procedure(
    ASender: TSynBaseCompletion;
    var ACurrentString: String;
    var APosition: Integer; // Defaults to -1. If left at -1 position will be calculated from CurrentString
    var AnX, AnY: Integer;        // Coordinates for the form
    var AnResult: TOnBeforeExeucteFlags
  ) of object;

  { TSynBaseCompletion }
  {*
    This show dropdown list of available keywords
  *}
  TSynBaseCompletion = class(TSynVirtualCompletion)
  private
    FOnBeforeExecute: TOnBeforeExecuteEvent;
    FShortCut: TShortCut;
    FExecCommandID: TSynEditorCommand;
    FEndOfTokenChr: string;
    FOnCodeCompletion: TCodeCompletionEvent;
    FToggleReplacesWhole: boolean;
    procedure Cancel(Sender: TObject);
    procedure Validate(Sender: TObject; KeyChar: TUTF8Char; Shift: TShiftState);
    function GetPreviousToken(FEditor: TCustomSynEdit): string;
  protected
    procedure OnFormPaint(Sender: TObject);
    procedure SetEditor(const Value: TCustomSynEdit); override;
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
    procedure DoBeforeExecute(var ACurrentString: String; var APosition: Integer; var AnX, AnY: Integer; var AnResult: TOnBeforeExeucteFlags); override;
  public
    constructor Create(AOwner: TComponent); override;
    function EditorsCount: integer; deprecated; // use EditorCount
    procedure AddCharAtCursor(AUtf8Char: TUTF8Char);
    procedure DeleteCharBeforeCursor;

    procedure Clear; virtual;
    procedure Sort; virtual;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
  published
    property ShortCut: TShortCut read FShortCut write SetShortCut;
    property EndOfTokenChr: string read FEndOfTokenChr write FEndOfTokenChr;
    property OnCodeCompletion: TCodeCompletionEvent
      read FOnCodeCompletion write FOnCodeCompletion;
    property ExecCommandID: TSynEditorCommand read FExecCommandID write FExecCommandID;
    property Editor;
    property ToggleReplaceWhole: boolean read FToggleReplacesWhole write FToggleReplacesWhole;// false=shift replaces left side, true=shift replaces whole word
    property OnBeforeExecute: TOnBeforeExecuteEvent read FOnBeforeExecute write FOnBeforeExecute;
  end;

  { TSynCompletion }

  TSynCompletion = class(TSynBaseCompletion)
  private
    function GetItemList: TStrings;
    procedure SetItemList(AValue: TStrings);
  protected
    function GetCompletionFormClass: TSynVirtualCompletionFormClass; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AddItem(AText: string);
    procedure Clear; override;
    procedure Sort; override;
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    property ItemList: TStrings read GetItemList write SetItemList;
  end;

  { Dual }

  { TSynItemList }

  TSynItemList = class(TStringList)
  protected
    FItemList: TStrings; //Link only to other list to sort
    procedure ExchangeItems(Index1, Index2: Integer); override;
  public
  end;

  { TSynDualCompletionForm }

  TSynDualCompletionForm = class(TSynCompletionForm)
  private
    FInsertList: TStrings;
    procedure SetInsertList(AValue: TStrings);
  protected
    function GetItemText(Index: Integer): string; override;
    function GetItemDisplay(Index: Integer): string; override;
  public
    constructor Create(AOwner: Tcomponent); override;
    destructor Destroy; override;
    property InsertList: TStrings read FInsertList write SetInsertList;
  end;

  { TSynDualCompletion }

  TSynDualCompletion = class(TSynCompletion)
  private
    function GetInsertList: TStrings;
    procedure SetInsertList(AValue: TStrings);
  protected
    function GetCompletionFormClass: TSynVirtualCompletionFormClass; override;
  public
    procedure Clear; override;
    procedure Sort; override;
    procedure AddItem(ADisplayText: string; AInsertText: string; AObject: TObject = nil);
    property InsertList: TStrings read GetInsertList write SetInsertList;
  end;

  { TSynAutoComplete }

  {*
    Complete current word template to actual code
    For extended format of like dci use SynEditAutoComplete.TSynEditAutoComplete
  *}

  TSynAutoComplete = class(TLazSynMultiEditPlugin)
  private
    FExecCommandID: TSynEditorCommand;
    FShortCut: TShortCut;
    fAutoCompleteList: TStrings;
    FEndOfTokenChr: string;
    procedure SetAutoCompleteList(List: TStrings);
  protected
    procedure DoEditorAdded(AValue: TCustomSynEdit); override;
    procedure DoEditorRemoving(AValue: TCustomSynEdit); override;
    procedure SetShortCut(Value: TShortCut);
    function GetPreviousToken(aEditor: TCustomSynEdit): string;
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
    procedure Execute(token: string; aEditor: TCustomSynEdit);
    function EditorsCount: integer;
    function GetTokenList: string;
    function GetTokenValue(Token: string): string;
  published
    property AutoCompleteList: TStrings read fAutoCompleteList
      write SetAutoCompleteList;
    property EndOfTokenChr: string read FEndOfTokenChr write FEndOfTokenChr;
    property ShortCut: TShortCut read FShortCut write SetShortCut;
    property ExecCommandID: TSynEditorCommand read FExecCommandID write FExecCommandID;
    property Editor;
  end;

procedure PrettyTextOut(c: TCanvas; x, y: integer; s: string);
procedure FormattedTextOut(TargetCanvas: TCanvas; const Rect: TRect; const Text: string; Selected: Boolean; Images: TImageList);
function FormattedTextWidth(TargetCanvas: TCanvas; const Text: string; Images: TImageList): Integer;
function StripFormatCommands(const FormattedString: string): string;
function PrettyTextToFormattedString(const APrettyText: string; AlternateBoldStyle: Boolean = False): string;

const
  ecSynCompletionExecute     = ecPluginFirstCompletion +  0;
  ecSynAutoCompletionExecute = ecPluginFirstCompletion +  1;

  // If extending the list, reserve space in SynEditKeyCmds

  ecSynCompletionCount = 2;

implementation

type
  TFormatCommand = (fcNoCommand, fcColor, fcStyle, fcColumn{deprecated 'use tab'}, fcTab, fcHSpace, fcImage);
  TFormatCommands = set of TFormatCommand;

  PFormatChunk = ^TFormatChunk;
  TFormatChunk = record
    Str: string;
    Command: TFormatCommand;
    Data: Pointer;
  end;

  PFormatStyleData = ^TFormatStyleData;
  TFormatStyleData = record
    Style: Char;
    Action: Integer;    // -1 = Reset, +1 = Set, 0 = Toggle
  end;

  TFormatChunkList = class
  private
    FChunks: TList;
    function GetCount: Integer;
    function GetChunk(Index: Integer): PFormatChunk;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(AChunk: PFormatChunk);
    property Count: Integer read GetCount;
    property Chunks[Index: Integer]: PFormatChunk read GetChunk; default;
  end;


const
  AllCommands = [fcColor..High(TFormatCommand)];
  DefTabChars = 8;

{ TSynDualCompletionForm }

procedure TSynDualCompletionForm.SetInsertList(AValue: TStrings);
begin
  FInsertList.Assign(AValue);
end;

function TSynDualCompletionForm.GetItemText(Index: Integer): string;
begin
  if (FInsertList.Count > 0) then
    Result := FInsertList[Index]
  else
    Result := FItemList[Index];
end;

function TSynDualCompletionForm.GetItemDisplay(Index: Integer): string;
begin
  Result :=inherited GetItemDisplay(Index);
end;

constructor TSynDualCompletionForm.Create(AOwner: Tcomponent);
begin
  inherited Create(AOwner);
  FInsertList := TSynItemList.Create;
  (FInsertList as TSynItemList).FItemList := FItemList;
  TStringList(FInsertList).OnChange := @StringListChange;
end;

destructor TSynDualCompletionForm.Destroy;
begin
  FInsertList.Free;
  inherited Destroy;
end;

{ TSynDualCompletion }

function TSynDualCompletion.GetInsertList: TStrings;
begin
  Result := (Form as TSynDualCompletionForm).InsertList;
end;

procedure TSynDualCompletion.SetInsertList(AValue: TStrings);
begin
  (Form as TSynDualCompletionForm).InsertList.Assign(AValue);
end;

function TSynDualCompletion.GetCompletionFormClass: TSynVirtualCompletionFormClass;
begin
  Result := TSynDualCompletionForm;
end;

procedure TSynDualCompletion.Clear;
begin
  inherited Clear;
  InsertList.Clear;
end;

procedure TSynDualCompletion.Sort;
begin
  //inherited DO NOT
  if (InsertList.Count > 0) then
  begin
    if InsertList.Count <> ItemList.Count then
      raise Exception.Create('ItemList and InsertList must same count');
    (InsertList as TStringList).Sort;
  end;
end;

procedure TSynDualCompletion.AddItem(ADisplayText: string; AInsertText: string; AObject: TObject);
begin
  ItemList.AddObject(ADisplayText, AObject);
  InsertList.AddObject(AInsertText, AObject);
end;

{ TSynCompletion }

procedure TSynCompletion.AddItem(AText: string);
begin
  ItemList.Add(AText);
end;

procedure TSynCompletion.Clear;
begin
  ItemList.Clear;
end;

procedure TSynCompletion.Sort;
begin
  (ItemList as TStringList).Sort;
end;

procedure TSynCompletion.BeginUpdate;
begin
  ItemList.BeginUpdate;
end;

procedure TSynCompletion.EndUpdate;
begin
  ItemList.EndUpdate;
end;

function TSynCompletion.GetItemList: TStrings;
begin
  Result := (Form as TSynCompletionForm).ItemList;
end;

procedure TSynCompletion.SetItemList(AValue: TStrings);
begin
  (Form as TSynCompletionForm).ItemList := AValue;
end;

function TSynCompletion.GetCompletionFormClass: TSynVirtualCompletionFormClass;
begin
  Result := TSynCompletionForm;
end;

constructor TSynCompletion.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

{ TSynBaseCompletionForm }

procedure TSynBaseCompletionForm.StringListChange(Sender: TObject);
begin
  if ItemList.Count - NbLinesInWindow < 0 then
    Scroll.Max := 0
  else
    Scroll.Max := ItemList.Count - NbLinesInWindow;
  Position := Position;
end;

function TSynBaseCompletionForm.GetItemText(Index: Integer): string;
begin
  Result := FItemList[Index];
end;

function TSynBaseCompletionForm.GetItemDisplay(Index: Integer): string;
begin
  Result := FItemList[Index];
end;

function TSynBaseCompletionForm.GetItemsCount: Integer;
begin
  Result := FItemList.Count;
end;

procedure TSynBaseCompletionForm.SetItemList(const Value: TStrings);
begin
  FItemList.Assign(Value);
  if Position>=ItemsCount then Position:=-1;
  Invalidate;
end;

constructor TSynBaseCompletionForm.Create(AOwner: Tcomponent);
begin
  inherited Create(AOwner);
  FItemList := TStringList.Create;
  TStringList(FItemList).OnChange := @StringListChange;
end;

destructor TSynBaseCompletionForm.Destroy;
begin
  FItemList.Free;
  inherited Destroy;
end;

{ TSynItemList }

procedure TSynItemList.ExchangeItems(Index1, Index2: Integer);
begin
  inherited ExchangeItems(Index1, Index2);
  if (FItemList<> nil) and (FItemList.Count > 0) then //Counts must identical
    FItemList.Exchange(Index1, Index2);
end;

function TFormatChunkList.GetCount: Integer;
begin
  Result := FChunks.Count;
end;

function TFormatChunkList.GetChunk(Index: Integer): PFormatChunk;
begin
  Result := FChunks[Index];
end;

procedure TFormatChunkList.Clear;
var
  C: PFormatChunk;
  StyleFormatData: PFormatStyleData;
begin
  while FChunks.Count > 0 do
  begin
    C := FChunks.Last;
    FChunks.Delete(FChunks.Count-1);

    case C^.Command of
    fcStyle:
      begin
        StyleFormatData := C^.Data;
        Dispose(StyleFormatData);
      end;
    else;
    end;

    Dispose(C);
  end;
end;

constructor TFormatChunkList.Create;
begin
  inherited Create;
  FChunks := TList.Create;
end;

destructor TFormatChunkList.Destroy;
begin
  Clear;
  FChunks.Free;
  inherited Destroy;
end;

procedure TFormatChunkList.Add(AChunk: PFormatChunk);
begin
  FChunks.Add(AChunk);
end;

function ParseFormatChunks(const FormattedString: string; ChunkList: TFormatChunkList;
  const StripCommands: TFormatCommands): Boolean;
var
  CurChar: Char;
  CurPos: Integer;
  CurrentChunk: string;
  ErrorFound: Boolean;

  procedure NextChar;
  begin
    Inc(CurPos);
    {$IFOPT R+}
    // Work-around Delphi's annoying behaviour of failing the RangeCheck when
    // reading the final #0 char
    if CurPos = Length(FormattedString) +1 then
      CurChar := #0
    else
    {$ENDIF}
    CurChar := FormattedString[CurPos];
  end;

  procedure AddStringChunk;
  var
    C: PFormatChunk;
  begin
    C := New(PFormatChunk);
    C^.Str := CurrentChunk;
    C^.Command := fcNoCommand;
    C^.Data := nil;
    ChunkList.Add(C);

    CurrentChunk := '';
  end;

  procedure AddCommandChunk(ACommand: TFormatCommand; Data: Pointer);
  var
    C: PFormatChunk;
  begin
    C := New(PFormatChunk);
    C^.Str := '';
    C^.Command := ACommand;
    C^.Data := Data;
    ChunkList.Add(C);
  end;

  procedure ParseEscapeSequence;
  var
    Command: string;
    Parameter: string;
    CommandType: TFormatCommand;
    Data: Pointer;
  begin
    Assert(CurChar = '\');
    NextChar;
    if CurChar = '\' then
    begin
      CurrentChunk := CurrentChunk  + '\';
      NextChar;
      Exit;
    end;

    if CurrentChunk <> '' then
      AddStringChunk;

    Command := '';
    while (CurChar <> '{') and (CurPos <= Length(FormattedString)) do
    begin
      Command := Command +CurChar;
      NextChar;
    end;

    if CurChar = '{' then
    begin
      NextChar;
      Parameter := '';
      while (CurChar <> '}') and (CurPos <= Length(FormattedString)) do
      begin
        Parameter := Parameter + CurChar;
        NextChar;
      end;

      if CurChar = '}' then
      begin
        Command := UpperCase(Command);

        Data := nil;
        CommandType := fcNoCommand;

        if Command = 'COLOR' then
        begin
          try
            Data := Pointer(StringToColor(Parameter));
            CommandType := fcColor;
          except
            CommandType := fcNoCommand;
            ErrorFound := True;
          end;
        end else
        if Command = 'COLUMN' then //deprecated
        begin
          if Parameter <> '' then
          begin
            Data := Pointer(StrToInt(Parameter));
          end;
          CommandType := fcColumn;
        end else
        if Command = 'TAB' then
        begin
          if Parameter <> '' then
          begin
            Data := Pointer(StrToInt(Parameter));
          end;
          CommandType := fcTab;
        end else
        if Command = 'HSPACE' then
        begin
          try
            Data := Pointer(StrToInt(Parameter));
            CommandType := fcHSpace;
          except
            CommandType := fcNoCommand;
            ErrorFound := True;
          end;
        end else
        if Command = 'IMAGE' then
        begin
          try
            Data := Pointer(StrToInt(Parameter));
            CommandType := fcImage;
          except
            CommandType := fcNoCommand;
            ErrorFound := True;
          end;
        end else
        if Command = 'STYLE' then
        begin
          if (Length(Parameter) = 2)
            and CharInSet(Parameter[1], ['+', '-', '~'])
            and CharInSet(UpperCase(Parameter[2])[1],
              ['B', 'I', 'U', 'S']) then
          begin
            CommandType := fcStyle;
            if not (fcStyle in StripCommands) then
            begin
              Data := New(PFormatStyleData);
              PFormatStyleData(Data)^.Style := UpperCase(Parameter[2])[1];
              case Parameter[1] of
              '+': PFormatStyleData(Data)^.Action := 1;
              '-': PFormatStyleData(Data)^.Action := -1;
              '~': PFormatStyleData(Data)^.Action := 0;
              end;
            end;
          end else
          begin
            CommandType := fcNoCommand;
            ErrorFound := True;
          end;
        end else
          ErrorFound := True;

        if (CommandType <> fcNoCommand) and (not (CommandType in StripCommands)) then
          AddCommandChunk(CommandType, Data);

        NextChar;
      end;
    end;
    Result := not ErrorFound;
  end;

  procedure ParseString;
  begin
    Assert(CurChar <> '\');
    while (CurChar <> '\') and (CurPos <= Length(FormattedString)) do
    begin
      CurrentChunk := CurrentChunk +CurChar;
      NextChar;
    end;
  end;

begin
  Assert(Assigned(ChunkList));

  if FormattedString = '' then
    Exit;

  ErrorFound := False;
  CurrentChunk := '';
  CurPos := 1;
  CurChar := FormattedString[1];

  while CurPos <= Length(FormattedString) do
  begin
    if CurChar = '\' then
      ParseEscapeSequence
    else
      ParseString;
  end;

  if CurrentChunk <> '' then
    AddStringChunk;
end;

function StripFormatCommands(const FormattedString: string): string;
var
  Chunks: TFormatChunkList;
  i: Integer;
begin
  Chunks := TFormatChunkList.Create;
  try
    ParseFormatChunks(FormattedString, Chunks, AllCommands);

    Result := '';
    for i := 0 to Chunks.Count -1 do
      Result := Result + Chunks[i]^.Str;

  finally
    Chunks.Free;
  end;
end;

function PaintChunks(TargetCanvas: TCanvas; const Rect: TRect;
  ChunkList: TFormatChunkList; Images: TImageList;
  Invisible: Boolean): Integer;
var
  i: Integer;
  X: Integer;
  C: PFormatChunk;
  Count: Integer;
  LastStart: Integer;
  Style: TFontStyles;
  OldFont: TFont;
begin
  OldFont := TFont.Create;
  try
    OldFont.Assign(TargetCanvas.Font);

    LastStart := Rect.Left;
    X := Rect.Left;

    TargetCanvas.Brush.Style := bsClear;

    for i := 0 to ChunkList.Count -1 do
    begin
      C := ChunkList[i];

      case C^.Command of
      fcNoCommand:
        begin
          if not Invisible then
            TargetCanvas.TextOut(X, Rect.Top, C^.Str);

          Inc(X, TargetCanvas.TextWidth(C^.Str));
          if X > Rect.Right then
            Break;
        end;
      fcColor:
        if not Invisible then
          TargetCanvas.Font.Color := TColor(C^.Data);
      fcStyle:
        begin
          case PFormatStyleData(C^.Data)^.Style of
          'I': Style := [fsItalic];
          'B': Style := [fsBold];
          'U': Style := [fsUnderline];
          'S': Style := [fsStrikeout];
          else Assert(False);
          end;


          case PFormatStyleData(C^.Data)^.Action of
          -1: TargetCanvas.Font.Style := TargetCanvas.Font.Style - Style;
          0: if TargetCanvas.Font.Style * Style = [] then
               TargetCanvas.Font.Style := TargetCanvas.Font.Style + Style
             else
               TargetCanvas.Font.Style := TargetCanvas.Font.Style - Style;
          1: TargetCanvas.Font.Style := TargetCanvas.Font.Style + Style;
          else Assert(False);
          end;
        end;
      fcTab:
        begin
          Count := Integer(C^.Data);
          if Count = 0  then
            Count := DefTabChars;
          Inc(LastStart, TargetCanvas.TextWidth('W') * Count);
          if LastStart > X then
            X := LastStart;
          if X > Rect.Right then
            Break;
        end;
      fcColumn:
        begin
          Count := Integer(C^.Data);
          if Count = 0  then
            Count := DefTabChars;
          Inc(LastStart, TargetCanvas.TextWidth('W') * Count);
          if LastStart > X then
            X := LastStart;
          if X > Rect.Right then
            Break;
        end;
      fcHSpace:
        begin
          Inc(X, Integer(C^.Data));
          if X > Rect.Right then
            Break;
        end;
      fcImage:
        begin
          Assert(Assigned(Images));

          Images.Draw(TargetCanvas, X, Rect.Top, Integer(C^.Data));

          Inc(X, Images.Width);
          if X > Rect.Right then
            Break;
        end;
      end;
    end;

    Result := X;
    TargetCanvas.Font.Assign(OldFont);
  finally
    OldFont.Free;
    TargetCanvas.Brush.Style := bsSolid;
  end;
end;

procedure FormattedTextOut(TargetCanvas: TCanvas; const Rect: TRect; const Text: string; Selected: Boolean; Images: TImageList);
var
  Chunks: TFormatChunkList;
  StripCommands: TFormatCommands;
begin
  Chunks := TFormatChunkList.Create;
  try
    if Selected then
      StripCommands := [fcColor]
    else
      StripCommands := [];

    ParseFormatChunks(Text, Chunks, StripCommands);
    PaintChunks(TargetCanvas, Rect, Chunks, Images, False);
  finally
    Chunks.Free;
  end;
end;

function FormattedTextWidth(TargetCanvas: TCanvas; const Text: string; Images: TImageList): Integer;
var
  Chunks: TFormatChunkList;
  TmpRect: TRect;
begin
  Chunks := TFormatChunkList.Create;
  try
    TmpRect := Rect(0, 0, MaxInt, MaxInt);

    ParseFormatChunks(Text, Chunks, [fcColor]);
    Result := PaintChunks(TargetCanvas, TmpRect, Chunks, Images, True);
  finally
    Chunks.Free;
  end;
end;

function PrettyTextToFormattedString(const APrettyText: string; AlternateBoldStyle: Boolean = False): string;
var
  i: Integer;
  Color: TColor;
Begin
  Result := '';
  i := 1;
  while i <= Length(APrettyText) do
    case APrettyText[i] of
      #1, #2:
        begin
          Color := (Ord(APrettyText[i + 3]) shl 8
            +Ord(APrettyText[i + 2])) shl 8
            +Ord(APrettyText[i + 1]);

          Result := Result+'\color{'+ColorToString(Color)+'}';

          Inc(i, 4);
        end;
      #3:
        begin
          if CharInSet(UpperCase(APrettyText[i + 1])[1], ['B', 'I', 'U']) then
          begin
            Result := Result + '\style{';

            case APrettyText[i + 1] of
            'B': Result := Result + '+B';
            'b': Result := Result + '-B';
            'I': Result := Result + '+I';
            'i': Result := Result + '-I';
            'U': Result := Result + '+U';
            'u': Result := Result + '-U';
            end;

            Result := Result + '}';
          end;
          Inc(i, 2);
        end;
      #9:
        begin
          Result := Result + '\tab{}';
          if AlternateBoldStyle then
            Result := Result + '\style{~B}';
          Inc(i);
        end;
      else
        Result := Result + APrettyText[i];
        Inc(i);
    end;
end;

function IsIdentifierChar(p: PChar): boolean; inline;
{$IF FPC_FULLVERSION >= 20701}
var
  u: UnicodeString;
  i: Integer;
  L: SizeUInt;
{$ENDIF}
begin
  Result := p^ in ['a'..'z','A'..'Z','0'..'9','_'];
  if Result then exit;

  {$IF FPC_FULLVERSION >= 20701}
  if p^ <= #127 then exit;
  i := UTF8CodepointSize(p);
  SetLength(u, i);
  // wide chars of UTF-16 <= bytes of UTF-8 string
  if ConvertUTF8ToUTF16(PWideChar(u), i + 1, p, i, [toInvalidCharToSymbol], L) = trNoError
  then begin
    SetLength(u, L - 1);
    if L > 1 then
      Result := TCharacter.IsLetterOrDigit(u, 1);
  end;
  {$ENDIF}
end;

{ TSynBaseCompletionFormScrollBar }

procedure TSynBaseCompletionFormScrollBar.DoAutoAdjustLayout(
  const AMode: TLayoutAdjustmentPolicy; const AXProportion, AYProportion: Double
  );
begin
  if AMode in [lapAutoAdjustWithoutHorizontalScrolling, lapAutoAdjustForDPI] then
    Width := ScaleScreenToFont(GetSystemMetrics(SM_CYVSCROLL));
end;

{ TSynCompletionForm }

constructor TSynCompletionForm.Create(AOwner: Tcomponent);
begin
  inherited Create(AOwner);
  SmartEdit := True;
end;

{ TSynBaseCompletionFormSizeDrag }

procedure TSynBaseCompletionFormSizeDrag.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  FMouseDownPos.x := x + Left;
  FMouseDownPos.y := y + Top;
  FMouseLastPos.x := x + Left;
  FMouseLastPos.y := y + Top;
  FWinSize.x := TSynVirtualCompletionForm(Owner).Width;
  FWinSize.y := TSynVirtualCompletionForm(Owner).Height;
  TSynVirtualCompletionForm(Owner).IncHintLock;
  MouseCapture := True;
end;

procedure TSynBaseCompletionFormSizeDrag.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  F: TSynVirtualCompletionForm;
begin
  inherited MouseMove(Shift, X, Y);
  x := x + Left;
  y := y + Top;
  if (FMouseDownPos.y < 0) or
     ((FMouseLastPos.x = x) and (FMouseLastPos.y = y))
  then
    exit;
  FMouseLastPos.x := x;
  FMouseLastPos.y := y;

  F := TSynVirtualCompletionForm(Owner);
  F.Width :=
    Max(FWinSize.x + x - FMouseDownPos.x, 100);
  F.NbLinesInWindow :=
    Max((FWinSize.y + y - FMouseDownPos.y) div F.FontHeight, 3);
end;

procedure TSynBaseCompletionFormSizeDrag.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  FMouseDownPos.y := -1;
  MouseCapture := False;
  TSynVirtualCompletionForm(Owner).DecHintLock;

  if (FWinSize.x <> TSynVirtualCompletionForm(Owner).Width) or
     (FWinSize.y <> TSynVirtualCompletionForm(Owner).Height)
  then
    TSynVirtualCompletionForm(Owner).DoOnDragResize(Owner);
end;

constructor TSynBaseCompletionFormSizeDrag.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FMouseDownPos.y := -1;
end;

procedure TSynBaseCompletionFormSizeDrag.DoAutoAdjustLayout(
  const AMode: TLayoutAdjustmentPolicy; const AXProportion, AYProportion: Double
  );
begin
  // do nothing
end;

procedure TSynBaseCompletionFormSizeDrag.Paint;
var
  I: Integer;
  D: TThemedElementDetails;
begin
  Canvas.Brush.Color := clBtnFace;
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(ClientRect);
  Canvas.Pen.Color := clBtnShadow;

  D := ThemeServices.GetElementDetails(tsUpperTrackVertNormal);
  ThemeServices.DrawElement(Canvas.Handle, D, ClientRect);

  I := 2;
  while I < Height do
  begin
    Canvas.MoveTo(ClientRect.Right-I, ClientRect.Bottom-1-1);
    Canvas.LineTo(ClientRect.Right-1, ClientRect.Bottom-I-1);
    Inc(I, 3);
  end;
end;

{ TSynVirtualCompletionForm }

constructor TSynVirtualCompletionForm.Create(AOwner: Tcomponent);
begin
  ControlStyle := ControlStyle + [csNoDesignVisible];
  FResizeLock := 1; // prevent DoResize (on Handle Creation) do reset LinesInWindow
  FDoubleClickSelects := True;
  FHintLock := 0;
  BeginFormUpdate;
  KeyPreview:= True;
  // we have no resource => must be constructed using CreateNew
  inherited CreateNew(AOwner, 1);
  Scroll := TSynBaseCompletionFormScrollBar.Create(self);
  Scroll.Kind := sbVertical;
  Scroll.OnChange := @ScrollChange;
  Scroll.Parent := Self;
  Scroll.OnEnter := @ScrollGetFocus;
  Scroll.OnScroll := @ScrollScroll;
  Scroll.TabStop := False;
  Scroll.Visible := True;

  SizeDrag := TSynBaseCompletionFormSizeDrag.Create(Self);
  SizeDrag.Parent := Self;
  SizeDrag.BevelInner := bvNone;
  SizeDrag.BevelOuter := bvNone;
  SizeDrag.Caption := '';
  SizeDrag.AutoSize := False;
  SizeDrag.BorderStyle := bsNone;
  SizeDrag.Anchors := [akBottom, akRight, akLeft];
  SizeDrag.AnchorSideLeft.Side := asrTop;
  SizeDrag.AnchorSideLeft.Control := Scroll;
  SizeDrag.AnchorSideRight.Side := asrBottom;
  SizeDrag.AnchorSideRight.Control := Self;
  SizeDrag.AnchorSideBottom.Side := asrBottom;
  SizeDrag.AnchorSideBottom.Control := Self;
  SizeDrag.Cursor := crSizeNWSE;
  SizeDrag.Visible := False;

  SizeDrag.OnKeyPress:=@SDKeyPress;
  SizeDrag.OnKeyDown:=@SDKeyDown;
  SizeDrag.OnUTF8KeyPress:=@SDUtf8KeyPress;

  Scroll.Anchors:=[akTop,akRight, akBottom];
  Scroll.AnchorSide[akTop].Side := asrTop;
  Scroll.AnchorSide[akTop].Control := self;
  Scroll.AnchorSide[akRight].Side := asrBottom;
  Scroll.AnchorSide[akRight].Control := Self;
  Scroll.AnchorSide[akBottom].Side := asrTop;
  Scroll.AnchorSide[akBottom].Control := SizeDrag;

  DrawBorderWidth := 1;
  FTextColor:=clBlack;
  FTextSelectedColor:=clWhite;
  Caption:='Completion';
  Color:=clNone;
  FBackgroundColor:=clWhite;
  FDrawBorderColor:=clBlack;
  FHint := TSynBaseCompletionHint.Create(Self);
  FHint.FormStyle := fsSystemStayOnTop;
  {$IFDEF HintClickWorkaround}
  FHint.OnMouseDown :=@HintWindowMouseDown;
  {$ENDIF}
  FHintTimer := TTimer.Create(nil);
  FHintTimer.Enabled := False;
  FHintTimer.OnTimer := @OnHintTimer;
  FHintTimer.Interval := 0;
  FLongLineHintTime := 0;
  FLongLineHintType := sclpExtendRightOnly;
  Visible := false;
  ClSelect := clHighlight;
  FNbLinesInWindow := 6;
  FontChanged(Font);
  ShowHint := False;
  EndFormUpdate;
  FResizeLock := 0;

  BorderStyle := bsNone;
  FormStyle := fsSystemStayOnTop;
end;

procedure TSynVirtualCompletionForm.Deactivate;
begin
  {$IFDEF VerboseFocus}
  DebugLnEnter(['>> TSynVirtualCompletionForm.Deactivate ']);
  try
  {$ENDIF}
  // completion box lost focus
  // this can happen when a hint window is clicked => ToDo
  Visible := False;
  FHintTimer.Enabled := False;
  FHint.Visible := False;
  if Assigned(OnCancel) then OnCancel(Self);
  if (FCurrentEditor<>nil) and (TCustomSynEdit(fCurrentEditor).HandleAllocated)
  then
    SetCaretRespondToFocus(TCustomSynEdit(FCurrentEditor).Handle,true);
  {$IFDEF VerboseFocus}
  finally
    DebugLnExit(['<< TSynVirtualCompletionForm.Deactivate ']);
  end
  {$ENDIF}
end;

destructor TSynVirtualCompletionForm.Destroy;
begin
  UnRegisterHandlers;
  FreeAndNil(Scroll);
  FreeAndNil(SizeDrag);
  FHintTimer.Free;
  FHint.Free;
  inherited destroy;
end;

procedure TSynVirtualCompletionForm.ShowItemHint(AIndex: Integer);
var
  R: TRect;
  P: TPoint;
  M: TMonitor;
  MinLeft: Integer;
  AHint: string;
begin
  FHintTimer.Enabled := False;
  if Visible and (AIndex >= 0) and (AIndex < ItemsCount) and
     (FLongLineHintType <> sclpNone) and
     (FHintLock = 0)
  then begin
    FHint.Font.Assign(Font);
    // CalcHintRect uses the current index
    FHint.Index := AIndex;
    // calculate the size

    if UsePrettyText then
      AHint := StripFormatCommands(ItemList[AIndex])
    else
      AHint := ItemList[AIndex];

    AHint := ItemList[AIndex];

    R := FHint.CalcHintRect(Monitor.Width, AHint, nil);

    AHint := ItemList[AIndex];

    if (R.Right <= Scroll.Left) then begin
      FHint.Hide;
      Exit;
    end;

    // calculate the position
    M := Monitor;
    P := ClientToScreen(Point(0, (AIndex - Scroll.Position) * FFontHeight));
    case FLongLineHintType of
      // ClientWidth may be too much, if part of the ClientWidth extends to another screen.
      sclpExtendHalfLeft:      MinLeft := Max(M.Left,  P.X - ClientWidth div 2);
      sclpExtendUnlimitedLeft: MinLeft := M.Left;
      else                     MinLeft := P.X;
    end;
    P.X := Max(MinLeft,
               Min(P.X,          // Start at drop-down Left boundary
                   M.Left + M.Width - R.Right - 1
                  )              // Or push left, if hitting right Monitor border
              );
    P.Y := Max(M.Top, Min(P.Y, M.Top + M.Height - R.Bottom - 1));
    // actually Width and Height
    R.Right := Min(r.Right, M.Left + M.Width - 1 - P.X);
    R.Bottom := Min(r.Bottom, M.Top + M.Height - 1 - P.Y);

    FHint.HintRect := Bounds(P.X, P.Y, R.Right, R.Bottom);

    if (not FHint.IsVisible) and (FLongLineHintTime > 0) then
      FHintTimer.Enabled := True
    else
      OnHintTimer(nil);
  end
  else begin
    FHint.Hide;
  end;
end;

procedure TSynVirtualCompletionForm.OnHintTimer(Sender: TObject);
begin
  FHintTimer.Enabled := False;
  FHint.ActivateHint(ItemList[FHint.Index]);
  FHint.Invalidate;
end;

procedure TSynVirtualCompletionForm.KeyDown(var Key: Word; Shift: TShiftState);
var
  i: integer;
  Handled: Boolean;
begin
  {$IFDEF VerboseKeys}
  DebugLnEnter(['TSynVirtualCompletionForm.KeyDown ',Key,' Shift=',ssShift in Shift,' Ctrl=',ssCtrl in Shift,' Alt=',ssAlt in Shift]);
  try
  {$ENDIF}
  //debugln('TSynVirtualCompletionForm.KeyDown A Key=',dbgs(Key));
  inherited KeyDown(Key,Shift);
  if Key=VK_UNKNOWN then exit;
  Handled:=true;
  case Key of
// added the VK_XXX codes to make it more readable / maintainable
    VK_RETURN:
      if Assigned(OnValidate) then
        OnValidate(Self, '', Shift);
    VK_ESCAPE:
      if Assigned(OnCancel) then OnCancel(Self);
    // I do not think there is a worst way to do this, but laziness rules :-)
    VK_PRIOR:
      for i := 1 to NbLinesInWindow do
        SelectPrec;
    VK_NEXT:
      for i := 1 to NbLinesInWindow do
        SelectNext;
    VK_END:
      Position := ItemsCount - 1;
    VK_HOME:
      Position := 0;
    VK_UP:
      if ssCtrl in Shift then
        Position := 0
      else
        SelectPrec;
    VK_DOWN:
      if ssCtrl in Shift then
        Position := ItemsCount - 1
      else
        SelectNext;
    VK_BACK:
      if (Shift = []) and (Length(CurrentString) > 0) then begin
        if Assigned(OnKeyDelete) then OnKeyDelete(Self);
        DeleteCharBeforeCursor;
      end;
    VK_DELETE:
      begin
        if Assigned(OnKeyDelete) then OnKeyDelete(Self);
        DeleteCharAfterCursor;
      end;
    VK_TAB:
      begin
        if Assigned(OnKeyCompletePrefix) then OnKeyCompletePrefix(Self);
      end;
    VK_LEFT:
      begin
        if (Shift = []) and (Length(CurrentString) > 0) then begin
          if Assigned(OnKeyPrevChar) then OnKeyPrevChar(Self);
        end;
      end;
    VK_Right:
      begin
        if Assigned(OnKeyNextChar) then OnKeyNextChar(Self);
      end;
  else
    Handled:=false;
  end;
  if Handled then Key:=VK_UNKNOWN;
  Invalidate;
  {$IFDEF VerboseKeys}
  finally
    DebugLnExit(['TSynVirtualCompletionForm.KeyDown ',Key,' Shift=',ssShift in Shift,' Ctrl=',ssCtrl in Shift,' Alt=',ssAlt in Shift]);
  end;
  {$ENDIF}
end;

procedure TSynVirtualCompletionForm.KeyPress(var Key: char);
begin
  debugln('TSynVirtualCompletionForm.KeyPress A Key="',DbgStr(Key),'"');
  if Assigned(OnKeyPress) then
    OnKeyPress(Self, Key);
  debugln('TSynVirtualCompletionForm.KeyPress B Key="',DbgStr(Key),'"');
  if Key=#0 then exit;
  case key of //
    #33..'z':
      begin
        if Key<>#0 then
          AddCharAtCursor(key);
        Key:=#0;
      end;
    #8: ;
  else
    if (ord(key)>=32) and Assigned(OnValidate) then begin
      OnValidate(Self, Key, []);
      Key:=#0;
    end else begin
      if Assigned(OnCancel) then OnCancel(Self);
      Key:=#0;
    end;
  end; // case
  Invalidate;
  //debugln('TSynVirtualCompletionForm.KeyPress END Key="',DbgStr(Key),'"');
end;

procedure TSynVirtualCompletionForm.AddCharAtCursor(AUtf8Char: TUTF8Char);
begin
  CurrentString := CurrentString + AUtf8Char;
  if SmartEdit then
    if CurrentEditor <> nil then
      (CurrentEditor as TCustomSynEdit).CommandProcessor(ecChar, AUtf8Char, nil);
end;

procedure TSynVirtualCompletionForm.DeleteCharBeforeCursor;
begin
  if SmartEdit then
    if CurrentEditor <> nil then
      (CurrentEditor as TCustomSynEdit).CommandProcessor(ecDeleteLastChar, #0, nil);
  CurrentString := UTF8Copy(CurrentString, 1, UTF8Length(CurrentString) - 1);
end;

{$IFDEF HintClickWorkaround}
procedure TSynVirtualCompletionForm.HintWindowMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  p: TPoint;
begin
  p := ScreenToClient(FHint.ClientToScreen(Point(X, Y)));
  MouseDown(Button, Shift, p.X, p.Y);
end;
{$ENDIF}

procedure TSynVirtualCompletionForm.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  OldPosition: Integer;
begin
  OldPosition := Position;
  y := (y - 1) div FFontHeight;
  Position := Scroll.Position + y;
  if DoubleClickSelects and (ssDouble in Shift) and (Position = OldPosition) and
    Assigned(OnValidate)
  then
    OnValidate(Self, '', Shift);
end;

procedure TSynVirtualCompletionForm.MouseMove(Shift: TShiftState; X,Y: Integer);
begin
  if ((Scroll.Visible) and (x > Scroll.Left)) or
     (y  < DrawBorderWidth) or (y >= ClientHeight - DrawBorderWidth)
  then
    exit;
  Y := (Y - DrawBorderWidth) div FFontHeight;
  ShowItemHint(Scroll.Position + Y);
end;

procedure TSynVirtualCompletionForm.Paint;
var
  i, Ind: integer;
  PaintWidth, YYY, LeftC, RightC, BottomC: Integer;
  Capt: String;
  lSelected: Boolean;
begin
//Writeln('[TSynVirtualCompletionForm.Paint]');

  // update scroll bar
  Scroll.Enabled := ItemsCount > NbLinesInWindow;
  Scroll.Visible := (ItemsCount > NbLinesInWindow) or ShowSizeDrag;

  if Scroll.Visible and Scroll.Enabled then
  begin
    Scroll.Max := ItemsCount - 1;
    Scroll.LargeChange := NbLinesInWindow;
    Scroll.PageSize := NbLinesInWindow;
  end
  else
  begin
    Scroll.PageSize := 1;
    Scroll.Max := 0;
  end;

  PaintWidth := Width - Scroll.Width - 2*DrawBorderWidth;
  LeftC := DrawBorderWidth;
  RightC := LeftC + PaintWidth;

  //DebugLn(['TSynVirtualCompletionForm.Paint NbLinesInWindow=',NbLinesInWindow,' ItemsCount=',ItemsCount]);
  for i := 0 to Min(NbLinesInWindow - 1, ItemsCount - Scroll.Position - 1) do
  begin
    YYY := LeftC + FFontHeight * i;
    BottomC := (FFontHeight * (i + 1))+1;
    lSelected := i + Scroll.Position = Position;
    if lSelected then
    begin
      Canvas.Brush.Color := clSelect;
      Canvas.Pen.Color := clSelect;
      Canvas.Rectangle(LeftC, YYY, RightC, BottomC);
      Canvas.Pen.Color := clBlack;
      Canvas.Font.Color := TextSelectedColor;
      if UsePrettyText then
        Hint := StripFormatCommands(ItemList[Position])
      else
        Hint := ItemList[Position];
    end
    else
    begin
      Canvas.Brush.Color := BackgroundColor;
      Canvas.Font.Color := TextColor;
      Canvas.FillRect(Rect(LeftC, YYY, RightC, BottomC));
    end;
    //DebugLn(['TSynVirtualCompletionForm.Paint ',i,' ',ItemList[Scroll.Position + i]]);
    Ind := i + Scroll.Position;
    Capt := ItemList[Scroll.Position + i];
    if not Assigned(OnPaintItem)
    or not OnPaintItem(Capt, Canvas, LeftC, YYY, Ind = Position, Ind)
    then
      if UsePrettyText then
        FormattedTextOut(Canvas, Rect(LeftC+2, YYY, RightC, BottomC), Capt, lSelected, nil)
      else
        Canvas.TextOut(LeftC+2, YYY, Capt);
  end;
  // paint the rest of the background
  if NbLinesInWindow > ItemsCount - Scroll.Position then
  begin
    Canvas.brush.color := color;
    i:=(FFontHeight * ItemsCount)+1;
    Canvas.FillRect(Rect(LeftC, i, RightC, Height));
  end;
  // draw a rectangle around the window
  if DrawBorderWidth > 0 then
  begin
    Canvas.Brush.Color := DrawBorderColor;
    Canvas.FillRect(0, 0, Width, DrawBorderWidth);
    Canvas.FillRect(Width-DrawBorderWidth, 0, Width, Height);
    Canvas.FillRect(0, Height-DrawBorderWidth, Width, Height);
    Canvas.FillRect(0, 0, DrawBorderWidth, Height);
  end;
end;

function TSynVirtualCompletionForm.Focused: Boolean;
begin
  Result:=(inherited Focused) or SizeDrag.Focused;
end;

procedure TSynVirtualCompletionForm.AppDeactivated(Sender: TObject);
begin
  {$IFDEF VerboseFocus}
  DebugLn(['>> TSynVirtualCompletionForm.AppDeactivated ']);
  {$ENDIF}
  Deactivate;
end;

procedure TSynVirtualCompletionForm.ScrollChange(Sender: TObject);
begin
  if Position < Scroll.Position then
    Position := Scroll.Position
  else
  if Position > Scroll.Position + NbLinesInWindow - 1 then
    Position := Scroll.Position + NbLinesInWindow - 1;
  Invalidate;
end;

procedure TSynVirtualCompletionForm.ScrollGetFocus(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TSynVirtualCompletionForm.ScrollScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  if ScrollPos > (Scroll.Max - Scroll.PageSize) + 1 then
    ScrollPos := Scroll.Max - Scroll.PageSize + 1;
  FHint.Hide;
  ShowItemHint(Position);
end;

procedure TSynVirtualCompletionForm.SelectNext;
begin
  if Position < ItemsCount - 1 then
    Position := Position + 1;
end;

procedure TSynVirtualCompletionForm.SelectPrec;
begin
  if Position > 0 then
    Position := Position - 1;
end;

procedure TSynVirtualCompletionForm.DoEditorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (not Visible) or (FCurrentEditor = nil) or (Sender <> FCurrentEditor) then exit;
  KeyDown(Key, Shift);
  Key := 0;
end;

procedure TSynVirtualCompletionForm.DoEditorKeyPress(Sender: TObject; var Key: char);
begin
  if (not Visible) or (FCurrentEditor = nil) or (Sender <> FCurrentEditor) then exit;
  KeyPress(Key);
  Key := #0;
end;

procedure TSynVirtualCompletionForm.DoEditorUtf8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  if (not Visible) or (FCurrentEditor = nil) or (Sender <> FCurrentEditor) then exit;
  UTF8KeyPress(UTF8Key);
  UTF8Key := '';
end;

procedure TSynVirtualCompletionForm.SDKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  KeyDown(key,shift);
end;

procedure TSynVirtualCompletionForm.SDKeyPress(Sender: TObject; var Key: char);
begin
  KeyPress(key);
end;

procedure TSynVirtualCompletionForm.SDUtf8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
begin
  UTF8KeyPress(UTF8Key);
end;

procedure TSynVirtualCompletionForm.UTF8KeyPress(var UTF8Key: TUTF8Char);
begin
  {$IFDEF VerboseKeys}
  debugln('TSynVirtualCompletionForm.UTF8KeyPress A UTF8Key="',DbgStr(UTF8Key),'" ',dbgsName(TObject(TMethod(OnUTF8KeyPress).Data)));
  {$ENDIF}
  if Assigned(OnUTF8KeyPress) then
    OnUTF8KeyPress(Self, UTF8Key);
  if UTF8Key='' then
    exit;

  if UTF8Key=#8 then
  begin
    // backspace
  end
  else
  if (Length(UTF8Key)>=1) and (not IsIdentifierChar(@UTF8Key[1])) then
  begin
    // non identifier character
    // if it is special key then eat it
    if (Length(UTF8Key) = 1) and (UTF8Key[1] < #32) then
    begin
      if Assigned(OnCancel) then
        OnCancel(Self);
    end
    else
    if Assigned(OnValidate) then
      OnValidate(Self, UTF8Key, []);
    UTF8Key := '';
  end
  else
  if (UTF8Key<>'') then
  begin
    // identifier character
    AddCharAtCursor(UTF8Key);
    UTF8Key := '';
  end;
  {$IFDEF VerboseKeys}
  debugln('TSynVirtualCompletionForm.UTF8KeyPress END UTF8Key="',DbgStr(UTF8Key),'"');
  {$ENDIF}
end;

procedure TSynVirtualCompletionForm.SetCurrentString(const Value: string);
var
  i: integer;
begin
  FCurrentString := Value;
  //debugln('TSynVirtualCompletionForm.SetCurrentString FCurrentString=',FCurrentString);
  if Assigned(FOnSearchPosition) then begin
    i:=Position;
    FOnSearchPosition(i);
    Position:=i;
  end else begin
    if FCaseSensitive then begin
      for i := 0 to Pred(ItemsCount) do
        if 0 = CompareStr(fCurrentString,
          Copy(InsertList[i], 1, Length(fCurrentString)))
        then begin
          Position := i;
          break;
        end;
    end else begin
      for i := 0 to Pred(ItemsCount) do
        if 0 = UTF8CompareLatinTextFast(fCurrentString,
                                    Copy(InsertList[i], 1, Length(fCurrentString)))
        then begin
          Position := i;
          break;
        end;
    end;
  end;
end;

procedure TSynVirtualCompletionForm.DoOnResize;
begin
  inherited DoOnResize;
  if ([csLoading,csDestroying]*ComponentState<>[]) or (Scroll=nil) then exit;
  if (fFontHeight > 0) and (FResizeLock = 0) then
  begin
    FNbLinesInWindow := (Height-2*DrawBorderWidth+(fFontHeight-1)) div fFontHeight;
    Invalidate;
  end;
end;

procedure TSynVirtualCompletionForm.SetBackgroundColor(const AValue: TColor);
begin
  if FBackgroundColor <> AValue then
  begin
    FBackgroundColor := AValue;
    Color := AValue;
    FHint.Color := AValue;
  end;
end;

procedure TSynVirtualCompletionForm.FontChanged(Sender: TObject);
var
  TextMetric: TTextMetric;
begin
  inc(FResizeLock);   // prevent DoResize from recalculating NbLinesInWindow
  try
    inherited;
    FillChar(TextMetric{%H-},SizeOf(TextMetric),0);
    GetTextMetrics(Canvas.Handle, TextMetric);
    FFontHeight := TextMetric.tmHeight+2;
    SetNblinesInWindow(FNbLinesInWindow);
    if SizeDrag<>nil then
      SizeDrag.Height := Max(7, FFontHeight * 2 div 3);
  finally
    dec(FResizeLock);
  end;
end;

procedure TSynVirtualCompletionForm.WMMouseWheel(var Msg: TLMMouseEvent);
const
  WHEEL_DELTA = 120;
var
  WheelClicks: Integer;
begin
  Inc(FMouseWheelAccumulator, Msg.WheelDelta);
  WheelClicks := FMouseWheelAccumulator div WHEEL_DELTA;
  FMouseWheelAccumulator := FMouseWheelAccumulator - WheelClicks * WHEEL_DELTA;
  WheelClicks := WheelClicks * Mouse.WheelScrollLines;
  Scroll.Position := Max(0, Min(ItemsCount - NbLinesInWindow, Scroll.Position - WheelClicks));
end;

procedure TSynVirtualCompletionForm.SetLongLineHintTime(const AValue: Integer);
begin
  if FLongLineHintTime = AValue then exit;
  FLongLineHintTime := AValue;
  FHintTimer.Interval := AValue;
end;

procedure TSynVirtualCompletionForm.EditorStatusChanged(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  if (scTopLine in Changes) and Assigned(OnCancel) then
    OnCancel(Self);
end;

procedure TSynVirtualCompletionForm.SetShowSizeDrag(const AValue: Boolean);
begin
  if FShowSizeDrag = AValue then exit;
  FShowSizeDrag := AValue;
  SizeDrag.Visible := AValue;
  if SizeDrag.Visible then
    Scroll.BorderSpacing.Bottom := 0
  else
    Scroll.BorderSpacing.Bottom := FDrawBorderWidth;
end;

procedure TSynVirtualCompletionForm.RegisterHandlers(EditOnly: Boolean);
begin
  if FCurrentEditor <> nil then begin
    FCurrentEditor.RegisterStatusChangedHandler
    (@EditorStatusChanged, [scTopLine]);
    // Catch Editor events. Some Widgetset may report keys to the editor,
    // if the user types faster, then the app can open the form
    FCurrentEditor.RegisterBeforeKeyDownHandler(@DoEditorKeyDown);
    FCurrentEditor.RegisterBeforeKeyPressHandler(@DoEditorKeyPress);
    FCurrentEditor.RegisterBeforeUtf8KeyPressHandler(@DoEditorUtf8KeyPress);
  end;
  if not EditOnly then
    Application.AddOnDeactivateHandler(@AppDeactivated);
end;

procedure TSynVirtualCompletionForm.UnRegisterHandlers(EditOnly: Boolean);
begin
  if FCurrentEditor <> nil then begin
    FCurrentEditor.UnRegisterStatusChangedHandler(@EditorStatusChanged);
    FCurrentEditor.UnregisterBeforeKeyDownHandler(@DoEditorKeyDown);
    FCurrentEditor.UnregisterBeforeKeyPressHandler(@DoEditorKeyPress);
    FCurrentEditor.UnregisterBeforeUtf8KeyPressHandler(@DoEditorUtf8KeyPress);
  end;
  if not EditOnly then
    Application.RemoveOnDeactivateHandler(@AppDeactivated);
end;

procedure TSynVirtualCompletionForm.SetCurrentEditor(const AValue: TCustomSynEdit);
begin
  if FCurrentEditor = AValue then exit;
  UnRegisterHandlers(True);
  FCurrentEditor := AValue;
  if Visible then
    RegisterHandlers(True);
end;

procedure TSynVirtualCompletionForm.SetDrawBorderWidth(const AValue: Integer);
begin
  if FDrawBorderWidth = AValue then exit;
  FDrawBorderWidth := AValue;
  NbLinesInWindow := NbLinesInWindow;
  Scroll.BorderSpacing.Top := FDrawBorderWidth;
  Scroll.BorderSpacing.Right := FDrawBorderWidth;
  if SizeDrag.Visible then
    Scroll.BorderSpacing.Bottom := 0
  else
    Scroll.BorderSpacing.Bottom := FDrawBorderWidth;
  SizeDrag.BorderSpacing.Right := FDrawBorderWidth;
  SizeDrag.BorderSpacing.Bottom := FDrawBorderWidth;
end;

procedure TSynVirtualCompletionForm.SetVisible(Value: Boolean);
begin
  if Visible = Value then exit;;

  if Value then
    RegisterHandlers
  else
    UnRegisterHandlers;

  inherited SetVisible(Value);
end;

procedure TSynVirtualCompletionForm.IncHintLock;
begin
  inc(FHintLock);
  FHint.Hide
end;

procedure TSynVirtualCompletionForm.DecHintLock;
begin
  dec(FHintLock);
  if FHintLock = 0 then
    ShowItemHint(Position);
end;

procedure TSynVirtualCompletionForm.DeleteCharAfterCursor;
begin
  if SmartEdit then
    if CurrentEditor <> nil then
      (CurrentEditor as TCustomSynEdit).CommandProcessor(ecDeleteChar, #0, nil);
end;

procedure TSynVirtualCompletionForm.DoOnDragResize(Sender: TObject);
begin
  if assigned(FOnDragResized) then
    FOnDragResized(Sender);
end;

procedure TSynVirtualCompletionForm.ClearCurrentString;
begin
  FCurrentString := '';
  FPosition := 0;
end;

procedure TSynVirtualCompletionForm.SetNbLinesInWindow(
  const Value: Integer);
begin
  inc(FResizeLock);   // prevent DoResize from recalculating NbLinesInWindow
  try
    FNbLinesInWindow := Value;
    Height := fFontHeight * NbLinesInWindow + 2*DrawBorderWidth;
  finally
    dec(FResizeLock);
  end;
end;

procedure TSynVirtualCompletionForm.SetPosition(Value: Integer);
begin
  Value := MinMax(Value, 0, ItemsCount - 1);
  if FPosition <> Value then begin
    FPosition := Value;
    if Position < Scroll.Position then
      Scroll.Position := Position
    else if Scroll.Position < Position - NbLinesInWindow + 1 then
      Scroll.Position := Position - NbLinesInWindow + 1;
    Invalidate;
    if Assigned(OnPositionChanged) then OnPositionChanged(Self);
  end;
  if Showing then
    ShowItemHint(Position);
end;

{ TSynVirtualCompletion }

constructor TSynVirtualCompletion.Create(AOwner: TComponent);
begin
  FWidth := 262;
  inherited Create(AOwner);
  Form := GetCompletionFormClass.Create(nil); // Do not create with owner, or the designer will make it visible
  Form.Width := FWidth;
  FAutoUseSingleIdent := True;
end;

destructor TSynVirtualCompletion.Destroy;
begin
  inherited Destroy;
  FreeAndNil(Form);
end;

function TSynVirtualCompletion.GetOnUTF8KeyPress: TUTF8KeyPressEvent;
begin
  Result:=Form.OnUTF8KeyPress;
end;

procedure TSynVirtualCompletion.SetOnUTF8KeyPress(
  const AValue: TUTF8KeyPressEvent);
begin
  Form.OnUTF8KeyPress:=AValue;
end;

function TSynVirtualCompletion.GetFontHeight:integer;
begin
  Result:=Form.FontHeight;
end;

function TSynVirtualCompletion.GetOnSearchPosition:TSynBaseCompletionSearchPosition;
begin
  Result:=Form.OnSearchPosition;
end;

procedure TSynVirtualCompletion.SetOnSearchPosition(
  NewValue :TSynBaseCompletionSearchPosition);
begin
  Form.OnSearchPosition:=NewValue;
end;

function TSynVirtualCompletion.GetOnKeyCompletePrefix: TNotifyEvent;
begin
  Result:=Form.OnKeyCompletePrefix;
end;

procedure TSynVirtualCompletion.SetOnKeyCompletePrefix(const AValue: TNotifyEvent);
begin
  Form.OnKeyCompletePrefix:=AValue;
end;

function TSynVirtualCompletion.GetOnKeyNextChar: TNotifyEvent;
begin
  Result:=Form.OnKeyNextChar;
end;

procedure TSynVirtualCompletion.SetOnKeyNextChar(const AValue: TNotifyEvent);
begin
  Form.OnKeyNextChar:=AValue;
end;

function TSynVirtualCompletion.GetOnKeyPrevChar: TNotifyEvent;
begin
  Result:=Form.OnKeyPrevChar;
end;

procedure TSynVirtualCompletion.SetOnKeyPrevChar(const AValue: TNotifyEvent);
begin
  Form.OnKeyPrevChar:=AValue;
end;

procedure TSynVirtualCompletion.DoBeforeExecute(var ACurrentString: String; var APosition: Integer; var AnX, AnY: Integer; var AnResult: TOnBeforeExeucteFlags);
begin
end;

procedure TSynVirtualCompletion.Execute(s: string; x, y: integer);
var
  CurSynEdit: TCustomSynEdit;
  p: Integer;
  r: TOnBeforeExeucteFlags;
begin
  //writeln('TSynVirtualCompletion.Execute ',Form.CurrentEditor.Name);

  //Todo: This is dangerous, if other plugins also change/changed the flag.
  FAddedPersistentCaret := False;
  FChangedNoneBlink := False;

  Form.ClearCurrentString;
  p := -1;
  r := [];

  DoBeforeExecute(s, p, x, y, r);
  if befAbort in r then
    exit;

  CurrentString := s;
  if p >= 0 then
    Position := p;

  if Assigned(OnExecute) then
    OnExecute(Self);
  if (Form.ItemsCount=1) and Assigned(OnValidate) and FAutoUseSingleIdent then begin
    OnValidate(Form, '', []);
    exit;
  end;
  if (Form.ItemsCount=0) and Assigned(OnCancel) then begin
    OnCancel(Form);
    exit;
  end;

  if (Form.CurrentEditor is TCustomSynEdit) then begin
    CurSynEdit:=TCustomSynEdit(Form.CurrentEditor);
    FAddedPersistentCaret := not(eoPersistentCaret in CurSynEdit.Options);
    FChangedNoneBlink := (eoPersistentCaretStopBlink in CurSynEdit.Options2);
    if FAddedPersistentCaret then
      CurSynEdit.Options:=CurSynEdit.Options+[eoPersistentCaret];
    if FChangedNoneBlink then
      CurSynEdit.Options2:=CurSynEdit.Options2-[eoPersistentCaretStopBlink];
  end;
  Form.SetBounds(x,y,Form.Width,Form.Height);
  Form.Show;
  Form.Position := Form.Position;
end;

procedure TSynVirtualCompletion.Execute(s: string; TopLeft: TPoint);
begin
  Execute(s, TopLeft.x, TopLeft.y);
end;

procedure TSynVirtualCompletion.Execute(s: string; TokenRect: TRect);
var
  SpaceBelow, SpaceAbove: Integer;
  Mon: TMonitor;
  MRect: TRect;
begin
  Mon := Screen.MonitorFromPoint(TokenRect.TopLeft);
  if Mon = nil then begin
    Execute(s, TokenRect.Left, TokenRect.Bottom);
    exit;
  end;

  MRect := Mon.WorkareaRect; // BoundsRect on Windows, if overlap with Taskbar is desired
  TokenRect.Left := Max(MRect.Left, Min(TokenRect.Left, MRect.Right - Form.Width));

  SpaceBelow := MRect.Bottom - TokenRect.Bottom;
  SpaceAbove := TokenRect.Top - MRect.Top;
  if Form.Height < SpaceBelow then
    Execute(s, TokenRect.Left, TokenRect.Bottom)
  else
  if Form.Height < SpaceAbove then
    Execute(s, TokenRect.Left, TokenRect.Top - Form.Height)
  else
  begin
    if SpaceBelow > SpaceAbove then begin
      Form.NbLinesInWindow := Max(SpaceBelow div Form.FontHeight, 3); // temporary height
    Execute(s, TokenRect.Left, TokenRect.Bottom);
    end else begin
      Form.NbLinesInWindow := Max(SpaceAbove div Form.FontHeight, 3); // temporary height
      Execute(s, TokenRect.Left, TokenRect.Top - Form.Height);
    end;
  end;
end;

function TSynVirtualCompletion.GetCurrentString: string;
begin
  result := Form.CurrentString;
end;

function TSynVirtualCompletion.GetNbLinesInWindow: Integer;
begin
  Result := Form.NbLinesInWindow;
end;

function TSynVirtualCompletion.GetOnCancel: TNotifyEvent;
begin
  Result := Form.OnCancel;
end;

function TSynVirtualCompletion.GetOnKeyPress: TKeyPressEvent;
begin
  Result := Form.OnKeyPress;
end;

function TSynVirtualCompletion.GetOnPaintItem: TSynBaseCompletionPaintItem;
begin
  Result := Form.OnPaintItem;
end;

function TSynVirtualCompletion.GetOnValidate: TValidateEvent;
begin
  Result := Form.OnValidate;
end;

function TSynVirtualCompletion.GetPosition: Integer;
begin
  Result := Form.Position;
end;

procedure TSynVirtualCompletion.SetCurrentString(const Value: string);
begin
  form.CurrentString := Value;
end;

procedure TSynVirtualCompletion.SetDoubleClickSelects(const AValue: Boolean);
begin
  Form.DoubleClickSelects := AValue;
end;

procedure TSynVirtualCompletion.SetLongLineHintTime(const AValue: Integer);
begin
  Form.LongLineHintTime := AValue;
end;

procedure TSynVirtualCompletion.SetLongLineHintType(const AValue: TSynCompletionLongHintType);
begin
  Form.LongLineHintType := AValue;
end;

procedure TSynVirtualCompletion.SetNbLinesInWindow(const Value: Integer);
begin
  form.NbLinesInWindow := Value;
end;

procedure TSynVirtualCompletion.SetOnCancel(const Value: TNotifyEvent);
begin
  form.OnCancel := Value;
end;

procedure TSynVirtualCompletion.SetOnKeyDown(const AValue: TKeyEvent);
begin
  Form.OnKeyDown:=AValue;
end;

procedure TSynVirtualCompletion.SetOnKeyPress(const Value: TKeyPressEvent);
begin
  Form.OnKeyPress := Value;
end;

procedure TSynVirtualCompletion.SetOnMeasureItem(
  const AValue: TSynBaseCompletionMeasureItem);
begin
  Form.OnMeasureItem := AValue;
end;

procedure TSynVirtualCompletion.SetOnPositionChanged(const AValue: TNotifyEvent);
begin
  Form.OnPositionChanged :=  AValue;
end;

procedure TSynVirtualCompletion.SetOnPaintItem(const Value: TSynBaseCompletionPaintItem);
begin
  form.OnPaintItem := Value;
end;

procedure TSynVirtualCompletion.SetPosition(const Value: Integer);
begin
  form.Position := Value;
end;

procedure TSynVirtualCompletion.SetOnValidate(const Value: TValidateEvent);
begin
  form.OnValidate := Value;
end;

function TSynVirtualCompletion.GetClSelect: TColor;
begin
  Result := Form.ClSelect;
end;

function TSynVirtualCompletion.GetDoubleClickSelects: Boolean;
begin
  Result := Form.DoubleClickSelects;
end;

function TSynVirtualCompletion.GetLongLineHintTime: Integer;
begin
  Result := Form.LongLineHintTime;
end;

function TSynVirtualCompletion.GetLongLineHintType: TSynCompletionLongHintType;
begin
  Result := Form.LongLineHintType;
end;

function TSynVirtualCompletion.GetOnKeyDown: TKeyEvent;
begin
  Result:=Form.OnKeyDown;
end;

function TSynVirtualCompletion.GetCaseSensitive: boolean;
begin
  Result := Form.CaseSensitive;
end;

function TSynVirtualCompletion.GetUsePrettyText: boolean;
begin
  Result := Form.UsePrettyText;
end;

function TSynVirtualCompletion.GetOnMeasureItem: TSynBaseCompletionMeasureItem;
begin
  Result := Form.OnMeasureItem;
end;

function TSynVirtualCompletion.GetOnPositionChanged: TNotifyEvent;
begin
  Result := Form.OnPositionChanged;
end;

function TSynVirtualCompletion.GetShowSizeDrag: Boolean;
begin
  Result := Form.ShowSizeDrag;
end;

procedure TSynVirtualCompletion.SetCaseSensitive(const AValue: boolean);
begin
  Form.CaseSensitive := AValue;
end;

procedure TSynVirtualCompletion.SetUsePrettyText(const AValue: boolean);
begin
  Form.UsePrettyText := AValue;
end;

procedure TSynVirtualCompletion.SetClSelect(const Value: TColor);
begin
  Form.ClSelect := Value;
end;

function TSynVirtualCompletion.GetOnKeyDelete: TNotifyEvent;
begin
  result := Form.OnKeyDelete;
end;

procedure TSynVirtualCompletion.SetOnKeyDelete(const Value: TNotifyEvent);
begin
  form.OnKeyDelete := Value;
end;

procedure TSynVirtualCompletion.SetShowSizeDrag(const AValue: Boolean);
begin
  Form.ShowSizeDrag := AValue;
end;

procedure TSynVirtualCompletion.SetWidth(Value: Integer);
begin
  FWidth := Value;
  Form.Width := FWidth;
  Form.SetNbLinesInWindow(Form.FNbLinesInWindow);
end;

procedure TSynVirtualCompletion.Deactivate;
var
  CurSynEdit: TCustomSynEdit;
begin
  if (Form<>nil) and (Form.CurrentEditor is TCustomSynEdit)
  then begin
    CurSynEdit:=TCustomSynEdit(Form.CurrentEditor);
    if FAddedPersistentCaret then
      CurSynEdit.Options:=CurSynEdit.Options-[eoPersistentCaret];
    if FChangedNoneBlink then
      CurSynEdit.Options2:=CurSynEdit.Options2+[eoPersistentCaretStopBlink];
  end;
  if Assigned(Form) then Form.Deactivate;
end;

function TSynVirtualCompletion.IsActive: boolean;
begin
  Result:=(Form<>nil) and (Form.Visible);
end;

function TSynVirtualCompletion.TheForm: TSynVirtualCompletionForm;
begin
  Result:=Form;
end;

procedure PrettyTextOut(c: TCanvas; x, y: integer; s: string);
var
  i: integer;
  OldFontColor: TColor;
  OldFontStyle: TFontStyles;
begin
  OldFontColor:=c.Font.Color;
  OldFontStyle:=c.Font.Style;
  c.Font.Style:=[];
  c.Font.Color:=clBlack;
  try
    i := 1;
    while i <= Length(s) do
      case s[i] of
        #1: begin
            C.Font.Color := (Ord(s[i + 3]) shl 8 + Ord(s[i + 2])) shl 8 + Ord(s[i + 1]);
            inc(i, 4);
          end;
        #2: begin
            C.Font.Color := (Ord(s[i + 3]) shl 8 + Ord(s[i + 2])) shl 8 + Ord(s[i + 1]);
            inc(i, 4);
          end;
        #3: begin
            case s[i + 1] of
              'B': c.Font.Style := c.Font.Style + [fsBold];
              'b': c.Font.Style := c.Font.Style - [fsBold];
              'U': c.Font.Style := c.Font.Style + [fsUnderline];
              'u': c.Font.Style := c.Font.Style - [fsUnderline];
              'I': c.Font.Style := c.Font.Style + [fsItalic];
              'i': c.Font.Style := c.Font.Style - [fsItalic];
            end;
            inc(i, 2);
          end;
      else
        C.TextOut(x, y, s[i]);
        x := x + c.TextWidth(s[i]);
        inc(i);
      end;
  except
  end;
  c.Font.Color:=OldFontColor;
  c.Font.Style:=OldFontStyle;
end;

{ TSynBaseCompletion }

procedure TSynBaseCompletion.OnFormPaint(Sender: TObject);
begin

end;

procedure TSynBaseCompletion.Cancel(Sender: TObject);
var
  F: TSynVirtualCompletionForm;
begin
  F := Sender as TSynVirtualCompletionForm;
  if F.CurrentEditor <> nil then begin
    if (F.CurrentEditor as TCustomSynEdit).Owner is TWinControl then
      TWinControl((F.CurrentEditor as TCustomSynEdit).Owner).SetFocus;
    (F.CurrentEditor as TCustomSynEdit).SetFocus;
  end;
end;

procedure TSynBaseCompletion.Validate(Sender: TObject; KeyChar: TUTF8Char;
  Shift: TShiftState);
var
  F: TSynVirtualCompletionForm;
  Value, CurLine: string;
  NewBlockBegin, NewBlockEnd: TPoint;
  LogCaret: TPoint;
  HighlighterIdentChars: TSynIdentChars;
begin
  //debugln('TSynBaseCompletion.Validate ',dbgsName(Sender),' ',dbgs(Shift),' Position=',dbgs(Position));
  F := Sender as TSynVirtualCompletionForm;
  // Note: Form.Visible can be false, for example when completion only contains one item
  if F.CurrentEditor is TCustomSynEdit then
    with TCustomSynEdit(F.CurrentEditor) do begin
      BeginUndoBlock{$IFDEF SynUndoDebugBeginEnd}('TSynBaseCompletion.Validate'){$ENDIF};
      BeginUpdate;
      try
        if Editor.Highlighter<>nil then
          HighlighterIdentChars := Editor.Highlighter.IdentChars
        else
          HighlighterIdentChars := [];
        LogCaret := LogicalCaretXY;
        NewBlockBegin:=LogCaret;
        CurLine:=Lines[NewBlockBegin.Y - 1];
        while (NewBlockBegin.X>1) and (NewBlockBegin.X-1<=length(CurLine))
        and ((IsIdentifierChar(@CurLine[NewBlockBegin.X-1]))
             or (CurLine[NewBlockBegin.X-1] in HighlighterIdentChars))
        do
          dec(NewBlockBegin.X);
        //BlockBegin:=NewBlockBegin;
        if (ssShift in Shift)=ToggleReplaceWhole then begin
          // replace the whole word
          NewBlockEnd := LogCaret;
          CurLine:=Lines[NewBlockEnd.Y - 1];
          while (NewBlockEnd.X<=length(CurLine))
          and ((IsIdentifierChar(@CurLine[NewBlockEnd.X]))
               or (CurLine[NewBlockEnd.X] in HighlighterIdentChars))
          do
            inc(NewBlockEnd.X);
        end else begin
          // replace only prefix
          NewBlockEnd := LogCaret;
        end;
        //debugln('TSynBaseCompletion.Validate B Position=',dbgs(Position));
        if Position>=0 then begin
          if Assigned(FOnCodeCompletion) then
          begin
            Value := Form.InsertList[Position];
            FOnCodeCompletion(Value, TextBetweenPoints[NewBlockBegin, NewBlockEnd],
                              NewBlockBegin, NewBlockEnd, KeyChar, Shift);
            if (CompareCarets(NewBlockBegin, NewBlockEnd) <> 0) or (Value <> '') then
            begin
              TextBetweenPointsEx[NewBlockBegin, NewBlockEnd, scamEnd] := Value;
              TCustomSynEdit(F.CurrentEditor).SetFocus;
            end;
          end else
          begin
            TextBetweenPointsEx[NewBlockBegin, NewBlockEnd, scamEnd] := Form.InsertList[Position];
            TCustomSynEdit(F.CurrentEditor).SetFocus;
          end;
        end
        else
        if (Form.ItemsCount = 0) then
          Cancel(Sender);
      finally
        EndUpdate;
        EndUndoBlock{$IFDEF SynUndoDebugBeginEnd}('TSynBaseCompletion.Validate'){$ENDIF};
      end;
    end;
end;

constructor TSynBaseCompletion.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Form.OnValidate := @Validate;
  Form.OnCancel := @Cancel;
  Form.OnPaint:=@OnFormPaint;
  FEndOfTokenChr := '()[].';
  fShortCut := Menus.ShortCut(Ord(' '), [ssCtrl]);
  FExecCommandID := ecSynCompletionExecute;
end;

procedure TSynBaseCompletion.SetShortCut(Value: TShortCut);
begin
  FShortCut := Value;
end;

procedure TSynBaseCompletion.TranslateKey(Sender: TObject; Code: word; SState: TShiftState;
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

procedure TSynBaseCompletion.ProcessSynCommand(Sender: TObject; AfterProcessing: boolean;
  var Handled: boolean; var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer;
  HandlerData: pointer);
var
  p: TPoint;
  i: integer;
begin
  if Handled or (Command <> FExecCommandID) then
    exit;

  i := IndexOfEditor(Sender as TCustomSynEdit);
  if i >= 0 then begin
    with sender as TCustomSynEdit do begin
      if not ReadOnly then begin
        p := ClientToScreen(Point(CaretXPix, CaretYPix + LineHeight + 1));
        Editor := Sender as TCustomSynEdit; // Will set Form.SetCurrentEditor
        Execute(GetPreviousToken(Sender as TCustomSynEdit), p.x, p.y);
        Handled := True;
      end;
    end;
  end;

end;

procedure TSynBaseCompletion.DoBeforeExecute(var ACurrentString: String; var APosition: Integer; var AnX, AnY: Integer; var AnResult: TOnBeforeExeucteFlags);
begin
  if Assigned(FOnBeforeExecute) then
    FOnBeforeExecute(Self, ACurrentString, APosition, AnX, AnY, AnResult);
end;

function TSynBaseCompletion.GetPreviousToken(FEditor: TCustomSynEdit): string;
var
  s: string;
  i: integer;
begin
  if FEditor <> nil then begin
    s := FEditor.LineText;
    i := FEditor.LogicalCaretXY.X - 1;
    if i > length(s) then
      result := ''
    else begin
      while (i > 0) and (s[i] > ' ') and (pos(s[i], FEndOfTokenChr) = 0) do
        Begin
          dec(i);
        end;
      result := copy(s, i + 1, FEditor.LogicalCaretXY.X - i - 1);
    end;
  end
  else
    result := '';
end;

procedure TSynBaseCompletion.DoEditorAdded(AValue: TCustomSynEdit);
begin
  inherited DoEditorAdded(AValue);

  AValue.RegisterCommandHandler(@ProcessSynCommand, nil);
  AValue.RegisterKeyTranslationHandler(@TranslateKey);
end;

procedure TSynBaseCompletion.DoEditorRemoving(AValue: TCustomSynEdit);
begin
  inherited DoEditorRemoving(AValue);
  if Form.CurrentEditor = AValue then
    Form.SetCurrentEditor(nil);

  AValue.UnregisterCommandHandler(@ProcessSynCommand);
  AValue.UnRegisterKeyTranslationHandler(@TranslateKey);
end;

procedure TSynBaseCompletion.SetEditor(const Value: TCustomSynEdit);
begin
  inherited SetEditor(Value);
  Form.SetCurrentEditor(Value);
end;

function TSynBaseCompletion.EditorsCount: integer;
begin
  result := EditorCount;
end;

procedure TSynBaseCompletion.AddCharAtCursor(AUtf8Char: TUTF8Char);
begin
  Form.AddCharAtCursor(AUtf8Char);
end;

procedure TSynBaseCompletion.DeleteCharBeforeCursor;
begin
  Form.DeleteCharBeforeCursor;
end;

procedure TSynBaseCompletion.Clear;
begin
end;

procedure TSynBaseCompletion.Sort;
begin
end;

procedure TSynBaseCompletion.BeginUpdate;
begin
end;

procedure TSynBaseCompletion.EndUpdate;
begin
end;

{ TSynAutoComplete }

constructor TSynAutoComplete.Create(AOwner: TComponent);
begin
  inherited;
  FEndOfTokenChr := '()[].';
  fAutoCompleteList := TStringList.Create;
  fShortCut := Menus.ShortCut(Ord(' '), [ssShift]);
  FExecCommandID := ecSynAutoCompletionExecute;
end;

procedure TSynAutoComplete.SetShortCut(Value: TShortCut);
begin
  FShortCut := Value;
end;

destructor TSynAutoComplete.destroy;
begin
  FreeAndNil(fAutoCompleteList);
  inherited;
end;

function TSynAutoComplete.EditorsCount: integer;
begin
  Result := EditorCount;
end;

procedure TSynAutoComplete.Execute(token: string; aEditor: TCustomSynEdit);
var
  Temp: string;
  i, j, prevspace: integer;
  StartOfBlock: tpoint;
begin
//Writeln('[TSynAutoComplete.Execute] Token is "',Token,'"');
  i := AutoCompleteList.IndexOf(token);
  if i <> -1 then begin
    for j := 1 to length(token) do
      aEditor.CommandProcessor(ecDeleteLastChar, ' ', nil);
    inc(i);
    StartOfBlock := Point(-1, -1);
    PrevSpace := 0;
    while (i < AutoCompleteList.Count) and
      (length(AutoCompleteList[i]) > 0) and
      (AutoCompleteList[i][1] = '=') do begin
      for j := 0 to PrevSpace - 1 do
        aEditor.CommandProcessor(ecDeleteLastChar, ' ', nil);
      Temp := AutoCompleteList[i];
      PrevSpace := 0;
      while (length(temp) >= PrevSpace + 2) and (temp[PrevSpace + 2] <= ' ') do
        inc(PrevSpace);
      for j := 2 to length(Temp) do begin
        aEditor.CommandProcessor(ecChar, Temp[j], nil);
        if Temp[j] = '|' then
          StartOfBlock := aEditor.CaretXY
      end;
      inc(i);
      if (i < AutoCompleteList.Count) and
        (length(AutoCompleteList[i]) > 0) and
        (AutoCompleteList[i][1] = '=') then
        aEditor.CommandProcessor(ecLineBreak, ' ', nil);
    end;
    if (StartOfBlock.x <> -1) and (StartOfBlock.y <> -1) then begin
      aEditor.CaretXY := StartOfBlock;
      aEditor.CommandProcessor(ecDeleteLastChar, ' ', nil);
    end;
  end;
end;

function TSynAutoComplete.GetPreviousToken(aEditor: TCustomSynEdit): string;
var
  s: string;
  i: integer;
begin
  if aEditor <> nil then begin
    s := aEditor.LineText;
    i := aEditor.LogicalCaretXY.X - 1;
    if i > length(s) then
      result := ''
    else begin
      while (i > 0) and (s[i] > ' ') and (pos(s[i], FEndOfTokenChr) = 0) do
        dec(i);
      result := copy(s, i + 1, aEditor.LogicalCaretXY.X - i - 1);
    end;
  end
  else
    result := '';
end;

procedure TSynAutoComplete.TranslateKey(Sender: TObject; Code: word; SState: TShiftState;
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

procedure TSynAutoComplete.ProcessSynCommand(Sender: TObject; AfterProcessing: boolean;
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
        Execute(GetPreviousToken(Sender as TCustomSynEdit), Sender as TCustomSynEdit);
        Handled := True;
      end;
    end;
  end;
end;

procedure TSynAutoComplete.SetAutoCompleteList(List: TStrings);
begin
  fAutoCompleteList.Assign(List);
end;

procedure TSynAutoComplete.DoEditorAdded(AValue: TCustomSynEdit);
begin
  inherited DoEditorAdded(AValue);
  AValue.RegisterCommandHandler(@ProcessSynCommand, nil);
  AValue.RegisterKeyTranslationHandler(@TranslateKey);
end;

procedure TSynAutoComplete.DoEditorRemoving(AValue: TCustomSynEdit);
begin
  inherited DoEditorRemoving(AValue);
  AValue.UnregisterCommandHandler(@ProcessSynCommand);
  AValue.UnRegisterKeyTranslationHandler(@TranslateKey);
end;

function TSynAutoComplete.GetTokenList: string;
var
  List: TStringList;
  i: integer;
begin
  Result := '';
  if AutoCompleteList.Count < 1 then Exit;
  List := TStringList.Create;
  i := 0;
  while (i < AutoCompleteList.Count) do begin
    if (length(AutoCompleteList[i]) > 0) and (AutoCompleteList[i][1] <> '=') then
      List.Add(Trim(AutoCompleteList[i]));
    inc(i);
  end;
  Result := List.Text;
  List.Free;
end;

function TSynAutoComplete.GetTokenValue(Token: string): string;
var
  i: integer;
  List: TStringList;
begin
  Result := '';
  i := AutoCompleteList.IndexOf(Token);
  if i <> -1 then begin
    List := TStringList.Create;
    Inc(i);
    while (i < AutoCompleteList.Count) and
      (length(AutoCompleteList[i]) > 0) and
      (AutoCompleteList[i][1] = '=') do begin
      if Length(AutoCompleteList[i]) = 1 then
        List.Add('')
      else
        List.Add(Copy(AutoCompleteList[i], 2, Length(AutoCompleteList[i])));
      inc(i);
    end;
    Result := List.Text;
    List.Free;
  end;
end;

{ TSynBaseCompletionHint }

procedure TSynBaseCompletionHint.Paint;
var
  R: TRect;
begin
  if FCompletionForm.Position = FIndex then
    Canvas.Brush.Color := FCompletionForm.ClSelect
  else
    Canvas.Brush.Color := Color;

  Canvas.Pen.Width := 1;
  R := ClientRect;
  Canvas.FillRect(R);
  DrawEdge(Canvas.Handle, R, BDR_RAISEDOUTER, BF_RECT);
  Canvas.Font.Color := FCompletionForm.TextColor;

  if not Assigned(FCompletionForm.OnPaintItem)
  or not FCompletionForm.OnPaintItem(Caption, Canvas, 1, 1,
                                     FCompletionForm.Position = FIndex, FIndex)
  then
  begin
    if FCompletionForm.UsePrettyText then
    begin
      InflateRect(R, -1, -1);
      FormattedTextOut(Canvas, R, Caption, FCompletionForm.Position = FIndex, nil);
    end
    else
      Canvas.TextOut(2, 2, Caption);
  end;
end;

constructor TSynBaseCompletionHint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Canvas.Brush.Style := bsSolid;
  FCompletionForm := AOwner as TSynVirtualCompletionForm;
  Color := FCompletionForm.BackgroundColor;
  AutoHide := False;
  Visible := False;
end;

function TSynBaseCompletionHint.CalcHintRect(MaxWidth: Integer; const AHint: string;
  AData: pointer): TRect;
var
  P: TPoint;
begin
  if Assigned(FCompletionForm.OnMeasureItem) then
  begin
    Result.TopLeft := Point(0, 0);
    P := FCompletionForm.OnMeasureItem(AHint, Canvas,
                                     FCompletionForm.Position = FIndex, FIndex);
    Result.Bottom := P.Y + 2;
    Result.Right := P.X + 4;
  end
  else
    Result := Rect(0, 0, Canvas.TextWidth(AHint) + 4, FCompletionForm.FontHeight);
end;

const
  SynComplutionCommandStrs: array[0..1] of TIdentMapEntry = (
    (Value: ecSynCompletionExecute;       Name: 'ecSynCompletionExecute'),
    (Value: ecSynAutoCompletionExecute;   Name: 'ecSynAutoCompletionExecute')
  );

function IdentToSynCompletionCommand(const Ident: string; var Cmd: longint): boolean;
begin
  Result := IdentToInt(Ident, Cmd, SynComplutionCommandStrs);
end;

function SynCompletionCommandToIdent(Cmd: longint; var Ident: string): boolean;
begin
  Result := (Cmd >= ecPluginFirstCompletion) and (Cmd - ecPluginFirstCompletion < ecSynCompletionCount);
  if not Result then exit;
  Result := IntToIdent(Cmd, Ident, SynComplutionCommandStrs);
end;

procedure GetEditorCommandValues(Proc: TGetStrProc);
var
  i: integer;
begin
  for i := Low(SynComplutionCommandStrs) to High(SynComplutionCommandStrs) do
    Proc(SynComplutionCommandStrs[I].Name);
end;


initialization
  RegisterKeyCmdIdentProcs(@IdentToSynCompletionCommand,
                           @SynCompletionCommandToIdent);
  RegisterExtraGetEditorCommandValues(@GetEditorCommandValues);
end.

