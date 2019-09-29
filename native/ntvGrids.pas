unit ntvGrids;

{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}
{$M+}{$H+}
interface

uses
  SysUtils, Variants, Messages, Classes, Graphics, Controls,
  LCLType, LCLIntf, fgl,
  StdCtrls, Dialogs, Math, Menus, Forms, ImgList, Contnrs,
  ColorUtils, mnClasses, UniDates,
  ntvCtrls, ntvThemes;

const
  sGridVersion = 'NativeGrid=v1.0';

  sColWidth = 80;
  sCellMargin = 3;
  sIndicatorWidth = 40;
  sFringeWidth = 40;



  cntv_EOC = #9;
  cntv_EOL = #13;
  cntv_X_EOC = #29;
  cntv_X_EOL = #30;
  cntv_X_DATA = #31;

type
  EntvGridException = class(Exception)
  end;

  TntvKeyAction = (
    keyaNone, keyaDown, keyaUp, keyaLeft, keyaRight, keyaPageUp, keyaPageDown, keyaHome, keyaEnd,
    keyaReturn, keyaTab, keyaEscape, keyaCopy, keyaPaste, keyaCut, keyaFindFirst, keyaFindNext,
    keyaHelp, keyaDelete, keyaInsert, keyaInsertLine, keyaDeleteLine, keyaEdit, keyaBrowse,
    keyaNew, keyaProperty, keyaWordLeft, keyaWordRight, keyaScrollUp, keyaScrollDown,
    keyaSave, keyaTopPage, keyaBottomPage, keyaTop, keyaBottom, keyaSelectLeft, keyaSelectRight,
    keyaSelectUp, keyaSelectDown, keyaSelectPageUp, keyaSelectPageDown, keyaDropDown,
    keyaDropUp, keyaSaveAll, keyaFindPrior, keyaSelectHome, keyaSelectEnd, keyaBack, keyaForward,
    keyaComplete, keyaHiComplete, keyaLoComplete, keyaSelectTop, keyaSelectBottom,
    keyaExecute, keyaViewer, keyaFit, keyaFunction, keyaRepeat, keyaGoTo
  );

  TntvMasterAction = (
    mactLeft,
    mactRight,
    mactUp,
    mactDown,
    mactPageUp,
    mactPageDown,
    mactHome,
    mactEnd,
    mactEdit,
    mactExit,
    mactCancel,
    mactAccept
  );

  { IMaster }

  IMaster = interface(IInterface)
    ['{37D0D904-0354-4988-A50E-3E306F2F1883}']
    procedure MasterAction(vMove: TntvMasterAction);
  end;

  TntvRows = class;
  TntvRow = class;

  { TntvList }

  TntvList<_Object_> = class abstract(TmnObjectList<_Object_>)
  public
    type
      TOnGetKey = function(Index: Integer): Integer of object;
      TOnCompareLine = function(Index1, Index2: Integer): Integer of object;
  private
    FUpdateCount: Integer;
    procedure InternalQuickSort(L, R: Integer; SCompare: TOnCompareLine);
    function GetUpdating: Boolean;
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    function CreateItem: _Object_; virtual; abstract;
    function Require(Index: Integer): _Object_; override;
    procedure Changed; virtual;
    procedure CountChanged; virtual;
    procedure Update;
  public
    procedure BeginUpdate;
    procedure EndUpdate;
    function SortByRange(vFrom, vTo: Integer; OnCompareLine: TOnCompareLine): Boolean;
    function Sort(OnCompareLine: TOnCompareLine): Boolean;
    function SortByTree(OnGetParentKey, OnGetKey: TOnGetKey): Boolean;
    property Updating: Boolean read GetUpdating;
  end;

  { TntvCell }

  TntvCell = class(TPersistent)
  private
    FText: String;
    FData: Integer;
    FRow: TntvRow;
  protected
    procedure SetText(const S: String); virtual;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(ARow: TntvRow); virtual;
    destructor Destroy; override;
    property Text: String read FText write SetText;
    property Data: Integer read FData write FData default 0;
  end;

  TntvCellClass = class of TntvCell;

  { TntvRow }

  TntvRow = class(TntvList<TntvCell>)
  private
    FData: Integer;
    FModified: Boolean;
    FRows: TntvRows;
  protected
    procedure Assign(Source: TntvRow);
    function CreateItem: TntvCell; override;
  public
    constructor Create(ARows: TntvRows); virtual;
    procedure Changed; override;
    destructor Destroy; override;
    function SafeGet(Index: Integer): TntvCell;
    property Data: Integer read FData write FData;
    property Modified: Boolean read FModified write FModified;
  end;

  TntvRowClass = class of TntvRow;
  TntvCustomGrid = class;

  { TntvRows }

  TntvRows = class(TntvList<TntvRow>)
  private
    FGrid: TntvCustomGrid;
  protected
    procedure Changed; override;
    procedure CountChanged; override;
    function CreateItem: TntvRow; override;
  public
    constructor Create(Grid: TntvCustomGrid);
    function SafeGet(vRow: Integer): TntvRow; overload;
    function SafeGet(vRow, vCol: Integer): TntvCell; overload;
    function CheckEmpty(vRow: Integer): Boolean;
    function CopyLine(vFromRow, vToRow: Integer): Boolean;
  end;

  TntvColumnKind = (cokText, cokDate, cokTime, cokNumber, cokBoolean, cokData);
  TntvDragAfterMode = (damNone, damScroll, damDrag);
  TntvState = (dgsNone, dgsDown, dgsDrag, dgsResizeCol, dgsResizeRow, dgsDragSelect, dgsDragMove);

  TntvGridArea = (garNone, garNormal, garHeader, garFooter, garIndicator, garFringe);
  TntvCellDrawState = set of (csdDown, csdSelected, csdCurrent, csdFixed, csdRightToLeft, csdFirstCell, csdLastCell, csdOpened);
  TntvGridLines = (glNone, glVertical, glHorizontal, glBoth);
  TntvGridOpenEdit = (goeNone, goeReturn, goeChar, goeMouse);

  TntvSetCell = (swcText, swcData, swcRefresh, swcComplete, swcCheckInfo);
  TntvSetCells = set of TntvSetCell;

  TntvColumn = class;
  TntvColumns = class;

  TOnGetColor = procedure(Sender: TntvCustomGrid; Column: TntvColumn; vRow: Integer; var vColor: TColor) of object;
  TOnNotifyRow = procedure(Sender: TntvCustomGrid; vRow: Integer) of object;
  TOnNotifyCell = procedure(Sender: TntvCustomGrid; Column: TntvColumn; vRow: Integer) of object;
  TOnNotifyButton = procedure(Sender: TntvCustomGrid; Column: TntvColumn; X, Y: Integer) of object;
  TOnIsReadOnly = procedure(Sender: TntvCustomGrid; Column: TntvColumn; vRow: Integer; var vReadOnly: Boolean) of object;
  TOnValueChanged = procedure(Sender: TntvCustomGrid; Column: TntvColumn; vRow: Integer) of object;

  { TntvColumnProperty }

  TntvColumnProperty = class(TComponent)
  private
    FVisible: Boolean;
    FCaption: String;
    FWidth: Integer;
    FIndex: Integer;
  public
    property Index: Integer read FIndex write FIndex;
  published
    property Caption: String read FCaption write FCaption;
    property Visible: Boolean read FVisible write FVisible;
    property Width: Integer read FWidth write FWidth;
  end;

  { TntvGridProperty }

  TntvGridProperty = class(TComponent)
  private
    FSettledCols: Integer;
    FSettledColor: TColor;
    FOddColor: TColor;
    FEvenColor: TColor;
    FReturnColumns: Integer;
    FVerticalJump: Boolean;
    FCapacity: Integer;
    FCurRow: Integer;
    FCurCol: Integer;
    FGridLines: TntvGridLines;
    FLinesColor: TColor;
  protected
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetChildOwner: TComponent; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromFile(FileName: String);
    procedure SaveToFile(FileName: String);
  published
    property ReturnColumns: Integer read FReturnColumns write FReturnColumns default 0;
    property CurRow: Integer read FCurRow write FCurRow default -1;
    property CurCol: Integer read FCurCol write FCurCol default -1;
    property Capacity: Integer read FCapacity write FCapacity default 10000;
    property VerticalJump: Boolean read FVerticalJump write FVerticalJump default False;
    property GridLines: TntvGridLines read FGridLines write FGridLines;
    property LinesColor: TColor read FLinesColor write FLinesColor default $00DDDDDD;
    property OddColor: TColor read FOddColor write FOddColor default $00F8F8F8;
    property EvenColor: TColor read FEvenColor write FEvenColor default clWhite;
    property SettledColor: TColor read FSettledColor write FSettledColor default $00EAEAEA;
    property SettledCols: Integer read FSettledCols write FSettledCols default 0;
  end;

  { TntvColumnInfo }

  TntvColumnInfo = record
    Width: Integer;
    TextFormat: TTextStyle;
    Caption: String;
    Title: String;
    Query: String;
    Hint: String;
    Value: Variant;
    Kind: TntvColumnKind;
    IsTotal: Boolean;
    Total: Currency;
    ReadOnly: Boolean;
    Visible: Boolean;
    AutoSize: Boolean;
    ShowImage: Boolean;
    OrignalWidth: Integer;
    ID: Integer;
    EmptyZero: Boolean;
    Store: Boolean;
    StoreCol: Integer;
  end;

  { TntvColumn }

  TntvColumn = class(TInterfacedPersistent, IMaster)
  private
    FName: String;
    FColumns: TntvColumns;
    FBiDiMode: TBiDiMode;
    FParentBiDiMode: Boolean;
    FImageIndex: Integer;
    FAlignment : TAlignment;
    FCurrencyFormat: String;
    FShowButton: Boolean;

    FVisibleIndex: Integer;

    FMasterActionLock: Boolean;

    FEnabled: Boolean;
    FOldData: Integer;
    FBtnRect: TRect;
    FDown: Boolean;
    FMouseInHeader: Boolean;
    procedure BiDiModeChanged(vInvalidate: Boolean);
    procedure ParentBiDiModeChanged;
    function GetGuid: String;
    function GetIsNull: Boolean;
    function GetAsCurrency: Currency;
    function GetAsFloat: Double;
    function GetAsVariant: Variant;
    function GetAsDate: TDateTime;
    function GetAsDateTime: TDateTime;
    function GetAsInteger: Integer;
    function GetAsTime: TDateTime;
    function GetAsTotal: Currency;
    function GetActiveData: Integer;
    procedure SetAlignment(AValue: TAlignment);
    procedure SetAsCurrency(const Value: Currency);
    procedure SetAsFloat(const Value: Double);
    procedure SetAsDate(const Value: TDateTime);
    procedure SetAsDateTime(const Value: TDateTime);
    procedure SetAsInteger(const Value: Integer);
    procedure SetAsVariant(const Value: Variant);
    function GetAsString: String;
    procedure SetAsString(const Value: String);
    procedure SetAsTime(Value: TDateTime);
    procedure SetAsTotal(const Value: Currency);
    procedure SetData(const Value: Integer);
    procedure SetVisible(const Value: Boolean);
    function IsBiDiModeStored: Boolean;
    procedure SetBiDiMode(const Value: TBiDiMode);
    procedure SetParentBiDiMode(const Value: Boolean);
    procedure SetWidth(const Value: Integer);
    function GetAsBoolean: Boolean;
    procedure SetAsBoolean(const Value: Boolean);
    function GetItems(Row: Integer): TntvCell;
    function GetActiveRow: Integer;
    procedure SetImageIndex(const Value: Integer);
    function GetImageList: TImageList;
    procedure SetEnabled(const Value: Boolean);
    procedure SetShowButton(const Value: Boolean);
    procedure SetDown(const Value: Boolean);
    procedure SetMouseInHeader(const Value: Boolean);
  protected
    Info: TntvColumnInfo;
    FEditControl: TControl;
    function GetTextStyle(vCentered: Boolean = False): TTextStyle; overload;
    function GetButtonRect(vRect: TRect): TRect; virtual;
    procedure GetCurrentColor(vRow: Integer; var vColor: TColor); virtual;
    procedure InternalLookup(vData: Integer; var vText: String); virtual;
    function GetRect(vRow: Integer; var vRect: TRect): Boolean;
    function GetCellArea(vRow: Integer; var vRect: TRect): Boolean;
    function GetTextArea(vRow: Integer; var vRect: TRect): Boolean;
    procedure CorrectCellRect(var Rect: TRect);
    procedure Draw(Canvas: TCanvas; vDrawState: TntvCellDrawState; vRow: Integer; vRect: TRect; vArea: TntvGridArea); virtual;
    procedure DrawHeader(Canvas: TCanvas; vRow: Integer; vRect: TRect; State: TntvCellDrawState); virtual;
    procedure DrawCell(Canvas: TCanvas; vRow: Integer; vRect: TRect; State: TntvCellDrawState; const vColor: TColor); virtual;
    procedure DrawHint(Canvas: TCanvas; vRow: Integer; vRect: TRect; State: TntvCellDrawState; const vColor: TColor); virtual;
    procedure DrawFooter(Canvas: TCanvas; vRow: Integer; vRect: TRect; State: TntvCellDrawState); virtual;
    function GetCaption: String;
    function GetDisplayName: String;
    function UseRightToLeftAlignment: Boolean; dynamic;
    function UseRightToLeftReading: Boolean;
    procedure ValidateInfo(var Text: String; var Data: Integer); virtual;
    procedure CheckInfo(var Text: String; var Data: Integer); virtual;
    procedure Validate; virtual;
    procedure Validated; virtual;
    procedure ValueChanging; virtual;
    procedure ValueChanged; virtual;
    procedure CompleteCell(vComplete, vForce: Boolean);

    procedure InternalSetInfo(vRow: Integer; vText: String; vData: Integer; vSetCell: TntvSetCells; vForce: Boolean = False);
    function SetInfo(vRow: Integer; vText: String; vData: Integer; vSetCellKind: TntvSetCells = [swcText, swcData, swcRefresh, swcComplete]): Boolean;

    procedure RefreshCell(vRow: Integer); overload;
    procedure RefreshRow(vRow: Integer); overload;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure KeyDown(var KEY: Word; Shift: TShiftState); virtual;

    procedure MasterAction(vMove: TntvMasterAction);

    function CanEdit: Boolean; virtual;
    function CreateEdit: TControl; virtual;
    procedure NeedEdit;
    function CloseEdit(Accept: Boolean): Boolean;
    function OpenEdit(OpenBy: TntvGridOpenEdit; Key: Char): Boolean;
    procedure HideEdit; virtual;
    procedure ShowEdit(vChar: Char = #0); virtual;
    procedure FreeEdit; virtual;
    procedure SendChar(vChar: Char); virtual;

    procedure SelectAll; virtual;
    procedure SelectPos(X: Integer); virtual;

    function GetEditData: Integer; virtual;
    procedure SetEditData(const Value: Integer); virtual;
    function GetEditText: String; virtual;
    procedure SetEditText(const Value: String); virtual;
    procedure DragOver(Source: TObject; State: TDragState; vRow: Integer; var Accept: Boolean); virtual;
    procedure DragDrop(Source: TObject; vRow: Integer); virtual;
    function CreateColumnProperty(AOwner: TComponent): TntvColumnProperty;
    procedure SetColumnProperty(AProperty: TntvColumnProperty);
    procedure CurRowChanged; virtual;
    property Down: Boolean read FDown write SetDown;
    property MouseInHeader: Boolean read FMouseInHeader write SetMouseInHeader;

    property Name: String read FName;
    property Columns: TntvColumns read FColumns;
  public
    function GetReadOnly: Boolean;
    constructor Create(vColumns: TntvColumns; vTitle: String = ''; vName: String = ''; vID: Integer = 0; vEnabled: Boolean = True); virtual;
    procedure Invalidate;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function Grid: TntvCustomGrid;
    procedure Complete;
    procedure Zero(vRow: Integer);
    procedure InvalidateHeader;
    //<edit>
    function IsFocused(Handle: THandle = 0): Boolean; virtual;
    property EditData: Integer read GetEditData write SetEditData;
    property EditText: String read GetEditText write SetEditText;
    procedure SetValue(const vText: String; vData: Integer);
    procedure SetSilentValue(const vText: String; vData: Integer);
    procedure Lookup(const vData: Integer; vForce: Boolean = False);
    function GetCell(vRow: Integer): TntvCell;
    function GetData(vRow: Integer): Integer;
    function GetText(vRow: Integer): String;

    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsDouble: Double read GetAsFloat write SetAsFloat;
    property AsDate: TDateTime read GetAsDate write SetAsDate;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsString: String read GetAsString write SetAsString;
    property AsTime: TDateTime read GetAsTime write SetAsTime;
    property AsTotal: Currency read GetAsTotal write SetAsTotal;
    property Data: Integer read GetActiveData write SetData;
    property OldData: Integer read FOldData;
    property Kind: TntvColumnKind read Info.Kind write Info.Kind;
    property Query: String read Info.Query write Info.Query;
    property Total: Currency read Info.Total write Info.Total;
    property StoreCol: Integer read Info.StoreCol write Info.StoreCol;
    property VisibleIndex: Integer read FVisibleIndex write FVisibleIndex;
    property EmptyZero: Boolean read Info.EmptyZero write Info.EmptyZero;
    property Items[Row: Integer]: TntvCell read GetItems; default;
    property ActiveRow: Integer read GetActiveRow;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property ImageList: TImageList read GetImageList default nil;
    property ShowButton: Boolean read FShowButton write SetShowButton default False;
    property BtnRect: TRect read FBtnRect write FBtnRect;
    property TextFormat: TTextStyle read Info.TextFormat write Info.TextFormat;
  published
    property BiDiMode: TBiDiMode read FBiDiMode write SetBiDiMode stored IsBiDiModeStored;
    property ParentBiDiMode: Boolean read FParentBiDiMode write SetParentBiDiMode default True;
    property Hint: String read Info.Hint write Info.Hint;
    property IsTotal: Boolean read Info.IsTotal write Info.IsTotal default False;
    property ReadOnly: Boolean read Info.ReadOnly write Info.ReadOnly default False;
    //Title a column header text
    property Title: String read Info.Title write Info.Title;
    //Caption keep it empty until you want override the title that will draw in header
    property Caption: String read Info.Caption write Info.Caption;
    property Visible: Boolean read Info.Visible write SetVisible default True;
    property Width: Integer read Info.Width write SetWidth default sColWidth;
    property Id: Integer read Info.Id write Info.Id default 0;
    property Store: Boolean read Info.Store write Info.Store default True;
    property AutoSize: Boolean read Info.AutoSize write Info.AutoSize default False;
    property ShowImage: Boolean read Info.ShowImage write Info.ShowImage default False;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property CurrencyCustomFormat: String read FCurrencyFormat write FCurrencyFormat;
  end;

  TntvColumnClass = class of TntvColumn;

  { TntvColumns }

  TntvColumns = class(TmnObjectList<TntvColumn>)
  private
    FGrid: TntvCustomGrid;
    function GetItems(Index: Integer): TntvColumn;
    procedure SetItems(Index: Integer; ColNode: TntvColumn);
  protected
    FCapacity: Integer;
    function Search(vName: String): Integer;
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    Default: Boolean;
    constructor Create(vGrid: TntvCustomGrid); virtual;
    destructor Destroy; override;
    procedure ClearTotals;
    function Find(ID: Integer): TntvColumn; overload;
    function Find(const Name: String): TntvColumn; overload;
    property Grid: TntvCustomGrid read FGrid;
    property Capacity: Integer read FCapacity;
    property Items[Index: Integer]: TntvColumn read GetItems write SetItems; default;
  end;

  TntvSelectedRows = class(TFPGList<Integer>)
  private
  public
    function Found(const Row: Integer): Integer;
  end;

  TGridSelected = class(TPersistent)
  private
    FColor: TColor;
    FStartRow: Integer;
    FGrid: TntvCustomGrid;
    FTextColor: TColor;
    FEndRow: Integer;
    FSelectedRows: TntvSelectedRows;
    procedure SetColor(const Value: TColor);
    procedure SetTextColor(const Value: TColor);
  public
    constructor Create(Grid: TntvCustomGrid);
    destructor Destroy; override;
    procedure InternalSelect(Row: Integer);
    procedure Select(vOldRow, vRow: Integer; Shift: TShiftState);
    function IsSelected(vRow: Integer): Boolean;
    property StartRow: Integer read FStartRow write FStartRow;
    property EndRow: Integer read FEndRow write FEndRow;
    property Rows: TntvSelectedRows read FSelectedRows;
  published
    property Color: TColor read FColor write SetColor default clHighlight;
    property TextColor: TColor read FTextColor write SetTextColor default clWhite;
  end;

  TColumnList = class(TList)
  private
    function GetItems(Index: Integer): TntvColumn;
  public
    function Add(Item: TntvColumn): Integer;
    function Find(const Name: String): TntvColumn;
    property Items[Index: Integer]: TntvColumn read GetItems; default;
  end;

  { TntvCustomGrid }

  TntvCustomGrid = class(TntvCustomControl)
  private
    FRows: TntvRows;
    FSelected: TGridSelected;

    FStartSizing: Integer;
    FLockAutoSumCount: Integer;
    FChaseCell: Boolean;
    FClkCol: Integer;
    FClkRow: Integer;
    FClkArea: TntvGridArea;
    FColumnEdit: TntvColumn;
    FColumns: TntvColumns;
    FColWidth: Integer;
    FOldRow: Longint;
    FCurCol: Integer;
    FCurRow: Integer;
    FDesignColumns: Boolean;
    FDragAfter: Integer;
    FDragAfterMode: TntvDragAfterMode;
    FDualColor: Boolean;
    FEvenColor: TColor;
    FFixedCols: Integer;
    FFollowDrag: Boolean;
    FFooter: Boolean;
    FGridLines: TntvGridLines;
    FHeader: Boolean;
    FLockCount: Integer;
    FModified: Boolean;
    FOddColor: TColor;
    FOldX: Integer;
    FOldY: Integer;
    FState: TntvState;
    FStateBtn: Boolean;

    FOnAfterEdit: TNotifyEvent;
    FOnChanged: TNotifyEvent;
    FOnCellClick: TOnNotifyCell;
    FOnRowClick: TOnNotifyRow;
    FOnValueChanged: TOnValueChanged;
    FOnCurChanged: TOnNotifyCell;
    FOnCurRowChanged: TOnNotifyRow;
    FOnCountChanged: TNotifyEvent;
    FOnModified: TNotifyEvent;
    FOnPopupMenu: TNotifyEvent;
    FOnGetColor: TOnGetColor;
    FOnIsReadOnly: TOnIsReadOnly;

    FMultiSelect: Boolean;
    FRowRefresh: Boolean;
    FReadOnly: Boolean;
    FRow: Integer;
    FRowHeight: Integer;
    FRowNumbers: Boolean;
    FCapacity: Integer;
    FScrollBars: TScrollStyle;

    FRowSelect: Boolean;
    FSideCol: Integer;
    FTopRow: Integer;
    FSettledCols: Integer;
    FIndicator: Boolean;
    FFringe: Boolean;
    FFringeWidth: Integer;
    FIndicatorWidth: Integer;
    FVisibleColumns: TColumnList;
    FBorderStyle: TBorderStyle;
    FScrollTimer: Cardinal;
    FSettledColor: TColor;
    FSolid: Boolean;
    FActiveRow: Integer;
    FImageList: TImageList;
    FImageChangeLink: TChangeLink;
    FFullHeader: Boolean;
    FAttemptCapture: Boolean;
    FVerticalJump: Boolean;
    FReturnColumns: Integer;
    FWantTab: Boolean;
    FWantReturn: Boolean;
    FDownCol: Integer;
    FOnButtonClick: TOnNotifyButton;
    FUnderMouseCol: Integer;
    FLinesColor: TColor;
    FFixedColor: TColor;
    procedure ChangeCursor(x, y: Integer);
    function GetValues(Row, Col: Integer): string;
    procedure SetValues(Row, Col: Integer; AValue: string);
    procedure SetImageList(Value: TImageList);
    procedure ImageListChange(Sender: TObject);
    procedure CalcNewWidth(X: Smallint);
    procedure BeginCapture(X, Y: Integer);
    procedure EndCapture;
    function IsSelected(vRow: Integer; vCol: Integer): Boolean;
    function IsCurrent(vRow: Integer; vCol: Integer): Boolean;
    function GetTextRange(vStartRow, vEndRow: Integer; SpecialFormat: Boolean): String;
    procedure ColsScroll(vCols: Integer);
    procedure DrawColSizeLine(X: Integer);
    procedure SetLockAutoSum(const Value: Boolean);
    function GetLockAutoSum: Boolean;
    function GetLock: Boolean;
    procedure SetLock(const Value: Boolean);
    procedure SetModified(Value: Boolean);

    function GetColRect(vCol: Integer; out vRect: TRect): Boolean;
    function GetRowRect(vRow: Integer; out vRect: TRect): Boolean;
    function GetCellRect(vRow: Integer; vCol: Integer; var vRect: TRect): Boolean; overload;
    function GetCellRect(vRow: Integer; vCol: Integer; vArea: TntvGridArea; var vRect: TRect): Boolean; overload;
    function GetCurrentColumn: TntvColumn;
    function GetCompletedRows(vHeight: Integer): Integer; overload;
    function GetVisibleRows(vHeight: Integer): Integer; overload;
    function GetVisibleRows: Integer; overload;
    function GetVisibleCol(vVertCol: Integer; var vFactCol: Integer): Boolean;
    function GetCompletedCols(vWidth: Integer): Integer;
    function InColsWidth(X: Integer; var vCol: Integer): Boolean;
    function GetVisibleColumn(vCol: Integer): TntvColumn;
    procedure RowsScroll(vRows: Integer);
    procedure SetColumnEdit(const Value: TntvColumn);
    procedure SetCurCol(const Value: Integer);
    procedure SetCurRow(const Value: Integer);
    procedure SetColWidth(Value: Integer);
    procedure SetEvenColor(Value: TColor);
    procedure SetFixedCols(Value: Integer);
    procedure SeTntvGridLines(Value: TntvGridLines);
    procedure SetOddColor(Value: TColor);
    procedure SetRowHeight(Value: Integer);
    procedure SetScrollBars(Value: TScrollStyle);
    procedure ShowScrolls(Value: TScrollStyle);

    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); Message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); Message WM_GETDLGCODE;
    procedure WMHScroll(var Message: TWMHScroll); Message WM_HSCROLL;
    procedure WMKillFocus(var Message: TWMKillFocus); Message WM_KILLFOCUS;
    procedure DoExit; override;
    procedure WMSetFocus(var Message: TWMSetFocus); Message WM_SETFOCUS;
    procedure WMSize(var Message: TWMSize); Message WM_SIZE;
    procedure WMTimer(var Message: TWMTimer); Message WM_Timer;
    procedure WMVScroll(var Message: TWMVScroll); Message WM_VSCROLL;

    procedure CMExit(var Message: TCMExit); Message CM_Exit;
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); Message CM_DESIGNHITTEST;
    procedure CMBiDiModeChanged(var Message: TMessage); Message CM_BIDIMODECHANGED;
    procedure SetLinesColor(const Value: TColor);
    procedure SetFixedColor(const Value: TColor);
    procedure SetMultiSelect(const Value: Boolean);
    property ColumnEdit: TntvColumn read FColumnEdit write SetColumnEdit;
    procedure SetSettledCols(const Value: Integer);
    procedure SetFooter(const Value: Boolean);
    procedure SetFringe(const Value: Boolean);
    procedure SetHeader(const Value: Boolean);
    procedure SetIndicator(const Value: Boolean);
    function GetCurCell: TntvCell;
    function FlipRect(const cliRect, vRect: TRect): TRect;
    procedure SetSettledColor(const Value: TColor);
    procedure CopyClick(Sender: TObject);
    procedure CopyAllClick(Sender: TObject);
    procedure DeleteLineClick(Sender: TObject);
    procedure InsertLineClick(Sender: TObject);
    procedure SortUpClick(Sender: TObject);
    procedure SortDownClick(Sender: TObject);
    procedure PasteClick(Sender: TObject);
    procedure SetIndicatorWidth(const Value: Integer);
    procedure SetFringeWidth(const Value: Integer);
    procedure SetDualColor(const Value: Boolean);
    procedure SetChaseCell(const Value: Boolean);
    procedure SetRowNumbers(const Value: Boolean);
    procedure SetRowSelect(const Value: Boolean);
    function GetItems(Index: Integer): TntvRow;
    procedure SetInternalActiveRow(const Value: Integer);
    procedure SetActiveRow(const Value: Integer);
    procedure SetFullHeader(const Value: Boolean);
    function OnSortByCol(Index1, Index2: Integer): Integer;
  protected
    FSortColumn: TntvColumn;
    FSortDown: Boolean;
    FindingText: String;
    ShouldCurChange: Boolean;
    procedure Loaded; override;
    procedure Paint; override;

    function GetDefaultRowHeight: integer; virtual;
    function GetTextStyle(vCentered: Boolean = False): TTextStyle; virtual;
    function CanEdit(const Row, Col: Integer): Boolean; virtual;
    function GetReadOnly: Boolean; virtual;
    procedure ProcessScrollTimer;
    procedure PasteRange(vText: String; SpecialFormat, vCheckData: Boolean); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure KeyPress(var Key: char); override;

    procedure Validate(AColumn: TntvColumn); virtual;
    procedure UpdateColumnsWidths;
    procedure UpdateScrollBar;
    procedure ColsChanged; virtual;
    procedure DoModified; virtual;
    procedure GetColor(Column: TntvColumn; vRow: Integer; var vColor: TColor);
    procedure GetCurrentColor(Column: TntvColumn; vRow: Integer; var vColor: TColor); virtual;
    procedure Validated(AColumn: TntvColumn); virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure CurChanged(vRow: Integer; vCol: Integer); virtual;
    procedure DoAfterEdit(AColumn: TntvColumn; vRow: Integer); virtual;
    procedure DoBeforeEdit(AColumn: TntvColumn; vRow: Integer); virtual;
    procedure DoClickArea(vRow: Integer; vCol: Integer; vArea: TntvGridArea); virtual;
    procedure DoCurChange(var vRow: Integer; var vCol: Integer);
    procedure DoCurRowChanging(vOldRow, vNewRow: Integer); virtual;
    procedure DoCurRowChanged; virtual;
    function InternalInvalidate: Boolean; virtual;
    procedure DoButtonClick(Column: TntvColumn; X, Y: Integer); virtual;
    procedure DrawFixed(Canvas: TCanvas; vRect: TRect; S: String; vDrawState: TntvCellDrawState);
    procedure Draw(Canvas: TCanvas; wndRect, pntRect: TRect);
    procedure DrawRow(Canvas: TCanvas; vRow: Integer; vRect, pntRect: TRect; vArea: TntvGridArea);
    procedure CountChanged;
    function GetMaxSideCol: Integer;
    function GetAllowedRows: Integer;
    function GetCompletedRows: Integer; overload;
    function GetCount: Integer; virtual;
    function GetFactRow(vRow: Integer): Integer;
    function GetMaxCols: Integer; virtual;
    function GetCapacity: Integer; virtual;
    function GetVirtualCol(vCol: Integer): Integer;
    function GetVirtualRow(vRow: Integer): Integer;
    procedure InternalSetCurCol(vCol: Integer; Selecting: Boolean);
    procedure InternalSetCurRow(vRow: Integer; Selecting: Boolean);
    function InvlidateRowsClient: Boolean;
    //BlankRow is a new row ready for type, hint draw in it with gray color
    function IsBlankRow(vRow: Integer): Boolean;
    function IsSelecting: Boolean;
    procedure LockChanged(Value: Boolean); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SelectRows(vOldRow, vRow: Integer);
    procedure SetCount(Value: Integer);
    procedure SetCapacity(Value: Integer);
    procedure SetSideCol(vCol: Integer);
    procedure SetTopRow(vRow: Integer); virtual;
    function VirtualHeight: Integer;
    function VirtualWidth: Integer;
    function VirtualClient: TRect;
    function RowsClient: TRect;
    function MovingRect: TRect;
    procedure ValueChanging(AColumn: TntvColumn); virtual;
    procedure ValueChanged(AColumn: TntvColumn); virtual;
    property LockAutoSum: Boolean read GetLockAutoSum write SetLockAutoSum;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyAccident(vKeyAccident: TntvKeyAction; var Resumed: Boolean); virtual;
    function CreateRows: TntvRows; virtual;
    procedure InitPopupMenu; virtual;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    procedure DoIsReadOnly(Column: TntvColumn; vRow: Integer; var vReadOnly: Boolean); virtual;
    function GetColumnProperties: TntvGridProperty;
    procedure SetColumnProperties(List: TntvGridProperty);
    procedure TryDeleteRow(vRow: Integer);
    procedure TryInsertRow(vRow: Integer);
    procedure CanDeleteRow(vRow: Integer; var Accept: Boolean); virtual;
    procedure RowDeleted(vRow: Integer); virtual;
    procedure ColChanged(OldCol, NewCol: TntvColumn); virtual;
    procedure DoChanged; virtual;
    //Usefull to override it and control jumping after editing cell
    procedure MoveCurrentHorizontal(var vCol: Integer); virtual;
    procedure DrawString(vCanvas: TCanvas; vText: String; vRect: TRect; vFormat: TTextStyle; vClipping: Boolean = False);

    property RowHeight: Integer read FRowHeight write SetRowHeight;
    property ColWidth: Integer read FColWidth write SetColWidth;
  public
    ReportRow: Integer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddTotals(vRow: Integer);
    procedure SubTotals(vRow: Integer);
    procedure MoveTop;
    procedure MoveUp;
    procedure MoveDown;
    procedure MoveBottom;
    procedure Clear; virtual;
    procedure Reset; virtual;
    procedure StartUpdate;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    function CheckEmpty(vRow: Integer): Boolean;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    function PosToCoord(x, y: Integer; out vRow: Integer; out vCol: Integer; out vArea: TntvGridArea; vCorrect: Boolean = True): Boolean;
    function FindRowNext(vStartRow: Integer; vInCol: Integer; vText: String; vPartial: Boolean = True): Integer;
    procedure ClipboardCopy(Selected: Boolean);
    procedure ClipboardPaste;
    procedure CheckData; virtual;
    procedure CorrectRow(vRow: Integer); virtual;
    procedure DecCurCol(Selecting: Boolean = False);
    procedure DecCurRow(Selecting: Boolean = False);
    function OpenEdit(OpenBy: TntvGridOpenEdit; Key: Char = #0): Boolean;
    function CancelEdit: Boolean;
    procedure CloseEdit;
    function Editing: Boolean;
    procedure AfterEdit(AColumn: TntvColumn; vRow: Integer);
    procedure AfterPaste(AColumn: TntvColumn; vRow: Integer); virtual;
    procedure AfterPasteRow(vRow: Integer); virtual;
    procedure MoveCurrentRowCol(var vRow, vCol: Integer);
    procedure MoveCurrent;
    function CanMoveCurrent: Boolean;
    function GetGridText(SpecialFormat: Boolean; vSelected: Boolean = False): String;
    function Search(vRow: Integer; vText: String): Boolean; virtual;
    function IncCurCol(Selecting: Boolean = False): Boolean;
    function IncCurRow(Selecting: Boolean = False): Boolean;
    procedure DeleteRow(vRow: Integer); virtual;
    procedure InsertRow(vRow: Integer); virtual;
    procedure InsertRows(vRow, vCount: Integer); virtual;

    function IsValidCell(vRow: Integer; vCol: Integer): Boolean;

    function InvalidateCell(vRow: Integer; vCol: Integer): Boolean;
    procedure InvalidateRow(vRow: Integer);
    procedure RefreshFooter;
    procedure RefreshHeader;
    procedure RefreshCell(vRow: Integer; vCol: Integer); overload;
    procedure RefreshCell(vRow: Integer; vCol: Integer; Area: TntvGridArea); overload;
    procedure RefreshCol(vCol: Integer);
    procedure RefreshCurrent;
    procedure RefreshRow(vRow: Integer);
    procedure DublicateRow(vRow: Integer); virtual;
    procedure ResetPosition;
    procedure CheckPosition;
    procedure SetCurCell(vRow: Integer; vCol: Integer; Selecting, CtrlSelecting: Boolean);
    procedure PasteText(vText: String; SpecialFormat: Boolean);
    procedure ShowCol(vCol: Integer);
    procedure ShowRow(vRow: Integer);
    procedure SortByColumn(Column: TntvColumn; SortDown: Boolean = False);
    function FindText(Column: TntvColumn; From: Integer; S: String): Boolean;
    property Columns: TntvColumns read FColumns;
    property VisibleColumns: TColumnList read FVisibleColumns;
    property CurrentColumn: TntvColumn read GetCurrentColumn;
    property Locked: Boolean read GetLock write SetLock;
    property Modified: Boolean read FModified write SetModified;
    property CurCell: TntvCell read GetCurCell;
    property TopRow: Integer read FTopRow write SetTopRow;
    property Rows: TntvRows read FRows;
    property ActiveRow: Integer read FActiveRow write SetActiveRow;
    //for published
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property RowRefresh: Boolean read FRowRefresh write FRowRefresh default False;
    property ChaseCell: Boolean read FChaseCell write SetChaseCell default False;
    property CurCol: Integer read FCurCol write SetCurCol stored False;
    property CurRow: Integer read FCurRow write SetCurRow stored False;
    property DesignColumns: Boolean read FDesignColumns write FDesignColumns default True;
    property DragAfterMode: TntvDragAfterMode read FDragAfterMode write FDragAfterMode default damNone;
    property DualColor: Boolean read FDualColor write SetDualColor default True;
    property Count: Integer read GetCount write SetCount stored False default 0;
    property FixedCols: Integer read FFixedCols write SetFixedCols default 0;
    property FixedColor: TColor read FFixedColor write SetFixedColor default clBtnFace;
    property FollowDrag: Boolean read FFollowDrag write FFollowDrag default False;

    property GridLines: TntvGridLines read FGridLines write SeTntvGridLines default glBoth;
    property LinesColor: TColor read FLinesColor write SetLinesColor default $00DDDDDD;
    property Footer: Boolean read FFooter write SetFooter default False;
    property Header: Boolean read FHeader write SetHeader default True;
    property Indicator: Boolean read FIndicator write SetIndicator default True;
    property Fringe: Boolean read FFringe write SetFringe default False;
    property IndicatorWidth: Integer read FIndicatorWidth write SetIndicatorWidth default sIndicatorWidth;
    property FringeWidth: Integer read FFringeWidth write SetFringeWidth default sFringeWidth;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default True;
    property Color default clBtnFace;
    property OddColor: TColor read FOddColor write SetOddColor default $00F8F8F8;
    property EvenColor: TColor read FEvenColor write SetEvenColor default clWhite;
    property SettledColor: TColor read FSettledColor write SetSettledColor default $00EAEAEA;
    property SettledCols: Integer read FSettledCols write SetSettledCols default 0;
    property ReturnColumns: Integer read FReturnColumns write FReturnColumns default 0;
    property VerticalJump: Boolean read FVerticalJump write FVerticalJump default False;
    property OnAfterEdit: TNotifyEvent read FOnAfterEdit write FOnAfterEdit;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property OnCellClick: TOnNotifyCell read FOnCellClick write FOnCellClick;
    property OnButtonClick: TOnNotifyButton read FOnButtonClick write FOnButtonClick;
    property OnRowClick: TOnNotifyRow read FOnRowClick write FOnRowClick;
    property OnValueChanged: TOnValueChanged read FOnValueChanged write FOnValueChanged;
    property OnCurChanged: TOnNotifyCell read FOnCurChanged write FOnCurChanged;
    property OnCurRowChanged: TOnNotifyRow read FOnCurRowChanged write FOnCurRowChanged;
    property OnCountChanged: TNotifyEvent read FOnCountChanged write FOnCountChanged;
    property OnModified: TNotifyEvent read FOnModified write FOnModified;
    property OnPopupMenu: TNotifyEvent read FOnPopupMenu write FOnPopupMenu;
    property ReadOnly: Boolean read GetReadOnly write FReadOnly default False;
    property OnIsReadOnly: TOnIsReadOnly read FOnIsReadOnly write FOnIsReadOnly;
    property RowNumbers: Boolean read FRowNumbers write SetRowNumbers default True;
    property Capacity: Integer read GetCapacity write SetCapacity default 5;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default ssBoth;
    property Selected: TGridSelected read FSelected write FSelected;
    property RowSelect: Boolean read FRowSelect write SetRowSelect default False;
    property SideCol: Integer read FSideCol write SetSideCol stored False;
    property Solid: Boolean read FSolid write FSolid default False;
    property ImageList: TImageList read FImageList write SetImageList;
    property OnGetColor: TOnGetColor read FOnGetColor write FOnGetColor;
    property FullHeader: Boolean read FFullHeader write SetFullHeader default False;
    property WantTab: Boolean read FWantTab write FWantTab default False;
    property WantReturn: Boolean read FWantReturn write FWantReturn default False;
    property Font;

    property Items[Index: Integer]: TntvRow read GetItems;
    property Values[Row, Col: Integer]: string read GetValues write SetValues; default;
  end;

  TntvGrid = class(TntvCustomGrid)
  public
  published
    property Align;
    property Anchors;
    property RowRefresh;
    property ParentBidiMode;
    property BorderStyle;
    property BidiMode;
    property ChaseCell;
    property Color;
    property ColWidth;
    property CurCol;
    property CurRow;
    property Caption;
    property DesignColumns;
    property DragAfterMode;
    property DragCursor;
    property DragMode;
    property DualColor;
    property Count;
    property Enabled;
    property EvenColor;
    property FixedCols;
    property FixedColor;
    property SettledCols;
    property FollowDrag;
    property Font;
    property GridLines;
    property Footer;
    property Header;
    property Indicator;
    property Fringe;
    property IndicatorWidth;
    property FringeWidth;
    property Height;
    property Hint;
    property Left;
    property MultiSelect;
    property Solid;
    property OddColor;
    property SettledColor;
    property ReturnColumns;
    property VerticalJump;
    property OnIsReadOnly;
    property OnGetColor;
    property OnAfterEdit;
    property OnChanged;
    property OnClick;
    property OnCellClick;
    property OnButtonClick;
    property OnValueChanged;
    property OnRowClick;
    property OnCurChanged;
    property OnCurRowChanged;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnCountChanged;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnModified;
    property OnPopupMenu;
    property ParentFont;
    property PopupMenu;
    property ReadOnly;
    property RowHeight;
    property RowNumbers;
    property Capacity;
    property ScrollBars;
    property Selected;
    property RowSelect;
    property ShowHint;
    property SideCol;
    property TabOrder;
    property TabStop default True;
    property Tag;
    property Top;
    property Visible;
    property Width;
    property ImageList;
    property FullHeader;
    property WantTab;
  end;

  { TntvEdit }

  TntvEdit = class(TEdit)
  private
    Master: IMaster;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); Message WM_GetDlgCode;
    procedure WMKillFocus(var Message: TWMKillFocus); Message WM_KILLFOCUS;
  protected
    procedure DoExit; override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TntvStandardColumn = class(TntvColumn)
  protected
    function Edit: TntvEdit;
    function CreateEdit: TControl; override;
    procedure HideEdit; override;
    procedure SendChar(vChar: Char); override;
    procedure SelectAll; override;
    procedure SelectPos(X: Integer); override;
    procedure ShowEdit(vChar: Char); override;
    function GetEditText: String; override;
    procedure SetEditText(const Value: String); override;
  public
    function IsFocused(Handle: THandle = 0): Boolean; override;
  end;

  TntvCheckColumn = class(TntvColumn)
  private
  protected
    procedure Toggle;
    function CanEdit: Boolean; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DrawCell(Canvas: TCanvas; vRow: Integer; vRect: TRect; State: TntvCellDrawState; const vColor: TColor); override;
  public
  end;

  TntvImageColumn = class(TntvColumn)
  private
  protected
    function CanEdit: Boolean; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DrawCell(Canvas: TCanvas; vRow: Integer; vRect: TRect; State: TntvCellDrawState; const vColor: TColor); override;
  public
  end;

const
  sSetCellFull: TntvSetCells = [low(TntvSetCell)..high(TntvSetCell)] - [swcCheckInfo];

var
  CF_NativeGRID: Word;

function KeyDownToKeyAction(var KEY: Word; Shift: TShiftState): TntvKeyAction;
function SpliteStr(var vText: String; const vStr: String; vPart: Integer; vComma: Char; vQuoted: Char = '"'): Boolean;
function GetPartStr(const vStr: String; vComma: Char; vPart: Integer; vQuoted: Char = '"'): String;
function GetPartValue(const Str: String; const Default: String = ''; vComma: Char = '='): String;

implementation

uses
  StrUtils, Clipbrd, Types, ExtCtrls;

const
  MAX_SCROLL = 32767;

var
  FCellNodeCount: Integer = 0;

function PixelsPerPoint: Integer;
begin
  Result := Screen.PixelsPerInch div 72;
end;

function PointsToPixels(Points: Integer): Integer;
begin
  Result := Screen.PixelsPerInch * Points div 72;
end;

function GetPartValue(const Str: String; const Default: String; vComma: Char): String;
var
  P: Integer;
begin
  P := AnsiPos(vComma, Str);
  if P <> 0 then
    Result := Copy(Str, P + 1, MaxInt)
  else
    Result := Default;
end;

function SpliteText(var vBeginPos, vCount: Integer; const vStr: String; vPart: Integer; vComma: Char; vQuoted: Char = '"'): Boolean;
var
  j, i, LastPos, l: Integer;
  InQuoted: Boolean;
begin
  i := 0;
  j := 0;
  LastPos := 1;
  l := length(vStr);
  InQuoted := False;
  while i < l do
  begin
    i := i + 1;
    if (vStr[i] = vComma) and (not InQuoted) then
    begin
      if j >= vPart then
      begin
        i := i - 1;
        break;
      end;
      j := j + 1;
      LastPos := i + 1;
    end
    else if (vQuoted <> #0) and (vStr[i] = vQuoted) then
      InQuoted := not InQuoted;
  end;
  Result := j = vPart;
  if Result then
  begin
    vBeginPos := LastPos;
    vCount := i - LastPos + 1;
  end
  else
  begin
    vBeginPos := 0;
    vCount := 0;
  end;
end;

function SpliteStr(var vText: String; const vStr: String; vPart: Integer; vComma: Char; vQuoted: Char): Boolean;
var
  b, c: Integer;
begin
  Result := SpliteText(b, c, vStr, vPart, vComma, vQuoted);
  if Result then
    vText := Copy(vStr, b, c)
  else
    vText := '';
end;

function GetPartStr(const vStr: String; vComma: Char; vPart: Integer; vQuoted: Char): String;
begin
  SpliteStr(Result, vStr, vPart, vComma, vQuoted);
end;

function KeyDownToKeyAction(var KEY: Word; Shift: TShiftState): TntvKeyAction;
begin
  Result := keyaNone;
  if Shift = [] then
    case Key of
      VK_Tab: Result := keyaTab;
      VK_Return: Result := keyaReturn;
      VK_Escape: Result := keyaEscape;
      VK_Left: Result := keyaLeft;
      VK_Right: Result := keyaRight;
      VK_Up: Result := keyaUp;
      VK_Down: Result := keyaDown;
      VK_Prior: Result := keyaPageUp;
      VK_Next: Result := keyaPageDown;
      VK_Insert: Result := keyaInsert;
      VK_Delete: Result := keyaDelete;
      VK_F2: Result := keyaEdit;
      VK_F3: Result := keyaFindNext;
      VK_Home: Result := keyaHome;
      VK_End: Result := keyaEnd;
      VK_F5: Result := keyaFunction;
      VK_F4: Result := keyaBrowse;
    end
  else if Shift = [ssCtrl] then
    case Key of
      VK_Return: Result := keyaFit;
      VK_Escape: Result := keyaNone;
      VK_Left: Result := keyaWordLeft;
      VK_Right: Result := keyaWordRight;
      VK_Up: Result := keyaScrollUp;
      VK_Down: Result := keyaScrollDown;
      VK_Prior: Result := keyaTopPage;
      VK_Next: Result := keyaBottomPage;
      VK_Delete, Ord('X'): Result := keyaCut;
      VK_Insert, Ord('C'): Result := keyaCopy;
      Ord('V'): Result := keyaPaste;
      Ord('G'): Result := keyaGoTo;
      VK_F2: Result := keyaSave;
      VK_F3: Result := keyaFindFirst;
      VK_Home: Result := keyaTop;
      VK_End: Result := keyaBottom;
      VK_Space: Result := keyaLoComplete;
      VK_F5: Result := keyaViewer;
      VK_F12: Result := keyaExecute;
      Ord('D'), 222: Result := keyaRepeat;
    end
  else if Shift = [ssShift] then
    case Key of
      VK_Return: Result := keyaReturn;
      VK_Escape: Result := keyaEscape;
      VK_Left: Result := keyaSelectLeft;
      VK_Right: Result := keyaSelectRight;
      VK_Up: Result := keyaSelectUp;
      VK_Down: Result := keyaSelectDown;
      VK_Prior: Result := keyaSelectPageUp;
      VK_Next: Result := keyaSelectPageDown;
      VK_Insert: Result := keyaPaste;
      VK_Delete: Result := keyaCut;
      VK_F2: Result := keyaSaveAll;
      VK_F3: Result := keyaFindPrior;
      VK_Home: Result := keyaSelectHome;
      VK_End: Result := keyaSelectEnd;
      VK_Space: Result := keyaHiComplete;
    end
  else if Shift = [ssShift, ssCtrl] then
    case Key of
      VK_Home: Result := keyaSelectTop;
      VK_END: Result := keyaSelectBottom;
      VK_Space: Result := keyaComplete;
    end
  else if Shift = [ssAlt] then
    case Key of
      VK_Return: Result := keyaProperty;
      VK_Escape: Result := keyaNone;
      VK_Left: Result := keyaBack;
      VK_Right: Result := keyaForward;
      VK_Up: Result := keyaNone;
      VK_Down: Result := keyaNone;
      VK_Prior: Result := keyaNone;
      VK_Next: Result := keyaNone;
      VK_Insert: Result := keyaInsertLine;
      VK_Delete: Result := keyaDeleteLine;
      VK_F2: Result := keyaNone;
      VK_F3: Result := keyaNone;
      VK_Home: Result := keyaNone;
      VK_End: Result := keyaNone;
    end;
end;

procedure TntvList<_Object_>.InternalQuickSort(L, R: Integer; SCompare: TOnCompareLine);
var
  I, J: Integer;
  P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while (SCompare(I, P) < 0) do
        Inc(I);
      while (SCompare(J, P) > 0) do
        Dec(J);
      if (I <= J) then
      begin
        if (SCompare(J, P) <> SCompare(I, P)) then
          Exchange(I, J);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      InternalQuickSort(L, J, SCompare);
    L := I;
  until I >= R;
end;

procedure TntvList<_Object_>.Changed;
begin
end;

procedure TntvList<_Object_>.CountChanged;
begin

end;

function TntvList<_Object_>.SortByRange(vFrom, vTo: Integer; OnCompareLine: TOnCompareLine): Boolean;
begin
  if Count > 0 then
  begin
    InternalQuickSort(vFrom, vTo, OnCompareLine);
    Result := True;
  end
  else
    Result := False;
end;

function TntvList<_Object_>.Sort(OnCompareLine: TOnCompareLine): Boolean;
begin
  Result := SortByRange(0, Count - 1, OnCompareLine);
end;

function TntvList<_Object_>.SortByTree(OnGetParentKey, OnGetKey: TOnGetKey): Boolean;
var
  I, J: Integer;
  H, L, K: Integer;
  aParent: Integer;
begin
  L := 0;
  H := 0;
  K := 0;
  aParent := 0;
  while H < Count - 1 do
  begin
    for I := K to Count - 1 do
    begin
      if aParent = OnGetParentKey(I) then
      begin
        J := I;
        while J > L do
        begin
          Exchange(J - 1, J);
          Dec(J);
        end;
        Inc(L);
      end;
    end;
    aParent := OnGetKey(H);
    K := L;
    Inc(H);
    L := H;
  end;
  Result := True;
end;

constructor TntvCell.Create(ARow: TntvRow);
begin
  inherited Create;
  FRow := ARow;
{$IFOPT D+}
  Inc(FCellNodeCount);
{$ENDIF}
end;

destructor TntvCell.Destroy;
begin
  inherited;
{$IFOPT D+}
  Dec(FCellNodeCount);
{$ENDIF}
end;

procedure TntvCell.Assign(Source: TPersistent);
begin
  if TntvCell(Source) <> nil then
  begin
    FText := TntvCell(Source).FText;
    FData := TntvCell(Source).FData;
  end;
end;

function TntvRow.CreateItem: TntvCell;
begin
  Result := TntvCell.Create(Self);
end;

constructor TntvRow.Create(ARows: TntvRows);
begin
  inherited Create;
  FRows := ARows;
end;

procedure TntvRow.Changed;
begin
  inherited Changed;
end;

destructor TntvRow.Destroy;
begin
  inherited;
end;

procedure TntvRow.Assign(Source: TntvRow);
begin
  FData := TntvRow(Source).FData;
end;

constructor TntvRows.Create(Grid: TntvCustomGrid);
begin
  inherited Create;
  FGrid := Grid;
end;

function TntvRows.CreateItem: TntvRow;
begin
  Result := TntvRow.Create(Self);
end;

procedure TntvRows.Changed;
begin
  inherited Changed;
end;

procedure TntvRows.CountChanged;
begin
  inherited;
  FGrid.CountChanged;
end;

function TntvRows.CheckEmpty(vRow: Integer): Boolean;
var
  aCol: Integer;
  aCell: TntvCell;
begin
  Result := True;
  if (Items[vRow] <> nil) and (Items[vRow].Count <> 0) then
  begin
    for aCol := 0 to Items[vRow].Count - 1 do
    begin
      aCell := TntvCell(Items[vRow].Items[aCol]);
      if aCell <> nil then
      begin
        Result := Result and (aCell.Text = '');
      end;
      if not Result then
        exit;
    end;
  end;
end;

function TntvRows.CopyLine(vFromRow, vToRow: Integer): Boolean;
var
  aCol: Integer;
  aCell: TntvCell;
begin
  Result := True;
  if (Items[vFromRow] <> nil) and (Items[vFromRow].Count <> 0) then
  begin
    for aCol := 0 to Items[vFromRow].Count - 1 do
    begin
      aCell := Items[vFromRow].Items[aCol];
      if aCell <> nil then
      begin
        Items[vFromRow][aCol].Assign(aCell);
      end
      else
      begin
        if (Items[vToRow] <> nil) and (Items[vToRow].Count <> 0) then
          Items[vToRow].Delete(aCol);
      end;
    end;
  end
  else
  begin
    Delete(vFromRow);
  end;
end;

procedure TntvCell.SetText(const S: String);
begin
  if FText <> S then
  begin
    FText := S;
//    FRow.Changed; //now work
  end;
end;

function TntvList<_Object_>.Require(Index: Integer): _Object_;
begin
  if Count <= Index then
  begin
    Count := Index + 1;
    Result := nil;
  end
  else
    Result := inherited Require(Index);
  if Result = nil then
  begin
    Result := CreateItem;
    Put(Index, Result);
  end;
  //Put(Index, Result);//move with create item
end;

function TntvRows.SafeGet(vRow: Integer): TntvRow;
begin
  if (vRow < Count) then
    Result := Items[vRow]
  else
    Result := nil;
end;

function TntvRow.SafeGet(Index: Integer): TntvCell;
begin
  if (Index < Count) then
    Result := TntvCell(inherited Items[Index])
  else
    Result := nil;
end;

function TntvRows.SafeGet(vRow, vCol: Integer): TntvCell;
var
  aRow: TntvRow;
begin
  aRow := SafeGet(vRow);
  if (aRow <> nil) and (vRow <= Count) then
    Result := aRow.SafeGet(vCol)
  else
    Result := nil;
end;

procedure TntvList<_Object_>.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TntvList<_Object_>.EndUpdate;
begin
  if FUpdateCount > 0 then
    Dec(FUpdateCount);
  Update;
end;

procedure TntvList<_Object_>.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if (FUpdateCount = 0) then
    CountChanged;
end;

function TntvList<_Object_>.GetUpdating: Boolean;
begin
  Result := (FUpdateCount > 0);
end;

procedure TntvList<_Object_>.Update;
begin
  if FUpdateCount = 0 then
    Changed;
end;

destructor TntvColumn.Destroy;
begin
  inherited;
end;

procedure TntvColumn.Assign(Source: TPersistent);
begin
  if Source is TntvColumn then
  begin
    Info := TntvColumn(Source).Info;
  end
  else
    inherited Assign(Source);
end;

procedure TntvColumn.Validate;
begin
  Grid.Validate(self);
end;

procedure TntvColumn.ValueChanging;
begin
end;

procedure TntvColumn.InternalLookup(vData: Integer; var vText: String);
begin

end;

procedure TntvColumn.Draw(Canvas: TCanvas; vDrawState: TntvCellDrawState; vRow: Integer; vRect: TRect; vArea: TntvGridArea);
var
  aColor, aTextColor: TColor;
  aSelected, aNewRow: Boolean;
  txtRect: TRect;
begin
  if vArea = garNormal then
  begin
    GetCurrentColor(vRow, aColor); //zaher the Row
  end;
  aSelected := Grid.IsSelected(vRow, VisibleIndex);
  aNewRow := (vRow = Grid.Count);
  if aSelected then
  begin
    vDrawState := vDrawState + [csdSelected];
    if not Grid.Focused and (Grid.RowSelect or Grid.IsCurrent(vRow, VisibleIndex)) then
    begin
      aColor := MixColors(aColor, Grid.Selected.Color, 150);
      if aNewRow then
        aTextColor := Lighten(Grid.Font.Color, 150)
      else
        aTextColor := Grid.Font.Color;
    end
    else
    begin
      aColor := Grid.Selected.Color;
      if aNewRow then
        aTextColor := MixColors(Grid.Selected.TextColor, Grid.Selected.Color, 150)
      else
        aTextColor := Grid.Selected.TextColor;
    end;
  end
  else
  begin
    if aNewRow then
      aTextColor := clBtnFace
    else
      aTextColor := Grid.Font.Color;
  end;
  if (VisibleIndex < Grid.FixedCols) or (vArea = garHeader) or (vArea = garFooter) then
  begin
    vDrawState := vDrawState + [csdFixed];
    if Grid.ChaseCell and ((Grid.CurCol = VisibleIndex) and (vArea = garHeader)) then
      aColor := MixColors(clBtnFace, Grid.Selected.Color, 230)
    else
      aColor := clBtnFace;
    aTextColor := clBtnText;
  end;

  if not aSelected and (VisibleIndex < Grid.SettledCols) and (Grid.SettledColor <> clNone) then
  begin
    aColor := MixColors(aColor, Grid.SettledColor, 200);
  end;
  if (vRow = Grid.FClkRow) and (VisibleIndex = Grid.FClkCol) and (Grid.FStateBtn) and (Grid.FClkArea = vArea) then
    vDrawState := vDrawState + [csdDown];
  if (VisibleIndex = Grid.FCurCol) and (vRow = Grid.FCurRow) and (Grid.Focused or (csDesigning in Grid.ComponentState)) then
    vDrawState := vDrawState + [csdCurrent];
  Canvas.Font.Color := aTextColor;
  txtRect := vRect;
  Canvas.Brush.Color := aColor;
  CorrectCellRect(txtRect);
  case vArea of
    garNormal:
    begin
      if aNewRow then
        DrawHint(Canvas, vRow, txtRect, vDrawState, aColor)
      else
      if (vRow < Grid.Count) then
        DrawCell(Canvas, vRow, txtRect, vDrawState, aColor)
      else
        Canvas.FillRect(txtRect);
    end;
    garHeader: DrawHeader(Canvas, vRow, txtRect, vDrawState);
    garFooter: DrawFooter(Canvas, vRow, txtRect, vDrawState);
  end;
  if (csdFixed in vDrawState) then
  begin
    Grid.DrawFixed(Canvas, vRect, '', vDrawState);
  end
  else
  begin
    if (Grid.FGridLines in [glHorizontal, glBoth]) then
    begin
      Canvas.Pen.Color := Grid.LinesColor;
      if Grid.UseRightToLeftAlignment then
      begin
        Canvas.MoveTo(vRect.Left, vRect.Bottom - 1);
        Canvas.LineTo(vRect.Right, vRect.Bottom - 1);
      end
      else
      begin
        Canvas.MoveTo(vRect.Left, vRect.Bottom - 1);
        Canvas.LineTo(vRect.Right, vRect.Bottom - 1);
      end;
    end;
    if (Grid.FGridLines in [glVertical, glBoth]) then
    begin
      Canvas.Pen.Color := Grid.LinesColor;
      if Grid.UseRightToLeftAlignment then
      begin
        Canvas.MoveTo(vRect.Left, vRect.Top);
        Canvas.LineTo(vRect.Left, vRect.Bottom);
      end
      else
      begin
        Canvas.MoveTo(vRect.Right - 1, vRect.Top);
        Canvas.LineTo(vRect.Right - 1, vRect.Bottom);
      end;
    end;
  end;
end;

procedure TntvColumn.DrawCell(Canvas: TCanvas; vRow: Integer; vRect: TRect; State: TntvCellDrawState; const vColor: TColor);
var
  aCell: TntvCell;
  y: Integer;
  aImage: Integer;
  aImageRect: TRect;
begin
  aCell := GetCell(vRow);
  if (ImageList <> nil) and (ShowImage) then
  begin
    if UseRightToLeftAlignment then
    begin
      aImageRect := Rect(vRect.Right - ImageList.Width, vRect.Top, vRect.Right, vRect.Bottom);
      vRect.Right := vRect.Right - ImageList.Width;
    end
    else
    begin
      aImageRect := Rect(vRect.Left, vRect.Top, vRect.Left + ImageList.Width, vRect.Bottom);
      vRect.Left := vRect.Left + ImageList.Width;
    end;
    Canvas.Brush.Color := Grid.Color;
    Canvas.FillRect(aImageRect);
  end;
  Canvas.Brush.Color := vColor;
  Canvas.FillRect(vRect);
  if aCell <> nil then
  begin
    if (ImageList <> nil) and (ShowImage) then
    begin
      aImage := ImageIndex;
      if aImage < 0 then
        aImage := aCell.Data;
      if aImage >= 0 then
      begin
        y := (aImageRect.Bottom - aImageRect.Top) div 2 - ImageList.Height div 2;
        ImageList.Draw(Canvas, aImageRect.Left, aImageRect.Top + y, aImage);
      end;
    end;
    InflateRect(vRect, - sCellMargin, - sCellMargin);
    Canvas.Font.Color :=clBlack;
    Grid.DrawString(Canvas, aCell.Text, vRect, GetTextStyle, True);
  end;
end;

procedure TntvColumn.DrawFooter(Canvas: TCanvas; vRow: Integer; vRect: TRect; State: TntvCellDrawState);
var
  aText: String;
begin
  Canvas.Brush.Color := Grid.FixedColor;
  Canvas.FillRect(vRect);
  if IsTotal then
  begin
    aText := CurrToStr(Total);
    InflateRect(vRect, -1, -1);
    Grid.DrawString(Canvas, aText, vRect, GetTextStyle, True);
  end;
end;

procedure TntvColumn.DrawHeader(Canvas: TCanvas; vRow: Integer; vRect: TRect; State: TntvCellDrawState);
var
  aText: String;
  aLeft: Integer;
begin
  aText := GetCaption;
  if FShowButton then
  begin
    FBtnRect := GetButtonRect(vRect);
    if UseRightToLeftAlignment then
    begin
      aLeft := FBtnRect.Right;
      vRect.Left := FBtnRect.Right;
    end
    else
    begin
      aLeft := FBtnRect.Left;
      vRect.Right := FBtnRect.Left;
    end;
    Grid.DrawString(Canvas, aText, vRect, GetTextStyle(True), True);
    if FDown then
    begin
      Frame3D(Canvas, FBtnRect, clGray, clSilver, 1);
    end
    else
    if FMouseInHeader then
    begin
      Canvas.Pen.Color := clBtnShadow;
      Canvas.MoveTo(aLeft, FBtnRect.Top);
      Canvas.LineTo(aleft, FBtnRect.Bottom - 1);
      Canvas.Pen.Color := $00EEEEEE;
      Canvas.MoveTo(aLeft, FBtnRect.Bottom - 1);
      Canvas.LineTo(aLeft, FBtnRect.Bottom - 2);

      Canvas.MoveTo(aLeft + 1, FBtnRect.Bottom - 2);
      Canvas.LineTo(aLeft + 1, FBtnRect.Top);
    end;
    //DrawSign(Canvas, FBtnRect, FSign, True, True);
  end
  else
    Grid.DrawString(Canvas, aText, vRect, GetTextStyle(True), True);
end;

function TntvColumn.CloseEdit(Accept: Boolean): Boolean;
begin
  if FEditControl = nil then
    raise Exception.Create('Column.EditControl is null');

  HideEdit;
  try
    if Accept then
    begin
      SetInfo(ActiveRow, EditText, EditData, sSetCellFull);
    end;
  finally
    Grid.AfterEdit(Self, ActiveRow);
    FreeEdit; //moved after Grid.AfterEdit for Bug and WMKeyDown not completed
    Result := True;
  end;
end;

function TntvColumn.OpenEdit(OpenBy: TntvGridOpenEdit; Key: Char): Boolean;
begin
  if GetReadOnly then
    Result := False
  else
  begin
    if CanEdit then
    begin
      Grid.ColumnEdit := Self;
      NeedEdit;
      if FEditControl <> nil then
      begin
        EditData := Data;
        case OpenBy of
          goeReturn:
          begin
            EditText := AsString;
            ShowEdit(#0);
            SelectAll;
          end;
          goeChar:
          begin
            EditText := '';
            ShowEdit(Key);
            SendChar(Key);
          end;
          goeMouse:
          begin
            EditText := AsString;
            ShowEdit;
            //SelectPos(Param);
          end;
          else
          begin
            EditText := AsString;
            ShowEdit(#0);
          end;
        end;
        Result := True;
      end
      else
      begin
        Result := False;
        Grid.ColumnEdit := nil;
      end;
    end
    else
      Result := False;
  end;
end;

function TntvColumn.GetAsCurrency: Currency;
begin
  Result := StrToCurr(AsString);
end;

function TntvColumn.GetAsDate: TDateTime;
var
  s: String;
begin
  s := AsString;
  if (s = '') and EmptyZero then
    Result := 0
  else
    Result := StrToDate(s);
end;

function TntvColumn.GetAsInteger: Integer;
begin
  Result := StrToIntDef(AsString, 0);
end;

function TntvColumn.GetAsTime: TDateTime;
var
  s: String;
begin
  s := AsString;
  if (s = '') and EmptyZero then
    Result := 0
  else
    Result := StrToTime(AsString);
end;

function TntvColumn.GetAsTotal: Currency;
begin
  Result := Total;
end;

function TntvColumn.GetCaption: String;
begin
  if Info.Caption <> '' then
    Result := Info.Caption
  else
    Result := Info.Title;
end;

function TntvColumn.GetActiveData: Integer;
begin
  Result := GetData(ActiveRow);
end;

function TntvColumn.GetDisplayName: String;
begin
  Result := Name;
end;

function TntvColumn.GetCell(vRow: Integer): TntvCell;
begin
  with Grid do
    if Store then
    begin
      Result := FRows.SafeGet(vRow, StoreCol);
    end
    else
      Result := nil;
end;

function TntvColumn.GetAsString: String;
var
  aCell: TntvCell;
begin
  aCell := GetCell(ActiveRow);
  if aCell <> nil then
    Result := aCell.Text
  else
    Result := '';
end;

function TntvColumn.GetTextStyle(vCentered: Boolean): TTextStyle;
begin
  Result := Grid.GetTextStyle(vCentered);
  Result.RightToLeft := UseRightToLeftReading;
end;

procedure TntvColumn.SetAlignment(AValue: TAlignment);
begin
  if FAlignment =AValue then Exit;
  FAlignment :=AValue;
end;

function TntvColumn.Grid: TntvCustomGrid;
begin
  Result := FColumns.FGrid;
end;

procedure TntvColumn.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TntvColumn.Complete;
begin
  CompleteCell(True, True);
end;

procedure TntvColumn.CompleteCell(vComplete, vForce: Boolean);
begin
  if vComplete and (vForce or not Grid.Locked) then
  begin
    Grid.Locked := True;
    try
      Grid.ValueChanging(Self);
      ValueChanging;
      ValueChanged;
      Grid.ValueChanged(Self);
    finally
      Grid.Locked := False;
    end;
  end;
end;

procedure TntvColumn.InternalSetInfo(vRow: Integer; vText: String; vData: Integer; vSetCell: TntvSetCells; vForce: Boolean);
var
  aCell: TntvCell;
begin
  if not Grid.Locked then
    Grid.Modified := True;
  Grid.ActiveRow := vRow;
  if (swcCheckInfo in vSetCell) then
    CheckInfo(vText, vData);
  ValidateInfo(vText, vData);
  if not Grid.LockAutoSum and IsTotal then
  begin
    aCell := GetCell(vRow);
    if aCell <> nil then
      Info.Total := Info.Total - StrToCurr(aCell.Text);
  end;
  try
    if Store then
    begin
      with Grid do
      begin
        aCell := FRows[vRow][StoreCol];
        FOldData := aCell.Data;
        if swcText in vSetCell then
          aCell.Text := vText;
        if swcData in vSetCell then
          aCell.Data := vData;
      end;
    end;
    if not Grid.LockAutoSum and IsTotal then
      Info.Total := Info.Total + StrToCurr(vText);
    if not (swcCheckInfo in vSetCell) then
      Validate;

    CompleteCell((swcComplete in vSetCell), vForce);
  finally
    FOldData := 0;
  end;
  if (swcRefresh in vSetCell) then
  begin
    if (not Grid.Rows.Updating) then
    begin
      if Grid.RowRefresh then
      begin
        if not Grid.Locked then
          RefreshRow(vRow);
      end
      else
        RefreshCell(vRow);
      Grid.RefreshFooter;
    end;
  end;
  if not Grid.Locked then
    Grid.Rows[vRow].Modified := True;
end;

procedure TntvColumn.SetValue(const vText: String; vData: Integer);
begin
  InternalSetInfo(ActiveRow, vText, vData, [swcText, swcData, swcRefresh, swcComplete]);
end;

procedure TntvColumn.RefreshCell(vRow: Integer);
begin
  Grid.RefreshCell(vRow, VisibleIndex);
end;

procedure TntvColumn.SetAsCurrency(const Value: Currency);
begin
  if (Value = 0) and EmptyZero then
    AsString := ''
  else
  if Value = 0 then
    AsString := '0'
  else
    AsString := CurrToStr(Value);
  if IsTotal and Grid.LockAutoSum then
    Info.Total := Info.Total + Value;
end;

procedure TntvColumn.SetAsDate(const Value: TDateTime);
begin
  if Value <> 0 then
    AsString := DateToStr(Value)
  else
    AsString := '';
end;

procedure TntvColumn.SetAsInteger(const Value: Integer);
begin
  if (Value = 0) and EmptyZero then
    AsString := ''
  else
    AsString := IntToStr(Value);
end;

procedure TntvColumn.SetAsString(const Value: String);
begin
  InternalSetInfo(ActiveRow, Value, 0, [swcText, swcRefresh, swcComplete]);
end;

procedure TntvColumn.SetAsTime(Value: TDateTime);
begin
  if Value <> 0 then
    AsString := TimeToStr(Value)
  else
    AsString := '';
end;

procedure TntvColumn.SetAsTotal(const Value: Currency);
begin
  Total := Value;
  CompleteCell(True, False);
end;

procedure TntvColumn.SetData(const Value: Integer);
begin
  InternalSetInfo(ActiveRow, '', Value, [swcData, swcRefresh]);
end;

procedure TntvColumn.Lookup(const vData: Integer; vForce: Boolean);
var
  aText: String;
begin
  InternalLookup(vData, aText);
  InternalSetInfo(ActiveRow, aText, vData, sSetCellFull, vForce);
end;

procedure TntvColumn.Zero(vRow: Integer);
begin
  if Grid.Locked then
    Info.Total := Info.Total - AsCurrency;
  AsString := '';
end;

destructor TntvColumns.Destroy;
begin
  inherited;
end;

procedure TntvColumns.ClearTotals;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].Total := 0;
  end;
end;

function TntvColumns.GetItems(Index: Integer): TntvColumn;
begin
  Result := TntvColumn(inherited Items[Index]);
end;

function TntvColumns.Search(vName: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
  begin
    if Items[i].Name = vName then
    begin
      Result := i;
      Break;
    end;
  end;
end;

procedure TntvColumns.SetItems(Index: Integer; ColNode: TntvColumn);
begin
  inherited Items[Index] := ColNode;
end;

procedure TntvColumns.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if (FGrid <> nil) then
    FGrid.ColsChanged;
end;

constructor TntvColumns.Create(vGrid: TntvCustomGrid);
begin
  inherited Create;
  FGrid := vGrid;
end;

constructor TntvCustomGrid.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := [csDoubleClicks, csCaptureMouse, csClickEvents, csSetCaption, csReflector];
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FBorderStyle := bsSingle;
  FRows := CreateRows;
  FVisibleColumns := TColumnList.Create;
  FSelected := TGridSelected.Create(Self);
  FColumns := TntvColumns.Create(Self);
  FDesignColumns := True;
  FScrollBars := ssBoth;
  FCurCol := 0;
  FCurRow := 0;
  FOldRow := 0;
  FOldX := 0;
  FOldY := 0;
  FCapacity := 5;
  MultiSelect := True;
  Height := 130;
  Width := 260;
  FRowNumbers := True;
  Color := clBtnFace;
  TabStop := True;
  FOddColor := clWhite;
  FEvenColor := clWhite;
  FSettledColor := $00EAEAEA;
  FFixedCols := 0;
  FTopRow := 0;
  FSideCol := 0;
  FDragAfter := 0;
  FGridLines := glBoth;
  FLinesColor := $00DDDDDD;
  FColWidth := sColWidth;
  FStateBtn := False;
  FModified := False;
  FRow := 0;
  FIndicator := True;
  FDualColor := True;
  FOddColor := $00F8F8F8;
  FHeader := True;
  FIndicatorWidth := sIndicatorWidth;
  FFringeWidth := sFringeWidth;
  ShouldCurChange := False;
  FDownCol := -1;
  FUnderMouseCol := -1;
  FFixedColor := clBtnFace;
  FRowHeight := GetDefaultRowHeight;
end;

destructor TntvCustomGrid.Destroy;
begin
  Columns.Free;
  Selected.Free;
  FreeAndNil(FVisibleColumns);
  FreeAndNil(FRows);
  FImageChangeLink.Free;
  inherited;
end;

procedure TntvCustomGrid.AddTotals(vRow: Integer);
var
  i: Integer;
  aColGrid: TntvColumn;
  aCell: TntvCell;
begin
  for i := 0 to Columns.Count - 1 do
  begin
    aColGrid := Columns[i];
    if (aColGrid <> nil) and (aColGrid.IsTotal) then
    begin
      aCell := aColGrid.GetCell(vRow);
      if aCell <> nil then
        aColGrid.Total := aColGrid.Total + StrToCurr(aCell.Text);
    end;
  end;
end;

procedure TntvCustomGrid.Validate(AColumn: TntvColumn);
begin
end;

procedure TntvCustomGrid.BeginUpdate;
begin
  Rows.BeginUpdate;
  Locked := True;
end;

procedure TntvCustomGrid.CalcNewWidth(X: Smallint);
begin
  if UseRightToLeftAlignment then
    X := FVisibleColumns[FClkCol].Width - X
  else
    X := FVisibleColumns[FClkCol].Width + X;
  if X < 10 then
    X := 10;
  FVisibleColumns[FClkCol].Width := X;
end;

procedure TntvCustomGrid.BeginCapture(X, Y: Integer);
begin
  case FState of
    dgsDown:
    begin
    end;
    dgsDrag:
    begin
      FOldX := X;
      FOldY := Y;
    end;
    dgsResizeCol:
    begin
      FOldX := X;
      DrawColSizeLine(X);
    end;
  end;
end;

function TntvCustomGrid.IsSelected(vRow: Integer; vCol: Integer): Boolean;
begin
  if MultiSelect and not ((FSelected.StartRow <= 0) and (FSelected.EndRow <= 0) and (FSelected.FSelectedRows.Count = 0)) then
    Result := FSelected.IsSelected(vRow)
  else
    Result := ((((vCol = FCurCol) or (RowSelect)) and (vRow = FCurRow) and (not Editing)));
end;

procedure TntvCustomGrid.Clear;
begin
  Rows.BeginUpdate;
  try
    Columns.ClearTotals;
    FUnderMouseCol := 0;
    Count := 0;
  finally
    Rows.EndUpdate;
  end;
end;

procedure TntvCustomGrid.Reset;
begin
  Locked := True;
  try
    Clear;
    FModified := False;
  finally
    Locked := False;
  end;
end;

procedure TntvCustomGrid.ClipboardCopy(Selected: Boolean);
var
  S: String;
begin
  S := GetGridText(False, Selected);
  if S <> '' then
  begin
    Clipboard.Clear;
    //Clipboard.AddFormat()    //    SetClipboardText(CF_TEXT, S);

    S := GetGridText(True, Selected);
    //    SetClipboardText(CF_NativeGRID, S);
  end;
end;

function TntvCustomGrid.GetTextRange(vStartRow, vEndRow: Integer; SpecialFormat: Boolean): String;
var
  r: Integer;
  c: Integer;
  aText: String;
  aData: Integer;
  IsC, IsR: Boolean;
  AColumn: TntvColumn;
  aCell: TntvCell;
  aColumns: TColumnList;
  i: Integer;
begin
  Result := '';
  Screen.Cursor := crHourGlass;
  aColumns := TColumnList.Create;
  try
    if SpecialFormat then
    begin
      for i := 0 to Columns.Count - 1 do
        aColumns.Add(Columns[i]);
    end
    else
    begin
      for i := 0 to VisibleColumns.Count - 1 do
        aColumns.Add(VisibleColumns[i]);
    end;
    isR := False;
    if vEndRow >= Count then
      vEndRow := Count - 1;
    if vStartRow < 0 then
      vStartRow := 0;
    for r := vStartRow to vEndRow do
    begin
      if IsR then
      begin
        if SpecialFormat then
          Result := Result + cntv_X_EOL
        else
          Result := Result + cntv_EOL + #10;
      end;
      isR := True;
      IsC := False;
      for c := 0 to aColumns.Count - 1 do
      begin
        if IsC then
        begin
          if SpecialFormat then
            Result := Result + cntv_X_EOC
          else
            Result := Result + cntv_EOC;
        end;
        AColumn := Columns[c];
        aCell := AColumn.GetCell(r);
        if aCell <> nil then
        begin
          aText := aCell.Text;
          aData := aCell.Data;
        end
        else
        begin
          aText := '';
          aData := 0;
        end;
        if SpecialFormat then
        begin
          if aText <> '' then
          begin
            Result := Result + aText + cntv_X_DATA;
            Result := Result + IntToStr(aData) + cntv_X_DATA;
            Result := Result + AColumn.Name;
          end;
        end
        else
          Result := Result + aText;
        IsC := True;
      end;
    end;
  finally
    aColumns.Free;
    Screen.Cursor := crDefault;
  end;
end;

procedure TntvCustomGrid.ClipboardPaste;
var
  aStr: String;
begin
  if not ReadOnly then
  begin
    if Clipboard.HasFormat(CF_NativeGRID) then
    begin
      //      aStr := GetClipboardText(CF_NativeGRID);
      PasteText(aStr, True);
    end
    else
    begin
      aStr := Clipboard.AsText;
      PasteText(aStr, False);
    end;
    Modified := True;
    DoChanged;
  end;
end;

const
  sCheckOrValidate: array[Boolean] of TntvSetCells = ([], [swcCheckInfo]);

procedure TntvCustomGrid.PasteRange(vText: String; SpecialFormat, vCheckData: Boolean);
var
  r: Integer;
  aClip: String;
  ntvEOC: Char;
  ntvEOL: Char;

  procedure LinePaste;
  var
    i, c: Integer;
    S: String;
    AColumn: TntvColumn;
    aColName: String;
    aData: Integer;
    aText: String;
  begin
    c := CurCol;
    i := 0;
    while (SpliteStr(S, aClip, i, ntvEOC, #0)) do
    begin
      if SpecialFormat then
      begin
        aText := GetPartStr(S, cntv_X_DATA, 0, #0);
        aData := StrToIntDef(GetPartStr(S, cntv_X_DATA, 1, #0), 0);
        aColName := GetPartStr(S, cntv_X_DATA, 2, #0);
        if (aColName <> '') then
        begin
          AColumn := VisibleColumns.Find(aColName) as TntvColumn;
          if (AColumn <> nil) and (not AColumn.GetReadOnly) then
          begin
            AColumn.SetInfo(R, aText, aData, [swcText, swcData] + sCheckOrValidate[vCheckData]);
            AfterPaste(AColumn, R);
            //            AfterEdit(AColumn, R);
          end;
        end;
      end
      else
      if c < VisibleColumns.Count then
      begin
        AColumn := VisibleColumns[c];
        if (not AColumn.GetReadOnly) then
        begin
          AColumn.SetInfo(R, S, 0, [swcText, swcData] + sCheckOrValidate[vCheckData]);
          AfterPaste(AColumn, R);
          //        AfterEdit(AColumn, R);
        end;
      end;
      Inc(i);
      Inc(c);
    end;
    AfterPasteRow(R);
  end;

var
  i: Integer;
begin
  Screen.Cursor := crHourGlass;
  BeginUpdate;
  try
    vText := StringReplace(vText, #13#10, #13, [rfReplaceAll]);
    LockAutoSum := True;
    r := CurRow;
    i := 0;
    if SpecialFormat then
    begin
      ntvEOC := cntv_X_EOC;
      ntvEOL := cntv_X_EOL;
    end
    else
    begin
      ntvEOC := cntv_EOC;
      ntvEOL := cntv_EOL;
    end;
    while (r < Capacity) and (SpliteStr(aClip, vText, i, ntvEOL, #0)) do
    begin
      if aClip <> '' then
      begin
        InsertRow(r);
        ActiveRow := r;
        LinePaste;
        AddTotals(r);
        if vCheckData then
          CheckData;
        Inc(r);
      end;
      Inc(i);
    end;
  finally
    EndUpdate;
    Screen.Cursor := crDefault;
  end;
end;

procedure TntvCustomGrid.CMDesignHitTest(var Message: TCMDesignHitTest);
var
  aRow: Integer;
  aCol: Integer;
  aArea: TntvGridArea;
begin
  if (FState in [dgsResizeCol, dgsResizeRow]) or (PosToCoord(Message.XPos, Message.YPos, aRow, aCol, aArea) and (aArea = garHeader) and (InColsWidth(Message.XPos, aCol))) then
  begin
    Message.Result := 1;
  end
  else
    inherited;
end;

procedure TntvCustomGrid.ColsChanged;
var
  m, c, i: Integer;
begin
  if (csLoading in ComponentState) then
    Exit;
  FVisibleColumns.Clear;
  for i := 0 to Columns.Count - 1 do
  begin
    if Columns[i].Visible and Columns[i].Enabled then
    begin
      Columns[i].VisibleIndex := FVisibleColumns.Add(Columns[i]);
    end
    else
      Columns[i].VisibleIndex := -1;
  end;
  c := 0;
  m := -1;
  for i := 0 to Columns.Count - 1 do
    if Columns[i].Store then
    begin
      if Columns[i].StoreCol < 0 then
      begin
        Inc(m);
        Columns[i].StoreCol := m;
      end
      else
      if Columns[i].Info.StoreCol > m then
        m := Columns[i].StoreCol;
      Inc(c);
    end;
  Columns.FCapacity := c;
  UpdateScrollBar;
  InternalInvalidate;
end;

procedure TntvCustomGrid.ColsScroll(vCols: Integer);
var
  c, Sign, aCols, aCol: Integer;
  W, X: Integer;
  vrtRect, ARect: TRect;
begin
  ARect := ClientRect;
  vrtRect := MovingRect;
  vrtRect.Top := ARect.Top;
  vrtRect.Bottom := ARect.Bottom;
  aCols := Abs(vCols);
  Sign := vCols div aCols;
  W := vrtRect.Right - vrtRect.Left;
  if Sign < 0 then
    aCol := FSideCol - 1
  else
    aCol := FSideCol;
  X := 0;
  c := 0;
  while (c < aCols) and (X < W) do
  begin
    X := X + FVisibleColumns[aCol].Width;
    aCol := aCol + Sign;
    c := c + 1;
  end;
  if X > W then
    X := W;
  if UseRightToLeftAlignment then
  begin
    X := -X;
    vrtRect := FlipRect(ClientRect, vrtRect);
  end;
  ScrollWindow(Handle, -X * Sign, 0, nil, @vrtRect);
  //ScrollWindowEx(Handle, -X * Sign, 0, @vrtRect, @vrtRect, 0, nil, SW_INVALIDATE or SW_ERASE);
end;

procedure TntvCustomGrid.ValueChanging(AColumn: TntvColumn);
begin
end;

procedure TntvCustomGrid.CheckData;
begin

end;

procedure TntvCustomGrid.CorrectRow(vRow: Integer);
begin
end;

procedure TntvCustomGrid.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Params.Style := Params.Style or WS_TABSTOP or BorderStyles[FBorderStyle];
    if NewStyleControls and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
    if FScrollBars in [ssHorizontal, ssBoth] then
      Style := Style or WS_HSCROLL;
    if FScrollBars in [ssVertical, ssBoth] then
      Style := Style or WS_VSCROLL;
  end;
end;

procedure TntvCustomGrid.CreateWnd;
begin
  inherited;
  ShowScrolls(FScrollBars);
end;

procedure TntvCustomGrid.CurChanged(vRow: Integer; vCol: Integer);
begin
  if not Locked then
  begin
    if Assigned(FOnCurChanged) then
    begin
      //        Items.ColToId(vCol);
      FOnCurChanged(Self, FVisibleColumns[vCol], vRow);
    end;
  end;
end;

procedure TntvCustomGrid.DecCurCol(Selecting: Boolean = False);
begin
  InternalSetCurCol(FCurCol - 1, Selecting);
end;

procedure TntvCustomGrid.DecCurRow(Selecting: Boolean = False);
begin
  InternalSetCurRow(FCurRow - 1, Selecting);
end;

procedure TntvCustomGrid.DoAfterEdit(AColumn: TntvColumn; vRow: Integer);
begin
  if Assigned(FOnAfterEdit) then
    FOnAfterEdit(self);
end;

procedure TntvCustomGrid.DoBeforeEdit(AColumn: TntvColumn; vRow: Integer);
begin

end;

procedure TntvCustomGrid.DoButtonClick(Column: TntvColumn; X, Y: Integer);
begin
  if Assigned(FOnButtonClick) then
    FOnButtonClick(Self, Column, X, Y);
end;

procedure TntvCustomGrid.DoChanged;
begin
  if Assigned(OnChanged) and not Locked then
    OnChanged(Self);
end;

procedure TntvCustomGrid.DoClickArea(vRow: Integer; vCol: Integer; vArea: TntvGridArea);
begin
  case vArea of
    garNormal:
      if Assigned(FOnCellClick) then
        FOnCellClick(Self, FVisibleColumns[vCol], vRow);
    garIndicator:
      if Assigned(FOnRowClick) then
        FOnRowClick(Self, vRow);
  end;
end;

procedure TntvCustomGrid.DoCurChange(var vRow: Integer; var vCol: Integer);
begin
end;

procedure TntvCustomGrid.DoCurRowChanging(vOldRow, vNewRow: Integer);
begin
end;

procedure TntvCustomGrid.DoCurRowChanged;
begin
  if Assigned(FOnCurRowChanged) then
    FOnCurRowChanged(Self, CurRow);
end;

procedure TntvCustomGrid.DeleteRow(vRow: Integer);
var
  c: Integer;
begin
  if vRow < Count then
  begin
    SubTotals(vRow);
    Rows.Delete(vRow);
    RowDeleted(vRow);
    if Solid then
      Capacity := Count;
  end;
end;

procedure TntvCustomGrid.TryDeleteRow(vRow: Integer);
var
  Accept: Boolean;
begin
  if not ReadOnly and not Solid then
  begin
    Accept := True;
    CanDeleteRow(vRow, Accept);
    if Accept then
      DeleteRow(vRow);
  end;
end;

function TntvCustomGrid.InternalInvalidate: Boolean;
begin
  if (not Rows.Updating) then
  begin
    Invalidate;
    Result := True;
  end
  else
    Result := False;
end;

function TntvCustomGrid.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := True;
  TopRow := TopRow + 3;
end;

function TntvCustomGrid.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := True;
  TopRow := TopRow - 3;
end;

procedure TntvCustomGrid.Draw(Canvas: TCanvas; wndRect, pntRect: TRect);
var
  vrtRect, TmpRect, SavePntRect: TRect;
  aRow: Integer;
  aRect: TRect;
begin
  Canvas.Pen.Color := Color;
  SavePntRect := PntRect;
  vrtRect := RowsClient;
  if Header and (pntRect.Top < vrtRect.Top) then
  begin
    tmpRect := vrtRect;
    tmpRect.Top := 0;
    tmpRect.Bottom := vrtRect.Top;
    DrawRow(Canvas, -1, tmpRect, pntRect, garHeader);
    ExcludeClipRect(Canvas.Handle, TmpRect.Left, tmpRect.Top, tmpRect.Right, tmpRect.Bottom);
    pntRect.Top := vrtRect.Top;
  end;

  if Footer and (pntRect.Bottom > vrtRect.Bottom) then
  begin
    tmpRect := vrtRect;
    tmpRect.Top := vrtRect.Bottom;
    tmpRect.Bottom := ClientRect.Bottom;
    DrawRow(Canvas, -1, tmpRect, pntRect, garFooter);
    ExcludeClipRect(Canvas.Handle, TmpRect.Left, tmpRect.Top, tmpRect.Right, tmpRect.Bottom);
    pntRect.Bottom := vrtRect.Bottom;
  end;

  aRow := GetCompletedRows(pntRect.Top - vrtRect.Top);
  aRect := vrtRect;
  aRect.Bottom := aRect.Top + RowHeight;
  OffsetRect(aRect, 0, aRow * FRowHeight);
  aRow := GetVirtualRow(aRow);
  while (aRect.Top < pntRect.Bottom) and (aRow < Capacity) do
  begin
    DrawRow(Canvas, aRow, aRect, pntRect, garNormal);
    OffsetRect(aRect, 0, FRowHeight);
    Inc(aRow);
  end;
  if (aRect.Top < pntRect.Bottom) then
  begin
    TmpRect := pntRect;
    TmpRect.Top := aRect.Top;
    Canvas.Brush.Color := Color;
    Canvas.FillRect(TmpRect);
  end;
end;

function TntvCustomGrid.FlipRect(const cliRect, vRect: TRect): TRect;
begin
  Result := vRect;
  if UseRightToLeftAlignment then
  begin
    Result.Right := cliRect.Right - vRect.Left;
    Result.Left := cliRect.Right - vRect.Right;
  end;
end;

procedure TntvCustomGrid.DrawRow(Canvas: TCanvas; vRow: Integer; vRect, pntRect: TRect; vArea: TntvGridArea);
var
  X, W: Integer;
  aColCount, aCol, vrtCol: Integer;
  cliRect, TmpRect, aRect: TRect;
  s: String;
  aDrawState: TntvCellDrawState;
begin
  cliRect := ClientRect;
  X := vRect.Left;
  pntRect := FlipRect(cliRect, pntRect);
  if Indicator then
  begin
    tmpRect := vRect;
    tmpRect.Right := tmpRect.Left + IndicatorWidth;
    X := tmpRect.Right;
    if (pntRect.Left < IndicatorWidth) then
    begin
      aDrawState := [csdLastCell, csdFirstCell];
      if ((FState = dgsDown) and (FClkArea = garIndicator) and (vRow = FClkRow) and (FClkRow >= 0)) then
      begin
        if FStateBtn then
          aDrawState := aDrawState + [csdDown];
      end;
      if vRow = CurRow then
        Canvas.Brush.Color := MixColors(FFixedColor, Selected.Color, 230)
      else
        Canvas.Brush.Color := FFixedColor;
      tmpRect := FlipRect(cliRect, tmpRect);
      Canvas.FillRect(tmpRect);
      if RowNumbers and (vRow >= 0) then
      begin
        s := IntToStr(vRow + 1);
        if vRow >= Count then
          Canvas.Font.Color := clGray
        else
          Canvas.Font.Color := clBlack;
        SetTextColor(Canvas.Handle, ColorToRgb(Canvas.Font.Color));
      end
      else
        s := '';
      DrawFixed(Canvas, tmpRect, s, aDrawState);
      ExcludeClipRect(Canvas.Handle, TmpRect.Left, tmpRect.Top, tmpRect.Right, tmpRect.Bottom);
    end;
  end;
  if Fringe then
  begin
    tmpRect := vRect;
    tmpRect.Right := cliRect.Right;
    tmpRect.Left := tmpRect.Right - FringeWidth;
    if (pntRect.Right > (vRect.Right - FringeWidth)) then
    begin
      Canvas.Brush.Color := FFixedColor;
      tmpRect := FlipRect(cliRect, tmpRect);
      aDrawState := [csdLastCell, csdFirstCell];
      if (FState = dgsDown) and (FClkArea = garFringe) and (vRow = FClkRow) and (FClkRow >= 0) then
      begin
        if FStateBtn then
          aDrawState := aDrawState + [csdDown];
      end;
      Canvas.FillRect(tmpRect);
      DrawFixed(Canvas, tmpRect, '', aDrawState);
      ExcludeClipRect(Canvas.Handle, TmpRect.Left, tmpRect.Top, tmpRect.Right, tmpRect.Bottom);
    end;
  end;
  aCol := 0;
  aColCount := FVisibleColumns.Count;
  while (X < pntRect.Right) do
  begin
    vrtCol := GetVirtualCol(aCol);
    if vrtCol >= aColCount then
      break;
    W := FVisibleColumns[vrtCol].Width;
    aRect := Rect(X, vRect.Top, X + W, vRect.Bottom);
    aRect := FlipRect(cliRect, aRect);
    aDrawState := [];
    if (vrtCol = aColCount - 1) then
      aDrawState := aDrawState + [csdLastCell];
    if (vrtCol = 0) then
      aDrawState := aDrawState + [csdFirstCell];
    FVisibleColumns[vrtCol].Draw(Canvas, aDrawState, vRow, aRect, vArea);
    Inc(aCol);
    X := X + W;
  end;
  if (X < pntRect.Right) then
  begin
    TmpRect := pntRect;
    TmpRect.Left := X;
    TmpRect.Top := vRect.Top;
    TmpRect.Bottom := vRect.Bottom;
    tmpRect := FlipRect(cliRect, tmpRect);
    if (vArea = garHeader) and FullHeader then
    begin
      Canvas.Brush.Color := FFixedColor;
      Canvas.FillRect(TmpRect);
      DrawFixed(Canvas, tmpRect, '', [csdFixed, csdOpened]);
    end
    else
    begin
      Canvas.Brush.Color := Color;
      Canvas.FillRect(TmpRect);
    end;
  end;
end;

procedure TntvCustomGrid.DrawColSizeLine(X: Integer);
var
  DC: HDC;
  NewPen, OldPen: HPEN;
  OldRop2: Integer;
begin
  DC := GetDC(Handle);
  OldRop2 := SetROP2(DC, R2_NOT);
  NewPen := CreatePen(BS_SOLID, 1, ColorToRgb(clBtnShadow));
  OldPen := SelectObject(DC, NewPen);
  MoveToEx(DC, X, 0, nil);
  LineTo(DC, X, ClientRect.Bottom);
  SelectObject(DC, OldPen);
  DeleteObject(NewPen);
  SetROP2(DC, OldRop2);
  ReleaseDC(Handle, DC);
end;

function TntvCustomGrid.CancelEdit: Boolean;
var
  aColumnEdit: TntvColumn;
begin
  if Editing then
  begin
    aColumnEdit := ColumnEdit;
    ColumnEdit := nil;
    ColumnEdit.CloseEdit(False);
    RefreshCurrent;
    Result := True;
  end
  else
    Result := False;
end;

function TntvCustomGrid.Editing: Boolean;
begin
  Result := (ColumnEdit <> nil);
end;

function TntvCustomGrid.OpenEdit(OpenBy: TntvGridOpenEdit; Key: Char): Boolean;
begin
  Result := not ReadOnly and (CurrentColumn <> nil);
  if Result then
  begin
    SetInternalActiveRow(CurRow);
    if CanEdit(CurRow, CurCol) then
    begin
      DoBeforeEdit(CurrentColumn, CurRow);
      Result := CurrentColumn.OpenEdit(OpenBy, Key);
      Modified := True;
    end
    else
      Result := False;
  end;
end;

procedure TntvCustomGrid.CountChanged;
begin
  if not (csDestroying in ComponentState) then
  begin
    if Count > Capacity then
      Capacity := Count;
    UpdateScrollBar;
    InternalInvalidate;
    //  CheckPosition;
    if Assigned(FOnCountChanged) then
    begin
      FOnCountChanged(Self);
    end;
    DoCurRowChanged;
    Modified := True;
    DoChanged;
  end;
end;

procedure TntvCustomGrid.CloseEdit;
var
  aColumnEdit: TntvColumn;
begin
  if Editing and not Locked then
  begin
    aColumnEdit := ColumnEdit;
    ColumnEdit := nil;
    aColumnEdit.CloseEdit(True);
  end;
end;

procedure TntvCustomGrid.EndUpdate;
begin
  CheckPosition;
  Rows.EndUpdate;
  Locked := False;
  LockAutoSum := False;
end;

procedure TntvCustomGrid.EndCapture;
begin
  case FState of
    dgsDown:
    begin
      FStateBtn := False;
      RefreshCell(FClkRow, FClkCol, FClkArea);
    end;
    dgsDrag:
    begin
      KillTimer(0, 1);
      if FStateBtn = True then
      begin
        Click;
        FStateBtn := False;
      end;
      RefreshCell(FClkRow, FClkCol);
    end;
    dgsResizeCol:
    begin
      DrawColSizeLine(FOldX);
      CalcNewWidth(FOldX - FStartSizing);
    end;
  end;
  FState := dgsNone;
  FAttemptCapture := False;
end;

function TntvCustomGrid.GetMaxSideCol: Integer;
var
  i, w, r: Integer;
  vrtRect: TRect;
begin
  vrtRect := MovingRect;
  w := vrtRect.Right - vrtRect.Left;
  Result := FVisibleColumns.Count - 1;
  i := Result;
  if i >= 0 then
  begin
    r := FVisibleColumns[i].Width;
    while i > SettledCols do
    begin
      Dec(i);
      r := r + FVisibleColumns[i].Width;
      if r > w then
      begin
        break;
      end;
      Dec(Result);
    end;
  end;
end;

function TntvCustomGrid.GetAllowedRows: Integer;
begin
  Result := Capacity - GetCompletedRows;
end;

function TntvCustomGrid.GetLockAutoSum: Boolean;
begin
  Result := (FLockAutoSumCount > 0);
end;

function TntvCustomGrid.GetCellRect(vRow: Integer; vCol: Integer; var vRect: TRect): Boolean;
var
  aColRect: TRect;
begin
  if IsValidCell(vRow, vCol) then
  begin
    Result := GetRowRect(vRow, vRect);
    Result := Result and GetColRect(vCol, aColRect);
    if Result then
    begin
      vRect.Left := aColRect.Left;
      vRect.Right := aColRect.Right;
    end;
  end
  else
    Result := False;
end;

function TntvCustomGrid.GetCompletedRows: Integer;
begin
  Result := GetCompletedRows(VirtualHeight);
end;

function TntvCustomGrid.GetCompletedRows(vHeight: Integer): Integer;
begin
  Result := vHeight div FRowHeight;
end;

function TntvCustomGrid.GetCurrentColumn: TntvColumn;
begin
  if (FVisibleColumns.Count > 0) and (CurCol < FVisibleColumns.Count) then
    Result := FVisibleColumns[CurCol]
  else
    Result := nil;
end;

function TntvCustomGrid.GetCount: Integer;
begin
  Result := FRows.Count;
end;

function TntvCustomGrid.GetFactRow(vRow: Integer): Integer;
begin
  Result := vRow - FTopRow;
end;

function TntvCustomGrid.GetLock: Boolean;
begin
  Result := (FLockCount > 0);
end;

function TntvCustomGrid.GetMaxCols: Integer;
begin
  Result := FVisibleColumns.Count;
end;

function TntvCustomGrid.GetRowRect(vRow: Integer; out vRect: TRect): Boolean;
begin
  vRow := GetFactRow(vRow);
  Result := vRow < GetVisibleRows;
  if Result then
  begin
    vRect := VirtualClient;
    vRect.Top := vRect.Top + vRow * FRowHeight;
    vRect.Bottom := vRect.Top + FRowHeight;
  end;
end;

function TntvCustomGrid.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TntvCustomGrid.GetGridText(SpecialFormat: Boolean; vSelected: Boolean = False): String;
var
  aCell: TntvCell;
  aData: Integer;
begin
  if not vSelected then
    Result := GetTextRange(0, Count - 1, SpecialFormat)
  else
  if Selected.StartRow > -1 then
    Result := GetTextRange(Selected.StartRow, Selected.EndRow, SpecialFormat)
  else
  begin
    aCell := FVisibleColumns[CurCol].GetCell(CurRow);
    if aCell <> nil then
    begin
      Result := aCell.Text;
      aData := aCell.Data;
    end
    else
    begin
      Result := '';
      aData := 0;
    end;
    if SpecialFormat and (Result <> '') then
      Result := Result + cntv_X_DATA + IntToStr(aData) + cntv_X_DATA + IntToStr(FVisibleColumns[CurCol].Id);
  end;
  if SpecialFormat then
    Result := 'NaticeGrid=v1.0' + cntv_X_EOL + Result;
end;

function TntvCustomGrid.GetVirtualCol(vCol: Integer): Integer;
begin
  if (vCol < FSettledCols) then
    Result := vCol
  else
    Result := vCol + (FSideCol - FSettledCols);
end;

function TntvCustomGrid.GetVirtualRow(vRow: Integer): Integer;
begin
  Result := vRow + FTopRow;
end;

function TntvCustomGrid.GetVisibleCol(vVertCol: Integer; var vFactCol: Integer): Boolean;
var
  R, W, vCol, fCol: Integer;
begin
  Result := False;
  R := VirtualClient.Left;
  vFactCol := 0;
  fCol := 0;
  repeat
    begin
      vFactCol := fCol;
      vCol := GetVirtualCol(fCol);
      W := FVisibleColumns[vCol].Width;
      R := R + W;
      fCol := fCol + 1;
    end
  until (R >= VirtualClient.Right) or (vCol >= vVertCol);
  if (vCol = vVertCol) and not (R > VirtualClient.Right) then
    Result := True;
end;

function TntvCustomGrid.GetVisibleRows: Integer;
begin
  if HandleAllocated then
    Result := GetVisibleRows(VirtualHeight)
  else
    Result := Capacity;
end;

function TntvCustomGrid.GetVisibleRows(vHeight: Integer): Integer;
begin
  Result := GetCompletedRows(vHeight + FRowHeight - 1);
end;

function TntvCustomGrid.GetCompletedCols(vWidth: Integer): Integer;
var
  W, R: Integer;
  aCols, vrtCol: Integer;
begin
  R := VirtualClient.Left;
  aCols := 0;
  repeat
    begin
      Result := aCols;
      vrtCol := GetVirtualCol(aCols);
      if vrtCol >= FVisibleColumns.Count then
        break;
      W := FVisibleColumns[vrtCol].Width;
      R := R + W;
      aCols := aCols + 1;
    end
  until (R >= vWidth);
end;

function TntvCustomGrid.Search(vRow: Integer; vText: String): Boolean;
var
  R: Integer;
begin
  R := FindRowNext(vRow, CurrentColumn.StoreCol, vText, True);
  if R < 0 then
  begin
    Result := False;
  end
  else
  begin
    ShouldCurChange := True;
    CurRow := R;
    Result := True;
  end;
end;

function TntvCustomGrid.IncCurCol(Selecting: Boolean = False): Boolean;
begin
  InternalSetCurCol(FCurCol + 1, Selecting);
  Result := True;
end;

function TntvCustomGrid.IncCurRow(Selecting: Boolean = False): Boolean;
begin
  InternalSetCurRow(FCurRow + 1, Selecting);
  Result := True;
end;

function TntvCustomGrid.InColsWidth(X: Integer; var vCol: Integer): Boolean;
var
  R: Integer;
  aCol, vrtCol: Integer;
begin
  Result := False;
  if UseRightToLeftAlignment then
  begin
    X := ClientRect.Right - X;
    X := X - 1;
  end
  else
    X := X + 1;
  vCol := -1;
  R := VirtualClient.Left;
  aCol := 0;
  vrtCol := GetVirtualCol(aCol);
  while vrtCol < FVisibleColumns.Count do
  begin
    R := R + FVisibleColumns[vrtCol].Width;
    if Abs(X - R) <= 3 then
    begin
      vCol := vrtCol;
      Result := True;
      break;
    end;
    Inc(aCol);
    vrtCol := GetVirtualCol(aCol);
  end;
end;

procedure TntvCustomGrid.InsertRow(vRow: Integer);
begin
  Rows.BeginUpdate;
  try
    if vRow < Rows.Count then
      Rows.Insert(vRow, nil)
    else
      Rows.Count := vRow + 1;
  finally
    Rows.EndUpdate;
  end;
end;

procedure TntvCustomGrid.TryInsertRow(vRow: Integer);
begin
  if not ReadOnly and not Solid then
    InsertRow(vRow);
end;

procedure TntvCustomGrid.InsertRows(vRow, vCount: Integer);
begin
  Rows.BeginUpdate;
  try
    if Count < vRow then
      Count := vRow;
    if Rows.Capacity < (vRow + vCount) then
      Rows.Capacity := (vRow + vCount);
    while vCount > 0 do
    begin
      Rows.Insert(vRow, nil);
      Inc(vRow);
      Dec(vCount);
    end;
  finally
    Rows.EndUpdate;
  end;
end;

procedure TntvCustomGrid.InternalSetCurCol(vCol: Integer; Selecting: Boolean);
begin
  SetCurCell(FCurRow, vCol, Selecting, False);
end;

procedure TntvCustomGrid.InternalSetCurRow(vRow: Integer; Selecting: Boolean);
begin
  SetCurCell(vRow, FCurCol, Selecting, False);
end;

function TntvCustomGrid.InvalidateCell(vRow: Integer; vCol: Integer): Boolean;
var
  aRect: TRect;
begin
  if GetCellRect(vRow, vCol, ARect) then
  begin
    InvalidateRect(Handle, @aRect, False);
    Result := True;
  end
  else
    Result := False;
end;

procedure TntvCustomGrid.InvalidateRow(vRow: Integer);
var
  ARect: TRect;
  aRow: Integer;
begin
  aRow := GetFactRow(vRow);
  if not ((aRow < 0) or (aRow > GetVisibleRows)) then
  begin
    if GetRowRect(vRow, ARect) then
    begin
      if UseRightToLeftAlignment then
        ARect := FlipRect(ClientRect, ARect);
      InvalidateRect(Handle, @ARect, False);
    end;
  end;
end;

function TntvCustomGrid.InvlidateRowsClient: Boolean;
var
  ARect: TRect;
begin
  ARect := RowsClient;
  InvalidateRect(Handle, @ARect, False);
  Result := True;
end;

function TntvCustomGrid.IsBlankRow(vRow: Integer): Boolean;
begin
  Result := ((vRow = Count) and (Count >= 0));
end;

function TntvCustomGrid.IsSelecting: Boolean;
begin
  if (FState = dgsDrag) and MultiSelect or (GetKeyShiftState = [ssShift]) then
  begin
    Result := True;
  end
  else
  begin
    Result := False;
  end;
end;

function TntvCustomGrid.IsValidCell(vRow: Integer; vCol: Integer): Boolean;
begin
  if (vRow < Capacity) and (vRow >= 0) and (vCol < FVisibleColumns.Count) and (vCol >= 0) then
    Result := True
  else
    Result := False;
end;

procedure TntvCustomGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
  aKeyEvent: TntvKeyAction;
  aResumed: Boolean;
begin
  if MouseCapture then
    EndCapture;
  inherited;
  aKeyEvent := KeyDownToKeyAction(Key, Shift);
  aResumed := False;
  if aKeyEvent <> keyaNone then
  begin
    KeyAccident(aKeyEvent, aResumed);
    if aResumed then
      Key := 0;
  end;
  if (not aResumed) and (CurrentColumn <> nil) then
    CurrentColumn.KeyDown(Key, Shift);
end;

procedure TntvCustomGrid.Loaded;
begin
  inherited;
  ColsChanged;
end;

procedure TntvCustomGrid.LockChanged(Value: Boolean);
begin
end;

procedure TntvCustomGrid.CopyClick(Sender: TObject);
begin
  ClipboardCopy(True);
end;

procedure TntvCustomGrid.CopyAllClick(Sender: TObject);
begin
  ClipboardCopy(False);
end;

procedure TntvCustomGrid.DeleteLineClick(Sender: TObject);
begin
  TryDeleteRow(CurRow);
end;

procedure TntvCustomGrid.InsertLineClick(Sender: TObject);
begin
  TryInsertRow(CurRow);
end;

procedure TntvCustomGrid.PasteClick(Sender: TObject);
begin
  if not ReadOnly then
    ClipboardPaste;
end;

procedure TntvCustomGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  aCol: Integer;
  BtnRect: TRect;

  procedure DoButtonNow;
  begin
    FState := dgsDown;
    FStateBtn := True;
    RefreshCell(FClkRow, FClkCol, FClkArea);
    BeginCapture(X, Y);
  end;

begin
  inherited;
  FAttemptCapture := True;
  CloseEdit;
  if PosToCoord(X, Y, FClkRow, FClkCol, FClkArea) then
  begin
    aCol := FClkCol;
    case FClkArea of
      garIndicator, garFooter, garFringe:
        if FAttemptCapture then
        begin
          DoButtonNow;
        end;
      garHeader:
      begin
        if aCol > -1 then
        begin
          BtnRect := VisibleColumns[aCol].FBtnRect;
          if (VisibleColumns[aCol].ShowButton) and (PtInRect(BtnRect, Point(X, Y))) then
          begin
            VisibleColumns[aCol].Down := True;
            FDownCol := aCol;
          end
          else
          if FAttemptCapture then
          begin
            if InColsWidth(X, aCol) then
            begin
              FClkCol := aCol;
              FState := dgsResizeCol;
              FStartSizing := X;
              BeginCapture(X, Y);
            end
            else
              DoButtonNow;
          end;
        end;
      end;
      garNormal:
        if not (csDesigning in ComponentState) then
        begin
          if FSelected.IsSelected(FClkRow) then
          begin
            if FAttemptCapture then
              DoButtonNow;
            if FMultiSelect and RowSelect then
              FSelected.Select(FOldRow, FClkRow, Shift);
            Exit;
          end;
          if aCol >= FixedCols then
          begin
            if {not ShowHeader  and } InColsWidth(X, aCol) then
            begin
              FClkCol := aCol;
              FState := dgsResizeCol;
              FStartSizing := X;
              BeginCapture(X, Y);
            end
            else
            begin
              SetFocus;
              SetCurCell(FClkRow, FClkCol, (ssShift in Shift), (ssCtrl in Shift));
              if FMultiSelect and RowSelect then
              begin
                FSelected.Select(FOldRow, FClkRow, Shift);
                if ssCtrl in Shift then
                  DoCurRowChanged;
              end;
              if ssDouble in Shift then
                OpenEdit(goeMouse, #0)
              else
              begin
                if FAttemptCapture then
                begin
                  FState := dgsDrag;
                  BeginCapture(X, Y);
                end;
              end;
              CurrentColumn.MouseDown(Button, Shift, X, Y);
            end;
          end
          else
          if FAttemptCapture then
            DoButtonNow;
        end;
    end;
  end;
end;

procedure TntvCustomGrid.StartUpdate;
var
  aRP: Boolean;
begin
  aRP := Count > 0;
  Rows.BeginUpdate;
  Locked := True;
  LockAutoSum := True;
  Reset;
  if aRP then
    ResetPosition;
end;

procedure TntvCustomGrid.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then
  begin
    if (FImageList <> nil) and (AComponent = ImageList) then
    begin
      ImageList := nil;
    end;
  end;
end;

procedure TntvCustomGrid.Paint;
begin
  if (not Rows.Updating) then
  begin
    Canvas.Font := Font;
    Draw(Canvas, ClientRect, Canvas.ClipRect);
  end;
end;

//Ported from Lazarus Grid
function GetWorkingCanvas(const Canvas: TCanvas): TCanvas;
var
  DC: HDC;
begin
  if (Canvas = nil) or (not Canvas.HandleAllocated) then
  begin
    DC := GetDC(0);
    Result := TCanvas.Create;
    Result.Handle := DC;
  end
  else
    Result := Canvas;
end;

//Ported from Lazarus Grid
procedure FreeWorkingCanvas(var Canvas: TCanvas);
begin
  ReleaseDC(0, Canvas.Handle);
  FreeAndNil(Canvas);
end;

//Ported from Lazarus Grid
function TntvCustomGrid.GetDefaultRowHeight: integer;
var
  TmpCanvas: TCanvas;
begin
  tmpCanvas := GetWorkingCanvas(Canvas);
  tmpCanvas.Font := Font;
  tmpCanvas.Font.PixelsPerInch := Font.PixelsPerInch;
  result := tmpCanvas.TextHeight('Fj')+7;
  if tmpCanvas <> Canvas then
    FreeWorkingCanvas(tmpCanvas);
end;

function TntvCustomGrid.GetTextStyle(vCentered: Boolean): TTextStyle;
begin
  Finalize(Result);
  Result.RightToLeft := UseRightToLeftReading;
  Result.Layout := tlCenter;
  Result.SingleLine := True;
  Result.Opaque := True;

  if vCentered then
    Result.Alignment := taCenter
  else if Result.RightToLeft then
    Result.Alignment := taRightJustify
  else
    Result.Alignment := taLeftJustify;
end;

function TntvCustomGrid.PosToCoord(x, y: Integer; out vRow: Integer; out vCol: Integer; out vArea: TntvGridArea; vCorrect: Boolean): Boolean;
var
  vrtRect: TRect;
begin
  if UseRightToLeftAlignment then
    X := ClientRect.Right - x;
  vrtRect := VirtualClient;
  Result := True;
  if Header and (y < vrtRect.Top) then
  begin
    vRow := -1;
    vArea := garHeader;
  end
  else
  if Footer and (y > vrtRect.Bottom) then
  begin
    vRow := -1;
    vArea := garFooter;
  end
  else
  begin
    vRow := GetVirtualRow(GetVisibleRows(y - vrtRect.Top) - 1);
    vArea := garNormal;
    Result := Result and (vRow < Capacity);
    if vRow >= Capacity then
    begin
      vRow := Capacity - 1;
      if not vCorrect then
        vArea := garNone;
    end;
  end;

  if Indicator and (x < VirtualClient.Left) then
  begin
    vCol := -1;
    if vArea = garNormal then
      vArea := garIndicator;
  end
  else
  if Fringe and (x > VirtualClient.Right) then
  begin
    vCol := -1;
    if vArea = garNormal then
      vArea := garFringe;
  end
  else
  begin
    vCol := GetVirtualCol(GetCompletedCols(X));
    Result := Result and (vCol < FVisibleColumns.Count);
    if (vCol >= FVisibleColumns.Count) then
    begin
      vCol := FVisibleColumns.Count - 1;
      if not vCorrect then
        vArea := garNone;
    end;
  end;
end;

procedure TntvCustomGrid.RefreshFooter;
var
  aRect: TRect;
begin
  if Footer then
  begin
    aRect := ClientRect;
    aRect.Top := RowsClient.Bottom;
    InvalidateRect(Handle, @aRect, False);
  end;
end;

procedure TntvCustomGrid.RefreshCell(vRow: Integer; vCol: Integer);
begin
  if InvalidateCell(vRow, vCol) then
  begin
    //    UpdateWindow(False);
  end;
end;

procedure TntvCustomGrid.RefreshCol(vCol: Integer);
var
  ARect: TRect;
begin
  if GetColRect(vCol, ARect) then
    InvalidateRect(Handle, @ARect, False);
end;

procedure TntvCustomGrid.RefreshRow(vRow: Integer);
begin
  InvalidateRow(vRow);
end;

procedure TntvCustomGrid.DublicateRow(vRow: Integer);
begin
  Rows.BeginUpdate;
  try
    Rows.Insert(vRow, nil);
    Rows.CopyLine(vRow - 1, vRow);
  finally
    Rows.EndUpdate;
  end;
  AddTotals(vRow);
end;

procedure TntvCustomGrid.ResetPosition;
begin
  FCurRow := 0;
  FCurCol := FFixedCols;
  FTopRow := 0;
  FSideCol := FSettledCols;
  UpdateScrollBar;
end;

procedure TntvCustomGrid.RowsScroll(vRows: Integer);
var
  ARect: TRect;
begin
  ARect := RowsClient;
  ScrollWindow(Handle, 0, FRowHeight * (vRows), nil, @ARect);
end;

procedure TntvCustomGrid.SelectRows(vOldRow, vRow: Integer);
var
  OldBegin: Integer;
begin
  OldBegin := Selected.StartRow;
  if vRow = -1 then
  begin
    if not ((Selected.StartRow < 0) and (Selected.EndRow < 0)) or (FSelected.FSelectedRows.Count > 0) then
    begin
      FSelected.FSelectedRows.Clear;
      Selected.StartRow := -1;
      Selected.EndRow := -1;
      InternalInvalidate;
    end;
  end
  else
  begin
    if OldBegin < 0 then
    begin
      Selected.StartRow := vOldRow;
      Selected.EndRow := vRow;
    end
    else
    begin
      Selected.EndRow := vRow;
    end;
    InternalInvalidate;
  end;
end;

procedure TntvCustomGrid.SetLockAutoSum(const Value: Boolean);
begin
  if Value then
    Inc(FLockAutoSumCount)
  else
  if FLockAutoSumCount > 0 then
    Dec(FLockAutoSumCount);
end;

procedure TntvCustomGrid.SetColumnEdit(const Value: TntvColumn);
begin
  if FColumnEdit <> Value then
    FColumnEdit := Value;
end;

procedure TntvCustomGrid.SetCurCell(vRow: Integer; vCol: Integer; Selecting, CtrlSelecting: Boolean);
var
  OldCol: Integer;
  IsRowChanged: Boolean;
begin
  if (FCurRow <> vRow) or (FCurCol <> vCol) then
  begin
    CloseEdit;
  end;
  if (vRow > (Capacity - 1)) then
    vRow := Capacity - 1;
  if (vRow < 0) then
    vRow := 0;
  if (vCol > (FVisibleColumns.Count - 1)) then
    vCol := FVisibleColumns.Count - 1;
  if (vCol < FFixedCols) then
    vCol := FFixedCols;
  FOldRow := FCurRow;
  OldCol := FCurCol;
  DoCurChange(vRow, vCol);
  FCurRow := vRow;
  FCurCol := vCol;

  if FIndicator and (FOldRow <> FCurRow) then
    RefreshCell(FOldRow, -1, garIndicator);
  if FHeader and (OldCol <> FCurCol) then
    RefreshCell(-1, OldCol, garHeader);

  if (OldCol <> vCol) or (FOldRow <> vRow) then
  begin
    if RowRefresh and ((OldCol <> vCol) and (FOldRow = vRow)) then
    //nothing
    else
    if RowSelect then
    begin
      if not (CtrlSelecting and FMultiSelect) then
        RefreshRow(FOldRow);
    end
    else
      RefreshCell(FOldRow, OldCol);

    if MultiSelect then
    begin
      if not (CtrlSelecting and FRowSelect) then
      begin
        if Selecting then
          SelectRows(FOldRow, vRow)
        else
          SelectRows(FOldRow, -1);
      end;
    end;
    CurChanged(FOldRow, OldCol);
  end;

  IsRowChanged := False;
  if (FOldRow <> vRow) or ShouldCurChange then
  begin
    ShowRow(vRow);
    DoCurRowChanging(FOldRow, vRow);
    IsRowChanged := True;
  end;

  if OldCol <> vCol then
  begin
    ColChanged(GetVisibleColumn(OldCol), GetVisibleColumn(vCol));
    ShowCol(vCol);
  end;

  if (OldCol <> vCol) or (FOldRow <> vRow) then
  begin
    if RowRefresh and ((OldCol <> vCol) and (FOldRow = vRow)) then
      InvalidateRow(vRow)
    else
      InvalidateCell(vRow, vCol);
  end;

  if FIndicator and (FOldRow <> FCurRow) then
    RefreshCell(FCurRow, -1, garIndicator);
  if FHeader and (OldCol <> FCurCol) then
    RefreshCell(0, FCurCol, garHeader);
  if IsRowChanged then
  begin
    if (VisibleColumns.Count > 0) then
      VisibleColumns[FCurCol].CurRowChanged;
    if not (CtrlSelecting and MultiSelect and RowSelect) then
      DoCurRowChanged;
  end;
  ShouldCurChange := False;
end;

procedure TntvCustomGrid.SetCurCol(const Value: Integer);
begin
  InternalSetCurCol(Value, False);
end;

procedure TntvCustomGrid.SetCurRow(const Value: Integer);
begin
  InternalSetCurRow(Value, False);
end;

procedure TntvCustomGrid.SetColWidth(Value: Integer);
begin
  FColWidth := Value;
  if not (csLoading in ComponentState) then
  begin
    UpdateScrollBar;
    InternalInvalidate;
  end;
end;

procedure TntvCustomGrid.SetCount(Value: Integer);
begin
  if not (csLoading in ComponentState) then //zaher temp
    FRows.Count := Value;
end;

procedure TntvCustomGrid.SetEvenColor(Value: TColor);
begin
  if FEvenColor <> Value then
  begin
    FEvenColor := Value;
    InternalInvalidate;
  end;
end;

procedure TntvCustomGrid.SetFixedColor(const Value: TColor);
begin
  if FFixedColor <> Value then
  begin
    FFixedColor := Value;
    Invalidate;
  end;
end;

procedure TntvCustomGrid.SetFixedCols(Value: Integer);
begin
  if FFixedCols <> Value then
  begin
    FFixedCols := Value;
    if FSettledCols < Value then
      FSettledCols := Value;
    ResetPosition;
    UpdateScrollBar;
    InternalInvalidate;
  end;
end;

procedure TntvCustomGrid.SeTntvGridLines(Value: TntvGridLines);
begin
  if FGridLines <> Value then
  begin
    FGridLines := Value;
    InternalInvalidate;
  end;
end;

procedure TntvCustomGrid.SetLinesColor(const Value: TColor);
begin
  if FLinesColor <> Value then
  begin
    FLinesColor := Value;
    InternalInvalidate;
  end;
end;

procedure TntvCustomGrid.SetLock(const Value: Boolean);
begin
  if Value then
  begin
    Inc(FLockCount);
    if FLockCount = 1 then
    begin
      LockChanged(True);
    end;
  end
  else
  begin
    if FLockCount > 0 then
    begin
      Dec(FLockCount);
      if FLockCount = 0 then
      begin
        LockChanged(False);
      end;
    end;
  end;
end;

procedure TntvCustomGrid.SetModified(Value: Boolean);
var
  OldValue: Boolean;
begin
  if FModified <> Value then
  begin
    if not Locked then
    begin
      OldValue := FModified;
      FModified := Value;
      try
        if Assigned(FOnModified) then
          FOnModified(Self);
        if Value then
          DoModified;
      except
        FModified := OldValue;
        raise;
      end;
    end;
  end;
end;

procedure TntvCustomGrid.SetMultiSelect(const Value: Boolean);
begin
  if FMultiSelect <> Value then
  begin
    FMultiSelect := Value;
    if not Value then
    begin
      Selected.StartRow := -1;
      Selected.EndRow := -1;
      Selected.FSelectedRows.Clear;
      InternalInvalidate;
    end;
  end;
end;

procedure TntvCustomGrid.SetOddColor(Value: TColor);
begin
  if FOddColor <> Value then
  begin
    FOddColor := Value;
    InternalInvalidate;
  end;
end;

procedure TntvCustomGrid.SetRowHeight(Value: Integer);
begin
  if Value = FRowHeight then
    exit;
  FRowHeight := Value;
  if FRowHeight <= 0 then
    FRowHeight := 17;
  UpdateScrollBar;
  InternalInvalidate;
end;

procedure TntvCustomGrid.SetCapacity(Value: Integer);
begin
  if FCapacity <> Value then
  begin
    FCapacity := Value;
    UpdateScrollBar;
    InternalInvalidate;
  end;
end;

procedure TntvCustomGrid.SetScrollBars(Value: TScrollStyle);
begin
  if FScrollBars <> Value then
  begin
    FScrollBars := Value;
    ShowScrolls(FScrollBars);
  end;
end;

procedure TntvCustomGrid.PasteText(vText: String; SpecialFormat: Boolean);
var
  aText: String;
  aGuid: String;
  P, aData: Integer;
  aCheckData: Boolean;
  ntvEOC: Char;
  ntvEOL: Char;
  aColumn: TntvColumn;
begin
  aCheckData := True;
  if SpecialFormat then
  begin
    P := AnsiPos(cntv_X_EOL, vText);
    if p > 0 then
    begin
      aGuid := GetPartValue(Copy(vText, 1, p - 1));
      aCheckData := not SameText(aGuid, sGridVersion);
      Delete(vText, 1, p);
    end;
    ntvEOC := cntv_X_EOC;
    ntvEOL := cntv_X_EOL;
  end
  else
  begin
    ntvEOC := cntv_EOC;
    ntvEOL := cntv_EOL;
  end;

  if not Solid and (AnsiPos(ntvEOC, vText) > 0) or (AnsiPos(ntvEOL, vText) > 0) then
    PasteRange(vText, SpecialFormat, aCheckData)
  else
  begin
    if not FVisibleColumns[CurCol].GetReadOnly then
    begin
      vText := GetPartStr(vText, ntvEOL, 0, #0);
      ActiveRow := CurRow;
      aColumn := FVisibleColumns[CurCol];
      if SpecialFormat then
      begin
        aText := GetPartStr(vText, cntv_X_DATA, 0, #0);
        aData := StrToIntDef(GetPartStr(vText, cntv_X_DATA, 1, #0), 0);
        aColumn.SetInfo(ActiveRow, aText, aData, sSetCellFull + sCheckOrValidate[aCheckData or (aData = 0)]);
      end
      else
        aColumn.SetInfo(ActiveRow, vText, 0, sSetCellFull + sCheckOrValidate[aCheckData]);
      AfterPaste(aColumn, ActiveRow);
      AfterEdit(aColumn, ActiveRow);
    end;
  end;
  DoChanged;
end;

procedure TntvCustomGrid.SetSideCol(vCol: Integer);
var
  aDelta, OldSideCol: Integer;
begin
  if (FSideCol <> vCol) then
  begin
    CloseEdit;
  end;
  if (vCol < FSettledCols) then
    vCol := FSettledCols;
  if (vCol > GetMaxSideCol) then
    vCol := GetMaxSideCol;
  OldSideCol := FSideCol;
  if OldSideCol <> vCol then
  begin
    aDelta := vCol - OldSideCol;
    ColsScroll(aDelta);
  end;
  FSideCol := vCol;
  UpdateScrollBar;
  //  UpdateWindow(Handle);
end;

procedure TntvCustomGrid.SetTopRow(vRow: Integer);
var
  aDelta, OldTopRow, aFact: Longint;
begin
  if (FTopRow <> vRow) then
  begin
    CloseEdit;
  end;
  aFact := Capacity - GetCompletedRows;
  if aFact < 0 then
    aFact := 0;
  if (vRow >= aFact) then
    vRow := aFact;
  if (vRow < 0) then
    vRow := 0;
  OldTopRow := FTopRow;
  aDelta := OldTopRow - vRow;
  if Abs(aDelta) < (GetCompletedRows) then
  begin
    RowsScroll(aDelta);
  end
  else
  begin
    InvlidateRowsClient;
  end;
  FTopRow := vRow;
  UpdateScrollBar;
  //  UpdateWindow(Handle);
end;

procedure TntvCustomGrid.ShowCol(vCol: Integer);
var
  aCol, aVrtCol: Integer;
  AWidth: Integer;
begin
  if vCol < FSideCol then
  begin
    SetSideCol(vCol);
  end
  else
  begin
    if not GetVisibleCol(vCol, aCol) then
    begin
      aCol := 0;
      AWidth := VirtualClient.Right;
      while (AWidth > VirtualClient.Left) and (aCol < FSettledCols) do //zaher
      begin
        AWidth := AWidth - FVisibleColumns[aCol].Width;
        aCol := aCol + 1;
      end;
      aCol := vCol;
      aVrtCol := aCol;
      AWidth := AWidth - FVisibleColumns[aCol].Width;
      while (AWidth > VirtualClient.Left) do
      begin
        aVrtCol := aCol;
        aCol := aCol - 1;
        AWidth := AWidth - FVisibleColumns[aCol].Width;
      end;
      SetSideCol(aVrtCol);
    end;
    RefreshCell(CurRow, vCol);
  end;
end;

procedure TntvCustomGrid.ShowRow(vRow: Integer);
begin
  if vRow < 0 then
    vRow := 0;
  if vRow < FTopRow then
  begin
    SetTopRow(vRow);
  end
  else
  if (vRow >= (FTopRow + GetCompletedRows)) then
  begin
    SetTopRow(vRow - GetCompletedRows + 1);
  end
  else
  begin
    if RowSelect then
      RefreshRow(vRow)
    else
      RefreshCell(vRow, CurCol);
  end;
end;

procedure TntvCustomGrid.ShowScrolls(Value: TScrollStyle);
begin
  case Value of
    ssNone:
    begin
      ShowScrollBar(Handle, SB_BOTH, False);
    end;
    ssVertical:
    begin
      ShowScrollBar(Handle, SB_VERT, True);
      ShowScrollBar(Handle, SB_HORZ, False);
    end;
    ssHorizontal:
    begin
      ShowScrollBar(Handle, SB_VERT, False);
      ShowScrollBar(Handle, SB_HORZ, True);
    end;
    ssBoth:
    begin
      ShowScrollBar(Handle, SB_BOTH, True);
    end;
  end;
end;

procedure TntvCustomGrid.SubTotals(vRow: Integer);
var
  i: Integer;
  aColGrid: TntvColumn;
  aCell: TntvCell;
begin
  for i := 0 to Columns.Count - 1 do
  begin
    aColGrid := Columns[i];
    if (aColGrid <> nil) and (aColGrid.IsTotal) then
    begin
      aCell := aColGrid.GetCell(vRow);
      if aCell <> nil then
        aColGrid.Total := aColGrid.Total - StrToCurr(aCell.Text);
    end;
  end;
end;

function TntvCustomGrid.VirtualHeight: Integer;
var
  vrtRect: TRect;
begin
  vrtRect := RowsClient;
  Result := vrtRect.Bottom - vrtRect.Top;
end;

function TntvCustomGrid.VirtualClient: TRect;
begin
  Result := ClientRect;
  if Indicator then
    Result.Left := Result.Left + IndicatorWidth;
  if Fringe then
    Result.Right := Result.Right - FringeWidth;
  if Header then
    Result.Top := Result.Top + RowHeight;
  if Footer then
    Result.Bottom := Result.Bottom - RowHeight;
end;

procedure TntvCustomGrid.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TntvCustomGrid.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := DLGC_WANTARROWS or DLGC_WANTCHARS;
  if WantTab and CanMoveCurrent then
    Message.Result := Message.Result or DLGC_WANTTAB
  else
    Message.Result := Message.Result and not DLGC_WANTTAB;
{  if WantReturn and CanMoveCurrent then
     Message.Result := Message.Result or DLGC_WANTMESSAGE
   else
     Message.Result := Message.Result and not DLGC_WANTMESSAGE;}
end;

procedure TntvCustomGrid.WMHScroll(var Message: TWMHScroll);
var
  c: Integer;
begin
  case Message.ScrollCode of
    SB_LINEDOWN:
    begin
      SetSideCol(FSideCol - (Ord(UseRightToLeftAlignment) * 2 - 1));
    end;
    SB_LINEUP:
    begin
      SetSideCol(FSideCol + (Ord(UseRightToLeftAlignment) * 2 - 1));
    end;
    SB_PAGEDOWN:
    begin
      c := GetCompletedCols(VirtualWidth) - SettledCols;
      if c <= 0 then
        c := 1;
      SetSideCol(FSideCol - c * (Ord(UseRightToLeftAlignment) * 2 - 1));
    end;
    SB_PAGEUP:
    begin
      c := GetCompletedCols(VirtualWidth) - SettledCols;
      if c <= 0 then
        c := 1;
      SetSideCol(FSideCol + c * (Ord(UseRightToLeftAlignment) * 2 - 1));
    end;
    SB_TOP:
    begin
      SetSideCol(SettledCols);
    end;
    SB_BOTTOM:
    begin
      SetSideCol(FVisibleColumns.Count - 1);
    end;
    SB_THUMBTRACK:
    begin
      c := GetMaxSideCol - SettledCols;
      if UseRightToLeftAlignment then
        SetSideCol((c - Message.Pos) + SettledCols)
      else
        SetSideCol(Message.Pos + SettledCols);
    end;
  end;
  Message.Result := 0;
end;

procedure TntvCustomGrid.RefreshCurrent;
begin
  if RowSelect then
    RefreshRow(FCurRow)
  else
    RefreshCell(FCurRow, FCurCol);
end;

procedure TntvCustomGrid.WMKillFocus(var Message: TWMKillFocus);
begin
  EndCapture;
  RefreshCurrent;
  Message.Result := 0;
end;

procedure TntvCustomGrid.DoExit;
begin
  inherited DoExit;
end;

procedure TntvCustomGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  aRect: TRect;
  PT: TPoint;
  b: Boolean;
  aRow: Integer;
  aCol: Integer;
  aArea: TntvGridArea;
begin
  inherited;
  if PosToCoord(X, Y, aRow, aCol, aArea) then
  begin
    ChangeCursor(X, Y);
    if (aCol > -1) then
    begin
      if (aArea = garHeader) then
      begin
        if (FUnderMouseCol >= 0) and (FUnderMouseCol <> aCol) and (FUnderMouseCol < VisibleColumns.Count) then
          VisibleColumns[FUnderMouseCol].MouseInHeader := False;
        VisibleColumns[aCol].MouseInHeader := True;
        FUnderMouseCol := aCol;
      end
      else
        VisibleColumns[aCol].MouseInHeader := False;
    end;
  end;

  if FDownCol > -1 then
  begin
    if (PtInRect(VisibleColumns[FDownCol].FBtnRect, Point(X, Y))) then
      VisibleColumns[FDownCol].Down := True
    else
      VisibleColumns[FDownCol].Down := False;
  end;

  case FState of
    dgsDrag:
    begin
      if (csLButtonDown in ControlState) then
      begin
        if (FScrollTimer = 0) then
        begin
          FScrollTimer := SetTimer(Handle, 1001, 100, nil);
        end
        else
          ProcessScrollTimer;
      end;
    end;
    dgsDown:
    begin
      if GetCellRect(FClkRow, FClkCol, FClkArea, aRect) then
      begin
        Pt.X := X;
        Pt.Y := Y;
        b := PtInRect(aRect, Pt);
      end
      else
        b := False;
      if FStateBtn <> b then
      begin
        FStateBtn := b;
        RefreshCell(FClkRow, FClkCol, FClkArea);
      end;
    end;
    dgsResizeCol:
    begin
      DrawColSizeLine(FOldX);
      FOldX := X;
      DrawColSizeLine(FOldX);
    end;
  end;
end;

procedure TntvCustomGrid.ChangeCursor(x, y: Integer);
var
  aCol: Integer;
  aRow: Integer;
  aArea: TntvGridArea;
  PT: TPoint;
begin
  PT := Point(x, y);
  if PosToCoord(Pt.X, Pt.Y, aRow, aCol, aArea) and ((aArea in [garHeader, garNormal]) and (InColsWidth(PT.X, aCol)) or (FState = dgsResizeCol)) then
    SetCursor(crHSplit)
  else
    SetCursor(crDefault);
end;

function TntvCustomGrid.GetValues(Row, Col: Integer): string;
var
  aRow: TntvRow;
  aCell: TntvCell;
begin
  aRow := Rows[Row];
  aCell := aRow[Col];
  Result := aCell.Text;
end;

procedure TntvCustomGrid.SetValues(Row, Col: Integer; AValue: string);
var
  aRow: TntvRow;
  aCell: TntvCell;
begin
  aRow := Rows[Row];
  aCell := aRow[Col];
  aCell.Text := AValue;
  RefreshCell(Row, Col);
end;

procedure TntvCustomGrid.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  if (Editing and (ColumnEdit.IsFocused(Message.FocusedWnd))) then
  begin
    CloseEdit;
  end;
  RefreshCurrent;
  Message.Result := 1;
end;

procedure TntvCustomGrid.WMSize(var Message: TWMSize);
begin
  inherited;
  if not (csLoading in ComponentState) then
  begin
    UpdateColumnsWidths;
    UpdateScrollBar;
  end;
end;

procedure TntvCustomGrid.WMTimer(var Message: TWMTimer);
var
  Pt: TPoint;
begin
  case FState of
    dgsDrag:
    begin
      case FDragAfterMode of
        damNone:
        begin
          Pt := ScreenToClient(Mouse.CursorPos);
          if not PtInRect(VirtualClient, Pt) then
            Perform(WM_MOUSEMOVE, 0, MakeLong(Word(Pt.x), Word(Pt.y)));
        end;
      end;
    end;
  end;
end;

procedure TntvCustomGrid.WMVScroll(var Message: TWMVScroll);
begin
  case Message.ScrollCode of
    SB_LINEDOWN:
    begin
      SetTopRow(FTopRow + 1);
    end;
    SB_LINEUP:
    begin
      SetTopRow(FTopRow - 1);
    end;
    SB_PAGEDOWN:
    begin
      SetTopRow(FTopRow + GetCompletedRows);
    end;
    SB_PAGEUP:
    begin
      SetTopRow(FTopRow - GetCompletedRows);
    end;
    SB_TOP:
    begin
      SetTopRow(0);
    end;
    SB_BOTTOM:
    begin
      SetTopRow(Capacity - 1);
    end;
    SB_THUMBTRACK:
    begin
      SetTopRow(Message.Pos);
    end;
  end;
  //  UpdateWindow(Handle);
  Message.Result := 0;
end;

{ TGridSelected }

constructor TGridSelected.Create(Grid: TntvCustomGrid);
begin
  FStartRow := -1;
  FEndRow := -1;
  FGrid := Grid;
  Color := clHighlight;
  FTextColor := clHighlightText;
  FSelectedRows := TntvSelectedRows.Create;
end;


function TntvColumn.CreateEdit: TControl;
begin
  Result := nil;
end;

procedure TntvColumn.FreeEdit;
begin
  Application.ReleaseComponent(FEditControl);
  FEditControl := nil;
end;

function TntvColumn.GetEditData: Integer;
begin
  Result := 0;
end;

function TntvColumn.GetEditText: String;
begin
  Result := '';
end;

procedure TntvColumn.HideEdit;
begin
end;

function TntvColumn.IsFocused(Handle: THandle = 0): Boolean;
begin
  Result := False;
end;

procedure TntvColumn.SetEditData(const Value: Integer);
begin
end;

procedure TntvColumn.SetEditText(const Value: String);
begin

end;

procedure TntvColumn.ShowEdit(vChar: Char);
begin
end;

destructor TGridSelected.Destroy;
begin
  FreeAndNil(FSelectedRows);
  inherited;
end;

function TGridSelected.IsSelected(vRow: Integer): Boolean;
begin
  Result := ((abs(vRow - FStartRow) + abs(vRow - FEndRow)) = abs(FStartRow - FEndRow));
  if Result then
    Exit
  else
    Result := (FSelectedRows.Found(vRow) > -1);
end;

procedure TGridSelected.Select(vOldRow, vRow: Integer; Shift: TShiftState);
begin
  if (ssCtrl in Shift) then
  begin
    if (FStartRow = -1) and (FEndRow = -1) and (FSelectedRows.Count = 0) then
      FSelectedRows.Add(vOldRow);
    InternalSelect(vRow);
  end
  else
  begin
    FSelectedRows.Clear;
    if not (ssShift in Shift) then
    begin
      FStartRow := -1;
      FEndRow := -1;
    end;
  end;
  FGrid.InternalInvalidate;
end;

procedure TGridSelected.InternalSelect(Row: Integer);
var
  BCount, ACount: Integer;
  Idx: Integer;
begin
  if ((abs(Row - FStartRow) + abs(Row - FEndRow)) = abs(FStartRow - FEndRow)) then
  begin
    if ABS(FEndRow - FStartRow) = 0 then
    begin
      FStartRow := -1;
      FEndRow := -1;
      if FSelectedRows.Count = 0 then
        FGrid.FCurRow := Row;
    end
    else
    begin
      BCount := ABS(Row - FStartRow);
      ACount := ABS(FEndRow - Row);
      if BCount >= ACount then
      begin
        for Idx := Row + 1 to FEndRow do
          FSelectedRows.Add(Idx);
        FEndRow := Row - 1;
        if Row = 0 then
          FGrid.FCurRow := Row
        else
          FGrid.FCurRow := Row - 1;
      end
      else
      begin
        for Idx := FStartRow to Row - 1 do
          FSelectedRows.Add(Idx);
        FStartRow := Row + 1;
        FGrid.FCurRow := Row + 1;
      end;
      if Assigned(FGrid.OnCurRowChanged) then
        FGrid.OnCurRowChanged(FGrid, FGrid.FCurRow);
    end;
    Exit;
  end;
  Idx := FSelectedRows.Found(Row);
  if Idx > -1 then
  begin
    FSelectedRows.Delete(Idx);
    if (FSelectedRows.Count = 0) and (FStartRow = -1) and (FEndRow = -1) then
      FGrid.FCurRow := Row;
    if Assigned(FGrid.OnCurRowChanged) then
      FGrid.OnCurRowChanged(FGrid, Row);
  end
  else
    FSelectedRows.Add(Row);
end;

procedure TGridSelected.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    FGrid.InternalInvalidate;
  end;
end;

procedure TGridSelected.SetTextColor(const Value: TColor);
begin
  if FTextColor <> Value then
  begin
    FTextColor := Value;
    FGrid.InternalInvalidate;
  end;
end;

{ TntvEdit }

constructor TntvEdit.Create(AOwner: TComponent);
begin
  inherited;
  BorderStyle := bsNone;
end;

destructor TntvEdit.Destroy;
begin
  inherited Destroy;
end;

function TntvStandardColumn.CreateEdit: TControl;
var
  aEdit: TntvEdit;
begin
  aEdit := TntvEdit.Create(nil);
  aEdit.Master := Self;
  aEdit.Parent := Grid;
  aEdit.BiDiMode := BidiMode;
  Result := aEdit;
end;

procedure TntvStandardColumn.HideEdit;
begin
  FEditControl.Hide;
end;

procedure TntvStandardColumn.ShowEdit(vChar: Char);
var
  aEdit: TntvEdit;
  aRect: TRect;
  aColor: TColor;
begin
  GetCurrentColor(ActiveRow, aColor);
  GetTextArea(ActiveRow, aRect);
  aEdit := (FEditControl as TntvEdit);
  aEdit.Color := aColor;
  aEdit.Font.Color := Grid.Font.Color;
  aEdit.BoundsRect := aRect;
  aEdit.AdjustSize;
  aEdit.Top := aEdit.Top + (aRect.Height - aEdit.Height) div 2; //centering it
  FEditControl.Show;
  aEdit.SetFocus;
end;

function TntvStandardColumn.GetEditText: String;
begin
  Result := (FEditControl as TntvEdit).Text;
end;

function TntvStandardColumn.IsFocused(Handle: THandle = 0): Boolean;
begin
  Result := (FEditControl as TntvEdit).Handle = Handle;
end;

procedure TntvStandardColumn.SetEditText(const Value: String);
begin
  (FEditControl as TntvEdit).Text := Value;
end;

procedure TntvEdit.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  if Master <> nil then
    Message.Result := DLGC_WANTCHARS or DLGC_WANTARROWS;
end;

procedure TntvEdit.DoExit;
begin
  inherited;
end;

procedure TntvEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Shift = [] then
  begin
    case Key of
      VK_RETURN: Master.MasterAction(mactAccept);
      VK_ESCAPE: Master.MasterAction(mactCancel);
      VK_DOWN: Master.MasterAction(mactDown);
      VK_UP: Master.MasterAction(mactUp);
      //      VK_LEFT:Master.MasterAction(mactLeft);
      //      VK_RIGHT:Master.MasterAction(mactRight);
    end;
  end;
end;

procedure TntvEdit.KeyPress(var Key: Char);
begin
  inherited;
  case Key of
    #13, #27: Key := #0;
  end;
end;

procedure TntvEdit.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  Master.MasterAction(mactExit);
end;

{ TntvStandardColumn }

procedure TntvCustomGrid.UpdateScrollBar;
var
  aMax: Integer;
  aScrollInfo: TScrollInfo;
begin
  if Rows.Updating or (ScrollBars = ssNone) or not HandleAllocated or not Showing then
    Exit;
  if FScrollBars in [ssHorizontal, ssBoth] then
  begin
    aScrollInfo.cbSize := SizeOf(aScrollInfo);
    aScrollInfo.fMask := SIF_RANGE or SIF_PAGE or SIF_POS or SIF_DISABLENOSCROLL;
    aMax := GetMaxSideCol - SettledCols;
    aScrollInfo.nMin := 0;
    aScrollInfo.nMax := aMax;
    aScrollInfo.nPage := 1;
    if UseRightToLeftAlignment then
      aScrollInfo.nPos := aMax - (SideCol - SettledCols)
    else
      aScrollInfo.nPos := (SideCol - SettledCols);
    SetScrollInfo(Handle, SB_HORZ, aScrollInfo, True);
  end;

  if FScrollBars in [ssVertical, ssBoth] then
  begin
    aScrollInfo.cbSize := SizeOf(aScrollInfo);
    aScrollInfo.fMask := SIF_RANGE or SIF_PAGE or SIF_POS or SIF_DISABLENOSCROLL;
    aMax := Count;
    if aMax <= MAX_SCROLL then
    begin
      aScrollInfo.nMin := 0;
      aScrollInfo.nMax := Max(0, aMax - 1);
      aScrollInfo.nPage := GetCompletedRows;
      aScrollInfo.nPos := TopRow;
    end
    else
    begin
      aScrollInfo.nMin := 0;
      aScrollInfo.nMax := MAX_SCROLL;
      aScrollInfo.nPage := MulDiv(MAX_SCROLL, GetCompletedRows, aMax);
      aScrollInfo.nPos := MulDiv(MAX_SCROLL, TopRow, aMax);
    end;
    SetScrollInfo(Handle, SB_VERT, aScrollInfo, True);
  end;
end;

function TntvCustomGrid.MovingRect: TRect;
var
  i, w: Integer;
begin
  w := 0;
  for i := 0 to SettledCols - 1 do
  begin
    if i >= FVisibleColumns.Count then
      break;
    w := w + FVisibleColumns[i].Width;
  end;
  Result := VirtualClient;
  Result.Left := Result.Left + w;
end;

function TntvCustomGrid.VirtualWidth: Integer;
var
  vrtRect: TRect;
begin
  vrtRect := VirtualClient;
  Result := vrtRect.Right - vrtRect.Left;
end;

procedure TntvCustomGrid.SetSettledCols(const Value: Integer);
begin
  if FSettledCols <> Value then
  begin
    if Value < FixedCols then
      FSettledCols := FixedCols
    else
      FSettledCols := Value;
    if FSideCol < SettledCols then
      FSideCol := SettledCols;
  end;
end;

procedure TntvCustomGrid.SetFooter(const Value: Boolean);
begin
  if FFooter <> Value then
  begin
    FFooter := Value;
    InternalInvalidate;
  end;
end;

procedure TntvCustomGrid.SetFringe(const Value: Boolean);
begin
  if FFringe <> Value then
  begin
    FFringe := Value;
    InternalInvalidate;
  end;
end;

procedure TntvCustomGrid.SetHeader(const Value: Boolean);
begin
  if FHeader <> Value then
  begin
    FHeader := Value;
    InternalInvalidate;
  end;
end;

procedure TntvCustomGrid.SetIndicator(const Value: Boolean);
begin
  if FIndicator <> Value then
  begin
    FIndicator := Value;
    InternalInvalidate;
  end;
end;

procedure TntvCustomGrid.DrawFixed(Canvas: TCanvas; vRect: TRect; S: String; vDrawState: TntvCellDrawState);
begin
  Canvas.Pen.Color := clWindowFrame;
  if not (csdDown in vDrawState) then
  begin
    if UseRightToLeftAlignment then
    begin
      Canvas.Pen.Color := clBtnShadow;
      Canvas.MoveTo(vRect.Left, vRect.Bottom - 1);
      Canvas.LineTo(vRect.Right, vRect.Bottom - 1);
      if csdFirstCell in vDrawState then
      begin
        Canvas.MoveTo(vRect.Right - 1, vRect.Top);
        Canvas.LineTo(vRect.Right - 1, vRect.Bottom + 1);
      end
      else
      begin
        Canvas.MoveTo(vRect.Right - 1, vRect.Top + 2);
        Canvas.LineTo(vRect.Right - 1, vRect.Bottom - 2);
      end;

      if not (csdOpened in vDrawState) then
      begin
        if not ((csdLastCell in vDrawState) and not FullHeader) then
        begin
          Canvas.Pen.Color := clBtnFace;
          Canvas.MoveTo(vRect.Left, vRect.Top);
          Canvas.LineTo(vRect.Left, vRect.Bottom - 1);
        end;
      end;
      Canvas.Pen.Color := $00EEEEEE;
      Canvas.MoveTo(vRect.Left, vRect.Top);
      Canvas.LineTo(vRect.Right, vRect.Top);
      if not (csdOpened in vDrawState) then
      begin
        if (csdLastCell in vDrawState) and not FullHeader then
        begin
          Canvas.MoveTo(vRect.Left, vRect.Top);
          Canvas.LineTo(vRect.Left, vRect.Bottom);
        end;
        begin
          Canvas.MoveTo(vRect.Left, vRect.Top + 2);
          Canvas.LineTo(vRect.Left, vRect.Bottom - 2);
        end;
      end;
    end
    else
    begin
      Canvas.Pen.Color := clBtnShadow;
      Canvas.MoveTo(vRect.Left, vRect.Bottom - 1);
      Canvas.LineTo(vRect.Right, vRect.Bottom - 1);
      if not (csdOpened in vDrawState) then
      begin
        if (csdLastCell in vDrawState) and not FullHeader then
        begin
          Canvas.MoveTo(vRect.Right - 1, vRect.Bottom - 1);
          Canvas.LineTo(vRect.Right - 1, vRect.Top);
        end
        else
        begin
          Canvas.MoveTo(vRect.Right - 1, vRect.Bottom - 3);
          Canvas.LineTo(vRect.Right - 1, vRect.Top + 1);
        end;
      end;

      if not (csdFirstCell in vDrawState) then
      begin
        Canvas.Pen.Color := clBtnFace;
        Canvas.MoveTo(vRect.Left, vRect.Top);
        Canvas.LineTo(vRect.Left, vRect.Bottom - 1);
      end;
      Canvas.Pen.Color := $00EEEEEE;
      Canvas.MoveTo(vRect.Right - 1, vRect.Top);
      Canvas.LineTo(vRect.Left - 1, vRect.Top);
      if csdFirstCell in vDrawState then
      begin
        Canvas.MoveTo(vRect.Left, vRect.Top);
        Canvas.LineTo(vRect.Left, vRect.Bottom - 1);
      end
      else
      begin
        Canvas.MoveTo(vRect.Left, vRect.Top + 2);
        Canvas.LineTo(vRect.Left, vRect.Bottom - 2);
      end;
    end;
  end
  else
  begin
    Canvas.Pen.Color := clLtGray;
    Canvas.MoveTo(vRect.Left, vRect.Bottom - 1);
    Canvas.LineTo(vRect.Right - 1, vRect.Bottom - 1);
    Canvas.LineTo(vRect.Right - 1, vRect.Top);
    Canvas.Pen.Color := clBtnShadow;
    Canvas.LineTo(vRect.Left, vRect.Top);
    Canvas.LineTo(vRect.Left, vRect.Bottom - 1);
  end;
  if s <> '' then
  begin
    InflateRect(vRect, -1, -1);
    if (csdDown in vDrawState) then
    begin
      vRect.Left := vRect.Left + 1;
      vRect.Top := vRect.Top + 1;
    end;

    DrawString(Canvas, s, vRect, GetTextStyle(True), True);
  end;
end;

procedure TntvCustomGrid.RefreshCell(vRow: Integer; vCol: Integer; Area: TntvGridArea);
var
  aRect: TRect;
begin
  if GetCellRect(vRow, vCol, Area, ARect) then
    InvalidateRect(Handle, @ARect, False);
end;

function TntvCustomGrid.GetColRect(vCol: Integer; out vRect: TRect): Boolean;
var
  X: Integer;
  aWidth, aCol, fCol: Integer;
begin
  if (vCol >= 0) and (vCol < FVisibleColumns.Count) then
  begin
    vRect := ClientRect;
    fCol := 0;
    X := VirtualClient.Left;
    aCol := GetVirtualCol(0);
    aWidth := FVisibleColumns[aCol].Width;
    while (X < VirtualClient.Right) and (aCol < vCol) do
    begin
      X := X + aWidth;
      fCol := fCol + 1;
      aCol := GetVirtualCol(fCol);
      aWidth := FVisibleColumns[aCol].Width;
    end;
    vRect.Left := X;
    vRect.Right := vRect.Left + aWidth;
    Result := (aCol = vCol);
    if UseRightToLeftAlignment then
      vRect := FlipRect(ClientRect, vRect);
  end
  else
    Result := False;
end;

procedure TntvCustomGrid.RefreshHeader;
var
  aRect: TRect;
begin
  if Header then
  begin
    aRect := ClientRect;
    aRect.Bottom := RowsClient.Top;
    InvalidateRect(Handle, @aRect, False);
  end;
end;

function TntvCustomGrid.GetCellRect(vRow: Integer; vCol: Integer; vArea: TntvGridArea; var vRect: TRect): Boolean;
begin
  Result := False;
  case vArea of
    garHeader:
      if Header then
      begin
        if GetColRect(vCol, vRect) then
        begin
          vRect.Top := 0;
          vRect.Bottom := VirtualClient.Top;
          Result := True;
        end;
      end;
    garFooter:
      if Footer then
      begin
        vRect := VirtualClient;
        if GetColRect(vCol, vRect) then
        begin
          vRect.Top := VirtualClient.Bottom;
          vRect.Bottom := ClientRect.Bottom;
          Result := True;
        end;
      end;
    garIndicator:
      if Indicator then
      begin
        vRect := VirtualClient;
        if GetRowRect(vRow, vRect) then
        begin
          vRect.Left := 0;
          vRect.Right := VirtualClient.Left;
          if UseRightToLeftAlignment then
            vRect := FlipRect(ClientRect, vRect);
          Result := True;
        end;
      end;
    garFringe:
      if Fringe then
      begin
        if GetRowRect(vRow, vRect) then
        begin
          vRect.Left := VirtualClient.Right;
          vRect.Right := ClientRect.Right;
          if UseRightToLeftAlignment then
            vRect := FlipRect(ClientRect, vRect);
          Result := True;
        end;
      end;
    else
      Result := GetCellRect(vRow, vCol, vRect);
  end;
end;

procedure TntvColumn.NeedEdit;
begin
  if FEditControl = nil then
    FEditControl := CreateEdit;
end;

procedure TntvColumn.SendChar(vChar: Char);
begin
end;

procedure TntvStandardColumn.SendChar(vChar: Char);
begin
  inherited;
  PostMessage((FEditControl as TntvEdit).Handle, WM_CHAR, Word(vChar), 0);
end;

function TntvCustomGrid.CheckEmpty(vRow: Integer): Boolean;
begin
  Result := FRows.CheckEmpty(vRow);
end;

function TntvCustomGrid.FindRowNext(vStartRow: Integer; vInCol: Integer; vText: String; vPartial: Boolean): Integer;
var
  i: Integer;
  aCell: TntvCell;
begin
  Result := -1;
  for i := vStartRow to Count - 1 do
  begin
    aCell := FRows.SafeGet(i, vInCol);
    if (aCell <> nil) and (Pos(vText, aCell.Text) > 0) then
    begin
      Result := i;
      exit;
    end;
  end;
end;

function TntvCustomGrid.GetCurCell: TntvCell;
begin
  Result := FRows[CurRow][CurCol];
end;

procedure TntvColumn.MasterAction(vMove: TntvMasterAction);
begin
  if not FMasterActionLock then
    try
      FMasterActionLock := True;
      case vMove of
        mactLeft:
        begin
          Grid.CloseEdit;
          Grid.SetFocus;
          Grid.IncCurCol;
        end;
        mactRight:
        begin
          Grid.CloseEdit;
          Grid.SetFocus;
          Grid.DecCurRow;
        end;
        mactUp:
        begin
          Grid.CloseEdit;
          Grid.SetFocus;
          Grid.DecCurRow;
        end;
        mactDown:
        begin
          Grid.CloseEdit;
          Grid.SetFocus;
          Grid.IncCurRow;
        end;
        mactExit:
          Grid.CloseEdit;
        mactCancel:
        begin
          Grid.CancelEdit;
          Grid.SetFocus;
        end;
        mactAccept:
        begin
          Grid.CloseEdit;
          Grid.SetFocus;
          Grid.MoveCurrent;
        end;
      end;
    finally
      FMasterActionLock := False;
    end;
end;

procedure TntvCustomGrid.MoveBottom;
var
  c: Integer;
begin
  c := FRows.Count - 1;
  if (CurRow < c) then
  begin
    FRows.Move(CurRow, c);
    Refresh;
    Rows[CurRow].Modified := True;
    Rows[c].Modified := True;
    CurRow := c;
    Modified := True;
  end;
end;

procedure TntvCustomGrid.MoveCurrent;
var
  aRow, aCol: Integer;
begin
  MoveCurrentRowCol(aRow, aCol);
  SetCurCell(aRow, aCol, False, False);
end;

procedure TntvCustomGrid.AfterEdit(AColumn: TntvColumn; vRow: Integer);
begin
  DoAfterEdit(AColumn, vRow);
  {  if not RowRefresh then
    RefreshRow(vRow);}
  DoChanged;
end;

procedure TntvCustomGrid.AfterPaste(AColumn: TntvColumn; vRow: Integer);
begin
end;

procedure TntvCustomGrid.AfterPasteRow(vRow: Integer);
begin
end;

constructor TntvColumn.Create(vColumns: TntvColumns; vTitle: String; vName: String; vID: Integer; vEnabled: Boolean);
begin
  inherited Create;
  if vName = '' then
    FName := Copy(ClassName, 2, MaxInt)
  else
    FName := vName;
  Info.Title := vTitle;
  FColumns := vColumns;
  FImageIndex := -1;
  FParentBiDiMode := True;
  FShowButton := False;
  FMouseInHeader := False;
  FEnabled := vEnabled;

  Info.Visible := True;
  Info.Store := True;
  Info.StoreCol := -1;
  Info.ID := vID;
  Info.Width := sColWidth;

  FVisibleIndex := -1;

  FColumns.Add(Self);
end;

procedure TntvColumn.SelectAll;
begin
end;

procedure TntvStandardColumn.SelectAll;
begin
  (FEditControl as TntvEdit).SelectAll;
end;

procedure TntvColumn.SelectPos(X: Integer);
begin
end;

procedure TntvStandardColumn.SelectPos(X: Integer);
var
  c: Integer;
begin
  c := SendMessage(Edit.Handle, EM_CHARFROMPOS, 0, X - Edit.Left);
  Edit.SelLength := 0;
  Edit.SelStart := c;
end;

function TntvStandardColumn.Edit: TntvEdit;
begin
  Result := (FEditControl as TntvEdit);
end;

procedure TntvColumn.SetVisible(const Value: Boolean);
begin
  if Info.Visible <> Value then
  begin
    Info.Visible := Value;
    //Changed(False);TODO
  end;
end;

{ TColumnList }

function TColumnList.Add(Item: TntvColumn): Integer;
begin
  Result := inherited Add(Item);
end;

function TColumnList.Find(const Name: String): TntvColumn;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(Items[i].Name, Name) then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TColumnList.GetItems(Index: Integer): TntvColumn;
begin
  Result := TntvColumn(inherited Items[Index]);
end;

function TntvColumn.GetCellArea(vRow: Integer; var vRect: TRect): Boolean;
begin
  Result := Grid.GetCellRect(vRow, VisibleIndex, vRect);
  if Result then
    CorrectCellRect(vRect);
end;

function TntvColumn.IsBiDiModeStored: Boolean;
begin
  Result := not ParentBiDiMode;
end;

procedure TntvColumn.SetBiDiMode(const Value: TBiDiMode);
begin
  if FBiDiMode <> Value then
  begin
    FBiDiMode := Value;
    FParentBiDiMode := False;
    BiDiModeChanged(False);
  end;
end;

procedure TntvColumn.SetParentBiDiMode(const Value: Boolean);
begin
  if FParentBiDiMode <> Value then
  begin
    FParentBiDiMode := Value;
    if (Grid <> nil) then
      ParentBiDiModeChanged;
  end;
end;

procedure TntvColumn.BiDiModeChanged(vInvalidate: Boolean);
begin
  if vInvalidate then
    Invalidate;
end;

procedure TntvColumn.ParentBiDiModeChanged;
begin
  if FParentBiDiMode then
  begin
    if Grid <> nil then
      BiDiMode := Grid.BiDiMode;
    FParentBiDiMode := True;
  end;
end;

procedure TntvColumn.Invalidate;
begin
  Grid.RefreshCol(VisibleIndex);
end;

procedure TntvColumn.InvalidateHeader;
var
  aRect: TRect;
begin
  Grid.GetCellRect(0, VisibleIndex, garHeader, aRect);
  DrawHeader(Grid.Canvas, 0, aRect, []);
  Grid.Canvas.Pen.Color := clBtnShadow;
  Grid.Canvas.MoveTo(aRect.Right - 1, aRect.Top + 1);
  Grid.Canvas.LineTo(aRect.Right - 1, aRect.Bottom - 2);
  Grid.Canvas.MoveTo(aRect.Right, aRect.Bottom - 1);
  Grid.Canvas.LineTo(aRect.Left - 1, aRect.Bottom - 1);
  Grid.Canvas.Pen.Color := clBtnFace;
  Grid.Canvas.MoveTo(aRect.Left, aRect.Bottom - 2);
  Grid.Canvas.LineTo(aRect.Left, aRect.Bottom - 3);
  Grid.Canvas.Pen.Color := $00EEEEEE;
  Grid.Canvas.LineTo(aRect.Left, aRect.Top + 1);
  Grid.Canvas.Pen.Color := clBtnFace;
  Grid.Canvas.MoveTo(aRect.Left, aRect.Top + 1);
  Grid.Canvas.LineTo(aRect.Left, aRect.Top);
  Grid.Canvas.Pen.Color := $00EEEEEE;
  Grid.Canvas.LineTo(aRect.Right, aRect.Top);
end;

function TntvColumn.UseRightToLeftAlignment: Boolean;
begin
  Result := BiDiMode = bdRightToLeft;
end;

function TntvColumn.UseRightToLeftReading: Boolean;
begin
  Result := BiDiMode <> bdLeftToRight;
end;

procedure TntvColumn.SetWidth(const Value: Integer);
begin
  if Info.Width <> Value then
  begin
    Info.Width := Value;
    Info.OrignalWidth := Value;
    if Grid <> nil then
    begin
      Grid.UpdateColumnsWidths;
      Grid.UpdateScrollBar;
      Grid.InternalInvalidate;
    end;
  end;
end;

procedure TntvCustomGrid.DoModified;
begin

end;

procedure TntvCustomGrid.CMBiDiModeChanged(var Message: TMessage);
var
  i: Integer;
begin
  inherited;
  for i := 0 to FColumns.Count - 1 do
    FColumns[i].ParentBiDiModeChanged;
end;

function TntvCustomGrid.CreateRows: TntvRows;
begin
  Result := TntvRows.Create(Self);
end;

function TntvColumn.GetReadOnly: Boolean;
begin
  Result := ReadOnly or Grid.ReadOnly;
  if not Result then
    Grid.DoIsReadOnly(Self, ActiveRow, Result);
end;

procedure TntvCustomGrid.KeyAccident(vKeyAccident: TntvKeyAction; var Resumed: Boolean);
var
  AColumn: TntvColumn;

  procedure CopyToDown;
  var
    aCell: TntvCell;
  begin
    if not ReadOnly then
      if CurRow > 0 then
      begin
        AColumn := FVisibleColumns[CurCol];
        aCell := AColumn.GetCell(CurRow - 1);
        if aCell <> nil then
          AColumn.SetInfo(CurRow, aCell.Text, aCell.Data, sSetCellFull);
        AfterEdit(AColumn, CurRow);
        IncCurRow(False);
      end;
  end;

begin
  Resumed := True;
  case vKeyAccident of
    keyaTab:
      MoveCurrent;
    keyaEdit:
      OpenEdit(goeReturn);
    keyaReturn:
      if WantReturn then
        Resumed := False
      else
        OpenEdit(goeReturn);
    keyaRepeat:
    begin
      CopyToDown;
    end;
    keyaDelete:
    begin
      if (FVisibleColumns[CurCol].GetText(CurRow) <> '') or (FVisibleColumns[CurCol].GetData(CurRow) <> 0) then
      begin
        FVisibleColumns[CurCol].SetInfo(CurRow, '', 0);
        AfterEdit(FVisibleColumns[CurCol], CurRow);
      end;
    end;
    keyaPaste:
      ClipboardPaste;
    keyaCopy:
      ClipboardCopy(True);
    keyaDown, keyaSelectDown:
      InternalSetCurRow(FCurRow + 1, vKeyAccident = keyaSelectDown);
    keyaUp, keyaSelectUp:
      InternalSetCurRow(FCurRow - 1, vKeyAccident = keyaSelectUp);
    keyaLeft, keyaSelectLeft:
    begin
      if UseRightToLeftAlignment then
        IncCurCol(vKeyAccident = keyaSelectLeft)
      else
        DecCurCol(vKeyAccident = keyaSelectLeft);
    end;
    keyaRight, keyaSelectRight:
    begin
      if UseRightToLeftAlignment then
        DecCurCol(vKeyAccident = keyaSelectRight)
      else
        IncCurCol(vKeyAccident = keyaSelectRight);
    end;
    keyaPageDown:
      InternalSetCurRow(FCurRow + GetCompletedRows, vKeyAccident = keyaSelectPageDown);
    keyaPageUp:
      InternalSetCurRow(FCurRow - GetCompletedRows, False);
    keyaHome, keyaSelectHome:
      InternalSetCurCol(FFixedCols, vKeyAccident = keyaSelectHome);
    keyaEnd, keyaSelectEnd:
      InternalSetCurCol(FVisibleColumns.Count - 1, vKeyAccident = keyaSelectEnd);
    keyaDeleteLine: TryDeleteRow(CurRow);
    keyaInsertLine: TryInsertRow(CurRow);
    keyaTop, keyaSelectTop, keyaTopPage: InternalSetCurRow(0, vKeyAccident = keyaSelectTop);
    keyaBottom, keyaSelectBottom, keyaBottomPage: InternalSetCurRow(Count - 1, vKeyAccident = keyaSelectBottom);
    keyaScrollDown:
    begin
      MoveDown;
    end;
    keyaScrollUp:
    begin
      MoveUp;
    end;
    else
      Resumed := False;
  end;
end;

procedure TntvCustomGrid.ProcessScrollTimer;
var
  Pt: TPoint;
  aArea: TntvGridArea;
  aRow, aCol: Integer;
begin
  Pt := ScreenToClient(Mouse.CursorPos);
  if PtInRect(VirtualClient, Pt) then
  begin
    if PosToCoord(Pt.X, Pt.Y, aRow, aCol, aArea) and (aArea = garNormal) then
    begin
      SetCurCell(aRow, aCol, False, False);
    end;
  end
  else
  begin
    if Pt.Y < VirtualClient.Top then
      DecCurRow(False)
    else
    if Pt.Y > VirtualClient.Bottom then
      IncCurRow(False);
  end;
end;

function TntvCustomGrid.IsCurrent(vRow: Integer; vCol: Integer): Boolean;
begin
  Result := (vCol = FCurCol) and (vRow = FCurRow);
end;

procedure TntvCustomGrid.DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  aRow: Integer;
  aCol: Integer;
  aArea: TntvGridArea;
begin
  inherited;
  if not ReadOnly and not Accept then
  begin
    if PosToCoord(x, y, aRow, aCol, aArea) and (aArea = garNormal) then
      Columns[aCol].DragOver(Source, State, aRow, Accept);
  end;
end;

procedure TntvCustomGrid.DragDrop(Source: TObject; X, Y: Integer);
var
  aRow: Integer;
  aCol: Integer;
  aArea: TntvGridArea;
begin
  inherited;
  if not ReadOnly then
  begin
    if PosToCoord(x, y, aRow, aCol, aArea) and (aArea = garNormal) then
      Columns[aCol].DragDrop(Source, aRow);
  end;
end;

procedure TntvColumn.DragDrop(Source: TObject; vRow: Integer);
begin
end;

procedure TntvColumn.DragOver(Source: TObject; State: TDragState; vRow: Integer; var Accept: Boolean);
begin
  Accept := False;
end;

function TntvColumn.SetInfo(vRow: Integer; vText: String; vData: Integer; vSetCellKind: TntvSetCells): Boolean;
begin
  if not GetReadOnly then
  begin
    InternalSetInfo(vRow, vText, vData, vSetCellKind);
    Validated;
    Result := True;
  end
  else
    Result := False;
end;


procedure TntvColumn.SetMouseInHeader(const Value: Boolean);
begin
  if FMouseInHeader <> Value then
  begin
    FMouseInHeader := Value;
    if FShowButton then
    begin
      Invalidate;
      //InvalidateHeader;
    end;
  end;
end;

procedure TntvCustomGrid.SetSettledColor(const Value: TColor);
begin
  FSettledColor := Value;
end;

{ TntvCheckColumn }

function TntvCheckColumn.CanEdit: Boolean;
begin
  Result := False;
end;

procedure TntvCheckColumn.DrawCell(Canvas: TCanvas; vRow: Integer; vRect: TRect; State: TntvCellDrawState; const vColor: TColor);
var
  aState: TCheckBoxState;
  aCell: TntvCell;
begin
  Canvas.Brush.Color := vColor;
  Canvas.FillRect(vRect);
  aCell := GetCell(vRow);
  if aCell <> nil then
  begin
    if StrToIntDef(aCell.Text, 0) <> 0 then
      aState := cbChecked
    else
      aState := cbUnchecked;
  end
  else
    aState := cbUnchecked;
  //DrawCheckBox(Canvas, vRect, aState, True);
end;

procedure TntvCheckColumn.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if not GetReadOnly then
    if (Shift = []) and (Key = vk_Space) then
    begin
      Toggle;
    end;
end;

procedure TntvCheckColumn.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if not GetReadOnly then
    Toggle;
end;

function TntvColumn.GetAsBoolean: Boolean;
begin
  Result := AsInteger <> 0;
end;

procedure TntvColumn.SetAsBoolean(const Value: Boolean);
begin
  AsInteger := Ord(Value);
end;

function TntvColumn.CanEdit: Boolean;
begin
  Result := True;
end;

function TntvColumn.GetRect(vRow: Integer; var vRect: TRect): Boolean;
begin
  Result := Grid.GetCellRect(vRow, VisibleIndex, vRect);
  if Result then
  begin
    CorrectCellRect(vRect);
  end;
end;

procedure TntvColumn.KeyDown(var KEY: Word; Shift: TShiftState);
begin

end;

procedure TntvCheckColumn.Toggle;
begin
  if not ReadOnly and Grid.CanEdit(Grid.CurRow, Grid.CurCol) then
  begin
    if not Grid.Solid or (Grid.CurRow < Grid.Count) then
    begin
      Grid.ActiveRow := Grid.CurRow; //zaher
      AsBoolean := not AsBoolean;
      Grid.AfterEdit(Self, Grid.CurRow);
    end;
  end;
end;

{ TntvImageColumn }

function TntvImageColumn.CanEdit: Boolean;
begin
  Result := False;
end;

procedure TntvImageColumn.DrawCell(Canvas: TCanvas; vRow: Integer; vRect: TRect; State: TntvCellDrawState; const vColor: TColor);
begin
  Canvas.Brush.Color := vColor;
  Canvas.FillRect(vRect);
end;

procedure TntvImageColumn.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
end;

procedure TntvImageColumn.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

procedure TntvCustomGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  aRow: Integer;
  aCol: Integer;
  aArea: TntvGridArea;
  BtnRect: TRect;
begin
  inherited;
  EndCapture;
  if mbLeft = Button then
  begin
    if PosToCoord(X, Y, aRow, aCol, aArea) and (FClkRow = aRow) and (FClkCol = aCol) and (FClkArea = aArea) then
      DoClickArea(aRow, aCol, aArea);
    if (FClkArea = garHeader) then
    begin
      if (aCol = FDownCol) and (aCol > -1) then
      begin
        if VisibleColumns[aCol].ShowButton then
        begin
          BtnRect := VisibleColumns[FDownCol].FBtnRect;
          if PtInRect(BtnRect, Point(X, Y)) then
          begin
            DoButtonClick(VisibleColumns[aCol], X, Y);
          end;
          VisibleColumns[FDownCol].Down := False;
        end;
        FDownCol := -1;
      end;
    end;
  end;
end;

procedure TntvCustomGrid.KeyPress(var Key: char);
begin
  inherited;
  if (Key > #31) and (GetKeyShiftState = []) then
  begin
    if Editing then
    begin
      if not ReadOnly and (CurrentColumn <> nil) then
      begin
        CurrentColumn.SendChar(Key);
      end;
    end
    else
      OpenEdit(goeChar, Key);
  end;
end;

procedure TntvCustomGrid.InitPopupMenu;
begin
  if CurrentColumn <> nil then
  begin
  end;
end;

procedure TntvCustomGrid.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
begin
  inherited;
  InitPopupMenu;
  Handled := True;
end;

procedure TntvColumn.CorrectCellRect(var Rect: TRect);
begin
  if (Grid.FGridLines in [glVertical, glBoth]) then
  begin
    if Grid.UseRightToLeftAlignment then
      Rect.Left := Rect.Left + 1
    else
      Rect.Right := Rect.Right - 1;
  end;
  if (Grid.FGridLines in [glHorizontal, glBoth]) then
    Rect.Bottom := Rect.Bottom - 1;
end;

function TntvCustomGrid.RowsClient: TRect;
begin
  Result := ClientRect;
  if Header then
    Result.Top := Result.Top + RowHeight;
  if Footer then
    Result.Bottom := Result.Bottom - RowHeight;
end;

procedure TntvCustomGrid.SetIndicatorWidth(const Value: Integer);
begin
  if FIndicatorWidth <> Value then
  begin
    FIndicatorWidth := Value;
    InternalInvalidate;
  end;
end;

procedure TntvCustomGrid.SetFringeWidth(const Value: Integer);
begin
  if FFringeWidth <> Value then
  begin
    FFringeWidth := Value;
    InternalInvalidate;
  end;
end;

procedure TntvCustomGrid.SetDualColor(const Value: Boolean);
begin
  if FDualColor <> Value then
  begin
    FDualColor := Value;
    InternalInvalidate;
  end;
end;

procedure TntvCustomGrid.SetChaseCell(const Value: Boolean);
begin
  if FChaseCell <> Value then
  begin
    FChaseCell := Value;
    InternalInvalidate;
  end;
end;

procedure TntvCustomGrid.SetRowNumbers(const Value: Boolean);
begin
  if FRowNumbers <> Value then
  begin
    FRowNumbers := Value;
    InternalInvalidate;
  end;
end;

procedure TntvCustomGrid.SetRowSelect(const Value: Boolean);
begin
  if FRowSelect <> Value then
  begin
    FRowSelect := Value;
    InternalInvalidate;
  end;
end;

procedure TntvCustomGrid.UpdateColumnsWidths;
var
  aColumn: TntvColumn;
  i, w, r: Integer;
  vrtRect: TRect;
begin
  vrtRect := MovingRect;
  w := vrtRect.Right - vrtRect.Left;
  i := FVisibleColumns.Count;
  aColumn := nil;
  r := w;
  if i > 0 then
  begin
    while i > SettledCols do
    begin
      Dec(i);
      if (r - FVisibleColumns[i].Info.OrignalWidth) < 0 then
      begin
        break;
      end;
      if FVisibleColumns[i].Info.AutoSize then
        aColumn := FVisibleColumns[i];
      r := r - FVisibleColumns[i].Info.OrignalWidth;
    end;
    if aColumn <> nil then
    begin
      aColumn.Info.Width := aColumn.Info.OrignalWidth + r;
    end;
  end;
end;

function TntvColumn.GetItems(Row: Integer): TntvCell;
begin
  Result := Grid.FRows[Row][StoreCol];
end;

procedure TntvCustomGrid.CheckPosition;
begin
  if (Count > 0) then
  begin
    if (FCurRow > 0) and (FCurRow >= Count) then
      FCurRow := Count - 1;
    if (FTopRow > 0) and (FTopRow >= Count) then
    begin
      FTopRow := Count - 1;
      UpdateScrollBar;
    end;
  end;
end;

function TntvCustomGrid.GetItems(Index: Integer): TntvRow;
begin
  Result := Rows[Index];
end;

procedure TntvColumn.DrawHint(Canvas: TCanvas; vRow: Integer; vRect: TRect; State: TntvCellDrawState; const vColor: TColor);
begin
  Canvas.Brush.Color := vColor;
  Canvas.FillRect(vRect);
  InflateRect(vRect, -1, -1);
  Grid.DrawString(Canvas, Hint, vRect, GetTextStyle, True);
end;

procedure TntvColumn.GetCurrentColor(vRow: Integer; var vColor: TColor);
begin
  Grid.GetColor(Self, vRow, vColor);
end;

procedure TntvCustomGrid.GetCurrentColor(Column: TntvColumn; vRow: Integer; var vColor: TColor);
begin
  if Assigned(OnGetColor) then
    OnGetColor(Self, Column, vRow, vColor);
end;

procedure TntvCustomGrid.GetColor(Column: TntvColumn; vRow: Integer; var vColor: TColor);
begin
  if Odd(vRow) and DualColor then
    vColor := OddColor
  else
    vColor := EvenColor;
  if (Column.VisibleIndex < SettledCols) and (SettledColor <> clNone) then
  begin
    vColor := MixColors(vColor, SettledColor, 200);
  end;
  if vRow < Count then
    GetCurrentColor(Column, vRow, vColor);
end;

procedure TntvCustomGrid.SetInternalActiveRow(const Value: Integer);
begin
  if Editing then
    raise EntvGridException.Create('Can not change ActiveRow in edit mode');
  FActiveRow := Value;
end;

function TntvColumn.GetActiveRow: Integer;
begin
  Result := Grid.ActiveRow;
end;

procedure TntvCustomGrid.SetActiveRow(const Value: Integer);
begin
  if FActiveRow <> Value then
  begin
    SetInternalActiveRow(Value);
    if FActiveRow > Count - 1 then
      Count := FActiveRow + 1;
  end;
end;

function TntvColumn.GetAsDateTime: TDateTime;
var
  s: String;
begin
  s := AsString;
  if (s = '') and EmptyZero then
    Result := 0
  else
    Result := StrToDate(AsString);
end;

function TntvColumn.GetAsFloat: Double;
begin
  Result := StrToFloatDef(AsString, 0);
end;

function TntvColumn.GetAsVariant: Variant;
begin
  Result := AsString;
end;

function TntvColumn.GetButtonRect(vRect: TRect): TRect;
begin
  if UseRightToLeftAlignment then
    Result := Rect(vRect.Left - 2, vRect.Top + 1, vRect.Left + 14, vRect.Top + Grid.RowHeight - 1)
  else
    Result := Rect(vRect.Right - 16, vRect.Top + 1, vRect.Right, vRect.Top + Grid.RowHeight - 1);
end;

function TntvColumn.GetGuid: String;
begin
  Result := Name;
end;

function TntvColumn.GetIsNull: Boolean;
begin
  Result := GetCell(ActiveRow) = nil;
end;

procedure TntvColumn.SetAsDateTime(const Value: TDateTime);
begin
  if Value <> 0 then
    AsString := DateToStr(Value)
  else
    AsString := '';
end;

procedure TntvColumn.SetAsFloat(const Value: Double);
begin
  if (Value = 0) and EmptyZero then
    AsString := ''
  else
    AsString := FloatToStr(Value);
  if IsTotal and Grid.LockAutoSum then
    Info.Total := Info.Total + Value;
end;

procedure TntvColumn.SetAsVariant(const Value: Variant);
begin
  AsString := Value;
end;

procedure TntvCustomGrid.ValueChanged(AColumn: TntvColumn);
begin
  if Assigned(FOnValueChanged) then
    FOnValueChanged(Self, AColumn, ActiveRow);
end;

procedure TntvColumn.ValueChanged;
begin
end;

procedure TntvColumn.RefreshRow(vRow: Integer);
begin
  Grid.RefreshRow(vRow);
end;

function TntvColumn.CreateColumnProperty(AOwner: TComponent): TntvColumnProperty;
begin
  Result := TntvColumnProperty.Create(AOwner);
  Result.Name := Name;
  Result.Visible := Visible;
  Result.Width := Width;
  Result.Caption := Caption;
end;

function TntvCustomGrid.GetColumnProperties: TntvGridProperty;
var
  i: Integer;
begin
  Result := TntvGridProperty.Create(nil);
  Result.Capacity := Capacity;
  Result.ReturnColumns := ReturnColumns;
  Result.GridLines := GridLines;
  Result.LinesColor := LinesColor;
  Result.VerticalJump := VerticalJump;
  Result.SettledColor := SettledColor;
  Result.OddColor := OddColor;
  Result.EvenColor := EvenColor;
  Result.SettledCols := SettledCols;
  Result.CurRow := CurRow;
  Result.CurCol := CurCol;

  for i := 0 to Columns.Count - 1 do
    if Columns[i].Enabled then
    begin
      Columns[i].CreateColumnProperty(Result);
    end;
end;

procedure TntvCustomGrid.SetColumnProperties(List: TntvGridProperty);
var
  i: Integer;
  aProperty: TntvColumnProperty;
  aColumn: TntvColumn;
begin
  SettledColor := List.SettledColor;
  OddColor := List.OddColor;
  EvenColor := List.EvenColor;
  SettledCols := List.SettledCols;
  ReturnColumns := List.ReturnColumns;
  Capacity := List.Capacity;
  VerticalJump := List.VerticalJump;
  GridLines := List.GridLines;
  LinesColor := List.LinesColor;
  for i := 0 to List.ComponentCount - 1 do
    if List.Components[i] is TntvColumnProperty then
    begin
      aProperty := List.Components[i] as TntvColumnProperty;
      aProperty.Index := i;
      aColumn := Columns.Find(aProperty.Name);
      if aColumn <> nil then
        TntvColumn(aColumn).SetColumnProperty(aProperty);
    end;
  ResetPosition;
  if List.CurRow >= 0 then
    CurRow := List.CurRow;
  if List.CurCol >= 0 then
    CurCol := List.CurCol;
end;

{ TntvGridProperty }

constructor TntvGridProperty.Create(AOwner: TComponent);
begin
  inherited;
  FOddColor := clWhite;
  FEvenColor := clWhite;
  FSettledColor := $00EAEAEA;
  FCapacity := 10000;
  FCurRow := -1;
  FCurCol := -1;
  FGridLines := glBoth;
  FLinesColor := $00DDDDDD;
end;

function TntvGridProperty.GetChildOwner: TComponent;
begin
  Result := Self;
end;

procedure TntvGridProperty.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  i: Integer;
begin
  for i := 0 to ComponentCount - 1 do
  begin
    Proc(Components[i]);
  end;
end;

procedure TntvGridProperty.LoadFromFile(FileName: String);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TntvGridProperty.LoadFromStream(Stream: TStream);
var
  m: TMemoryStream;
  r: TReader;
begin
  if GetClass(TntvColumnProperty.ClassName) = nil then
    RegisterClass(TntvColumnProperty);
  m := TMemoryStream.Create;
  try
    ObjectTextToBinary(Stream, m);
    m.Seek(0, soFromBeginning);
    r := TReader.Create(m, 2048);
    try
      r.ReadRootComponent(Self);
    finally
      r.Free;
    end
  finally
    m.Free;
  end;
end;

procedure TntvGridProperty.SaveToFile(FileName: String);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TntvGridProperty.SaveToStream(Stream: TStream);
var
  m: TMemoryStream;
  w: TWriter;
begin
  m := TMemoryStream.Create;
  try
    w := TWriter.Create(m, 2048);
    try
      w.WriteRootComponent(Self);
    finally
      w.Free;
    end;
    m.Seek(0, soFromBeginning);
    ObjectBinaryToText(m, Stream);
  finally
    m.Free;
  end;
end;

procedure TntvColumn.SetColumnProperty(AProperty: TntvColumnProperty);
begin
  //Index := AProperty.Index;
  Visible := AProperty.Visible;
  Width := AProperty.Width;
  Caption := AProperty.Caption;
end;

procedure TntvCustomGrid.DoIsReadOnly(Column: TntvColumn; vRow: Integer; var vReadOnly: Boolean);
begin
  vReadOnly := False;
  if Assigned(FOnIsReadOnly) then
    FOnIsReadOnly(Self, Column, vRow, vReadOnly);
end;

procedure TntvCustomGrid.ImageListChange(Sender: TObject);
begin
  InternalInvalidate;
end;

procedure TntvCustomGrid.SetImageList(Value: TImageList);
begin
  if FImageList <> Value then
  begin
    if FImageList <> nil then
      FImageList.UnRegisterChanges(FImageChangeLink);
    FImageList := Value;
    if FImageList <> nil then
    begin
      FImageList.RegisterChanges(FImageChangeLink);
      FImageList.FreeNotification(Self);
    end;
    if not (csLoading in ComponentState) then
      InternalInvalidate;
  end;
end;

procedure TntvColumn.SetImageIndex(const Value: Integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Invalidate;
  end;
end;

function TntvColumn.GetImageList: TImageList;
begin
  Result := Grid.FImageList;
end;

function TntvColumn.GetTextArea(vRow: Integer; var vRect: TRect): Boolean;
begin
  Result := GetCellArea(vRow, vRect);
  if Result and (ImageList <> nil) and (ShowImage) then
  begin
    if UseRightToLeftAlignment then
      vRect.Right := vRect.Right - ImageList.Width
    else
      vRect.Left := vRect.Left + ImageList.Width;
  end;
  InflateRect(vRect, - sCellMargin, - sCellMargin);
end;

procedure TntvCustomGrid.SetFullHeader(const Value: Boolean);
begin
  if FFullHeader <> Value then
  begin
    FFullHeader := Value;
    InternalInvalidate;
  end;
end;

procedure TntvCustomGrid.CanDeleteRow(vRow: Integer; var Accept: Boolean);
begin
  Accept := True;
end;

procedure TntvCustomGrid.RowDeleted(vRow: Integer);
begin
end;

function TntvCustomGrid.CanEdit(const Row, Col: Integer): Boolean;
begin
  Result := True;
end;

function TntvCustomGrid.CanMoveCurrent: Boolean;
var
  aRow, aCol: Integer;
begin
  MoveCurrentRowCol(aRow, aCol);
  Result := (aRow <> CurRow) or (aCol <> CurCol);
end;

procedure TntvColumn.ValidateInfo(var Text: String; var Data: Integer);
begin

end;

procedure TntvCustomGrid.CMExit(var Message: TCMExit);
begin
  inherited;
  if FAttemptCapture then
    FAttemptCapture := False;
end;

procedure TntvCustomGrid.MoveDown;
begin
  if (CurRow < FRows.Count - 1) then
  begin
    FRows.Exchange(CurRow, CurRow + 1);
    RefreshRow(CurRow);
    RefreshRow(CurRow + 1);
    Rows[CurRow].Modified := True;
    Rows[CurRow + 1].Modified := True;
    IncCurRow(False);
    Modified := True;
  end;
end;

procedure TntvCustomGrid.MoveTop;
begin
  if (FRows.Count > 1) and (CurRow > 0) then
  begin
    FRows.Move(CurRow, 0);
    Refresh;
    Rows[CurRow].Modified := True;
    Rows[0].Modified := True;
    CurRow := 0;
    Modified := True;
  end;
end;

procedure TntvCustomGrid.MoveUp;
begin
  if (FRows.Count > 1) and (CurRow > 0) then
  begin
    FRows.Exchange(CurRow, CurRow - 1);
    RefreshRow(CurRow);
    RefreshRow(CurRow - 1);
    Rows[CurRow].Modified := True;
    Rows[CurRow - 1].Modified := True;
    DecCurRow(False);
    Modified := True;
  end;
end;

procedure TntvCustomGrid.Validated(AColumn: TntvColumn);
begin
end;

procedure TntvColumn.Validated;
begin
  Grid.Validated(self);
end;

procedure TntvColumn.SetShowButton(const Value: Boolean);
begin
  if FShowButton <> Value then
  begin
    FShowButton := Value;
    Invalidate;
  end;
end;

procedure TntvColumn.SetSilentValue(const vText: String; vData: Integer);
begin
  InternalSetInfo(ActiveRow, vText, vData, [swcText, swcData, swcRefresh]);
end;

procedure TntvCustomGrid.ColChanged(OldCol, NewCol: TntvColumn);
begin
end;

function TntvCustomGrid.GetVisibleColumn(vCol: Integer): TntvColumn;
begin
  if (vCol < FVisibleColumns.Count) and (FVisibleColumns.Count > 0) then
    Result := FVisibleColumns[vCol]
  else
    Result := nil;
end;

function TntvColumns.Find(ID: Integer): TntvColumn;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if Items[i].Id = ID then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TntvColumns.Find(const Name: String): TntvColumn;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(Items[i].Name, Name) then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TntvCustomGrid.GetReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

function TntvCustomGrid.FindText(Column: TntvColumn; From: Integer; S: String): Boolean;
var
  i: Integer;
  aText: String;
begin
  Result := False;
  for i := From to Rows.Count - 1 do
  begin
    ActiveRow := i;
    aText := Column.AsString;
    if SearchBuf(PChar(aText), Length(aText), 0, 0, S) <> nil then
    begin
      CurRow := i;
      Result := True;
      break;
    end;
  end;
end;

procedure TntvColumn.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    if FEnabled then
      Visible := False;
  end;
end;

procedure TntvColumn.SetDown(const Value: Boolean);
begin
  if FDown <> Value then
  begin
    FDown := Value;
    Invalidate;
  end;
end;

procedure TntvColumn.CurRowChanged;
begin
end;

function TntvColumn.GetData(vRow: Integer): Integer;
var
  aCell: TntvCell;
begin
  aCell := GetCell(vRow);
  if aCell <> nil then
    Result := aCell.Data
  else
    Result := 0;
end;

procedure TntvCustomGrid.SortByColumn(Column: TntvColumn; SortDown: Boolean);
begin
  FSortColumn := Column;
  FSortDown := SortDown;
  BeginUpdate;
  try
    Rows.Sort(OnSortByCol);
    Refresh;
    Modified := True;
    DoChanged;
  finally
    FSortColumn := nil;
    EndUpdate;
  end;
end;

function TntvCustomGrid.OnSortByCol(Index1, Index2: Integer): Integer;

  function CompareNumber(A1, A2: Currency): Integer;
  begin
    if A1 > A2 then
      Result := 1
    else
    if A1 < A2 then
      Result := -1
    else
      Result := 0;
  end;

begin
  case FSortColumn.Kind of
    cokDate: Result := CompareNumber(StrToDate(FSortColumn.GetText(Index1)), StrToDate(FSortColumn.GetText(Index2)));
    cokTime: Result := CompareNumber(StrToTime(FSortColumn.GetText(Index1)), StrToTime(FSortColumn.GetText(Index2)));
    cokNumber: Result := CompareNumber(StrToCurr(FSortColumn.GetText(Index1)), StrToCurr(FSortColumn.GetText(Index2)));
    else
      Result := CompareText(FSortColumn.GetText(Index1), FSortColumn.GetText(Index2));
  end;
  if FSortDown then
    Result := -Result;
end;

function TntvColumn.GetText(vRow: Integer): String;
var
  aCell: TntvCell;
begin
  aCell := GetCell(vRow);
  if aCell <> nil then
    Result := aCell.Text
  else
    Result := '';
end;

procedure TntvCustomGrid.SortDownClick(Sender: TObject);
begin
  SortByColumn(CurrentColumn, True);
end;

procedure TntvCustomGrid.SortUpClick(Sender: TObject);
begin
  SortByColumn(CurrentColumn);
end;

procedure TntvCustomGrid.MoveCurrentHorizontal(var vCol: Integer);
begin
  Inc(vCol);
end;

procedure TntvCustomGrid.DrawString(vCanvas: TCanvas; vText: String; vRect: TRect; vFormat: TTextStyle; vClipping: Boolean = False);
begin
  vFormat.Clipping := vClipping;
  vCanvas.TextRect(vRect, vRect.Left, vRect.Top, vText, vFormat);
end;

procedure TntvCustomGrid.MoveCurrentRowCol(var vRow, vCol: Integer);
var
  aReturnColumns: Integer;

  function FindFirstCol: Integer;
  var
    i: Integer;
  begin
    Result := 0;
    for i := 0 to FVisibleColumns.Count - 1 do
    begin
      if not FVisibleColumns[i].ReadOnly and FVisibleColumns[i].Enabled then
      begin
        Result := i;
        break;
      end;
    end;
  end;

begin
  vCol := CurCol;
  vRow := CurRow;
  if VerticalJump then
  begin
    vRow := vRow + 1;
    if (vRow >= Capacity) then
    begin
      vCol := vCol + 1;
      vRow := 0;
      if (vCol >= FVisibleColumns.Count) then
      begin
        vCol := 0;
      end;
    end;
  end
  else
  begin
    aReturnColumns := FVisibleColumns.Count;
    if (aReturnColumns > ReturnColumns) and (ReturnColumns <> 0) then
      aReturnColumns := ReturnColumns;
    if (CurCol = aReturnColumns - 1) and (vRow < (Capacity - 1)) then
    begin
      vCol := FindFirstCol;
      vRow := vRow + 1;
    end
    else
    if vCol < aReturnColumns - 1 then
      MoveCurrentHorizontal(vCol);
  end;
end;

procedure TntvColumn.CheckInfo(var Text: String; var Data: Integer);
begin

end;

{ TntvSelectedRows }

function TntvSelectedRows.Found(const Row: Integer): Integer;
var
  Idx: Integer;
begin
  Result := -1;
  for Idx := 0 to Count - 1 do
  begin
    if Items[Idx] = Row then
    begin
      Result := Idx;
      Break;
    end;
  end;
end;

initialization
  CF_NativeGRID := RegisterClipboardFormat('NativeGRID');
finalization
  {$IFOPT D+}
    if FCellNodeCount <> 0 then
      MessageBox(0, PChar('ntvNodeCount ' + IntToStr(FCellNodeCount)), '', 0);
  {$ENDIF}
end.
