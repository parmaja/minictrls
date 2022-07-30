unit mnSynHighlighterConfig;
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

interface

uses
  Classes, Graphics,
  SynEditTypes, SynEditHighlighter, mnSynUtils;


type
  TtkTokenKind = (tkNone, tkKey, tkSubKey, tkSection, tkText, tkComment, tkDocument, tkNull, tkNumber,
    tkSpace, tkString, tkSymbol, tkUnknown);

  TSynConfRange = (cnfrNone, cnfrKey, cnfrSubKey, cnfrValue, cnfrSection);

  { TSynConfigSyn }

  TSynConfigSyn = class(TSynCustomHighlighter)
  private
    FLine: PChar;
    FProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    FTokenPos: Integer;
    FTokenID: TtkTokenKind;
    FCommentAttri: TSynHighlighterAttributes;
    FDocumentAttri: TSynHighlighterAttributes;
    FTextAttri: TSynHighlighterAttributes;
    FSectionAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    procedure TextProc;
    procedure BracketSectionOpenProc;
    procedure BSSectionOpenProc;
    procedure LFProc;
    procedure NullProc;
    procedure CRProc;
    procedure SpaceProc;
    procedure CommentProc;
    procedure DocumentProc;
    procedure KeyProc;
    procedure EqualProc;
    procedure ParamProc;
    procedure DotProc;
    procedure NumberProc;
    procedure DQStringProc;  // ""
    procedure SQStringProc; // ''
    procedure MakeMethodTables;
  protected
    FRange: TSynConfRange;
    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource: String; override;
    function IsFilterStored: Boolean; override;
  public
    procedure SetRange(Value: Pointer); override;
    function GetRange: Pointer; override;
    procedure ResetRange; override;
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetTokenID: TtkTokenKind;
    procedure SetLine(const NewValue: String; LineNumber:Integer); override;
    function GetToken: String; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
  published
    property SpaceAttri  : TSynHighlighterAttributes read FSpaceAttri write FSpaceAttri;
    property CommentAttri: TSynHighlighterAttributes read FCommentAttri write FCommentAttri;
    property DocumentAttri: TSynHighlighterAttributes read FDocumentAttri write FDocumentAttri;
    property TextAttri   : TSynHighlighterAttributes read FTextAttri write FTextAttri;
    property SectionAttri: TSynHighlighterAttributes read FSectionAttri write FSectionAttri;
    property KeyAttri    : TSynHighlighterAttributes read FKeyAttri write FKeyAttri;
    property NumberAttri : TSynHighlighterAttributes read FNumberAttri write FNumberAttri;
    property StringAttri : TSynHighlighterAttributes read FStringAttri write FStringAttri;
    property SymbolAttri : TSynHighlighterAttributes read FSymbolAttri write FSymbolAttri;
  end;

implementation

uses
  SynEditStrConst;

procedure TSynConfigSyn.MakeMethodTables;
var
  i: Char;
begin
  for i := #0 to #255 do
    case i of
      #0      : FProcTable[i] := @NullProc;
      #10 {LF}: FProcTable[i] := @LFProc;
      #13 {CR}: FProcTable[i] := @CRProc;
      #1..#9, #11, #12, #14..#32: FProcTable[i] := @SpaceProc;
      '=' : FProcTable[i] := @EqualProc;
      '.' : FProcTable[i] := @DotProc;
      '?' : FProcTable[i] := @ParamProc;
      '"'  : FProcTable[i] := @DQStringProc;
      '''' : FProcTable[i] := @SQStringProc;
      '#' : FProcTable[i] := @DocumentProc;
      ';' : FProcTable[i] := @CommentProc;
      '[' : FProcTable[i] := @BracketSectionOpenProc;
      ']' : FProcTable[i] := @BracketSectionOpenProc;
      '\' : FProcTable[i] := @BSSectionOpenProc;
      '0'..'9': FProcTable[i] := @NumberProc;
    else
      FProcTable[i] := @TextProc;
    end;
end;

constructor TSynConfigSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment);
  FCommentAttri.Style := [];
  FCommentAttri.Foreground := clGreen;
  AddAttribute(FCommentAttri);
  FDocumentAttri := TSynHighlighterAttributes.Create('Document');
  FDocumentAttri.Style := [];
  FDocumentAttri.Foreground := clGreen;
  AddAttribute(FDocumentAttri);
  FTextAttri := TSynHighlighterAttributes.Create(SYNS_AttrText);
  AddAttribute(FTextAttri);
  FSectionAttri := TSynHighlighterAttributes.Create(SYNS_AttrSection);
  FSectionAttri.Style := [];
  AddAttribute(FSectionAttri);
  FKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrKey);
  AddAttribute(FKeyAttri);
  FNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
  AddAttribute(FNumberAttri);
  FSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(FSpaceAttri);
  FStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString);
  AddAttribute(FStringAttri);
  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
  AddAttribute(FSymbolAttri);

  SetAttributesOnChange(@DefHighlightChange);

  FDefaultFilter := SYNS_FilterINI;
  MakeMethodTables;
end;

procedure TSynConfigSyn.SetLine(const NewValue: String; LineNumber:Integer);
begin
  inherited;
  FLine := PChar(NewValue);
  Run := 0;
  FRange := cnfrNone;
  FTokenID := tkNone;
  Next;
end;

procedure TSynConfigSyn.BracketSectionOpenProc;
begin
  FTokenID := tkSection;
  inc(Run);
  while FLine[Run] <> #0 do
    case FLine[Run] of
      ']':
        begin
          inc(Run);
          break
        end;
      #10: break;
      #13: break;
    else inc(Run);
    end;
end;

procedure TSynConfigSyn.BSSectionOpenProc;
begin
  if FRange = cnfrNone then
  begin
    FTokenID := tkSection;
    inc(Run);
    while FLine[Run] <> #0 do
      case FLine[Run] of
        #10: break;
        #13: break;
      else
        inc(Run);
      end;
  end
  else
  begin
    FTokenID := tkSymbol;
    inc(Run);
  end;
end;

procedure TSynConfigSyn.CRProc;
begin
  FTokenID := tkSpace;
  Case FLine[Run + 1] of
    #10: inc(Run, 2);
  else inc(Run);
  end;
end;

procedure TSynConfigSyn.EqualProc;
begin
  if FRange = cnfrValue then
    TextProc
  else
  begin
    inc(Run);
    FTokenID := tkSymbol;
    SetRange(Pointer(cnfrValue));
  end;
end;

procedure TSynConfigSyn.ParamProc;
begin
  inc(Run);
  if FRange = cnfrValue then
    FTokenID := tkSymbol;
  SetRange(Pointer(cnfrValue));
end;

procedure TSynConfigSyn.DotProc;
begin
  if FRange = cnfrValue then
    TextProc
  else
  begin
    inc(Run);
    FTokenID := tkSymbol;
    SetRange(Pointer(cnfrSubKey));
  end;
end;

procedure TSynConfigSyn.KeyProc;
begin
  FTokenID := tkKey;
  inc(Run);
  while FLine[Run] <> #0 do
    case FLine[Run] of
      '=': break;
      '.': break;
      #10: break;
      #13: break;
    else
        inc(Run);
    end;
end;

procedure TSynConfigSyn.TextProc;
begin
  if FRange <> cnfrValue then
    KeyProc
  else
  begin
    FTokenID := tkText;
    Inc(Run);
    while (FLine[Run] in [#128..#191]) OR // continued utf8 subcode
     ((FLine[Run] <> #0) and (FProcTable[FLine[Run]] = @TextProc)) do
       Inc(Run);
  end;
end;

procedure TSynConfigSyn.LFProc;
begin
  FTokenID := tkSpace;
  inc(Run);
end;

procedure TSynConfigSyn.NullProc;
begin
  FTokenID := tkNull;
end;

procedure TSynConfigSyn.NumberProc;
begin
  if FRange <> cnfrValue then
    KeyProc
  else begin
    inc(Run);
    FTokenID := tkNumber;
    while FLine[Run] in ['0'..'9', '.', 'e', 'E'] do inc(Run);
    if FLine[Run] in ['a'..'z','A'..'Z'] then TextProc;
  end;
end;

// ;
procedure TSynConfigSyn.CommentProc;
begin
  FTokenID := tkComment;
  inc(Run);
  if (FLine[Run] <> #0) and (FLine[Run] in ['#', ';', '*']) then
    DocumentProc
  else
  while FLine[Run] <> #0 do
    case FLine[Run] of
      #10: break;
      #13: break;
    else inc(Run);
    end;
end;

// #
procedure TSynConfigSyn.DocumentProc;
begin
  FTokenID := tkDocument;
  inc(Run);
  while FLine[Run] <> #0 do
    case FLine[Run] of
      #10: break;
      #13: break;
    else inc(Run);
    end;
end;

procedure TSynConfigSyn.SpaceProc;
begin
  inc(Run);
  FTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do
    inc(Run);
end;

// ""
procedure TSynConfigSyn.DQStringProc;
begin
  if FRange in [cnfrNone, cnfrKey] then
    FTokenID := tkKey
  else if FRange = cnfrSubKey then
    FTokenID := tkSubKey
  else
    FTokenID := tkString;
  inc(Run);
  while not (FLine[Run] in [#0, #10, #13]) do
  begin
    if (FLine[Run] = '"') then
    begin
      inc(Run);
      //FTokenID := tkText;
      break;
    end;
    Inc(Run);
  end;
end;

// ''
procedure TSynConfigSyn.SQStringProc;
begin
  if FRange in [cnfrNone, cnfrKey] then
    FTokenID := tkKey
  else if FRange = cnfrSubKey then
    FTokenID := tkSubKey
  else
    FTokenID := tkString;
  inc(Run);
  while not (FLine[Run] in [#0, #10, #13]) do
  begin
    if (FLine[Run] = '''') then
    begin
      inc(Run);
      //FTokenID := tkText;
      break;
    end;
    Inc(Run);
  end;
end;

procedure TSynConfigSyn.Next;
begin
  FTokenPos := Run;
  fProcTable[fLine[Run]];
end;

function TSynConfigSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
    SYN_ATTR_NUMBER: Result := fNumberAttri;
    SYN_ATTR_DIRECTIVE: Result := FSectionAttri;
  else
    Result := nil;
  end;
end;

function TSynConfigSyn.GetEol: Boolean;
begin
  Result := fTokenId = tkNull;
end;

function TSynConfigSyn.GetToken: String;
var
  Len: LongInt;
begin
  Len := Run - FTokenPos;
  SetString(Result, (FLine + FTokenPos), Len);
end;

procedure TSynConfigSyn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenLength := Run - FTokenPos;
  TokenStart := FLine + FTokenPos;
end;

function TSynConfigSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynConfigSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkDocument: Result := FDocumentAttri;
    tkText   : Result := FTextAttri;
    tkSection: Result := FSectionAttri;
    tkKey    : Result := FKeyAttri;
    tkSubKey : Result := FKeyAttri;
    tkNumber : Result := FNumberAttri;
    tkSpace  : Result := FSpaceAttri;
    tkString : Result := FStringAttri;
    tkSymbol : Result := FSymbolAttri;
    tkUnknown: Result := FTextAttri;
    else
      Result := nil;
  end;
end;

function TSynConfigSyn.GetTokenKind: Integer;
begin
  Result := Ord(FTokenId);
end;

function TSynConfigSyn.GetTokenPos: Integer;
begin
 Result := FTokenPos;
end;

function TSynConfigSyn.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

function TSynConfigSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterINI;
end;

procedure TSynConfigSyn.SetRange(Value: Pointer);
begin
  inherited;
  FRange := TSynConfRange(PtrInt(Value));
end;

function TSynConfigSyn.GetRange: Pointer;
begin
  Result := Pointer(PtrInt(FRange));
end;

procedure TSynConfigSyn.ResetRange;
begin
  inherited;
  FRange := cnfrNone;
end;

class function TSynConfigSyn.GetLanguageName: string;
begin
  Result := 'INI';
end;

function TSynConfigSyn.GetSampleSource: String;
begin
  Result := '# Syntax highlighting'#13#10+
            '[Options]'#13#10+
            'Visible = true'+#13#10+
            'Protected = false'+#13#10+
            'Port = 80'#13#10+
            'Host = "server"'#13#10+
            ';End of INI'#13#10;
end;

initialization
  RegisterPlaceableHighlighter(TSynConfigSyn);
end.
