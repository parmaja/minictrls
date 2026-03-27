unit mnSynHighlighterJSON5;
{$mode objfpc}{$H+}
{**
 * NOT COMPLETED
 *
 *  This file is part of the "Mini Library"
 *
 * @url       http://www.sourceforge.net/projects/minilib
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey
 *
 *}

interface

uses
  Classes, SysUtils,
  SynEdit, SynEditTypes,
  SynEditHighlighter, mnSynHighlighterMultiProc,
  mnSQLProcessor, mnSynHighlighterLua, HTMLProcessor;

type

  { TJSON5Processor }

  TJSON5Processor = class(TCommonSynProcessor)
  private
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetEndOfLineAttribute: TSynHighlighterAttributes; override;
  public
    procedure Created; override;

    procedure SlashProc;
    procedure BlockProc;
    procedure StringBQOpenProc;

    procedure GreaterProc;
    procedure LowerProc;
    procedure DeclareProc;

    procedure Next; override;

    procedure Prepare; override;
    procedure MakeProcTable; override;
  end;

  { TSynJSON5Syn }

  TSynJSON5Syn = class(TSynMultiProcSyn)
  private
  protected
    function GetSampleSource: string; override;
  public
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure InitProcessors; override;
  published
  end;

const

  SYNS_LangJSON5 = 'JSON5';
  SYNS_FilterJSON5 = 'JSON5 Files (*.json)|*.json;(*.json5)|*.json5';

  cJSON5Sample =  `
{
  // comments
  unquoted: 'and you can quote me on that',
  singleQuotes: 'I can use "double quotes" here',
  lineBreaks: "Look, Mom! \
No \\n's!",
  hexadecimal: 0xdecaf,
  leadingDecimalPoint: .8675309, andTrailing: 8675309.,
  positiveSign: +1,
  trailingComma: 'in objects', andIn: ['arrays',],
  "backwardsCompatible": "with JSON5",
}
`;

implementation

uses
  mnUtils;

procedure TJSON5Processor.GreaterProc;
begin
  Parent.FTokenID := tkSymbol;
  Inc(Parent.Run);
  if Parent.FLine[Parent.Run] in ['=', '>'] then
    Inc(Parent.Run);
end;

procedure TJSON5Processor.LowerProc;
begin
  Parent.FTokenID := tkSymbol;
  Inc(Parent.Run);
  case Parent.FLine[Parent.Run] of
    '=': Inc(Parent.Run);
    '<':
      begin
        Inc(Parent.Run);
        if Parent.FLine[Parent.Run] = '=' then
          Inc(Parent.Run);
      end;
  end;
end;

procedure TJSON5Processor.DeclareProc;
begin
  Parent.FTokenID := tkSymbol;
  Inc(Parent.Run);
  case Parent.FLine[Parent.Run] of
    '=': Inc(Parent.Run);
    ':':
      begin
        Inc(Parent.Run);
        if Parent.FLine[Parent.Run] = '=' then
          Inc(Parent.Run);
      end;
  end;
end;

procedure TJSON5Processor.SlashProc;
begin
  Inc(Parent.Run);
  case Parent.FLine[Parent.Run] of
    '/':
      begin
        CommentSLProc;
      end;
    '*':
      begin
        Inc(Parent.Run);
        if Parent.FLine[Parent.Run] = '*' then
          DocumentMLProc
        else
          CommentMLProc;
      end;
  else
    Parent.FTokenID := tkSymbol;
  end;
end;

procedure TJSON5Processor.BlockProc;
begin
  Inc(Parent.Run);
  Parent.FTokenID := tkSymbol;
end;

procedure TJSON5Processor.StringBQOpenProc;
var
  aProcessor: string;
begin
  aProcessor := '';
  while Parent.FLine[Parent.Run] in ['a'..'z', 'A'..'Z', '0'..'9', '_', '-', '.', '/', '\'] do
  begin
    aProcessor := aProcessor + Parent.FLine[Parent.Run];
    Inc(Parent.Run);
  end;
  if Parent.Processors.Switch(aProcessor, True) then
    Parent.fTokenID := tkProcessor
  else
    StringBQProc;
end;

procedure TJSON5Processor.MakeProcTable;
var
  I: Char;
begin
  inherited;
  for I := #0 to #255 do
    case I of
      '''': ProcTable[I] := @StringSQProc;
      '"': ProcTable[I] := @StringDQProc;
      '/': ProcTable[I] := @SlashProc;
      '{': ProcTable[I] := @BlockProc;
      '>': ProcTable[I] := @GreaterProc;
      '<': ProcTable[I] := @LowerProc;
      ':': ProcTable[I] := @DeclareProc;
      '0'..'9':
        ProcTable[I] := @NumberProc;
    //else
      'A'..'Z', 'a'..'z', '_':
        ProcTable[I] := @IdentProc;
    end;
end;

procedure TJSON5Processor.Next;
begin
  Parent.FTokenPos := Parent.Run;
  if (Parent.FLine[Parent.Run] in [#0, #10, #13]) then
    ProcTable[Parent.FLine[Parent.Run]]
  else case Range of
    rscComment:
      CommentMLProc;
    rscDocument:
      DocumentMLProc;
    rscStringBQ:
      StringProc;
    rscStringSQ, rscStringDQ:
      StringProc;
  else
    if ScanMatch(ProcessorChar) then
    begin
      if Parent.Processors.MainProcessor = Self then
        StringBQOpenProc
      else
      begin
        Parent.Processors.Switch(Parent.Processors.MainProcessor);
        //Inc(Parent.Run);
        Parent.FTokenID := tkProcessor;
      end;
    end
    else
      CallProcTable;
  end;
end;

procedure TJSON5Processor.Prepare;
begin
  inherited;
//  EnumerateKeywords(Ord(tkKeyword), sJSON5Keywords, TSynValidStringChars, @DoAddKeyword);
//  EnumerateKeywords(Ord(tkFunction), sJSON5Functions, TSynValidStringChars, @DoAddKeyword);
  SetRange(rscUnknown);
end;

function TJSON5Processor.GetEndOfLineAttribute: TSynHighlighterAttributes;
begin
  if (Range = rscDocument) or (LastRange = rscDocument) then
    Result := Parent.DocumentAttri
  else
    Result := inherited GetEndOfLineAttribute;
end;

procedure TJSON5Processor.Created;
begin
  inherited Created;
  CloseSpecialComment := '';
  CaseSensitive := False;
end;

function TJSON5Processor.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

{ TSynJSON5Syn }

constructor TSynJSON5Syn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultFilter := SYNS_FilterJSON5;
end;

procedure TSynJSON5Syn.InitProcessors;
begin
  inherited;
  Processors.Add(TJSON5Processor.Create(Self, 'JSON5', '`'));
  Processors.Add(TSQLProcessor.Create(Self, 'SQL', '`'));
  Processors.Add(THTMLProcessor.Create(Self, 'HTML', '`'));
  Processors.Add(TLUAProcessor.Create(Self, 'LUA', '`'));

  Processors.MainProcessor := Processors.Find(TJSON5Processor);
end;

class function TSynJSON5Syn.GetLanguageName: string;
begin
  Result := SYNS_LangJSON5;
end;

function TSynJSON5Syn.GetSampleSource: string;
begin
  Result := cJSON5Sample;
end;

initialization
  RegisterPlaceableHighlighter(TSynJSON5Syn);
finalization
end.

