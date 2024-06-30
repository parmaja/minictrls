unit mnSynHighlighterPy;
{$mode objfpc}{$H+}
{**
 *
 *  This file is part of the "Mini Library"
 *
 * @url       http://www.sourceforge.net/projects/minilib
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey

 https://github.com/jangko/nppPy/blob/master/nppPy.Py
 https://github.com/saem/vscode-Py/blob/main/snippets/Py.json

 *}

interface

uses
  Classes, SysUtils,
  SynEdit, SynEditTypes,
  SynEditHighlighter, SynHighlighterHashEntries, mnSynHighlighterMultiProc;

type

  { TPyProcessor }

  TPyProcessor = class(TCommonSynProcessor)
  private
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetEndOfLineAttribute: TSynHighlighterAttributes; override;
    procedure Created; override;
  public
    procedure QuestionProc;
    procedure DirectiveProc;
    procedure SharpProc;
    procedure DQProc;
    procedure SQProc;

    procedure GreaterProc;
    procedure LowerProc;

    procedure Next; override;

    procedure Prepare; override;
    procedure MakeProcTable; override;
  end;

  { TmnSynPySyn }

  TmnSynPySyn = class(TSynMultiProcSyn)
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

  SYNS_LangPy = 'Py';
  SYNS_FilterPy = 'Py Lang Files (*.Py)|*.Py';

  cPySample =
      '## defines function'#13#10+
      'func fact(n: int)'#13#10+
      '    #n = n + 1'#13#10+
      '    return @[10]'#13#10+
      ''#13#10;

{$INCLUDE 'PyKeywords.inc'}

implementation

uses
  mnUtils;

procedure TPyProcessor.GreaterProc;
begin
  Parent.FTokenID := tkSymbol;
  Inc(Parent.Run);
  if Parent.Peek in ['=', '>'] then
    Inc(Parent.Run);
end;

procedure TPyProcessor.LowerProc;
begin
  Parent.FTokenID := tkSymbol;
  Inc(Parent.Run);
  case Parent.Peek of
    '=': Inc(Parent.Run);
    '<':
      begin
        Inc(Parent.Run);
        if Parent.Peek = '=' then
          Inc(Parent.Run);
      end;
  end;
end;

procedure TPyProcessor.SharpProc;
begin
  Inc(Parent.Run);
  case Parent.FLine[Parent.Run] of
    '#':
      begin
        Inc(Parent.Run);
        DocumentSLProc
      end;
  else
    CommentSLProc;
  end;
end;

procedure TPyProcessor.DQProc;
begin
  SetRange(rscStringDQ);
  Inc(Parent.Run);
  case Parent.Peek of
    '"':
      begin
        if Parent.Peek(1) = '"' then
        begin
          Inc(Parent.Run);
          SpecialStringProc;
        end
        else
          StringProc;
      end;
  else
    StringProc;
  end;
end;

procedure TPyProcessor.SQProc;
begin
  SetRange(rscStringSQ);
  Inc(Parent.Run);
  case Parent.Peek of
    '''':
      begin
        if Parent.Peek(1) = '''' then
        begin
          Inc(Parent.Run);
          SpecialDocumentMLProc;
        end
        else
          StringProc;
      end;
  else
    StringProc;
  end;
end;

procedure TPyProcessor.MakeProcTable;
var
  I: Char;
begin
  inherited;
  for I := #0 to #255 do
    case I of
      '?': ProcTable[I] := @QuestionProc;
      '''': ProcTable[I] := @SQProc;
      '"': ProcTable[I] := @DQProc;
      '#': ProcTable[I] := @SharpProc;
      '>': ProcTable[I] := @GreaterProc;
      '<': ProcTable[I] := @LowerProc;
      '0'..'9':
        ProcTable[I] := @NumberProc;
      'A'..'Z', 'a'..'z', '_':
        ProcTable[I] := @IdentProc;
    end;
end;

procedure TPyProcessor.QuestionProc;
begin
  Inc(Parent.Run);
  case Parent.Peek of
    '>':
      begin
        Parent.Processors.Switch(Parent.Processors.MainProcessor);
        Inc(Parent.Run);
        Parent.FTokenID := tkProcessor;
      end
  else
    Parent.FTokenID := tkSymbol;
  end;
end;

procedure TPyProcessor.DirectiveProc;
begin
  Parent.FTokenID := tkProcessor;
  WordProc;
end;

procedure TPyProcessor.Next;
begin
  Parent.FTokenPos := Parent.Run;
  if (Parent.Peek in [#0, #10, #13]) then
    ProcTable[Parent.Peek]
  else case Range of
    rscComment:
      CommentMLProc;
    rscDocument:
      DocumentMLProc;
    rscSpecialComment:
      SpecialCommentMLProc;
    rscSpecialDocument:
      SpecialDocumentMLProc;
    rscStringSQ, rscStringDQ, rscStringBQ:
      StringProc;
    rscSpecialString:
      SpecialStringProc;
  else
    if ProcTable[Parent.Peek] = nil then
      UnknownProc
    else
      ProcTable[Parent.Peek];
  end;
end;

procedure TPyProcessor.Prepare;
begin
  inherited;
  EnumerateKeywords(Ord(tkKeyword), sPyKeywords, TSynValidStringChars, @DoAddKeyword);
  EnumerateKeywords(Ord(tkType), sPyTypes, TSynValidStringChars, @DoAddKeyword);
  EnumerateKeywords(Ord(tkValue), sPyValues, TSynValidStringChars, @DoAddKeyword);
  EnumerateKeywords(Ord(tkFunction), sPyFunctions, TSynValidStringChars, @DoAddKeyword);
  SetRange(rscUnknown);
end;

function TPyProcessor.GetEndOfLineAttribute: TSynHighlighterAttributes;
begin
  if (Range = rscDocument) or (LastRange = rscDocument) then
    Result := Parent.DocumentAttri
  else
    Result := inherited GetEndOfLineAttribute;
end;

procedure TPyProcessor.Created;
begin
  inherited Created;
  CloseSpecialString := '"""';
  CloseComment := '';
  CloseSpecialDocument := '''''''';
end;

function TPyProcessor.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars + ['.'];
end;

constructor TmnSynPySyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultFilter := SYNS_FilterPy;
end;

procedure TmnSynPySyn.InitProcessors;
begin
  inherited;
  Processors.Add(TPyProcessor.Create(Self, 'Py'));

  Processors.MainProcessor := 'Py';
  Processors.DefaultProcessor := 'Py';
end;

class function TmnSynPySyn.GetLanguageName: string;
begin
  Result := 'Py';
end;

function TmnSynPySyn.GetSampleSource: string;
begin
  Result := cPySample;
end;

end.
