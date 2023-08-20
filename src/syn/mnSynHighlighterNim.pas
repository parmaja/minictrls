unit mnSynHighlighterNim;
{$mode objfpc}{$H+}
{**
 *
 *  This file is part of the "Mini Library"
 *
 * @url       http://www.sourceforge.net/projects/minilib
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey

 https://github.com/jangko/nppnim/blob/master/nppnim.nim
 https://github.com/saem/vscode-nim/blob/main/snippets/nim.json

 *}

interface

uses
  Classes, SysUtils,
  SynEdit, SynEditTypes,
  SynEditHighlighter, SynHighlighterHashEntries, mnSynHighlighterMultiProc;

type

  { TNimProcessor }

  TNimProcessor = class(TCommonSynProcessor)
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

    procedure GreaterProc;
    procedure LowerProc;

    procedure Next; override;

    procedure Prepare; override;
    procedure MakeProcTable; override;
  end;

  { TmnSynNimSyn }

  TmnSynNimSyn = class(TSynMultiProcSyn)
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

  SYNS_LangNim = 'Nim';
  SYNS_FilterNim = 'Nim Lang Files (*.Nim)|*.Nim';

  cNimSample =
      '#defines function'#13#10+
      'func fact(n: int)'#13#10+
      '  return @[10]'#13#10+
      ''#13#10;

{$INCLUDE 'NimKeywords.inc'}

implementation

uses
  mnUtils;

procedure TNimProcessor.GreaterProc;
begin
  Parent.FTokenID := tkSymbol;
  Inc(Parent.Run);
  if Parent.Peek in ['=', '>'] then
    Inc(Parent.Run);
end;

procedure TNimProcessor.LowerProc;
begin
  Parent.FTokenID := tkSymbol;
  Inc(Parent.Run);
  case Parent.Peek of
    '=':
		  Inc(Parent.Run);
    '<':
      begin
        Inc(Parent.Run);
        if Parent.Peek = '=' then
          Inc(Parent.Run);
      end;
  end;
end;

procedure TNimProcessor.SharpProc;
begin
  Inc(Parent.Run);
  case Parent.Peek of
    '[':
      CommentMLProc;
    '#':
      begin
        if Parent.Peek(1) = '[' then
        begin
          Inc(Parent.Run);
          SpecialDocumentMLProc
        end
        else
          DocumentSLProc
      end;
  else
    CommentSLProc;
  end;
end;

procedure TNimProcessor.DQProc;
begin
  SetRange(rscStringDQ);
  Inc(Parent.Run);
  case Parent.Peek of
    '"':
      begin
        if Parent.Peek(1) = '"' then
        begin
          Inc(Parent.Run);
          SpecialStringProc
        end
        else
          StringProc;
      end;
  else
    StringProc;
	end;
end;

procedure TNimProcessor.MakeProcTable;
var
  I: Char;
begin
  inherited;
  for I := #0 to #255 do
    case I of
      '?': ProcTable[I] := @QuestionProc;
      '''': ProcTable[I] := @StringSQProc;
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

procedure TNimProcessor.QuestionProc;
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

procedure TNimProcessor.DirectiveProc;
begin
  Parent.FTokenID := tkProcessor;
  WordProc;
end;

procedure TNimProcessor.Next;
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

procedure TNimProcessor.Prepare;
begin
  inherited;
  EnumerateKeywords(Ord(tkKeyword), sNimKeywords, TSynValidStringChars, @DoAddKeyword);
  EnumerateKeywords(Ord(tkType), sNimTypes, TSynValidStringChars, @DoAddKeyword);
  EnumerateKeywords(Ord(tkValue), sNimValues, TSynValidStringChars, @DoAddKeyword);
  EnumerateKeywords(Ord(tkFunction), sNimFunctions, TSynValidStringChars, @DoAddKeyword);
  SetRange(rscUnknown);
end;

function TNimProcessor.GetEndOfLineAttribute: TSynHighlighterAttributes;
begin
  if (Range = rscDocument) or (LastRange = rscDocument) then
    Result := Parent.DocumentAttri
  else
    Result := inherited GetEndOfLineAttribute;
end;

procedure TNimProcessor.Created;
begin
  inherited Created;
  CloseSpecialString := '"""';
  CloseComment := ']#';
  CloseSpecialDocument := ']##';
end;

function TNimProcessor.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars + ['.'];
end;

constructor TmnSynNimSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultFilter := SYNS_FilterNim;
end;

procedure TmnSynNimSyn.InitProcessors;
begin
  inherited;
  Processors.Add(TNimProcessor.Create(Self, 'Nim'));

  Processors.MainProcessor := 'Nim';
  Processors.DefaultProcessor := 'Nim';
end;

class function TmnSynNimSyn.GetLanguageName: string;
begin
  Result := 'Nim';
end;

function TmnSynNimSyn.GetSampleSource: string;
begin
  Result := cNimSample;
end;

end.
