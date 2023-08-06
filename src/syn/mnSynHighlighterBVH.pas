unit mnSynHighlighterBVH;
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
 * https://research.cs.wisc.edu/graphics/Courses/cs-838-1999/Jeff/BVH.html
 *
 *}

interface

uses
  Classes, SysUtils,
  SynEdit, SynEditTypes,
  SynHighlighterHashEntries, SynEditHighlighter, mnSynHighlighterMultiProc;

type

  { TBVHProcessor }

  TBVHProcessor = class(TCommonSynProcessor)
  private
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetEndOfLineAttribute: TSynHighlighterAttributes; override;
  public
    procedure Created; override;
    procedure QuestionProc;
    procedure SlashProc;
    procedure BlockProc;

    procedure GreaterProc;
    procedure LowerProc;
    procedure DeclareProc;

    procedure Next; override;

    procedure Prepare; override;
    procedure MakeProcTable; override;
  end;

  { TSynDSyn }

  TSynBVHSyn = class(TSynMultiProcSyn)
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

  SYNS_LangBVH = 'BVH';
  SYNS_FilterBVH = 'BVH Files (*.BVH)|*.BVH';

  cBVHSample =  'HIERARCHY'
                +'ROOT Hips'#13
                +'{'#13
                +''#13
                +'}'#13;

const
  sBVHTypes =
    'Xposition,'+
    'Yposition,'+
    'Zposition,'+
    'Zrotation,'+
    'Yrotation,'+
    'Xrotation,'+
    //not types
    'Hips,'+
    'Neck,'+
    'Chest,'+
    'Head';

  sBVHKeywords =
    'hierarchy,'+
    'root,'+
    'offset,'+
    'channels,'+
    'end,' +
    'site,' +
    'motion,' +
    'frames,' +
    'frame,' +
    'time,' +
    'joint';

implementation

uses
  mnUtils;

procedure TBVHProcessor.GreaterProc;
begin
  Parent.FTokenID := tkSymbol;
  Inc(Parent.Run);
  if Parent.FLine[Parent.Run] in ['=', '>'] then
    Inc(Parent.Run);
end;

procedure TBVHProcessor.LowerProc;
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

procedure TBVHProcessor.DeclareProc;
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

procedure TBVHProcessor.SlashProc;
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

procedure TBVHProcessor.BlockProc;
begin
  Inc(Parent.Run);
  case Parent.FLine[Parent.Run] of
    '*': SpecialCommentMLProc;
  else
    Parent.FTokenID := tkSymbol;
  end;
end;

procedure TBVHProcessor.MakeProcTable;
var
  I: Char;
begin
  inherited;
  for I := #0 to #255 do
    case I of
      '?': ProcTable[I] := @QuestionProc;
      '''': ProcTable[I] := @StringSQProc;
      '"': ProcTable[I] := @StringDQProc;
      '`': ProcTable[I] := @StringBQProc;
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

procedure TBVHProcessor.QuestionProc;
begin
  Inc(Parent.Run);
  case Parent.FLine[Parent.Run] of
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

procedure TBVHProcessor.Next;
begin
  Parent.FTokenPos := Parent.Run;
  if (Parent.FLine[Parent.Run] in [#0, #10, #13]) then
    ProcTable[Parent.FLine[Parent.Run]]
  else case Range of
    rscComment:
    begin
      CommentMLProc;
    end;
    rscDocument:
    begin
      DocumentMLProc;
    end;
    rscStringSQ, rscStringDQ, rscStringBQ:
      StringProc;
  else
    if ProcTable[Parent.FLine[Parent.Run]] = nil then
      UnknownProc
    else
      ProcTable[Parent.FLine[Parent.Run]];
  end;
end;

procedure TBVHProcessor.Prepare;
begin
  inherited;
  EnumerateKeywords(Ord(tkKeyword), sBVHKeywords, TSynValidStringChars, @DoAddKeyword);
  EnumerateKeywords(Ord(tkType), sBVHTypes, TSynValidStringChars, @DoAddKeyword);
  SetRange(rscUnknown);
end;

function TBVHProcessor.GetEndOfLineAttribute: TSynHighlighterAttributes;
begin
  if (Range = rscDocument) or (LastRange = rscDocument) then
    Result := Parent.DocumentAttri
  else
    Result := inherited GetEndOfLineAttribute;
end;

procedure TBVHProcessor.Created;
begin
  inherited Created;
  CloseSpecialComment := '*}';
end;

function TBVHProcessor.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

constructor TSynBVHSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultFilter := SYNS_FilterBVH;
end;

procedure TSynBVHSyn.InitProcessors;
begin
  inherited;
  Processors.Add(TBVHProcessor.Create(Self, 'BVH'));

  Processors.MainProcessor := 'BVH';
  Processors.DefaultProcessor := 'BVH';
end;

class function TSynBVHSyn.GetLanguageName: string;
begin
  Result := SYNS_LangBVH;
end;

function TSynBVHSyn.GetSampleSource: string;
begin
  Result := cBVHSample;
end;

initialization
  RegisterPlaceableHighlighter(TSynBVHSyn);
finalization
end.
