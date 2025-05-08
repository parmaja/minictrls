unit mnSQLProcessor;
{$mode objfpc}{$H+}
{**
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
  SysUtils, Controls, Graphics,
  Classes, SynEditTypes, SynEditHighlighter,
	mnSynUtils, mnSynHighlighterMultiProc,
	SynHighlighterHashEntries;

type
  { TSardProcessor }

  TSQLProcessor = class(TCommonSynProcessor)
  private
  protected
   function GetIdentChars: TSynIdentChars; override;
   function GetEndOfLineAttribute: TSynHighlighterAttributes; override;
  public
   procedure Created; override;
   procedure QuestionProc;
   procedure SlashProc;
   procedure MinusProc;

   procedure GreaterProc;
   procedure LowerProc;
   procedure DeclareProc;
   procedure VariableProc;

   procedure Next; override;

   procedure Prepare; override;
   procedure MakeProcTable; override;
  end;

const
  // keywords
  StdSQLKeywords: string =
    'abort,add,after,all,alter,analyze,and,as,asc,attach,autoincrement,'+
    'before,begin,between,by,cascade,case,cast,check,collate,column,commit,'+
    'conflict,constraint,create,cross,current_date,current_time,current_timestamp,'+
    'database,default,deferrable,deferred,delete,desc,detach,distinct,drop,each,'+
    'else,end,escape,except,exclusive,exists,explain,fail,for,foreign,from,full,'+
    'glob,group,having,if,ignore,immediate,in,index,indexed,initially,inner,insert,'+
    'instead,intersect,into,is,isnull,join,key,left,like,limit,match,natural,not,'+
    'notnull,null,of,offset,on,or,order,outer,plan,pragma,primary,query,raise,'+
    'references,regexp,reindex,release,rename,replace,restrict,right,rollback,'+
    'row,savepoint,select,set,table,temp,temporary,then,to,transaction,trigger,'+
    'union,unique,update,using,vacuum,values,view,virtual,with,when,where';

  // functions
  StdSQLFunctions =
    'avg,count,group_concat,max,min,sum,total,'+
    'abs,changes,coalesce,ifnull,hex,last_insert_rowid,length,'+
    'load_extension,lower,ltrim,nullif,quote,random,randomblob,round,rtrim,'+
    'soundex,StdSQL_version,substr,total_changes,trim,typeof,upper,zeroblob,'+
    'date,time,datetime,julianday,strftime,split_part,SubString';

  // types
  StdSQLTypes = 'blob,char,character,decimal,double,float,boolean,real,integer,' +
    'numeric,precision,smallint,timestamp,varchar';

implementation

uses
  SynEditStrConst;

{ TStdSQLSyn }

procedure TSQLProcessor.GreaterProc;
begin
  Parent.FTokenID := tkSymbol;
  Inc(Parent.Run);
  if Parent.FLine[Parent.Run] in ['=', '>'] then
    Inc(Parent.Run);
end;

procedure TSQLProcessor.LowerProc;
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

procedure TSQLProcessor.DeclareProc;
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

procedure TSQLProcessor.SlashProc;
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

procedure TSQLProcessor.MinusProc;
begin
  Inc(Parent.Run);
  case Parent.FLine[Parent.Run] of
    '-':
      begin
        CommentSLProc;
      end;
  else
    Parent.FTokenID := tkSymbol;
  end;
end;

procedure TSQLProcessor.VariableProc;
var
  i: integer;
begin
  Parent.FTokenID := tkVariable;
  i := Parent.Run;
  repeat
    Inc(i);
  until not (IdentTable[Parent.FLine[i]]);
  Parent.Run := i;
end;

procedure TSQLProcessor.MakeProcTable;
var
  I: Char;
begin
  inherited;
  for I := #0 to #255 do
    case I of
      '''': ProcTable[I] := @StringSQProc;
      '"': ProcTable[I] := @StringDQProc;
      '-': ProcTable[I] := @MinusProc;
      '/': ProcTable[I] := @SlashProc;
      '>': ProcTable[I] := @GreaterProc;
      '<': ProcTable[I] := @LowerProc;
      ':': ProcTable[I] := @VariableProc;
      '@': ProcTable[I] := @VariableProc;
      '?': ProcTable[I] := @VariableProc;
      '0'..'9':
        ProcTable[I] := @NumberProc;
      '{', '}', '.', ',', ';', '(', ')', '[', ']', '~':
        ProcTable[I] := @SymbolProc;
      'A'..'Z', 'a'..'z', '_':
        ProcTable[I] := @IdentProc;
    end;
end;

procedure TSQLProcessor.QuestionProc;
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

procedure TSQLProcessor.Next;
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

procedure TSQLProcessor.Prepare;
begin
  inherited;
  EnumerateKeywords(Ord(tkKeyword), StdSQLKeywords, TSynValidStringChars, @DoAddKeyword);
  EnumerateKeywords(Ord(tkFunction), StdSQLFunctions, TSynValidStringChars, @DoAddKeyword);
  EnumerateKeywords(Ord(tkType), StdSQLTypes, TSynValidStringChars, @DoAddKeyword);
  SetRange(rscUnknown);
end;

function TSQLProcessor.GetEndOfLineAttribute: TSynHighlighterAttributes;
begin
  if (Range = rscDocument) or (LastRange = rscDocument) then
    Result := Parent.DocumentAttri
  else
    Result := inherited GetEndOfLineAttribute;
end;

procedure TSQLProcessor.Created;
begin
  inherited;
  CaseSensitive := False;
end;

function TSQLProcessor.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

end.
