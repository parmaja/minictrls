unit mnSynHighlighterStdSQL;
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
  SysUtils, Controls, Graphics, Classes,
  mnSQLProcessor, mnSynHighlighterMultiProc,
	SynEditTypes, SynEditHighlighter, mnSynUtils, SynHighlighterHashEntries;

type

  { TSynDSyn }

  TmnSynStdSQLSyn = class(TSynMultiProcSyn)
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

implementation

uses
  SynEditStrConst;

const
  SYNS_LangSard = 'SQL';
  SYNS_FilterSard = 'SQL Files (*.sql)|*.sql';

  cSQLSample = '/* SQL Example*/'#13#10 +
    #13#10 +
    'create table employees ('#13#10 +
    '        id int not null,'#13#10 +
    '        name char(30) not null,'#13#10 +
    '        primary key (id),'#13#10 +
    '        index name (name));'#13#10 +
    #13#10 +
    '// Single line comment'#13#10+
    'select name from employees'#13#10+
    'where id=?id and name="Unkown"'#13#10;

{ TmnSynStdSQLSyn }

constructor TmnSynStdSQLSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultFilter := SYNS_FilterSard;
end;

procedure TmnSynStdSQLSyn.InitProcessors;
begin
  inherited;
  Processors.Add(TSardProcessor.Create(Self, 'Sard'));

  Processors.MainProcessor := 'Sard';
  Processors.DefaultProcessor := 'Sard';
end;

class function TmnSynStdSQLSyn.GetLanguageName: string;
begin
  Result := SYNS_LangSard;
end;

function TmnSynStdSQLSyn.GetSampleSource: string;
begin
  Result := cSQLSample;
end;

initialization
  RegisterPlaceableHighlighter(TmnSynStdSQLSyn);
finalization
end.
