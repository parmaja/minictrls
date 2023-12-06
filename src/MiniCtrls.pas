{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit MiniCtrls;

{$warn 5023 off : no warning about unused units}
interface

uses
  PHPUtils, GUIMsgBox, mnFonts, ColorUtils, mnSynUtils, PHPProcessor, 
  HTMLProcessor, mnSynHighlighterXHTML, mnSynHighlighterStdSQL, 
  mnSynHighlighterApache, mnSynHighlighterConfig, mnSynHighlighterCpp, 
  mnSynHighlighterD, mnSynHighlighterFirebird, mnSynHighlighterLua, 
  mnSynHighlighterMultiProc, mnSynHighlighterGo, mnSynHighlighterSARD, 
  mnSynHighlighterLSL, mnSynHighlighterBVH, mnSynParamsHint, mnSynCompletion, 
  ntvBoard, ntvCtrls, ntvDotMatrix, ntvGrids, ntvImgBtns, ntvPageControls, 
  ntvPanels, ntvPixelGrids, ntvProgressBars, ntvRegCtrls, ntvSplitters, 
  ntvStdThemes, ntvTabs, ntvTabSets, ntvThemes, ntvUtils, CSVOptionsForms, 
  ParamsForms, mnSynHighlighterNim, mnSynHighlighterPy, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ntvRegCtrls', @ntvRegCtrls.Register);
end;

initialization
  RegisterPackage('MiniCtrls', @Register);
end.
