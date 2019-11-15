unit ntvCtrls; 

{$mode objfpc}{$H+}

interface

uses
  Classes, Messages, Controls, ExtCtrls, SysUtils, Math, Contnrs, Graphics, Forms,
  LCLType, LCLIntf, LMessages, LCLProc,
  ntvUtils;

type

  { TntvCustomControl }

  TntvCustomControl= class(TCustomControl)
  protected
    procedure InvalidateRect(ARect : TRect; Erase : Boolean); virtual; //this should be in Lazarus TCustomControl
  end;

implementation

{ TntvCustomControl }

procedure TntvCustomControl.InvalidateRect(ARect: TRect; Erase: Boolean);
begin
  LCLIntf.InvalidateRect(Handle, @ARect, Erase);
end;

end.

