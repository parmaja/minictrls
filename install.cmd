lazbuild .\lib\MiniCtrls.lpk
lazbuild --add-package .\native\NativeLib.lpk
lazbuild -d --build-ide= 
pause
