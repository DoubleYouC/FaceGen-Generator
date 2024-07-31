@echo off

SETLOCAL ENABLEEXTENSIONS
SET batdir=%~dp0

set scriptName=FO4Edit\Edit^ Scripts\FaceGen Generator.pas
set script="%batdir%%scriptName%"

WHERE /Q reg.exe
FOR /F "tokens=2* skip=2" %%a in ('reg.exe query "HKCR\FO4Script\DefaultIcon" /v ""') do SET fo4edit=%%b
FOR /F "tokens=3* skip=2" %%a in ('reg.exe query "HKLM\SOFTWARE\Wow6432Node\Bethesda Softworks\Fallout4" /v "installed path"') do Set fo4path=%%b

if NOT EXIST %script% (
	echo.
	echo ERROR: Can't find %script%.
	echo.
	pause
	exit
) else (
	echo Found script: %script%.
)

if NOT EXIST "%fo4edit%" (
if exist "fo4edit.exe" (
    set fo4edit="fo4edit.exe"
) else if exist "xedit.exe" (
    set fo4edit="xedit.exe"
) else (
	echo.
	echo ERROR: Can't find FO4Edit ^(e.g. FO4Edit.exe or xEdit.exe^).
	echo.
	echo Set your "Start in" location to where xEdit is installed.
	echo.
	pause
	exit
)
)

echo Found FO4Edit: "%fo4edit%"

echo.
echo Starting FaceGen Generator FO4Edit script...
start "FO4Edit" /B "%fo4edit%" -FO4 -autoload -script:%script%

exit