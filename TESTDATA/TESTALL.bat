@echo off
D:
CD D:\
del /q NMA.COM > NUL
del /q NMA.EXE > NUL
echo Trying to compile COM
CALL TESTDATA\COM.BAT NMA
echo Trying to compile EXE
CALL TESTDATA\EXE.BAT NMA

CD D:\TESTS
TASM cmp.asm > NUL
TLINK cmp > NUL
CD D:\TESTDATA
del /q NMA.COM > NUL
del /q NMA.EXE > NUL

COPY ..\TESTS\CMP.EXE . > NUL
COPY D:\NMA.COM . > NUL
COPY D:\NMA.EXE . > NUL
DEL D:\RESULT.TXT > NUL

echo Testing...
echo ================================================

echo INPUT01
CALL TESTONE INPUT01
echo INPUT02
CALL TESTONE INPUT02
echo INPUT03
CALL TESTONE INPUT03
echo EDGE01
CALL TESTONE EDGE01
echo EDGE02
CALL TESTONE EDGE02
echo ================================================

:end
del NMA.COM
del NMA.EXE
del CMP.EXE
CD D:\
type D:\RESULT.TXT
