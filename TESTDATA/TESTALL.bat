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
del /q CMP.EXE > NUL

COPY ..\TESTS\CMP.EXE . > NUL
COPY D:\NMA.COM . > NUL
COPY D:\NMA.EXE . > NUL
DEL D:\RESULT.TXT > NUL

echo Testing...
echo ================================================

CALL TESTORIG.BAT
CALL TESTSTD.BAT
CALL TESTZADV.BAT
CALL TESTCUST.BAT
echo ================================================

:end
del NMA.COM
del NMA.EXE
del CMP.EXE
CD D:\
type D:\RESULT.TXT
