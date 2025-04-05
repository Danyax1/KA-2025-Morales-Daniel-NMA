D:
del /q NMA.COM > NUL
del /q NMA.EXE > NUL
echo Trying to compile COM
CALL E:\TESTDATA\COM.BAT NMA
echo Trying to compile EXE
CALL E:\TESTDATA\EXE.BAT NMA

E:
CD E:\TESTS
TASM cmp.asm > NUL
TLINK cmp > NUL
CD E:\TESTDATA
del /q NMA.COM > NUL
del /q NMA.EXE > NUL

COPY ..\TESTS\CMP.EXE . > NUL
COPY D:\NMA.COM . > NUL
COPY D:\NMA.EXE . > NUL