@echo off
E:
CD E:\TESTDATA

COPY INPUTS\%1.NMA . > NUL
DEL %1.ACT > NUL
DEL %1.BAD > NUL
NMA %1.NMA > %1.ACT
CALL FC2 /B %1
DEL %1.NMA > NUL