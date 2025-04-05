@ECHO OFF
copy OK\%2.OK . > NUL
cmp %2.OK %2.ACT %2.res
del %2.OK > NUL
if errorlevel 1 goto FAILED
echo %2 PASSED >> D:\RESULT.TXT
goto PASSED
:FAILED
copy %2.ACT %2.BAD > NUL
echo %2 FAILED, see %2.res diff >> D:\RESULT.TXT
goto END
:PASSED
del %2.res > NUL
:END