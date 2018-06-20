@ECHO OFF
SETLOCAL
SET year=%1
SET type=%2

IF "%type%"=="haskell" (
    ghci haskell/study/exam%year%.hs
) 
IF "%type%"=="prolog" (
    swipl prolog/study/exam%year%.pl
)