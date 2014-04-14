@echo off
echo.
echo  +-----------------------+
echo  + DELETE ... OLD COPIES +
echo  +   vertcon.out         +
echo  +   vertcon.prt         +
echo  +-----------------------+
echo.
      del vertcon.out
      del vertcon.prt
echo.
echo  +-----------------------+
echo  +   EXECUTE PROGRAM     +
echo  +                       +
echo  +   vertcon.exe         +
echo  +-----------------------+
echo.
      pause
      vertcon web <new_web.con >vertcon.prt
echo.
echo.
      dir vertcon.*  
echo.
echo  +-----------------------+
echo  +   LIST THE OUTPUT     +
echo  +                       +
echo  +   vertcon.prt         +
echo  +-----------------------+
echo.
      pause
      type vertcon.prt | more
echo.
echo on
