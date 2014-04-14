@echo off
echo.
echo  +-----------------------+
echo  + DELETE ... OLD COPIES +
echo  +   vertcon.out         +
echo  +   vertcon.prt         +
echo  +   type2.out           +
echo  +-----------------------+
echo.
      del vertcon.out
      del vertcon.prt
      del type2.out  
echo.
echo  +-----------------------+
echo  +   EXECUTE PROGRAM     +
echo  +                       +
echo  +   vertcon.exe         +
echo  +-----------------------+
echo.
      pause
      vertcon  <new_run.con >vertcon.prt
echo.
echo.
      dir type2.out  
      dir vertcon.*  
echo.
echo  +-----------------------+
echo  +   LIST THE OUTPUT     +
echo  +                       +
echo  +   type2.out           +
echo  +   vertcon.prt         +
echo  +-----------------------+
echo.
      pause 
      type type2.out | more
echo.
echo.
      pause
      type vertcon.prt | more
echo.
echo on
