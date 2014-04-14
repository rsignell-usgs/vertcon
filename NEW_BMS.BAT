@echo off
echo.
echo  +-----------------------+
echo  + DELETE ... OLD COPIES +
echo  +   vertcon.out         +
echo  +   vertcon.prt         +
echo  +   DATUM_29.BMS        +
echo  +-----------------------+
echo.
      del vertcon.out 
      del vertcon.prt
      del datum_29.bms
echo.
echo  +-----------------------+
echo  +   EXECUTE PROGRAM     +
echo  +                       +
echo  +   vertcon.exe         +
echo  +-----------------------+
echo.
      pause
      vertcon <new_bms.con >vertcon.prt 
echo.
echo.
      dir DATUM_29.BMS
      dir vertcon.*  
echo.
echo  +-----------------------+
echo  +   LIST THE OUTPUT     +
echo  +                       +
echo  +   DATUM_29.BMS        +
echo  +   vertcon.prt         +
echo  +-----------------------+
echo.
      pause 
      type DATUM_29.BMS | more
echo.
echo.
      pause
      type vertcon.prt | more
echo.
echo on
