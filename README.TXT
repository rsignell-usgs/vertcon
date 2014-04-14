"@(#)vertcon.doc	1.1 - 00/04/17 10:27:24 NGS"
----------------------------------------------------------------------
README file for VERTCON v2.0     199408.18   RJF/dgm
README file for VERTCON v2.1     200309.29   RWS      

PURPOSE:  Program VERTCON computes the modeled difference in orthometric
          height between the North American Vertical Datum of 1988 (NAVD 88)
          and the National Geodetic Vertical Datum of 1929 (NGVD 29) for a
          given location specified by latitude and longitude.

A partial list of contents of the VERTCON distribution is:

   vertcon.exe    VERTical datum CONversion program
                  (compiled from VERTCON.FOR, a FORTRAN source code)

   vertcone.94    VERTCON datum transformation grid file;  eastern USA
                  (non-readable, i.e., binary, file)

   vertconc.94    VERTCON datum transformation grid file;  central USA
                  (non-readable, i.e., binary, file)

   vertconw.94    VERTCON datum transformation grid file;  western USA
                  (non-readable, i.e., binary, file)

   README.TXT     User's instruction file  (this file you are reading)

A number of sample output and batch files are included as examples, in 
addition to some utility routines described later in this document.

To install:
   1) Open a DOS window (or a Command Prompt window).
   2) Make a subdirectory on hard disk; 
        for example:  mkdir NGVDCONV
   3) Go into subdirectory; 
        for example:  cd NGVDCONV
   4) Copy the downloaded files into the subdirectory

To execute:
   Type     vertcon           and follow the prompts.

To terminate:
   VERTCON computations can be stopped at any time by the Control-C
   (i.e., <ctrl-c>) key combination.  Interactive processing
   can also be terminated by entering 0. (i.e., zero WITH DECIMAL POINT)

BUT PLEASE DON'T START YET;  KEEP READING THIS DOCUMENT.

How program VERTCON works:
   The software and three files of datum transformation grids for the
conterminous United States (CONUS) are provided in the distribution.  VERTCON
returns the orthometric height difference between NAVD 88 and NGVD 29 at the
geodetic position specified by the user.  VERTCON interpolates the datum
transformation at a point from the appropriate grid in your subdirectory.

Data Input:
   The user can key in latitude and longitude on a point-by-point basis or can
create an input file using a text editor.  Several file formats are provided,
including the internal bench mark file record format of the Vertical Network
Branch, NGS.  These formats are detailed in a "Help" menu option which appears
when the input filename is specified.

   Most horizontal positions of the bench marks used to generate VERTCON were
scaled from USGS topographic maps.  The estimated uncertainty of the scaled
positions, 6", is greater than the differences between NAD 27 and NAD 83.
Therefore, the latitude and longitude provided to VERTCON can be on either
the NAD 27 or NAD 83 datum.

Data Output:
   Results are collected into an output file.  The default name of this file is
VERTCON.OUT, but the user can choose any legal filename.  (A word of advice:
don't use misleading extensions such as .EXE, .BAT, etc.).  The format
of the output file is linked to the format of the input file to maintain
consistency.
 
------------------------>  THE SENSE OF THE SIGNS  <--------------------------

  The grids contain a model of (NAVD 88 - NGVD 29) height differences.

  If a NAVD 88 height is desired when a NGVD 29 height is given, 
       ADD the model value ALGEBRAICALLY to the NGVD 29 height.

  If a NGVD 29 height is desired when a NAVD 88 height is given, 
       SUBTRACT the model value ALGEBRAICALLY from the NAVD 88 height.

The VERTCON 2.0 Model
  The VERTCON 2.0 model was computed on May 5, 1994 using 381,833 datum
difference values.  A key part of the computation procedure was the
development of the predictable, physical components of the differences 
between the NAVD 88 and NGVD 29 datums.  This included models of refraction
effects on geodetic leveling, and gravity and elevation influences on the
new NAVD 88 datum.  Tests of the predictive capability of the physical
model show a 2.0 cm RMS agreement at our 381,833 data points.  For this 
reason, the VERTCON 2.0 model can be considered accurate at the 2 cm
(one sigma) level.  Since 381,833 data values were used to develop the
corrections to the physical model, VERTCON 2.0 will display even better 
overall accuracy than that displayed by the uncorrected physical model.  This 
higher accuracy will be particularly noticable in the eastern United States.  

Using VERTCON 2.0
  It should be emphasized that VERTCON 2.0 is a datum transformation model,
and can not maintain the full vertical control accuracy of geodetic leveling.
Ideally, one should process level data using the latest reduction software
and adjust it to established NAVD 88 control.  However, VERTCON 2.0 accuracy
is suitable for a variety of mapping and charting purposes.

  The VERTCON 2.0 model expresses datum differences between NAVD 88 and NGVD
29 due to removal of distortions in the level data, as well as due to the
physical differences in the height systems.  In some rare cases, these local
NGVD 29 distortions could be 20 cm or more.  If both ends of your old
vertical survey were tied to one of these "problem" lines, then the datum
difference of the problem line is appropriate to use to transform the survey
data.  If both ends of a vertical survey are tied to "undistorted lines",
then it is appropriate to use a slightly distant point to compute the 
transformation, no matter how close your survey data may approach a given
problem line.  The possible presense of a problem NGVD 29 line in the
vicinity of your survey will become evident if dramatically different datum
transformation values are computed within a small area.

  It must also be emphasized that VERTCON 2.0 is not to be considered
reliable beyond the boundaries of the lower 48 United States.  The VERTCON 
program will interpolate values in Canada, Mexico, or in the ocean, due
to the grid structure of the model.  Those values do not contain important
model components present in the conterminous U.S. model.  Future versions 
of VERTCON may be extended into neighboring countries.

The Defense Mapping Agency
   The Defense Mapping Agency (DMA) has been of immense help in this
endeavor.  DMA has provided a major portion of the NGS land gravity data
set.  DMA has also been instrumental in the creation of the various 30"
elevation grids in existence.  Although the work of the DMA generally
precludes public recognition, their cooperation in this work is gratefully
acknowledged.
 
Other Programs:
   The datum shift grids and VERTCON software are provided on standard disc
operating system (DOS) controlled (IBM-compatible) personal computers (PC).
In support of other computer systems, the following utility software is 
included:

convasci  --  copy unformatted (binary) grid files into ASCII files for
              transfer to other systems

convbin   --  will restore the ASCII files into binary grid files on the new
              system.

Other Future Plans:
   A continuing development effort is underway to improve VERTCON results.
NGVD 29 normal orthometric heights are being analyzed for localized monument
and/or crustal motion effects, for inconsistent adjustments, and other effects.
Computed height differences which are significantly influenced by such effects
will be flagged and rated for reliability in future versions.

For More Information
 
    For Products Available From the National Geodetic Survey:

                     National Geodetic Information Center
                     N/NGS1, SSMC3-9450
                     National Geodetic Survey, NOAA
                     Telephone: 301-713-3242
                     E-Mail: ngs.infocenter@noaa.gov
 
                     David B. Zilkoski
                     NOAA, National Geodetic Survey, N/NGS
                     E-Mail: Dave.Zilkoski@noaa.gov
A special word of thanks goes to our colleague, Sandford R. Holdahl, who has
recently retired.  Sandy made the first predictions of the vertical datum
differences in 1983, and is a co-author of the VERTCON 2.0 model.

README file   199408.18   RJF/dgm

Vertcon 2.10 is a modification of Vertcon 2.0 code to make it accessible
via the National Geodetic Survey (NGS) Geodetic TOOL Kit on the Web and
allow negative orthometric heights to be entered.

The program was modified to accept a single command line parameter. The
parameter may be either a blank, OHT or WEB.  No parameter, the blank
option, will be cause the program to execute with the normal output. If
the "OHT" option is used, e.g. vertcon oht, the program will execute
so that the user may see the program prompts and enter an orthometric
height and select either the NGVD29 or NAVD88 datum.  The "WEB" option
is used only for web execution, e.g. vertcon web. 

Warnings to those who compile the source code: (1)The grid files are in binary 
format. If you download these files from the NGS web site, you should receive 
the binary data in little endian form, which is correct for Intel processors.
(2) The grid files are opened for direct access with a record size of 1848 
bytes. The Fortran standard says that the unit of measurement of the record
size is implementation dependent, and the default for some compilers
is to measure this in (4 byte) words. Most compilers allow the user to 
override the default.


README file   200309.29   RWS

