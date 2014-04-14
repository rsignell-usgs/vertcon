      program convasci

*** convert binary arrays to ASCII 

      character*40 fname
      character*8 pgm
      character*56 ident
      integer dummy
      real*4  margin
      real*4  zrec(461)
      common/static/zrec
      lin =11
      lout=21

      write(6,2)
    2 format(' Enter UNFORMATTED (binary) input grid file name: '/
     .  ' [options: VERTCONE.94, VERTCONC.94, VERTCONW.94]')
      read(5,1) fname
    1 format(a)
      open(lin,file=fname,status='old',form='unformatted',
     .   access='direct',recl=1848)
      write(6,11) fname
   11 format(' Opened UNFORMATTED (input) file - ',a)
      fname(5:7)='ASC'
      open(lout,file=fname,status='new',form='formatted')
      write(6,12) fname
   12 format(' Opened ASCII (output) file - ',a)

       nrec=1
      read(lin,rec=nrec) ident,pgm,nlo,nla,nz,
     .  glomn,dglo,glamn,dgla,margin

c      write(6,13) ident,pgm,nlo,nla,nz,
c    .  glomn,dglo,glamn,dgla,margin
c  13 format(' Header - ',a56,a8/2i6,i3,5f11.5)

       ident(1:11)=fname(1:11)
      write(lout,'(a56,a8/3i4,5f12.5)') ident,pgm,nlo,nla,nz,
     .  glomn,dglo,glamn,dgla,margin

*** read and write the rows

        do 40 irow=1,nla
          nrec=nrec+1
         read(lin,rec=nrec) dummy,(zrec(i),i=1,nlo)
         write (lout,'(6f12.6)') (zrec(i),i=1,nlo)
   40 continue

      close(lin,status='keep')
      close(lout,status='keep')
      write(6,14)
   14 format(' Normal end -stop')
      stop
      end
