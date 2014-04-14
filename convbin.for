      program convbin 

*** convert ASCII arrays to binary

      character*40 fname
      real*4  zrec(461)
      integer dummy
      character*8 pgm
      character*56 ident
      real*4  margin
      common/static/zrec
       data dummy/0/
      lin =11
      lout=21

      write(6,2)
    2 format(' Enter ASCII (formatted) input grid file name: '/
     .  ' [options: VERTASCE.94, VERTASCC.94, VERTASCW.94]')
      read(5,1) fname
    1 format(a)
      open(lin,file=fname,status='old',form='formatted')
      write(6,11) fname
   11 format(' Opened FORMATTED (input) file - ',a)
      fname(5:7)='CON'
      open(lout,file=fname,status='new',form='unformatted',
     .   access='direct',recl=1848)
      write(6,12) fname
   12 format(' Opened UNFORMATTED (output) file - ',a)

      read(lin,'(a56,a8/3i4,5f12.5)') ident,pgm,nlo,nla,nz,
     .  glomn,dglo,glamn,dgla,margin

c      write(6,13) ident,pgm,nlo,nla,nz,
c    .  glomn,dglo,glamn,dgla,margin
c  13 format(' Header - ',a56,a8/2i6,i3,5f11.5)

       ident(1:11)=fname(1:11)
       nrec=1
      write(lout,rec=nrec) ident,pgm,nlo,nla,nz,
     .  glomn,dglo,glamn,dgla,margin

*** read and write the rows

        do 40 irow=1,nla
         read(lin,'(6f12.6)') (zrec(i),i=1,nlo)
          nrec=nrec+1
         write(lout,rec=nrec) dummy,(zrec(i),i=1,nlo)
   40 continue
      close(lin,status='keep')
      close(lout,status='keep')
      write(6,14)
   14 format(' Normal end -stop')
      stop
      end
