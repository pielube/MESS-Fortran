
!    Lettura dal file city.dat
!    Lubello: Dic 2019

subroutine ReadCity()

      USE MODparam, ONLY: MaxBuild
      USE MODcity,  ONLY: iTimeStart,iTimeEnd,Nbuild, BuildName

      implicit real(8) (a-h,o-z), integer(i-n)

      character(10), parameter                    :: namef='city.dat'    !Input file name

      ! Reading ambient data
      ! ====================

      open(2,file=namef,status='old',access='sequential',form='formatted',err=7000)
      iline=0
      rewind 2

      !* Reading number of data points
      iline=iline+1
      read(2,*,err=7010) iTimeStart  !Ora di inizio simulazione
      iline=iline+1
      read(2,*,err=7010) iTimeEnd    !Ora di fine simulazione

      !* Reading number of data points
      iline=iline+1
      read(2,*,err=7010) Nbuild !numero edifici che determinano la city

      if(Nbuild.lt.1       .or. &
         Nbuild.gt.MaxBuild     )   then
        write(*,*) 'Numero di punti eccessivo in ',namef
        stop 'input data error!'
      endif

      ! Reading actual data

      do ii=1,Nbuild
        iline=iline+1
        read(2,*,err=7010) BuildName  (ii) ! Nome dell'edificio
      enddo

      close(2)

      return

7000  continue
      write(*,7100) namef
      write(8,7100) namef
7100  format(/,'ERROR: Open error on "',a,'" file')
      stop 'Error!'

7010  continue
      write(*,7110) namef,iline
      write(8,7110) namef,iline
7110  format(/,'ERROR: Read error on "',a,'" file',/,   &
           t12,'Error line:           ',i2)
      stop 'Error!'



end subroutine ReadCity