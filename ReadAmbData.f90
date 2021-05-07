
! AmbData.dat file reading
! Lubello: dic 2019


subroutine ReadAmbData()

      USE MODparam   , ONLY: NhourYear
      USE MODambient , ONLY: ndataAmb,hourAmb,TempAmb,AirDens,SolIrr,WindSpeed

      implicit real(8) (a-h,o-z), integer(i-n)

      character(11), parameter                    :: namef='AmbData.dat'    !Input file name

      ! Reading ambient data
      ! ====================

      open(2,file=namef,status='old',access='sequential',form='formatted',err=7000)
      iline=0
      rewind 2

      ! Reading number of data points and checking if in bounds

      iline=iline+1
      read(2,*,err=7010) ndataAmb

      if (ndataAmb.lt.1 .or. ndataAmb.gt.NhourYear) then
        ! Error! Too few or too many ambient data points
        write(*,*)'Error! Too few or too many ambient data points'
        stop 'Input error!'
      else
        ! Ok Acceptable number of timesteps
      endif

      ! Reading actual data

      do ii=1,ndataAmb
        iline=iline+1
        read(2,*,err=7010) hourAmb   (ii), & ! Hour             [h] (decimale) dell'anno (0= mezzanotte dell'anno fino a 8760h che è fine anno)
                           Tempamb   (ii), & ! Temperature      [C]
                           AirDens   (ii), & ! Air density      [kg/m^3]
                           SolIrr    (ii), & ! Solar irradiance [W/m^2]
                           WindSpeed (ii)    ! Wind speed       [m/s]
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
           t12,'Error line:           ',i6)
      stop 'Error!'

end subroutine ReadAmbData