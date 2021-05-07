
! globalparam.dat file reading
! Lubello: apr 2020


subroutine ReadGlobalParam()

      USE MODGlobalParam    , ONLY: RefYear,IndAgeing
     

      implicit real(8) (a-h,o-z), integer(i-n)

      character(15), parameter   :: namef1='globalparam.dat'    !Input file for global paramaters
      

      ! Reading global parameteres
      ! --------------------------

      open(2,file=namef1,status='old',access='sequential',form='formatted',err=7000)
      iline=0
      rewind 2

      iline=iline+1
      read(2,*,err=7010) RefYear

      diff = abs(RefYear/4.d0-int(RefYear/4.d0))
      if(diff .lt. 0.00001d0)then
        write(*,*) 'Warning: you chose a leap year, while we are working with 365days-years'
      else
      endif

      iline=iline+1
      read(2,*,err=7010) IndAgeing

      if(IndAgeing.ne.0 .and. IndAgeing.ne.1) then
        write(*,*) 'Wrong option for IndAgeing in ',namef1
        stop 'Input data error!'
      endif


      close(2)

      return

7000  continue
      write(*,7100) namef1
      write(8,7100) namef1
7100  format(/,'ERROR: Open error on "',a,'" file')
      stop 'Error!'
      
7010  continue
      write(*,7110) namef1,iline
      write(8,7110) namef1,iline
7110  format(/,'ERROR: Read error on "',a,'" file',/,   &
           t12,'Error line:           ',i6)
      stop 'Error!'


end subroutine ReadGlobalParam