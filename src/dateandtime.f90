
! Subroutine to convert from hour of the year (numbered progressively) to YYYY-MM-DD HH:mm:ss format
! YYYY must be given as an input
! Warning 1: leap years are considered to be of 365 days as well
! Warning 2: works only with hourly timesteps

! Lubello, feb 2021

subroutine hourtodate(year,   & !(I) Reference year
                      hourin, & !(I) Hour of the year
                      date)     !(O) Date in YYYY-MM-DD HH:mm:ss

      implicit real(8) (a-h,o-z), integer(i-n)

      integer,   intent(IN   ) :: year,hourin
      character(len=19), intent(  OUT) :: date

      character(len=4) :: YYYY
      character(len=2) :: MM,DD,HH,min,ss
      character(len=1) :: singDD,singHH

      integer :: day,daymonth,hourday


      hour = hourin -1
      day = int(hour/24.d0) + 1


      if(day .le. 31)then
        MM = '01'
        daymonth = day
        hourday = int(hour-(day-1)*24.d0)
      elseif(day.gt.31  .and. day.le.59)then
        MM = '02'
        daymonth = day-31
        hourday = int(hour-(day-1)*24.d0)
      elseif(day.gt.59  .and. day.le.90)then
        MM = '03'
        daymonth = day-59
        hourday = int(hour-(day-1)*24.d0)
      elseif(day.gt.90  .and. day.le.120)then
        MM = '04'
        daymonth = day-90
        hourday = int(hour-(day-1)*24.d0)
      elseif(day.gt.120  .and. day.le.151)then
        MM = '05'
        daymonth = day-120
        hourday = int(hour-(day-1)*24.d0)
      elseif(day.gt.151  .and. day.le.181)then
        MM = '06'
        daymonth = day-151
        hourday = int(hour-(day-1)*24.d0)
      elseif(day.gt.181  .and. day.le.212)then
        MM = '07'
        daymonth = day-181
        hourday = int(hour-(day-1)*24.d0)
      elseif(day.gt.212  .and. day.le.243)then
        MM = '08'
        daymonth = day-212
        hourday = int(hour-(day-1)*24.d0)
      elseif(day.gt.243  .and. day.le.273)then
        MM = '09'
        daymonth = day-243
        hourday = int(hour-(day-1)*24.d0)
      elseif(day.gt.273  .and. day.le.304)then
        MM = '10'
        daymonth = day-273
        hourday = int(hour-(day-1)*24.d0)
      elseif(day.gt.304  .and. day.le.334)then
        MM = '11'
        daymonth = day-304
        hourday = int(hour-(day-1)*24.d0)
      elseif(day.gt.334  .and. day.le.365)then
        MM = '12'
        daymonth = day-334
        hourday = int(hour-(day-1)*24.d0)
      else
        write(*,*) 'Internal error: hour to date conversion'
        stop 'Internal error!'
      endif

      write(YYYY,'(i4)') year
      
      if(daymonth.lt.10)then
        write(singDD,'(i1)') daymonth
        DD = '0'//singDD
      else
        write(DD,'(i2)')   daymonth
      endif

      if(hourday.lt.10)then
        write(singHH,'(i1)') hourday
        HH = '0'//singHH
      else
        write(HH,'(i2)')   hourday
      endif

      min = '00'
      ss  = '00'

      date = YYYY//'-'//MM//'-'//DD//' '//HH//':'//min//':'//ss

end subroutine hourtodate