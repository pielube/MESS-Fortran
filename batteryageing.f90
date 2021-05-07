
! Battery ageing
! Lubello, Papi: apr 2020

subroutine batteryageing(WeeklyChargeHist, & ! Array of weekly charge history
                         ccyc,              & ! State of Health, to be updated
                         Damage)                

     USE MODparam,       ONLY: NhourWeek

      implicit real(8) (a-h,o-z), integer(i-n)

      real(8), intent(IN   ), dimension(NhourWeek) :: WeeklyChargeHist
      real(8), intent(INOUT)                       :: ccyc
      integer                                      :: Ncyc
      real(8),                dimension(10008)     :: grezzata2    

      real(8), intent(  OUT)                       :: Damage  



      ! Calling rainflow to put WeeklyChargeHist in order

      call newrainflow(NhourWeek,        &
                       WeeklyChargeHist, &
                       Ncyc,             &
                       grezzata2)


      ! Weekly damage estimation with Wohler

      Damage =0.d0

      do idam=1,20
        Damage = Damage + grezzata2(idam)/(1512.4525d0*((dble(idam)/20.d0)**(-0.968423d0)))
        !Damage = Damage + grezzata2(idam)/(151245.25d0*((dble(idam)/20.d0*100)**(-0.968423d0)))
      enddo


      ccyc = ccyc + Damage

      ! Updating SoH with new weekly damage
      !SoH = SoH*(1-Damage*0.3)   ! Damage tra 0 e 1, damage = 1 corrisponde a SoH = 70%      

      return

end subroutine batteryageing