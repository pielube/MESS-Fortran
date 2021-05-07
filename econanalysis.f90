
! Economic analysis subroutine
! Lubello: gen 2020

subroutine econanalysis(enExpendComplete,       & ! (I) Electrical energy expenditure for each time step
                        enExpendGasComplete,    & ! (I) Gas expenditure at each time step
                        DataEconAn,             & ! (I) Components,life expectancy, capex exp
                        curveNPV,               & ! (O) NPV curve
                        TotActInvest)             ! (O) Total actualized investment


      USE MODparam, ONLY: NdayYear,NhourDay,NhourYear,Nyears,Nstep,MaxComp,MaxBuild,MaxGiorni,MaxHours

      implicit real(8) (a-h,o-z), integer(i-n)

      real(8), dimension(MaxHours),             intent(IN   ):: enExpendComplete,enExpendGasComplete
      real(8), dimension( 13,MaxComp,MaxBuild), intent(IN   ):: DataEconAn 
      real(8), dimension(0:MaxGiorni),          intent(  OUT):: curveNPV
      real(8),                                  intent(  OUT):: TotActInvest
               
      real(8), dimension(0:MaxGiorni) :: Expend
      real(8), parameter:: rate   = 0.05d0 ! Interest rate


      ! DataEconAn description 
      ! First index:  1- ID component, 2- Number of substitutions, 3- Components CAPEX, 4 to 13- Year in which it will be substituted
      ! Second index: Which component of the building
      ! Third index:  Which building


      ! Daily interest rate

      rateupd = (1.d0+rate)**(1.d0/dble(NdayYear)) - 1.d0    ! rateupd = rate / 365.d0

      ! Calculations

      Expend(0)=0.d0
      do iday=1,Nstep
        Expend(iday) = 0.d0
        do ih=1,NhourDay !24 h/day
        Expend(iday) = Expend(iday)+                                &
                       enExpendComplete   ((iday-1)*NhourDay+ih)+   &
                       enExpendGasComplete((iday-1)*NhourDay+ih) !spesa alla fine del giorno
        enddo
      enddo

      TotActInvest = 0.d0 ! Total actualized investment inizialization

      ! Adding CAPEX costs to the daily expenditure

      do jj=1,MaxBuild ! Iterating on the buildings
        do ii=1,MaxComp !Iterating on the components

          ID = DataEconAn(1,ii,jj) !tipo del componente

          select case (ID)

          case(0)
            ! Matrix zeros, no action required

          case(1,2)
            ! Component "Demand", no action required

          case(3,4,5,6,7,8,9,10,11,12,13,14)
            ! All other components
            Nsost            = nint(DataEconAn(2,ii,jj))
            CompCapex        =      DataEconAn(3,ii,jj)

            do isost=0,Nsost
            
              if(isost .eq. 0)then
                iday = 0
              else
                iday=DataEconAn(3+isost,ii,jj)*NdayYear
              endif
            
              if(iday.ge.Nstep) then
                ! Making sure we are not investing on new components the last day of the last year
              else
                ! Adding CAPEX
                Expend(iday) = Expend(iday) - CompCapex                        ! CAPEX added at the beginning of the day
                TotActInvest = TotActInvest + CompCapex/((1.d0+rateupd)**iday) ! Total actualized investment
              endif

            enddo

          case default
            write(*,*) 'Internal error: missing component ID in econanalysis'
            stop 'Internal error!'
          endselect

        enddo
      enddo


      ! Computing the cumulative curve of energy expend considering int rate

      curveNPV(0) = Expend(0)  ! Starting instant

      do iday=1,Nstep
        curveNPV(iday) = curveNPV(iday-1) + Expend(iday)/((1.d0+rateupd)**iday)
      enddo

      return

end subroutine econanalysis