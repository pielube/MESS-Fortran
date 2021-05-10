 
! ================================================================
! Main program MESS - Multi Energy System Simulator (2021 version)
! ================================================================
! Pietro Lubello, Carlo Carcasci

program MESS2021


      USE MODcoord, ONLY: icase
      USE MODparam, ONLY: NhourYear,Nstep,Maxloc,MaxGiorni

      USE MODAddHourlyData, ONLY: AddHourlyData
      USE MODbattery,       ONLY: iswitch             !<<< WIP: temporary, to be removed
      USE MODelectrolyzer,  ONLY: iswitchelectrolyzer !<<< WIP: temporary, to be removed
      USE MODfuelcell,      ONLY: iswitchfuelcell     !<<< WIP: temporary, to be removed

      implicit real(8) (a-h,o-z), integer(i-n)

      real(8), dimension(Maxloc,NhourYear) :: deltaEEelectr,sourceEEelectr,sinkEEelectr
      real(8), dimension(Maxloc,NhourYear) :: deltaEEheatAmb,sourceEEheatAmb,sinkEEheatAmb
      real(8), dimension(Maxloc,NhourYear) :: deltaEEheatWat,sourceEEheatWat,sinkEEheatWat
      real(8), dimension(Maxloc,NhourYear) :: deltaEEcoolAmb,sourceEEcoolAmb,sinkEEcoolAmb

      real(8), dimension(NhourYear) :: deltaEEelectrAggr,sourceEEelectrAggr,sinkEEelectrAggr
      real(8), dimension(NhourYear) :: deltaEEheatAmbAggr,sourceEEheatAmbAggr,sinkEEheatAmbAggr
      real(8), dimension(NhourYear) :: deltaEEheatWatAggr,sourceEEheatWatAggr,sinkEEheatWatAggr
      real(8), dimension(NhourYear) :: deltaEEcoolAmbAggr,sourceEEcoolAmbAggr,sinkEEcoolAmbAggr

      real(8) :: deltaEEelectrAggrYear,sourceEEelectrAggrYear,sinkEEelectrAggrYear   
      real(8) :: deltaEEheatAmbAggrYear,sourceEEheatAmbAggrYear,sinkEEheatAmbAggrYear  
      real(8) :: deltaEEheatWatAggrYear,sourceEEheatWatAggrYear,sinkEEheatWatAggrYear  
      real(8) :: deltaEEcoolAmbAggrYear,sourceEEcoolAmbAggrYear,sinkEEcoolAmbAggrYear 

      real(8)                        :: ActPBP,ProfInd
      real(8), dimension(0:MaxGiorni):: curveNPV0, curveNPV, deltaNPV

      real(8), dimension(NhourYear)  :: EnBoughtOutAggr ,EnSoldOutAggr ,GasConsumpAggr, &
                                        EnBoughtOutAggr0,EnSoldOutAggr0,GasConsumpAggr0


      ! Program version

      character( 9), parameter ::Version= &     
              !'0.00.00.a'   ! First 2021 version            15/02/2021 PL
               '0.01.00.a'   ! Added new components          31/03/2021 PL
               !-.--.--.-|<-Formatted till here

      character(10), parameter ::DateVer= '31/03/2021'   ! Release date
                                          !dd/mm/yyyy|<-Formatted till here

      ! Machine Precision

      Ieps =Precision(1.e0)
      IepsD=Precision(1.d0)

      Eps4Mac=10.d0**(-Ieps )
      Eps8Mac=10.d0**(-IepsD)


      ! Initializing warning.out file

      open(unit=8,file='warning.out',form='formatted',status='UNKNOWN')
      rewind 8

      ! Writing version and machine precision

      write( *,9000)Version,DateVer
      write( 8,9000)Version,DateVer
9000  format(/, 5x,57('*'),/,/, &
               10x,'PROGRAM:  "MESS: Multi Energy System Simulator"' ,/,&
               10x,'Version:  ',a9,/, &
               10x,'Release date: ',a10,' [dd/mm/yyyy]',/,/, &
      5x,57('*'),/,/)

      write(*,9001) Ieps,IEpsD
      write(8,9001) Ieps,IEpsD
9001  format(' Machine precision: Single-> Eps = 10^-',i2.2,/, &
             '                    Double-> EpsD= 10^-',i2.2,/ )


      call CPU_TIME(t1)

      ! 1) Pre-process
      !    Reading and storing input data
      !    ------------------------------

      call ReadInputs()


      ! 2) Solving
      !    -------

      iswitch = 0             ! <<< WIP: necessary for battery ageing
      iswitchelectrolyzer = 0 ! <<< WIP: necessary for electrolyzer
      iswitchfuelcell     = 0 ! <<< WIP: necessary for fuelcell
      AddHourlyData = 0.d0    ! <<< WIP: Initialization of AddHourlyData, should be moved

      !VAN0 = 0.d0 ! se vuoi mettere icase =1,1
      !VAN  = 0.d0 ! se vuoi mettere icase =0,0

      do icase=0,1

      if(icase.eq.0)then
        write(*,*) " Reference case"
      else
        write(*,*) " User defined case"
      endif

      GasConsumpAggr         = 0.d0

      call solver(deltaEEelectr,sourceEEelectr,sinkEEelectr,    & ! Single location hourly energy balances
                deltaEEheatAmb,sourceEEheatAmb,sinkEEheatAmb, &
                deltaEEheatWat,sourceEEheatWat,sinkEEheatWat, &
                deltaEEcoolAmb,sourceEEcoolAmb,sinkEEcoolAmb, &

                deltaEEelectrAggr,sourceEEelectrAggr,sinkEEelectrAggr,    & ! Whole aggregate hourly energy balances
                deltaEEheatAmbAggr,sourceEEheatAmbAggr,sinkEEheatAmbAggr, &
                deltaEEheatWatAggr,sourceEEheatWatAggr,sinkEEheatWatAggr, &
                deltaEEcoolAmbAggr,sourceEEcoolAmbAggr,sinkEEcoolAmbAggr, &

                deltaEEelectrAggrYear,sourceEEelectrAggrYear,sinkEEelectrAggrYear,    & ! Whole aggregate whole year balances   
                deltaEEheatAmbAggrYear,sourceEEheatAmbAggrYear,sinkEEheatAmbAggrYear, &
                deltaEEheatWatAggrYear,sourceEEheatWatAggrYear,sinkEEheatWatAggrYear, &
                deltaEEcoolAmbAggrYear,sourceEEcoolAmbAggrYear,sinkEEcoolAmbAggrYear, &

                curveNPV,               & !(O) NPV curve (€)
                TotActInvest,           & !(O) Total actualized investment
                EnBoughtOutAggr,EnSoldOutAggr,GasConsumpAggr)


        ! Saving NPV curve in the reference case

        select case(icase)
        case(0)
          curveNPV0(0:Nstep) = curveNPV(0:Nstep)
          EnBoughtOutAggr0   = EnBoughtOutAggr
          EnSoldOutAggr0     = EnSoldOutAggr
          GasConsumpAggr0    = GasConsumpAggr
        end select

      enddo ! icase


      ! 3) Economic analysis post-process
      !    ------------------------------

      call NPVcalc(curveNPV0,    & ! (I) NPV case 0
                   curveNPV,     & ! (I) NPV case 1
                   TotActInvest, & ! (I) Total actualized investment
                   deltaNPV,     & ! (O) VAN difference between case 1 and case 0
                   ActPBP,       & ! (O) Actualized payback period
                   ProfInd)        ! (O) Profit index

      call CPU_TIME(t2)
      write(*,*) " "
      write(*,9004),t2-t1
9004  format(' Solving took',f8.4,' seconds')


      ! 4) Exporting output data   
      !    ---------------------

      call WriteOutputs(ActPBP,ProfInd,curveNPV0,curveNPV,deltaNPV,      &
                        EnBoughtOutAggr0,EnSoldOutAggr0,GasConsumpAggr0, &
                        EnBoughtOutAggr ,EnSoldOutAggr ,GasConsumpAggr , &                 
                        deltaEEelectr,sourceEEelectr,sinkEEelectr,       &
                        deltaEEheatAmb,sourceEEheatAmb,sinkEEheatAmb,    &
                        deltaEEheatWat,sourceEEheatWat,sinkEEheatWat,    &
                        deltaEEcoolAmb,sourceEEcoolAmb,sinkEEcoolAmb,    &
                        deltaEEelectrAggr,sourceEEelectrAggr,sinkEEelectrAggr,    &
                        deltaEEheatAmbAggr,sourceEEheatAmbAggr,sinkEEheatAmbAggr, &
                        deltaEEheatWatAggr,sourceEEheatWatAggr,sinkEEheatWatAggr, &
                        deltaEEcoolAmbAggr,sourceEEcoolAmbAggr,sinkEEcoolAmbAggr)
 

      call CPU_TIME(t3)
      write(*,9005),t3-t2
9005  format(' Writing results took',f8.4,' seconds')
      write(*,9006),t3-t1
9006  format(' Total time of operation was',f8.4,' seconds')


      stop ' Apparently everything went well!'

end program MESS2021