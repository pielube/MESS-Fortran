
! City subroutine
! Lubello, Carcasci: dic 2019


subroutine city(deltaEEelectr,sourceEEelectr,sinkEEelectr,    & ! Single building hourly energy balances
                deltaEEheatAmb,sourceEEheatAmb,sinkEEheatAmb, &
                deltaEEheatWat,sourceEEheatWat,sinkEEheatWat, &
                deltaEEcoolAmb,sourceEEcoolAmb,sinkEEcoolAmb, &

                deltaEEelectrCity,sourceEEelectrCity,sinkEEelectrCity,    & ! Whole city hourly energy balances
                deltaEEheatAmbCity,sourceEEheatAmbCity,sinkEEheatAmbCity, &
                deltaEEheatWatCity,sourceEEheatWatCity,sinkEEheatWatCity, &
                deltaEEcoolAmbCity,sourceEEcoolAmbCity,sinkEEcoolAmbCity, &

                deltaEEelectrCityYear,sourceEEelectrCityYear,sinkEEelectrCityYear,    & ! Whole city whole year balances   
                deltaEEheatAmbCityYear,sourceEEheatAmbCityYear,sinkEEheatAmbCityYear, &
                deltaEEheatWatCityYear,sourceEEheatWatCityYear,sinkEEheatWatCityYear, &
                deltaEEcoolAmbCityYear,sourceEEcoolAmbCityYear,sinkEEcoolAmbCityYear, &

                curveNPV,               & !(O) andamento VAN (€)
                TotActInvest,           & !(O) Total actualized investment
                EnBoughtOutCity,        &
                EnSoldOutCity,          &
                GasConsumpCity)


      USE MODcoord,       ONLY: iyear,itime,ibuild,itimeperweek,itimeperday
      USE MODGlobalParam, ONLY: IndAgeing
      USE MODparam,       ONLY: NdayYear,NhourDay,NhourWeek,NhourYear,Nyears,Nstep,MaxComp,MaxBuild,MaxGiorni,MaxHours
      USE MODambient,     ONLY: TempAmb,AirDens,SolIrr,WindSpeed,RelHumidity
      USE MODcity,        ONLY: iTimeStart,iTimeEnd,Nbuild, BuildName


      implicit real(8) (a-h,o-z), integer(i-n)


      ! Argument declarations

      real(8), dimension(MaxBuild,NhourYear), intent(  OUT) :: deltaEEelectr,sourceEEelectr,sinkEEelectr
      real(8), dimension(MaxBuild,NhourYear), intent(  OUT) :: deltaEEheatAmb,sourceEEheatAmb,sinkEEheatAmb
      real(8), dimension(MaxBuild,NhourYear), intent(  OUT) :: deltaEEheatWat,sourceEEheatWat,sinkEEheatWat
      real(8), dimension(MaxBuild,NhourYear), intent(  OUT) :: deltaEEcoolAmb,sourceEEcoolAmb,sinkEEcoolAmb

      real(8), dimension(NhourYear), intent(  OUT) :: deltaEEelectrCity,sourceEEelectrCity,sinkEEelectrCity
      real(8), dimension(NhourYear), intent(  OUT) :: deltaEEheatAmbCity,sourceEEheatAmbCity,sinkEEheatAmbCity
      real(8), dimension(NhourYear), intent(  OUT) :: deltaEEheatWatCity,sourceEEheatWatCity,sinkEEheatWatCity
      real(8), dimension(NhourYear), intent(  OUT) :: deltaEEcoolAmbCity,sourceEEcoolAmbCity,sinkEEcoolAmbCity

      real(8), intent(  OUT) :: deltaEEelectrCityYear,sourceEEelectrCityYear,sinkEEelectrCityYear   
      real(8), intent(  OUT) :: deltaEEheatAmbCityYear,sourceEEheatAmbCityYear,sinkEEheatAmbCityYear  
      real(8), intent(  OUT) :: deltaEEheatWatCityYear,sourceEEheatWatCityYear,sinkEEheatWatCityYear  
      real(8), intent(  OUT) :: deltaEEcoolAmbCityYear,sourceEEcoolAmbCityYear,sinkEEcoolAmbCityYear  

                                                                  
      real(8),       dimension(0:MaxGiorni),        intent(  OUT) :: curveNPV
      real(8),                                      intent(  OUT) :: TotActInvest

      real(8),       dimension(NhourYear),          intent(  OUT) :: EnBoughtOutCity ,EnSoldOutCity

      real(8),       dimension(NhourYear),          intent(  OUT) :: GasConsumpCity


      ! Local declarations

      real(8),       dimension(            MaxBuild)              :: EnBoughtOut,EnSoldOut
      real(8),       dimension(            MaxBuild)              :: enExpend,enExpendGas
      real(8),       dimension( 13,MaxComp,MaxBuild)              :: DataEconAn
                                                                  
      real(8),       dimension(            MaxBuild)              :: GasConsump
                                                                  
      integer,       dimension(            MaxBuild)              :: NValData
      real(8),       dimension(100,MaxComp,MaxBuild)              :: ValData
                                                                  
      real(8),       dimension(NhourYear)                         :: enExpendCity,enExpendGasCity

      real(8),       dimension(MaxHours)                          :: enExpendComplete,enExpendGasComplete


 
      ! Performing calculations: each building (inner) at each timestep (outern)
      ! ------------------------------------------------------------------------

      ! Whole city - whole year initialization (outern inizialization)

      deltaEEelectrCityYear  = 0.d0
      sourceEEelectrCityYear = 0.d0
      sinkEEelectrCityYear   = 0.d0

      deltaEEheatAmbCityYear  = 0.d0
      sourceEEheatAmbCityYear = 0.d0 
      sinkEEheatAmbCityYear   = 0.d0

      deltaEEheatWatCityYear  = 0.d0
      sourceEEheatWatCityYear = 0.d0 
      sinkEEheatWatCityYear   = 0.d0

      deltaEEcoolAmbCityYear  = 0.d0
      sourceEEcoolAmbCityYear = 0.d0 
      sinkEEcoolAmbCityYear   = 0.d0

      EnBoughtOutCityYear= 0.d0
      EnSoldOutCityYear  = 0.d0
      enExpendCityYear   = 0.d0
      enExpendGasCityYear= 0.d0

   
      do 4000 iyear=1,30

      write(*,*) "iyear iteration: ", iyear    
          
      do 3000 itime=iTimeStart,iTimeEnd ! itime DO cyle (outern)

        ! Counting weekly hours to compute ageing

        if(iyear.eq.1 .and. itime.eq.1)then
          itimeperweek = 0
          itimeperday  = 0
        else
        endif

        if(itimeperweek.eq.NhourWeek)then
          itimeperweek = 0
        else
        endif

        if(itimeperday.eq.NhourDay)then
          itimeperday = 0
        else
        endif

        itimeperweek = itimeperweek + 1
        itimeperday  = itimeperday  + 1

        ! Finished counting weekly hours to compute ageing


      ! >>>>> WIP: working hours =/ from hourly data <<<<<

        !call ryp2(NhourYear,2,1,              &
        !          ndataAmb,hourAmb,TempAmb,   &
        !          dble(itime),TempAmb1,       &
        !          IndErrAmb)                  
        !
        !call ryp2(NhourYear,2,1,              &
        !          ndataAmb,hourAmb,AirDens,   &
        !          dble(itime),AirDens1,       &
        !          IndErrAmb)                  
        !
        !call ryp2(NhourYear,2,1,              &
        !          ndataAmb,hourAmb,SolIrr,    &
        !          dble(itime),SolIrr1,        &
        !          IndErrAmb)
        !
        !call ryp2(NhourYear,2,1,              &
        !          ndataAmb,hourAmb,WindSpeed, &
        !          dble(itime),WindSpeed1,     &
        !          IndErrAmb)

      ! >>>>> WIP: working hours =/ from hourly data <<<<<


        TempAmb1 = TempAmb(itime)
        AirDens1  = AirDens(itime)
        SolIrr1   = SolIrr(itime)
        WindSpeed1 = WindSpeed(itime)
        RelHumidity1 = RelHumidity


        ! Whole city hourly inizialization (each year, inner inizialization)

        deltaEEelectrCity       (itime) = 0.d0
        sourceEEelectrCity      (itime) = 0.d0
        sinkEEelectrCity        (itime) = 0.d0
                                 
        deltaEEheatAmbCity      (itime) = 0.d0
        sourceEEheatAmbCity     (itime) = 0.d0
        sinkEEheatAmbCity       (itime) = 0.d0
                                 
        deltaEEheatWatCity      (itime) = 0.d0
        sourceEEheatWatCity     (itime) = 0.d0
        sinkEEheatWatCity       (itime) = 0.d0
                                 
        deltaEEcoolAmbCity      (itime) = 0.d0
        sourceEEcoolAmbCity     (itime) = 0.d0
        sinkEEcoolAmbCity       (itime) = 0.d0

        EnBoughtOutCity       (itime) = 0.d0
        EnSoldOutCity         (itime) = 0.d0
        enExpendCity          (itime) = 0.d0
        enExpendGasCity       (itime) = 0.d0

        GasConsumpCity        (itime) = 0.d0        


        do 1000 ibuild=1,Nbuild ! ib DO cyle (inner)
     
          call edificio(TempAmb1,AirDens1,SolIrr1,WindSpeed1,RelHumidity1, & ! (I) Ambient data 

                        BuildName  (ibuild),     &

                        deltaEEelectr(ibuild,itime),   & 
                        sourceEEelectr(ibuild,itime),  & 
                        sinkEEelectr(ibuild,itime),    & 
                        
                        deltaEEheatAmb(ibuild,itime),  & 
                        sourceEEheatAmb(ibuild,itime), & 
                        sinkEEheatAmb(ibuild,itime),   & 
                        
                        deltaEEheatWat(ibuild,itime),  & 
                        sourceEEheatWat(ibuild,itime), & 
                        sinkEEheatWat(ibuild,itime),   & 
                        
                        deltaEEcoolAmb(ibuild,itime),  & 
                        sourceEEcoolAmb(ibuild,itime), & 
                        sinkEEcoolAmb(ibuild,itime),   & 

                        GasConsump        (ibuild),    &

                        EnBoughtOut(ibuild),                      &
                        EnSoldOut  (ibuild),                      &
                        enExpend   (ibuild),                      &
                        enExpendGas(ibuild),                      &
                        DataEconAn (1,1,ibuild),                  &

                        NValData       (ibuild),       &
                        ValData    (1,1,ibuild))
          

          ! Whole city hourly balances

          deltaEEelectrCity       (itime) = deltaEEelectrCity       (itime) + deltaEEelectr   (ibuild,itime)
          sourceEEelectrCity      (itime) = sourceEEelectrCity      (itime) + sourceEEelectr  (ibuild,itime)
          sinkEEelectrCity        (itime) = sinkEEelectrCity        (itime) + sinkEEelectr    (ibuild,itime)

          deltaEEheatAmbCity      (itime) = deltaEEheatAmbCity      (itime) + deltaEEheatAmb  (ibuild,itime)
          sourceEEheatAmbCity     (itime) = sourceEEheatAmbCity     (itime) + sourceEEheatAmb (ibuild,itime)
          sinkEEheatAmbCity       (itime) = sinkEEheatAmbCity       (itime) + sinkEEheatAmb   (ibuild,itime)

          deltaEEheatWatCity      (itime) = deltaEEheatWatCity      (itime) + deltaEEheatWat  (ibuild,itime)
          sourceEEheatWatCity     (itime) = sourceEEheatWatCity     (itime) + sourceEEheatWat (ibuild,itime)
          sinkEEheatWatCity       (itime) = sinkEEheatWatCity       (itime) + sinkEEheatWat   (ibuild,itime)

          deltaEEcoolAmbCity      (itime) = deltaEEcoolAmbCity      (itime) + deltaEEcoolAmb  (ibuild,itime)
          sourceEEcoolAmbCity     (itime) = sourceEEcoolAmbCity     (itime) + sourceEEcoolAmb (ibuild,itime)
          sinkEEcoolAmbCity       (itime) = sinkEEcoolAmbCity       (itime) + sinkEEcoolAmb   (ibuild,itime)

          enExpendCity          (itime) = enExpendCity          (itime) + enExpend          (ibuild)
          enExpendGasCity       (itime) = enExpendGasCity       (itime) + enExpendGas       (ibuild)
 
          GasConsumpCity        (itime) = GasConsumpCity        (itime) + GasConsump        (ibuild)
          EnBoughtOutCity       (itime) = EnBoughtOutCity       (itime) + EnBoughtOut       (ibuild)
          EnSoldOutCity         (itime) = EnSoldOutCity         (itime) + EnSoldOut         (ibuild)


1000    continue ! ib DO cycle

        ! Whole city whole year balances

        deltaEEelectrCityYear   = deltaEEelectrCityYear   + deltaEEelectrCity   (itime)
        sourceEEelectrCityYear  = sourceEEelectrCityYear  + sourceEEelectrCity  (itime)
        sinkEEelectrCityYear    = sinkEEelectrCityYear    + sinkEEelectrCity    (itime)

        deltaEEheatAmbCityYear  = deltaEEheatAmbCityYear  + deltaEEheatAmbCity  (itime)
        sourceEEheatAmbCityYear = sourceEEheatAmbCityYear + sourceEEheatAmbCity (itime)
        sinkEEheatAmbCityYear   = sinkEEheatAmbCityYear   + sinkEEheatAmbCity   (itime)
                                                                                
        deltaEEheatWatCityYear  = deltaEEheatWatCityYear  + deltaEEheatWatCity  (itime)
        sourceEEheatWatCityYear = sourceEEheatWatCityYear + sourceEEheatWatCity (itime)
        sinkEEheatWatCityYear   = sinkEEheatWatCityYear   + sinkEEheatWatCity   (itime)
                                                                                
        deltaEEcoolAmbCityYear  = deltaEEcoolAmbCityYear  + deltaEEcoolAmbCity  (itime)
        sourceEEcoolAmbCityYear = sourceEEcoolAmbCityYear + sourceEEcoolAmbCity (itime)
        sinkEEcoolAmbCityYear   = sinkEEcoolAmbCityYear   + sinkEEcoolAmbCity   (itime)
                                                           
        EnBoughtOutCityYear = EnBoughtOutCityYear + EnBoughtOutCity(itime)
        EnSoldOutCityYear   = EnSoldOutCityYear   + EnSoldOutCity  (itime)
        enExpendCityYear    = enExpendCityYear    + enExpendCity   (itime)
        enExpendGasCityYear = enExpendGasCityYear + enExpendGasCity(itime)


3000  continue ! itime DO cycle

      if(IndAgeing .eq. 0)then
        goto 4010
      else
      endif
      
      istart = int((iyear-1)*365*24+1)
      iend   = int((iyear-1)*365*24+365)

      enExpendComplete   (istart:iend) = enExpendCity
      enExpendGasComplete(istart:iend) = enExpendGasCity
      
4000  continue ! iyear DO cycle      


      ! Economic analysis
      ! -----------------

4010  if(IndAgeing .eq. 0)then
        do ii = 1,30
          istart = int((ii-1)*365*24+1)
          iend   = int((ii-1)*365*24+365)
          enExpendComplete   (istart:iend) = enExpendCity
          enExpendGasComplete(istart:iend) = enExpendGasCity
        enddo
      else
      endif

      call econanalysis(enExpendComplete,    & ! (I) Electrical energy expenditure for each time step
                        enExpendGasComplete, & ! (I) Gas expenditure at each time step
                        DataEconAn,          & ! (I) Components,life expectancy, capex exp
                        curveNPV,            & ! (O) NPV curve
                        TotActInvest)          ! (O) Total actualized investment


      return

end subroutine city