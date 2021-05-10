
! Solver subroutine
! Solving all locations on a hourly basis, computing aggregated balances, calling economic analysis
! Lubello, Carcasci: dic 2019


subroutine solver(deltaEEelectr,sourceEEelectr,sinkEEelectr,    & ! Single location hourly energy balances
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
                  EnBoughtOutAggr,        &
                  EnSoldOutAggr,          &
                  GasConsumpAggr)


      USE MODcoord,       ONLY: iyear,itime,iloc,itimeperweek,itimeperday
      USE MODGlobalParam, ONLY: iTimeStart,iTimeEnd,IndAgeing
      USE MODparam,       ONLY: NdayYear,NhourDay,NhourWeek,NhourYear,Nyears,Nstep,MaxComp,Maxloc,MaxGiorni,MaxHours
      USE MODambient,     ONLY: TempAmb,AirDens,SolIrr,WindSpeed,RelHumidity
      USE MODAggr,        ONLY: Nloc, locName

      implicit real(8) (a-h,o-z), integer(i-n)

      real(8), dimension(Maxloc,NhourYear), intent(  OUT) :: deltaEEelectr,sourceEEelectr,sinkEEelectr
      real(8), dimension(Maxloc,NhourYear), intent(  OUT) :: deltaEEheatAmb,sourceEEheatAmb,sinkEEheatAmb
      real(8), dimension(Maxloc,NhourYear), intent(  OUT) :: deltaEEheatWat,sourceEEheatWat,sinkEEheatWat
      real(8), dimension(Maxloc,NhourYear), intent(  OUT) :: deltaEEcoolAmb,sourceEEcoolAmb,sinkEEcoolAmb

      real(8), dimension(NhourYear), intent(  OUT) :: deltaEEelectrAggr,sourceEEelectrAggr,sinkEEelectrAggr
      real(8), dimension(NhourYear), intent(  OUT) :: deltaEEheatAmbAggr,sourceEEheatAmbAggr,sinkEEheatAmbAggr
      real(8), dimension(NhourYear), intent(  OUT) :: deltaEEheatWatAggr,sourceEEheatWatAggr,sinkEEheatWatAggr
      real(8), dimension(NhourYear), intent(  OUT) :: deltaEEcoolAmbAggr,sourceEEcoolAmbAggr,sinkEEcoolAmbAggr

      real(8), intent(  OUT) :: deltaEEelectrAggrYear,sourceEEelectrAggrYear,sinkEEelectrAggrYear   
      real(8), intent(  OUT) :: deltaEEheatAmbAggrYear,sourceEEheatAmbAggrYear,sinkEEheatAmbAggrYear  
      real(8), intent(  OUT) :: deltaEEheatWatAggrYear,sourceEEheatWatAggrYear,sinkEEheatWatAggrYear  
      real(8), intent(  OUT) :: deltaEEcoolAmbAggrYear,sourceEEcoolAmbAggrYear,sinkEEcoolAmbAggrYear  

      real(8),                                      intent(  OUT) :: TotActInvest                                                            
      real(8),       dimension(0:MaxGiorni),        intent(  OUT) :: curveNPV
      real(8),       dimension(NhourYear),          intent(  OUT) :: EnBoughtOutAggr,EnSoldOutAggr,GasConsumpAggr

      integer,       dimension(            Maxloc) :: NValData
      real(8),       dimension(100,MaxComp,Maxloc) :: ValData

      real(8),       dimension( 13,MaxComp,Maxloc) :: DataEconAn

      real(8),       dimension(            Maxloc) :: EnBoughtOut,EnSoldOut,GasConsump                                                                  
      real(8),       dimension(            Maxloc) :: enExpend,enExpendGas                                                               
      real(8),       dimension(MaxHours)           :: enExpendComplete,enExpendGasComplete                                                             
      real(8),       dimension(NhourYear)          :: enExpendAggr,enExpendGasAggr



      ! 1) Solving the energy system
      !    -------------------------

      ! Aggregated locations, whole year balances initialization 

      deltaEEelectrAggrYear  = 0.d0
      sourceEEelectrAggrYear = 0.d0
      sinkEEelectrAggrYear   = 0.d0

      deltaEEheatAmbAggrYear  = 0.d0
      sourceEEheatAmbAggrYear = 0.d0 
      sinkEEheatAmbAggrYear   = 0.d0

      deltaEEheatWatAggrYear  = 0.d0
      sourceEEheatWatAggrYear = 0.d0 
      sinkEEheatWatAggrYear   = 0.d0

      deltaEEcoolAmbAggrYear  = 0.d0
      sourceEEcoolAmbAggrYear = 0.d0 
      sinkEEcoolAmbAggrYear   = 0.d0

      EnBoughtOutAggrYear= 0.d0
      EnSoldOutAggrYear  = 0.d0
      enExpendAggrYear   = 0.d0
      enExpendGasAggrYear= 0.d0

   
      do 4000 iyear=1,30 ! Years passing (only if working with battery ageing)
        write(*,*) "iyear iteration: ", iyear
        
        do 3000 itime=iTimeStart,iTimeEnd ! Hours of the year passing
        
          ! Initializing hour of the day and hour of the week counting
          if((iyear.eq.1 .and. itime.eq.1) .or. itimeperday.eq.NhourDay)then
            itimeperday = 0
          else
          endif
          
          if((iyear.eq.1 .and. itime.eq.1) .or. itimeperweek.eq.NhourWeek)then
            itimeperweek = 0
          else
          endif
        
          ! Counting hour of the day and hour of the week
          itimeperweek = itimeperweek + 1
          itimeperday  = itimeperday  + 1
        
          ! Aggregated locations, hourly balances initialization
        
          deltaEEelectrAggr   (itime) = 0.d0
          sourceEEelectrAggr  (itime) = 0.d0
          sinkEEelectrAggr    (itime) = 0.d0
                               
          deltaEEheatAmbAggr  (itime) = 0.d0
          sourceEEheatAmbAggr (itime) = 0.d0
          sinkEEheatAmbAggr   (itime) = 0.d0
                               
          deltaEEheatWatAggr  (itime) = 0.d0
          sourceEEheatWatAggr (itime) = 0.d0
          sinkEEheatWatAggr   (itime) = 0.d0
                               
          deltaEEcoolAmbAggr  (itime) = 0.d0
          sourceEEcoolAmbAggr (itime) = 0.d0
          sinkEEcoolAmbAggr   (itime) = 0.d0
        
          EnBoughtOutAggr     (itime) = 0.d0
          EnSoldOutAggr       (itime) = 0.d0
          enExpendAggr        (itime) = 0.d0
          enExpendGasAggr     (itime) = 0.d0
          GasConsumpAggr      (itime) = 0.d0        
        
          do 1000 iloc=1,Nloc ! Locations cycle
        
            call location(TempAmb(itime),AirDens(itime),SolIrr(itime),WindSpeed(itime),RelHumidity, & ! (I) Ambient data 
        
                          locName  (iloc),     &
        
                          deltaEEelectr(iloc,itime),   & 
                          sourceEEelectr(iloc,itime),  & 
                          sinkEEelectr(iloc,itime),    & 
                          
                          deltaEEheatAmb(iloc,itime),  & 
                          sourceEEheatAmb(iloc,itime), & 
                          sinkEEheatAmb(iloc,itime),   & 
                          
                          deltaEEheatWat(iloc,itime),  & 
                          sourceEEheatWat(iloc,itime), & 
                          sinkEEheatWat(iloc,itime),   & 
                          
                          deltaEEcoolAmb(iloc,itime),  & 
                          sourceEEcoolAmb(iloc,itime), & 
                          sinkEEcoolAmb(iloc,itime),   & 
        
                          GasConsump        (iloc),    &
        
                          EnBoughtOut(iloc),           &
                          EnSoldOut  (iloc),           &
                          enExpend   (iloc),           &
                          enExpendGas(iloc),           &
                          DataEconAn (1,1,iloc),       &
        
                          NValData       (iloc),       &
                          ValData    (1,1,iloc))
            
            ! Aggregated locations, hourly balances
        
            deltaEEelectrAggr   (itime) = deltaEEelectrAggr   (itime) + deltaEEelectr   (iloc,itime)
            sourceEEelectrAggr  (itime) = sourceEEelectrAggr  (itime) + sourceEEelectr  (iloc,itime)
            sinkEEelectrAggr    (itime) = sinkEEelectrAggr    (itime) + sinkEEelectr    (iloc,itime)
                                                              
            deltaEEheatAmbAggr  (itime) = deltaEEheatAmbAggr  (itime) + deltaEEheatAmb  (iloc,itime)
            sourceEEheatAmbAggr (itime) = sourceEEheatAmbAggr (itime) + sourceEEheatAmb (iloc,itime)
            sinkEEheatAmbAggr   (itime) = sinkEEheatAmbAggr   (itime) + sinkEEheatAmb   (iloc,itime)
                                                              
            deltaEEheatWatAggr  (itime) = deltaEEheatWatAggr  (itime) + deltaEEheatWat  (iloc,itime)
            sourceEEheatWatAggr (itime) = sourceEEheatWatAggr (itime) + sourceEEheatWat (iloc,itime)
            sinkEEheatWatAggr   (itime) = sinkEEheatWatAggr   (itime) + sinkEEheatWat   (iloc,itime)
                                                              
            deltaEEcoolAmbAggr  (itime) = deltaEEcoolAmbAggr  (itime) + deltaEEcoolAmb  (iloc,itime)
            sourceEEcoolAmbAggr (itime) = sourceEEcoolAmbAggr (itime) + sourceEEcoolAmb (iloc,itime)
            sinkEEcoolAmbAggr   (itime) = sinkEEcoolAmbAggr   (itime) + sinkEEcoolAmb   (iloc,itime)
        
            enExpendAggr        (itime) = enExpendAggr        (itime) + enExpend        (iloc)
            enExpendGasAggr     (itime) = enExpendGasAggr     (itime) + enExpendGas     (iloc)
        
            GasConsumpAggr      (itime) = GasConsumpAggr      (itime) + GasConsump      (iloc)
            EnBoughtOutAggr     (itime) = EnBoughtOutAggr     (itime) + EnBoughtOut     (iloc)
            EnSoldOutAggr       (itime) = EnSoldOutAggr       (itime) + EnSoldOut       (iloc)
        
1000      continue ! Locations cycle
        
          ! Aggregated locations, whole year balances
        
          deltaEEelectrAggrYear   = deltaEEelectrAggrYear   + deltaEEelectrAggr   (itime)
          sourceEEelectrAggrYear  = sourceEEelectrAggrYear  + sourceEEelectrAggr  (itime)
          sinkEEelectrAggrYear    = sinkEEelectrAggrYear    + sinkEEelectrAggr    (itime)
        
          deltaEEheatAmbAggrYear  = deltaEEheatAmbAggrYear  + deltaEEheatAmbAggr  (itime)
          sourceEEheatAmbAggrYear = sourceEEheatAmbAggrYear + sourceEEheatAmbAggr (itime)
          sinkEEheatAmbAggrYear   = sinkEEheatAmbAggrYear   + sinkEEheatAmbAggr   (itime)
                                                                                  
          deltaEEheatWatAggrYear  = deltaEEheatWatAggrYear  + deltaEEheatWatAggr  (itime)
          sourceEEheatWatAggrYear = sourceEEheatWatAggrYear + sourceEEheatWatAggr (itime)
          sinkEEheatWatAggrYear   = sinkEEheatWatAggrYear   + sinkEEheatWatAggr   (itime)
                                                                                  
          deltaEEcoolAmbAggrYear  = deltaEEcoolAmbAggrYear  + deltaEEcoolAmbAggr  (itime)
          sourceEEcoolAmbAggrYear = sourceEEcoolAmbAggrYear + sourceEEcoolAmbAggr (itime)
          sinkEEcoolAmbAggrYear   = sinkEEcoolAmbAggrYear   + sinkEEcoolAmbAggr   (itime)
                                                             
          EnBoughtOutAggrYear     = EnBoughtOutAggrYear     + EnBoughtOutAggr     (itime)
          EnSoldOutAggrYear       = EnSoldOutAggrYear       + EnSoldOutAggr       (itime)
          enExpendAggrYear        = enExpendAggrYear        + enExpendAggr        (itime)
          enExpendGasAggrYear     = enExpendGasAggrYear     + enExpendGasAggr     (itime)
        
3000    continue ! Hours of the year do cycle

        if(IndAgeing .eq. 0)then
          goto 4010
        else
        endif
        
        istart = int((iyear-1)*365*24+1)
        iend   = int((iyear-1)*365*24+365)
        
        enExpendComplete   (istart:iend) = enExpendAggr
        enExpendGasComplete(istart:iend) = enExpendGasAggr
      
4000  continue ! Years do cycle      



      ! 2) Economic analysis
      !    -----------------

4010  if(IndAgeing .eq. 0)then
        do ii = 1,30
          istart = int((ii-1)*365*24+1)
          iend   = int((ii-1)*365*24+365)
          enExpendComplete   (istart:iend) = enExpendAggr
          enExpendGasComplete(istart:iend) = enExpendGasAggr
        enddo
      else
      endif

      call econanalysis(enExpendComplete,    & ! (I) Electricity expenditure at each time step
                        enExpendGasComplete, & ! (I) Gas expenditure at each time step
                        DataEconAn,          & ! (I) Components,life expectancy, capex exp
                        curveNPV,            & ! (O) NPV curve
                        TotActInvest)          ! (O) Total actualized investment


      return

end subroutine solver