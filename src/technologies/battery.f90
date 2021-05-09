
! Simple battery model
! Lubello: jan 2020

subroutine SimpleBattery(CapacityNom,    & ! (I)  Capacity of the battery     [kWh]
                         ChargeMax,      & ! (I)  Battery maximum SoC         [-] default: 0.95
                         DoD,            & ! (I)  Depth Of Discharge          [-] default: 1.00
                         etaBatt1,       & ! (I)  Efficiency of the battery   [-] default: 0.95 hp1. cost, hp2. chargeff=dischargeff
                         CostSimpBatt,   & ! (I)  Cost per kWh simple battery [eur/kWh]
                         deltaProdDem,   & ! (IO) Delta production - demand   [kWh]
                         SoC,            & ! (IO) State of Charge             [-] default: 0.50
                         deltaBatt,      & ! (O)  Delta charging of the battery in timestep [kWh]
                         CapexSimpBatt)    ! (O)  Battery cost                [eur]


      USE MODcoord,       ONLY: icase,iyear,itime,iloc,itimeperweek,itimeperday
      USE MODambient,     ONLY: TempAmb
      USE MODParam,       ONLY: timestep,NhourDay,NhourWeek,NhourYear,Runiv,eNepero
      USE MODGlobalParam, ONLY: IndAgeing
      USE MODbattery,     ONLY: ccyc,ccal,csum,tref_cal,SoHBattery,WeeklyChargeHist,SOHtemp,SOHtempbis,ccal2,totalccal,totalccyc,iswitch


      implicit real(8) (a-h,o-z), integer(i-n)

      real(8), intent(IN   ) :: CapacityNom,ChargeMax,DoD,etaBatt1,CostSimpBatt
      real(8), intent(INOUT) :: deltaProdDem,SoC
      real(8), intent(  OUT) :: deltaBatt,CapexSimpBatt

      real(8) :: Charge,ChargeMin


      ! If battery capacity = 0 then skip everything

      if(CapacityNom.eq.0.d0)then
        SoC           = 0.d0
        deltaBatt     = 0.d0
        CapexSimpBatt = 0.d0
        goto 1000
      else
      endif

      Capacity       = CapacityNom*SoHBattery(iloc) ! [kWh]
      StartingCharge = Capacity*SoC                   ! [kWh] Charge at beginning if timestep
      SoCInitial     = StartingCharge/Capacity        ! [-] Initial SoC
      ChargeMin      = 1.d0 - DoD                     ! [-]


      if (deltaProdDem .ge. 0.d0) then
        ! More energy produced than consumed -> Battery charging or selling (if full)


        ! etaBatt function of Crate - charging
        ! >>> WIP: to be improved <<<
        CRate = deltaProdDem/Capacity
        if(CRate .le. 0.2d0)then
          etaBatt = 1.d0
        else
          etaBatt = -0.06d0*CRate+1.011d0
        endif
        
        if(etaBatt .le. 0.95d0)then
          etaBatt = 0.95d0
        else
        endif
        ! >>> WIP: to be improved <<<


        Charge = StartingCharge + deltaProdDem*etaBatt ! [kWh] Temp battery charge level, might be > Capacity*SoCmax

        if (Charge .gt. ChargeMax*Capacity) then
          ! Battery full, selling
          deltaProdDem = (Charge - ChargeMax*Capacity)/etaBatt
          Charge       =  ChargeMax*Capacity
        else
          ! Battery charging
          deltaProdDem =  0.d0
        endif

      else
        ! Less energy produced than consumed -> Battery discharging or buying (if empty)


        ! etaBatt function of Crate - discharging
        ! >>> WIP: to be improved <<<
        CRate = -deltaProdDem/Capacity
        etaBatt = -0.0535d0*CRate+0.999d0
        
        if(etaBatt .le. 0.95d0)then
          etaBatt = 0.84d0
        else
        endif
        ! >>> WIP: to be improved <<<


        Charge = StartingCharge + deltaProdDem/etaBatt ! [kWh] Temp battery charge level, might be < Capacity*SoCmin

        if (Charge .lt. ChargeMin*Capacity) then
          ! Battery empty, buying
          deltaProdDem = (Charge - ChargeMin*Capacity)*etaBatt
          Charge       =  ChargeMin*Capacity
        else
          ! Battery discharging
          deltaProdDem =  0.d0
        endif

      endif

      ! Final and average state of charge
      SoC = Charge/Capacity
      SoCavg = (SoCInitial+SoC)/2.d0

      ! Energy released or accumulated
      ! Released = discharging = deltaBatt>0 | Accumulated = charging = deltaBatt<0
      deltaBatt = StartingCharge - Charge

      ! Battery cost
      CapexSimpBatt = CostSimpBatt * CapacityNom



      !------------ Ageing -----------------


      ! Battery temperature (is it ok to consider it = to ambient where the battery is stored temperature?)
      TempModif     = 0.714d0*TempAmb(itime)  + 4.28d0 !-20 a 50 -> -10 a 40 i.e. esterno/garage non condizionato
      !TempModif     = 0.1429*TempAmb(itime) + 17.857 !-20 a 50 -> 15 a 25 i.e. interno
      TempModifKelv = TempModif + 273.15d0

      ! Calendar (shelf) ageing parameters
      Ea      = 61.082d0
      T0      = 45.d0+273.15d0
      U0      = 3.6970d0
      alfaTaf = -1.d0 !0.0323 !0.798d0
      FTaf    = 96485.3365d0
      aCal    = 0.003503 !3.2940d-6!6.6269d-4

      ! Formula 3 Laisuo et al.
      aa = dexp(-132.52d0*SoCAvg**4 + 428.67d0*SoCAvg**3 - 531.07d0*SoCAvg**2 + 294.29d0*SoCAvg - 42.42d0)
      bb = 42869.d0*SoCAvg**4 - 138698.d0*SoCAvg**3 + 171637.d0*SoCAvg**2 - 94294.d0*SoCAvg + 11333.d0
      RateCapacityFade = aa*dexp(bb/TempModifKelv)

      ! Formula 11 Laisuo et al.
      alpha0Temp = 0.000323d0
      Templambda = 3586.3d0
      alphaTemp = alpha0Temp*eNepero**(Templambda/TempModifKelv)


      ! Option 1 - No calendar ageing
      !ccal1 = 0.d0 
      ! Option 2 - Always calendar ageing
      ccal1 = (RateCapacityFade * (1.d0 + 1.d0 - SOHBattery(iloc))**(-alphaTemp)) * timestep*10.d0/24.d0
      ! Option 3 - Calendar ageing only when no cycle ageing
      !if(SoC.eq.SoCInitial)then
      !  ccal1 = (RateCapacityFade * (1.d0 + 1.d0 - SOHBattery(iloc))**(-alphaTemp)) * timestep*10.d0/24.d0
      !else
      !  ccal1 = 0.d0
      !endif

      ccal1 = ccal1 / 2.85d0 !* 4.d0 / 1.d3 /(4.d0*2.85d0) ![kWh]

      if(ccal1.gt.0.d0)then
        ccal2 = ccal2 + ccal1
      else
      ! no calendar ageing for such conditions (T, SoC)
      ! ParTempSoC would result negative, hance SoH would improve
      endif



      ! Storing weekly charge history and calling battery ageing once a week
      
      if(IndAgeing.eq.1 .and. icase.eq.1) then  ! icase = 1 dovrebbe essere ridondante, se non è 0 non siamo qui dentro
        ! Ageing active and not baseline case, then storing SoC for each week hour
        WeeklyChargeHist(itimeperweek,iloc) = SoC

        if(itimeperweek .eq. NhourWeek) then
          ! A week has passed and has been stored, call battery cyclic ageing
          call batteryageing(WeeklyChargeHist(:,iloc),ccyc(iloc),Damage)
          WeeklyChargeHist(:,iloc) = 0.d0 ! Reinitializing weekly charge history

          if(itimeperday .eq. NhourDay)then
            ccal(iloc) = ccal(iloc)+ccal2
            ccal2 = 0.d0    
          else
          endif  

        else
        endif
      
      else
      endif

      ! Save ccal and ccyc contributions to first substituition and save results
      if(iswitch .ne. 1)then
        if((ccyc(iloc)*0.3d0 + ccal(iloc)) .gt. 0.3d0)then
          totalccal=ccal(iloc)
          totalccyc=ccyc(iloc)*0.3d0
          iswitch=1
        else
        endif
      else
      endif

      SOHBattery(iloc) = 1.d0 - ccyc(iloc)*0.3d0 - ccal(iloc) 


      ! >>>TEMP: grafici
      tempNumber = (itime+(iyear-1)*Nhouryear)/(Nhourweek)
      if(tempNumber - dble(int(tempNumber)) .lt. 0.00001)then
        SOHtempbis(int(tempNumber)+1) = SOHBattery(iloc)
      else
      endif

      SOHtemp(itime+(iyear-1)*Nhouryear)=SOHBattery(iloc)
      ! >>>TEMP: grafici




1000  return

end subroutine SimpleBattery