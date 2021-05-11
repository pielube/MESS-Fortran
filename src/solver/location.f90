
! Location subroutine
! Lubello, Carcasci: dic 2019

subroutine location(TempAmb,         & !(I) Ambient temperature [°C]
                    AirDens,         & !(I) Air density         [kg/m3]
                    SolIrr,          & !(I) Solar irradiance    [W/m2]
                    WindSpeed,       & !(I) Wind speed          [m/s]
                    RelHumdity,      & !(I) Relative humidity   [-]
                    locName,         & !(I) location name
                                     
                    deltaEEelectr,   & !(O) Electrical energy balance  [kWh]
                    sourceEEelectr,  & !(O) Electrical energy produced [kWh]
                    sinkEEelectr,    & !(O) Electrical energy consumed [kWh]

                    deltaEEheatAmb,  & !(O) Ambient heat energy balance  [kWh]
                    sourceEEheatAmb, & !(O) Ambient heat energy produced [kWh]
                    sinkEEheatAmb,   & !(O) Ambient heat energy consumed [kWh]

                    deltaEEheatWat,  & !(O) Hot water energy balance  [kWh]
                    sourceEEheatWat, & !(O) Hot water energy produced [kWh]
                    sinkEEheatWat,   & !(O) Hot water energy consumed [kWh]

                    deltaEEcoolAmb, & !(O) Cooling energy balance   [kWh]
                    sourceEEcoolAmb, & !(O) Cooling energy produced [kWh]
                    sinkEEcoolAmb,   & !(O) Cooling energy consumed [kWh]

                    GasConsump,      & !(O) Natural gas consumption [Sm3]

                    EnBoughtOut,     & !(O) Electrical energy bought [kWh]
                    EnSoldOut,       & !(O) Electrical energy sold   [kWh]
                    enExpend,        & !(O) Expenditure for electrical energy [eur]
                    enExpendGas,     & !(O) Expenditure for natural gas       [eur]

                    DataEconAn,      & !(O) Data necessary for economic analysis

                    NValData,        & !(IO) Number of components in the location
                    ValData)           !(IO) 2-D array of columns->components rows->technology-specific parameters


      USE MODcoord,         ONLY: icase,iyear,itime,iloc
      USE MODGlobalParam,   ONLY: IndAgeing
      USE MODparam,         ONLY: NhourYear,MaxComp
      USE MODprices,        ONLY: elenPriceBuy,elenPriceSell,gasPriceHouseS               ! >>> WIP: see prices input
      USE MODbattery,       ONLY: SoHBattery,ccyc,ccal,csum,tref_cal,ccal2,TempArrBattDeg ! >>> WIP: should be in battery's subroutine
      USE MODAddHourlyData, ONLY: AddHourlyData
      USE MODlocation,      ONLY: Nelem
      USE MODwhichtechs,    ONLY: whichtechs

      implicit real(8) (a-h,o-z), integer(i-n)

      ! Argument declarations

      real(8),                               intent(IN   ) :: TempAmb,AirDens,SolIrr,WindSpeed
      character(15),                         intent(IN   ) :: locName

      real(8),                               intent(  OUT) :: deltaEEelectr,sourceEEelectr,sinkEEelectr
      real(8),                               intent(  OUT) :: deltaEEheatAmb,sourceEEheatAmb,sinkEEheatAmb
      real(8),                               intent(  OUT) :: deltaEEheatWat,sourceEEheatWat,sinkEEheatWat
      real(8),                               intent(  OUT) :: deltaEEcoolAmb,sourceEEcoolAmb,sinkEEcoolAmb

      real(8),                               intent(  OUT) :: GasConsump
                                                        
      real(8),                               intent(  OUT) :: EnBoughtOut,EnSoldOut
      real(8),                               intent(  OUT) :: enExpend,enExpendGas
      real(8),       dimension( 13,MaxComp), intent(  OUT) :: DataEconAn

      integer,                               intent(INOUT) :: NValData
      real(8),       dimension(100,MaxComp), intent(INOUT) :: ValData

      ! Local declarations

      integer,      dimension(MaxComp) :: IndCase
      character(15)                    :: locNameTemp
      character(20)                    :: StringAux

      ! Various inizializations

      ! electricity

      deltaEEelectr  = 0.d0
      sourceEEelectr = 0.d0
      sinkEEelectr   = 0.d0 
   
      ! heat                    

      deltaEEheatAmb      = 0.d0
      deltaEEheatWat      = 0.d0
      !deltaEEheatProcess  = 0.d0
      !deltaEEheatOther    = 0.d0

      sourceEEheatAmb     = 0.d0
      sourceEEheatWat     = 0.d0
      !sourceEEheatProcess = 0.d0
      !sourceEEheatOther   = 0.d0

      sinkEEheatAmb       = 0.d0
      sinkEEheatWat       = 0.d0
      !sinkEEheatProcess   = 0.d0
      !sinkEEheatOther     = 0.d0

      ! cooling

      deltaEEcoolAmb      = 0.d0
      sourceEEcoolAmb     = 0.d0
      sinkEEcoolAmb       = 0.d0

      ! gas
      !deltaEEgas   = 0.d0
      GasConsump = 0.d0

      ! economics
      EnBoughtOut       = 0.d0
      EnSoldOut         = 0.d0
      enExpend          = 0.d0
      enExpendGas       = 0.d0



      ! Reading of which elements is composed the location and their input data
      ! ------------------------------------------------------------------------

      call estens(locName,locNameTemp,'dat')

      if(itime.eq.1) then ! Reading only at first iteration (values stored in NValData,ValData)

        open(2,file=locNameTemp,status='old',err=7000)
        iline=0
        rewind 2

        read(2,*,err=7010) Nelem(iloc)   ! Number of elements composing the unit
        iline=iline+1

        ! Searching elements composing the unit, through their string

        do 500 ii=1,Nelem(iloc)
          iline=iline+1
          read(2,*,err=7010) StringAux   ! Name of the element

          if    (StringAux(1:8).eq.'[DemWel]')  then
            ! String found: electrical demand
            ! There is electrical demand! Saving ID and numb of inputs:
            ValData   (1,ii) = 01 !ID
            DataEconAn(1,ii) = 01 !ID
            IndCase   (  ii) =  0 ! 0 => simulated when icase = 0
            Ndat=3  ! Number of inputs
            whichtechs(iloc,ii) = '01'

          elseif(StringAux(1:8).eq.'[DemQth]')  then
            ! String found: thermal demand
            ! There is thermal demand! Saving ID and numb of inputs:
            ValData   (1,ii) = 02 !ID
            DataEconAn(1,ii) = 02 !ID
            IndCase   (  ii) =  0 ! 0 => simulated when icase = 0
            Ndat=6  ! Number of inputs
            whichtechs(iloc,ii) = '02'


          elseif(StringAux(1:11).eq.'[SolPhoto]') then
            ! String found: Photvoltaic array
            ! Photovoltaic panels exist! Saving ID and numb of inputs:
            ValData   (1,ii) = 03 !ID
            DataEconAn(1,ii) = 03 !ID
            IndCase   (  ii) =  1 ! 1 => not simulated when icase = 0
            Ndat=4
            whichtechs(iloc,ii) = '03'


          elseif(StringAux(1:11).eq.'[WindTurb]') then
            ! String found: Wind turbine
            ! Wind turbine exists! Saving ID and numb of inputs:
            ValData(1,ii)    = 04 !ID
            DataEconAn(1,ii) = 04 !ID
            IndCase   (  ii) =  1 ! 1 => not simulated when icase = 0
            Ndat=7 ! Number of inputs
            whichtechs(iloc,ii) = '04'


          elseif(StringAux(1:9).eq.'[SolColl]')  then
            ! String found: Solar collector
            ! Solar collector exists! Saving ID and numb of inputs:
            ValData(1,ii)    = 05 !ID
            DataEconAn(1,ii) = 05 !ID
            IndCase   (  ii) =  1 ! 1 => not simulated when icase = 0
            Ndat=7 ! Number of inputs
            whichtechs(iloc,ii) = '05'


          elseif(StringAux(1:9).eq.'[AirCond]')   then
            ! String found: air conditioner / heat pump
            ! Boiler exists! Saving ID and numb of inputs:
            ValData(1,ii)    = 06 !ID
            DataEconAn(1,ii) = 06 !ID
            IndCase   (  ii) =  0 ! 0 => simulated when icase = 0
            Ndat=3 ! Number of inputs
            whichtechs(iloc,ii) = '06'


          elseif(StringAux(1:10).eq.'[HeatPump]')   then
            ! String found: Heat pump
            ! Boiler exists! Saving ID and numb of inputs:
            ValData(1,ii)    = 07 !ID
            DataEconAn(1,ii) = 07 !ID
            IndCase   (  ii) =  1 ! 1 => not simulated when icase = 0
            Ndat=3 ! Number of inputs
            whichtechs(iloc,ii) = '07'


          elseif(StringAux(1:8).eq.'[HVACel]')   then
            ! String found: Heat pump
            ! Boiler exists! Saving ID and numb of inputs:
            ValData(1,ii)    = 08 !ID
            DataEconAn(1,ii) = 08 !ID
            IndCase   (  ii) =  1 ! 1 => not simulated when icase = 0
            Ndat=10 ! Number of inputs
            whichtechs(iloc,ii) = '08'


          elseif(StringAux(1:9).eq.'[HVACgas]')   then
            ! String found: Gas Heat pump
            ! Boiler exists! Saving ID and numb of inputs:
            ValData(1,ii)    = 09 !ID
            DataEconAn(1,ii) = 09 !ID
            IndCase   (  ii) =  1 ! 1 => not simulated when icase = 0
            Ndat=12 ! Number of inputs
            whichtechs(iloc,ii) = '09'


          elseif(StringAux(1:11).eq.'[ElChiller]')   then
            ! String found: Standard boiler
            ! Boiler exists! Saving ID and numb of inputs:
            ValData(1,ii)    = 10 !ID
            DataEconAn(1,ii) = 10 !ID
            IndCase   (  ii) =  1 ! 1 => not simulated when icase = 0
            Ndat=6 ! Number of inputs
            whichtechs(iloc,ii) = '10'


          elseif(StringAux(1:11).eq.'[SimpBatt]') then
            ! String found: Simple battery
            ! Simple battery exists! Saving ID and numb of inputs:
            ValData(1,ii)    = 11 !ID
            DataEconAn(1,ii) = 11 !ID
            IndCase   (  ii) =  1 ! 1 => not simulated when icase = 0
            Ndat=7 ! Number of inputs
            whichtechs(iloc,ii) = '11'


          elseif(StringAux(1:16).eq.'[HydrogenSystem]') then
            ! String found: Hydrogen system (electrolyzer, tank, fuel cell)
            ! Simple battery exists! Saving ID and numb of inputs:
            ValData(1,ii)    = 12 !ID
            DataEconAn(1,ii) = 12 !ID
            IndCase   (  ii) =  1 ! 1 => not simulated when icase = 0
            Ndat=20 ! Number of inputs
            whichtechs(iloc,ii) = '12'


          elseif(StringAux(1:6).eq.'[CCHP]')   then
            ! String found: Standard boiler
            ! Boiler exists! Saving ID and numb of inputs:
            ValData(1,ii)    = 13 !ID
            DataEconAn(1,ii) = 13 !ID
            IndCase   (  ii) =  1 ! 1 => not simulated when icase = 0
            Ndat=13 ! Number of inputs
            whichtechs(iloc,ii) = '13'


          elseif(StringAux(1:8).eq.'[Boiler]')   then
            ! String found: Standard boiler
            ! Boiler exists! Saving ID and numb of inputs:
            ValData(1,ii)    = 14 !ID
            DataEconAn(1,ii) = 14 !ID
            IndCase   (  ii) =  0 ! 0 => simulated when icase = 0
            Ndat=2 ! Number of inputs
            whichtechs(iloc,ii) = '14'


          else
            ! String not found
            write(*,*) 'String not found: input error!'
            stop 'String error!'
          endif


          ! Reading input data of each element

          do idat=1,Ndat
            iline=iline+1
            read(2,*,err=7010) ValData(idat+1,ii) 
          enddo

 500    continue ! End of do cycle with ii

        close(2)

        NValData=Nelem(iloc) ! Number of components

      endif


      ! Solving the location
      ! ====================

      Nelem(iloc)=NValData ! Number of components

      ! Searching elements composing the unit
      do 1000 ii=1,Nelem(iloc)

        if(indCase(ii).gt.icase) then
          ! Skipping compoennts not to be simulated in case 0
          DataEconAn(2,ii) = 0.d0 
          DataEconAn(3,ii) = 0.d0   !capex
          goto 1000
        endif


        ID=ValData(1,ii) !ID component of location

        select case(ID)

        case(01) ! [DemWel]

          Indtype=nint(ValData(2,ii))
          IndWel =nint(ValData(3,ii))
          Welinp =     ValData(4,ii)

          call DemandWel(Indtype,   & ! (I) Which profile to choose in demand_Wel.dat
                         IndWel,    & ! (I) Which ref value to consider: 1 Max power [kW], 2 Mean power [kW], 3 Yealy en consump [kWh/year]
                         Welinp,    & ! (I) Actual ref value (demand scaled accordingly)
                         WWel1)       ! (O) Energy demand at given timestep [kWh]

          sinkEEelectr   = sinkEEelectr   + WWel1
          deltaEEelectr = sourceEEelectr - sinkEEelectr

          AddHourlyData(iloc,ii,1,itime) = WWel1

          DataEconAn(2,ii) = 0.d0
          DataEconAn(3,ii) = 0.d0


        case(02) ! [DemQth]

          IndQth          =nint(ValData(2,ii))
          QQAmbHeatIn     =     ValData(3,ii)
          QQWatHeatIn     =     ValData(4,ii)
          QQProcessHeatIn =     ValData(5,ii)
          QQAmbCoolIn     =     ValData(6,ii)
          QQOtherHeatIn   =     ValData(7,ii)

          call DemandTherm(IndQth,                                                              & ! (I) Which ref value to consider: 1 Max power [kW], 2 Mean power [kW], 3 Yealy en consump [kWh/year]
                           QQAmbHeatIn,QQWatHeatIn,QQProcessHeatIn,QQAmbCoolIn,QQOtherHeatIn,   & ! (I) Actual ref value (demand scaled accordingly)
                           QQAmbHeat,  QQWatHeat,  QQProcessHeat,  QQAmbCool,  QQOtherHeat)       ! (O) Energy demand at given timestep [kWh]

          sinkEEheatAmb       = sinkEEheatAmb     + QQAmbHeat
          sinkEEheatWat       = sinkEEheatWat     + QQWatHeat
          sinkEEcoolAmb       = sinkEEcoolAmb     + QQAmbCool

          deltaEEheatAmb     = sourceEEheatAmb     - sinkEEheatAmb
          deltaEEheatWat     = sourceEEheatWat     - sinkEEheatWat
          deltaEEcoolAmb     = sourceEEcoolAmb     - sinkEEcoolAmb

          AddHourlyData(iloc,ii,1,itime) = QQAmbHeat
          AddHourlyData(iloc,ii,2,itime) = QQWatHeat
          AddHourlyData(iloc,ii,3,itime) = QQAmbCool

          DataEconAn(2,ii) = 0.d0
          DataEconAn(3,ii) = 0.d0 


        case(03) ! [SolPhoto]

          Area       =     ValData(2,ii)
          AreaFrac   =     ValData(3,ii)
          ConvEffMod =     ValData(4,ii)
          CostSolPhoto=    ValData(5,ii)

          call PhotoArrEq(Area,           & ! (I) Photovoltaic panel area               [m^2]
                          AreaFrac,       & ! (I) Area fraction with active solar cells [-]
                          SolIrr,         & ! (I) Solar irradiance                      [W/m^2]
                          ConvEffMod,     & ! (I) Module conversion efficiency          [-]
                          CostSolPhoto,   & ! (I) Cost per kW panels                    [eur/kW]
                          WWelPhoto,      & ! (O) AC Energy produced                    [kWh]
                          CapexSolPhoto)    ! (O) Capex Sol Photo                       [eur]

          sourceEEelectr = sourceEEelectr + WWelPhoto
          deltaEEelectr = sourceEEelectr - sinkEEelectr

          AddHourlyData(iloc,ii,1,itime) = WWelPhoto

          DataEconAn(2,ii) = 1.d0
          DataEconAn(3,ii) = CapexSolPhoto
          DataEconAn(4,ii) = 15.d0


        case(04) ! [WindTurb]

          SweptArea      =   ValData(2,ii)
          etaWindTurb    =   ValData(3,ii)
          RatedPower     =   ValData(4,ii)
          RatedWindSpeed =   ValData(5,ii)
          CutIn          =   ValData(6,ii)
          CutOut         =   ValData(7,ii)
          CostWindTurb   =   ValData(8,ii)

          call WindTurb(AirDens,          & ! (I) Air density                       [kg/m^3]
                        WindSpeed,        & ! (I) Wind speed                        [m/s]
                        SweptArea,        & ! (I) Swept area                        [m^2] e.g. 39.6 m^2 (Aircon 10/10 kW)
                        etaWindTurb,      & ! (I) Wind turbine efficiency           [-] default: 0.45 (ca. 0.593*0.76, i.e. Betz*efficiency)
                        RatedPower,       & ! (I) Rated power of the wind turbine   [kW]
                        RatedWindSpeed,   & ! (I) Rated wind speed                  [m/s] e.g. 11.0 m/s (Aircon 10/10 kW)
                        CutIn,            & ! (I) Cut in wind speed                 [m/s] e.g.  2.5 m/s (Aircon 10/10 kW)
                        CutOut,           & ! (I) Cout out wind speed               [m/s] e.g. 32.0 m/s (Aircon 10/10 kW)
                        CostWindTurb,     & ! (I) Cost per kW turbine               [eur/kW] default: 990 eur/kW from EnergyPlan Cost Database
                        WWelWind,         & ! (O) Energy produced                   [kWh]
                        CapexWindTurb)      ! (O) Capex wind turbine                [eur]

          sourceEEelectr = sourceEEelectr + WWelWind
          deltaEEelectr = sourceEEelectr - sinkEEelectr

          AddHourlyData(iloc,ii,1,itime) = WWelWind

          DataEconAn(2,ii) = 1.d0
          DataEconAn(3,ii) = CapexWindTurb
          DataEconAn(4,ii) = 20.d0


        case(05) ! [SolColl]

          Area        = ValData(2,ii)
          Fr          = ValData(3,ii)
          TauAlpha    = ValData(4,ii)
          Ul          = ValData(5,ii)
          Tin         = ValData(6,ii)
          CostSolColl = ValData(7,ii)
          SoCTank     = ValData(8,ii)

          QQdemSolColl = deltaEEheatWat ! Type of demand solar panels are used for
                                                                                          
          call SolarColl(Area,          &          ! (I)  Flat solar collector area       [m^2]
                         Fr,            &          ! (I)  Collector heat removal factor   [-] default: 0.75-0.80
                         SolIrr,        &          ! (I)  Solar irradiance                [W/m^2]
                         TauAlpha,      &          ! (I)  From eq. 5.9.3                  [-] deafault: 0.80
                         Ul,            &          ! (I)  Heat transfer coefficient       [W/(m^2*K)] realistic value: 9.5 (Duffie pg.294)
                         Tin,           &          ! (I)  Fluid temperature in            [K]
                         TempAmb,       &          ! (I)  Air temperature                 [K]
                         CostSolColl,   &          ! (I)  Cost per m^2 panels             [eur/m^2]
                         QQdemSolColl,  &          ! (I)  Energy demand                   [kWh]
                         SoCTank,       &          ! (IO) State of charge                 [-]
                         QQprodSolColl, &          ! (O)  Heat produced (might be stored) [kWh]
                         QQSolColl,     &          ! (O)  Heat out system (panels + tank) [kWh]
                         QQdiscSolColl, &          ! (O)  Heat discarded (would result in SoC > 1) [kWh]
                         CapexSolColl)             ! (O)  Capex                           [eur]

          ValData(8,ii) = SoCTank

          sourceEEheatWat     = sourceEEheatWat     + QQSolColl
          deltaEEheatWat     = sourceEEheatWat     - sinkEEheatWat

          AddHourlyData(iloc,ii,1,itime) = SoCTank
          AddHourlyData(iloc,ii,2,itime) = QQprodSolColl
          AddHourlyData(iloc,ii,3,itime) = QQSolColl
          AddHourlyData(iloc,ii,4,itime) = QQdiscSolColl

          DataEconAn(2,ii) = 1.d0
          DataEconAn(3,ii) = CapexSolColl
          DataEconAn(4,ii) = 25.d0


        case(06) ! [AirCond]

          COPc          = ValData(2,ii)
          QQcoolRatedP  = ValData(3,ii)
          CostAirCond   = ValData(4,ii)

          QQcoolDemAirCond = deltaEEcoolAmb

          call AirCond(COPc,               & ! (I) COP cooling             [-]
                       QQcoolRatedP,       & ! (I) Cooling rated power     [kW]
                       CostAirCond,        & ! (I) Cost  TEMP: capex       [eur]
                       QQcoolDemAirCond,   & ! (I) Cooling demand          [kWh]
                       QQcoolOutAirCond,   & ! (O) Cooling effect          [kWh]
                       ElEnConsAC,         & ! (O) Electricity consumption [kWh]
                       CapexAirCond)         ! (O) Capex                   [eur]

          ! electrical
          sinkEEelectr  = sinkEEelectr + ElEnConsAC
          deltaEEelectr = sourceEEelectr - sinkEEelectr

          ! thermal
          sourceEEcoolAmb  = sourceEEcoolAmb + QQcoolOutAirCond
          deltaEEcoolAmb   = sourceEEcoolAmb - sinkEEcoolAmb

          AddHourlyData(iloc,ii,1,itime) = QQcoolOutAirCond
          AddHourlyData(iloc,ii,2,itime) = ElEnConsAC

          DataEconAn(2,ii) = 1.d0
          DataEconAn(3,ii) = CapexAirCond
          DataEconAn(4,ii) = 20.d0


        case(07) ! [HeatPump]

          COPh          = ValData(2,ii)
          QQheatRatedP  = ValData(3,ii)
          CostHeatPump  = ValData(4,ii)

          QQheatDemHeatPump = deltaEEheatAmb

          call HeatPump(COPh,                & ! (I) COP heating             [-]
                        QQheatRatedP,        & ! (I) Heating rated power     [kW]
                        CostHeatPump,        & ! (I) Cost                    [eur/kW]
                        QQheatDemHeatPump,   & ! (I) Heat demand             [kWh]
                        QQheatOutHeatPump,   & ! (O) Heating effect          [kWh]
                        ElEnConsHP,          & ! (O) Electricity consumption [kWh]
                        CapexHeatPump)         ! (O) Capex                   [eur]

          ! electrical
          sinkEEelectr  = sinkEEelectr + ElEnConsHP
          deltaEEelectr = sourceEEelectr - sinkEEelectr

          ! thermal
          sourceEEheatAmb = sourceEEheatAmb + QQheatOutHeatPump
          deltaEEheatAmb  = sourceEEheatAmb - sinkEEheatAmb

          AddHourlyData(iloc,ii,1,itime) = QQheatOutHeatPump
          AddHourlyData(iloc,ii,2,itime) = ElEnConsHP

          DataEconAn(2,ii) = 1.d0
          DataEconAn(3,ii) = CapexHeatPump
          DataEconAn(4,ii) = 20.d0


        case(08) ! [HVACel]

          IndexHVACel     = ValData(2,ii)
          COPh_nom        = ValData(3,ii)
          COPc_nom        = ValData(4,ii)
          CapacityhP_nom  = ValData(5,ii)
          CapacityhP_max  = ValData(6,ii)
          CapacityhP_min  = ValData(7,ii)
          CapacitycP_nom  = ValData(8,ii)
          CapacitycP_max  = ValData(9,ii)
          CapacitycP_min  = ValData(10,ii)
          CostHVACel      = ValData(11,ii)

          ThBalHVACel     = deltaEEheatAmb
          CoolBalHVACel   = deltaEEcoolAmb

          call HVACel(IndexHVACel,       & ! (I) 0: both first heating, 1: both, first cooling 2: heating, 3: cooling
                      COPh_nom,          & ! (I) Heating COP nominal             [-]         
                      COPc_nom,          & ! (I) Cooling COP nominal             [-]
                      CapacityhP_nom,    & ! (I) Heating rated power nominal     [kW] 
                      CapacityhP_max,    & ! (I) Max Heating HP Power            [kW]
                      CapacityhP_min,    & ! (I) Min Heating HP Power            [kW]  
                      CapacitycP_nom,    & ! (I) Cooling rated power nominal     [kW] 
                      CapacitycP_max,    & ! (I) Max Cooling HP Power            [kW]
                      CapacitycP_min,    & ! (I) Min Cooling HP Power            [kW]  
                      TempAmb,           & ! (I) Outdoor temperature             [°C]
	                CostHVACel,        & ! (I) Cost                            [€/kW]
                      ThBalHVACel,       & ! (IO) Thermal energy balance [kWh]
                      CoolBalHVACel,     & ! (IO) Cooling effect balance [kWh]
	                ThProdHVACel,      & ! (O) Thermal energy production       [kWh]
		          CoolProdHVACel,    & ! (O) Cooling effect                  [kWh]
                      ElConsHVACel,      & ! (O) Electricity consumption real    [kWh]  
                      CapexHVACel)         ! (O) Capex                              [€]

          ! electrical
          sinkEEelectr   = sinkEEelectr + ElConsHVACel
          deltaEEelectr = sourceEEelectr - sinkEEelectr

          ! thermal - only for ambient heat
          sourceEEheatAmb = sourceEEheatAmb + ThProdHVACel
          deltaEEheatAmb  = sourceEEheatAmb - sinkEEheatAmb

          ! cooling - only for ambient cooling
          sourceEEcoolAmb = sourceEEcoolAmb + CoolProdHVACel
          deltaEEcoolAmb  = sourceEEcoolAmb - sinkEEcoolAmb

          AddHourlyData(iloc,ii,1,itime) = ThProdHVACel  
          AddHourlyData(iloc,ii,2,itime) = CoolProdHVACel
          AddHourlyData(iloc,ii,3,itime) = ElConsHVACel 

          DataEconAn(2,ii) = 1.d0
          DataEconAn(3,ii) = CapexHVACel
          DataEconAn(4,ii) = 15.d0

       case(09) ! [HVACgas]

          IndexHVACgas    = ValData(2,ii)
          IndexRecovery   = ValData(3,ii)
          PERh_nom        = ValData(4,ii)
          CapacityhP_nom  = ValData(5,ii)
          CapacityhP_max  = ValData(6,ii)
          CapacityhP_min  = ValData(7,ii)
          PERc_nom        = ValData(8,ii)
          CapacitycP_nom  = ValData(9,ii)
          CapacitycP_max  = ValData(10,ii)
          CapacitycP_min  = ValData(11,ii)
          Rec_nom         = ValData(12,ii)
          CostHVACgas     = ValData(13,ii)

          ThBalHVACgas    = deltaEEheatAmb
          CoolBalHVACgas  = deltaEEcoolAmb

          call HVACgas(IndexHVACgas,      & ! (I) 0: both first heating, 1: both, first cooling 2: heating, 3: cooling ! <<< WIP: 0,1 as a function on local regulations on HVAC
                       IndexRecovery,     & ! (I) 0: OFF 1: ON **
                       PERh_nom,          & ! (I) Heating PER nominal             [-]         
                       CapacityhP_nom,    & ! (I) Heating rated power nominal     [kW] 
                       CapacityhP_max,    & ! (I) Max HP Power                    [kW]
                       CapacityhP_min,    & ! (I) Min HP Power                    [kW] 
                       PERc_nom,          & ! (I) Cooling PER nominal             [-] 
                       CapacitycP_nom,    & ! (I) Cooling rated power nominal     [kW] 
                       CapacitycP_max,    & ! (I) Max Cooling HP Power            [kW]
                       CapacitycP_min,    & ! (I) Min Cooling HP Power            [kW]
                       Rec_nom,           & ! (I) Recovery heat nominal           [kW]
                       TempAmb,           & ! (I) Outdoor temperature             [°C]
                       CostHVACgas,       & ! (I) Cost                            [€/kW]
                       ThBalHVACgas,      & ! (IO) Thermal energy balance         [kWh]
                       CoolBalHVACgas,    & ! (IO) Cooling effect balance         [kWh]	                 
	                 ThProdHVACgas,     & ! (O) Thermal energy production       [kWh]
		           CoolProdHVACgas,   & ! (O) Cooling effect                  [kWh]
			     GasConsHVACgas,    & ! (O) Natural gas consumption         [Sm3]
			     ElConsHVACgas,     & ! (O) Electricity consumption         [kWh] 
			     Rec_Heat,          & ! (O) Recovery heat                   [kWh]  
			     CapexHVACgas)        ! (O) Capex                           [€]

          ! electrical
          sinkEEelectr  = sinkEEelectr + ElConsHVACgas
          deltaEEelectr = sourceEEelectr - sinkEEelectr

          ! thermal - only for ambient heat
          sourceEEheatAmb = sourceEEheatAmb + ThProdHVACgas + Rec_Heat !**
          deltaEEheatAmb  = sourceEEheatAmb - sinkEEheatAmb
          !** Rec_Heat added here for Villa Donatello, where no distinction is made between ambient heating and hot water
          !   If a distinction is made, even if same T level is used for both, Rec_Heat should be used for hot water when working as cooling

          ! cooling - only for ambient cooling
          sourceEEcoolAmb = sourceEEcoolAmb + CoolProdHVACgas
          deltaEEcoolAmb  = sourceEEcoolAmb - sinkEEcoolAmb

          ! gas
          GasConsump = GasConsump + GasConsHVACgas

          AddHourlyData(iloc,ii,1,itime) = ThProdHVACgas
          AddHourlyData(iloc,ii,2,itime) = CoolProdHVACgas
          AddHourlyData(iloc,ii,3,itime) = GasConsHVACgas
          AddHourlyData(iloc,ii,4,itime) = ElConsHVACgas
          AddHourlyData(iloc,ii,5,itime) = Rec_Heat

          DataEconAn(2,ii) = 1.d0
          DataEconAn(3,ii) = CapexHVACgas
          DataEconAn(4,ii) = 15.d0


       case(10) ! [ElectricChiller]

          CapacityP_frigo_nom = ValData(2,ii)
          CapacityP_frigo_min = ValData(3,ii)
          CapacityP_frigo_max = ValData(4,ii)
          COPnomElChiller     = ValData(5,ii)
          PowerFanElChiller   = ValData(6,ii)
          Cost_El_Chiller     = ValData(7,ii)

          QQcoolDemElChiller = deltaEEcoolAmb

          call El_Chiller(CapacityP_frigo_nom,   & ! (I)  Frigo Capacity nominal   [kW] 
			        CapacityP_frigo_min,   & ! (I)  Min Power                [kW] 
			        CapacityP_frigo_max,   & ! (I)  Max Power                [kW]
                          COPnomElChiller,       & ! (I)  COP nominal              [-]
	                    Cost_El_Chiller,       & ! (I)  Cost                     [€/kW]
		              PowerFanElChiller,     & ! (I)  Nominal Power fan        [kW]
		              TempAmb,               & ! (I)  Outdoor temperature      [°C]
                          RelHumdity,            & ! (I)  Relative humidity        [%]
			        QQcoolDemElChiller,    & ! (IO) Frigo Demand in          [kWh] 
	                    CoolProdElChiller,     & ! (O)  Frigo production         [kWh]        
			        ElConsElChiller,       & ! (O)  Electric consumption     [kWh]
			        ElConsFanElChiller,    & ! (O)  Electric consumption fan [kWh]
			        COPElChiller,          & ! (O)  COP real                 [-]
			        CapexElChiller)          ! (O) Capex                     [eur]

          ! electrical
          sinkEEelectr   = sinkEEelectr + ElConsElChiller + ElConsFanElChiller
          deltaEEelectr = sourceEEelectr - sinkEEelectr

          ! cooling
          sourceEEcoolAmb = sourceEEcoolAmb + CoolProdElChiller
          deltaEEcoolAmb  = sourceEEcoolAmb - sinkEEcoolAmb

          AddHourlyData(iloc,ii,1,itime) = CoolProdElChiller
          AddHourlyData(iloc,ii,2,itime) = ElConsElChiller
          AddHourlyData(iloc,ii,3,itime) = ElConsFanElChiller
          AddHourlyData(iloc,ii,4,itime) = COPElChiller

          DataEconAn(2,ii) = 1.d0
          DataEconAn(3,ii) = CapexElChiller
          DataEconAn(4,ii) = 15.d0


        case(11) ! [SimpBatt]

          CapacityNom   =     ValData(2,ii)
          SoCBatteryMax =     ValData(3,ii)
          DoD           =     ValData(4,ii)
          etaBatt       =     ValData(5,ii)
          SoCBattery    =     ValData(6,ii) ! >>> WIP: Maybe this should work as SoHBattery <<<
          if(itime.eq.1 .and. iyear.eq.1)then
            SoHBattery   =    ValData(7,ii)
            ccal2 = 0.d0
            ccal(iloc) = 0.d0
            ccyc(iloc) = 0.d0
            csum(iloc) = 0.d0
            tref_cal     = 0.d0
          else
          endif
          CostSimpBatt =      ValData(8,ii)

          deltaBatt = deltaEEelectr

          call SimpleBattery(CapacityNom,    & ! (I)  Capacity of the battery     [kW]
                             SoCBatteryMax,  & ! (I)  Battery maximum SoC         [-]
                             DoD,            & ! (I)  Depth Of Discharge          [-]
                             etaBatt,        & ! (I)  Efficiency of the battery   [-]
                             CostSimpBatt,   & ! (I)  Cost per kWh simple battery [eur/kWh]
                             deltaBatt,      & ! (IO) Delta production - demand   [kWh]
                             SoCBattery,     & ! (IO) State of Charge             [-]
                             deltaBattery,   & ! (O)  Energy accumulated (<0) or produced (>0) [kWh]
                             CapexSimpBatt)    ! (O)  Battery cost                [eur]

          ValData(6,ii) = SoCBattery

          !electricity
          deltaEEelectr = deltaBatt

          AddHourlyData(iloc,ii,1,itime) = SoCBattery
          AddHourlyData(iloc,ii,2,itime) = deltaBattery
 
          DataEconAn(3,ii) = CapexSimpBatt 
        
          selectcase(IndAgeing)

          case(0) ! Ageing not used

            DataEconAn(2,ii) = 2.d0
            DataEconAn(4,ii) = 10.d0
            DataEconAn(5,ii) = 20.d0

          case(1) ! Ageing used

            if(itime.eq.1 .and. iyear.eq.1) then
              ! first time instant, will be updated with first subst
              Nsost = 1
              DataEconAn(2,ii) = Nsost
              DataEconAn(4,ii) = 30.d0
            else
              ! updating DataEconAn every time SoH < 0.70
              if(SoHBattery(iloc) .le. 0.70d0)then
                Nsost = DataEconAn(2,ii)
                Nsost = Nsost + 1
                DataEconAn(2,ii) = Nsost
                DataEconAn(3+Nsost,ii) = dble(itime+((iyear-1)*NhourYear))/dble(NhourYear)
                tref_cal = dble(itime+((iyear-1)*NhourYear))/dble(NhourYear)
                write(*,*) 'sost',tref_cal
                ccal2 = 0.d0
                ccal(iloc)=0.d0
                ccyc(iloc)=0.d0
                SoHBattery(iloc) = 1.d0
              endif
            endif

          endselect

         TempArrBattDeg = DataEconAn(:,ii)


        case(12) ! [HydrogenSystem]

          ! electrolyzer
          CellNumber      = ValData(2,ii)
          CellActiveArea  = ValData(3,ii)
          CurrDensityMax  = ValData(4,ii)
          PowerNominal    = ValData(5,ii)
          OperatingTemp   = ValData(6,ii)
          AnodePressure   = ValData(7,ii)
          OperatingPress  = ValData(8,ii)
          CostElectr      = ValData(9,ii)

          ! fuel cell
          FC_CellNumber    = ValData(10,ii)
          FC_CellArea      = ValData(11,ii)
          FC_MaxCurrDens   = ValData(12,ii)
          FC_NominalPower  = ValData(13,ii)
          FC_OperatingTemp = ValData(14,ii)
          FC_FuelPress     = ValData(15,ii)
          FC_AirPress      = ValData(16,ii)
          FC_Cost          = ValData(17,ii)

          ! hydrogen tank
          TankVol                 = ValData(18,ii) 
          StorageMaxWorkPressure  = ValData(19,ii)
          TankCost                = ValData(20,ii)
          SoCHydrogen             = ValData(21,ii)  
       
          DeltaEnergy = deltaEEelectr

          call HydrogenSystem(DeltaEnergy,        & ! (IO)  Electrical energy balance   [kWh]
                              SoCHydrogen,        & ! (IO)  State of Charge             [-]
                              TempAmb,            & ! (I)  Ambient temperature          [K]

                              CellNumber,         & ! (I)  Stack cell number             [-]
                              CellActiveArea,     & ! (I)  Stack cell active area        [cm^2]   <------
                              CurrDensityMax,     & ! (I)  Cell max. current density     [A/cm^2]
                              PowerNominal,       & ! (I)  Max. power absorbable         [kW]
                              OperatingTemp,      & ! (I)  Stack Operating temperature   [K]
                              AnodePressure,      & ! (I)  Anode pressure                [Pa]
                              OperatingPress,     & ! (I)  Stack operating pressure (cathode pressure) [Pa]
                              CostElectr,         & ! (I)  Electrolyzer cost per kW      [eur/kW]

                              FC_CellNumber,      & ! (I) n° of fuel cells               [-]
                              FC_CellArea,        & ! (I) fuel cell active area          [cm^2]
                              FC_MaxCurrDens,     & ! (I) FC max. current density        [A/cm^2]
                              FC_NominalPower,    & ! (I) Max. power deliverable         [kW]
                              FC_OperatingTemp,   & ! (I) FC operating temperature       [K] 
                              FC_FuelPress,       & ! (I) Fuel supply pressure (Anode)   [K]   
                              FC_AirPress,        & ! (I) Air supply pressure (Cathode)  [K]
                              FC_Cost,            & ! (I) FC system cost                 [eur/kW]                     

                              TankVol,                & ! (I)  Tank volume                   [m^3]
                              StorageMaxWorkPressure, & ! (I)  Maximun tank working pressure [Pa]
                              TankCost,               & ! (I)  Tank cost                     [Eur/kg]

                              etaElectr,          & ! (O)  Electrolyzer efficiency       [-]
                              WatCons,            & ! (O)  Water volumetric flow in      [m^3] 

                              etaFC,              & ! (O) Stack efficiency               [-]
                              FC_Heat,            & ! (O) Generated heat                 [kWh]       

                              deltaHydrogenVolume,   & ! (O) Hydrogen produced (> 0) or consumed (< 0) [Sm3]
                              DeltaElectricalEnergy, & ! (O) El energy entering (< 0) or leaving (> 0) the system [kWh] 
                              
                              !ComprPower,         & ! (O)  Power input for compression  [kW]  
                              !NewEtaElectr,       & ! (O)  New electrolyzer efficiency  [-]
                                                                                         
                              SystemCapex)           ! (O)  overall System capex         [eur] 
                              

          ValData(21,ii)  = SoCHydrogen

          ! electrical
          deltaEEelectr = DeltaEnergy

          AddHourlyData(iloc,ii,1,itime) = SoCHydrogen
          AddHourlyData(iloc,ii,2,itime) = deltaHydrogenVolume 
          AddHourlyData(iloc,ii,3,itime) = DeltaElectricalEnergy 
          AddHourlyData(iloc,ii,4,itime) = FC_Heat
          if(DeltaElectricalEnergy.lt.0.d0)then
            AddHourlyData(iloc,ii,5,itime) = etaElectr
          elseif(DeltaElectricalEnergy.gt.0.d0)then
            AddHourlyData(iloc,ii,5,itime) = etaFC
          else
            AddHourlyData(iloc,ii,5,itime) = 0.d0
          endif

          DataEconAn(2,ii) = 1.d0
          DataEconAn(3,ii) = SystemCapex
          DataEconAn(4,ii) = 15.d0


       case(13) ! [CCHP]

          IndexCCHP            = ValData(2,ii)
          CapacityP_el_nom     = ValData(3,ii)
          CapacityP_el_min     = ValData(4,ii)
          CapacityP_el_max     = ValData(5,ii)
          etaElectrical        = ValData(6,ii)
          etaThermal           = ValData(7,ii)
	    CapacityP_frigo_nom  = ValData(8,ii)
	    CapacityP_frigo_min  = ValData(9,ii)
	    CapacityP_frigo_max  = ValData(10,ii)
	    eta_frigo_nom        = ValData(11,ii)
          Power_fan            = ValData(12,ii)
          Cost_Cogeneration    = ValData(13,ii)
          Cost_Abs_Chiller     = ValData(14,ii)

          ElBalance   = deltaEEelectr
          ThBalance   = deltaEEheatAmb !+ deltaEEheatWat ! <<< WIP: right now only ambient heating, what about hot water?
          CoolBalance = deltaEEcoolAmb

          call CCHP(IndexCCHP,           & ! (I) 0:CHP, 1: CCHP
                    CapacityP_el_nom,    & ! (I) Electric Capacity nominal           [kW]  ! Power
                    CapacityP_el_min,    & ! (I) Min Power                           [kW]  ! Power
		        CapacityP_el_max,    & ! (I) Max Power                           [kW]  ! Power
                    etaElectrical,       & ! (I) Electrical efficiency               [-]
                    etaThermal,          & ! (I) Thermal efficiency                  [-]
		        CapacityP_frigo_nom, & ! (I) Frigo Capacity nominal              [kW]  ! Power
		        CapacityP_frigo_min, & ! (I) Min Frigo Power                     [kW]  ! Power
		        CapacityP_frigo_max, & ! (I) Max Frigo Power                     [kW]  ! Power
		        eta_frigo_nom,       & ! (I) Abs chiller nominal efficiency      [-]
		        Power_fan,           & ! (I) Nominal Power fan                   [kW]
	              Cost_Cogeneration,   & ! (I) Cost Cogeneration                   [€/kW]
	              Cost_Abs_Chiller,    & ! (I) Cost Abs Chiller                    [€/kW]
		        TempAmb,             & ! (I) Outdoor temperature                 [°C]
		        ElBalance,           & ! (IO) Electric Demand in                [kWh] ! energy balance
		        ThBalance,           & ! (IO) Thermal Demand in                 [kWh] ! energy balance 
		        CoolBalance,         & ! (IO) Cooling Demand in                 [kWh] ! energy balance 
		        El_Production,       & ! (O) Electric production                 [kWh] 
                    Heat_Production,     & ! (O) Overall heat produced               [kWh]
		        Heat_Heating,        & ! (O) Thermal production                  [kWh]
		        HeatConsChiller,     & ! (O) Thermal consumption Abs Chiller     [kWh] 
                    HeatResidual,        & ! (O) Heat discarded                      [kWh]		
                    Frigo_Production,    & ! (O) Frigo production                    [kWh]
                    Gas_Cons_real,       & ! (O) Gas consumption                     [Sm3]
		        El_Cons_fan,         & ! (O) Electric consumption fan            [kWh]
		        Capex_Cogeneration,  & ! (O) Capex Cogeneration                  [€]
                    Capex_Abs_Chiller)     ! (O) Capex Abs Chiller                   [€]   

          
          ! electricity
          sourceEEelectr = sourceEEelectr + El_Production
          sinkEEelectr   = sinkEEelectr + El_Cons_fan
          deltaEEelectr = sourceEEelectr - sinkEEelectr
          ! thermal
          sourceEEheatAmb = sourceEEheatAmb + Heat_Heating
          deltaEEheatAmb     = sourceEEheatAmb     - sinkEEheatAmb
          ! cooling
          sourceEEcoolAmb = sourceEEcoolAmb + Frigo_Production
          deltaEEcoolAmb     = sourceEEcoolAmb     - sinkEEcoolAmb
          ! gas
          GasConsump = GasConsump + Gas_Cons_real

          AddHourlyData(iloc,ii,1,itime) = El_Production
          AddHourlyData(iloc,ii,2,itime) = Heat_Production 
          AddHourlyData(iloc,ii,3,itime) = Frigo_Production
          AddHourlyData(iloc,ii,4,itime) = Gas_Cons_real
          AddHourlyData(iloc,ii,5,itime) = El_Cons_fan
          AddHourlyData(iloc,ii,6,itime) = HeatConsChiller

          DataEconAn(2,ii) = 1.d0
          DataEconAn(3,ii) = Capex_Cogeneration + Capex_Abs_Chiller
          DataEconAn(4,ii) = 20.d0


        case(14) ! [Boiler]

          etaBoil     = ValData(2,ii)
          CostBoiler  = ValData(3,ii)

          QQdemBoil = deltaEEheatAmb + deltaEEheatWat 

          call Boiler(QQdemBoil,      & ! (I) Heat demand       [kWh]
                      etaBoil,        & ! (I) Boiler efficiency [-]
                      CostBoiler,     & ! (I) Boiler cost       [eur/kW]
                      QQfuelHour,     & ! (O) Fuel consumption  [kWh]
                      GasConsumpBoil, & ! (O) Fuel consumption  [Sm3]
                      CapexBoiler)      ! (O) Boiler capex      [eur]

          GasConsump = GasConsump + GasConsumpBoil

          DataEconAn(2,ii) = 1.d0
          DataEconAn(3,ii) = CapexBoiler
          DataEconAn(4,ii) = 20.d0

          AddHourlyData(iloc,ii,1,itime) = QQfuelHour
          AddHourlyData(iloc,ii,2,itime) = GasConsumpBoil


        case default
          ! Code not found
          write(*,*) 'Code not found: internal error!'
          stop 'Index error!'
        end select

1000  continue ! end do cycle with ii on Nelem(iloc)




      ! deltaEEelectr is the electrical energy balance, Excess > 0, Deficiency < 0
      ! EnSoldOut and EnBoughtOut saved to keep track of how much energy is sold or bought throughout the year
      if(deltaEEelectr.ge.0.d0) then
        EnSoldOut = deltaEEelectr
        enExpend = deltaEEelectr*elenPriceSell(itime)
      else
        EnBoughtOut = -deltaEEelectr
        enExpend = deltaEEelectr*elenPriceBuy(itime)
      endif

      ! GasConsump is always >= 0, hence the - in the following equation:
      enExpendGas = - GasConsump*GasPriceHouseS(itime)


      return

7000  continue
      write(*,7100) locName//'.dat'
      write(8,7100) locName//'.dat'
7100  format(/,'ERROR: Open error on "',a,'" file')
      stop 'Error!'


7010  continue
      write(*,7110) locName//'.dat'
      write(8,7110) locName//'.dat'
7110  format(/,'ERROR: Read error on "',a,'" file')
      stop 'Error!'


end subroutine location