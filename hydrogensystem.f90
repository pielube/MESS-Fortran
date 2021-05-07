
! Hydrogen subroutine: electricity to electricity
! Electrolyzer + Storage + Fuel cell
! Grazzini, Lubello Feb 2021


subroutine HydrogenSystem(DeltaEnergy,           & ! (IO)  Electrical energy balance    [kWh]
                          SoC,                   & ! (IO)  State of Charge              [-]
                          AmbTemp,               & ! (I)  Ambient temperature           [K]
                                                 
                          CellNumber,            & ! (I)  Stack cell number             [-]
                          CellActiveArea,        & ! (I)  Stack cell active area        [cm^2]   <-----
                          CurrDensityMax,        & ! (I)  Cell max. current density     [A/cm^2]
                          PowerNominal,          & ! (I)  Max. power absorbable         [kW]
                          OperatingTemp,         & ! (I)  Stack Operating temperature   [K]
                          AnodePressure,         & ! (I)  Anode pressure                [Pa]
                          OperatingPress,        & ! (I)  Stack operating pressure (cathode pressure) [Pa]
                          CostElectr,            & ! (I)  Electrolyzer cost per kW      [eur/kW]
                          !------------------    
                          FC_CellNumber,         & ! (I) n° of fuel cells               [-]
                          FC_CellArea,           & ! (I) fuel cell active area          [cm^2]
                          FC_MaxCurrDens,        & ! (I) FC max. current density        [A/cm^2]
                          FC_NominalPower,       & ! (I) Max. power deliverable         [kW]
                          FC_OperatingTemp,      & ! (I) FC operating temperature       [K] 
                          FC_FuelPress,          & ! (I) Fuel supply pressure (Anode)   [K]   
                          FC_AirPress,           & ! (I) Air supply pressure (Cathode)  [K]
                          FC_Cost,               & ! (I) FC system cost                 [eur/kW]                     
                          !------------------    
                          TankVol,               & ! (I) Tank volume                   [m^3]
                          TankMaxPressure,       & ! (I) Maximun tank working pressure [Pa]
                          TankCost,              & ! (I) Tank cost                     [Eur/kg]
                          !------------------    
                          etaElectr,             & ! (O) Electrolyzer efficiency       [-]
                          WatCons,               & ! (O) Water volumetric flow in      [m^3] 
                          !------------------    
                          etaFC,                 & ! (O) Stack efficiency               [-]
                          FC_Heat,               & ! (O) Generated heat                 [kWh]       
                          !------------------    
                          deltaHydrogenVolume,   & ! (O) Hydrogen produced (> 0) or consumed (< 0) [Sm3]
                          deltaElectricalEnergy, & ! (O) El energy entering (< 0) or leaving (> 0) the system [kWh] 

                          !ComprPower,           & ! (O) Power imput for compression   [kW]  
                          !NewEtaElectr,         & ! (O) New electrolyzer efficiency   [-]

                          SystemCapex)             ! (O) Overall system capex          [eur] 
 
      USE MODcoord, ONLY:itime
      USE MODParam, ONLY: timestep,FaradayConst,Runiv,LHVh2,Tstandard,H2MolMass,GammaPerfectGas,Rh2,rhoStdh2                                      
                        
      implicit real(8) (a-h,o-z), integer(i-n)

      real(8), intent(IN   ) :: CellNumber,CellActiveArea,CurrDensityMax,PowerNominal,AmbTemp,OperatingTemp,OperatingPress,CostElectr,AnodePressure ! input electrolyzer
      real(8), intent(IN   ) :: FC_CellNumber,FC_MaxCurrDens,FC_CellArea,FC_NominalPower,FC_Cost,FC_OperatingTemp,FC_FuelPress,FC_AirPress ! input fuel cell                        
      real(8), intent(IN   ) :: TankVol,TankMaxPressure,TankCost ! input storage
                                
      real(8), intent (INOUT):: DeltaEnergy,Soc                                                          
      
      real(8), intent(   OUT):: etaElectr,WatCons ! output electrolyzer
      real(8), intent(   OUT):: etaFC,FC_Heat ! output fuel cell
      real(8), intent(   OUT):: deltaHydrogenVolume,DeltaElectricalEnergy,SystemCapex ! ,ComprPower,NewEtaElectr
      
      ! Local variables

      real(8) :: ChargeMax,            & ![kWh]
                 ChargeMin,            & ![kWh]
                 ChargeMaxUsable,      & ![kWh]
                 StartingPress,        & ![Pa]
                 StartingCharge,       & ![kWh]
                 TankMinPressure,      & ![Pa]
                 TankMaxMass,          & ![kg]
                 AdiabaticComprWork,   & ![J]
                 ComprWorkMol,         & ![]
                 ComprCapex,           & ![Eur]
                 ElectrCapex,          & ![Eur]
                 ElectrOpex,           & ![Eur/anno]
                 TankCapex,            & ![Eur]
                 TankOpex,             & ![Eur/anno]
                 FCStackCapex,         & ![Eur]
                 FCStackOpex,          & ![Eur/anno]
                 Vstack,               & ![V]
                 HydroMol,             & ![mol] Produced moles of hydrogen    
                 etaFaraday,           & ![-]   Faraday efficiency            
                 DeltaElEnergy,        & ![kWh] Not absorbed electrical energy
                 DeltaE,               & ![kWh] electrical energy that can't be supplyed by fc
                 deltaHydrogen,        & ![kWh] H2 prodotto dall'elettrolizzatore (energia)                     
                 Charge,               & ![kWh] Charge of the storage system         
                 TankPress,            & ![Pa]  Tank Pressure                         
                 SystemOpex              ![eur/year] overall system opex                  
               
      real(8), parameter:: CurrDensityMin = 0.05d0,       &  ! [A/cm^2] Cell min. current density  
                           FC_MinCurrDens = 0.0d0            ! [A/cm^2]
                           !ComprIsoEff = 0.8d0   ! [-] decide if it should be an input or it can be fixed


      ! Initialization
      etaElectr            = 0.d0          
      WatCons              = 0.d0            
      etaFC                = 0.d0             
      FC_Heat              = 0.d0            
      deltaHydrogenVolume  = 0.d0
      deltaElectricalEnerg = 0.d0

      ! Storage parameters calculation (they could be computed just once)
      rhoh2 = TankMaxPressure/(Rh2*Tstandard)       ! Hydrogen density at TankMaxPressure
      TankMaxMass = TankVol*rhoh2                   ! Max H2 mass storable
      ChargeMax = TankMaxMass*LHVh2*1000.d0/3600.d0 ![kWh]
      ChargeMin = ChargeMax*0.03d0                  ![kWh] SOC=0
      TankMinPressure = 0.04d0*TankMaxPressure         ![Pa] Hexagon
      ChargeMaxUsable = ChargeMax-ChargeMin         ![kWh] SOC=1

      
      ! Defining if the system is charging or discharging during the time step
      ! DeltaEnergy = Electrical energy produced - Electrical energy demand
      
      if (DeltaEnergy .gt. 0.d0) then
        !charging
        call Electrolyzer(DeltaEnergy,        & ! (I)  electrical energy balance      [kWh]        
                          CellNumber,         & ! (I)  Stack cell number              [-]
                          CellActiveArea,     & ! (I)  Stack cell active area         [cm^2]
                          CurrDensityMin,     & ! (I)  Cell min. current density      [A/cm^2]
                          CurrDensityMax,     & ! (I)  Cell max. current density      [A/cm^2]
                          PowerNominal,       & ! (I)  Max. power absorbable          [kW]
                          AmbTemp,            & ! (I)  Ambient temperature            [K]
                          OperatingTemp,      & ! (I)  Stack Operating temperature    [K]
                          OperatingPress,     & ! (I)  Stack operating pressure       [Pa]
                          CostElectr,         & ! (I)  Electrolyzer cost per kW       [eur/kW]
                          AnodePressure,      & ! (I)  Anode pressure                 [Pa]
                          deltaHydrogen,      & ! (O)  DeltaProd                      [kWh] ( > 0 cause I am producing hydrogen)
                          etaElectr,          & ! (O)  Electrolyzer efficiency        [-]
                          WatCons,            & ! (O)  Water volumetric flow in       [m^3]
                          DeltaElEnergy,      & ! (O)  Not absorbed electrical energy [kWh]
                          ElectrCapex,        & ! (O)  Capex [eur]
                          ElectrOpex,         & ! (O)  Opex [eur/y]
                          Vstack,             & ! (O)  Stack voltage                  [V]   ! necessary for compressor
                          HydroMol,           & ! (O)  Produced moles of hydrogen     [mol] ! necessary for compressor
                          etaFaraday)           ! (O)  Faraday efficiency             [-]   ! necessary for compressor
        
      else if (DeltaEnergy .lt. 0.d0) then
        !discharging
        call FuelCell(DeltaEnergy,       & ! (I)  electrical energy balance     [kWh]
                      FC_CellNumber,     & ! (I) n° of fuel cells           [-]
                      FC_MinCurrDens,    & ! (I) FC min. current density    [A/cm^2]
                      FC_MaxCurrDens,    & ! (I) FC max. current density    [A/cm^2]
                      FC_CellArea,       & ! (I) fuel cell active area      [cm^2]
                      FC_NominalPower,   & ! (I) Max. power deliverable     [kW]
                      FC_Cost,           & ! (I) FC system cost             [Eur/kW]                  
                      FC_OperatingTemp,  & ! (I) FC operating temperature   [K] 
                      FC_FuelPress,      & ! (I) Fuel supply pressure (Anode) [K]   
                      FC_AirPress,       & ! (I) Air supply pressure (Cathode)[K] 
                      DeltaE,            & ! (O) electrical energy that can't be supplyed by fc [kWh] 
                      deltaHydrogen,  & ! (O) energy hydrogen demand     [kWh] ( <0 cause I am consuming hydrogen)
                      etaFC,             & ! (O) Stack efficiency           [-]
                      FC_Heat,      & ! (O) Generated heat             [kWh]
                      FCStackCapex,      & ! (O) FC capex [eur]
                      FCStackOpex)         ! (O) FC opex [eur/y]
                     
      else if (DeltaEnergy .eq. 0.d0) then
        
        etaElectr = 0.d0      
        HydroProd = 0.d0      
        WatCons   = 0.d0        
        etaFC     = 0.d0         
        FC_HydroCons = 0.d0
        FC_Heat     = 0.d0   
        DeltaElectricalEnergy = 0.d0 
        deltaHydrogen = 0.d0  
                        
      end if 
      
      !-----------------------
      ! Storage calculation
      !-----------------------
        
      Startingcharge = ChargeMin + Soc*(ChargeMaxUsable)                 ![kWh]
      Startingpress = TankMinPressure +Soc*(TankMaxPressure-TankMinPressure)   ![Pa]
 
     
      if (DeltaHydrogen .gt. 0.d0) then
        ! Charging: storing hydrogen into the tank
        Charge      = StartingCharge+DeltaHydrogen             ![kWh]           
        ! Checking if I am trying to store more hydrogen then what I can store
          if(Charge .gt. ChargeMax)then
            DeltaHydrogen = DeltaHydrogen-(Charge-ChargeMax) ![kWh] hydrogen actually stored 
            DeltaElectricalEnergy = - DeltaHydrogen/EtaElectr  ![kWh] electricity actually consumed < 0 cause I am storing energy (sink for the building)
            DeltaEnergy = (Charge-ChargeMax)/EtaElectr + DeltaElEnergy ! [kWh] energy balance
            Charge      = ChargeMax       
          else
            !DeltaHydrogen = DeltaHydrogen  useless, left as a comment for clarity        
            DeltaElectricalEnergy = - DeltaHydrogen/EtaElectr  ![kWh] electricity actually consumed
            DeltaEnergy =  DeltaElEnergy ! [kWh] updated energy balance
            !Charge      = Charge useless, left as a comment for clarity 
          end if


          Soc = (Charge - ChargeMin) / ChargeMaxUsable
          TankPress   = TankMinPressure +Soc*(TankMaxPressure-TankMinPressure)
     
          !! Compressor <<< WIP 
          !If (TankPress .gt. OperatingPress) then
          !  AdiabaticComprWork = (GammaPerfectGas/(GammaPerfectGas-1))*OperatingPress*TankVol*((TankPress/OperatingPress)**(GammaPerfectGas/(GammaPerfectGas-1))-1)  ![J]
          !  ComprWork    = AdiabaticComprWork/ &
          !                 ComprIsoEff                    ![J]             
          !  ComprWorkMol = ComprWork/HydroMol             ![J/mol]
          !  ComprPower   = ComprWork/ &
          !                 (timestep*1000.d0*3600.d0)     ![kW]           
          !  !ComprCapex   = (5840.d0*(ComprPower)**0.82d0)*0.85d0  ![Eur @ 2020] Luyben 2018
          !             
          !  ! New Electrolyzer efficiency                
          !  NewEtaElectr = CellNumber*(LHVh2*1.d6*H2MolMass-ComprWorkMol)*etaFaraday/(2.d0*Vstack*FaradayConst) ! [-]
          !
          !else if (TankPress .le. OperatingPress) then
          !
          !  ComprWork    = 0.d0
          !  ComprWorkMol = 0.d0
          !  ComprPower   = 0.d0
          !  !ComprCapex   = 0.d0
          !  TankCapex    = TankCost*TankMaxMass 
          !  TankOpex     = TankCapex*0.10d0 
          !  NewEtaElectr = etaElectr
          !
          !end if  
      
      else if (DeltaHydrogen .lt. 0.d0) then
        ! Discharging: taking hydrogen from the tank
        Charge      = StartingCharge+deltaHydrogen            ![kWh]
        ! Checking if I am trying to take more hydrogen then how much is left
          if(Charge .lt. ChargeMin)then 
            deltaHydrogen = deltaHydrogen+(ChargeMin-Charge) ![kWh] hydrogen actually used by the fuel cell 
            DeltaElectricalEnergy = -deltaHydrogen*etaFC     ![kWh] electricity actually produced > 0 cause I am releasing energy (source for the building)
            DeltaEnergy = (Charge-ChargeMin)*etaFC - DeltaE  ![kWh] updated energy balance
            Charge      = ChargeMin             
          else 
            !FC_deltaHydrogen = FC_deltaHydrogen useless, left as a comment for clarity
            DeltaElectricalEnergy = - deltaHydrogen*etaFC
            DeltaEnergy = -DeltaE
            !Charge = Charge useless, left as a comment for clarity           
          end if
          
        Soc = (Charge - ChargeMin) / ChargeMaxUsable

      else if (DeltaHydrogen .eq. 0.d0) then 
        
        etaElectr = 0.d0      
        HydroProd = 0.d0      
        WatCons   = 0.d0        
        etaFC     = 0.d0         
        FC_HydroCons = 0.d0
        FC_Heat     = 0.d0   
        DeltaElectricalEnergy = 0.d0  
        deltaHydrogen = 0.d0

      end if

      !!Compressor <<< WIP
      !ComprPower = 0.d0     
      !NewEtaElectr = 0.d0              

      ! For completeness, converting deltaElectricalEnergy to the correspondent amount of hydrogen [Sm3]
      ! produced by the electrolyzer to be stored in the tank (> 0) 
      ! or
      ! consumed by the fuel cell taking it from the tank (< 0)
      ! NB the sign is the opposite of deltaElectricalEnergy

      deltaHydrogenVolume = deltaHydrogen/(LHVh2*rhoStdh2)*3600.d0/1000.d0 ![Sm3]
      

      ! Computing system costs (they could be computed just once)
      ! NB redundancy with costs computed in electrolyzers and fuel cell subroutine, code can be improved here

      ! tank cost
      TankCapex = TankCost*TankMaxMass!+ComprCapex   ![eur]
      TankOpex = TankCapex*0.10d0                ![eur/anno]
      ! electrolyzer cost
      ElectrCapex = Costelectr*PowerNominal      ![eur]
      ElectrOpex = ElectrCapex*0.02d0            ![eur/anno]  
      ! fuel cell cost
      FCStackCapex = FC_Cost*FC_NominalPower     ![eur]
      FCStackOpex = FCStackCapex*0.02d0          ![eur/anno]
      ! overall cost
      SystemCapex = ElectrCapex + TankCapex + FCStackCapex  ![eur]
      SystemOpex = ElectrOpex + TankOpex + FCStackOpex      ![eur/anno] 
      
      return      
            
end Subroutine HydrogenSystem