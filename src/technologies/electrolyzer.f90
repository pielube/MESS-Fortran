
! Electrolyzer subroutine
! Grazzini, Lubello: feb 2021

subroutine Electrolyzer(DeltaEnergy,        & ! (I)  electrical energy balance      [kWh]        
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
                        deltaHydrogen,      & ! (O)  DeltaProd                      [kWh]
                        etaElectr,          & ! (O)  Electrolyzer efficiency        [-]
                        WatCons,            & ! (O)  Water volumetric flow in       [m^3]
                        DeltaElEnergy,      & ! (O)  Not absorbed electrical energy [kWh]
                        ElectrCapex,        & ! (O)  Capex                          [eur]
                        ElectrOpex,         & ! (O)  Opex                           [eur/y]
                        Vstack,             & ! (O)  Stack voltage                  [V]   ! necessary for compressor
                        HydroMol,           & ! (O)  Produced moles of hydrogen     [mol] ! necessary for compressor
                        etaFaraday)           ! (O)  Faraday efficiency             [-]   ! necessary for compressor     

      USE MODcoord, ONLY: itime,iloc                       
      USE MODParam, ONLY: timestep,FaradayConst,Runiv,eNepero,LHVh2,Pstandard,Tstandard,h2oMolMass,rhoStdh2o,rhoStdh2,h2MolMass 
      USE MODelectrolyzer, ONLY: iswitchelectrolyzer,coeffAel,coeffBel                                   
                                
      implicit real(8) (a-h,o-z), integer(i-n)
       
      real(8), intent(IN   )  :: DeltaEnergy,CellNumber,CellActiveArea,CurrDensityMin,CurrDensityMax,PowerNominal,AmbTemp,OperatingTemp,OperatingPress,CostElectr,AnodePressure       
      real(8), intent(  OUT) :: Vstack,HydroMol,deltaHydrogen,etaFaraday,etaElectr,WatCons,DeltaElEnergy, ElectrCapex,ElectrOpex

      ! Local declarations
                 
      real(8), parameter :: MembThickness       = 250.d0,    & ! [micron]
                            Lambda              = 20.d0,     & ! [-] Cell mositure content
                            AnodeCurrDensity    = 0.00013d0, & ! [A/cm^2]
                            CathodeCurrDensity  = 0.001d0,   & ! [A/cm^2]
                            CTCanode            = 0.6d0,     & ! [-]
                            CTCcathode          = 0.45d0       ! [-]
                                
      real(8) ::  PowerInput,      &  ! [kW]
                  coeff_A,         &  ! [-]
                  coeff_B,         &  ! [-]
                  Current,         &  ! [A]
                  HydroProdMol,    &  ! [mol/h] 
                  HydroProd           ! [Sm3] 
      
             
      ! Linear regression to approx V-I curve of the electrolyzer
      ! NB could be called only once if storing coefficients

      if(iswitchelectrolyzer(iloc).eq.0)then
                  
        call Coefficients(CellNumber,         & ! (I) n° of electr. cells           [-]
                          CurrDensityMin,     & ! (I) Minimum current density       [A/cm^2]
                          CurrDensityMax,     & ! (I) Maximum current density       [A/cm^2]
                          OperatingTemp,      & ! (I) Electr. operating temperature [K]
                          OperatingPress,     & ! (I) Electr. operating pressure    [Pa]
                          AnodePressure,      & ! (I) Anode operating pressure      [Pa]
                          MembThickness,      & ! (I) Membrane thickness of the cell [micron]
                          Lambda,             & ! (I) Cell mositure content [-]
                          AnodeCurrDensity,   & ! (I) [A/cm^2]
                          CathodeCurrDensity, & ! (I) [A/cm^2]
                          CTCanode,           & ! (I) Anode Charge Transfer coefficient   [-] 
                          CTCcathode,         & ! (I) Cathode Charge Transfer coefficient [-] 
                          coeffAel(iloc),            & ! (O) grade 0 coeff  [-]
                          coeffBel(iloc))              ! (O) grade 1 coeff  [-]

        iswitchelectrolyzer(iloc) = 1

      else
      endif


      ! Actual electrical power absorbed by the electrolyzer      

      PowerInput = DeltaEnergy/timestep ![kW] electrical power absorbed by electrolyzer
            
      if (PowerInput .le. PowerNominal) then
        DeltaElEnergy=0.d0
        !PowerInput=PowerInput useless, left as a comment for clarity       
      else if (PowerInput .gt. PowerNominal) then
        DeltaElEnergy=(PowerInput-PowerNominal)*timestep ![kWh] not absorbed electrical energy
        PowerInput=PowerNominal  
      end if

                        
      ! Finding the working point of the electrolyzer by explicitly solving the system:
      ! PowerInput=CellCurrDensity*CellActiveArea*Vstack
      ! Vstack=coeffBel(iloc)*CellCurrDensity+coeffAel(iloc)

      CellCurrDensity = (-coeffAel(iloc)+SQRT(coeffAel(iloc)**2.d0+4.d0*coeffBel(iloc)*(PowerInput*1000.d0/CellActiveArea)))/(2.d0*coeffBel(iloc))  ! [A/cm^2]

      ! Checking if resulting current density is high enough for the electrolyzer to start, otherwise hydrogen prod = 0
      if(CellCurrDensity .lt. CurrDensityMin)then
        deltaHydrogen = 0.d0
        etaElectr = 0.d0    
        WatCons = 0.d0     
        DeltaElEnergy = 0.d0 
        Vstack = 0.d0      
        HydroMol = 0.d0     
        etaFaraday = 0.d0
        goto 9999
      else
      endif   

      Vstack=coeffBel(iloc)*CellCurrDensity+coeffAel(iloc)
      Current = CellCurrDensity*CellActiveArea  ![A]      
      
      ! Computing electrolyzer's efficiency and  hydrogen energy output    
      
      etaFaraday = 9.95d-1*eNepero**((-9.5788d0-0.0555d0*AmbTemp)/(Current/(CellActiveArea*CellNumber/10000.d0))+ &
                   (1502.7083d0-70.8005d0*AmbTemp)/((Current/(CellActiveArea*CellNumber/10000.d0))**2.d0)) ! [-] Faraday efficiency

      etaElectr = CellNumber*LHVh2*1.d6*h2MolMass*etaFaraday/(2.d0*Vstack*FaradayConst) ! [-] Electrolyzer efficiency

      ! Hydrogen Production
      
      HydroProdMol = (etaFaraday*CellNumber*Current*3600.d0)/(2.d0*FaradayConst) ![mol/h]   (Guilbert 2020)  
      HydroMol = HydroProdMol*timestep                                           ![mol] ! da rivedere, serve per il compressore ma si può togliere
      HydroProd = HydroMol*H2MolMass/rhoStdh2                                    ![Sm^3]           
      deltaHydrogen= HydroProd*LHVh2*rhoStdh2*(1000.d0/3600.d0)                  ![kWh]

   
      !Water consumption 
      
      WatCons = HydroProd*rhoStdh2*H2Omolmass/H2MolMass/etaElectr/rhoStdh2o ! [m3]

9999  continue

      !Capex and Opex
      ElectrCapex = Costelectr*PowerNominal          ![Eur]
      ElectrOpex = ElectrCapex*0.02d0                ![Eur/anno]                         

  
end subroutine Electrolyzer  


! Subroutine to compute the V-I curve of the electrolyzer and its linear regression coefficients

subroutine Coefficients(CellNumber,         & ! (I)  n° of electr. cells           [-]
                        CurrDensityMin,     & ! (I)  Minimum current density       [A/cm^2]
                        CurrDensityMax,     & ! (I)  Maximum current density       [A/cm^2]
                        OperatingTemp,      & ! (I)  Electr. operating temperature [K]
                        OperatingPress,     & ! (I)  Electr. operating pressure    [Pa]
                        AnodePressure,      & ! (I)  Anode operating pressure      [Pa]
                        MembThickness,      & ! (I) [micron] Membrane thickness of the cell
                        Lambda,             & ! (I) [-] Cell mositure content
                        AnodeCurrDensity,   & ! (I)  [A/cm^2]
                        CathodeCurrDensity, & ! (I)  [A/cm^2]
                        CTCanode,           & ! (I) [-] Anode Charge Transfer coefficient
                        CTCcathode,         & ! (I) ! [-] Cathode Charge Transfer coefficient 
                        coeff_A,            & ! (O)  grade 0 coeff  [-]
                        coeff_B)              ! (O)  grade 1 coeff  [-] 
 
      USE MODParam,       ONLY: FaradayConst,Runiv,eNepero                                       
                                                            
      implicit real(8) (a-h,o-z), integer(i-n)
       
      real(8), intent(IN   ) :: CellNumber,CurrDensityMin,CurrDensityMax,OperatingTemp,OperatingPress,AnodePressure,MembThickness,Lambda,AnodeCurrDensity,CathodeCurrDensity,CTCanode,CTCcathode     
      real(8), intent(  OUT) :: coeff_A,coeff_B                                                                                 
                                                                                                                      
                                                                                                                    
      real(8)::   CellCurrDensity, &  ! [A/cm^2]                                                                              
                  pO2,             &  ! [Pa]                                                                                
                  pH2,             &  ! [Pa]
                  Ecell,           &  ! [V]
                  Vact_an,         &  ! [V]
                  Vact_cat,        &  ! [V]
                  Vact,            &  ! [V]
                  MaxCurrDensity_2,&  ! [A/cm^2]
                  Vdiff,           &  ! [V]
                  MembConductivity,&  ! [S/cm]
                  Rcell,           &  ! [cm^2/S]
                  Vohmic              ! [V]
                  
      integer, parameter :: Ndatapoints = 40  ! NB if changing Ndatapoints must be changed also in LinRegLeastSquares     
      real(8), dimension(Ndatapoints) :: vector_i, vector_V 
                                 
      vector_i = 0.d0    ![A/cm^2]
      vector_V = 0.d0    ![V]
      
      
      do i = 1,Ndatapoints 
      
        CellCurrDensity = (CurrDensityMax-CurrDensityMin)/Ndatapoints*i
        vector_i(i) = (CurrDensityMax-CurrDensityMin)/Ndatapoints*i      ![A/cm^2]
        
        
        ! Polarization (V-i) curve calculation
        ! V is obtained by summing 4 terms
      
        ! 1- Cell open curcuit voltage
        
        pH2O = eNepero**(11.676d0-(3816.44d0/(OperatingTemp-46.13d0))) ! [atm]
        pO2  = AnodePressure/101325.d0-pH2O                         ! [atm]
        pH2  = OperatingPress/101325.d0-pH2O                        ! [atm]

        Ecell = 1.229d0-0.85d-3*(OperatingTemp-298.15d0)+4.3085d-5*OperatingTemp*LOG(pH2*(pO2**0.5)/pH2O) ![V] Cell open curcuit voltage   
          
        ! 2- Cell activation overpotential
          
        Vact_an  = (Runiv*OperatingTemp*LOG(vector_i(i)/AnodeCurrDensity))/(2.d0*CTCanode*FaradayConst)     ! [V]
        Vact_cat = Runiv*OperatingTemp*LOG(vector_i(i)/CathodeCurrDensity)/(4.d0*CTCcathode*FaradayConst) ! [V]
          
        Vact = Vact_an + Vact_cat  ! [V] Cell activation overpotential
          
        ! 3- Cell mass transport overpotential
        
        MaxCurrDensity_2 = CurrDensityMax + 0.0001d0
          
        Vdiff = -Runiv*OperatingTemp*LOG(1.d0-vector_i(i)/MaxCurrDensity_2)/(2.d0*FaradayConst) ! [V] Cell mass transport overpotential
          
        ! 4- Cell Ohmic losses
          
        MembConductivity = (0.005139d0*Lambda-0.00326d0)*eNepero**(1268.d0*(1.d0/303.d0 - 1.d0/Operatingtemp)) ! [S/cm]
        Rcell = (MembThickness/10000.d0)/Membconductivity  ! [cm^2/S]
          
        Vohmic = vector_i(i)*Rcell          ! [V] Cell Ohmic losses
          
        ! Resulting stack voltage
          
        vector_V(i) = CellNumber*(Ecell+Vact+Vdiff+Vohmic) ! [V]

      end do

      ! Linear regression coefficients calculation

      call LinRegLeastSquares(vector_i, & ! (I) operating i    [A/cm^2]
                              vector_V, & ! (I) operating V    [V]
                              coeff_A,  & ! (O) grade 0 coeff  [-]
                              coeff_B)    ! (O) grade 1 coeff  [-] 
       
      
end subroutine Coefficients   
