

Subroutine FuelCell(DeltaEnergy,       & ! (I)  electrical energy balance     [kWh]
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
                    FC_deltaHydrogen,  & ! (O) energy hydrogen demand     [kWh]
                    etaFC,             & ! (O) Stack efficiency           [-]
                    FC_Heat,           & ! (O) Generated heat             [kWh]
                    FCStackCapex,      & ! (O) FC capex [eur]
                    FCStackOpex)         ! (O) FC opex [eur/y]

      USE MODcoord,    ONLY: iloc                       
      USE MODParam,    ONLY: timestep,FaradayConst,Runiv,eNepero,LHVh2,H2MolMass,deltaG0,Pstandard,Tstandard,HHVh2,HHVh2Mol,rhoStdh2
      USE MODfuelcell, ONLY: iswitchfuelcell,coeffAfc,coeffBfc                                                       
                  
      implicit real(8) (a-h,o-z), integer(i-n)
      
      real(8), intent(IN   ) :: DeltaEnergy,FC_CellNumber,FC_MinCurrDens,FC_MaxCurrDens,FC_CellArea,FC_NominalPower,FC_Cost,FC_OperatingTemp,FC_FuelPress,FC_AirPress
      real(8), intent(   OUT) :: DeltaE,FC_deltaHydrogen,etaFC,FC_Heat,FCStackCapex,FCStackOpex

      ! Local declarations
                            
      real(8), parameter ::  Lambda      = 23.d0,           & ![-] Cell mositure content
                              FC_AnodeCurrDens=0.000000009d0 , & ! (I)[A/cm^2]
                              FC_CathodeCurrDens= 0.001d0       ! (I)[A/cm^2]          
      
      real(8) ::  ReqPower,     &  ! [kW]
                  CellCurr,     &  ! [A]
                  eta_voltage,  &  ! [-]
                  pO2,          &  ! [-]
                  pH2O,         &  ! [-]
                  pH2,          &  ! [-]
                  E0,           &  ! [-]
                  deltaG,       &  ! [kJ/mol]
                  eta_th,       &  ! [-]
                  FC_HydroCons
                  
                  
      ! richiamo la subroutine che mi calcola i coefficienti della retta

      if(iswitchfuelcell(iloc).eq.0)then
   
        call Coefficients_FC(FC_CellNumber,      & ! (I) n° of fuel cells              [-]
                             FC_MinCurrDens,     & ! (I) FC min. current density       [A/cm^2]
                             FC_MaxCurrDens,     & ! (I) FC max. current density       [A/cm^2]
                             FC_CellArea,        & ! (I) fuel cell active area         [cm^2] 
                             FC_NominalPower,    & ! (I) Max. power deliverable        [kW]                
                             FC_OperatingTemp,   & ! (I) FC operating temperature      [K] 
                             FC_FuelPress,       & ! (I) Fuel supply pressure (Anode)  [Pa]   
                             FC_AirPress,        & ! (I) Air supply pressure (Cathode) [Pa] 
                             FC_AnodeCurrDens,   & ! (I) Cell mositure content         [-]
                             FC_CathodeCurrDens, & ! (I)[A/cm^2]
                             Lambda,             & ! (I)[A/cm^2]      
                             coeffAfc(iloc),            & ! (O) grade 0 coeff [-]
                             coeffBfc(iloc))              ! (O) grade 1 coeff [-]

        iswitchfuelcell(iloc) = 1

      else
      endif
                         
      ! Actual electrical power produced by the FC      
                       
      ReqPower = -DeltaEnergy/timestep ![kW] electrical power produced by FC
        
      if (ReqPower .le. FC_NominalPower) then
        DeltaE=0
        !ReqPower=ReqPower useless, left as a comment for clarity   
      else if (ReqPower .gt. FC_NominalPower) then
        DeltaE=(ReqPower-FC_NominalPower)*timestep ![kWh] electrical energy that can't be supplyed by fc
        ReqPower=FC_NominalPower     
      end if
       

      ! Finding the working point of the electrolyzer by explicitly solving the system:
      ! ReqPower=FC_CellCurrDensity*FC_CellArea*FC_Vstack
      ! FC_Vstack=coeffBfc(iloc)*FC_CellCurrDensity+coeffAfc(iloc)
        
      FC_CellCurrDensity = (-coeffAfc(iloc)+SQRT(coeffAfc(iloc)**2.d0+4.d0*coeffBfc(iloc)*(ReqPower*1000.d0/FC_CellArea)))/(2.d0*coeffBfc(iloc))  ! [A/cm^2]
      FC_Vstack=coeffBfc(iloc)*FC_CellCurrDensity+coeffAfc(iloc)

      CellCurr = FC_CellCurrDensity*FC_CellArea  ![A]        
        

      ! Computing FC's efficiency and  hydrogen energy demand    
        
      pO2 = (FC_AirPress*0.21d0)/101325.d0  ![atm]
      pH2O = 1.d0                           ![atm]
      pH2 = FC_FuelPress/101325.d0          ![atm]    
      E0 = 1.229d0-0.85d-3*(FC_OperatingTemp-298.15d0)+4.3085d-5*FC_OperatingTemp*LOG(pH2*(pO2**0.5)/pH2O)  ![V] Open circuit voltage 
      deltaG = deltaG0-Runiv*FC_OperatingTemp*LOG(pH2*SQRT(pO2)/pH2O)/(2.d0*FaradayConst)  ![kJ/mol] Gibbs free energy at actual conditions
      
      eta_voltage = FC_Vstack/(E0*FC_CellNumber) ![-] Voltage efficiency 
      eta_th = - deltaG/HHVh2Mol                       ![-] Thermodynamic efficiency
     
      etaFC = eta_th*eta_voltage ![-] FC efficiency
     
     
      ! Hydrogen demand
     
      FC_HydroCons = CellCurr*FC_CellNumber*3600.d0/(95719.25d0*1000.d0)/rhoStdh2*timestep  ![Sm3] (Chavan 2017)
      FC_deltaHydrogen = - FC_HydroCons*rhoStdh2*HHVh2*1000.d0/3600.d0  ![kWh]
     
     
      ! Process heat, could be recovered
      
      FC_Heat = ((1.481d0*FC_CellNumber)/FC_Vstack-1.d0)*ReqPower*timestep  ![kWh]

      ! Capex and Opex
      FCStackCapex = FC_Cost*FC_NominalPower        ![eur]
      FCStackOpex = FCStackCapex*0.02d0            ![eur/y]
          
      
 end subroutine FuelCell


! Subroutine to compute the V-I curve of the fuel cell and its linear regression coefficients
 
 subroutine Coefficients_FC(FC_CellNumber,     & ! (I) n° of fuel cells           [-]
                         FC_MinCurrDens,    & ! (I) FC min. current density    [A/cm^2]
                         FC_MaxCurrDens,    & ! (I) FC max. current density    [A/cm^2]
                         FC_CellArea,       & ! (I) fuel cell active area      [cm^2] 
                         FC_NominalPower,   & ! (I) Max. power deliverable     [kW]                
                         FC_OperatingTemp,  & ! (I) FC operating temperature   [K] 
                         FC_FuelPress,      & ! (I) Fuel supply pressure (Anode) [K]   
                         FC_AirPress,       & ! (I) Air supply pressure (Cathode)[K]
                         FC_AnodeCurrDens,  &  ![-] Cell mositure content
                         FC_CathodeCurrDens, & ! (I)[A/cm^2]
                         Lambda, &             ! (I)[A/cm^2]            
                         coeff_A,           & ! (O) grade 0 coeff  [-]
                         coeff_B)             ! (O) grade 1 coeff  [-] 
             
      USE MODParam,       ONLY: FaradayConst,Runiv,eNepero                                       
                                       
      implicit real(8) (a-h,o-z), integer(i-n)
       
      real(8), intent(IN   ) :: FC_CellNumber,FC_MinCurrDens,FC_MaxCurrDens,FC_CellArea,FC_NominalPower,FC_OperatingTemp,FC_FuelPress,FC_AirPress,FC_AnodeCurrDens,FC_CathodeCurrDens,Lambda         
      
      real(8) ::  CellCurrDensity, &  ! [A/cm^2]
                  CellCurr,        &  ! [A]
                  pO2,             &  ! [Pa]
                  pH2,             &  ! [Pa]
                  E0,              &  ! [V]
                  CTC,             &  ! [-]
                  Lact_an,         &  ! [V]
                  Lact_cat,        &  ! [V]
                  Lact,            &  ! [V]
                  rho_m,           &  ! [Ohm*cm]
                  MembThickness,   &  ! [micro-m]
                  Lohm                ! [Ohm] 
                  
      integer, parameter :: Ndatapoints = 40 ! NB if changing Ndatapoints must be changed also in LinRegLeastSquares            
      real(8), dimension(Ndatapoints) :: vector_i, vector_V 
              
      vector_i = 0.d0    ![A/cm^2]
      vector_V = 0.d0    ![V]
      
      do i = 1,Ndatapoints 
      
        CellCurrDensity = (FC_MaxCurrDens-FC_MinCurrDens)/Ndatapoints*i
        vector_i(i) = (FC_MaxCurrDens-FC_MinCurrDens)/Ndatapoints*i      ![A/cm^2]
        
        CellCurr = vector_i(i)*FC_CellArea                   ![A]
        
      
        ! Polarization (V-i) curve calculation
        ! V is obtained by summing 3 terms (concentration losses are negligible)
      
        ! 1- Cell open curcuit voltage
        
        pO2 = (FC_AirPress*0.21d0)/101325.d0 ![atm]
        pH2O = 1.d0                          ![atm]
        pH2 = FC_FuelPress/101325.d0         ![atm]
            
        E0 = 1.229d0-0.85d-3*(FC_OperatingTemp-298.15d0)+4.3085d-5*FC_OperatingTemp*LOG(pH2*(pO2**0.5)/pH2O)  ![V]
           
        ! 2- Activation losses
        
        if (FC_NominalPower .le. 6.d0) then
          CTC = 0.45d0
        else if (FC_NominalPower .gt. 6.d0) then
          CTC = 0.4d0
        end if
          
        Lact_cat = -Runiv*FC_OperatingTemp*LOG10(FC_CathodeCurrDens)/(CTC*4.d0*FaradayConst)+Runiv*FC_OperatingTemp*LOG10(vector_i(i))/(CTC*4.d0*FaradayConst) ![V]
        Lact_an = -Runiv*FC_OperatingTemp*LOG10(FC_AnodeCurrDens)/(CTC*2.d0*FaradayConst)+Runiv*FC_OperatingTemp*LOG10(vector_i(i))/(CTC*2.d0*FaradayConst) ![V]
      
        Lact = Lact_cat +Lact_an  ![V] Activation losses
       
        ! 3- Ohmic losses
     
        rho_m = (181.6d0*(1.d0+0.03d0*(CellCurr/FC_CellArea)+0.062d0*((FC_OperatingTemp/303.d0)**2.d0)*(CellCurr/FC_CellArea)**2.5d0))/ &
                ((Lambda-0.634d0-3.d0*(CellCurr/FC_CellArea))*eNepero**(4.18d0*(FC_OperatingTemp-303.d0)/FC_OperatingTemp))                           ![Ohm*cm] specific membrane recistence
        
        if (FC_NominalPower .le. 6.d0) then
          MembThickness = 100.d0
        else if (FC_NominalPower .gt. 6.d0) then
          MembThickness = 145.d0
        end if
          
        Rm = rho_m*MembThickness/(10000.d0*FC_CellArea)   ![Ohm]
     
        Lohm = CellCurr*Rm  ![Ohm] Ohmic losses
      
        ! Resulting stack voltage
     
        vector_V(i) = FC_CellNumber*(E0-Lact-Lohm) ![V] 

      end do

      ! Linear regression coefficients calculation
   
      call LinRegLeastSquares(vector_i, & ! (I) operating i    [A/cm^2]
                              vector_V, & ! (I) operating V    [V]
                              coeff_A,  & ! (O) grade 0 coeff  [-]
                              coeff_B)    ! (O) grade 1 coeff  [-]
      
end subroutine Coefficients_FC    
