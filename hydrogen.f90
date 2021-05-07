
! Electrolyzer, tank, fuel cell
! Lubello: may 2020

subroutine Hydrogen(ScSpecEn,      & ! (I)  Specific scission energy             [kWh/m3]
                    etaElectrol,   & ! (I)  Electrolyzer efficiency              [-]
                    TankVol,       & ! (I)  Tank volume                          [m3]
                    StorageEnDens, & ! (I)  Energy density of the storage system [kWh/m3]
                    etaFC,         & ! (I)  Fuel cell efficiency                 [-]
                    CostElectrol,  & ! (I)  Electrolyzer                         [eur]
                    CostTank,      & ! (I)  Tank                                 [eur]
                    CostFC,        & ! (I)  Fuel cell                            [eur]
                    SoC,           & ! (IO) State of Charge                      [-]
                    deltaHydrogen, & ! (IO) deltaProdDem                         [kWh]
                    CapexHydrogen)   ! (O)  Capex for the whole system           [eur]


      USE MODParam,       ONLY: timestep,Runiv,rhoStdh2o,h2oMolMass


      implicit real(8) (a-h,o-z), integer(i-n)

      real(8), intent(IN   ) :: ScSpecEn,etaElectrol
      real(8), intent(IN   ) :: TankVol,StorageEnDens
      real(8), intent(IN   ) :: etaFC
      real(8), intent(IN   ) :: CostElectrol,CostTank,CostFC
      real(8), intent(INOUT) :: SoC, deltaHydrogen
      real(8), intent(  OUT) :: CapexHydrogen


      Temp  = 30.d0 +273.15d0 ![K]
      Press = 5.d5            ![Pa] DA METTERE COME INPUT

      ChargeMin = 0.d0                  ![kWh]
      ChargeMax = TankVol*StorageEnDens ![kWh]
      StartingCharge = SoC*ChargeMax    ![kWh]


      if(deltaHydrogen .ge. 0.d0) then
      ! Charging (or null)

        !PortWatIn    = deltaHydrogen/(ScSpecEn*etaElectrol*timestep) ![m3/h]
        !PortMolWatIn = PortWatIn*rhoStdh2o/WatMolMass                   ![mol/h]  
        !PortMolHydr  = PortWatIn                                     ![mol/h]
        !PortVolHydr  = PortMolHydr*(Runiv/3.6d6)*Temp/Press          ![m3/h]
        !deltaCharge  = PortVolHydr*timestep*StorageEnDens            ![kWh]

        etaResult = (rhoStdh2o*Runiv*Temp*StorageEnDens            ) / &
                    (ScSpecEn*etaElectrol*WatMolMass*3.6d6*Press)
        deltaCharge = deltaHydrogen * etaResult

        Charge       = StartingCharge + deltaCharge         ![kWh]

        if(Charge .gt. ChargeMax)then
          deltaCharge = Charge-ChargeMax
          Charge      = ChargeMax
          deltaHydrogen = deltaCharge/etaResult
        else
          deltaHydrogen = 0.d0
        endif


      else
      ! Discharging
        deltaCharge = deltaHydrogen/etaFC
        Charge      = StartingCharge + deltaCharge

        if(Charge .lt. ChargeMin)then
          deltaCharge   = Charge-ChargeMin
          deltaHydrogen = deltaCharge*etaFC
          Charge        = ChargeMin
        else
          deltaHydrogen = 0.d0
        endif

      endif 


      SoC = Charge/ChargeMax

      CapexHydrogen = CostElectr + CostTank + CostFC


1000  return

end subroutine Hydrogen