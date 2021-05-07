
! Boiler subroutine
! Lubello: feb 2021


subroutine Boiler(QQdem,       & ! (I) Heat demand       [kWh]
                  etaBoil,     & ! (I) Boiler efficiency [-]
                  CostBoiler,  & ! (I) Boiler cost       [eur/kW]
                  QQconsump,   & ! (O) Fuel consumption  [kWh]
                  GasConsump,  & ! (O) Fuel consumption  [Sm3]
                  CapexBoiler)   ! (O) Boiler capex      [eur]

      USE MODcoord,  ONLY: ibuild
      USE MODParam,  ONLY: timestep, NsecHour,LHVch4,rhoStdch4
      USE MODboiler, ONLY: BoilerPeak

      implicit real(8) (a-h,o-z), integer(i-n)

      real(8), intent(IN   ) :: QQdem,etaBoil,CostBoiler
      real(8), intent(  OUT) :: QQconsump,GasConsump,CapexBoiler


      BoilerPeak(ibuild) = max(BoilerPeak(ibuild),-QQdem/timestep) ! Updating peak power requested to the boiler

      if(QQdem .ge. 0.d0)then
        QQconsump  = 0.d0 ! [kWh]
        GasConsump = 0.d0 ! [Sm3]
      else
        QQconsump  = - QQdem/etaBoil ! [kWh] minus sign to make the consumption positive
        GasConsump = QQconsump*NsecHour/(LHVch4*rhoStdch4) ! [Sm3] = ([kWh]*[s/h])/([kJ/kg][kg/Sm3])
      endif

      ! Capex
      CapexBoiler = BoilerPeak(ibuild)*1.1d0*CostBoiler

      return

end subroutine Boiler