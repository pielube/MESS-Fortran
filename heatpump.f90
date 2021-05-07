
! Heat pump
! Lubello: may 2020

subroutine HeatPump(COPh,         & ! (I) COP heating             [-]
                    QQheatRatedP, & ! (I) Heating rated power     [kW]
                    CostHeatPump, & ! (I) Cost         [eur]
                    QQheatDem,    & ! (I) Heat demand             [kWh]
                    QQheatOut,    & ! (O) Heating effect          [kWh]
                    ElEnConsump,  & ! (O) Electricity consumption [kWh]
                    CapexHeatPump)  ! (O) Capex                   [eur]

      USE MODparam,  ONLY: timestep

      implicit real(8) (a-h,o-z), integer(i-n)

      real(8), intent(IN   ) :: COPh,QQheatRatedP,CostHeatPump,QQheatDem
      real(8), intent(  OUT) :: QQheatOut,ElEnConsump,CapexHeatPump

      real(8), parameter     :: factMin = 0.0, factMax = 1.5


      QQheatOut = 0.d0
      QQheatRated = QQheatRatedP*timestep ![kWh]
      QQheatDemPos = - QQheatDem


      ! Heat pump calculations

        if(QQheatDemPos .eq. 0.d0)then
          QQheatOut   = 0.d0
          ElEnConsump = 0.d0
        elseif(QQheatDemPos .gt. QQheatRated*factMax)then
          QQheatOut   = QQheatRated*factMax
          ElEnConsump = QQheatOut/COPh
        elseif(QQheatDemPos .lt. QQheatRated*factMin)then
          QQheatOut   = QQheatDemPos
          ElEnConsump = QQheatRated*factMin/COPh
        else
          QQheatOut   = QQheatDemPos
          ElEnConsump = QQheatOut/COPh
        endif


      ! Capex expenditure

      CapexHeatPump = CostHeatPump*QQheatRatedP


      return

end subroutine HeatPump