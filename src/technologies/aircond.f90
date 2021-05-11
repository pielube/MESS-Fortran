
! Air conditioner
! Lubello: mar 2020

subroutine AirCond(COPc,         & ! (I) COP cooling             [-]
                   QQcoolRatedP, & ! (I) Cooling rated power     [kW]
                   CostAirCond,  & ! (I) Cost                    [eur/kW]
                   QQcoolDem,    & ! (I) Cooling demand          [kWh]
                   QQcoolOut,    & ! (O) Cooling effect          [kWh]
                   ElEnConsump,  & ! (O) Electricity consumption [kWh]
                   CapexAirCond)   ! (O) Capex                   [eur]

      USE MODparam,  ONLY: timestep

      implicit real(8) (a-h,o-z), integer(i-n)

      real(8), intent(IN   ) :: COPc,QQcoolRatedP,CostAirCond,QQcoolDem
      real(8), intent(  OUT) :: QQcoolOut,ElEnConsump,CapexAirCond

      real(8), parameter     :: factMin = 0.3, factMax = 1.5


      QQcoolOut = 0.d0
      QQcoolRated = QQcoolRatedP*timestep ![kWh]
      QQcoolDemPos = - QQcoolDem

        if(QQcoolDemPos .eq. 0.d0)then
          QQcoolOut   = 0.d0
          ElEnConsump = 0.d0
        elseif(QQcoolDemPos .gt. QQcoolRated*factMax)then
          QQcoolOut   = QQcoolRated*factMax
          ElEnConsump = QQcoolOut/COPc
        elseif(QQcoolDemPos .lt. QQcoolRated*factMin)then
          QQcoolOut   = QQcoolDemPos
          ElEnConsump = QQcoolRated*factMin/COPc
        else
          QQcoolOut   = QQcoolDemPos
          ElEnConsump = QQcoolOut/COPc
        endif

      CapexAirCond = CostAirCond*QQcoolRatedP ! Capex

      return

end subroutine AirCond