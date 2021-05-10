
! Flat plate collector subroutine
! Lubello: mar 2020

! References
! 1) Duffie, J. A., & Beckman, W. A. (2013). Solar engineering of thermal processes. John Wiley & Sons. Chapter 6, pg.290

subroutine SolarColl(Area,        & ! (I)  Flat solar collector area       [m^2]
                     Fr,          & ! (I)  Collector heat removal factor   [-] default: 0.75-0.80
                     SolIrr,      & ! (I)  Solar irradiance                [W/m^2]
                     TauAlpha,    & ! (I)  From eq. 5.9.3                  [-] deafault: 0.80
                     Ul,          & ! (I)  Heat transfer coefficient       [W/(m^2*K)] realistic value: 9.5 (Duffie pg.294)
                     Tin,         & ! (I)  Fluid temperature in            [K]
                     Tair,        & ! (I)  Air temperature                 [K]
                     CostSolColl, & ! (I)  Cost per m^2 panels             [eur/m^2]
                     QQdem,       & ! (I)  Energy demand                   [kWh]
                     SoC,         & ! (IO) State of charge                 [-]
                     QQproduced,  & ! (O)  Heat produced (might be stored) [kWh]
                     QQsolColl,   & ! (O)  Heat out system (panels + tank) [kWh]
                     QQdisc,      & ! (O)  Heat discarded (would result in SoC > 1) [kWh]
                     CapexSolColl)  ! (O)  Capex                           [eur]

      USE MODparam,  ONLY: timestep,NsecHour,cpH2O,rhoStdh2o

      implicit real(8) (a-h,o-z), integer(i-n)

      real(8), intent(IN   ) :: Area,Fr,SolIrr,taualpha,Ul,Tin,Tair,CostSolColl,QQdem
      real(8), intent(INOUT) :: SoC
      real(8), intent(  OUT) :: QQsolColl,QQproduced,CapexSolColl


      ! Tank sizing and initial charge

      VolTank  = Area*75.d0/1.d3                                      ! [m3]
      Capacity = VolTank*rhoStdh2o*cpH2O*(60.d0-45.d0)/dble(NsecHour) ! [kWh]
      Charge   = Capacity *SoC                                        ! [kWh]


      ! Power (converted to energy) produced by the panels

      Rad = SolIrr*TauAlpha ! [W/m2]
      Disp = Ul*(Tin-Tair)  ! [W/m2]

      if(Rad .gt. Disp) then
        QQ = Area*Fr*(Rad-Disp)/1.d3 ! [kW]
      else
        QQ = 0.d0 ! [kW]
      endif

      QQproduced = QQ * timestep ! [kWh]


      ! Tank balances and final SoC

      Charge = Charge*(1.d0 - 0.05d0)

      if (QQproduced .ge. abs(QQdem)) then
        QQsolColl = abs(QQdem)
        Charge = Charge + (QQproduced - abs(QQdem))
      else

        if (Charge .gt. (abs(QQdem) - QQproduced)) then
          QQsolColl = abs(QQdem)
          Charge = Charge - (abs(QQdem) - QQproduced)
        else
          QQsolColl = QQproduced + Charge
          Charge = 0.d0
        endif

      endif

      if (Charge .gt. Capacity) then
        QQdisc = Charge - Capacity
        Charge = Capacity
      else
      endif

      SoC = Charge/Capacity


      CapexSolColl = Area*CostSolColl ! Capex

       return

end subroutine SolarColl