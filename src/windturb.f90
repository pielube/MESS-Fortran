
! Wind turbine subroutine
! Lubello: jan 2020


subroutine WindTurb(rho,              & ! (I) Air density                       [kg/m^3]
                    WindSpeed,        & ! (I) Wind speed                        [m/s]
                    SweptArea,        & ! (I) Swept area                        [m^2] e.g. 39.6 m^2 (Aircon 10/10 kW)
                    etaWindTurb,      & ! (I) Wind turbine efficiency           [-] default: 0.45 (ca. 0.593*0.76, i.e. Betz*efficiency)
                    RatedPower,       & ! (I) Rated power of the wind turbine   [kW]
                    RatedWindSpeed,   & ! (I) Rated wind speed                  [m/s] e.g. 11.0 m/s (Aircon 10/10 kW)
                    CutIn,            & ! (I) Cut in wind speed                 [m/s] e.g.  2.5 m/s (Aircon 10/10 kW)
                    CutOut,           & ! (I) Cout out wind speed               [m/s] e.g. 32.0 m/s (Aircon 10/10 kW)
                    CostWindTurb,     & ! (I) Cost per kW turbine               [eur/kW] default: 990 eur/kW from EnergyPlan Cost Database
                    WWel,             & ! (O) Energy produced                   [kWh]
                    CapexWindTurb)      ! (O) Capex wind turbine                [eur]

      USE MODparam,  ONLY: timestep

      implicit real(8) (a-h,o-z), integer(i-n)

      real(8), intent(IN   ) :: rho,WindSpeed,SweptArea,etaWindTurb,RatedPower,RatedWindSpeed,CutIn,CutOut,CostWindTurb
      real(8), intent(  OUT) :: WWel,CapexWindTurb

      WWel          = 0.d0
      CapexWindTurb = 0.d0

      if(WindSpeed.ge.CutIn .and. &
         WindSpeed.le.CutOut) then
        ! Wind speed allows the turbine to produce power
        if(WindSpeed.gt.RatedWindSpeed)then
          ! Wind speed higher than rated wind speed, max power produced
          WWel = RatedPower
        else
          ! Wind speed higher lower rated wind speed, power according to Betz
          WWel = 0.5d0*rho*SweptArea*WindSpeed**3*etaWindTurb/1.d3
        endif

      elseif(WindSpeed.lt.CutIn)then
        ! Wind speed lower than cut in speed, power=0
        WWel = 0.d0

      else
        ! Wind speed higher than cut out speed, power=0
        WWel = 0.d0

      endif


      WWel = WWel * timestep ! [kWh]

      ! Capex exp for the wind turbine
      CapexWindTurb = CostWindTurb*RatedPower

      return

end subroutine WindTurb
