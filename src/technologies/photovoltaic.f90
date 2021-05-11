
! Photovoltaic panels subroutine
! Lubello: oct 2019

subroutine PhotoArrEq(Area,           & ! (I) Photovoltaic panel area               [m^2]
                      fActive,        & ! (I) Area fraction with active solar cells [-] default: 0.95 - 1.00
                      SolIrr,         & ! (I) Solar irradiance                      [W/m^2]
                      etaCell,        & ! (I) Module conversion efficiency          [-] default: 0.20
                      CostSolPhoto,   & ! (I) Cost per kW panels                    [eur/kW]
                      WWel,           & ! (O) AC Energy produced                    [kWh]
                      CapexSolPhoto)    ! (O) Capex Sol Photo                       [eur]

      USE MODparam,  ONLY: timestep

      implicit real(8) (a-h,o-z), integer(i-n)
      
      real(8), intent(IN   ) :: Area,fActive,SolIrr,etaCell,CostSolPhoto
      real(8), intent(  OUT) :: WWel,CapexSolPhoto

      real(8)                :: etaInvert
      real(8), parameter     :: Ainv = 97.004, Binv = -1.58, Cinv = -0.362

      WWel          = 0.d0
      CapexSolPhoto = 0.d0
      WWelPeak      = Area*fActive*1.d3*etaCell/1.d3 ! [kW] Nominal power
      WWel = Area*fActive*SolIrr*etaCell/1.d3        ! [kW] Power output

      ! Inverter
      if(WWel.ne.0.d0)then
        WWadim    = WWel/WWelPeak
        etaInvert = (Ainv + Binv*WWadim + Cinv/WWadim)/100.d0
        WWel      = WWel*etaInvert
      else
      endif

      WWel = WWel*timestep ! [kWh] Energy produced

      CapexSolPhoto = CostSolPhoto*WWelPeak ! [eur] Capex
      
      return

end subroutine PhotoArrEq