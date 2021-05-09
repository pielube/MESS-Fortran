
! ICE subroutine
! Lubello: oct 2019

subroutine ICEEq(PP,        & ! (I) Power required           [kW]
                 etaICE,    & ! (I) ICE thermal efficiency   [-]
                 LHV,       & ! (I) Fuel LHV                 [kJ/(kgK)]
                 PortFuel)    ! (O) Fuel mass flow           [kg/s]

       implicit real(8) (a-h,o-z), integer(i-n)

       real(8), intent(IN   ) :: PP,etaICE,LHV
       real(8), intent(  OUT) :: PortFuel

       PortFuel = PP/(LHV*etaICE)

       return

end subroutine ICEEq