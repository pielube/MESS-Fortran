
! Modules for variables taken as input from file
! Lubello, Carcasci: dic 2019
! General parameters, demands, ambient conditions, energy prices

! General parameters    

module MODGlobalParam

  integer :: RefYear,IndAgeing

endmodule MODGlobalParam

    
! Demand structure definition as a type

Module MODdemand

  USE MODparam, ONLY: NhourYear,MaxTypDem

  ! Typical structure of a demand

  TYPE TYPdemandEl

      integer                                 :: Ndata,NtypDem ! Number of demand hours, Numero di tipologie
      real(8), dimension(NhourYear          ) :: hour          ! Array of demand hours      [h]
      real(8), dimension(NhourYear,MaxTypDem) :: Demand        ! Adimensional power request [-]
      real(8), dimension(          MaxTypDem) :: Max,Ave       ! Maximum value, average value

  END TYPE TYPdemandEl


  TYPE TYPdemandTh

      integer                       :: Ndata                         ! Number of demand hours
      real(8), dimension(NhourYear) :: hour                          ! Array of demand hours               [h]
      real(8), dimension(NhourYear) :: AmbHeat                       ! Ad thermal power ambient heating    [-]
      real(8), dimension(NhourYear) :: WatHeat                       ! Ad thermal power water heating      [-]
      real(8), dimension(NhourYear) :: ProcessHeat                   ! Ad process heat                     [-]
      real(8), dimension(NhourYear) :: AmbCool                       ! Ad cooling                          [-]
      real(8), dimension(NhourYear) :: OtherHeat                     ! Ad thermal power other e.g. kitchen [-]
      real(8)                       :: MaxAmbHeat,AveAmbHeat         ! Maximum value, average value
      real(8)                       :: MaxWatHeat,AveWatHeat         ! Maximum value, average value
      real(8)                       :: MaxProcessHeat,AveProcessHeat ! Maximum value, average value
      real(8)                       :: MaxAmbCool,AveAmbCool         ! Maximum value, average value
      real(8)                       :: MaxOtherHeat,AveOtherHeat     ! Maximum value, average value

  END TYPE TYPdemandTh


  ! Implemented demands

  TYPE(TYPdemandEl) :: WWel     ! Electrical power demand   [-]
  TYPE(TYPdemandTh) :: QQth     ! Thermal    power demand   [-]


end Module MODdemand


! Ambient conditions

Module MODambient

  USE MODparam, ONLY: NhourYear

  integer                       :: NdataAmb      ! Number of hours
  real(8), dimension(NhourYear) :: hourAmb       ! Array of hours                  [h]
  real(8), dimension(NhourYear) :: TempAmb,    & ! Array of ambient temperatures   [C]
                                   AirDens,    & ! Array of air density            [kg/m^3]
                                   SolIrr,     & ! Solar irradiance                [W/m^2]
                                   WindSpeed     ! Wind speed                      [m/s]

  real(8), parameter :: RelHumidity = 0.5d0 ! <<< WIP: rel humidty = const.

end Module MODambient


! Energy prices

Module MODprices

  USE MODparam, ONLY: NhourYear

  integer                       :: NdataPrice     ! Number of hours
  real(8), dimension(NhourYear) :: hourPrice      ! Array of hours                        [h]
  real(8), dimension(NhourYear) :: elenPriceBuy   ! Array of electricity prices           [eur/kWh]
  real(8), dimension(NhourYear) :: elenPriceSell  ! Array of electricity prices           [eur/kWh]
  real(8), dimension(NhourYear) :: gasPriceHouseS ! Array of NG prices - Small  household [eur/Sm3]
  real(8), dimension(NhourYear) :: gasPriceHouseM ! Array of NG prices - Medium household [eur/Sm3] 
  real(8), dimension(NhourYear) :: gasPriceHouseL ! Array of NG prices - Large  household [eur/Sm3]

end Module MODprices


! City info module

module MODcity

  USE MODparam, ONLY: MaxBuild

  integer :: iTimeStart,iTimeEnd                  ! Starting and ending timesteps
  integer :: Nbuild                               ! Number of buildings
  character(15), dimension(MaxBuild) :: BuildName ! Array of building names


end module

! Edificio info module

module MODedificio

  USE MODparam, ONLY: MaxBuild

  integer, dimension(MaxBuild) :: Nelem

end module

