
! Modules for internal variables and parameters
! Lubello: dic 2019
! Simulation coordinates, paramateres, technology-related modules, modules for data output


! Module for simulation coordinates

module MODcoord

  integer :: icase        ! 0 = no battery ageing, 1 = battery ageing
  integer :: iyear        ! Which year are we in index
  integer :: itime        ! Which timestep are we in index
  integer :: itimeperweek ! Which timestep are we in in a week index
  integer :: itimeperday  ! Which timestep are we in in a day index
  integer :: iloc         ! Which location are we in in index

endmodule MODcoord


! Parameters

module MODParam

  ! Time

  real(8), parameter:: timestep    = 1                   ! 1 = 1 hour
  integer, parameter:: NsecHour    = 3600,             & ! Seconds per hour
                       NdayYear    = 365,              & ! Days per year
                       NhourDay    = 24,               & ! Hours per day
                       NhourWeek   = NhourDay*7,       & ! Hours per week
                       NhourYear   = NdayYear*NhourDay   ! Hours per year
  integer, parameter:: Nyears      = 30                  ! Number of years
  integer, parameter:: Nstep       = Nyears*NdayYear     ! Number of steps <<< WIP: should change for timestep not equal to 1 hour

  ! Max number of elements per different elements type

  integer, parameter:: MaxTypDem =   25,                 & ! Max number of different demand profiles in demand_Wel.dat
                       Maxloc    =  100,                 & ! Max number of locations
                       MaxComp   =   10,                 & ! Max components number per location
                       MaxAddPar =    6,                 & ! Max number of additional hourly parameters to be stored for each tech
                       MaxGiorni = NdayYear*Nyears + 60, & ! Max number of days in the time horizon considered
                       MaxHours  = NhourYear*Nyears+ 60    ! Max number of hours in the time horizon considered

  ! Physical constants <<< WIP: number of significant digits and units of measure should be consistent

  real(8), parameter :: Pstandard = 101325.d0,    & ! [Pa]
                        Tstandard =  288.15d0,    & ! [K]
                        rhoStdch4 = 0.6796,       & ! [kg/Sm3]    CH4 density @ T = 15°C p = 101325 Pa
                        rhoStdh2  = 0.0841,       & ! [kg/Sm3]    H2  density @ T = 15°C p = 101325 Pa
                        rhoStdh2o =  999.06d0,    & ! [kg/m3]     H2O density @ T = 15°C p = 101325 Pa
                        Runiv      = 8.3144621,   & ! [J/(mol*K)]
                        Rh2        = 4124.2d0,    & ! [J/(kg*K)] 
                        FaradayConst = 96485.d0,  & ! [C/mol]     Faraday constant
                        deltaG0      = -237.17d0, & ! [kJ/mol]    Gibbs free energy @ T = 25°C p = 101325 Pa
                        GammaPerfectGas = 1.4d0,  & ! [-]         Gamma = cp/cv  
                        LHVch4     = 47600.d0,    & ! [kJ/kg]     CH4 LHV
                        LHVh2      = 119.96d0,    & ! [MJ/kg]     H2 LHV
                        HHVh2      = 141.88,      & ! [MJ/kg]     H2 HHV
                        HHVh2Mol   = 285.83d0,    & ! [kJ/mol]
                        cpH2O      = 4.186,       & ! [kJ/(kg*K)] Water specific heat
                        h2oMolMass = 0.01801528,  & ! [kg/mol]    Water molar mass
                        H2MolMass = 2.01588d-3      ! [kg/mol]

  ! Math costants

  real(8), parameter:: eNepero    = 2.71828182845904523536d0

endmodule MODParam


! Technology-related modules

module MODbattery

  USE MODParam, ONLY: NhourWeek,NhourYear,Nyears,Maxloc

  integer :: iswitch

  real(8)                              :: tref_cal,ccal2,totalccal,totalccyc
  real(8), dimension(Maxloc)           :: ccyc,ccal,csum,SoHBattery
  real(8), dimension(NhourWeek,Maxloc) :: WeeklyChargeHist
  real(8), dimension(Nhouryear*Nyears) :: SOHtemp
  real(8), dimension(1565)             :: SOHtempbis
  real(8), dimension(13)               :: TempArrBattDeg ! <<< WIP: verifying how battery degradation works


endmodule MODbattery

module MODboiler ! <<< WIP: only one boiler per location is allowed

  USE MODParam, ONLY: Maxloc

  real(8), dimension(Maxloc) :: BoilerPeak

endmodule MODboiler

module MODelectrolyzer

  USE MODParam, ONLY: Maxloc      

  integer, dimension(Maxloc) :: iswitchelectrolyzer
  real(8), dimension(Maxloc) :: coeffAel,coeffBel

endmodule MODelectrolyzer

module MODfuelcell

  USE MODParam, ONLY: Maxloc      

  integer, dimension(Maxloc) :: iswitchfuelcell
  real(8), dimension(Maxloc) :: coeffAfc,coeffBfc

endmodule MODfuelcell


! Modules to help with data output

module MODAddHourlyData ! Module to store additional hourly data to be given as outputs

  USE MODParam, ONLY: Maxloc,MaxComp,MaxAddPar,NhourYear

  real(8), dimension(Maxloc,    & ! For each location
                     MaxComp,   & ! For each component (technology) of the location
                     MaxAddPar, & ! For each additional parameter
                     NhourYear) & ! For each hour of the reference year !<<< WIP: to be changed if the simulation goes on for more than 1y (e.g. ageing)
                     :: AddHourlyData                            

endmodule MODAddHourlyData

module MODwhichtechs ! Module to store which tech in each location

  USE MODParam, ONLY: Maxloc,MaxComp

  character(len=2), dimension(Maxloc,MaxComp) :: whichtechs

endmodule MODwhichtechs