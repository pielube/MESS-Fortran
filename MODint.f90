
! Modules for internal variables and parameters
! Lubello: dic 2019
! Simulation coordinates, paramateres, battery module


! Module for simulation coordinates

module MODcoord

  integer :: icase        ! 0 = no battery ageing, 1 = battery ageing
  integer :: iyear        ! Which year are we in index
  integer :: itime        ! Which timestep are we in index
  integer :: itimeperweek ! Which timestep are we in in a week index
  integer :: itimeperday  ! Which timestep are we in in a day index
  integer :: ibuild       ! Which building are we in in index

endmodule MODcoord


! Parameters

module MODParam

  ! Time data

  real(8), parameter:: timestep    = 1    ! 1 = 1 hour
  integer, parameter:: NsecHour    = 3600,             & ! Num sec per hour
                       NdayYear    = 365,              & ! Num di giorni all'anno   (365d/y)
                       NhourDay    = 24,               & ! Num di ore    al  giorno ( 24h/d)
                       NhourWeek   = NhourDay*7,       & ! Number of hours per week
                       NhourYear   = NdayYear*NhourDay   ! Numero di ore all'anno
  integer, parameter:: Nyears      = 30
  integer, parameter:: Nstep       = Nyears*NdayYear     ! >>> WIP: forse sarebbe meglio un nome meno generico

  ! Max number of elements per different elements type

  integer, parameter:: MaxTypDem =   25,                & ! Massimo Num di tipologie di domanda
                       MaxBuild  =  100,                & ! Massimo Num di edifici
                       MaxComp   =   10,                & ! Massimo Num di componenti per ogni edificio
                       MaxAddPar =    6                   ! Max number of additional hourly parameters to be stored for each tech
  integer, parameter:: MaxGiorni = NdayYear*Nyears + 60 
  integer, parameter:: MaxHours  = NhourYear*Nyears+ 60

  integer, parameter:: MaxDemand = 25 ! Max number of different demand profiles

  ! Physical parameters

  real(8), parameter :: Pstandard = 101325.d0, & ! [Pa]
                        Tstandard =  288.15d0 ! [K]

  real(8), parameter:: LHVch4     = 47600.d0, & ! [kJ/kg]  CH4 LHV
                       LHVh2      = 119.96d0, & ! [MJ/kg]  H2 LHV
                       HHVh2      = 141.88,   & ! [MJ/kg]  H2 HHV
                       HHVh2Mol   = 285.83d0  ! [kJ/mol]

  real(8), parameter:: rhoStdch4  = 0.6796, &       ! [kg/Sm3] CH4 density @ T = 15°C p = 101325 Pa
                       rhoStdh2   = 0.0841, &       ! [kg/Sm3] H2 density @ T = 15°C p = 101325 Pa
                       rhoStdh2o     =  999.06d0 ! [kg/m3] H2O density @ T = 15°C p = 101325 Pa

  real(8), parameter:: Runiv      = 8.3144621, &    ! [J/(mol*K)]
                       Rh2        = 4124.2d0       ! [J/(kg*K)]   

  real(8), parameter:: cpH2O      = 4.186 ! [kJ/(kg*K)] Water specific heat
                       

  real(8), parameter:: h2oMolMass = 0.01801528,      &   ! [kg/mol] Water molar mass
                       H2MolMass = 2.01588d-3 ! [kg/mol]

  real(8), parameter :: FaradayConst      = 96485.d0 ! [C/mol] Faraday constant

  real(8), parameter :: deltaG0     = -237.17d0, & ! [kJ/mol] Gibbs free energy @ T = 25°C p = 101325 Pa
                        GammaPerfectGas = 1.4d0 ! [-] Gamma = cp/cv

  ! Math costants

  real(8), parameter:: eNepero    = 2.71828182845904523536d0

endmodule MODParam


! Battery related module

module MODbattery

  USE MODParam, ONLY: NhourWeek,NhourYear,Nyears,MaxBuild

  integer :: iswitch

  real(8), dimension(MaxBuild) :: ccyc,ccal,csum,SoHBattery
  real(8)                       :: tref_cal,ccal2,totalccal,totalccyc

  real(8), dimension(NhourWeek,MaxBuild) :: WeeklyChargeHist
  real(8), dimension(Nhouryear*Nyears)   :: SOHtemp
  real(8), dimension(1565)               :: SOHtempbis

  real(8), dimension(13) :: TempArrBattDeg !<<< WIP: verifying how battery degradation works


endmodule MODbattery


! Boiler related module >>> WIP: only one boiler per building is allowed

module MODboiler

  USE MODParam, ONLY: MaxBuild

  real(8), dimension(MaxBuild) :: BoilerPeak

endmodule MODboiler

module MODelectrolyzer

  USE MODParam, ONLY: MaxBuild      

  integer, dimension(MaxBuild) :: iswitchelectrolyzer
  real(8), dimension(Maxbuild) :: coeffAel,coeffBel

endmodule MODelectrolyzer

module MODfuelcell

  USE MODParam, ONLY: MaxBuild      

  integer, dimension(MaxBuild) :: iswitchfuelcell
  real(8), dimension(Maxbuild) :: coeffAfc,coeffBfc

endmodule MODfuelcell


! Module for additional hourly data to be stored and printed in the end

module MODAddHourlyData

  USE MODParam, ONLY: MaxBuild,MaxComp,MaxAddPar,NhourYear

  real(8), dimension(MaxBuild,  & ! For each building
                     MaxComp,   & ! For each component (technology) of the building
                     MaxAddPar, & ! For each additional parameter
                     NhourYear) & ! For each hour of the reference year !<<< WIP: to be changed if the simulation will go on for more than 1y (e.g. ageing)
                     :: AddHourlyData                            !         if ageing, if there are no diff in en balances btw 30y, remember to save just 1y         

endmodule MODAddHourlyData

module MODcitydescription

  USE MODParam, ONLY: MaxBuild,MaxComp

  character(len=2), dimension(MaxBuild,MaxComp) :: citytechs

endmodule MODcitydescription