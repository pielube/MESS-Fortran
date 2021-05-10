
! Electrical demand
! Carcasci: dic 2019

subroutine DemandWel(Indtype,   & ! (I) Which profile to choose in demand_Wel.dat
                     IndWel,    & ! (I) Which ref value to consider: 1 Max power [kW], 2 Mean power [kW], 3 Yealy en consump [kWh/year]
                     Welinp,    & ! (I) Actual ref value (demand scaled accordingly)
                     WWel1)       ! (O) Energy demand at given timestep [kWh]

      USE MODcoord,  ONLY: itime
      USE MODparam,  ONLY: timestep,NhourYear
      USE MODdemand, ONLY: WWel

      implicit real(8) (a-h,o-z), integer(i-n)

      integer, intent(IN   ):: Indtype,IndWel
      real(8), intent(IN   ):: Welinp
      real(8), intent(  OUT):: WWel1


      WWel1 = WWel%demand(itime,Indtype)

      select case(IndWel)
      case(1) ! Reference = Max power [kW]
        WWel1 = WWel1*Welinp/WWel%max(Indtype)

      case(2) ! Reference = Mean power [kW]
        WWel1=WWel1*Welinp/WWel%Ave(Indtype)

      case(3) ! Reference = Yearly overall energy consumption [kWh/year]
        WWel1=WWel1*Welinp/(WWel%Ave(Indtype)*8760.d0)

      case default
        write(*,*) 'Wrong option for WWel'
        stop 'Index error in "DemandWel"'
      end select

      WWel1 = WWel1 * timestep ![kWh]

      return

end subroutine DemandWel


! Thermal demand
! Lubello: feb 2020

subroutine DemandTherm(IndQth,                                                              & ! (I) Which ref value to consider: 1 Max power [kW], 2 Mean power [kW], 3 Yealy en consump [kWh/year]
                       QQAmbHeatIn,QQWatHeatIn,QQProcessHeatIn,QQAmbCoolIn,QQOtherHeatIn,   & ! (I) Actual ref value (demand scaled accordingly)
                       QQAmbHeat,  QQWatHeat,  QQProcessHeat,  QQAmbCool,  QQOtherHeat)       ! (O) Energy demand at given timestep [kWh]

      USE MODcoord,  ONLY: itime
      USE MODparam,  ONLY: timestep,NhourYear
      USE MODdemand, ONLY: QQth

      implicit real(8) (a-h,o-z), integer(i-n)

      integer, intent(IN   ):: IndQth
      real(8), intent(IN   ):: QQAmbHeatIn,QQWatHeatIn,QQProcessHeatIn,QQAmbCoolIn,QQOtherHeatIn
      real(8), intent(  OUT):: QQAmbHeat  ,QQWatHeat  ,QQProcessHeat  ,QQAmbCool  ,QQOtherHeat


      QQAmbHeat     = QQth%AmbHeat(itime)
      QQWatHeat     = QQth%WatHeat(itime)
      QQProcessHeat = QQth%ProcessHeat(itime)
      QQAmbCool     = QQth%AmbCool(itime)
      QQOtherHeat   = QQth%OtherHeat(itime)


      select case(IndQth)
      case(1) ! Reference = Max power [kW]
        QQAmbHeat     = QQAmbHeat    *QQAmbHeatIn    /QQth%MaxAmbHeat
        QQWatHeat     = QQWatHeat    *QQWatHeatIn    /QQth%MaxWatHeat
        QQProcessHeat = QQProcessHeat*QQProcessHeatIn/QQth%MaxProcessHeat
        QQAmbCool     = QQAmbCool    *QQAmbCoolIn    /QQth%MaxAmbCool
        QQOtherHeat   = QQOtherHeat  *QQOtherHeatIn  /QQth%MaxOtherHeat

      case(2) ! Reference = Mean power [kW]
        QQAmbHeat     = QQAmbHeat    *QQAmbHeatIn    /QQth%AveAmbHeat
        QQWatHeat     = QQWatHeat    *QQWatHeatIn    /QQth%AveWatHeat
        QQProcessHeat = QQProcessHeat*QQProcessHeatIn/QQth%AveProcessHeat
        QQAmbCool     = QQAmbCool    *QQAmbCoolIn    /QQth%AveAmbCool
        QQOtherHeat   = QQOtherHeat  *QQOtherHeatIn  /QQth%AveOtherHeat

      case(3) ! Reference = Yearly overall energy consumption [kWh/year]
        QQAmbHeat     = QQAmbHeat    *QQAmbHeatIn    /(QQth%AveAmbHeat    *8760.d0)
        QQWatHeat     = QQWatHeat    *QQWatHeatIn    /(QQth%AveWatHeat    *8760.d0)
        QQProcessHeat = QQProcessHeat*QQProcessHeatIn/(QQth%AveProcessHeat*8760.d0)
        QQAmbCool     = QQAmbCool    *QQAmbCoolIn    /(QQth%AveAmbCool    *8760.d0)
        QQOtherHeat   = QQOtherHeat  *QQOtherHeatIn  /(QQth%AveOtherHeat  *8760.d0)

      case default
        write(*,*) 'Wrong option for QQth'
        stop 'Index error in "DemandQth"'
      end select

      QQAmbHeat     = QQAmbHeat     * timestep ![kWh]
      QQWatHeat     = QQWatHeat     * timestep ![kWh]
      QQProcessHeat = QQProcessHeat * timestep ![kWh]
      QQAmbCool     = QQAmbCool     * timestep ![kWh]
      QQOtherHeat   = QQOtherHeat   * timestep ![kWh]

      return

end subroutine DemandTherm