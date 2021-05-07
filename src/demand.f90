
! Electrical demand
! Carcasci: dic 2019

! Thermal demand
! Lubello: feb 2019


subroutine DemandWel(Indtype,   & ! (I) Indice tipologia utenza  (1=...))
                     IndWel,    & ! (I) Indice potenza elettrica (1=Potenza [kW] massima, 2= Potenza media annuale[kW], 3= Energia annuale [kWh/y])
                     Welinp,    & ! (I) Reference power
                     WWel1)       ! (O) Energy demand at instant itime [kWh]


      USE MODcoord,     ONLY: itime
      USE MODparam   , ONLY: timestep,NhourYear
      USE MODdemand  , ONLY: WWel

      implicit real(8) (a-h,o-z), integer(i-n)

      integer, intent(IN   ):: Indtype,IndWel
      real(8), intent(IN   ):: Welinp
      real(8), intent(  OUT):: WWel1


      ! >>>>> WIP: working hours =/ from hourly data <<<<<

      !call ryp2(NhourYear,2,1, &
      !          WWel%ndata,WWel%hour(:),WWel%Demand(:,Indtype), &
      !          dble(itime),WWel1,    IndErrAmb)

      ! >>>>> WIP: working hours =/ from hourly data <<<<<


      WWel1 = WWel%demand(itime,Indtype)

      select case(IndWel)
      case(1)
        !* assegnata potenza (kW) massima
        WWel1 = WWel1*Welinp/WWel%max(Indtype)

      case(2)
        !* assegnata potenza (kW) media
        WWel1=WWel1*Welinp/WWel%Ave(Indtype)

      case(3)
        !* assegnata energia annuale (kWh/y)
        WWel1=WWel1*Welinp/(WWel%Ave(Indtype)*8760.d0)

      case default
        write(*,*) 'Wrong option for WWel'
        stop 'index error in "DemandWel"'
      end select

      WWel1 = WWel1 * timestep ![kWh]

      return


end subroutine DemandWel




subroutine DemandTherm(IndQth,                                                              & ! (I) Indice potenza termica (1=Potenza [kW] massima, 2= Potenza media annuale[kW], 3= Energia annuale [kWh/y])
                       QQAmbHeatIn,QQWatHeatIn,QQProcessHeatIn,QQAmbCoolIn,QQOtherHeatIn,   & ! (I) Reference power
                       QQAmbHeat,  QQWatHeat,  QQProcessHeat,  QQAmbCool,  QQOtherHeat)       ! (O) Energy demand at instant itime [kWh]


      USE MODcoord,    ONLY:itime
      USE MODparam,    ONLY: timestep,NhourYear
      USE MODdemand,   ONLY: QQth


      implicit real(8) (a-h,o-z), integer(i-n)

      integer, intent(IN   ):: IndQth
      real(8), intent(IN   ):: QQAmbHeatIn,QQWatHeatIn,QQProcessHeatIn,QQAmbCoolIn,QQOtherHeatIn
      real(8), intent(  OUT):: QQAmbHeat  ,QQWatHeat  ,QQProcessHeat  ,QQAmbCool  ,QQOtherHeat


      ! >>>>> WIP: working hours =/ from hourly data <<<<<

      !call ryp2(NhourYear,2,1, &
      !          QQth%ndata,QQth%hour(:),QQth%AmbHeat(:), &
      !          dble(itime),QQAmbHeat,    IndErrAmb)
      !
      !call ryp2(NhourYear,2,1, &
      !          QQth%ndata,QQth%hour(:),QQth%WatHeat(:), &
      !          dble(itime),QQWatHeat,    IndErrAmb)
      !
      !call ryp2(NhourYear,2,1, &
      !          QQth%ndata,QQth%hour(:),QQth%ProcessHeat(:), &
      !          dble(itime),QQProcessHeat,    IndErrAmb)
      !
      !call ryp2(NhourYear,2,1, &
      !          QQth%ndata,QQth%hour(:),QQth%AmbCool(:), &
      !          dble(itime),QQAmbCool,    IndErrAmb)
      !
      !call ryp2(NhourYear,2,1, &
      !          QQth%ndata,QQth%hour(:),QQth%OtherHeat(:), &
      !          dble(itime),QQOtherHeat,    IndErrAmb)

      ! >>>>> WIP: working hours =/ from hourly data <<<<<


      QQAmbHeat     = QQth%AmbHeat(itime)
      QQWatHeat     = QQth%WatHeat(itime)
      QQProcessHeat = QQth%ProcessHeat(itime)
      QQAmbCool     = QQth%AmbCool(itime)
      QQOtherHeat   = QQth%OtherHeat(itime)


      select case(IndQth)
      case(1)
        !* assegnata potenza (kW) massima
        QQAmbHeat     = QQAmbHeat    *QQAmbHeatIn    /QQth%MaxAmbHeat
        QQWatHeat     = QQWatHeat    *QQWatHeatIn    /QQth%MaxWatHeat
        QQProcessHeat = QQProcessHeat*QQProcessHeatIn/QQth%MaxProcessHeat
        QQAmbCool     = QQAmbCool    *QQAmbCoolIn    /QQth%MaxAmbCool
        QQOtherHeat   = QQOtherHeat  *QQOtherHeatIn  /QQth%MaxOtherHeat

      case(2)
        !* assegnata potenza (kW) media
        QQAmbHeat     = QQAmbHeat    *QQAmbHeatIn    /QQth%AveAmbHeat
        QQWatHeat     = QQWatHeat    *QQWatHeatIn    /QQth%AveWatHeat
        QQProcessHeat = QQProcessHeat*QQProcessHeatIn/QQth%AveProcessHeat
        QQAmbCool     = QQAmbCool    *QQAmbCoolIn    /QQth%AveAmbCool
        QQOtherHeat   = QQOtherHeat  *QQOtherHeatIn  /QQth%AveOtherHeat

      case(3)
        !* assegnata energia annuale (kWh/y)
        QQAmbHeat     = QQAmbHeat    *QQAmbHeatIn    /(QQth%AveAmbHeat    *8760.d0)
        QQWatHeat     = QQWatHeat    *QQWatHeatIn    /(QQth%AveWatHeat    *8760.d0)
        QQProcessHeat = QQProcessHeat*QQProcessHeatIn/(QQth%AveProcessHeat*8760.d0)
        QQAmbCool     = QQAmbCool    *QQAmbCoolIn    /(QQth%AveAmbCool    *8760.d0)
        QQOtherHeat   = QQOtherHeat  *QQOtherHeatIn  /(QQth%AveOtherHeat  *8760.d0)

      case default
        write(*,*) 'Wrong option for QQth'
        stop 'index error in "DemandQth"'
      end select

      QQAmbHeat     = QQAmbHeat     * timestep ![kWh]
      QQWatHeat     = QQWatHeat     * timestep ![kWh]
      QQProcessHeat = QQProcessHeat * timestep ![kWh]
      QQAmbCool     = QQAmbCool     * timestep ![kWh]
      QQOtherHeat   = QQOtherHeat   * timestep ![kWh]

      return


end subroutine DemandTherm