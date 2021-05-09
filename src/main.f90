 
! ================================================================
! Main program MESS - Multi Energy System Simulator (2021 version)
! ================================================================
! Pietro Lubello, Carlo Carcasci

program MultiEnSyst

      USE MODbatterytest
      USE MODwhichtechs, ONLY: whichtechs


      USE MODcoord, ONLY: icase
      USE MODparam, ONLY: NdayYear,NhourYear,Maxloc,Nyears,Nstep,MaxGiorni,MaxAddPar
      USE MODglobalparam, ONLY: RefYear


      USE MODAddHourlyData, ONLY: AddHourlyData

      USE MODbattery,     ONLY: TempArrBattDeg,SOHtemp,SOHtempbis,totalccal,totalccyc,iswitch !<<< WIP: temporary, to be removed
      USE MODelectrolyzer, ONLY: iswitchelectrolyzer
      USE MODfuelcell,     ONLY: iswitchfuelcell


      USE MODAggr,     ONLY: Nloc
      USE MODlocation, ONLY: Nelem

      implicit real(8) (a-h,o-z), integer(i-n)


      real(8), dimension(0:MaxGiorni):: curveNPV0, curveNPV, deltaNPV
      real(8)                        :: ActPBP,ProfInd

      real(8), dimension(NhourYear)  :: GasConsumpAggr , &
                                        GasConsumpAggr0

      real(8), dimension(NhourYear)  :: EnBoughtOutAggr ,EnSoldOutAggr , &
                                        EnBoughtOutAggr0,EnSoldOutAggr0

      type(Battery) :: batteria1

      real(8), dimension(Maxloc,NhourYear) :: deltaEEelectr,sourceEEelectr,sinkEEelectr
      real(8), dimension(Maxloc,NhourYear) :: deltaEEheatAmb,sourceEEheatAmb,sinkEEheatAmb
      real(8), dimension(Maxloc,NhourYear) :: deltaEEheatWat,sourceEEheatWat,sinkEEheatWat
      real(8), dimension(Maxloc,NhourYear) :: deltaEEcoolAmb,sourceEEcoolAmb,sinkEEcoolAmb

      real(8), dimension(NhourYear) :: deltaEEelectrAggr,sourceEEelectrAggr,sinkEEelectrAggr
      real(8), dimension(NhourYear) :: deltaEEheatAmbAggr,sourceEEheatAmbAggr,sinkEEheatAmbAggr
      real(8), dimension(NhourYear) :: deltaEEheatWatAggr,sourceEEheatWatAggr,sinkEEheatWatAggr
      real(8), dimension(NhourYear) :: deltaEEcoolAmbAggr,sourceEEcoolAmbAggr,sinkEEcoolAmbAggr

      real(8) :: deltaEEelectrAggrYear,sourceEEelectrAggrYear,sinkEEelectrAggrYear   
      real(8) :: deltaEEheatAmbAggrYear,sourceEEheatAmbAggrYear,sinkEEheatAmbAggrYear  
      real(8) :: deltaEEheatWatAggrYear,sourceEEheatWatAggrYear,sinkEEheatWatAggrYear  
      real(8) :: deltaEEcoolAmbAggrYear,sourceEEcoolAmbAggrYear,sinkEEcoolAmbAggrYear 

      character(len=19) :: timestamp
      character(len=19) :: temp 



      ! Program version
      ! ---------------

      character( 9), parameter ::Version= &                       ! Version
              !'0.00.00.a'   ! First 2021 version            15/02/2021 PL
               '0.01.00.a'   ! Added new components          31/03/2021 PL
               !-.--.--.-|<-Formatted till here

      character(10), parameter ::DateVer= '31/03/2021'   ! Release date
                                          !dd/mm/yyyy|<-Formatted till here



      call CPU_TIME(t1)

      iswitch = 0             ! <<< WIP: necessary for battery ageing
      iswitchelectrolyzer = 0 ! <<< WIP: necessary for electrolyzer
      iswitchfuelcell     = 0 ! <<< WIP: necessary for fuelcell
      AddHourlyData = 0.d0    ! <<< WIP: Initialization of AddHourlyData, should be moved


      ! Machine Precision
      ! -----------------

      Ieps =Precision(1.e0)
      IepsD=Precision(1.d0)

      Eps4Mac=10.d0**(-Ieps )
      Eps8Mac=10.d0**(-IepsD)


      ! Opening of some writing and reading files
      ! -----------------------------------------

      ! warning.out file

      open(unit=8,file='warning.out',form='formatted',status='UNKNOWN')
      rewind 8

      ! Writing version, machine precision etc.

      write( *,9000)Version,DateVer
      write( 8,9000)Version,DateVer
9000  format(/, 5x,57('*'),/,/, &
               10x,'PROGRAM:  "MESS: Multi Energy System Simulator"' ,/,&
               10x,'Version:  ',a9,/, &
               10x,'Release date: ',a10,' [dd/mm/yyyy]',/,/, &
      5x,57('*'),/,/)

      write(*,9001) Ieps,IEpsD
      write(8,9001) Ieps,IEpsD
9001  format(' Machine precision: Single-> Eps = 10^-',i2.2,/, &
             '                    Double-> EpsD= 10^-',i2.2,/ )


      ! Pre-process
      ! Data read and stored in corresponding modules
      ! ---------------------------------------------

      call ReadInputs()
      


      ! Calling outern layer: aggregate
      ! --------------------------

      !VAN0 = 0.d0 ! se vuoi mettere icase =1,1
      !VAN  = 0.d0 ! se vuoi mettere icase =0,0

      do icase=0,1

      GasConsumpAggr         = 0.d0

      call solver(deltaEEelectr,sourceEEelectr,sinkEEelectr,    & ! Single location hourly energy balances
                deltaEEheatAmb,sourceEEheatAmb,sinkEEheatAmb, &
                deltaEEheatWat,sourceEEheatWat,sinkEEheatWat, &
                deltaEEcoolAmb,sourceEEcoolAmb,sinkEEcoolAmb, &

                deltaEEelectrAggr,sourceEEelectrAggr,sinkEEelectrAggr,    & ! Whole aggregate hourly energy balances
                deltaEEheatAmbAggr,sourceEEheatAmbAggr,sinkEEheatAmbAggr, &
                deltaEEheatWatAggr,sourceEEheatWatAggr,sinkEEheatWatAggr, &
                deltaEEcoolAmbAggr,sourceEEcoolAmbAggr,sinkEEcoolAmbAggr, &

                deltaEEelectrAggrYear,sourceEEelectrAggrYear,sinkEEelectrAggrYear,    & ! Whole aggregate whole year balances   
                deltaEEheatAmbAggrYear,sourceEEheatAmbAggrYear,sinkEEheatAmbAggrYear, &
                deltaEEheatWatAggrYear,sourceEEheatWatAggrYear,sinkEEheatWatAggrYear, &
                deltaEEcoolAmbAggrYear,sourceEEcoolAmbAggrYear,sinkEEcoolAmbAggrYear, &

                curveNPV,               & !(O) NPV curve (€)
                TotActInvest,           & !(O) Total actualized investment
                EnBoughtOutAggr,        &
                EnSoldOutAggr,          &

                GasConsumpAggr)


        ! Saving VAN in the reference case

        select case(icase)
        case(0)
          curveNPV0(0:Nstep)      = curveNPV(0:Nstep)

          GasConsumpAggr0         = GasConsumpAggr

          EnBoughtOutAggr0        = EnBoughtOutAggr
          EnSoldOutAggr0          = EnSoldOutAggr

        end select

      enddo ! icase


      ! VAN analysis
      ! -----------

      call NPVcalc(curveNPV0,    & ! (I) VAN buying from grid
                   curveNPV,     & ! (I) VAN considered configuration
                   TotActInvest, & ! (I) Total actualized investment
                   deltaNPV,     & ! (O) VAN difference between two cases
                   ActPBP,       & ! (O) Actualized payback period
                   ProfInd)        ! (O) Profit index


      ! Writing output files
      ! --------------------

      ! 1) NPV analysis

      open(unit=95,file='NPVanalysis.out',form='formatted',status='UNKNOWN')
      rewind 95
      write(95,9010),ActPBP
9010  format(f12.4)!,t15,': Actualizazed PBP (Payback period)')
      write(95,9011),ProfInd
9011  format(f12.4)!,t15,': PI (Profit Index)')
      write(95,9012), deltaNPV(3649)
9012  format(f12.4)!,t15,': NPV(10)')
      write(95,9014), deltaNPV(7299)
9014  format(f12.4)!,t15,': NPV(20)')
      write(95,9015), deltaNPV(10949)
9015  format(f12.4)!,t15,': NPV(30)')


      open(unit=96,file='NPVcompletedata.out',form='formatted',status='UNKNOWN')
      rewind 96
      do i=0,Nstep
        write(96,*),curveNPV0(i),curveNPV(i),deltaNPV(i)
      enddo
9003  format(3(f15.4,2x))


      open(unit=99,file='NPVcompletedata.csv',form='formatted',status='UNKNOWN')
      rewind 99
      do i=0,Nstep
        write(99,9008),curveNPV0(i),curveNPV(i),deltaNPV(i)
      enddo
9008  format(2(f15.4,','),f12.4)


      ! 2) Reference year energy balances

      open(unit=97,file='output_case0.out',form='formatted',status='UNKNOWN')
      rewind 97
      do i=1,NhourYear
        write(97,9004),EnBoughtOutAggr0(i),EnSoldOutAggr0(i), &  
                       GasConsumpAggr0(i)
      enddo
9004  format(3(f12.4,2x))

      open(unit=98,file='output_case1.out',form='formatted',status='UNKNOWN')
      rewind 98
      do i=1,NhourYear
        write(98,9005),EnBoughtOutAggr (i),EnSoldOutAggr (i), &  
                       GasConsumpAggr (i)
      enddo
9005  format(3(f12.4,2x))


      ! 3) Battery degradation

      ! >>> TEMP: verifying how battery degradation works <<<
      open(unit=89,file='batterydegr.out',form='formatted',status='unknown')
      rewind 89
      do i=1,13
        write(89,9023),TempArrBattDeg(i)
      enddo
9023  format(f12.4)
      ! >>> TEMP: verifying how battery degradation works <<<


      ! >>> TEMP: grafici battery degradation <<<
      open(unit=88,file='SOHtemp.out',form='formatted',status='unknown')
      rewind 88
      do i=1,1565!NhourYear*Nyears
        write(88,9024),SOHtempbis(i) !SOHtemp(i)
      enddo
9024  format(f12.4)
      open(unit=87,file='ageing.out',form='formatted',status='unknown')
      rewind 87
      write(87,9025),totalccyc
      write(87,9025),totalccal
9025  format(f12.4)
      ! >>> TEMP: grafici battery degradation<<<



      ! 4) Detailed infos for techs

      open(unit=81,file='techinfo.csv',form='formatted',status='UNKNOWN')
      rewind 81
      write(81,9040,Advance = 'No') 'timestamp;'
      do ii=1,Nloc
        do jj = 1,Nelem(ii)
          do kk = 1,MaxAddPar
            if (ii.eq.Nloc .and. jj.eq.Nelem(ii) .and. kk.eq.MaxAddPar) then
              write(81,9041) ii,whichtechs(ii,jj),kk
            else
              write(81,9042,Advance = 'No') ii,whichtechs(ii,jj),kk
            endif
          enddo
        enddo
      enddo

9041  format(i0,a2,i0)
9042  format(i0,a2,i0,';')

      do ll = 1,NhourYear
        call hourtodate(RefYear,ll,timestamp)
        write(81,9070,Advance = 'No') timestamp
        do ii = 1,Nloc
          do jj = 1,Nelem(ii)
            do kk = 1,MaxAddPar
              if (ii.eq.Nloc .and. jj.eq.Nelem(ii) .and. kk.eq.MaxAddPar) then
                write(81,9043) AddHourlyData(ii,jj,kk,ll)
              else
                write(81,9044,Advance = 'No') AddHourlyData(ii,jj,kk,ll)
              endif
            enddo
          enddo
        enddo
      enddo

9043  format(f0.3)
9044  format(f0.3,';')
        

      ! 5) location balances

      open(unit=82,file='locationsenergybalances.csv',form='formatted',status='UNKNOWN')
      rewind 82
      write(82,9040,Advance = 'No') 'timestamp;'
      do ii = 1,Nloc
        if (ii.eq.Nloc) then
          write(82,9045) ii,ii,ii,ii,ii,ii,ii,ii,ii,ii,ii,ii
        else
          write(82,9046,Advance = 'No') ii,ii,ii,ii,ii,ii,ii,ii,ii,ii,ii,ii
        endif
      enddo

      do ii = 1,NhourYear
        call hourtodate(RefYear,ii,timestamp)
        write(82,9070,Advance = 'No') timestamp
        do jj = 1,Nloc
          if (jj.eq.Nloc) then
            write(82, 9047) deltaEEelectr(jj,ii),sourceEEelectr(jj,ii),sinkEEelectr(jj,ii),    &   
                            deltaEEheatAmb(jj,ii),sourceEEheatAmb(jj,ii),sinkEEheatAmb(jj,ii), &
                            deltaEEheatWat(jj,ii),sourceEEheatWat(jj,ii),sinkEEheatWat(jj,ii), &
                            deltaEEcoolAmb(jj,ii),sourceEEcoolAmb(jj,ii),sinkEEcoolAmb(jj,ii)
          else
            write(82, 9048, Advance = 'No') deltaEEelectr(jj,ii),sourceEEelectr(jj,ii),sinkEEelectr(jj,ii),    &   
                                            deltaEEheatAmb(jj,ii),sourceEEheatAmb(jj,ii),sinkEEheatAmb(jj,ii), &
                                            deltaEEheatWat(jj,ii),sourceEEheatWat(jj,ii),sinkEEheatWat(jj,ii), &
                                            deltaEEcoolAmb(jj,ii),sourceEEcoolAmb(jj,ii),sinkEEcoolAmb(jj,ii)
          endif
        enddo
      enddo

9045  format(i0'_EnBalanceElectr;',i0'_EnProdElectr;',i0'_EnConsElectr;',    & 
             i0'_EnBalanceAmbHeat;',i0'_EnProdAmbHeat;',i0'_EnConsAmbHeat;', &
             i0'_EnBalanceWatHeat;',i0'_EnProdWatHeat;',i0'_EnConsWatHeat;', &
             i0'_EnBalanceAmbCool;',i0'_EnProdAmbCool;',i0'_EnConsAmbCool')
9046  format(i0'_EnBalanceElectr;',i0'_EnProdElectr;',i0'_EnConsElectr;',    & 
             i0'_EnBalanceAmbHeat;',i0'_EnProdAmbHeat;',i0'_EnConsAmbHeat;', &
             i0'_EnBalanceWatHeat;',i0'_EnProdWatHeat;',i0'_EnConsWatHeat;', &
             i0'_EnBalanceAmbCool;',i0'_EnProdAmbCool;',i0'_EnConsAmbCool;')
9047  format(11(f12.3,';'),f12.3)
9048  format(12(f12.3,';'))


      ! 6) aggregate balances

      open(unit=83,file='cityenergybalances.csv',form='formatted',status='UNKNOWN')
      rewind 83
      write(83,9040,Advance = 'No') 'timestamp;'
      write(83,9051) 'EnBalanceElectr;' ,'EnProdElectr;' ,'EnConsElectr;' , &      
                     'EnBalanceAmbHeat;','EnProdAmbHeat;','EnConsAmbHeat;', &
                     'EnBalanceWatHeat;','EnProdWatHeat;','EnConsWatHeat;', &
                     'EnBalanceAmbCool;','EnProdAmbCool;','EnConsAmbCool'

      do ii = 1,NhourYear
        call hourtodate(RefYear,ii,timestamp)
        write(83,9070,Advance = 'No') timestamp
        write(83, 9052) deltaEEelectrAggr(ii),sourceEEelectrAggr(ii),sinkEEelectrAggr(ii),    &   
                        deltaEEheatAmbAggr(ii),sourceEEheatAmbAggr(ii),sinkEEheatAmbAggr(ii), &
                        deltaEEheatWatAggr(ii),sourceEEheatWatAggr(ii),sinkEEheatWatAggr(ii), &
                        deltaEEcoolAmbAggr(ii),sourceEEcoolAmbAggr(ii),sinkEEcoolAmbAggr(ii)
      enddo

9051  format(12a)
9052  format(11(f12.3,';'),f12.3)


9040  format(a10)
9070  format(a19,';')




!      efficiency = 0.99d0
!      SoC = 0.d0
!      batteria1 = Battery(efficiency,SoC)
!      write(*,*) batteria1%efficiency, batteria1%SoC
!      call aggiornamentoSoC(batteria1)
!      write(*,*) batteria1%efficiency, batteria1%SoC
 

      call CPU_TIME(t2)
      write(*,9006),t2-t1
9006  format(' Time of operation was',f8.4,' seconds')


      stop ' Apparently everything went well!'

end program MultiEnSyst