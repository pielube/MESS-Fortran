
! Subroutines to read and save in modules all input files
! ReadInputs calls all other subroutines 
! Lubello: may 2020

subroutine ReadInputs()

      implicit real(8) (a-h,o-z), integer(i-n)

      ! Global parameters
      call ReadGlobalParam()
      ! Ambient
      call ReadAmbData()
      ! Electrical demand
      call ReadDemWel()
      ! Thermal demand
      call ReadDemTherm()
      ! Energy prices
      call ReadEnPrices()
      ! aggregate structure
      call ReadAggr()

end subroutine ReadInputs


! globalparam.dat file reading
! Lubello: apr 2020

subroutine ReadGlobalParam()

      USE MODGlobalParam    , ONLY: RefYear,iTimeStart,iTimeEnd,IndAgeing
     

      implicit real(8) (a-h,o-z), integer(i-n)

      character(15), parameter   :: namef1='globalparam.dat'    !Input file for global paramaters
      

      ! Reading global parameteres
      ! --------------------------

      open(2,file=namef1,status='old',access='sequential',form='formatted',err=7000)
      iline=0
      rewind 2

      iline=iline+1
      read(2,*,err=7010) RefYear

      diff = abs(RefYear/4.d0-int(RefYear/4.d0))
      if(diff .lt. 0.00001d0)then
        write(*,*) 'Warning: you chose a leap year, while we are working with 365days-years'
      else
      endif

      iline=iline+1
      read(2,*,err=7010) iTimeStart  ! Simulation starting time
      iline=iline+1
      read(2,*,err=7010) iTimeEnd    ! Simulation ending time

      iline=iline+1
      read(2,*,err=7010) IndAgeing

      if(IndAgeing.ne.0 .and. IndAgeing.ne.1) then
        write(*,*) 'Wrong option for IndAgeing in ',namef1
        stop 'Input data error!'
      endif


      close(2)

      return

7000  continue
      write(*,7100) namef1
      write(8,7100) namef1
7100  format(/,'ERROR: Open error on "',a,'" file')
      stop 'Error!'
      
7010  continue
      write(*,7110) namef1,iline
      write(8,7110) namef1,iline
7110  format(/,'ERROR: Read error on "',a,'" file',/,   &
           t12,'Error line:           ',i6)
      stop 'Error!'


end subroutine ReadGlobalParam

! AmbData.dat file reading
! Lubello: dic 2019

subroutine ReadAmbData()

      USE MODparam   , ONLY: NhourYear
      USE MODambient , ONLY: ndataAmb,hourAmb,TempAmb,AirDens,SolIrr,WindSpeed

      implicit real(8) (a-h,o-z), integer(i-n)

      character(11), parameter                    :: namef='AmbData.dat'    !Input file name

      ! Reading ambient data
      ! ====================

      open(2,file=namef,status='old',access='sequential',form='formatted',err=7000)
      iline=0
      rewind 2

      ! Reading number of data points and checking if in bounds

      iline=iline+1
      read(2,*,err=7010) ndataAmb

      if (ndataAmb.lt.1 .or. ndataAmb.gt.NhourYear) then
        ! Error! Too few or too many ambient data points
        write(*,*)'Error! Too few or too many ambient data points'
        stop 'Input error!'
      else
        ! Ok Acceptable number of timesteps
      endif

      ! Reading actual data

      do ii=1,ndataAmb
        iline=iline+1
        read(2,*,err=7010) hourAmb   (ii), & ! Hour             [h] (decimale) dell'anno (0= mezzanotte dell'anno fino a 8760h che è fine anno)
                           Tempamb   (ii), & ! Temperature      [C]
                           AirDens   (ii), & ! Air density      [kg/m^3]
                           SolIrr    (ii), & ! Solar irradiance [W/m^2]
                           WindSpeed (ii)    ! Wind speed       [m/s]
      enddo

      close(2)

      return

7000  continue
      write(*,7100) namef
      write(8,7100) namef
7100  format(/,'ERROR: Open error on "',a,'" file')
      stop 'Error!'

7010  continue
      write(*,7110) namef,iline
      write(8,7110) namef,iline
7110  format(/,'ERROR: Read error on "',a,'" file',/,   &
           t12,'Error line:           ',i6)
      stop 'Error!'

end subroutine ReadAmbData

! demand_Wel.dat file reading
! Carcasci: dic 2019

subroutine ReadDemWel()

      USE MODparam , ONLY: NhourYear,MaxTypDem
      USE MODdemand, ONLY: WWel               ! Electrical power demand structure


      implicit real(8) (a-h,o-z), integer(i-n)

      character(14),   parameter                    :: namef='demand_Wel.dat'    !Input file name

      ! Reading demand data
      ! ====================

      open(2,file=namef,status='old',access='sequential',form='formatted',err=7000)
      iline=0
      rewind 2

      ! Reading number of data points

      iline=iline+1
      read(2,*,err=7010) WWel%ndata,WWel%NtypDem ! Number of data points and type of demands

      if(WWel%ndata.lt.1       .or. &
         WWel%ndata.gt.NhourYear     )   then
        write(*,*) 'Too many data points in ',namef
        stop 'Input data error!'
      endif

      if(WWel%NtypDem.lt.1       .or. &
         WWel%NtypDem.gt.MaxTypDem     )   then
        write(*,*) 'Too many data points in ',namef
        stop 'Input data error!'
      endif

      ! Reading actual data

      do j=1,WWel%NtypDem
        WWel%Max(j) =-1.d10
        WWel%Ave(j) = 0.d00
      enddo

      do ii=1,WWel%ndata
        iline=iline+1
        read(2,*,err=7010) WWEl%hour  (ii  ), & !
                          (WWel%Demand(ii,j),j=1,WWel%NtypDem)

        !* valore massimo e medio
        do j=1,WWel%NtypDem
          WWel%Max(j) =max(WWel%Demand(ii,j), WWel%Max(j))
          WWel%Ave(j) = WWel%Ave(j)+WWel%Demand(ii,j)
        enddo

      enddo

        !* valore massimo e medio
        do j=1,WWel%NtypDem
          WWel%Ave(j) = WWel%Ave(j)/dble(WWel%ndata)
        enddo

      close(2)

      return

7000  continue
      write(*,7100) namef
      write(8,7100) namef
7100  format(/,'ERROR: Open error on "',a,'" file')
      stop 'Error!'

7010  continue
      write(*,7110) namef,iline
      write(8,7110) namef,iline
7110  format(/,'ERROR: Read error on "',a,'" file',/,   &
           t12,'Error line:           ',i2)
      stop 'Error!'



end subroutine ReadDemWel

! demand_Th.dat file reading
! Lubello: feb 2020

subroutine ReadDemTherm()

      USE MODparam , ONLY: NhourYear,MaxTypDem
      USE MODdemand, ONLY: QQth               ! Thermal power demand structure


      implicit real(8) (a-h,o-z), integer(i-n)

      character(13),   parameter                    :: namef='demand_Th.dat'    !Input file name

      ! Reading demand data
      ! ====================

      open(2,file=namef,status='old',access='sequential',form='formatted',err=7000)
      iline=0
      rewind 2

      ! Reading number of data points

      iline=iline+1
      read(2,*,err=7010) QQth%ndata ! Number of data points

      if(QQth%ndata.lt.1       .or. &
         QQth%ndata.gt.NhourYear     )   then
        write(*,*) 'Too many data points in ',namef
        stop 'Input data error!'
      endif

      ! Reading actual data
      ! Inizialization

        QQth%MaxAmbHeat     =-1.d10
        QQth%AveAmbHeat     = 0.d00

        QQth%MaxWatHeat     =-1.d10
        QQth%AveWatHeat     = 0.d00

        QQth%MaxProcessHeat =-1.d10
        QQth%AveProcessHeat = 0.d00

        QQth%MaxAmbCool     =-1.d10
        QQth%AveAmbCool     = 0.d00

        QQth%MaxOtherHeat   =-1.d10
        QQth%AveOtherHeat   = 0.d00

      do ii=1,QQth%ndata
        iline=iline+1
        read(2,*,err=7010) QQth%hour(ii),        &
                           QQth%AmbHeat(ii),     &
                           QQth%WatHeat(ii),     &
                           QQth%ProcessHeat(ii), &
                           QQth%AmbCool(ii),     &
                           QQth%OtherHeat(ii)


        ! Max and mean (P1) value comp

          QQth%MaxAmbHeat     = max(QQth%AmbHeat(ii), QQth%MaxAmbHeat)
          QQth%AveAmbHeat     = QQth%AveAmbHeat+QQth%AmbHeat(ii)
          if (QQth%MaxAmbHeat .eq. 0.d0)then
            QQth%MaxAmbHeat =-1.d10
          endif

          QQth%MaxWatHeat     = max(QQth%WatHeat(ii), QQth%MaxWatHeat)
          QQth%AveWatHeat     = QQth%AveWatHeat+QQth%WatHeat(ii)
          if (QQth%MaxWatHeat .eq. 0.d0)then
            QQth%MaxWatHeat =-1.d10
          endif


          QQth%MaxProcessHeat = max(QQth%ProcessHeat(ii), QQth%MaxProcessHeat)
          QQth%AveProcessHeat = QQth%AveProcessHeat+QQth%ProcessHeat(ii)
          if (QQth%MaxProcessHeat .eq. 0.d0)then
            QQth%MaxProcessHeat =-1.d10
          endif

          QQth%MaxAmbCool     = max(QQth%AmbCool(ii), QQth%MaxAmbCool)
          QQth%AveAmbCool     = QQth%AveAmbCool+QQth%AmbCool(ii)
          if (QQth%MaxAmbCool .eq. 0.d0)then
            QQth%MaxAmbCool =-1.d10
          endif

          QQth%MaxOtherHeat   = max(QQth%OtherHeat(ii), QQth%MaxOtherHeat)
          QQth%AveOtherHeat   = QQth%AveOtherHeat+QQth%OtherHeat(ii)
          if (QQth%MaxOtherHeat .eq. 0.d0)then
            QQth%MaxOtherHeat =-1.d10
          endif


      enddo

        ! Mean value (P2)

          QQth%AveAmbHeat     = QQth%AveAmbHeat     /dble(QQth%ndata)
          QQth%AveWatHeat     = QQth%AveWatHeat     /dble(QQth%ndata)
          QQth%AveProcessHeat = QQth%AveProcessHeat /dble(QQth%ndata)
          QQth%AveAmbCool     = QQth%AveAmbCool     /dble(QQth%ndata)
          QQth%AveOtherHeat   = QQth%AveOtherHeat   /dble(QQth%ndata)

      close(2)


      return

7000  continue
      write(*,7100) namef
      write(8,7100) namef
7100  format(/,'ERROR: Open error on "',a,'" file')
      stop 'Error!'

7010  continue
      write(*,7110) namef,iline
      write(8,7110) namef,iline
7110  format(/,'ERROR: Read error on "',a,'" file',/,   &
           t12,'Error line:           ',i2)
      stop 'Error!'


end subroutine ReadDemTherm

! enprices.dat file reading
! Lubello: jan 2020

subroutine ReadEnPrices()

      USE MODparam    , ONLY: NhourYear
      USE MODprices   , ONLY: ndataPrice,hourPrice, &
                              elenPriceBuy,elenPriceSell, &                ! Electrical energy prices per hour
                              gasPriceHouseS,gasPriceHouseM,gasPriceHouseL ! Natural gas energy prices per hour (households)


      implicit real(8) (a-h,o-z), integer(i-n)

      character(14), parameter                    :: namef1='elenprices.dat'    !Input file name electrical energy
      character(13), parameter                    :: namef2='gasprices.dat'     !Input file name natural gas



      ! Reading electrical energy prices
      ! ================================

      open(2,file=namef1,status='old',access='sequential',form='formatted',err=7000)
      iline=0
      rewind 2

      ! Reading number of data points

      iline=iline+1
      read(2,*,err=7010) NdataPrice

      if(NdataPrice.lt.1 .or. NdataPrice.gt.NhourYear) then
        write(*,*) 'Too many data points in ',namef1
        stop 'Input data error!'
      endif


      ! Reading actual data

      do ii=1,ndataPrice
        iline=iline+1
        read(2,*,err=7010) hourPrice     (ii), & ! Ora [h] (decimale) dell'anno (0= mezzanotte dell'anno fino a 8760h che è fine anno)
                           elenPriceBuy  (ii), & ! Electrical energy buy  price [eur/kWh]
                           elenPriceSell (ii)    ! Electrical energy sell price [eur/kWh]

      enddo

      close(2)

      ! Reading natural gas prices
      ! ==========================

      open(3,file=namef2,status='old',access='sequential',form='formatted',err=7000)
      iline=0
      rewind 3

      ! Reading number of data points

      iline=iline+1
      read(3,*,err=7010) NdataPrice

      if(NdataPrice.lt.1 .or. NdataPrice.gt.NhourYear) then
        write(*,*) 'Too many data points in ',namef2
        stop 'Input data error!'
      endif


      ! Reading actual data

      do ii=1,ndataPrice
        iline=iline+1
        read(3,*,err=7010) hourPrice     (ii), & ! Ora [h] (decimale) dell'anno (0= mezzanotte dell'anno fino a 8760h che è fine anno)
                           gasPriceHouseS(ii), & ! Gas price small  household [eur/Sm3]
                           gasPriceHouseM(ii), & ! Gas price medium household [eur/Sm3]
                           gasPriceHouseL(ii)    ! Gas price large  household [eur/Sm3]


      enddo

      close(3)

      return

      ! 7000 - Errors electrical energy

7000  continue
      write(*,7100) namef1
      write(8,7100) namef1
7100  format(/,'ERROR: Open error on "',a,'" file')
      stop 'Error!'

7010  continue
      write(*,7110) namef1,iline
      write(8,7110) namef1,iline
7110  format(/,'ERROR: Read error on "',a,'" file',/,   &
           t12,'Error line:           ',i6)
      stop 'Error!'

      ! 8000 - Errors natural gas

8000  continue
      write(*,7100) namef2
      write(8,7100) namef2
8100  format(/,'ERROR: Open error on "',a,'" file')
      stop 'Error!'

8010  continue
      write(*,7110) namef2,iline
      write(8,7110) namef2,iline
8110  format(/,'ERROR: Read error on "',a,'" file',/,   &
           t12,'Error line:           ',i6)
      stop 'Error!'


end subroutine ReadEnPrices

!    Reading aggregate.dat
!    Lubello: Dic 2019

subroutine ReadAggr()

      USE MODparam, ONLY: Maxloc
      USE MODAggr,  ONLY: Nloc, locName

      implicit real(8) (a-h,o-z), integer(i-n)

      character(13), parameter                    :: namef='aggregate.dat'    ! File name



      open(2,file=namef,status='old',access='sequential',form='formatted',err=7000)
      iline=0
      rewind 2


      iline=iline+1
      read(2,*,err=7010) Nloc ! Number of locations


      if(Nloc.lt.1       .or. &
         Nloc.gt.Maxloc     )   then
        write(*,*) 'Too many locations, check ',namef
        stop 'Input data error!'
      endif

      do ii=1,Nloc
        iline=iline+1
        read(2,*,err=7010) locName  (ii) ! Locations' names
      enddo

      close(2)

      return

7000  continue
      write(*,7100) namef
      write(8,7100) namef
7100  format(/,'ERROR: Open error on "',a,'" file')
      stop 'Error!'

7010  continue
      write(*,7110) namef,iline
      write(8,7110) namef,iline
7110  format(/,'ERROR: Read error on "',a,'" file',/,   &
           t12,'Error line:           ',i2)
      stop 'Error!'



end subroutine ReadAggr

