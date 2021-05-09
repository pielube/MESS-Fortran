
! demand_Wel.dat file reading
! Carcasci: dic 2019

! demand_Th.dat file reading
! Lubello: feb 2019


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