
! enprices.dat file reading
! Lubello: gen 2020


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