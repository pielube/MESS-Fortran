
! Rainflow module
! Lubello: mar 2020

! Subroutine madre

subroutine newrainflow(NhourWeek,WeeklyChargeHist,Ncyc,grezzata2)
      
      USE MODrainflow
      
      implicit real(8) (a-h,o-z), integer(i-n)
      
      
      INTEGER, INTENT(IN   )          :: NhourWeek
      INTEGER, INTENT(  OUT)          :: Ncyc
      
      real(8), intent(IN   ), dimension(168)       :: WeeklyChargeHist !anche qui 168 forse meglio da module
      real(8), dimension(10008)                    :: grezzata1
      real(8), dimension(10008), intent(OUT)       :: grezzata2
      
      INTEGER                         :: Sttus
      
      
      ! Allocate Cyc arrays
      
      ALLOCATE ( CycMult(AnalRecs1), STAT=Sttus )
      
      IF ( Sttus /= 0 )  THEN
        write(*,*) 'Errore allocazione 3'
        stop
      ENDIF
      
      ALLOCATE ( CycRange(AnalRecs1), STAT=Sttus )
      
      IF ( Sttus /= 0 )  THEN
        write(*,*) 'Errore allocazione 4'
        stop
      ENDIF
      
      ALLOCATE ( CycMean(AnalRecs1), STAT=Sttus )
      
      IF ( Sttus /= 0 )  THEN
        write(*,*) 'Errore allocazione 5'
        stop
      ENDIF
      
      
      ! Allocate RF_Counts
      
      ALLOCATE ( RF_Counts(NumRFCols,NumRFRBins,NumRFMBins) , STAT=Sttus )  ! PROSSIMO STEP: DEFINIRE QUESTI NUMERIIIIIIII
      
      IF ( Sttus /= 0 )  THEN
           write(*,*) 'Errore allocazione 5'
           stop
      ENDIF
      
      
      ! Subroutine 1
      call newSrain(1,                & ! (I) Numero del file,          per ora = 1 da eliminare!
                    1,                & ! (I) Numero totale di colonne, per ora = 1
                    NhourWeek,        & ! (I) Numero di elementi di cui è composta la colonna, 10008 nel test
                    1,                & ! (I) Colonna analizzata, test con una colonna = 1, altrimenti probabilmente andrà fatto variare
                    0.5d0,            & ! (I) Proviamo con 0.5
                    WeeklyChargeHist, & ! (I) Dati da analizzare
                    Ncyc)               ! (O) Cicli contati
      
      
      ! Subroutine 2
      call newBinCount1(1,         & ! (I)
                        Ncyc,      & ! (I)
                        grezzata1)
      
      grezzata2 = grezzata1
      
      
      DEALLOCATE ( CycMult )
      DEALLOCATE ( CycRange )
      DEALLOCATE ( CycMean )
      
      DEALLOCATE (RF_Counts)
      
      
      return
end subroutine newrainflow


! Subroutine 1
subroutine newSrain(Fi,         & ! (I) Numero del file,          per ora = 1 da eliminare!
                    TotCols,    & ! (I) Numero totale di colonne, per ora = 1
                    AnalRecs,   & ! (I) Numero di elementi di cui è composta la colonna, 10008 nel test
                    RFC,        & ! (I) Colonna analizzata, test con una colonna = 1, altrimenti probabilmente andrà fatto variare
                    HCyc,       & ! (I) Proviamo con 0.5
                    ConvData,   & ! (I) Dati da analizzare
                    Ncyc)         ! (O) Cicli contati

      USE MODrainflow

      implicit real(8) (a-h,o-z), integer(i-n)

      ! Argument declarations.

      INTEGER, INTENT(IN   )             :: Fi
      INTEGER, INTENT(IN   )             :: RFC
      REAL(8), INTENT(IN   )             :: HCyc
      INTEGER, INTENT(  OUT)             :: Ncyc


      ! Local declarations.

      REAL(8), ALLOCATABLE            :: Harm      (:)
      REAL(8), ALLOCATABLE            :: Scratch   (:)
      REAL(8)                         :: Slope
      REAL(8)                         :: X
      REAL(8)                         :: Y

      INTEGER                         :: IR
      INTEGER                         :: Istart
      INTEGER                         :: J
      INTEGER                         :: NPts
      INTEGER                         :: Sttus

      ! Aggiunti da me (PIETRO)

      REAL(8), intent(IN   )          :: ConvData(1,168,1) ! Questo 168 dovrà essere un input (sarebbe NhourWeek)!

      INTEGER, INTENT(IN   )          :: AnalRecs
      INTEGER, INTENT(IN   )          :: TotCols


      ! >>> WIP: use this to derive ConvData from test input <<<
      !! location ConvData
      !call readtest(ConvData)
      ! >>> WIP: use this to derive ConvData from test input <<<


      ! Allocate the Scratch array.

      ALLOCATE ( Scratch(AnalRecs), STAT=Sttus )

      IF ( Sttus /= 0 )  THEN
        write(*,*) 'Errore allocazione 1'
        stop
      ENDIF


      ! Copy data for this column into the Scratch array.
      ! Use data from one file when not an aggregate analysis.

      DO IR=1,AnalRecs
        Scratch(IR) = ConvData(RFC,IR,Fi)
      ENDDO ! IR

         ! Allocate Harm array.

      ALLOCATE ( Harm(AnalRecs), STAT = Sttus )

      IF ( Sttus /= 0 )  THEN
        write(*,*) 'Errore allocazione 2'
        stop
      ENDIF


         ! Initialize the routine's variables.

      DO IR=1,AnalRecs
         Harm(IR) = 0.0
      ENDDO ! IR

      Harm(1) = Scratch(1)
      Harm(2) = Scratch(2)
      IR      = 2
      IStart  = 1
      J       = 0
      NCyc    = 0
      NPts    = 2

      IF ( Harm(1) <= Harm(2) )  THEN
         Slope = 1
      ELSE
         Slope = -1
      ENDIF


         ! Step 1.

      10 CONTINUE
      
      IR = IR + 1
      
      IF ( IR > AnalRecs )  GOTO 60
      
      NPts       = NPts + 1
      Slope      = -Slope
      Harm(NPts) = Scratch(IR)


         ! Step 2.

      20 CONTINUE

      IF ( NPts < IStart+1 )  GOTO 10
      
      X = Slope*( Harm(NPts) - Harm(NPts-1) )
      
      IF ( X    <= Smallest )  GOTO 200
      IF ( NPts <  IStart+2 )  GOTO 10
      
      Y = Slope*( Harm(NPts-2) - Harm(NPts-1) )


         ! Step 3.

      30 CONTINUE
      
      IF (X < Y )  GOTO 10
      
      IF ( ( X == Y ) .AND. ( IStart == NPts-2 ) )  GOTO 10
      IF ( ( X >  Y ) .AND. ( IStart == NPts-2 ) )  GOTO 40
      IF ( ( X >= Y ) .AND. ( IStart /= NPts-2 ) )  GOTO 50


         ! Step 4.

      40 CONTINUE

      IStart = IStart + 1
      
      GOTO 10


         ! Step 5.

      50 CONTINUE

      NCyc           = NCyc + 1
      CycMult (NCyc) = 1.0
      CycRange(NCyc) = Y
      CycMean (NCyc) = 0.5*( Harm(NPts-1) + Harm(NPts-2) )
      NPts           = NPts - 2
      Harm(NPts)     = Harm(NPts+2)
      GOTO 20


         ! Step 6.

      60 CONTINUE

      IF (HCyc < 0.999999) THEN
         GOTO 400
      ENDIF

         ! Counts half cycles as full cycles.

      J = J + 1

      IF ( J > IStart )  GOTO 900

      NPts       = NPts + 1
      Slope      = -Slope
      Harm(NPts) = Harm(J)


         ! Step 7.

      70 CONTINUE

      IF ( NPts <  IStart+1 )  GOTO 60

      X = Slope*( Harm(NPts) - Harm(NPts-1) )
      
      IF ( X    <= Smallest )  GOTO 300
      IF ( NPts <  IStart+2 )  GOTO 60

      Y = Slope*( Harm(NPts-2) - Harm(NPts-1) )


         ! Step 8

      80 CONTINUE
      
      IF ( X < Y )  GOTO 60


         ! Step 9

      90 CONTINUE
      
      NCyc           = NCyc + 1
      CycMult (NCyc) = 1.0
      CycRange(NCyc) = Y
      CycMean (NCyc) = 0.5*( Harm(NPts-1) + Harm(NPts-2) )
      NPts           = NPts - 2
      Harm(NPts)     = Harm(NPts+2)

      GOTO 70


      200 CONTINUE

      NPts       = NPts - 1
      Harm(NPts) = Harm(NPts+1)
      Slope      = -Slope
      
      GOTO 20


      300 CONTINUE

      NPts       = NPts - 1
      Harm(NPts) = Harm(NPts+1)
      Slope      = -Slope

      GOTO 70


         ! Counts half cycles as a user-specified value.

      400 CONTINUE
      
      DO J=2,NPts
         NCyc           = NCyc + 1
         CycMult (NCyc) = HCyc
         CycRange(NCyc) = ABS( Harm(J-1) - Harm(J) )
         CycMean (NCyc) = 0.5*( Harm(J-1) + Harm(J) )
      ENDDO ! J


         ! We're done.  Deallocate the temporary arrays.

      900   CONTINUE

      DEALLOCATE ( Harm )
      DEALLOCATE ( Scratch )


      return

end subroutine newSrain


! Subroutine 2
subroutine newBinCount1(IC,Ncyc,grezzata)

   !    This subroutine does the 1-D binning of the cycle-counted data.
   !    It bins the cycles over their min-to-max range for each column.

      USE MODrainflow


      ! Argument declarations.

      INTEGER, INTENT(IN)             :: IC
      INTEGER, INTENT(IN)             :: NCyc


      ! Local declarations.

      INTEGER                         :: IBR
      INTEGER                         :: IR

      REAL(8), dimension(1)                       :: MaxRng   ! PIETRO
      REAL(8), dimension(10008), INTENT(OUT)      :: grezzata ! PIETRO

      MaxRng(1) = 0.0d0 ! PIETRO
      grezzata = 0.0d0  ! PIETRO

      ! Zero out the RF_Counts array for this column.

      DO IBR=1,NumRFRBins
         RF_Counts(IC,IBR,1) = 0
      ENDDO ! IBR


      ! >>> WIP: Tolto e sostituito con pezzo sotto <<<
         ! Find the maximum range if automatic.

      !IF ( AutoRange(IC) )  THEN
      !
      !   MaxRng(IC) = Smallest
      !
      !   DO IR=1,NCyc
      !      IF ( MaxRng(IC) < CycRange(IR) )  MaxRng(IC) = CycRange(IR)
      !   ENDDO ! IR
      !
      !ENDIF
      ! >>> WIP: Tolto e sostituito con pezzo sotto <<<


      ! >>> WIP: Modifica fatta da me per eliminare autorange ed avere sempre come range 0-1 <<<
      MaxRng(IC) = 1.d0
      ! >>> WIP: Fine modifica <<<


         ! Loop through the data.  Determine the appropriate bin.
         ! Increment counts for that bin.

      DO IR=1,NCyc

         IBR = MIN0( INT( NumRFRBins*CycRange(IR)/MaxRng(IC) ) + 1 , NumRFRBins )

         RF_Counts(IC,IBR,1) = RF_Counts(IC,IBR,1) + CycMult(IR)

      ENDDO ! IR


      ! Scrivi su grezzata
      DO ii =1,NumRFRBins
        grezzata(ii) = RF_Counts(1,ii,1)
      ENDDO


         return

end subroutine newBinCount1
