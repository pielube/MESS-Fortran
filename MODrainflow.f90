
Module MODrainflow

   LOGICAL, PARAMETER              :: AutoRange    (1) = .true. ! >>> WIP: no more used <<<

   REAL(8), ALLOCATABLE            :: CycMult      (:)
   REAL(8), ALLOCATABLE            :: CycRange     (:)
   REAL(8), ALLOCATABLE            :: CycMean      (:)

   REAL(8), ALLOCATABLE            :: RF_Counts   (:,:,:)

   REAL(8), PARAMETER              :: Biggest  = HUGE( 1.0 )
   REAL(8), PARAMETER              :: Smallest = TINY( 1.0 )

   INTEGER, parameter              :: AnalRecs1 = 168
   INTEGER, parameter              :: NumRFCols  = 1
   INTEGER, parameter              :: NumRFRBins = 20
   INTEGER, parameter              :: NumRFMBins = 1

end Module MODrainflow