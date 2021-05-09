
! NPV subroutine
! Lubello: feb 2020

subroutine NPVcalc(curveNPV0,    & ! (I) VAN buying from grid
                   curveNPV,     & ! (I) VAN considered configuration
                   TotActInvest, & ! (I) Total actual investment
                   deltaNPV,     & ! (O) VAN difference between two cases
                   ActPBP,       & ! (O) Actualized payback period
                   ProfInd)        ! (O) Profit index


      USE MODparam, ONLY: NdayYear,Nyears,Nstep,MaxGiorni

      implicit real(8) (a-h,o-z), integer(i-n)

      ! Argument declarations

      real(8), dimension(0:MaxGiorni), intent(IN   ):: curveNPV0, curveNPV
      real(8),                         intent(IN   ):: TotActInvest
      real(8), dimension(0:MaxGiorni), intent(  OUT):: deltaNPV
      real(8),                         intent(  OUT):: ActPBP,ProfInd


      ! NPV as a difference between case 1 and case 0

      deltaNPV(0:Nstep) = curveNPV(0:Nstep) - curveNPV0(0:Nstep)

      ! Actualized payback period
      ii = 0
      do while ((deltaNPV(ii) .le. 0.d0) .and. (ii .le. Nstep))
        ii=ii+1
      enddo

      if(ii .ge. Nstep) then
        ActPBP = 9999.9999d0
      else
        ActPBP = dble(ii)/NdayYear
      endif

      ! Profit Index
      ProfInd = deltaNPV(Nstep)/TotActInvest

      return

end subroutine NPVcalc