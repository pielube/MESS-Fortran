
! Battery module
! Lubello, Zini: feb 2021

module MODbatterytest

      implicit real(8) (a-h,o-z), integer(i-n)

      type Battery
         real(8) :: efficiency,SoC
      end type Battery 

contains

      subroutine aggiornamentoSoC(this)

      type(Battery), intent(INOUT) :: this
      this%SoC = this%SoC + this%efficiency*0.1d0


      end subroutine aggiornamentoSoC

endmodule MODbatterytest