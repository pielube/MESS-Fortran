
! Heat pump, air conditioning or both - electrical motor
! Masi, Lubello: Feb 2021

subroutine HVACel(IndexHVACel,       & ! (I) 0: both first heating, 1: both, first cooling 2: heating, 3: cooling ! <<< WIP: 0,1 as a function on local regulations on HVAC
                  COPh_nom,          & ! (I) Heating COP nominal             [-]         
                  COPc_nom,          & ! (I) Cooling COP nominal             [-]
                  CapacityhP_nom,    & ! (I) Heating rated power nominal     [kW] 
                  CapacityhP_max,    & ! (I) Max Heating HP Power            [kW]
                  CapacityhP_min,    & ! (I) Min Heating HP Power            [kW]  
                  CapacitycP_nom,    & ! (I) Cooling rated power nominal     [kW] 
                  CapacitycP_max,    & ! (I) Max Cooling HP Power            [kW]
                  CapacitycP_min,    & ! (I) Min Cooling HP Power            [kW]  
                  T_amb,             & ! (I) Outdoor temperature             [°C]
	            CostHVACel,        & ! (I) Cost                            [€/kW]
                  ThBalance,         & ! (IO) Thermal energy balance [kWh]
                  CoolBalance,       & ! (IO) Cooling effect balance [kWh]
	            ThProduction,      & ! (O) Thermal energy production       [kWh]
		      CoolProduction,    & ! (O) Cooling effect                  [kWh]
                  Ele_Cons_real,     & ! (O) Electricity consumption real    [kWh]  
                  CapexHVACel)         ! (O) Capex                              [€]

      USE MODparam,  ONLY: timestep

      implicit real(8) (a-h,o-z), integer(i-n)

      integer, intent(IN   ) :: IndexHVACel
      real(8), intent(IN   ) :: COPh_nom,COPc_nom,CapacityhP_nom,CapacityhP_max,CapacityhP_min,CapacitycP_nom,CapacitycP_min,CapacitycP_max,T_amb,CostHVACel
      real(8), intent(  OUT) :: ThProduction,CoolProduction,Ele_Cons_real,CapexHVACel
      real(8), intent(INOUT) :: ThBalance,CoolBalance


      ! From power to energy and changing since to balances (easier to work)
	QQdemposTh = -ThBalance
	QQminTh     = CapacityhP_min*timestep
	QQmaxTh     = CapacityhP_max*timestep
	QQnomTh     = CapacityhP_nom*timestep
	QQdemposCool = -CoolBalance
	QQminCool     = CapacitycP_min*timestep
	QQmaxCool     = CapacitycP_max*timestep
	QQnomCool     = CapacitycP_nom*timestep

      ! Energy production and consumption inizialization
      ThProduction   = 0.d0 
      CoolProduction = 0.d0
      Ele_Cons_real  = 0.d0


      ! Selecting working mode for the HVACel component
      select case(IndexHVACel) 
      case(0) ! Component working as HP and AC, priority to HP

        if(QQdemposTh.gt.0.d0)then 

          HVACwm = 1.d0

          if(QQdemposTh .eq. 0.d0)then
            ThProduction = 0.d0
            PLR          = 0.d0
          elseif(QQdemposTh.gt.0.d0 .and. QQdemposTh.lt.QQminTh)then
            ThProduction = QQdemposTh
            PLR = QQminTh/QQnomTh
          elseif(QQdemposTh.ge.QQminTh .and. QQdemposTh.le.QQmaxTh)then
            ThProduction = QQdemposTh
            PLR = ThProduction/QQnomTh
          elseif(QQdemposTh .gt. QQmaxTh)then
            ThProduction = QQmaxTh
            PLR = ThProduction/QQnomTh
          else 
            write(*,*) 'Warning: thermal demand positive in HVACel!'
          endif

        elseif(QQdemposCool.gt.0.d0) then

          HVACwm = 2.d0

          if(QQdemposCool .eq. 0.d0)then
            CoolProduction = 0.d0
            PLR           = 0.d0
          elseif(QQdemposCool.gt.0.d0 .and. QQdemposCool.lt.QQminCool)then
            CoolProduction = QQdemposCool
            PLR = QQminCool/QQnomCool
          elseif(QQdemposCool.ge.QQminCool .and. QQdemposCool.le.QQmaxCool)then
            CoolProduction = QQdemposCool
            PLR = CoolProduction/QQnomCool
          elseif(QQdemposCool .gt. QQmaxCool)then
            CoolProduction = QQmaxCool
            PLR = CoolProduction/QQnomCool
          else 
            write(*,*) 'Warning: cooling demand positive in HVACel!'
          endif

        else
          HVACwm = 0.d0

        endif

      case(1) ! Component working as HP and AC, priority to AC

        if(QQdemposCool.gt.0.d0) then

          HVACwm = 2.d0

          if(QQdemposCool .eq. 0.d0)then
            CoolProduction = 0.d0
            PLR           = 0.d0
          elseif(QQdemposCool.gt.0.d0 .and. QQdemposCool.lt.QQminCool)then
            CoolProduction = QQdemposCool
            PLR = QQminCool/QQnomCool
          elseif(QQdemposCool.ge.QQminCool .and. QQdemposCool.le.QQmaxCool)then
            CoolProduction = QQdemposCool
            PLR = CoolProduction/QQnomCool
          elseif(QQdemposCool .gt. QQmaxCool)then
            CoolProduction = QQmaxCool
            PLR = CoolProduction/QQnomCool
          else 
            write(*,*) 'Warning: cooling demand positive in HVACel!'
          endif

        elseif(QQdemposTh.gt.0.d0)then

          HVACwm = 1.d0

          if(QQdemposTh .eq. 0.d0)then
            ThProduction = 0.d0
            PLR          = 0.d0
          elseif(QQdemposTh.gt.0.d0 .and. QQdemposTh.lt.QQminTh)then
            ThProduction = QQdemposTh
            PLR = QQminTh/QQnomTh
          elseif(QQdemposTh.ge.QQminTh .and. QQdemposTh.le.QQmaxTh)then
            ThProduction = QQdemposTh
            PLR = ThProduction/QQnomTh
          elseif(QQdemposTh .gt. QQmaxTh)then
            ThProduction = QQmaxTh
            PLR = ThProduction/QQnomTh
          else 
            write(*,*) 'Warning: thermal demand positive in HVACel!'
          endif

        else
          HVACwm = 0.d0
        endif

      case(2) ! Component working just as HP

        HVACwm = 1.d0

        if(QQdemposTh .eq. 0.d0)then
          ThProduction = 0.d0
          PLR          = 0.d0
        elseif(QQdemposTh.gt.0.d0 .and. QQdemposTh.lt.QQminTh)then
          ThProduction = QQdemposTh
          PLR = QQminTh/QQnomTh
        elseif(QQdemposTh.ge.QQminTh .and. QQdemposTh.le.QQmaxTh)then
          ThProduction = QQdemposTh
          PLR = ThProduction/QQnomTh
        elseif(QQdemposTh .gt. QQmaxTh)then
          ThProduction = QQmaxTh
          PLR = ThProduction/QQnomTh
        else 
          write(*,*) 'Warning: thermal demand positive in HVACel!'
        endif

      case(3) ! Component working just as AC

        HVACwm = 2.d0

        if(QQdemposCool .eq. 0.d0)then
          CoolProduction = 0.d0
          PLR           = 0.d0
        elseif(QQdemposCool.gt.0.d0 .and. QQdemposCool.lt.QQminCool)then
          CoolProduction = QQdemposCool
          PLR = QQminCool/QQnomCool
        elseif(QQdemposCool.ge.QQminCool .and. QQdemposCool.le.QQmaxCool)then
          CoolProduction = QQdemposCool
          PLR = CoolProduction/QQnomCool
        elseif(QQdemposCool .gt. QQmaxCool)then
          CoolProduction = QQmaxCool
          PLR = CoolProduction/QQnomCool
        else 
          write(*,*) 'Warning: cooling demand positive in HVACel!'
        endif

      case default  
        write(*,*) 'Input error: not existing HVACel Mode'
        stop 'Input error!'
      endselect  

     
      ! Computing electricity consumption and updating energy balances based on timestep-specific working mode
      if(HVACwm.eq. 0.d0)then
        ! HVACel not working: everything is 0, nothing else to compute 
      elseif(HVACwm.eq. 1.d0)then
        ! Heating in this timestep    
	  factCOPh1  = 19917.d-4*(PLR**3) - 42781.d-4*(PLR**2) + 31049.d-4*PLR + 1921.d-4  
	  factCOPh2  = 143.d-4*T_amb + 8201.d-4
	  COP_real   = COPh_nom*factCOPh1*factCOPh2
        Ele_Cons_real  = PLR*QQnomTh/COP_real	
        ThBalance =  -(QQdemposTh - ThProduction)
      elseif(HVACwm.eq. 2.d0)then
        ! Cooling in this timestep  
        factCOPc1  = 20719.d-4*(PLR**3) - 45208.d-4*(PLR**2) + 33019.d-4*PLR + 16.d-2
        factCOPc2  = 424.d-6*(T_amb**2) - 5467.d-5*T_amb + 2302.d-3
        COP_real   = COPc_nom*factCOPc1*factCOPc2
        Ele_Cons_real  = PLR*QQnomCool/COP_real
        CoolBalance = -(QQdemposCool - CoolProduction)
      else
        write(*,*) 'Internal error: not existing HVACel working mode'
        stop 'Internal error!'
      endif	  
	  
	 

      ! Capex expenditure
      if(IndexHVACel.eq.0 .or. IndexHVACel.eq.1 .or. IndexHVACel.eq.2)then
        CapexHVACel = CostHVACel*CapacityhP_nom  
      elseif(IndexHVACel.eq.3)then
        CapexHVACel = CostHVACel*CapacitycP_nom  
      else
        write(*,*) 'Input error: wrong IndexHVACel value!'
        stop 'Input error!'
      endif

	  
      return

end subroutine HVACel