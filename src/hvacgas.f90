
! Heat pump, air conditioning or both - gas engine
! Masi, Lubello: February 2021

subroutine HVACgas(IndexHVACgas,      & ! (I) 0: both, first heating, 1: both, first cooling 2: heating, 3: cooling ! <<< WIP: 0,1 as a function on local regulations on HVAC
                   IndexRecovery,     & ! (I) 0: OFF 1: ON **
                   PERh_nom,          & ! (I) Heating PER nominal             [-]         
                   CapacityhP_nom,    & ! (I) Heating rated power nominal     [kW] 
                   CapacityhP_max,    & ! (I) Max HP Power                    [kW]
                   CapacityhP_min,    & ! (I) Min HP Power                    [kW] 
                   PERc_nom,          & ! (I) Cooling PER nominal             [-] 
                   CapacitycP_nom,    & ! (I) Cooling rated power nominal     [kW] 
                   CapacitycP_max,    & ! (I) Max Cooling HP Power            [kW]
                   CapacitycP_min,    & ! (I) Min Cooling HP Power            [kW]
                   Rec_nom,           & ! (I) Recovery heat nominal           [kW]
                   T_amb,             & ! (I) Outdoor temperature             [°C]
                   CostHVACgas,       & ! (I) Cost                            [€/kW]
                   ThBalance,         & ! (IO) Thermal energy balance         [kWh]
                   CoolBalance,       & ! (IO) Cooling effect balance         [kWh]	                 
	             ThProduction,      & ! (O) Thermal energy production       [kWh]
		       CoolProduction,    & ! (O) Cooling effect                  [kWh]
			 Gas_Cons_sm3,      & ! (O) Natural gas consumption real    [Sm3]
			 Ele_Cons_real,     & ! (O) Electricity consumption real    [kWh] 
			 Rec_Heat,          & ! (O) Recovery heat                   [kWh]  
			 CapexHVACgas)        ! (O) Capex                           [€]

      ! Recovery:
      ! HEATING: just modify PER_nom and min and max capacities accordingly to account for a fixed share of recovery heat
      ! COOLING: add nominal recovery heat and it will be computed considering dependence on T and PLR

      USE MODParam,       ONLY: timestep,LHVch4,rhoStdch4

      implicit real(8) (a-h,o-z), integer(i-n)

      integer, intent(IN   ) :: IndexHVACgas,IndexRecovery

      real(8), intent(IN   ) :: PERh_nom,CapacityhP_nom,CapacityhP_max,CapacityhP_min, &
                                PERc_nom,CapacitycP_nom,CapacitycP_max,CapacitycP_min, &
                                Rec_nom, &
                                T_amb, &
                                CostHVACgas 
      real(8), intent(  OUT) :: Gas_Cons_sm3,Ele_Cons_real,Rec_Heat,CapexHVACgas  
      real(8), intent(INOUT) :: ThBalance,CoolBalance

      real(8) :: Total_Cons_real,PLR,PER_real,Gas_Cons_real
	  

      ! From power to energy and changing since to balances (easier to work)
	QQdemposTh   = -ThBalance
	QQminTh      = CapacityhP_min*timestep
	QQmaxTh      = CapacityhP_max*timestep
	QQnomTh      = CapacityhP_nom*timestep
	QQdemposCool = -CoolBalance
	QQminCool    = CapacitycP_min*timestep
	QQmaxCool    = CapacitycP_max*timestep
	QQnomCool    = CapacitycP_nom*timestep

      ! Output variables inizialization
      ThProduction   = 0.d0 
      CoolProduction = 0.d0
      Gas_Cons_real  = 0.d0
      Ele_Cons_real  = 0.d0
      Rec_Heat       = 0.d0


      ! Selecting working mode for the HVACgas component
      select case(IndexHVACgas) 
      case(0) ! Component working as HP and AC, priority to HP

        if(QQdemposTh.gt.0.d0)then 

          HVACwm = 1.d0

          if(QQdemposTh .eq. 0.d0)then
            !ThProduction = 0.d0 not necessary, left for clarity
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
            write(*,*) 'Warning: thermal demand positive in HVACgas!'
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
            write(*,*) 'Warning: cooling demand positive in HVACgas!'
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
            write(*,*) 'Warning: cooling demand positive in HVACgas!'
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
            write(*,*) 'Warning: thermal demand positive in HVACgas!'
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
          write(*,*) 'Warning: thermal demand positive in HVACgas!'
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
          write(*,*) 'Warning: cooling demand positive in HVACgas!'
        endif

      case default  
        write(*,*) 'Input error: not existing HVACgas Mode'
        stop 'Input error!'
      endselect  


      ! Computing gas and electricity consumption and updating energy balances based on timestep-specific working mode
      if(HVACwm.eq. 0.d0)then
        ! HVACgas not working in this timestep: everything is 0, nothing else to compute 
      elseif(HVACwm.eq. 1.d0)then
        ! Heating in this timestep  
        factPER1   = -17514.d-4*(PLR**3) + 21577.d-4*(PLR**2) - 109.d-4*PLR + 5997.d-4
        factPER2   = -2.53d-5*(T_amb**3) - 1.72d-4*(T_amb**2) + 2.64d-2*T_amb + 8.01d-1
        PER_real   = PERh_nom*factPER1*factPER2
        factCons1  = 2918.d-4*(PLR**2) - 17971.d-4*PLR + 25276.d-4
	  factCons2  = -6703.d-10*(T_amb**3) -42379.d-10*(T_amb**2) + 68658.d-8*T_amb + 92688.d-7
	  !factRec1   = 9585.d-4*PLR + 602.d-4
	  !factRec2   = 28696.d-8*(T_amb**2) + 10408.d-21*T_amb + 18377.d-5

        Total_Cons_real = ThProduction/PER_real
        Gas_Cons_real   = Total_Cons_real/(1+factCons1*factCons2) ![kWh]
        Gas_Cons_sm3    = Gas_Cons_real*3600.d0/LHVch4/rhoStdch4  ![Sm3]
        Ele_Cons_real   = Total_Cons_real - Gas_Cons_real
        ThBalance      = -(QQdemposTh - ThProduction)

      elseif(HVACwm.eq. 2.d0)then
        ! Cooling in this timestep  
        factPER1  = 81979.d-4*(PLR**3) - 16623.d-3*(PLR**2) + 10114.d-3*PLR - 6755.d-4
	  factPER2  = -66225.d-9*(T_amb**2) - 74158.d-7*T_amb + 13488.d-4
	  PER_real   = PERc_nom*factPER1*factPER2		
        factCons1  = -46795.d-3*(PLR**4) +13069.d-2*(PLR**3) - 1276.d-1*(PLR**2) +47989.d-3*PLR -32933.d-4
        factCons2  = -93666.d-10*(T_amb**2) + 73025.d-8*T_amb +14744.d-6
        factRec1   = 24727.d-4*(PLR**3) -52957.d-4*(PLR**2) +41074.d-4*PLR -2844.d-4
        factRec2   = 73112.d-7*T_amb + 75483.d-5

        Total_Cons_real = CoolProduction/PER_real
        Gas_Cons_real   = Total_Cons_real/(1+factCons1*factCons2) ![kWh]
        Gas_Cons_sm3    = Gas_Cons_real*3600.d0/LHVch4/rhoStdch4  ![Sm3]
        Ele_Cons_real   = Total_Cons_real - Gas_Cons_real
        CoolBalance      = -(QQdemposCool - CoolProduction)
		  
      else
        write(*,*) 'Internal error: not existing HVACgas working mode'
        stop 'Internal error!'
      endif	  


      ! Handling recovery if present and if Tamb.gt.2°C
      if(IndexRecovery.eq.1 .and. HVACwm.eq.2.d0 .and. T_amb.ge.2.d0)then
        Rec_Heat      = Rec_nom*factRec1*factRec2
        if(QQdemposTh.le.0.d0)then
          Rec_Heat = 0.d0
        else
          QQdemposTh = QQdemposTh - Rec_Heat
          if(QQdemposTh.le. 0.d0)then
            Rec_Heat = Rec_Heat+QQdemposTh
            QQdemposTh = 0.d0
          else
          endif
        endif
      else
        Rec_Heat = 0.d0
      endif
            

      ! Capex expenditure
      if(IndexHVACgas.eq.0 .or. IndexHVACgas.eq.1 .or. IndexHVACgas.eq.2)then
        CapexHVACgas = CostHVACgas*CapacityhP_nom
      elseif(IndexHVACgas.eq.3)then
        CapexHVACgas = CostHVACgas*CapacitycP_nom
      else
        write(*,*) 'Input error: wrong IndexHVACgas value!'
        stop 'Input error!'
      endif
	  
      return

end subroutine HVACgas