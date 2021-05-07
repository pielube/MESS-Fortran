
! Trigeneration / CCHP - Combined Cooling, Heat and Power
! Following electric demand
! Masi, Lubello, Zini:  February 2021

subroutine CCHP(IndexCCHP,           & ! (I) 0:CHP, 1: CCHP
                CapacityP_el_nom,    & ! (I) Electric Capacity nominal           [kW] 
                CapacityP_el_min,    & ! (I) Min Power                           [kW] 
		    CapacityP_el_max,    & ! (I) Max Power                           [kW]
                etaElectrical,       & ! (I) Electrical efficiency               [-]
                etaThermal,          & ! (I) Thermal efficiency                  [-]
		    CapacityP_frigo_nom, & ! (I) Frigo Capacity nominal              [kW]
		    CapacityP_frigo_min, & ! (I) Min Frigo Power                     [kW]
		    CapacityP_frigo_max, & ! (I) Max Frigo Power                     [kW]
		    eta_frigo_nom,       & ! (I) Abs chiller nominal efficiency      [-]
		    Power_fan,           & ! (I) Nominal Power fan                   [kW]
	          Cost_Cogeneration,   & ! (I) Cost Cogeneration                   [€/kW]
	          Cost_Abs_Chiller,    & ! (I) Cost Abs Chiller                    [€/kW]
		    T_amb,               & ! (I) Outdoor temperature                 [°C]
		    ElBalance,           & ! (IO) Electric Demand in                [kWh]
		    ThBalance,           & ! (IO) Thermal Demand in                 [kWh]
		    CoolBalance,         & ! (IO) Cooling Demand in                 [kWh]
		    El_Production,       & ! (O) Electric production                 [kWh] 
                Heat_Production,     & ! (O) Overall heat produced               [kWh]
		    Heat_Heating,        & ! (O) Thermal production                  [kWh]
		    HeatConsChiller,     & ! (O) Thermal consumption Abs Chiller     [kWh] 
                Heat_Residual,       & ! (O) Heat discarded                      [kWh]		
                Frigo_Production,    & ! (O) Frigo production                    [kWh]
                Gas_Cons_real,       & ! (O) Gas consumption                     [Sm3]
		    El_Cons_fan,         & ! (O) Electric consumption fan            [kWh]
		    Capex_Cogeneration,  & ! (O) Capex Cogeneration                  [€]
                Capex_Abs_Chiller)     ! (O) Capex Abs Chiller                   [€]   


      USE MODparam,  ONLY: timestep,LHVch4,rhoStdch4

      implicit real(8) (a-h,o-z), integer(i-n)

      integer, intent(IN   ) :: IndexCCHP
      real(8), intent(IN   ) :: CapacityP_el_nom,CapacityP_el_min,CapacityP_el_max,                        &
                                etaElectrical,etaThermal,                                                  &
                                CapacityP_frigo_nom,CapacityP_frigo_min,CapacityP_frigo_max,eta_frigo_nom, &
                                Power_fan,                                                                 &
                                T_amb,                                                                     &
                                Cost_Cogeneration,Cost_Abs_Chiller
      real(8), intent(  OUT) :: El_Production,Heat_Production,Heat_Heating,HeatConsChiller,Heat_Residual,Frigo_Production,Gas_Cons_real,El_Cons_fan, &
                                Capex_Cogeneration,Capex_Abs_Chiller
      real(8), intent(INOUT) :: ElBalance,ThBalance,CoolBalance   


      ! Verifying working mode and computing capex expenditure

      if(IndexCCHP.eq.0)then
        Capex_Cogeneration = Cost_Cogeneration*CapacityP_el_nom
        Capex_Abs_Chiller  = 0.d0
      elseif(IndexCCHP .eq.1) then
        Capex_Cogeneration = Cost_Cogeneration*CapacityP_el_nom
        Capex_Abs_Chiller  = Cost_Abs_Chiller*CapacityP_frigo_nom
      else
        write(*,*) 'Input error: IndexCCHP should be either 0 or 1!'
        stop 'Input error!'
      endif


	QQ_el_dem_pos     = -ElBalance                    
      QQ_heat_dem_pos   = -ThBalance                    
	QQ_frigo_dem_pos  = -CoolBalance
	QQ_el_min         = CapacityP_el_min*timestep
	QQ_el_max         = CapacityP_el_max*timestep
	QQ_el_nom         = CapacityP_el_nom*timestep
	QQ_frigo_min      = CapacityP_frigo_min*timestep
	QQ_frigo_max      = CapacityP_frigo_max*timestep
	QQ_frigo_nom      = CapacityP_frigo_nom*timestep


      ! Cooling tower consumption
      ! Will be updated later according to PLR (= 0 if abs chiller not working)

      if(T_amb.le.12.5d0)then
        T_w_in = 185.d-1
      else
        T_w_in = 8316.d-4*T_amb + 81589.d-4
      endif

      deltaT_app = T_w_in - T_amb                                                 
      if(deltaT_app.ge.1.d0)then
        El_Cons_fan = Power_fan*(29054.d-4*EXP(-937.d-3*deltaT_app))*timestep
      else
        El_Cons_fan = Power_fan*(29054.d-4*EXP(-937.d-3*1.d0))*timestep
      endif


      ! 1) Power part
	  
      if(QQ_el_dem_pos .lt. QQ_el_min)then
	  El_Production    = 0.d0
	  PLR_CHP          = 0.d0
	  PLR_Frigo        = 0.d0
	  Heat_Production  = 0.d0
	  El_Production    = 0.d0
        Frigo_Production = 0.d0
        Gas_Cons_real    = 0.d0
	  HeatConsChiller  = 0.d0
	  eta_tot_real     = 0.d0  
	  eta_el_real      = 0.d0
	  eta_heat_real    = 0.d0
	  eta_frigo_real   = 0.d0
        El_Cons_fan      = 0.d0
        goto 9999
      elseif(QQ_el_dem_pos .ge. QQ_el_min.and.QQ_el_dem_pos .le. QQ_el_max)then
        El_Production = QQ_el_dem_pos
	elseif(QQ_el_dem_pos .gt. QQ_el_max)then
	  El_Production = QQ_el_max
	else
        write(*,*) 'Something went wrong with electricity demand in CCHP: internal error!' ! <<< WIP: how do you handle this if? You should never be in here
        stop 'Internal error!'
	endif 

      PLR_CHP           = El_Production/QQ_el_nom
      eta_el_real       = (-25294.d-5*(PLR_CHP**2) + 62648.d-5*PLR_CHP + 62646.d-5)*(-4.d-5*(T_amb**2) - 4.d-4*T_amb + 1024.d-3)*etaElectrical
	Gas_Cons_real_kWh = El_Production/eta_el_real                  ![kWh]
      Gas_Cons_real     = Gas_Cons_real_kWh*3600.d0/LHVch4/rhoStdch4 ![Sm3]
      ElBalance         = -(QQ_el_dem_pos - El_Production)                      


      ! 2) Heating part

	eta_heat_real     = (58822.d-6*(PLR_CHP**2) - 26549.d-5*PLR_CHP + 12067.d-4)*etaThermal
	eta_tot_real      = eta_el_real + eta_heat_real   

	Heat_Production   = eta_heat_real*Gas_Cons_real_kWh
      Heat_Residual     = Heat_Production

      if(QQ_heat_dem_pos.le.0.d0)then
        ! Heat_Residual = Heat_Residual
        Heat_Heating = 0.d0
      elseif(QQ_heat_dem_pos.gt.0.d0 .and. QQ_heat_dem_pos.lt.Heat_Residual)then
        Heat_Heating  = QQ_heat_dem_pos
        Heat_Residual = Heat_Residual - QQ_heat_dem_pos
      elseif(QQ_heat_dem_pos.ge.Heat_Residual)then
        Heat_Heating = Heat_Residual
        Heat_Residual = 0.d0
      else
        write(*,*) 'Internal error! Unexpected case in CCHP'
        stop 'Internal error!'
      endif

      ThBalance = -(QQ_heat_dem_pos-Heat_Heating)


      ! 3) Cooling part

      if(Heat_Residual.eq.0.d0 .or. IndexCCHP.eq.0)then 
	  Frigo_Production = 0.d0
	  eta_frigo_real   = 0.d0
	  HeatConsChiller  = 0.d0
	  PLR_Frigo        = 0.d0
        El_Cons_fan      = 0.d0
        goto 9999
      elseif(Heat_Residual.lt.0.d0)then
        write(*,*) 'Internal error! Heat residual lower than zero'
        stop 'Internal error!'
      endif
 

      eta_frigo_real   = eta_frigo_nom*(-391.d-7*(T_w_in**3) + 285.d-5*(T_w_in**2) - 755.d-4*T_w_in + 176.d-2)
      FrigoProductionMax = Heat_Residual*eta_frigo_real
      deltaFrigo = QQ_frigo_dem_pos - FrigoProductionMax


      if(deltaFrigo.le.0.d0)then

        if(QQ_frigo_dem_pos .eq. 0.d0)then
        
          Frigo_Production = 0.d0
          PLR_Frigo        = 0.d0
	    eta_frigo_real   = 0.d0
	    HeatConsChiller  = 0.d0
          !Heat_Residual = Heat_Residual
        
        elseif(QQ_frigo_dem_pos.gt.0.d0 .and. QQ_frigo_dem_pos.lt.QQ_frigo_min)then
        
          Frigo_Production = QQ_frigo_dem_pos
          PLR_Frigo        = QQ_frigo_min/QQ_frigo_nom
          eta_frigo_real   = eta_frigo_nom*(-391.d-7*(T_w_in**3) + 285.d-5*(T_w_in**2) - 755.d-4*T_w_in + 176.d-2)
          HeatConsChiller  = QQ_frigo_min/eta_Frigo_real
          Heat_Residual     = Heat_Residual - HeatConsChiller
        
        elseif(QQ_frigo_dem_pos.ge.QQ_frigo_min .and. QQ_frigo_dem_pos.le.QQ_frigo_max)then
        
          Frigo_Production = QQ_frigo_dem_pos
          PLR_Frigo        = Frigo_Production/QQ_frigo_nom
          eta_frigo_real   = eta_frigo_nom*(-391.d-7*(T_w_in**3) + 285.d-5*(T_w_in**2) - 755.d-4*T_w_in + 176.d-2)
          HeatConsChiller  = Frigo_Production/eta_Frigo_real
          Heat_Residual     = Heat_Residual - HeatConsChiller
        
	  elseif(QQ_frigo_dem_pos .gt. QQ_frigo_max)then
        
	    Frigo_Production = QQ_frigo_max
          PLR_Frigo        = Frigo_Production/QQ_frigo_nom
          eta_frigo_real   = eta_frigo_nom*(-391.d-7*(T_w_in**3) + 285.d-5*(T_w_in**2) - 755.d-4*T_w_in + 176.d-2)
          HeatConsChiller  = Frigo_Production/eta_Frigo_real
          Heat_Residual     = Heat_Residual - HeatConsChiller
   
	  else 
	    write(*,*) 'Warning : CCHP working with a positive cooling demand!'
	  endif      

      else

        if(FrigoProductionMax.lt.QQ_frigo_min)then
        
          Frigo_Production = 0.d0
          PLR_Frigo        = 0.d0
	    eta_frigo_real   = 0.d0
	    HeatConsChiller  = 0.d0
          !Heat_Residual = Heat_Residual
        
        elseif(FrigoProductionMax.ge.QQ_frigo_min .and. FrigoProductionMax.le.QQ_frigo_max)then
        
          Frigo_Production = FrigoProductionMax
          PLR_Frigo        = Frigo_Production/QQ_frigo_nom
          eta_frigo_real   = eta_frigo_nom*(-391.d-7*(T_w_in**3) + 285.d-5*(T_w_in**2) - 755.d-4*T_w_in + 176.d-2)
          HeatConsChiller  = Frigo_Production/eta_Frigo_real
          Heat_Residual     = Heat_Residual - HeatConsChiller
        
	  elseif(FrigoProductionMax.gt.QQ_frigo_max)then
        
	    Frigo_Production = QQ_frigo_max
          PLR_Frigo        = Frigo_Production/QQ_frigo_nom
          eta_frigo_real   = eta_frigo_nom*(-391.d-7*(T_w_in**3) + 285.d-5*(T_w_in**2) - 755.d-4*T_w_in + 176.d-2)
          HeatConsChiller  = Frigo_Production/eta_Frigo_real
          Heat_Residual     = Heat_Residual - HeatConsChiller
   
	  else 
	    write(*,*) 'Warning : CCHP working with a positive cooling demand!'
	  endif      

      endif

      CoolBalance = -(QQ_frigo_dem_pos - Frigo_Production)        


      ! UpdatingcCooling tower fan electricity consumption
      El_Cons_fan = El_Cons_fan*PLR_Frigo


9999  continue
	  
      return

end subroutine CCHP
