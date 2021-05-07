
! Electric Chiller
! Masi, Lubello, Zini: February 2021

subroutine El_Chiller(CapacityP_frigo_nom,   & ! (I) Frigo Capacity nominal              [kW]  ! Power
			    CapacityP_frigo_min,   & ! (I) Min Power                           [kW]  ! Power
			    CapacityP_frigo_max,   & ! (I) Max Power                           [kW]  ! Power
                      COP_nom,               & ! (I) COP nominal                         [-]
	                Cost_El_Chiller,       & ! (I) Cost                                [€/kW]
		          Power_fan,             & ! (I) Nominal Power fan                   [kW]
		          T_amb,                 & ! (I) Outdoor temperature                 [°C]
                      RH,                    & ! (I) Relative humidity                    [%]
			    Frigo_Dem,             & ! (I/O) Frigo Demand in                   [kWh] ! energy balance
	                Frigo_Production,      & ! (O) Frigo production                    [kWh]        
			    El_Cons_real,          & ! (O) Electric consumption                [kWh]     
			    El_Cons_fan,           & ! (O) Electric consumption fan            [kWh]
			    COP_real,              & ! (O) COP real                            [-]
			    Capex_El_Chiller)        ! (O) Capex                               [€]

      USE MODparam,  ONLY: timestep
      USE MODcoord,  ONLY: itime

      implicit real(8) (a-h,o-z), integer(i-n)

      real(8), intent(IN   ) :: CapacityP_frigo_nom,CapacityP_frigo_min,CapacityP_frigo_max,COP_nom, &
                                Power_fan,                                                           &
                                T_amb,RH,                                                            &
                                Cost_El_Chiller                                                      
      real(8), intent(  OUT) :: Frigo_Production,El_Cons_real,COP_real,Capex_El_Chiller,El_Cons_fan
      real(8), intent(INOUT) :: Frigo_Dem	  
      
      QQ_frigo_dem_pos    = -Frigo_Dem                      ! ho già un'energia in ingresso, devo solo cambiare segno
      QQ_frigo_min        = CapacityP_frigo_min*timestep
      QQ_frigo_max        = CapacityP_frigo_max*timestep
      QQ_frigo_nom        = CapacityP_frigo_nom*timestep

      ! Output variables inizialization
      Frigo_Production = 0.d0
      El_Cons_real     = 0.d0
      El_Cons_fan      = 0.d0
      COP_real         = 0.d0
   

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


      ! Electric chiller
      if(QQ_frigo_dem_pos .eq. 0.d0 )then

        Frigo_Production = 0.d0
        PLR = 0.d0
        COP_real = 0.d0
        El_Cons_real = 0.d0

      elseif(QQ_frigo_dem_pos.gt.0.d0 .and. QQ_frigo_dem_pos.lt.QQ_frigo_min)then

        Frigo_Production = QQ_frigo_dem_pos
        PLR              = QQ_frigo_min/QQ_frigo_nom
        COP_real         = COP_nom*(31591.d-4*(PLR**3) - 69797.d-4*(PLR**2) + 46616.d-4*PLR + 1764.d-4)*(1.68d0*EXP(-1.71d-2*T_w_in))	  
        El_Cons_real     = QQ_frigo_min/COP_real 
       
      elseif(QQ_frigo_dem_pos .ge. QQ_frigo_min.and.QQ_frigo_dem_pos .le. QQ_frigo_max)then

        Frigo_Production = QQ_frigo_dem_pos
        PLR = Frigo_Production/QQ_frigo_nom
        COP_real        = COP_nom*(31591.d-4*(PLR**3) - 69797.d-4*(PLR**2) + 46616.d-4*PLR + 1764.d-4)*(1.68d0*EXP(-1.71d-2*T_w_in))
	  El_Cons_real    = Frigo_Production/COP_real

      elseif(QQ_frigo_dem_pos .gt. QQ_frigo_max)then

        Frigo_Production = QQ_frigo_max
        PLR = Frigo_Production/QQ_frigo_nom
        COP_real        = COP_nom*(31591.d-4*(PLR**3) - 69797.d-4*(PLR**2) + 46616.d-4*PLR + 1764.d-4)*(1.68d0*EXP(-1.71d-2*T_w_in))	  
	  El_Cons_real    = Frigo_Production/COP_real

      else 
        write(*,*) 'Warning : electric chiller working with a positive demand!'
        write(*,*), itime
        write(*,*), QQ_frigo_dem_pos
      endif 

      ! Updating  cooling balance     
      Frigo_Dem       = -(QQ_frigo_dem_pos - Frigo_Production)    

      ! UpdatingcCooling tower fan electricity consumption
      El_Cons_fan = El_Cons_fan*PLR                                     

      ! Capex expenditure
      Capex_El_Chiller = Cost_El_Chiller*CapacityP_frigo_nom

	  
      return

end subroutine El_Chiller