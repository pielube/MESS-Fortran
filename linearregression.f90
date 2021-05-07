
! Subroutine to compute coefficients for linear regression with least squares method
! Taken from Chapter 15 of:
! Perrin, Charles L. Numerical Recipes in Fortran 90: The Art of Scientific Computing, 
! By William H. Press, Saul A. Teukolsky, William T. Vetterling, and Brian P. Flannery. 
! Cambridge University Press: New York, 1996. 1997.


subroutine LinRegLeastSquares(vector_x, & ! (I) Array of x data points    
                              vector_y, & ! (I) Array of y data points   
                              A_coeff,  & ! (O) grade 0 coeff  [-] 
                              B_coeff)    ! (O) grade 1 coeff  [-] 
                        
                        
      implicit real(8) (a-h,o-z), integer(i-n)
      
      integer, parameter :: Ndatapoints = 40 
      
      real(8), dimension(Ndatapoints), intent(IN   ) :: vector_x,vector_y
      real(8), intent(  OUT) :: A_coeff,B_coeff
      
      real(8), dimension(Ndatapoints) ::  SIG = 0.d0 ! Standard deviation of each point
                        
      integer(8), parameter :: KWT = 0 ! [-]                          
                                        
      real(8):: CHI2,Q,SIGA,SIGB,SIGDAT, SS,ST2,Svector_x,SXOSS,Svector_y,T,WT
      
 
      ! Linear least square
                              
      Svector_x=0.d0     !initialize sums to zero
      Svector_y=0.d0
      ST2=0.d0
      B_coeff=0.d0
      
      if (KWT .NE. 0.d0) then   ! accumulate sums
        SS=0.d0
        do  i=1,Ndatapoints
          WT=1.d0/(SIG(i)**2.d0)  
          SS=SS+WT
          Svector_x=Svector_x+vector_x(i)*WT       ! with weights
          Svector_y=Svector_y+vector_y(i)*WT
        end do 
      else 
        do  i=1,Ndatapoints 
          Svector_x=Svector_x+vector_x(i)          ! without weights
          Svector_y=Svector_y+vector_y(i)
        end do 
        SS=FLOAT(Ndatapoints)  
      end if   
      SXOSS= Svector_x/SS 
      if (KWT .NE. 0.d0) then
        do  i=1,Ndatapoints
          T=(Svector_x-SXOSS)/SIG(i)
          ST2=ST2+T*T
          B_coeff=B_coeff+vector_y(i)/SIG(i) 
        end do 
      else 
        do  i=1,Ndatapoints
         T=vector_x(i)-SXOSS
         ST2=ST2+T*T
         B_coeff=B_coeff+T*vector_y(i)
        end do 
      end if
      B_coeff=B_coeff/ST2           ! solve for A,B
      A_coeff=(Svector_y-Svector_x*B_coeff)/SS
      SIGA=SQRT((1.d0+Svector_x*Svector_x/(SS*ST2))/SS)
      SIGB=SQRT(1.d0/ST2)
      CHI2=0.d0                     ! calculate x^2
      Q=1.d0
      if(KWT .EQ. 0.d0) then
        do  i=1,Ndatapoints
          CHI2=CHI2+(vector_y(i)-A_coeff-B_coeff*vector_x(i))**2.d0
        end do 
        SIGDAT=SQRT(CHI2/(Ndatapoints-2.d0))
        SIGA=SIGA*SIGDAT
        SIGB=SIGB*SIGDAT
      else
        do  i=1,Ndatapoints
          CHI2=CHI2+((vector_y(i)-A_coeff-B_coeff*vector_x(i))/SIG(i))**2.d0  
        end do 
        Q=1.d0 
      end if

      return

end subroutine LinRegLeastSquares          
