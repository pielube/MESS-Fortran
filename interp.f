!----------------------------------------------------------------------
!
!     * subroutine per interpolazione secondo una SPLINE
!     * Doppia Precisione
!
!     * Carcasci C. 02/02/2003
!     UPDATE
!     CC  30/03/05 -> estrapolazione a gradini e logaritmica
!     CC  30/03/05 -> Se due punti concidono, non si blocca
!     CC  22/06/05 -> Correzione interpolazione spline se nell'ultimo intervallo
!     CC  22/11/07 -> Scrittura Warning nell'etichetta 77
!     CC  02/01/15 -> funzioanmento se il vettore X e' orinato in senso decrescente
!                  -> Aggiunta IndErr (Indice Errore)
!     CC  04/04/15 -> correzione interpolazione primo/ultimo tratto (si usa una quadratica)
!
! INPUT
! ------
!     nmax  =num. max di valori (dichiarazione vettori)
c     inter =metodo di interpolazione:
c             =0 -> Funzione a Gradini (valore uguale al punto più vicino)
c             =1 -> lineare
c             =2 -> spline
c     iEstr =metodo di ESTRAPOLAZIONE:
c             =0 -> uguale al primo o ultimo valore
c             =1 -> lineare
c             =2 -> spline
c             =3 -> Logaritmica   [y=lg(a+bx)   ] 
c     n     =numero effetivo degli elementi del vettore
c     x     =vettore delle ascisse  della curva discretizzata (se due punti coincidenti, il vettore puo' essere accorciato)
c     y     =vettore delle ordinate della curva discretizzata
c     xd    =Ascissa per il quale si vuole interpolare (interpolante)
c
c ==> consigliato (ma non obbligatorio) Aprire l'unita' 77 per scrivere messaggi di warning
c
c OUPUT
c -----
c     yd    =(OUT) valore interpolato (determinato)
c
c     IndErr=    0 -> Tutto ok
c                1 -> Vettore composto da un solo valore. non si esegue l'interpolazione.
c                     Si assegna ad Yd l'unico valore
c                2 -> Estrapolazione limite inferiore
c                3 -> Estrapolazione limite Superiore
c               13 -> vettore X non ordinato in senso monotono (pero' non incide nell'interpolazione
c                     (il punto di interpolazione e' esterno alla zona di disuniformita')
c             1001 -> Vettore vuoto
c             1002 -> Numero dati inferiore alla dimensione del vettore. 
c                     Si limita il numero di valore alla dimensione del vettore
c             1003 -> Numero dati inferiore alla dimensione del vettore. 
c                     Problema nel calcolo di singola precisione
c             1013 -> vettore X non ordinato in senso monotono (pero' non incide nell'interpolazione
c                     (il punto di interpolazione e' interno alla zona di disuniformita'. 
c                      Si hanno piu' soluzioni. sara' presa la prima)
c             1015 -> vettore X cha ha 2 punti coincidenti, ma con diversa Y (inrepolazione impossibile)
c                     
c
c
c     * Se n=1 automaticamente prende il valore costante (iEstr=0)
c     * Se n=2 automaticamente si esegue una inter-estrapolazione lineare (Inter,iEstr=1)
c
c
c     * DOPPIA INERPOLAZIONE della matrice X(n1,n2), Y(n1,n2)
c     * e si vuole conoscere il valore relativo al punto X1,X2
c
c     si esegue una doppia interpolazione 
c     Si usa un altro vettore auxX(n2), auxY(n2)
c
c     do i=1,n2
c       call ryp(nmax,1,n1,X(1,i),Y(1,i),X1,auxY(i),IndErr)
c     enddo
c
c     call  ryp(nmax,1,n2,auxX,auxY,X2,YD,IndErr)
c
c
c----------------------------------------------------------------------

      subroutine ryp2(nmax,inter,iEstr,
     &                N,x,y,
     &                xd,yd,
     &                IndErr)

cc    parameter(nmax=10000)
      implicit real(8) (a-h,o-z)

      integer, intent(IN   ):: nmax,inter,iEstr
      integer, intent(INOUT):: N
      dimension x(nmax),y(nmax)
      real(8), intent(IN ):: Xd

      integer, intent(OUT):: IndErr
      real(8), intent(OUT):: Yd


      logical fileopen

c     -------------FINE DICHIARAZIONI-------------------------------------------

      !* si verifica se il programma di warning e' aperto
      inquire(unit=77,opened=fileopen)

      IndErr=0

      !* Errore (Vettore nullo)
      !* ----------------------


      if(N.le.0) then
       !write( *,4) N
        if(fileopen) then
        write(77,4) N
        endif
   4    format(/,' Error RYP.01: There is not any value into vector',/,
     &       t12,'number of data           n   =',i10          )

        indErr=1001

        return
      endif


c     * Caso particolare: un solo valore
c     * --------------------------------
      if(N.eq.1) then
**      write( *,9) N
        if(fileopen) then
        write(77,9) N
        endif
   9    format(/,' Warning RYP.01: just a point into vector'  ,/,
     &       t12,'number of data           n   =',i10          ,/,
     &       t12,'the result is equal to this point!'             )

        indErr=1

        yd=y(N)   !assegnazione risultato

        return
      endif

c     * controllo che il vettore sia eccessivamente lungo
c     * -------------------------------------------------
      if(N.GT.Nmax) then
        write( *,12) N,Nmax
        if(fileopen) then
        write(77,12) N,Nmax
        endif
  12    format(' Warning RYP.02: too many point into vector',/,
     &     t12,'number of data           n   =',i10         ,/,
     &     t12,'maximum number of data   nmax=',i10             )

        indErr=1002

        if(Xd.le.X(Nmax)) then
          N=Nmax   !riassegnazione numero nodi
        else
          write(*,*) 'inoltre esce dal range...'
          stop 'error!!'
        endif

      endif



      !* si ricercano e si eliminano punti coincidenti
      !* ----------------------------------------------
      !* si imposta tutto il campo
      i0=1
      n =n

  13  continue

      if(i0.gt.n-1) goto 16

      do 2 i=i0,N-1

        Dx=x(i+1)-x(i  )
        Dy=y(i+1)-y(i  )

        if(x(i  ).gt.1.d-12) Dx=Dx/x(i  )
        if(y(i  ).gt.1.d-12) Dy=Dy/y(i  )

        if(abs(Dx).lt.1.d-14 .and.
     &     abs(Dy).lt.1.d-14       ) then

          !* i punti coincidono... si elimina uno (i+1)

          N=N-1 !si toglie un valore

          !* si scala tutto il vettore di 1
          do ii=i+1,N
            x(ii)=x(ii+1)
            y(ii)=y(ii+1)
          enddo

          i0=i

          goto 13  ! si continua la ricerca

        else
          !* i punti sono proprio diversi... si stoppa

        endif
   2  continue

  16  continue


c     * Caso particolare: un solo valore (rimasto)
c     * --------------------------------
      if(N.eq.1) then
        !* e' rimasto un solo numero
**        write(*,9) N

        indErr=5

        yd=y(N)   !assegnazione risultato

        return
      endif


      !* Si stabilisce se il vettore e' in senso crescente o decrescente
      !* ---------------------------------------------------------
      if    (x(2).gt.x(1)) then
        !* vettore in senso   CRESCENTE
        Iord=1
      elseif(x(2).lt.x(1)) then
        !* vettore in senso DECRESCENTE
        Iord=2
      else
        !* punti coincienti -> non va bene
        Iord=0
        write(*,*) 'RYP internal error'
        stop 'internal error!!'
      endif


      !* controllo che i vettori siano ordinati in senso crescente
      !* ---------------------------------------------------------
      IndInvers=0
      do 14 i=1,N-1
        select case(Iord)
        case(1)
          !* senso   CRESCENTE -> si verifica senso dell'intero vettore
          if    (x(i+1).gt.x(i)) then
            !* vettore effettivame crescente -> tutto ok!

          elseif(x(i+1).lt.x(i)) then
            !* vettore effettivame Decrescente -> Ma era previsto che fosse crescente

            IndInvers=1  !Presenza di inversione del vettore (non piu' monotono

          else
            !* punti coincidenti ... Ma con diversa Y!

            IndErr=1015

            return
          endif


        case(2)
          !* senso DECRESCENTE -> si verifica senso dell'intero vettore
          if    (x(i  ).gt.x(i+1)) then
            !* vettore effettivame crescente -> tutto ok!

            IndInvers=1  !Presenza di inversione del vettore (non piu' monotono

          elseif(x(i  ).lt.x(i+1)) then
            !* vettore effettivame Decrescente -> Ma era previsto che fosse crescente
          else
            !* punti coincidenti ... Ma con diversa Y!

            IndErr=1015

            return
          endif

        case default
          write(*,*) 'RYP internal error (Iord)!!'

          stop
        end select
  14  continue


      !* controllo che il punto coincida con uno dato
      !* --------------------------------------------
      do 5 i=1,N
        if(x(i).eq.xd) then
          yd=y(i)
          return
        endif
   5  continue

c     * determinazione intervallo in cui appartiene Xd
c     * ----------------------------------------------
      Index=0 !interpolazione

      ind      =0
      NumInterv=0  !numero di intervalli che contengono il numero

      do i=1,N-1
        if((x(i  )-xd)*
     &     (x(i+1)-xd).le.0.d0  ) then  ! trovato intervallo
          NumInterv=NumInterv+1

          if(NumInterv.eq.1) then
            ind=i
          endif

          select case(IndInvers)
          case(0)
            !* monotona -> non importa continuare la ricerca
            goto 100
          case(1)
            !* X non monotona -> si controlla che ci siano anche altri intervalli
          case default
            write(*,*) 'RYP internal error!!'

            stop 'RYP Error'
          end select

        else
          !* Xd esterno all'intervallo
        endif
      enddo

      if    (NumInterv.eq.1) then
        !* vettore X non monotono, ma punto X esterno dall'intervallo
        indErr=13  

        i=ind
        goto 100

      elseif(NumInterv.eq.0) then
        !* punto Xd fuori dal range di X -> serve estrapolazione

      elseif(NumInterv.gt.1) then
        !* vettore X non monotono, punto X interno ai diversi intervalli (si hanno piu' soluzioni)
        indErr=1013

        i=ind
        goto 100

      else
        write(*,*) 'RYP internal error!!'

        stop
      endif


      !* xd non e' interno ad alcun intervallo: estrapolazione lineare
      !* -------------------------------------------------------------

      Index=1 !estrapolazione

      select case(Iord)
      case(1)
        !* senso   CRESCENTE -> il punto #1 e' il minimo
        if    (xd.lt.x(1)) then
          indErr=2 ! punto fuori dal range -> estrapolazione 
          i=1
        elseif(xd.gt.x(N)) then
          indErr=3
          i=N-1
        else
          write(*,*) 'RYP internal error!!'
          stop
        endif

      case(2)
        !* senso DECRESCENTE -> il punto #1 e' il massimo
        if    (xd.gt.x(1)) then
          indErr=3 ! punto fuori dal range -> estrapolazione 
          i=1
        elseif(xd.lt.x(N)) then
          indErr=2
          i=N-1
        else
          write(*,*) 'RYP internal error!!'
          stop
        endif

      case default
        write(*,*) 'RYP internal error (Iord)!!'

        stop
      end select

   7  format(' Warning RYP.03: Xd is external to range:',
     &   ' X',a3,'=x(',i4,')=',f8.3,' -> Xd=',f8.3)

 100  continue !inizio a interpolare

      if    ((inter.eq.0 .and. Index.eq.0) .or. 
     &       (iEstr.eq.0 .and. Index.eq.1)      ) then

        !* Funzione a Gradini: si assume il valore del punto piu' vicino
        !* -------------------------------------------------------------
        dx1=abs(xd-x(i  ))
        dx2=abs(xd-x(i+1))

        if(dx1.le.dx2) then
          yd=y(i  )
        else
          yd=y(i+1)
        endif

*      elseif((iEstr.eq.3 .and. Index.eq.1)      ) then
*c   
*c       * Funzione Logaritmica: y=lg(a+b*x)
*c       * --------------------
*c
*        bb= dexp(y(i+1)/y(i  ) ) /
*     &     (     x(i+1)-x(i  ) )
*        aa=y(i  )-log(bb*x(i))
*        aa=dexp(aa)
***        bb=(dexp(y(i+1))-dexp(y(i  )))/
***     &     (     x(i+1) -     x(i  ) )
***        aa=dexp(y(i+1))-bb*x(i+1)
*c
*        yd=dlog(aa+bb*xd)

      elseif( N    .eq.2                   .or.
     &       (inter.eq.1 .and. Index.eq.0) .or. 
     &       (iEstr.eq.1 .and. Index.eq.1)      ) then

        !* interpolazione lineare
        !* ----------------------
        if(N.eq.2) i=1

        dy=y(i+1)-y(i)
        dx=x(i+1)-x(i)

        yd=y(i)+dy*(xd-x(i))/dx

        return


      elseif(((N    .eq.3                              )
!    &                                                   .or.
!    &        (N    .gt.3 .and. (i.eq.1 .or. i.eq.N-1) )    
     &                                                      ).and.
     &       ((inter.eq.2 .and. Index.eq.0) .or. 
     &        (iEstr.eq.2 .and. Index.eq.1)                 )    ) then

        !* interpolazione con parabola
        !* ---------------------------
        if(N.eq.3) then
          i=1
        else
          !* per il primo e ultimo tratto, si traccia una parabila che passa per 3 punti
          if    (i.eq.  1) then
            i=1
          elseif(i.eq.N-1) then
            i=N-2
          else
            a=1./0.d0
          endif
        endif

        b1=  y(i  )
        b2= (y(i+1)-y(i  ))/(x(i+1)-x(i  ))
        b3=((y(i+2)-y(i+1))/(x(i+2)-x(i+1))-
     &      (y(i+1)-y(i  ))/(x(i+1)-x(i  ))  )/
     &     ( x(i+2)-x(i  )                   )

        yd=b1            +
     &     b2*(xd-x(i  ))+
     &     b3*(xd-x(i  ))*(xd-x(i+1))


      elseif( N    .gt.3                   .and.
     &       (inter.eq.2 .and. Index.eq.0) .or. 
     &       (iEstr.eq.2 .and. Index.eq.1)      ) then

        Irif=i
        if    (Irif.eq.1  ) then
          i =2
        elseif(Irif.eq.N-1) then
          i=N-2
        else
        endif

        !* interpolazione con cubiche
        !* --------------------------
        dx2=x(i+2)-x(i+1)
        dy2=y(i+2)-y(i+1) 
        dx1=x(i+1)-x(i  )
        dy1=y(i+1)-y(i  )
        dx0=x(i  )-x(i-1)
        dy0=y(i  )-y(i-1)

        x0 =x(i   )
        y0 =y(i   )
        x1 =x(i +1)
        y1 =y(i +1)

        slop0=0.5d0*(dy1/dx1+dy0/dx0)
        slop1=0.5d0*(dy2/dx2+dy1/dx1)

        if(Irif.eq.1  .or.
     &     Irif.eq.N-1    ) then
          !* primo o ultimo tratto (al bordo)
          if(Irif.eq.1    ) then
            !* punto nel primo intervallo
            i0 =2
            i1 =1
          elseif(Irif.eq.N-1) then
            i0 =N-1
            i1 =N
          else
            write(*,*) 'RYP internal error!'
            stop 'internal error!!'
          endif


          call cubic2(slop0,slop1,x0,y0,x1,y1,X(i0),yd,yd1) 

          !* Parabola che ha tangente nel punto (X0;Y0), passa per (X0;Y0) e (X1;Y1)
          aa=((y(i1)-y(i0)) -yd1*(X(i1)-X(i0)) ) /(X(i1)-X(i0))**2
          bb=yd1-2.d0*aa*X(i0)
          cc=Y(i0)-aa*X(i0)**2-bb*X(i0)

          Yd= aa*Xd**2
     &       +bb*Xd
     &       +cc   

        else
          !* punti interpolabili mediante una cubica (interni) -> gia' calcolati
          call cubic2(slop0,slop1,x0,y0,x1,y1,xd,yd,yd1) 
        endif


      else

        write( *,116) inter,iEstr
        if(fileopen) then
        write(77,116) inter,iEstr
        endif
 116    format(' Error RYP.03: Code Error:',/,
     &         t12,'Interpolation code: inter=',i3,/,
     &         t12,'Interpolation code: iEstr=',i3    )
	  stop 'Internal Error!!'
      end if


      return 
      end


      subroutine cubic2(slop0,slop1,x0,y0,x1,y1,x,
     &                  y,yd1)  !Valore e Derivata Prima
      implicit real(8) (a-h,o-z)

      real(8), intent(IN ):: x
      real(8), intent(OUT):: y,yd1

      !* scalatura x
      !* -----------
      x0n=0.d0
      x1n=x1-x0
      xn =x -x0

      d  =y0
      c  =slop0

      a11=x1n**3
      a12=x1n**2
      a13=y1-c*x1n-d
      a21=3.d0*(x1n**2)
      a22=2.d0*x1n
      a23=slop1-c

      det=a11*a22-a12*a21
      a  =(a13*a22-a12*a23)/det
      b  =(a11*a23-a21*a13)/det

      y  =   a*xn**3
     &    +  b*xn**2
     &    +  c*xn
     &    +  d

      !* derivata prima
      yd1= 3.d0*a*xn**2
     &    +2.d0*b*xn
     &    +1.d0*c

      return
      end subroutine cubic2

c----------------------------------------------------------------------
c
c     * subroutine per interpolazione secondo una SPLINE 
c     * Singola precisione
c
c----------------------------------------------------------------------
c
      subroutine ryp (nmax,inter,iEstr,n,x,y,xd,yd,indErr)
c
      parameter(Nmaxd=10000)
c**      implicit real (a-h,o-z)
      real(4),intent(IN ):: Xd
      real(4),intent(OUT):: Yd
      integer,intent(OUT):: indErr
      real             x (nmax ),y (nmax )

      double precision xd2      ,yd2
      double precision x2(Nmaxd),y2(Nmaxd)

      logical fileopen

c     -------------FINE DICHIARAZIONI-------------------------------------------

      !* si verifica se il programma di warning e' aperto
      inquire(unit=77,opened=fileopen)

c     * da singola precisione in doppia
      if(nmax.gt.Nmaxd) then
        write( *,10) nmax,Nmaxd
        if(fileopen) then
        write(77,10) nmax,Nmaxd
        endif
  10    format(/,/,' Errore nella Subr. RYP: ',
     &         'dimensione vettore eccessivo:',/,
     &         t10,'Dimensione vettore           : Nmax =',i10,/,
     &         t10,'Dimensione massima consentita: Nmaxd=',i10,/,
     &         t1 ,' ridurre dimensione vettore nella subr. chiamante',
     &             ' oppure cambiare parameter nella subr RYP!')
        indErr=1003

        stop ' internal error!!'
      endif

      nd   =n

      xd2=dble(xd)

      do i=1,n
        x2(i)=dble(x(i))
        y2(i)=dble(y(i))
      enddo

c     * richiamo subr. in doppia precisione
      call ryp2(nmaxd,inter,iEstr,nd,x2,y2,xd2,yd2,indErr)

c     * da doppia precisione a singola
      yd=sngl(yd2)

      return

      end subroutine ryp
c
c----------------------------------------------------------------------
c
c     * subroutine per interpolazione di una serie di curve
c     * caso tipico un grafico con piu' curve per diversi valori
c
c     * Carcasci C. 23/03/2005
c
c
c     * Singola Precisione
c
c     nmaxXX  =num. max di valori (dichiarazione vettori) sull'asse X del grafico
c     nmaxCur =num. max di curve  (dichiarazione vettori) 
c     inter   =metodo di interpolazione:
c               =1 -> lineare
c               =2 -> spline
c     iEstr   =metodo di ESTRAPOLAZIONE:
c               =0 -> uguale al primo o ultimo valore
c               =1 -> lineare
c               =2 -> spline         
c     NCurve  = Numero curve da interpolare (al massimo NmaxCur)
c     NDati   = Numero dati per ogni curva  (al massimo nmaxXX)
c     XX      = Ascisse  dei punti per ogni curva
c               XX(i,1)  i_esimo punto della prima   curva
c               XX(i,2)  i_esimo punto della seconda curva
c               XX(i,j)  i_esimo punto della j_esima curva
c     YY      = ordinate dei punti per ogni curva
c               YY(i,1)  i_esimo punto della prima   curva
c               YY(i,2)  i_esimo punto della seconda curva
c               YY(i,j)  i_esimo punto della j_esima curva
c     ZZ      = Valore di ogni curva
c               ZZ(  1)  dato          della prima   curva
c               ZZ(  2)  dato          della seconda curva
c               ZZ(  j)  dato          della j_esima curva
c
c     Xdat    = Ascissa            di cui vogliamo sapere il valore
c     Zdat    = VAlore della curva di cui vogliamo sapere il valore
c
c     Yris    =(OUT) valore interpolato (determinato)
c
c
c     * Se n=1 automaticamente prende il valore costante (iEstr=0)
c     * Se n=2 automaticamente si esegue una inter-estrapolazione lineare (Inter,iEstr=1)
c
c                              
c----------------------------------------------------------------------
c
      subroutine rypMLin (nmaxXX,nmaxCur,inter,iEstr,
     &                    NCurve,NDati,XX,YY,ZZ,
     &                    Xdat,Zdat,
     &                    Yris)
c
      implicit real (a-h,o-z)
c
      dimension NDati   (        nmaxCur)   !Numero Dati per ogni curva
c
      dimension XX(nmaxXX ,nmaxCur),
     &          YY(nmaxXX ,nmaxCur),
     &          ZZ(        nmaxCur)   !Valore per ogni curva


      parameter(MmaxCur=1000)

      dimension YZ(        MmaxCur)   !Valore per ogni curva relativo all'ascissa Xdat Richiesta


      logical fileopen

c     -------------FINE DICHIARAZIONI-------------------------------------------

      !* si verifica se il programma di warning e' aperto
      inquire(unit=77,opened=fileopen)


      !* alcuni controlli elementari
      !* ---------------------------
      if(NCurve .gt.MmaxCur) then
        write( *,50) NCurve
        if(fileopen) then
        write(77,50) NCurve
        endif
  50    format(/,/,' Numero curve da interpolare eccessive',/,
     &         t8,' Numero Curve         da interpolare: NCurve =',i6,/,
     &         t8,' Numero Curve Massimo da interpolare: MmaxCur=',i6)

        stop 'Interpolazione Error!'
      endif


c     * per ogni sucra si esegue l'interpolazione relativa all'ascissa richiesta
c     * ------------------------------------------------------------------------

c     * per ogni curva avremo un punto

      do icurva=1,NCurve
        call ryp(nmaxXX,inter,iEstr,
     &                         NDati(  icurva),
     &                         XX   (1,icurva),
     &                         YY   (1,icurva),
     &                         Xdat           ,
     &                         YZ   (  icurva),
     &                         indErr          )
 
      enddo

      !* ora si esegue una interpolazione a X fissata per le varie curve
      !* ---------------------------------------------------------------
      call  ryp(nmaxCur,inter,iEstr, NCurve,ZZ,YZ, Zdat,Yris,
     &                         indErr          )

      return

      end subroutine rypMLin
