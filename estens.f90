!-----------------------------------------------------------------------
!
!     subroutine per cambiare l'estensione al nome del file
!
!  dove: nomei= nome file originale (viene lasciato inalterato)
!        nomeo= nome file con nuova estensione
!        esten= estensione da mettere
!
!-----------------------------------------------------------------------

subroutine estens(nomei,nomeo,esten)

      integer, parameter:: MaxChar=15  !Lunghezza stringa

      character(MaxChar), INTENT (IN ) :: nomei   !Nome File da cambiare l'estensione
      character(MaxChar), INTENT (OUT) :: nomeo   !Nome File con Estensione Cambiata
      character(MaxChar)               :: nomeAux
      character(      3), INTENT (IN ) :: esten   !Estensione da metterci senza "." (per togliere l'estensione si deve impostare 3 spazi-bianchi)


      !* controllo che "nomei" non abbia all'inizio una lettera bianca
      !* -------------------------------------------------------------
      nomeAux = adjustl(nomei) !aggiusta i caratteri a sinistra, togliando spazi bionachi a sinistra

      !* cambio estensione (attenzione che il nome vecchio non l'abbia)
      !* -----------------
      Jpoint= index(nomeAux,'.') !Ricerca della posizione dove c'e' il "."

      if(Jpoint.eq.0) then
        !* file originale senza estensione (senza ".")
        jLung = len_trim(nomeAux) !Lunghezza del carattere
        
        if(jLung.gt.MaxChar-3-1) then
          !* nome file troppo lungo
          write(*,8020) nomeAux
          stop 'Error!'
        endif
        
        nomeo(      1:jLung  ) = nomeAux
        nomeo(jLung+1:jLung+1) = '.'
        nomeo(jLung+2:       ) =esten


      elseif(Jpoint.gt.MaxChar-3) then
        !* file con estensione oltre l'ottavo carattere
        write(*,8020) nomeAux
8020    format(/,' Error NM.03: file name too long: "',a,'"')
         stop 'Error!'


      else
        !estensione gia' presente
        jLung = Jpoint-1
        
        nomeo(      1:jLung  ) = nomeAux
        if(esten.eq.'   ') then
        nomeo(jLung+1:jLung+1) = ' '
        else
        nomeo(jLung+1:jLung+1) = '.'
        endif
        nomeo(jLung+2:       ) =esten

      endif

      return

end subroutine estens

