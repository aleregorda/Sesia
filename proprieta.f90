 subroutine proprieta(i,num_mat,materiale,Tref_M,Rho_M,Visc_M,alpha_M,Cp_M,Hr_M,Kc_M,ndisl,grain,Ea,Va,coeffA,m_grain,&
 Eadf,Vadf,coeffAdf,teta_fr_M,Cohe_M,t_mantle,t_cont,t_lcont,t_oc,t_air,t_sed,t_serp,t_mantleM,t_contM,t_lcontM,t_ocM,Rw_f,Rw_c,Ws,&
 diffusion,rheology,f_scale,t_wet,Visc_min,Visc_max)

 Implicit none

 integer, intent(out) :: num_mat,i,t_mantle,t_cont,t_lcont,t_oc,t_air,t_sed,t_serp,t_mantleM,t_contM,t_lcontM,t_ocM,t_wet
 character(len=*), dimension(:), allocatable, intent(out) :: materiale
 logical, dimension(:), allocatable, intent(out) :: diffusion
 integer, dimension(:), allocatable, intent(out) :: rheology
 double precision, dimension(:), allocatable, intent(out) :: Tref_M,Rho_M,Visc_M,alpha_M,Cp_M,Hr_M,Kc_M,ndisl,grain,Ea,Va,&
 coeffA,m_grain,Eadf,Vadf,coeffAdf,teta_fr_M,Cohe_M,Rw_f,Rw_c,Ws,f_scale,Visc_min,Visc_max

 num_mat=12                                                    !Numero di materiali del sistema

 allocate(materiale(num_mat),Tref_M(num_mat),Rho_M(num_mat),Visc_M(num_mat),alpha_M(num_mat),Cp_M(num_mat),Hr_M(num_mat),&
 Kc_M(num_mat),ndisl(num_mat),grain(num_mat),Ea(num_mat),Va(num_mat),coeffA(num_mat),m_grain(num_mat),Eadf(num_mat),Vadf(num_mat),&
 coeffAdf(num_mat),teta_fr_M(num_mat),Cohe_M(num_mat),Rw_f(num_mat),Rw_c(num_mat),Ws(num_mat),diffusion(num_mat),rheology(num_mat),&
 f_scale(num_mat),Visc_min(num_mat),Visc_max(num_mat))

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c     PROPRIETA' DEI MATERIALI - verificare che il numero di materiali sia uguale a num_mat
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!Materiale 1
 i=1
 t_cont=i
 materiale(i)='continental crust'
 Tref_M(i)=273.15D0                                              !Temperatura di riferimento
 Rho_M(i)=2800.D0                                                  !Densità
 Visc_M(i)=1.D22                                                  !Viscosità
 alpha_M(i)=3.D-05                                                  !Coefficiente espansione termica
 Cp_M(i)=750.D0                                                  !Calore specifico
 Hr_M(i)=1.15D-06                                                  !Calore radiogenico
 Kc_M(i)=2.25D0                                                  !Conduttività termica
 rheology(i)=4                                                   !1-lineare; 2-creep; 3-rigido-plastico; 4-visco-plastico
 f_scale(i)=1.D0                                                   !Fattore di scala per la reologia
 Visc_min(i)=1.D20                                                  !Viscosità minima
 Visc_max(i)=1.D25                                                  !Viscosità massima
!Disclocation creep
 coeffA(i)=1.135723D-28                                                  !Coefficiente pre-esponenziale dislocation creep
 ndisl(i)=3.2D0                                                  !Stress exponent dislocation creep
 Ea(i)=1.23D05                                                  !Energia di attivazione dislocation creep
 Va(i)=0.D0                                                  !Volume di attivazione dislocation creep
!Diffusion creep
 diffusion(i)=.false.                                                 !Utilizzo diffusion creep
 coeffAdf(i)=6.07931D-19                                                  !Coefficiente pre-esponenziale diffusion creep
 grain(i)=1.D-03                                                  !Dimensione grani per diffusion creep
 m_grain(i)=2.5D0                                                  !Esponente dimensione grani per diffusione creep
 Eadf(i)=3.D05                                                  !Energia di attivazione diffusion creep
 Vadf(i)=6.D-06                                                  !Volume di attivazione diffusion creep
!Plasticity
 teta_fr_M(i)=15.D0                                                  !Angolo di frizione iniziale
 Cohe_M(i)=2.D07                                                  !Coesione iniziale
 Rw_f(i)=7.5D0                                                       !Rapporto angolo di frizione per weakening
 Rw_c(i)=5.D0                                                       !Rapporto coesione per weakening
 Ws(i)=1.D0                                                       !Fattore di diminuzione viscosità per weakening

!Materiale 2
 i=2
 t_lcont=i
 materiale(i)='lower continental crust'
 Tref_M(i)=273.15D0                                              !Temperatura di riferimento
 Rho_M(i)=2950.D0                                                  !Densità
 Visc_M(i)=1.D22                                                  !Viscosità
 alpha_M(i)=3.D-05                                                  !Coefficiente espansione termica
 Cp_M(i)=750.D0                                                  !Calore specifico
 Hr_M(i)=0.75D-06                                                  !Calore radiogenico
 Kc_M(i)=2.25D0                                                  !Conduttività termica
 rheology(i)=4                                                   !1-lineare; 2-creep; 3-rigido-plastico; 4-visco-plastico
 f_scale(i)=1.D0                                                   !Fattore di scala per la reologia
 Visc_min(i)=1.D20                                                  !Viscosità minima
 Visc_max(i)=1.D25                                                  !Viscosità massima
!Disclocation creep
 coeffA(i)=8.574D-28                                                  !Coefficiente pre-esponenziale dislocation creep
 ndisl(i)=4.D0                                                  !Stress exponent dislocation creep
 Ea(i)=2.22815D05                                                  !Energia di attivazione dislocation creep
 Va(i)=0.D0                                                  !Volume di attivazione dislocation creep
!Diffusion creep
 diffusion(i)=.false.                                                 !Utilizzo diffusion creep
 coeffAdf(i)=6.07931D-19                                                  !Coefficiente pre-esponenziale diffusion creep
 grain(i)=1.D-03                                                  !Dimensione grani per diffusion creep
 m_grain(i)=2.5D0                                                  !Esponente dimensione grani per diffusione creep
 Eadf(i)=3.D05                                                  !Energia di attivazione diffusion creep
 Vadf(i)=6.D-06                                                  !Volume di attivazione diffusion creep
!Plasticity
 teta_fr_M(i)=15.D0                                                  !Angolo di frizione iniziale
 Cohe_M(i)=2.D07                                                  !Coesione iniziale
 Rw_f(i)=7.5D0                                                       !Rapporto angolo di frizione per weakening
 Rw_c(i)=5.D0                                                       !Rapporto coesione per weakening
 Ws(i)=1.D0                                                       !Fattore di diminuzione viscosità per weakening

!Materiale 3
 i=3
 t_oc=i
 materiale(i)='oceanic crust'
 Tref_M(i)=273.15D0                                              !Temperatura di riferimento
 Rho_M(i)=2900.D0                                                  !Densità
 Visc_M(i)=1.D22                                                  !Viscosità
 alpha_M(i)=3.D-05                                                  !Coefficiente espansione termica
 Cp_M(i)=750.D0                                                  !Calore specifico
 Hr_M(i)=0.D0                                                  !Calore radiogenico
 Kc_M(i)=2.25D0                                                  !Conduttività termica
 rheology(i)=4                                                   !1-lineare; 2-creep; 3-rigido-plastico; 4-visco-plastico
 f_scale(i)=1.D0                                                   !Fattore di scala per la reologia
 Visc_min(i)=1.D19                                                  !Viscosità minima
 Visc_max(i)=1.D25                                                  !Viscosità massima
!Disclocation creep
 coeffA(i)=1.393D-37                                                  !Coefficiente pre-esponenziale dislocation creep
 ndisl(i)=3.8D0                                                  !Stress exponent dislocation creep
 Ea(i)=8.9D05                                                  !Energia di attivazione dislocation creep
 Va(i)=3.2D-06                                                  !Volume di attivazione dislocation creep
!Diffusion creep
 diffusion(i)=.false.                                                 !Utilizzo diffusion creep
 coeffAdf(i)=6.07931D-19                                                  !Coefficiente pre-esponenziale diffusion creep
 grain(i)=1.D-03                                                  !Dimensione grani per diffusion creep
 m_grain(i)=2.5D0                                                  !Esponente dimensione grani per diffusione creep
 Eadf(i)=3.D05                                                  !Energia di attivazione diffusion creep
 Vadf(i)=6.D-06                                                  !Volume di attivazione diffusion creep
!Plasticity
 teta_fr_M(i)=15.D0                                                  !Angolo di frizione iniziale
 Cohe_M(i)=1.D07                                                  !Coesione iniziale
 Rw_f(i)=7.5D0                                                       !Rapporto angolo di frizione per weakening
 Rw_c(i)=5.D0                                                       !Rapporto coesione per weakening
 Ws(i)=1.D0                                                       !Fattore di diminuzione viscosità per weakening

!Materiale 4
 i=4
 materiale(i)='lower oceanic crust'
 Tref_M(i)=273.15D0                                              !Temperatura di riferimento
 Rho_M(i)=2900.D0                                                  !Densità
 Visc_M(i)=1.D22                                                  !Viscosità
 alpha_M(i)=3.D-05                                                  !Coefficiente espansione termica
 Cp_M(i)=750.D0                                                  !Calore specifico
 Hr_M(i)=0.D0                                                  !Calore radiogenico
 Kc_M(i)=2.25D0                                                  !Conduttività termica
 rheology(i)=4                                                   !1-lineare; 2-creep; 3-rigido-plastico; 4-visco-plastico
 f_scale(i)=1.D0                                                   !Fattore di scala per la reologia
 Visc_min(i)=1.D20                                                  !Viscosità minima
 Visc_max(i)=1.D25                                                  !Viscosità massima
!Disclocation creep
 coeffA(i)=1.990536D-11                                                  !Coefficiente pre-esponenziale dislocation creep
 ndisl(i)=3.4D0                                                  !Stress exponent dislocation creep
 Ea(i)=4.97D05                                                  !Energia di attivazione dislocation creep
 Va(i)=0.D0                                                  !Volume di attivazione dislocation creep
!Diffusion creep
 diffusion(i)=.false.                                                 !Utilizzo diffusion creep
 coeffAdf(i)=6.07931D-19                                                  !Coefficiente pre-esponenziale diffusion creep
 grain(i)=1.D-03                                                  !Dimensione grani per diffusion creep
 m_grain(i)=2.5D0                                                  !Esponente dimensione grani per diffusione creep
 Eadf(i)=3.D05                                                  !Energia di attivazione diffusion creep
 Vadf(i)=6.D-06                                                  !Volume di attivazione diffusion creep
!Plasticity
 teta_fr_M(i)=15.D0                                                  !Angolo di frizione iniziale
 Cohe_M(i)=2.D07                                                  !Coesione iniziale
 Rw_f(i)=7.5D0                                                       !Rapporto angolo di frizione per weakening
 Rw_c(i)=5.D0                                                       !Rapporto coesione per weakening
 Ws(i)=1.D0                                                       !Fattore di diminuzione viscosità per weakening

!Materiale 5
 i=5
 t_serp=i
 materiale(i)='serpentine'
 Tref_M(i)=273.15D0                                              !Temperatura di riferimento
 Rho_M(i)=2900.D0                                                  !Densità
 Visc_M(i)=1.D22                                                  !Viscosità
 alpha_M(i)=3.D-05                                                  !Coefficiente espansione termica
 Cp_M(i)=1250.D0                                                  !Calore specifico
 Hr_M(i)=0.D0                                                  !Calore radiogenico
 Kc_M(i)=2.25D0                                                  !Conduttività termica
 rheology(i)=4                                                   !1-lineare; 2-creep; 3-rigido-plastico; 4-visco-plastico
 f_scale(i)=1.D0                                                   !Fattore di scala per la reologia
 Visc_min(i)=1.D19                                                  !Viscosità minima
 Visc_max(i)=1.D25                                                  !Viscosità massima
!Disclocation creep
 coeffA(i)=1.393D-37                                                  !Coefficiente pre-esponenziale dislocation creep
 ndisl(i)=3.8D0                                                  !Stress exponent dislocation creep
 Ea(i)=8.9D05                                                  !Energia di attivazione dislocation creep
 Va(i)=3.2D-06                                                  !Volume di attivazione dislocation creep
!Diffusion creep
 diffusion(i)=.false.                                                 !Utilizzo diffusion creep
 coeffAdf(i)=6.07931D-19                                                  !Coefficiente pre-esponenziale diffusion creep
 grain(i)=1.D-03                                                  !Dimensione grani per diffusion creep
 m_grain(i)=2.5D0                                                  !Esponente dimensione grani per diffusione creep
 Eadf(i)=3.D05                                                  !Energia di attivazione diffusion creep
 Vadf(i)=6.D-06                                                  !Volume di attivazione diffusion creep
!Plasticity
 teta_fr_M(i)=15.D0                                                  !Angolo di frizione iniziale
 Cohe_M(i)=1.D07                                                  !Coesione iniziale
 Rw_f(i)=7.5D0                                                       !Rapporto angolo di frizione per weakening
 Rw_c(i)=5.D0                                                       !Rapporto coesione per weakening
 Ws(i)=1.D0                                                       !Fattore di diminuzione viscosità per weakening

!Materiale 6
 i=6
 t_mantle=i
 materiale(i)='upper mantle'
 Tref_M(i)=1603.15D0                                              !Temperatura di riferimento
 Rho_M(i)=3250.D0                                                  !Densità
 Visc_M(i)=1.D22                                                  !Viscosità
 alpha_M(i)=3.D-05                                                  !Coefficiente espansione termica
 Cp_M(i)=1250.D0                                                  !Calore specifico
 Hr_M(i)=0.D0                                                  !Calore radiogenico
 Kc_M(i)=2.25D0                                                  !Conduttività termica
 rheology(i)=4                                                   !1-lineare; 2-creep; 3-rigido-plastico; 4-visco-plastico
 f_scale(i)=1.D0                                                   !Fattore di scala per la reologia
 Visc_min(i)=1.D20                                                  !Viscosità minima
 Visc_max(i)=1.D25                                                  !Viscosità massima
!Disclocation creep
 coeffA(i)=2.42D-16                                                  !Coefficiente pre-esponenziale dislocation creep
 ndisl(i)=3.5D0                                                  !Stress exponent dislocation creep
 Ea(i)=5.4041D05                                                  !Energia di attivazione dislocation creep
 Va(i)=20.D-06                                                  !Volume di attivazione dislocation creep
!Diffusion creep
 diffusion(i)=.false.                                                 !Utilizzo diffusion creep
 coeffAdf(i)=6.07931D-19                                                  !Coefficiente pre-esponenziale diffusion creep
 grain(i)=1.D-03                                                  !Dimensione grani per diffusion creep
 m_grain(i)=2.5D0                                                  !Esponente dimensione grani per diffusione creep
 Eadf(i)=3.D05                                                  !Energia di attivazione diffusion creep
 Vadf(i)=6.D-06                                                  !Volume di attivazione diffusion creep
!Plasticity
 teta_fr_M(i)=15.D0                                                  !Angolo di frizione iniziale
 Cohe_M(i)=2.D07                                                  !Coesione iniziale
 Rw_f(i)=7.5D0                                                       !Rapporto angolo di frizione per weakening
 Rw_c(i)=5.D0                                                       !Rapporto coesione per weakening
 Ws(i)=1.D0                                                       !Fattore di diminuzione viscosità per weakening

!Materiale 7
 i=7
 materiale(i)='mantle'
 Tref_M(i)=1603.15D0                                              !Temperatura di riferimento
 Rho_M(i)=3250.D0                                                  !Densità
 Visc_M(i)=1.D22                                                  !Viscosità
 alpha_M(i)=3.D-05                                                  !Coefficiente espansione termica
 Cp_M(i)=1250.D0                                                  !Calore specifico
 Hr_M(i)=0.D0                                                  !Calore radiogenico
 Kc_M(i)=2.25D0                                                  !Conduttività termica
 rheology(i)=4                                                   !1-lineare; 2-creep; 3-rigido-plastico; 4-visco-plastico
 f_scale(i)=1.D0                                                   !Fattore di scala per la reologia
 Visc_min(i)=1.D20                                                  !Viscosità minima
 Visc_max(i)=1.D25                                                  !Viscosità massima
!Disclocation creep
 coeffA(i)=2.42D-16                                                  !Coefficiente pre-esponenziale dislocation creep
 ndisl(i)=3.5D0                                                  !Stress exponent dislocation creep
 Ea(i)=5.4041D05                                                  !Energia di attivazione dislocation creep
 Va(i)=20.D-06                                                  !Volume di attivazione dislocation creep
!Diffusion creep
 diffusion(i)=.false.                                                 !Utilizzo diffusion creep
 coeffAdf(i)=6.07931D-19                                                  !Coefficiente pre-esponenziale diffusion creep
 grain(i)=1.D-03                                                  !Dimensione grani per diffusion creep
 m_grain(i)=2.5D0                                                  !Esponente dimensione grani per diffusione creep
 Eadf(i)=3.D05                                                  !Energia di attivazione diffusion creep
 Vadf(i)=6.D-06                                                  !Volume di attivazione diffusion creep
!Plasticity
 teta_fr_M(i)=15.D0                                                  !Angolo di frizione iniziale
 Cohe_M(i)=2.D07                                                  !Coesione iniziale
 Rw_f(i)=7.5D0                                                       !Rapporto angolo di frizione per weakening
 Rw_c(i)=5.D0                                                       !Rapporto coesione per weakening
 Ws(i)=1.D0                                                       !Fattore di diminuzione viscosità per weakening

!Materiale 8
 i=8
 materiale(i)='continental crust'
 Tref_M(i)=273.15D0                                              !Temperatura di riferimento
 Rho_M(i)=2800.D0                                                  !Densità
 Visc_M(i)=1.D22                                                  !Viscosità
 alpha_M(i)=3.D-05                                                  !Coefficiente espansione termica
 Cp_M(i)=750.D0                                                  !Calore specifico
 Hr_M(i)=1.15D-06                                                  !Calore radiogenico
 Kc_M(i)=2.25D0                                                  !Conduttività termica
 rheology(i)=4                                                   !1-lineare; 2-creep; 3-rigido-plastico; 4-visco-plastico
 f_scale(i)=1.D0                                                   !Fattore di scala per la reologia
 Visc_min(i)=1.D20                                                  !Viscosità minima
 Visc_max(i)=1.D25                                                  !Viscosità massima
!Disclocation creep
 coeffA(i)=1.135723D-28                                                  !Coefficiente pre-esponenziale dislocation creep
 ndisl(i)=3.2D0                                                  !Stress exponent dislocation creep
 Ea(i)=1.23D05                                                  !Energia di attivazione dislocation creep
 Va(i)=0.D0                                                  !Volume di attivazione dislocation creep
!Diffusion creep
 diffusion(i)=.false.                                                 !Utilizzo diffusion creep
 coeffAdf(i)=6.07931D-19                                                  !Coefficiente pre-esponenziale diffusion creep
 grain(i)=1.D-03                                                  !Dimensione grani per diffusion creep
 m_grain(i)=2.5D0                                                  !Esponente dimensione grani per diffusione creep
 Eadf(i)=3.D05                                                  !Energia di attivazione diffusion creep
 Vadf(i)=6.D-06                                                  !Volume di attivazione diffusion creep
!Plasticity
 teta_fr_M(i)=15.D0                                                  !Angolo di frizione iniziale
 Cohe_M(i)=2.D07                                                  !Coesione iniziale
 Rw_f(i)=7.5D0                                                       !Rapporto angolo di frizione per weakening
 Rw_c(i)=5.D0                                                       !Rapporto coesione per weakening
 Ws(i)=1.D0                                                       !Fattore di diminuzione viscosità per weakening

!Materiale 9
 i=9
 materiale(i)='lower continental crust'
 Tref_M(i)=273.15D0                                              !Temperatura di riferimento
 Rho_M(i)=2900.D0                                                  !Densità
 Visc_M(i)=1.D22                                                  !Viscosità
 alpha_M(i)=3.D-05                                                  !Coefficiente espansione termica
 Cp_M(i)=750.D0                                                  !Calore specifico
 Hr_M(i)=0.75D-06                                                  !Calore radiogenico
 Kc_M(i)=2.25D0                                                  !Conduttività termica
 rheology(i)=4                                                   !1-lineare; 2-creep; 3-rigido-plastico; 4-visco-plastico
 f_scale(i)=1.D0                                                   !Fattore di scala per la reologia
 Visc_min(i)=1.D20                                                  !Viscosità minima
 Visc_max(i)=1.D25                                                  !Viscosità massima
!Disclocation creep
 coeffA(i)=8.574D-28                                                  !Coefficiente pre-esponenziale dislocation creep
 ndisl(i)=4.D0                                                  !Stress exponent dislocation creep
 Ea(i)=2.22815D05                                                  !Energia di attivazione dislocation creep
 Va(i)=0.D0                                                  !Volume di attivazione dislocation creep
!Diffusion creep
 diffusion(i)=.false.                                                 !Utilizzo diffusion creep
 coeffAdf(i)=6.07931D-19                                                  !Coefficiente pre-esponenziale diffusion creep
 grain(i)=1.D-03                                                  !Dimensione grani per diffusion creep
 m_grain(i)=2.5D0                                                  !Esponente dimensione grani per diffusione creep
 Eadf(i)=3.D05                                                  !Energia di attivazione diffusion creep
 Vadf(i)=6.D-06                                                  !Volume di attivazione diffusion creep
!Plasticity
 teta_fr_M(i)=15.D0                                                  !Angolo di frizione iniziale
 Cohe_M(i)=2.D07                                                  !Coesione iniziale
 Rw_f(i)=7.5D0                                                       !Rapporto angolo di frizione per weakening
 Rw_c(i)=5.D0                                                       !Rapporto coesione per weakening
 Ws(i)=1.D0                                                       !Fattore di diminuzione viscosità per weakening

!Materiale 10
 i=10
 t_sed=i
 materiale(i)='sediments'
 Tref_M(i)=273.15D0                                              !Temperatura di riferimento
 Rho_M(i)=2650.D0                                                  !Densità
 Visc_M(i)=1.D22                                                  !Viscosità
 alpha_M(i)=3.D-05                                                  !Coefficiente espansione termica
 Cp_M(i)=750.D0                                                  !Calore specifico
 Hr_M(i)=1.15D-06                                                  !Calore radiogenico
 Kc_M(i)=2.25D0                                                  !Conduttività termica
 rheology(i)=4                                                   !1-lineare; 2-creep; 3-rigido-plastico; 4-visco-plastico
 f_scale(i)=1.D0                                                   !Fattore di scala per la reologia
 Visc_min(i)=1.D20                                                  !Viscosità minima
 Visc_max(i)=1.D25                                                  !Viscosità massima
!Disclocation creep
 coeffA(i)=8.574D-28                                                  !Coefficiente pre-esponenziale dislocation creep
 ndisl(i)=4.D0                                                  !Stress exponent dislocation creep
 Ea(i)=2.22815D05                                                  !Energia di attivazione dislocation creep
 Va(i)=0.D0                                                  !Volume di attivazione dislocation creep
!Diffusion creep
 diffusion(i)=.false.                                                 !Utilizzo diffusion creep
 coeffAdf(i)=6.07931D-19                                                  !Coefficiente pre-esponenziale diffusion creep
 grain(i)=1.D-03                                                  !Dimensione grani per diffusion creep
 m_grain(i)=2.5D0                                                  !Esponente dimensione grani per diffusione creep
 Eadf(i)=3.D05                                                  !Energia di attivazione diffusion creep
 Vadf(i)=6.D-06                                                  !Volume di attivazione diffusion creep
!Plasticity
 teta_fr_M(i)=2.D0                                                  !Angolo di frizione iniziale
 Cohe_M(i)=5.D06                                                  !Coesione iniziale
 Rw_f(i)=1.D0                                                       !Rapporto angolo di frizione per weakening
 Rw_c(i)=5.D0                                                       !Rapporto coesione per weakening
 Ws(i)=1.D0                                                       !Fattore di diminuzione viscosità per weakening

!Materiale 11
 i=11
 t_wet=i
 materiale(i)='wet mantle'
 Tref_M(i)=1603.15D0                                              !Temperatura di riferimento
 Rho_M(i)=3250.D0                                                  !Densità
 Visc_M(i)=1.D22                                                  !Viscosità
 alpha_M(i)=3.D-05                                                  !Coefficiente espansione termica
 Cp_M(i)=1250.D0                                                  !Calore specifico
 Hr_M(i)=0.D0                                                  !Calore radiogenico
 Kc_M(i)=2.25D0                                                  !Conduttività termica
 rheology(i)=4                                                   !1-lineare; 2-creep; 3-rigido-plastico; 4-visco-plastico
 f_scale(i)=1.D0                                                   !Fattore di scala per la reologia
 Visc_min(i)=1.D20                                                  !Viscosità minima
 Visc_max(i)=1.D25                                                  !Viscosità massima
!Disclocation creep
 coeffA(i)=2.42D-16                                                  !Coefficiente pre-esponenziale dislocation creep
 ndisl(i)=3.5D0                                                  !Stress exponent dislocation creep
 Ea(i)=5.4041D05                                                  !Energia di attivazione dislocation creep
 Va(i)=20.D-06                                                  !Volume di attivazione dislocation creep
!Diffusion creep
 diffusion(i)=.false.                                                 !Utilizzo diffusion creep
 coeffAdf(i)=6.07931D-19                                                  !Coefficiente pre-esponenziale diffusion creep
 grain(i)=1.D-03                                                  !Dimensione grani per diffusion creep
 m_grain(i)=2.5D0                                                  !Esponente dimensione grani per diffusione creep
 Eadf(i)=3.D05                                                  !Energia di attivazione diffusion creep
 Vadf(i)=6.D-06                                                  !Volume di attivazione diffusion creep
!Plasticity
 teta_fr_M(i)=15.D0                                                  !Angolo di frizione iniziale
 Cohe_M(i)=2.D07                                                  !Coesione iniziale
 Rw_f(i)=7.5D0                                                       !Rapporto angolo di frizione per weakening
 Rw_c(i)=5.D0                                                       !Rapporto coesione per weakening
 Ws(i)=1.D0                                                       !Fattore di diminuzione viscosità per weakening

!Materiale 12
 i=12
 t_mantleM=i
 materiale(i)='melt'
 Tref_M(i)=273.15D0                                              !Temperatura di riferimento
 Rho_M(i)=2900.D0                                                  !Densità
 Visc_M(i)=1.D22                                                  !Viscosità
 alpha_M(i)=3.D-05                                                  !Coefficiente espansione termica
 Cp_M(i)=1250.D0                                                  !Calore specifico
 Hr_M(i)=0.D0                                                  !Calore radiogenico
 Kc_M(i)=2.25D0                                                  !Conduttività termica
 rheology(i)=4                                                   !1-lineare; 2-creep; 3-rigido-plastico; 4-visco-plastico
 f_scale(i)=1.D0                                                   !Fattore di scala per la reologia
 Visc_min(i)=1.D17                                                  !Viscosità minima
 Visc_max(i)=1.D25                                                  !Viscosità massima
!Disclocation creep
 coeffA(i)=2.42D-16                                                  !Coefficiente pre-esponenziale dislocation creep
 ndisl(i)=3.5D0                                                  !Stress exponent dislocation creep
 Ea(i)=5.4041D05                                                  !Energia di attivazione dislocation creep
 Va(i)=20.D-06                                                  !Volume di attivazione dislocation creep
!Diffusion creep
 diffusion(i)=.false.                                                 !Utilizzo diffusion creep
 coeffAdf(i)=6.07931D-19                                                  !Coefficiente pre-esponenziale diffusion creep
 grain(i)=1.D-03                                                  !Dimensione grani per diffusion creep
 m_grain(i)=2.5D0                                                  !Esponente dimensione grani per diffusione creep
 Eadf(i)=3.D05                                                  !Energia di attivazione diffusion creep
 Vadf(i)=6.D-06                                                  !Volume di attivazione diffusion creep
!Plasticity
 teta_fr_M(i)=15.D0                                                  !Angolo di frizione iniziale
 Cohe_M(i)=2.D07                                                  !Coesione iniziale
 Rw_f(i)=7.5D0                                                       !Rapporto angolo di frizione per weakening
 Rw_c(i)=5.D0                                                       !Rapporto coesione per weakening
 Ws(i)=1.D0                                                       !Fattore di diminuzione viscosità per weakening


!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccc NON MODFICARE cccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

 if(i.ne.num_mat)then
   print *,''
   print *,'ERRORE: Numero materiali non corretto'
   stop
 endif

 do i=1,num_mat
   if(rheology(i).ne.1.and.rheology(i).ne.2.and.rheology(i).ne.3.and.rheology(i).ne.4)then
     print *,''
     print *,'ERRORE: Reologia non corretta per ',materiale(i)
     stop
   endif
 enddo

 end subroutine proprieta