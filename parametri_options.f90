 subroutine parametri_options(iproc,OUT_DIR,t_initial,t_final,Cn,time_step,max_time_step)

 Implicit none

 logical :: err_detected
 integer :: option_ID,numarg
 character(len=150) :: arg,error
 integer, intent(in) :: iproc
 character(len=*), intent(inout) :: OUT_DIR
 double precision, intent(inout) :: t_initial,t_final,Cn,time_step,max_time_step

! OUT_DIR='../Output_models/Extension_models/Venus/Melting/PTg_Venus_Noise/Extension_0-20Ma/'   

 option_ID=1
 err_detected=.false.
 numarg=command_argument_count()
 
 if(numarg.gt.0)then
   if(iproc.eq.0)write(*,*)''
   if(iproc.eq.0)write(*,*)INT(numarg/2),'parameters changed with respect to parametri.f90'
   if(iproc.eq.0)write(*,*)''
   do while(option_ID.le.numarg)
     call getarg(option_ID,arg)
     if(arg.eq.'-t_initial')then
       option_ID=option_ID+1
       call getarg(option_ID,arg)
       read(arg,*)t_initial
       if(iproc.eq.0)write(*,*)'Running the code with initial time =',t_initial
     elseif(arg.eq.'-t_final')then
        option_ID=option_ID+1
        call getarg(option_ID,arg)
        read(arg,*)t_final
        if(iproc.eq.0)write(*,*)'Running the code with final time =',t_final
     elseif(arg.eq.'-Cn')then
        option_ID=option_ID+1
        call getarg(option_ID,arg)
        read(arg,*)Cn
        if(iproc.eq.0)write(*,*)'Running the code with Courant number =',Cn
     elseif(arg.eq.'-time_step')then
        option_ID=option_ID+1
        call getarg(option_ID,arg)
        read(arg,*)t_initial
        if(iproc.eq.0)write(*,*)'Running the code with time step =',time_step
     elseif(arg.eq.'-max_time_step')then
        option_ID=option_ID+1
        call getarg(option_ID,arg)
        read(arg,*)t_initial
        if(iproc.eq.0)write(*,*)'Running the code with maximum time step =',max_time_step
     elseif(arg.eq.'-OUT_DIR')then
       option_ID=option_ID+1
       call getarg(option_ID,arg)
       read(arg,*)OUT_DIR
       if(iproc.eq.0)write(*,*)'Running the code writing files in: ',OUT_DIR
     else
       call getarg(option_ID,arg)
       read(arg,*)error
       if(iproc.eq.0)write(*,*)'Invalid argument=',error
       stop
     endif
     option_ID=option_ID+1
   enddo
   if(iproc.eq.0)write(*,*)''
   if(iproc.eq.0)print *,'****************************************************************************************'
 endif

 end subroutine parametri_options