 subroutine changed_parameters(iproc,numarg,OUT_DIR,OUT_MODEL,OUT_PHASE,t_initial,t_final,Cn,time_step,max_time_step,power_law)

 Implicit none

 logical :: err_detected
 integer :: option_ID
 character(len=150) :: arg,error
 integer, intent(in) :: iproc
 integer, intent(out) :: numarg
 logical, intent(inout) :: power_law
 character(len=*), intent(inout) :: OUT_DIR,OUT_MODEL,OUT_PHASE
 double precision, intent(inout) :: t_initial,t_final,Cn,time_step,max_time_step

 option_ID=1
 err_detected=.false.
 numarg=command_argument_count()
 
 if(numarg.gt.0)then
   if(iproc.eq.0)then
     write(*,*)''
     write(*,*)INT(numarg/2),'parameters changed with respect to parametri.f90'
     write(*,*)''
     open(unit=1,file='changed_parameters.txt',status='unknown')
     write(1,*)INT(numarg/2),'parameters changed with respect to parametri.f90'
     write(1,*)''
   endif
   do while(option_ID.le.numarg)
     call getarg(option_ID,arg)
     if(arg.eq.'-t_initial')then
       option_ID=option_ID+1
       call getarg(option_ID,arg)
       read(arg,*)t_initial
       if(iproc.eq.0)then
         print *,'Running FALCON with initial time (Ma):',t_initial/1.D06
         write(1,*)'Running FALCON with initial time (Ma):',t_initial/1.D06
       endif
     elseif(arg.eq.'-t_final')then
        option_ID=option_ID+1
        call getarg(option_ID,arg)
        read(arg,*)t_final
        if(iproc.eq.0)then
          print *,'Running FALCON with final time (Ma):',t_final/1.D06
          write(1,*)'Running FALCON with final time (Ma):',t_final/1.D06
        endif
     elseif(arg.eq.'-Cn')then
        option_ID=option_ID+1
        call getarg(option_ID,arg)
        read(arg,*)Cn
        if(iproc.eq.0)then
          print *,'Running FALCON with Courant number:',Cn
          write(1,*)'Running FALCON with Courant number:',Cn
        endif
     elseif(arg.eq.'-time_step')then
        option_ID=option_ID+1
        call getarg(option_ID,arg)
        read(arg,*)t_initial
        if(iproc.eq.0)then
          print *,'Running FALCON with time step (Ma)',time_step/1.D06
          write(1,*)'Running FALCON with time step (Ma):',time_step/1.D06
        endif
     elseif(arg.eq.'-max_time_step')then
        option_ID=option_ID+1
        call getarg(option_ID,arg)
        read(arg,*)t_initial
        if(iproc.eq.0)then
          print *,'Running FALCON with maximum time step (Ma):',max_time_step/1.D06
          write(1,*)'Running FALCON with maximum time step (Ma):',max_time_step/1.D06
        endif
     elseif(arg.eq.'-OUT_DIR')then
       option_ID=option_ID+1
       call getarg(option_ID,arg)
       read(arg,*)OUT_DIR
       if(iproc.eq.0)then
         print *,'Running FALCON writing files in the main directory: ',OUT_DIR
         write(1,*)'Running FALCON writing files in the main directory: ',OUT_DIR
       endif
     elseif(arg.eq.'-OUT_MODEL')then
       option_ID=option_ID+1
       call getarg(option_ID,arg)
       read(arg,*)OUT_MODEL
       if(iproc.eq.0)then
         print *,'Running FALCON writing files in the model directory: ',OUT_MODEL
         write(1,*)'Running FALCON writing files in the model directory: ',OUT_MODEL
       endif
     elseif(arg.eq.'-OUT_PHASE')then
       option_ID=option_ID+1
       call getarg(option_ID,arg)
       read(arg,*)OUT_PHASE
       if(iproc.eq.0)then
         print *,'Running FALCON writing files in the phase directory: ',OUT_PHASE
         write(1,*)'Running FALCON writing files in the phase directory: ',OUT_PHASE
       endif
     elseif(arg.eq.'-power_law')then
       option_ID=option_ID+1
       call getarg(option_ID,arg)
       read(arg,*)power_law
       if(iproc.eq.0)then
         if(power_law)then
           print *,'Running FALCON with power law'
           write(1,*)'Running FALCON with power law'
         else
           print *,'Running FALCON without power law'
           write(1,*)'Running FALCON without power law'
         endif
       endif
     else
       call getarg(option_ID,arg)
       read(arg,*)error
       if(iproc.eq.0)print *,'Invalid argument=',error
       stop
     endif
     option_ID=option_ID+1
   enddo
   if(iproc.eq.0)then
     print *,''
     print *,'****************************************************************************************'
     close(1)
   endif
 endif

 end subroutine changed_parameters