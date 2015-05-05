program TDSE

  use initialization
  use time_evolution
  use plot
  use plplot

  implicit none

  integer :: sys
  integer :: meth
  integer, parameter :: N=100
  complex(8) :: psi(N), psi2(N,N)
  real(8) :: pot(N), pot2(N,N)
  real(8) :: t, dt=0.01, t_final, t_init=0d0
  integer :: x,y

  write(*,*) "1 = barrier, 2 = well, 3 = single slit, 4 = double slit"
  read(*,*) sys
  write(*,*) "1 = Euler, 2 = RK4"
  read(*,*) meth
  write(*,*) "Time"
  read(*,*) t_final
  t = t_init
  call init_psi(N,psi,psi2,sys)
  call init_sys(N,pot,pot2,sys)

  if (sys == 1 .or. sys == 2) then
     call plot_init()
     do while (t<t_final)
        call time_evo_1D(N,meth,psi,pot,dt)
        call plot_1D(N, psi, pot)
        t=t+dt
     end do
  else
     do while (t<t_final)
        call time_evo_2D(N,meth,psi2,pot2,dt)
       ! do x= 1, N
       !    do y =1, N
       !       if (CDABS(psi2(x,y))**2 > 100 .or. CDABS(psi2(x,y))**2 <-10) then
       !          write(*,*) "OUT OF BOUNDS", CDABS(psi2(x,y))**2
       !       end if
       !    end do
       ! end do
        t = t+dt
     end do
     call plot_init_2D()
     call plot_2D(N,psi2)
  end if
  call plot_close()

end program TDSE
