program TDSE

  use initialization
  use time_evolution
  use plot
  use plplot

  implicit none

  integer :: sys
  integer :: meth=2
  integer, parameter :: N=100
  complex(8) :: psi(N), psi2(N,N)
  real(8) :: pot(N), pot2(N,N)
  real(8) :: t, dt=0.01, t_final=30d0, t_init=0d0

  write(*,*) "1 = barrier, 2 = well, 3 = single slit, 4 = double slit"
  read(*,*) sys

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
     call plot_init_2D()
     do while (t<t_final)
        call time_evo_2D(N,meth,psi2,pot2,dt)
        call plot_2D(N,psi2)
        t = t+dt
     end do
  end if
  call plot_close()

end program TDSE
