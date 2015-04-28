program TDSE

  use initialization
  use time_evolution
!  use plot

  implicit none
  integer :: sys=1 ! can change later for different systems. (1 = barrier, 2 = hole, 3 = single slit, 4 = double slit)
  integer :: meth=1 ! change later for different calc methods (1= RK4, 2 = Euler)
  integer, parameter :: N=30 !number of bins
  complex(8) :: psi(N)
  real(8) :: pot(N)
  real(8) :: t, timestep=0.01, t_final=1, t_init=0.1
  integer :: i


  call init_random_seed()
  call init_psi(N,psi,sys)
  call init_sys(N,pot,sys)
  open(unit = 16, file="asdf.txt")
  open(unit = 17, file="asdf2.txt")
  t = t_init
  do i = 1, N
     write(16,*) i, CDABS(psi(i))**2
  end do
  do while (t<t_final)
     call time_evo(N,meth,psi,pot,t,timestep)
     t=t+timestep
  end do
  do i = 1, N
     write(17,*) i, CDABS(psi(i))**2
  end do
end program TDSE

subroutine init_random_seed()
  implicit none

  integer, allocatable :: seed(:)
  integer :: i, n, istat, dt(8), pid, t(2), s
  integer(8) :: count, tms
  call random_seed(size = n)
  allocate(seed(n))
  open(unit=30, file="/dev/urandom", access="stream",&
       form="unformatted", action="read", status="old", &
       iostat=istat)
  if (istat == 0) then
     read(30) seed
     close(30)
  else
     call system_clock(count)
     if (count /= 0) then
        t = transfer(count, t)
     else
        call date_and_time(values=dt)
        tms = (dt(1) - 1970)*365_8 * 24 * 60 * 60 * 1000 &
             + dt(2) * 31_8 * 24 * 60 * 60 * 1000 &
             + dt(3) * 24 * 60 * 60 * 60 * 1000 &
             + dt(5) * 60 * 60 * 1000 &
             + dt(6) * 60 * 1000 + dt(7) * 1000 &
             + dt(8)
        t = transfer(tms, t)
     end if
     s = ieor(t(1), t(2))
     pid = getpid() + 1099279 ! Add a prime
     s = ieor(s, pid)
     if (n >= 3) then
        seed(1) = t(1) + 36269
        seed(2) = t(2) + 72551
        seed(3) = pid
        if (n > 3) then
           seed(4:) = s + 37 * (/ (i, i = 0, n - 4) /)
        end if
     else
        seed = s + 37 * (/ (i, i = 0, n - 1 ) /)
     end if
  end if
  call random_seed(put=seed)
end subroutine init_random_seed
