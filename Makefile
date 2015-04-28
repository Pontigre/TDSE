FC = gfortran
FFLAGS = -Wall -Wextra -O3 -march=native -ffast-math #-std=f95
LDFLAGS =

FFLAGS += $(shell pkg-config --cflags plplotd-f95)
LIBS += $(shell pkg-config --libs plplotd-f95)

COMPILE = $(FC) $(FFLAGS)
LINK = $(FC) $(LDFLAGS)

OBJS =
OBJS += plot.o
OBJS += initialization.o
OBJS += time_evolution.o
OBJS += myprog.o
all: myprog

myprog: $(OBJS)
	$(LINK) -o $@ $^ $(LIBS)

%.o: %.f90
	$(COMPILE) -o $@ -c $<

.PHONY: clean
clean:
	$(RM) myprog $(OBJS) *.mod *.txt *.f90~ *.f90# Makefile~
