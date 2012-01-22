CC=gcc
HC=ghc

FILES = Fac.hs Array.hs

all : $(FILES:.hs=.c-run)

# Stop make from removing intermediate files
.SECONDARY:

%.c-run : %.c
	$(CC) $< -o $@

%.c : %.hs-run
	./$<

%.hs-run: %.hs
	$(HC) --make $< -o $@

clean:
	$(RM) *.o *.hi *.c-run *.hs-run $(FILES:.hs=.c)
