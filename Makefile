CC ?= gcc
CFLAGS ?= -O2 -Wall -Wextra
cArt: art.c fb.c
	$(CC) $(CFLAGS) -o cArt art.c fb.c -lm
clean:
	rm -f cArt
