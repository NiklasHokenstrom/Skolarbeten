
CFLAGS=-pthread -Wall -Wextra -std=c99 -D_XOPEN_SOURCE=600 -ggdball: large_test
CC=gcc

all: large_test

#large_test: large_testo

#large_test
large_test: large_test.o bst_test.o bst.o timing.o

large_test.o: large_test.c
	$(CC) -c - $(CFLAGS) large_test.c -o large_test.o -lm

bst_test.o: bst_test.c
	$(CC) -c - $(CFLAGS) bst_test.c -o bst_test.o -lm
bst.o: bst.c
	$(CC) -c - $(CFLAGS) bst.c -o bst.o -lm
timing.o: timing.c
	$(CC) -c - $(CFLAGS) timing.c -o timing.o -lm

clean:
	$(RM) *.o *.d *~
