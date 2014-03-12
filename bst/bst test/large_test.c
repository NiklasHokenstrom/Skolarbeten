#include <stdio.h>
#include <math.h>
#include <stdlib.h> /* rand(), srand() */
#include <time.h>   /* time() */
//#include "bst.h"
//#include "timing.h"
#include "bst_test.h"


int main()
{
  int n = 10;
  int nthreads;// = 2;
  int threads_iterations;// = 1000;
  int result;
  int succesful = 0; 
  int seed = time(NULL);
  srand(seed);
  for (int i = 1; i < n+1; i++) {
    nthreads = (int) pow(2.0, (double) (rand() % 6));
    threads_iterations = (int) pow(100.0, (double) (rand() % 1000));
    result = bst_test(nthreads, threads_iterations);
    succesful += result;
    if ( result == 1) {
      printf("Test %d sucessful!/n", i);
    }
  }
  printf("%d of %d tests was succesful!/n", succesful, n);
}
