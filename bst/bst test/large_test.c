#include <stdio.h>
//#include "bst.h"
//#include "timing.h"
#include "bst_test.h"


int main()
{
  int n = 10;
  int nthreads = 1;
  int threads_iterations = 16384;
  int result;
  int succesful = 0; 
  for (int i = 1; i < n+1; i++) {
    result = bst_test(nthreads*(pow(2*i, threads_iterations/(2*i));
    succesful += result;
    if ( result == 1) {
      printf("Test %d sucessful!/n", i);
    }
  }
  printf("%d of %d tests was succesful!/n", succesful, n);
}
