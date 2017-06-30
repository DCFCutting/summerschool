#include <stdio.h>
#include <time.h>
#include <omp.h>
int main(void)
{
  
  double difference;
  
  omp_lock_t writelock;
  int N=1000;
  omp_init_lock(&writelock);
  clock_t start=clock(), end;
  int var1;
  for(int i=0;i<N;i++)
    {
      var1 = 1;
      int reduc;
#pragma omp parallel firstprivate(var1) reduction(+:var1) 
      {
	
	printf("Region 1: var1=%i, \n", var1);
	for(int i=0;i<100000;i++)
	  {
	    
	    //omp_set_lock(&writelock);
	    var1++;
	    //omp_unset_lock(&writelock);
	  }
      }

      end=clock();
      difference+=(end-start)/CLOCKS_PER_SEC;
    }
  difference/=N;
  omp_destroy_lock(&writelock);
  printf("After region 1: var1=%i", var1);
  
  printf("Time taken =%G seconds \n",difference);
  return 0;
}
