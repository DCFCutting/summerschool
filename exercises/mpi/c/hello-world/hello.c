#include <stdio.h>
#include <mpi.h>
#include <stdlib.h>
#include <omp.h>

int main(int argc, char *argv[])
{
  printf("Hello world!\n");
  int size,required,provided;
  int rank;
  MPI_Comm comm=MPI_COMM_WORLD;
  
  required=MPI_THREAD_MULTIPLE;
  MPI_Init_thread(&argc,&argv,required,&provided);
  if(rank==0){
    printf("Asked for support %i and recieved support %i \n",required,provided);
  }
  MPI_Comm_size(comm,&size);
  
  
  MPI_Comm_rank(comm, &rank);
  fflush(stdout);
  
  if(rank==0){
    printf("There are %i processes in total. \n", size);
  }
#pragma omp parallel
  {
    int tid,tnum;
    tid=omp_get_thread_num();
    tnum=omp_get_num_threads();
    printf("I am rank %i of %i, and thread %i of %i \n",rank,size-1,tid,tnum);  
  }
  MPI_Finalize();
  return 0;
}
