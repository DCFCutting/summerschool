/* Main solver routines for heat equation solver */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <mpi.h>

#include "heat.h"

/* Exchange the boundary values */
void exchange(field *temperature, parallel_data *parallel)
{
    

  //Sendrecv method
  /*
  MPI_Sendrecv(temperature->data[1],temperature->ny,MPI_DOUBLE,parallel->nup,0,
	       temperature->data[temperature->nx+1],temperature->ny,MPI_DOUBLE,parallel->ndown,MPI_ANY_TAG,MPI_COMM_WORLD,&status);
    // Send to the down, receive from up
  MPI_Sendrecv(temperature->data[temperature->nx],temperature->ny,MPI_DOUBLE,parallel->ndown,0,
	       temperature->data[0],temperature->ny,MPI_DOUBLE,parallel->nup,MPI_ANY_TAG,MPI_COMM_WORLD,&status);
  */
  //Non-blocking send and recv
  // Send up, tag is 0

  MPI_Isend(temperature->data[1],temperature->ny,MPI_DOUBLE,parallel->nup,0,MPI_COMM_WORLD,&parallel->reqarray[sendup]);
  // Recv from down, tag is 0
  MPI_Irecv(temperature->data[temperature->nx+1],temperature->ny,MPI_DOUBLE,parallel->ndown,0,MPI_COMM_WORLD,&parallel->reqarray[recvdown]);
  //Send down, tag is 1
  MPI_Isend(temperature->data[temperature->nx],temperature->ny,MPI_DOUBLE,parallel->ndown,1,MPI_COMM_WORLD,&parallel->reqarray[senddown]);
  //Recv from up, tag is 1
  MPI_Irecv(temperature->data[0],temperature->ny,MPI_DOUBLE,parallel->nup,1,MPI_COMM_WORLD,&parallel->reqarray[recvup]);

  fflush(stdout);
  
}


/* Update the temperature values using five-point stencil */
void evolve(field *curr, field *prev, double a, double dt,parallel_data *parallel)
{
  int i, j;
  double dx2, dy2;

  
  /* Determine the temperature field at next time step
   * As we have fixed boundary conditions, the outermost gridpoints
   * are not updated. 
   * We are only updating points that do not depend on completed sends/receives.
   */
  dx2 = prev->dx * prev->dx;
  dy2 = prev->dy * prev->dy;
  for (i = 3; i < curr->nx-1; i++) {
    for (j = 1; j < curr->ny + 1; j++) {
      curr->data[i][j] = prev->data[i][j] + a * dt *
	((prev->data[i + 1][j] -
	  2.0 * prev->data[i][j] +
	  prev->data[i - 1][j]) / dx2 +
	 (prev->data[i][j + 1] -
	  2.0 * prev->data[i][j] +
	  prev->data[i][j - 1]) / dy2);
    }
  }



  // Wait for the sends and recieves to complete.
  MPI_Waitall(4,parallel->reqarray,parallel->statarray);


  // Do the border points
  int borderidx[4]={1,2,curr->nx-1,curr->nx};
  for (int i=0;i<4;i++){
    for(int j=1;j<curr->ny+1;j++){
      curr->data[borderidx[i]][j] = prev->data[borderidx[i]][j] + a * dt *
	((prev->data[borderidx[i] + 1][j] -
	  2.0 * prev->data[borderidx[i]][j] +
	  prev->data[borderidx[i] - 1][j]) / dx2 +
	 (prev->data[borderidx[i]][j + 1] -
	  2.0 * prev->data[borderidx[i]][j] +
	  prev->data[borderidx[i]][j - 1]) / dy2);
    }
  }      
  
}


