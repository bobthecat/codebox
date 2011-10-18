#include "stdio.h"
#include "ctype.h"
#include "stdlib.h"
#include "string.h"
void count_both( double *orig_p, double *new_p, int *nrow, int *ncol, int *count_lesser, int *count_greater);

void count_both( double *orig_p, double *new_p, int *nrow, int *ncol, int *count_lesser, int *count_greater) {
  
  int i, j, k, shift;

  //For each column in original matrix
  for(j = 0; j < (int) *ncol; j++) {
    //For readibility, calculate shift
    shift = j * (*ncol);
    // For each element in that column
    for (i = 0; i < (int) *nrow; i++) {
      //Go through every element in the same column of original matrix 
      for(k = 0; k < (int) *nrow; k++) {
	//if new element >= other element
	if(new_p[shift + k] >= orig_p[shift + i]) {
	  count_greater[shift + i]++;
	  printf("count_greater = %d\n", count_greater[shift + i]); 
	}
	//if new element <= other element
	if(new_p[shift + k] <= orig_p[shift + i])
	  count_lesser[shift + i]++;
      }
    }
  }
}






