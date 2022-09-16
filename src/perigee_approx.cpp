/*
*  AUTHORS:          Robyn Woollands (robyn.woollands@gmail.com)
*  DATE WRITTEN:     May 2017
*  LAST MODIFIED:    May 2017
*  AFFILIATION:      Department of Aerospace Engineering, Texas A&M University, College Station, TX
*  DESCRIPTION:      Determine approximate progee passage
*
* INPUT:
*    X     -- States (Position and Velocity)
*    normX -- Position magnitude
*    times -- Times corresponding to states X
*    n     -- # of elements in norm X
*
* OUTPUTS:
*    rp    -- State at perigee (km)
*    v     -- Velocity at perigee (km/s)
*    tp    -- Time of perigee passage (s)
*    ind   -- Index in array corresponding to perigee
*/

//from C++ std libs
// #include <math.h>
// #include <stdio.h>
// #include <string.h>
// #include <stdlib.h>
#include <iostream> 
#include <string>
#include <cmath>
#include <complex>
#include <cstdlib>
#include <cstring>

//from adaptive_picard_chebyshev_v4
#include "perigee_approx.h"
#include "c_functions.h"

void perigee_approx( double* X, double* normX, double* times, int n, double* rp, double* vp, double* tp, int* ind ){

  // Minimum radius
  double min;
  Cmin(normX,n,&min);
  int *in;
  in = ind;
  // printf("n %i\n", n);
  // Index corresponding to minimum radius
  double tmp;
  for (int i=0; i <= n; i++){
    tmp = fabs(normX[i] - min);
    if (tmp < 1e-10){
      *in = i;
      break;
    }
  }

  // State and times at Keplerian perigee
  for (int i=0; i<=2; i++){
    rp[i] = X[ID2(*in+1,i+1,n+1)];
    vp[i] = X[ID2(*in+1,i+4,n+1)];
  }
  *tp    = times[*in];
  // printf("ind %i\n", in);
}
