/*
*  AUTHORS:          Robyn Woollands (robyn.woollands@gmail.com)
*  DATE WRITTEN:     Feb 2017
*  LAST MODIFIED:    Feb 2017
*  AFFILIATION:      Department of Aerospace Engineering, Texas A&M University, College Station, TX
*  DESCRIPTION:      Header file
*/

// #ifndef __RG__
// #define __RG__

// #include <stdio.h>
// #include <math.h>
// #include <complex.h>
// #include <string.h>
// #include <stdlib.h>

#pragma once
#include <iostream> 
#include <string>
#include <cmath>
#include <complex>
#include <cstdlib>
#include "const.h"

void radial_gravity(double* X, double tol, double deg, double* grav);

double dot_product(double vector_a[], double vector_b[])

// #endif
