
//C++ std libs
#include <cmath>
#include <complex>
//from adaptive_picard_chebyshev_v4
#include "ecef2eci.h"
#include "eci2ecef.h"
#include "const.h"
#include "MGM120.cc"
#include "MGM120.h"



double P [(Max_Degree+3)*(Max_Degree+3)] = {0.0};
double scaleFactor [(Max_Degree+3)*(Max_Degree+3)] = {0.0};



void matmulMGM(double *A, double *B, double *OUT, int m, int n, int q) 
    /*!
    * \brief Matrix Multiplication
    * This is a simple matrix multiplication function
    *
    * \param[in] A Vector representation of matrix A
    * \param[in] B Vector representation of matrix B
    * \param[in] m Column dimension of A
    * \param[in] n Shared dimension of A and B
    * \param[in] q Row dimension of B
    * \param[out] OUT Matrix Output
    */
   {
	for(int i=0;i<m; i++){
		for(int j=0;j<q;j++){
			double sum = {0.0};
			for(int j1=0;j1<n;j1++)
			sum += A[IDX2F(i+1,j1+1,m)]*B[IDX2F(j1+1,j+1,n)];
			OUT[IDX2F(i+1,j+1,m)] = sum;
		}
	}
}


/*!
 * \brief Gravity Evaluation
 * This is the function that evaluates the spherical harmonic
 * series to provide acceloration
 *
 * \param[in] p 3-element position in ECEF
 * \param[in] Deg Degree and order of the series to be used
 * \param[in] delV_del_r_phi_lambda 9-element vector of gravity partials used to compute Jacobian fo$
 * \param[out] Gxyz Gravitational Acceloration Output
 */

void MGM( double* p, double* Gxyz, int DEG)
{

	double r             = {0.0};
	double phic          = {0.0};
	double lambda        = {0.0};
	double slambda       = {0.0};
	double clambda       = {0.0};
	double x = {0.0};
	double y = {0.0};
	double z = {0.0};
	double smlambda[Max_Degree+1] = {0.0};
	double cmlambda[Max_Degree+1] = {0.0};
	//int DEG = 3;
	// determine radius of this thread's node
	x = p[0];
	y = p[1];
	z = p[2];

	// Compute geocentric radius
	r = pow( x*x + y*y + z*z , 0.5 );

	// Compute geocentric latitude
	phic  = asin( z / r );
	// Compute lambda
	lambda  = atan2( y, x );
	while (lambda<0)
	lambda = lambda+2*C_PI;
	while (lambda>=2*C_PI)
	lambda = lambda-2*C_PI;


	slambda = sin(lambda);
	clambda = cos(lambda);
	smlambda[0] = 0.0;
	cmlambda[0] = 1.0;
	smlambda[1] = slambda;
	cmlambda[1] = clambda;

	for(int m=2; m < DEG + 1; m++){
		smlambda[m] = 2.0*clambda*smlambda[m-1] - smlambda[m-2];
		cmlambda[m] = 2.0*clambda*cmlambda[m-1] - cmlambda[m-2];
	}


	loc_gravLegendre( phic, scaleFactor, P , DEG);

	loc_gravityPCPF( p, P, DEG, smlambda, cmlambda, r, scaleFactor, Gxyz );


}

void MGMPot( double* p, double* Pot, int DEG)
{
	// determine radius of this thread's node
	double r             = {0.0};
	double phic          = {0.0};
	double lambda        = {0.0};
	double slambda       = {0.0};
	double clambda       = {0.0};
	double smlambda[Max_Degree+1] = {0.0};
	double cmlambda[Max_Degree+1] = {0.0};

	double x = p[0];
	double y = p[1];
	double z = p[2];
	int m;
	double P [(Max_Degree+3)*(Max_Degree+3)] = {0.0};
	double scaleFactor [(Max_Degree+3)*(Max_Degree+3)] = {0.0};

	// Compute geocentric radius
	r = pow( x*x + y*y + z*z , 0.5 );
	// Compute geocentric latitude
	phic  = asin( z / r );
	// Compute lambda
	lambda  = atan2( y, x );
	while (lambda<0){
		lambda = lambda+2*C_PI;
	}
	while (lambda>=2*C_PI){
		lambda = lambda-2*C_PI;
	}

	slambda = sin(lambda);
	clambda = cos(lambda);
	smlambda[0] = 0.0;
	cmlambda[0] = 1.0;
	smlambda[1] = slambda;
	cmlambda[1] = clambda;


	for(m=2;m<DEG+1;m++){
		smlambda[m] = 2.0*clambda*smlambda[m-1] - smlambda[m-2];
		cmlambda[m] = 2.0*clambda*cmlambda[m-1] - cmlambda[m-2];
	}
	// Compute normalized associated legendre polynomials

	loc_gravLegendre( phic, scaleFactor, P , DEG);

	loc_gravityPot( p, P, DEG, smlambda, cmlambda, r, scaleFactor, Pot );


}


void loc_gravLegendre( double phi, double* scaleFactor, double* P, int DEG )
{

	int k, p;
	double cphi = {0.0};
	double sphi = {0.0};
	cphi = cos(0.5*C_PI - phi);
	sphi = sin(0.5*C_PI - phi);
	// Seeds for recursion formula
	P[IDX2F(1,1,Max_Degree+3)] = 1.0;            // n = 0, m = 0;
	scaleFactor[IDX2F(1,1, Max_Degree+3)] = 0.0;
	P[IDX2F(2,1, Max_Degree+3)] = sqrt(3.0)*cphi ; // n = 1, m = 0;
	scaleFactor[IDX2F(2,1,Max_Degree+3)]  = 1.0;
	P[IDX2F(2,2,Max_Degree+3)] = sqrt(3.0)*sphi; // n = 1, m = 1;
	scaleFactor[IDX2F(2,2,Max_Degree+3)] = 0.0;
    for (int nn = 2; nn <= DEG+2;nn++){
		double n = (double)nn;
		k = nn + 1;
		for(int mm=0; mm<=n;mm++) {
			double m = (double)mm;
			p = mm + 1;
			// Compute normalized associated legendre polynomials, P, via recursion relations
			// Scale Factor needed for normalization of dUdphi partial derivative
			if (n == m){
				P[IDX2F(k,k,Max_Degree+3)] = sqrt(2*n+1.0)/sqrt(2.0*n)*sphi*P[IDX2F(k-1,k-1, Max_Degree+3)];
				scaleFactor[IDX2F(k,k,Max_Degree+3)] = 0.0;
			}
			else if (m == 0){
				P[IDX2F(k,p,Max_Degree+3)] = (sqrt(2*n+1)/n)*(sqrt(2*n-1.0)*cphi*P[IDX2F(k-1,p,Max_Degree+3)] - (n-1)/sqrt(2*n-3)* P[IDX2F(k-2,p, Max_Degree+3)] );
				scaleFactor[IDX2F(k,p,Max_Degree+3)] = sqrt( (n+1)*(n)/2);
			}
			else {
				P[IDX2F(k,p,Max_Degree+3)] = sqrt(2*n+1)/(sqrt(n+m)*sqrt(n-m))*(sqrt(2*n-1.0)*cphi*P[IDX2F(k-1,p,Max_Degree+3)] - sqrt(n+m-1.0)*sqrt(n-m-1)/sqrt(2*n-3)*P[IDX2F(k-2,p,Max_Degree+3)] );
				scaleFactor[IDX2F(k,p,Max_Degree+3)] = sqrt( (n+m+1)*(n-m));
			}
		}
	}
}



void loc_gravityPCPF( double* p, double* P, int DEG, double* smlambda, double* cmlambda, double r, double* scaleFactor, double* Gxyz )
{

	int k, j;
	double radu;
	double rRatio  = {0.0};
	double rRatio_n  = {0.0};

	// initialize summation of gravity in radial coordinates
	double dUdrSumN   = {1.0};
	double dUdphiSumN    = {0.0};
	double dUdlambdaSumN = {0.0};

	double dUdrSumM   = {0.0};
	double dUdphiSumM = {0.0};
	double dUdlambdaSumM = {0.0};

	double dUdr    = {0.0};
	double dUdphi  = {0.0};
	double dUdlambda = {0.0};



	double x = p[0];
	double y = p[1];
	double z = p[2];
	radu = r;
	rRatio = C_Req/radu;
	rRatio_n = rRatio;

    for (int n = 2; n <= DEG; n++) {
		k = n+1;
		rRatio_n = rRatio_n*rRatio;
		dUdrSumM      = 0.0;
		dUdphiSumM    = 0.0;
		dUdlambdaSumM = 0.0;
		for (int m = 0; m <= n; m++){
			j = m+1;
			dUdrSumM      = dUdrSumM + P[IDX2F(k,j,Max_Degree+3)] *(C[IDX2F(k,j,Max_Degree)]*cmlambda[j-1] + S[IDX2F(k,j,Max_Degree)]*smlambda[j-1]);
			dUdphiSumM    = dUdphiSumM + ((P[IDX2F(k,j+1,Max_Degree+3)]*scaleFactor[IDX2F(k,j,Max_Degree+3)]) - z/(sqrt(x*x + y*y))*m*P[IDX2F(k,j,Max_Degree+3)])*(C[IDX2F(k,j,Max_Degree)]*cmlambda[j-1] + S[IDX2F(k,j,Max_Degree)]*smlambda[j-1]);
			dUdlambdaSumM = dUdlambdaSumM + m*P[IDX2F(k,j,Max_Degree+3)]*(S[IDX2F(k,j,Max_Degree)]*cmlambda[j-1] - C[IDX2F(k,j,Max_Degree)]*smlambda[j-1]);
		}
		dUdrSumN      = dUdrSumN      + dUdrSumM*rRatio_n*k;
		dUdphiSumN    = dUdphiSumN    + dUdphiSumM*rRatio_n;
		dUdlambdaSumN = dUdlambdaSumN + dUdlambdaSumM*rRatio_n;
	}

	// gravity in spherical coordinates
	dUdr      = -C_MU/(radu*radu)*dUdrSumN ;
	dUdphi    =  C_MU/radu*dUdphiSumN ;
	dUdlambda =  C_MU/radu*dUdlambdaSumN ;

	//gravity in ECEF coordinates
	Gxyz[0] = ((1.0/radu)*dUdr - (z/(radu*radu*sqrt(x*x + y*y)))*dUdphi)*x - (dUdlambda/(x*x + y*y))*y;
	Gxyz[1] = ((1.0/radu)*dUdr - (z/(radu*radu*sqrt(x*x + y*y)))*dUdphi)*y + (dUdlambda/(x*x + y*y))*x;
	Gxyz[2] = (1.0/radu)*dUdr*z + ((sqrt(x*x + y*y))/(radu*radu))*dUdphi;
}

void loc_gravityPot( double* p, double* P, int DEG, double* smlambda, double* cmlambda, double r, double* scaleFactor, double* Pot )
{

	int k, j;
	double radu;
	double rRatio  = {0.0};
	double rRatio_n  = {0.0};

	// initialize summation of gravity in radial coordinates
	double dUdrSumN   = {1.0};
	double dUdphiSumN    = {0.0};
	double dUdlambdaSumN = {0.0};

	double dUdrSumM   = {0.0};
	double dUdphiSumM = {0.0};
	double dUdlambdaSumM = {0.0};

	double dUdr  = {0.0};
	double dUdphi  = {0.0};
	double dUdlambda = {0.0};
	double USum = {0.0};


	double x = p[0];
	double y = p[1];
	double z = p[2];
	int n;
	int m;

	radu = r;
	rRatio = C_Req/radu;
	rRatio_n = rRatio;
	// summation of gravity in radial coordinates

	for (n = 2; n <= DEG; n++) {
		k = n+1;
		rRatio_n = rRatio_n*rRatio;
		dUdrSumM      = 0.0;
		dUdphiSumM    = 0.0;
		dUdlambdaSumM = 0.0;
		for (m = 0; m <= n; m++){
			j = m+1;
			dUdrSumM      = dUdrSumM + P[IDX2F(k,j,Max_Degree+3)] *(C[IDX2F(k,j,Max_Degree)]*cmlambda[j-1] + S[IDX2F(k,j,Max_Degree)]*smlambda[j-1]);
			dUdphiSumM    = dUdphiSumM + ((P[IDX2F(k,j+1,Max_Degree+3)]*scaleFactor[IDX2F(k,j,Max_Degree+3)]) - z/(sqrt(x*x + y*y))*m*P[IDX2F(k,j,Max_Degree+3)])*(C[IDX2F(k,j,Max_Degree)]*cmlambda[j-1] + S[IDX2F(k,j,Max_Degree)]*smlambda[j-1]);
			dUdlambdaSumM = dUdlambdaSumM + m*P[IDX2F(k,j,Max_Degree+3)]*(S[IDX2F(k,j,Max_Degree)]*cmlambda[j-1] - C[IDX2F(k,j,Max_Degree)]*smlambda[j-1]);
			// printf("UsumM: %e\n ",dUdrSumM);
		}
		dUdrSumN      = dUdrSumN      + dUdrSumM*rRatio_n;
		dUdphiSumN    = dUdphiSumN    + dUdphiSumM*rRatio_n;
		dUdlambdaSumN = dUdlambdaSumN + dUdlambdaSumM*rRatio_n;
	}
	// gravity in spherical coordinates
	dUdr      =  C_MU/(radu)*dUdrSumN ;
	dUdphi    =  C_MU/radu*dUdphiSumN ;
	dUdlambda =  C_MU/radu*dUdlambdaSumN ;

	// gravity in potential
	// *Pot = MU * sqrt(dUdr*dUdr + dUdphi*dUdphi + dUdlambda*dUdlambda)/radu;
	*Pot =  sqrt(dUdr*dUdr);


}

/*!
* \brief Jacobi Integral
* This is the function computes the Jacobi Integral based on position and
* state vector
*
* This is a usefule tool for checking the accuracy of conservitive
* orbit propigation
*
* \param[in] solN State (position and velocity) vector in ECEF
* \param[in] Deg Degree and order of the serries to be used
* \param[out] H Jacobi Integral Output
*/
void jacobiIntegral(double t, double* solN, double* H, int Deg){

	double xI[3]    = {0.0};
	double vI[3]    = {0.0};
	double xECEF[3] = {0.0};
	double vECEF[3] = {0.0};

	xI[0] = solN[0];
	xI[1] = solN[1];
	xI[2] = solN[2];
	vI[0] = solN[3];
	vI[1] = solN[4];
	vI[2] = solN[5];

	// Convert from ECI to ECEF
	eci2ecef(t,xI,vI,xECEF,vECEF);

	solN[0] = xECEF[0];
	solN[1] = xECEF[1];
	solN[2] = xECEF[2];
	solN[3] = vECEF[0];
	solN[4] = vECEF[1];
	solN[5] = vECEF[2];

	double KE,PE,RotTerm;

	KE = 0.5*(solN[3]*solN[3] + solN[4]*solN[4] + solN[5]*solN[5]);
	MGMPot(solN, &PE, Deg);
	PE = -PE;
	RotTerm = 0.5*C_omega*C_omega*(solN[0]*solN[0] + solN[1]*solN[1]);
	*H  = PE + KE - RotTerm; // Hamiltonian

	// printf("KE: %e\tPE: %e\tRT: %e\tSum: %e\n ",KE,PE,RotTerm,*H);
	// getchar();

}