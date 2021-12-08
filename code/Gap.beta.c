// Output is the log likelihood from the convolution of two binomials.


# include<math.h>  /* contains log function used in ldbinom.fcn and gammln.fcn */
# include<stdio.h>  /* contains fprintf used in nrerror */

#define DIM1 32
#define DIM2 5
#define DIM3 3
#define DIM4 12


void likelihood_beta(long *data_array, double *llik_array, double *llik_sum, 
		long *n_qd, long *n_tr, long *n_yr, 
		double *Sn_array, double *Sd_array)
{
  /* data_array is c(n.qd,n.yr,3,n.tr)
     llik_array is c(n.qd,n.yr,n.tr)
     llik_array should be intialized in Splus to -Inf */

  int i,j,k,l, neginf,N0,D0,D1,iter;
  double Sn,Sd;
  int index4_fcn(int i,int j,int k, int l), index3_fcn(int i,int j,int l);
  double llik_fcn(int new0, int old0, int old1, double Sn, double Sd);

                      /* printf("%f\n",bigarray[1]);*/
  neginf=0; /* set to 1 if the nll is -Inf for any i,j,l. */
  iter=0;

for(l=0; l<*n_tr; l++)        /*for(l=0; l<*n_tr; l++)*/
    {
    for(j=0; j<*n_yr; j++)   /* for(j=0; j<*n_yr; j++) */
      {
      for (i=0; i<*n_qd; i++) 
	{
	    iter=iter+1;
	                     /*printf("%i\n",iter);
	                       printf("%i\n",data_array[0]);*/
	                     /*index1=index4_fcn(i,j,0,l);*/
	    N0=data_array[index4_fcn(i,j,0,l)];

	    D0=data_array[index4_fcn(i,j,1,l)];

	    D1=data_array[index4_fcn(i,j,2,l)];

	    Sn=Sn_array[index3_fcn(i,l,j)];

	    Sd=Sd_array[index3_fcn(i,l,j)];

	                    /* printf("%i,%i,%i,\n",N0,D0,D1);*/


	    if((N0+D0-D1)<0) 
	      {
		*llik_sum=-99e99; 
		 neginf=1;
		 llik_array[index3_fcn(i,l,j)]=-99e99;
	       /* skips llik_fnc if impossible combo of seedlings */
	      }
	    else
	      {
		llik_array[index3_fcn(i,l,j)]=llik_fcn(N0,D0,D1,Sn,Sd);

		if(neginf==0 && llik_array[index3_fcn(i,l,j)] != 9999)
		  {
		    *llik_sum=*llik_sum + llik_array[index3_fcn(i,l,j)];
	          }
	      }
	}
       }
     }
return;
}


/* llik_fcn function ----------------------------------------------- */

double llik_fcn(int N0, int D0, int D1, double Sn, double Sd)
{
  /* llik_fcn calculates the log likelihood  */

  int ns=0, max_new=0, min_new=0;
  double tmp=0, llik=0;
  double gammln(double xx);
  double bico(int n, int k);

                            /* printf("%i\n",N0);
                               printf("%i\n",D0);
                               printf("%i\n",D1); */

  if((D1-D0)>0)             /* min_new=IMAX(0,(D1-D0)); */ 
    min_new=(D1-D0);
  else
    min_new=0;


  if(N0>D1)                  /* max_new=IMIN(N0,D1); */
    max_new=D1;
  else
    max_new=N0;

                            /* printf("%d,%d\n",min_new,max_new); */


if(N0==0 && D0==0)
  llik=9999; /* 9999 needs to be converted to NA back in Splus */
else
  {
  for (ns=min_new; ns<=max_new; ns++) /* possible combinations of new seedlings */
     {

       tmp = tmp + bico(N0,ns)*pow(Sn,ns)*pow((1-Sn),(N0-ns))*
          bico(D0,(D1-ns))*pow(Sd,(D1-ns))*pow((1-Sd),(D0-D1+ns));

       /* printf("%f,%f,%f,%f,%f,%f,%f\n",tmp,bico(N0,ns),pow(Sn,ns),pow((1-Sn),
	  (N0-ns)),bico(D0,(D1-ns)),pow(Sd,(D1-ns)),pow((1-Sd),(D0-D1+ns))); */
     }
  llik=log(tmp);
                                /*printf("%f\n",llik);*/
  };
return llik;
}



/* bico function ----------------------------------------------- */

double bico(int n, int k)
{
  /* returns the binomial coefficient. */
        double factln(int n);
        return floor(0.5+exp(factln(n)-factln(k)-factln(n-k)));
}


/* factln function ----------------------------------------------- */

double factln(int n)
{
  /* returns ln(n!) */
double gammln(double xx);
void nrerror(char error_text[]);
static double a[101];

if (n < 0) nrerror("Negative factorial in routine factln");
if (n <= 1) return 0.0;
if (n <= 100) return a[n] ? a[n] : (a[n]=gammln(n+1.0)); 
else return gammln(n+1.0);
}


/* gammln function ----------------------------------------------- */

double gammln(double xx)
{
        double x,y,tmp,ser;
        static double cof[6]={76.18009172947146,-86.50532032941677,
		 24.01409824083091,-1.231739572450155,0.1208650973866179e-2,
                 -0.5395239384953e-5};
        int j;

        y=x=xx;
        tmp=x+5.5;
        tmp -= (x+0.5)*log(tmp);
        ser=1.000000000190015;
        for (j=0;j<=5;j++) ser += cof[j]/++y;                 

        return -tmp+log(2.5066282746310005*ser/x);
}



/* index3_fcn function ----------------------------------------------- */

int index3_fcn(int i,int l,int j)
{
  /* Array is DIM1 by DIM2 by DIM4 */
  int z;
  void nrerror(char error_text[]);

             /* printf("%f\n",3); */
if (i >= DIM1) nrerror("i index out of range");
if (j >= DIM2) nrerror("j index out of range");
if (l >= DIM4) nrerror("l index out of range");

z=0;
             /* printf("%d\n",z); */
z = i + l*DIM1 + j*DIM1*DIM4;
  return z;
}


/* index4_fcn function ----------------------------------------------- */

int index4_fcn(int i,int j,int k, int l)
{
  /* Array is DIM1 by DIM2 by DIM3 by DIM4 */
  int z;
  void nrerror(char error_text[]);
               /* printf("%f\n",4); */
if (i >= DIM1) nrerror("i index out of range");
if (j >= DIM2) nrerror("j index out of range");
if (k >= DIM3) nrerror("k index out of range");
if (l >= DIM4) nrerror("l index out of range");

z=0;
                /*printf("%d\n",z);*/
z = i + j*DIM1 + k*DIM1*DIM2 + l*DIM1*DIM2*DIM3;
  return z;
}



void nrerror(char error_text[])
{
     /* Numerical recipes standard error handler */
fprintf(stderr,"Numerical Recipes run-time error...\n");
fprintf(stderr,"%s\n",error_text);
fprintf(stderr,"...now exiting to system...\n");
exit(1);
}
