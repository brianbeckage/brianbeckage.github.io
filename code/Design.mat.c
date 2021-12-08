/* Design.mat.c:  multiplies design matrix by beta vector
 Equivalent to (in Splus):

ETAstar.new<-ETA.new<-array(NA,c(32,12,5))
for(i in 1:12){
	for(j in 1:5){ # We can only use n.yr-1 or 5 years of data
		ETAstar.new[,i,j]<-X[,i,j,]%*%Bstar[-22]
		ETA.new[,i,j]<-X[,i,j,]%*%B[-22]
		}}		# B[22] is Bd

 */

# include<stdio.h>  /* contains fprintf used in nrerror */

#define DIM1b 32
#define DIM2b 12
#define DIM3b 5
#define DIM4b 21

#define DIM1c 32
#define DIM2c 12
#define DIM3c 5
#define DIM4c 2

void designmat_fcn(double *eta_array, double *bstar, double *b, long *X_array, 
		long *n_qd, long *n_tr, long *n_yr, long *n_beta)
{
  /* eta_array is c(n.qd,n.tr,n.yr,2) or (32,12,5,2)
     X_array is c(n.qd,n.tr,n.yr,n_beta) or (32,12,5,21) excluding Bd*/

  int i,j,k,l;
  double tmp1,tmp2;
  int index4b_fcn(int i,int j,int k, int l);

                      /* printf("%f\n",bigarray[1]);*/


for(j=0; j<*n_tr; j++)        /*for(l=0; l<*n_tr; l++)*/
    {
    for(k=0; k<*n_yr; k++)   /* for(j=0; j<*n_yr; j++) */
      {
      for (i=0; i<*n_qd; i++) 
	{
	    tmp1=0.0;
	    tmp2=0.0;

	    for (l=0; l<*n_beta; l++) 
	      {
		tmp1=tmp1+X_array[index4b_fcn(i,j,k,l)]*b[l];
		tmp2=tmp2+X_array[index4b_fcn(i,j,k,l)]*bstar[l];

	      }
	    eta_array[index4c_fcn(i,j,k,0)]=tmp1; /* B */
	    eta_array[index4c_fcn(i,j,k,1)]=tmp2; /* Bstar */
	                /* printf("%i,%i,%i,\n",N0,D0,D1);*/
	}
      }
    }

return;
}




/* index4b_fcn function ----------------------------------------------- */

int index4b_fcn(int i,int j,int k, int l)
{
  /* Array is DIM1b by DIM2b by DIM3b by DIM4b */
  int z;
  void nrerror(char error_text[]);
               /* printf("%f\n",4); */
if (i >= DIM1b) nrerror("ib index out of range");
if (j >= DIM2b) nrerror("jb index out of range");
if (k >= DIM3b) nrerror("kb index out of range");
if (l >= DIM4b) nrerror("lb index out of range");

z=0;
                /*printf("%d\n",z);*/
z = i + j*DIM1b + k*DIM1b*DIM2b + l*DIM1b*DIM2b*DIM3b;
  return z;
}

/* index4c_fcn function ----------------------------------------------- */

int index4c_fcn(int i,int j,int k, int l)
{
  /* Array is DIM1c by DIM2c by DIM3c by DIM4c */
  int z;
  void nrerror(char error_text[]);
               /* printf("%f\n",4); */
if (i >= DIM1c) nrerror("ic index out of range");
if (j >= DIM2c) nrerror("jc index out of range");
if (k >= DIM3c) nrerror("kc index out of range");
if (l >= DIM4c) nrerror("lc index out of range");

z=0;
                /*printf("%d\n",z);*/
z = i + j*DIM1c + k*DIM1c*DIM2c + l*DIM1c*DIM2c*DIM3c;
  return z;
}


