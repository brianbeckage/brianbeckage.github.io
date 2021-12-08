# include<math.h>  /* contains log function used in ldbinom.fcn and gammln.fcn */
# include<stdio.h>  /* contains fprintf used in nrerror */

#define DIM1 32
#define DIM2 12
#define DIM3 5


void Qfunction(long *n_array, long *N_array, long *Nstar_array, 
		double *Q_array, long *n_qd, long *n_tr, long *n_yr)
{
  /* n_array ~ obseved new seedlings c(n.qd,n.yr,3,n.tr)
     N_array ~ imputed new seedlings c(n.qd,n.yr,n.tr)
     Nstar_array ~ array of candidate N's */

  int i,j,k,z,dif,jump;
  int index_fcn(int i,int j,int k);

                          /*printf("%f\n",n_array[1]);*/
                          /*printf("%i,%i,%i\n",*n_qd, *n_tr,*n_yr);*/	      

for (k=0; k<*n_yr; k++)
    {
    for (j=0; j<*n_tr; j++) 
      {
      for (i=0; i<*n_qd; i++) 
	{

	  z=index_fcn(i,j,k);
	  dif=0;
	  dif = N_array[z]-n_array[z];
	  jump = Nstar_array[z]-N_array[z];
	                 /*printf("%i,%i\n",dif, jump);*/

	  switch(dif)
	    {
	    case 0:
	      switch(jump)
		{
		case 0:
		  Q_array[z]=log((double)5/5);
		  break;
		case 1:
		  Q_array[z]=log((double)5/6);
		               /*printf("%f\n",((double)5/6));*/
		  break;
		case 2:
		  Q_array[z]=log((double)5/7);
		  break;
		case 3:
		  Q_array[z]=log((double)5/8);
		  break;
		case 4:
		  Q_array[z]=log((double)5/9);
		  break;
		}
	      break;

	    case 1:
	      switch(jump)
		{
		case -1:
		  Q_array[z]=log((double)6/5);
		  break;
		case 0:
		  Q_array[z]=log((double)6/6);
		  break;
		case 1:
		  Q_array[z]=log((double)6/7);	
		  break;
		case 2:
		  Q_array[z]=log((double)6/8);
		  break;
		case 3:
		  Q_array[z]=log((double)6/9);
		  break;
		case 4:
		  Q_array[z]=log((double)6/9);
		  break;
		}
	      break;

	    case 2:
	      switch(jump)
		{
		case -2:
		  Q_array[z]=log((double)7/5);
		  break;
		case -1:
		  Q_array[z]=log((double)7/6);
		  break;
		case 0:
		  Q_array[z]=log((double)7/7);
		  break;
		case 1:
		  Q_array[z]=log((double)7/8);	
		  break;
		case 2:
		  Q_array[z]=log((double)7/9);
		  break;
		case 3:
		  Q_array[z]=log((double)7/9);
		  break;
		case 4:
		  Q_array[z]=log((double)7/9);
		  break;
		}
	      break;

	    case 3:
	      switch(jump)
		{
		case -3:
		  Q_array[z]=log((double)8/5);
		  break;
		case -2:
		  Q_array[z]=log((double)8/6);
		  break;
		case -1:
		  Q_array[z]=log((double)8/7);
		  break;
		case 0:
		  Q_array[z]=log((double)8/8);
		  break;
		case 1:
		  Q_array[z]=log((double)8/9);	
		  break;
		case 2:
		  Q_array[z]=log((double)8/9);
		  break;
		case 3:
		  Q_array[z]=log((double)8/9);
		  break;
		case 4:
		  Q_array[z]=log((double)8/9);
		  break;
		}
	      break;

	    case 4:
	      switch(jump)
		{
		case -4:
		  Q_array[z]=log((double)9/5);
		  break;
		case -3:
		  Q_array[z]=log((double)9/6);
		  break;
		case -2:
		  Q_array[z]=log((double)9/7);
		  break;
		case -1:
		  Q_array[z]=log((double)9/8);
		  break;
		case 0:
		  Q_array[z]=log((double)9/9);
		  break;
		case 1:
		  Q_array[z]=log((double)9/9);		  
		  break;
		case 2:
		  Q_array[z]=log((double)9/9);
		  break;
		case 3:
		  Q_array[z]=log((double)9/9);
		  break;
		case 4:
		  Q_array[z]=log((double)9/9);
		  break;
		}
	      break;

	    case 5:
	      switch(jump)
		{
		case -4:
		  Q_array[z]=log((double)9/6);
		  break;
		case -3:
		  Q_array[z]=log((double)9/7);
		  break;
		case -2:
		  Q_array[z]=log((double)9/8);
		  break;
		case -1:
		  Q_array[z]=log((double)9/9);
		  break;
		case 0:
		  Q_array[z]=log((double)9/9);
		  break;
		case 1:
		  Q_array[z]=log((double)9/9);
		  break;
		case 2:
		  Q_array[z]=log((double)9/9);
		  break;
		case 3:
		  Q_array[z]=log((double)9/9);
		  break;
		case 4:
		  Q_array[z]=log((double)9/9);
		  break;
		}
	      break;

	    case 6:
	      switch(jump)
		{
		case -4:
		  Q_array[z]=log((double)9/7);
		  break;
		case -3:
		  Q_array[z]=log((double)9/8);
		  break;
		case -2:
		  Q_array[z]=log((double)9/9);
		  break;
		case -1:
		  Q_array[z]=log((double)9/9);
		  break;
		case 0:
		  Q_array[z]=log((double)9/9);
		  break;
		case 1:
		  Q_array[z]=log((double)9/9);	
		  break;
		case 2:
		  Q_array[z]=log((double)9/9);
		  break;
		case 3:
		  Q_array[z]=log((double)9/9);
		  break;
		case 4:
		  Q_array[z]=log((double)9/9);
		  break;
		}
	      break;

	    case 7:
	      switch(jump)
		{
		case -4:
		  Q_array[z]=log((double)9/8);
		  break;
		case -3:
		  Q_array[z]=log((double)9/9);
		  break;
		case -2:
		  Q_array[z]=log((double)9/9);
		  break;
		case -1:
		  Q_array[z]=log((double)9/9);
		  break;
		case 0:
		  Q_array[z]=log((double)9/9);
		  break;
		case 1:
		  Q_array[z]=log((double)9/9);	
		  break;
		case 2:
		  Q_array[z]=log((double)9/9);
		  break;
		case 3:
		  Q_array[z]=log((double)9/9);
		  break;
		case 4:
		  Q_array[z]=log((double)9/9);
		  break;
		}
	      break;

	    default:
		  Q_array[z]=log((double)9/9);
		  break;
	    }
	}
      }
    }
return;
}



/* index_fcn function ----------------------------------------------- */

int index_fcn(int i,int j,int k)
{
  /* Array is DIM1 by DIM2 by DIM4 */
  int z;
  void nrerror(char error_text[]);

             /* printf("%f\n",3); */
if (i >= DIM1) nrerror("i index out of range");
if (j >= DIM2) nrerror("j index out of range");
if (k >= DIM3) nrerror("k index out of range");

z=0;
             /* printf("%d\n",z); */
z = i + j*DIM1 + k*DIM1*DIM2;
  return z;
}



