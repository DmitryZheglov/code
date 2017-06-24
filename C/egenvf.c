#include <stdio.h>
#include <stdlib.h>
#include <math.h>

double Ay(int N,int m, int k,double h);
double egenf(int N,  int m, int k);
//double * egenv( int N);
double egenv(int N, int m, double h);
double max(int N, int m, double h);
double norm(int N);
double normprod(int N);
double maxprodij(int N, int i, int j);
double Ay( int N, int m, int k,double h){
	double ay;
	if((k>N-1)||(k<1) || (m > N-1) ||(m < 1)){
		printf("%s","error size ay");
		return -1;
	}
	ay = (egenf(N , m, k+1) - 2.*egenf(N , m, k) + egenf(N , m, k-1))*(1./(h*h));

	return ay;
}

double egenf(int N,  int m, int k){
	double z;
	if((k > N) || (k < 0) || (m < 1) ||(m > N-1)){
		printf("%s","error size vector(m or k) egenf");
		return -1;
	}
	if(k==0){
		z = -sin(M_PI*(double)m*(0.5)/(double)(N-1));
		//printf("%s %lf\n","f",z);
		return z;
	}
	else if(k==N){
		z = -sin(M_PI*(double)m*((double)N-1.5)/(double)(N-1));
		//printf("%s %lf\n","f",z);
		return z;
	}
	else{
		z = sin(M_PI*(double)m*((double)k-0.5)/(double)(N-1));
		//printf("%s %lf\n","f",z);
		return z;
	}
	
}
/*
double * egenv( int N){
	double *b;
	b=(double*)malloc(sizeof(double)*N);
	int i;
	for (i=0;i<N;i++){
		b[i] = (double)(i);
	}
	return b;
}
*/
double egenv(int N, int m, double h){
  double za;
  double s;
  if((m>N-1)||(m<1)){
    //printf("%s","error m egenv");
    return -1;
  }
  s = sin((M_PI*(double)m)/(double)(2*(N-1)));
  za = (4.*s*s)/(h*h);
  //za = 2.*(1.-cos((M_PI*(double)m)/(double)(N-0.5)));
  //za = za*h*h;
  //za = h*h*sin((M_PI*(double)m)/(double)(2*N-1))*sin((M_PI*(double)m)/(double)(2*N-1));
  //printf("%s %lf\n","v",za);
  return za;
}
double max(int N, int m, double h){
	double maks=0;
	int k;
	double dif;
	for(k=1;k<N;k++){
		dif = fabs(Ay(N,m,k,h)*(1./egenv(N,m,h))+egenf(N,m,k));
		//printf("%d %s %e %e %e\n",k,"max",Ay(N,m,k),egenv(N,m,h)*egenf(N,m,k)*h*h,dif);
		if (dif>maks){
			maks = dif;
		}
	}
	return maks;
}
/*
double norm(int N,double *mas){
	double norma = 0;
	int t = 0;
	int k;
	for(k=1;k<N+1;k++){
		if (max(N,k,mas[k])>norma){
			norma =max(N,k,mas[k]);
			t=k;
		}
	}
	printf("%s %d %s %e\n","k:",t,"norma:",norma);

	
}
*/

double norm(int N){
	double norma = 0;
	double max_m;
	int t = 0;
	int m;
	double h;//=(double)(N-1);
	h = 1./((double)(N-1));
	for(m=1;m<N;m++){
		max_m = max(N,m,h);
		if (max_m>norma){
			norma =max_m;
			t=m;
		}
	}
	printf("%s %d %s %e\n","m:",t,"norma:",norma);
	return 1;
	
}

double maxprodij(int N,int i,int j){
	double sum = 0;
	int k;
	if((i==j)&&(j==N-1)){
		for(k=1;k<N;k++){
			sum = sum +(egenf( N, i, k)*egenf(N,j, k))/(double)(N-1);
		}
	}
	else{
		for(k=1;k<N;k++){
		sum = sum +2*(egenf( N, i, k)*egenf(N,j, k))/(double)(N-1);
		}
	}
}



double normprod(int N){
	double max = 0;
	double dif ;
	int j;
	int k;
	int ti;
	int tj;
	int i;
	double z;
	for(i=1;i<N;i++){
		for(j=1;j<=i;j++){
			if(i==j){
				dif = fabs(1.-maxprodij(N,i,j));
				if(max<dif){
					max = dif;
					ti = i;
					tj = j;
				}
			}
			else{
				dif = maxprodij(N,i,j);
				if(max<dif){
					max = dif;
					ti = i;
					tj = j;
				}
			}
		}
	}
	printf("%e %d %d\n",max,ti,tj);
	return 1;
	
}
int main(void){
	//double *mas;
	int N;
	//int i;
	scanf("%d",&N);
	if(N == 1){
		printf("%s","error_size_matrix");
		return -1;
	}
	/*mas = (double *)malloc(N*sizeof(double));
	if(mas == NULL){
		printf("%s","memory error");
	}
	mas = egenv(N);
	for(i=;i<N;i++){
		printf("%e\n",mas[i]);
	}
	*/
	//printf("%e \n",maxprodij(N,99,99));
	//printf("%e \n",maxprodij(N,99,98));
	norm(N);
	normprod(N);
}
