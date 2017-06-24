#include <stdio.h>
#include <stdlib.h>
#include <math.h>

double xk( double k, double h);
double y( double x);
double f(double x);
double scald(int N, int m, double h);
double egenf(int N,  int m, int k);
double egenv(int N, int m, double h);

double xk( double k, double h){
	return h*k;
}

double y(double x){
	return exp(x)*(x*x*x*x-2.*x*x*x+x*x);   
}

double f(double x){
    	//for p=1
    	return exp(x)*(-8.*x*x*x+8.*x-2.);
	
}

double egenf(int N,  int m, int k){
	double z;
	if((k > N) || (k < 0) || (m < 1) ||(m > N-1)){
		printf("%s","error size vector(m or k) egenf");
		return -1;
	}
	if(m==1){
		z = cos(M_PI*((double)m-1.)*((double)k-0.5)/((double)N-1.));
		//printf("%s %lf\n","f",z);
		return z;
	}
	else{
		z = sqrt(2.)*cos(M_PI*((double)m-1.)*((double)k-0.5)/((double)N-1.));
		return z;
	}
}
double egenv(int N, int m, double h){
  double za;
  double s;
  if((m>N-1)||(m<1)){
    printf("%s\n","er");
    return -1;
  }
  s = sin((M_PI*((double)m-1.))/(2.*((double)N-1.)));
  za = (4.*s*s)/(h*h);
  return za;
}

double scald(int N, int m, double h){
	int i;
	double scal=0;
	for(i=0;i<N+1;i++){//!!!!!
		scal = scal + f(xk((double)i+0.5,h))*egenf(N,m,i)*h;
	}
	return scal;
}

int main(void){
	double *yk,*c;
	double h,norm=0;
	int N,i,m;
	scanf("%d",&N);
	if(N == 1){
		printf("%s","error_size_matrix");
		return -1;
	}
        h = 1./((double)N-1.);
	yk = (double*)malloc(sizeof(double)*(N+1));
	c = (double*)malloc(sizeof(double)*(N+1));
	if(yk == NULL){
		printf("%s","memory error");
	}
	for(i=0;i<N+1;i++){
		yk[i]=0;
	}
	for(m=1;m<N;m++){
		c[m] = scald(N,m,h)/(egenv(N,m,h)+1.);//p=0!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
	}
	for(i=0;i<N+1;i++){
		for(m=1;m<N;m++){
			yk[i] = yk[i]+c[m]*egenf(N,m,i);
		}
	}
	for(i=1;i<N;i++){
		printf("%e %e %25.12lf %e\n",xk((double)i+0.5,h),y(xk((double)i+0.5,h)),yk[i],(y(xk((double)i+0.5,h))-yk[i])/(h*h));
	}
	for(i=0;i<N+1;i++){
		yk[i] = yk[i] -  y(xk((double)i+0.5,h));
	}
	for(i=1;i<N;i++){//!!!
		norm = norm + yk[i]*yk[i]*h;
	}
	printf("Deflection:%20.18lf \n",sqrt(norm)/(h*h));
	norm = 0.;
	for(i=1;i<N;i++){//!!!
	    if (fabs(yk[i]) > norm){
	      norm = fabs(yk[i]);
	    }
	}
	printf("%e\n",norm/(h*h));
	return 0;
}

