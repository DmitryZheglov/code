#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define EPS 1e-5

//TASK y'(x) = f(x,y(x))
//[0,1]->(x0=0,x1=x0+h,...,xN)
//Method Runge Kytta(k1,k2,k3) O(h^3)

double xk( int k, double h);
void y(double *out,double x);
void f(double *out,double x,double *in,double h);
void RK(double *yn, int k, double h,int j);

double xk( int k, double h){
	return h*k;
}

void y(double* out, double x){
	out[0] = sin(x)*exp(x);
	out[1] = cos(x)*exp(x);
}
void f(double *out,double x,double *in,double h){
	out[0] = h*(in[0] + cos(x)*exp(x));
	out[1] = h*(in[1] - sin(x)*exp(x));
}

void RK(double *yn, int k, double h,int j){
	int i;
	double *k1,*k2,*k3, *y;
	k1 = (double*)malloc(sizeof(double)*k);
	k2 = (double*)malloc(sizeof(double)*k);
	k3 = (double*)malloc(sizeof(double)*k);

	//r2 = (double*)malloc(sizeof(double)*k);
	y = (double*)malloc(sizeof(double)*k);

	

	f(k1,xk(j,h),yn,h);
	for(i=0;i<k;i++){
		y[i] = yn[i] + k1[i]/2.;
	}


	f(k2,xk(j,h)+h/2.,y,h);
	for(i=0;i<k;i++){
		y[i] = yn[i] - k1[i] + 2.*k2[i];
	}

	f(k3,xk(j,h)+h,y,h);
	for(i=0;i<k;i++){
		yn[i] = yn[i]+(k1[i]+4*k2[i]+k3[i])/6.;
		//yn[i] = yn[i]+((1./6.)*k1[i]+(4./6.)*k2[i]+(1./6.)*k3[i]);
	}
	free(k1);
	free(k2);
	free(k3);
	free(y);
}




int main(void){
	int N,k,i,j;
	double h,b;
	double *yn,*yx;
	FILE *file;
	file = fopen("output.txt", "w");
	if(file == NULL)
	{
		printf("%s","can not read file!");
		return -1;
	}
	printf("%s\n","enter N");
	scanf("%d", &N);
	printf("%s\n","enter k(number equatins)");
	scanf("%d", &k);
	printf("%s\n","enter b(end of otrezka)");
	scanf("%lf", &b);
	h = b/(double)N;
	yx = (double *)malloc(k*sizeof(double));
	yn = (double *)malloc(k*sizeof(double));
	y(yx, xk(0,h));
    y(yn, xk(0,h));
	//printf("%25.18lf", xk(0,h));
	fprintf(file,"%25.18lf", xk(0,h));
	for(j=0;j<k;j++){
		printf("%25.18lf", yx[j]);
		printf("%25.18lf", yn[j]);
		fprintf (file, "%25.18lf", yn[j]); 
	}
	printf("\n");
	fprintf (file, "\r\n");
	for(i=1;i<N+1;i++){
		y(yx, xk(i,h));
		//printf("%25.18lf", xk(i,h));
		fprintf(file,"%25.18lf", xk(i-1,h));
		RK(yn, k, h,i-1);
		for(j=0;j<k;j++){
			printf("%25.18lf", yx[j]);
			printf("%25.18lf", yn[j]);
			printf("%25.18lf", (yn[j]-yx[j])/(h*h*h));
			fprintf (file, " %25.18lf", yn[j]); 
		}
		printf("\n");
		fprintf (file, "\r\n");
	}
	free(yn);
	free(yx);
	fclose(file);
	return 0;
} 
