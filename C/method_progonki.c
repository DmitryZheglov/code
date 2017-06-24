#include <stdio.h>
#include <stdlib.h>
#include <math.h>


double xk(double k, double h);
double y(double x);
double f(double x);
double p(double x);
double k(double x);

double xk(double k, double h){
	return h*(double)k;
}


double y(double x){
	return exp(x)*(x*x*x*x-2.*x*x*x+x*x);
}

double k(double x){
	//return 1.+0*x;
    return (x*x + 1.);
}

double p(double x){
	//return 1.+0*x;
	return cos(x);
}
double f(double x){
    //for p=1,k=1
    //return exp(x)*(-8.*x*x*x+8.*x-2.);
	//for give k,p;
	return exp(x)*(x*x*x*x-2.*x*x*x+x*x)*cos(x)-exp(x)*(x*x*x*x*x*x+8.*x*x*x*x*x+6.*x*x*x*x-12.*x*x*x+7.*x*x-8.*x+2.);
}

int main(void){
    int i,N;
    double *yk, *aa, *bb;
    double aN,cN,ak,ck,bk,h,s;//b0,c0,
    s=0;
    scanf("%d",&N);
    h = 1./((double)N-1.);
    //b0 = 1.;
    //c0 = 1.;
    aN =  1.;
    cN = 1.;
    yk = (double*)malloc(sizeof(double)*(N+1));
    aa = (double*)malloc(sizeof(double)*(N+1));
    bb = (double*)malloc(sizeof(double)*(N+1));
    aa[1] = 1.;//b0/c0;
    bb[1] = 0.;//0./c0;
    //printf("%lf %lf \n",aa[1],bb[1]);
    for(i=1;i<N;i++){
        bk = k(xk((double)i,h))/(h*h);
        ck = ((k(xk((double)i,h))+k(xk((double)i-1.,h)))/(h*h)+p(xk((double)i-0.5,h)));
        ak = k(xk((double)i-1.,h))/(h*h);
        aa[i+1] = bk/(ck-ak*aa[i]);
        bb[i+1] = (f(xk((double)i-0.5,h))+ak*bb[i])/(ck-ak*aa[i]);
        //printf("%lf %lf %lf \n",ck-ak*aa[i],aa[i+1],bb[i+1]);
    }
    yk[N] = (0.+aN*bb[N])/(cN-aN*aa[N]);//(f(xk(N,h),h)+aa[N]*bb[N])/(cN-aN*aa[N]);
    //yk[N] = bb[N]/(-1.-aa[N]);
    //yk[N-1] = - yk[N];
    for(i=N-1;i>-1;i--){
        yk[i] = aa[i+1]*yk[i+1]+bb[i+1];
    }
    //yk[1] = b1*yk[2]/(a1+c1)+f(xk(0.5,h));
    //yk[0] = -yk[1];
    
    for(i=1;i<N;i++){
        printf("%e %25.12lf %lf \n",y(xk((double)i-0.5,h)),yk[i],(y(xk((double)i-0.5,h))-yk[i])/(h*h));
    }
    
    for(i=0;i<N+1;i++){
        yk[i] = y(xk((double)i-0.5,h))-yk[i];
    }
    for (i=1; i<N; i++){
       s+=yk[i]*yk[i]*h;
    }
    //s+=y[0]*y[0]*h/2. + y[N]*y[N]*h/2.;
    printf ("Deflection:%20.18lf\n", sqrt(s)/(h*h));
    s=0;
    for(i=1;i<N;i++){//!!!
	    if (fabs(yk[i]) > s){
	      s = fabs(yk[i]);
	    }
	}
    printf("%e\n",s/(h*h));
}
