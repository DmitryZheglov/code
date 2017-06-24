#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
//#define EPS 1e-6
const double EPS = 1e-6;

double B(double x);
double f(double x,double y);
double p(double *a, double t,int n);
double l(double *x, double t,int n);
int cmpfunc(const void * a, const void * b);
int otraz(double **A, double *x, double *b, int n);


double f(double x,double y){
    //return fabs(x);
   // return sin(x)+cos(y);
   //return sin(x)+cos(y);
  return exp(-50*((x-0.2)*(x-0.2)+(y-0.2)*(y-0.2)))+exp(-50*((x-0.8)*(x-0.8)+(y-0.8)*(y-0.8)))+exp(-50*((x-0.5)*(x-0.5)+(y-0.5)*(y-0.5)));
}

double B(double x){
    if(fabs(x)<1){
        return 2./3.-x*x+0.5*pow(fabs(x),3);
    }
    else if(fabs(x)>2){
        return 0;
    }
    else{
        return 1./6.*pow((2.-fabs(x)),3);
    } 
}


double p(double *a, double t,int n){
    int i;
    double z,r;
    z = 0.;
    r = 1.;
    for(i = 0;i < n;i++){
        z = z + a[i]*r;
        r = r*t;
    }
    return z;
}
int otraz(double **A, double *x, double *b, int n){
    int i, j, k;
    double s;
    s=0.;
    for(j = 0;j < n - 1;j++){
            s = 0.;
            for(k = j ;k < n;k++){
                s = s + (A[k][j])*(A[k][j]);
            }
            s = sqrt(s);
            x[j] = A[j][j] - s;
            s = 0.;
            for(k = j + 1;k < n;k++){//!!!!!!!!!.
                x[k] = A[k][j];
                s = s + x[k]*x[k];//!!!!!!!!!!!+=
            }
            s = sqrt(s + x[j]*x[j]);
            for(k = j;k < n;k++){
                x[k] = x[k]/s;
            }
            for(i = j;i < n;i++){
                s = 0.;
                for(k = j;k < n;k++){
                    s = s + A[k][i]*x[k];
                }
                for(k = j; k < n; k++){
                    A[k][i] = A[k][i] - 2.*x[k]*s;
                }
            }
            s = 0;
            for(i = j;i < n;i++){
                s = s + x[i]*b[i];
            }
            for(i = j;i < n;i++){
                b[i] = b[i] - 2.*x[i]*s;
            }
            x[j] = 0;
        }

        x[n-1] = b[n-1]/A[n-1][n-1];
        

        for(i = n - 2;i >= 0; i--){
                x[i] = b[i];
                for(j = n - 1;j > i;j--){
                    x[i] = x[i] - A[i][j]*x[j];
                }
                x[i] = x[i]/A[i][i];
            }
        //
        /*
        printf ("Coefficients:\n");
        for(i = 0;i < n; i++){
            printf ("%s %d %lf %s","a",i,x[i], " ");
        }
        printf("\n");
	*/
        return 0;
}

/*
FILE *input;
input = fopen("output.txt","r");
fscanf(input, "%lf",& F[i][l]);
*/
int main(void){
    FILE *input;
    FILE *output;
    int N,n,i,j,k,l,u,v,red;
    double **A, **F,**R, *yN, *a, **C, *c, *bn, *x, *b,  x1, x2, s, *xN, *fN,xk, h, *alpha, *r,yk,max,d,yi,xj;
    input = fopen("output.txt","r");

    output = fopen("output2.txt","w");
    //fscanf(input,"%d",&n);
    printf("%s\n","vvedite red,N,m,c,d:");
    //n<N-2
    scanf("%d %d %d %lf %lf",&red,&N,&n, &x1, &x2);
    
    alpha = (double*)malloc(sizeof(double)*N);
    A = (double**)malloc(sizeof(double*)*(N));
    F = (double**)malloc(sizeof(double*)*(N));
    R = (double**)malloc(sizeof(double*)*(2*N-1));
    a = (double*)malloc(sizeof(double)*(N)*(n));
    b = (double*)malloc(sizeof(double)*(N));
    C = (double**)malloc(sizeof(double*)*(n));
    c = (double*)malloc(sizeof(double)*(n)*(n));
    bn = (double*)malloc(sizeof(double)*(n));
    x = (double*)malloc(sizeof(double)*(n));
    xN = (double*)malloc(sizeof(double)*N);
    yN = (double*)malloc(sizeof(double)*N);
    fN = (double*)malloc(sizeof(double)*N*N);
    r = (double*)malloc(sizeof(double)*(2*N-1)*(2*N-1));

    A[0] = &a[0];
    for (i = 1; i < N; i++)
    {
        A[i] = A[i-1]+n;
    }

    C[0] = &c[0];
    for (i = 1; i < n; i++)
    {
        C[i] = C[i-1]+n;
    }

    F[0] = &fN[0];
    for (i = 1; i < N; i++)
    {
        F[i] = F[i-1]+N;
    }


    R[0] = &r[0];
    for (i = 1; i < (2*N-1); i++)
    {
        R[i] = R[i-1]+(2*N-1);
    }
    //fprintf(input ,"%d %d %lf %lf\r\n", N,n, c, d);

    if(red==1){
        for(i=0;i<N;i++){
            fscanf(input, "%lf", &F[i][0]);
            //printf("%lf %s",F[i][0]," ");
            for(l=1;l<N;l++){
                fscanf(input, "%lf", &F[i][l]);
                //printf("%lf %s",F[i][l]," ");
            }
            //printf(" \n");
        }
    }

    //zap xN fN
    h = (x2-x1)/((double)(N-1));
    printf("%s %d \n ","N-1", N-1);
    printf("%s %lf \n","h:", h);
    for(i = 0;i<N;i++){
        xN[i] = x1 +i*h;
        yN[i] = x1 +i*h;
    }

    
    //zapolnenie
    //max = 0;
    for(j=0;j<N;j++){
        if(red==0){
            for(i = 0;i<N;i++){
                F[j][i] = f(xN[i],yN[j]);
            }
        }
        
        for(k = 0;k < N-1;k++){//по всем точка
        
            xk = (xN[k]+xN[k+1])/2.;
            /*
            for(i = 0; i < N; i++){
                alpha[i] = 1.;
            }
            */
            for(i = k+1; i < N; i++){
                alpha[i] = 1./pow(10,i-(k+1));
            }
            for(i = k; i >=0; i--){
                alpha[i] = 1./pow(10,k-i);
                
            }
            
            for(i=0;i<N;i++){
                b[i] = F[j][i];
                A[i][0] = 1.;
                for(u=1;u<n;u++){
                    A[i][u] = A[i][u-1]*xN[i];
                }
            }

            //C = A.T*D*A 
            for(i = 0; i < n; i++){
                for(u = 0; u < n; u++){
                    s = 0;
                    for(v = 0; v < N; v++){
                        s = s + A[v][i]*alpha[v]*A[v][u];
                    }
                    C[i][u] = s;
                }
            }
            //bn = A.T*D*b
            for(i = 0; i < n; i++){
                s = 0;
                for(u = 0; u < N; u++){
                    s = s + A[u][i]*alpha[u]*b[u];
                }
                bn[i] = s;
            }
            //otrazhenie Cx = bn
            otraz(C,x,bn,n);
            R[2*j][2*k] = F[j][k];
            R[2*j][2*k+1] = p(x, xk, n);
            /*
            if(max<fabs(f(xk,yN[j])-(p(x, xk, n)))){
                max = fabs(f(xk,yN[j])-(p(x, xk, n)));
            }
            */
           // printf("%d \n",k);
            //fprintf(output,"%lf %s %lf %s",F[j][k]," ",p(x, xk, n), " ");//n?n-1
        }
        //fprintf(output,"%lf \r\n",F[j][N-1]);
        R[2*j][2*(N-1)] = F[j][N-1];
    }
    //printf("%lf \n",max);
    
    printf("bspl\n");
    for(i=0; i<N; i++){
        //am1 = (6.*f[i]-f[i-1]-f[i+1])/8.
        if(red==0){
            for(j = 0;j<N;j++){
                F[j][i] = f(xN[i],yN[j]);
            }
        }
        

        for (k=0; k<N-1; k++){
            //printf("%d %d \n",i,k);
            yk = (yN[k]+yN[k+1])/2.;
            R[2*k+1][2*i] = (2*F[0][i]-F[1][i])*B((yk+h)/h) + (2*F[N-1][i]-F[N-2][i])*B((yk-(N)*h)/h);
            for (j=0;j<N;j++)
                R[2*k+1][2*i] = R[2*k+1][2*i] + F[j][i]*B((yk-j*h)/h);
        }
    }

    for(i=0;i<N-1;i++){
        for(j=0;j<N-1;j++){
            R[2*j+1][2*i+1] = (R[2*j][2*i+1]+R[2*j+1][2*i]+R[2*j+1][2*i+2]+R[2*j+2][2*i+1])/4;
        }
    }
    max = 0;
    h =  (x2-x1)/((double)(2*N-2));
    for(i=0;i<2*N-1;i++){
        yi = x1 + i*h;
        for(j=0;j<2*N-1;j++){
            xj = x1+j*h;
            d = fabs(R[i][j]-f(xj,yi));
            if(d>max){
                max = d;
            }
            fprintf(output,"%lf %s",R[i][j]," ");
        }
        fprintf(output,"\r\n");
    }
    printf("%e \n",max);
    

/*
    free(A);
    free(F);
    free(a);
    free(b);
    free(C);
    free(c);
    free(bn);
    free(x);
    free(xN);
    free(yN);
    free(fN);
    free(alpha);
    free(al);
    free(fx);
    free(FX);
    free(r);
    free(R);
    */
    return 0;
}
