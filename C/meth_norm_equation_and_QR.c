#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
//#define EPS 1e-6
const double EPS = 1e-6;

double f(double x);
double p(double *a, double t,int n);
double l(double *x, double t,int n);
int cmpfunc(const void * a, const void * b);
int otraz(double **A, double *x, double *b, int n);
int get_QR(double **A, double *x, double *b, int n, int m);


double f(double x){
   // return 1+x+2*x*x + 3*x*x*x +4*pow(x,4) +5*pow(x,5)+6*pow(x,6);
  return fabs(x);
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
/*
int prod_matrix(double **A, double **B, double **Q, n, m){
    int i,j,k;
    double s;
    for(i = 0; i < n; i++){
        for(j = 0; j < m; j++){
            s = 0;
            for(k = 0; k < m; k++){
                s = s + A[i][k]*B[j][k];
            }
            Q[i][j] = s;
        }
    }
}
*/
int get_QR(double **A, double *x, double *b, int n, int m){
    int i, j, k;
    double s;
    s=0.;
    for(j = 0;j < m ;j++){
        //printf("%d \n", j);
        //get w
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
        //end get w
        //U_i*A
        for(i = j;i < m;i++){
            s = 0.;
            for(k = j;k < n;k++){
                s = s + A[k][i]*x[k];
            }
            for(k = j; k < n; k++){
                A[k][i] = A[k][i] - 2.*x[k]*s;
            }
        }
        //U_i*b
        s = 0;
        for(i = j;i < n;i++){
            s = s + x[i]*b[i];
        }
        for(i = j;i < n;i++){
            b[i] = b[i] - 2.*x[i]*s;
        }
        x[j] = 0;
        //printf("%d \n", j);
            //for(i = j + 1;i < n;i++){
        //      A[i][j] = 0;
        // }
    }
    //print matrix
    
    printf ("Matrix:\n");
    for(i = 0; i<n;i++){
        for(j=0;j<m;j++){
            printf("%lf %s",A[i][j],"  ");
        }
        printf("\n");
    }
    
    x[m-1] = b[m-1]/A[m-1][m-1];
    for(i = m - 2;i >= 0; i--){
        x[i] = b[i];
        for(j = m - 1;j > i;j--){
            x[i] = x[i] - A[i][j]*x[j];
        }
        x[i] = x[i]/A[i][i];
    }
        //
    printf ("Coefficients:\n");
    for(i = 0;i < m; i++){
        printf ("%s %d %lf %s","a",i,x[i], " ");
    }
    printf("\n");

    return 0;
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
            s = 0;//for(i = 0; i< N; i++){
        //printf("%20.18lf %20.18lf %20.18lf %20.18lf \n",xN[i],p(x,xN[i],n+2),fN[i],fabs(p(x,xN[i],n+2)-fN[i]));
    
            for(i = j;i < n;i++){
                s = s + x[i]*b[i];
            }
            for(i = j;i < n;i++){
                b[i] = b[i] - 2.*x[i]*s;
            }
            x[j] = 0;
            //for(i = j + 1;i < n;i++){
        //      A[i][j] = 0;
        // }
        }
        /*
        printf ("Matrix:\n");
        for(i = 0; i<n;i++){
            for(j=0;j<n;j++){
                printf("%lf %s",A[i][j],"  ");
            }
            printf("\n");
        }
        */

        x[n-1] = b[n-1]/A[n-1][n-1];
        

        for(i = n - 2;i >= 0; i--){
                x[i] = b[i];
                for(j = n - 1;j > i;j--){
                    x[i] = x[i] - A[i][j]*x[j];
                }
                x[i] = x[i]/A[i][i];
            }
        //
        printf ("Coefficients:\n");
        for(i = 0;i < n; i++){
            printf ("%s %d %lf %s","a",i,x[i], " ");
        }
        printf("\n");

        return 0;
}


int main(void){
    //FILE *input;
    FILE *output;
    int N,n,i,j,k;
    double **A, *a, **C, *c, *bn, *x, *b,  x1, x2, s, *xN, *fN, h, *alpha, *xqr,max;
    //input = fopen("input.txt","w");
    output = fopen("output.txt","w");
    //fscanf(input,"%d",&n);
    printf("%s\n","vvedite N,m,c,d:");
    //n<N-2
    scanf("%d %d %lf %lf",&N,&n, &x1, &x2);
    
    alpha = (double*)malloc(sizeof(double)*N);
    A = (double**)malloc(sizeof(double*)*(N));
    a = (double*)malloc(sizeof(double)*(N)*(n));
    b = (double*)malloc(sizeof(double)*(N));
    C = (double**)malloc(sizeof(double*)*(n));
    c = (double*)malloc(sizeof(double)*(n)*(n));
    bn = (double*)malloc(sizeof(double)*(n));
    x = (double*)malloc(sizeof(double)*(n));
    xqr = (double*)malloc(sizeof(double)*(N));
    xN = (double*)malloc(sizeof(double)*N);
    fN = (double*)malloc(sizeof(double)*N);

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
    //fprintf(input ,"%d %d %lf %lf\r\n", N,n, c, d);

    //zap xN fN
    h = (x2-x1)/((double)(N-1));
    for(i = 0;i<N;i++){
        xN[i] = x1 +i*h;
        fN[i] = f(xN[i]);
    }

    //alpha
    for(i = 0; i < N; i++){
        alpha[i] = 1.;
    }
    
    //zapolnenie
    
    for(i=0;i<N;i++){
        b[i] = fN[i];
	    //fprintf(input ,"%lf %lf\r\n", x[i],b[i]);
        A[i][0] = 1.;
        for(j=1;j<n;j++){
            A[i][j] = A[i][j-1]*xN[i];
        }
    }

    //print matrix
    /*
    printf ("Matrix:\n");
    for(i = 0; i<N;i++){
        for(j=0;j<n;j++){
            printf("%lf %s",A[i][j],"  ");
        }
        printf("\n");
    }
    */

    //normalnogo vectora!!!!!!!!!!!!!!!!!!!!!!!!!

    //C = A.T*D*A 
    for(i = 0; i < n; i++){
        for(j = 0; j < n; j++){
            s = 0;
            for(k = 0; k < N; k++){
                s = s + A[k][i]*alpha[k]*A[k][j];
            }
            C[i][j] = s;
        }
    }
    //bn = A.T*D*b
    for(i = 0; i < n; i++){
        s = 0;
        for(k = 0; k < N; k++){
            s = s + A[k][i]*alpha[k]*b[k];
        }
        bn[i] = s;
    }
    //otrazhenie Cx = bn
    otraz(C,x,bn,n);
    
    //printf("Krasava \n");
    //%20.18lf
    //printf("%d %d \n", N, n);
    for(i=0;i<N;i++){
        b[i] = sqrt(alpha[i])*fN[i];
	    //fprintf(input ,"%lf %lf\r\n", x[i],b[i]);
        A[i][0] = 1.;
        for(j=1;j<n;j++){
            A[i][j] = sqrt(alpha[i])*A[i][j-1]*xN[i];
        }
    }
    get_QR(A, xqr, b, N, n);
    for(i = 0; i< N; i++){
      fprintf(output,"%20.18lf %20.18lf %20.18lf %20.18lf %20.18lf %20.18lf %20.18lf \r \n", xN[i], fN[i], p(x, xN[i],n), p(xqr, xN[i],n),fabs(fN[i]-p(x, xN[i],n)),fabs(fN[i]-p(xqr, xN[i],n)),fabs(p(x, xN[i],n)-p(xqr, xN[i],n)) );
    }
    s = 0;
    max = 0;
    for(i = 0; i< N; i++){
      s = s + (fN[i]-p(x, xN[i],n))*(fN[i]-p(x, xN[i],n));
      if(max < fabs(fN[i]-p(x, xN[i],n))){
	max = fabs(fN[i]-p(x, xN[i],n));
      }
    }
    printf("%s %20.18lf \n","L2 NORM ",sqrt(s));
    printf("%s %20.18lf \n","C NORM ",max);
    
    s = 0;
    max = 0;
    for(i = 0; i< N; i++){
      s = s + (fN[i]-p(xqr, xN[i],n))*(fN[i]-p(xqr, xN[i],n));
      if(max < fabs(fN[i]-p(xqr, xN[i],n))){
	max = fabs(fN[i]-p(xqr, xN[i],n));
      }
    }
    printf("%s %20.18lf \n","L2 QR ",sqrt(s));
    printf("%s %20.18lf \n","C QR ",max);
    
    free(A);
    free(a);
    free(b);
    free(C);
    free(c);
    free(bn);
    free(x);
    free(xN);
    free(fN);
    free(alpha);
    free(xqr);
    
    return 0;
}