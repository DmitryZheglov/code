#include <stdio.h>
#include <stdlib.h>
#include <math.h>

double f(double x);
double p(double *a, double t,int n);
double l(double *x, double t,int n);

double f(double x){
    return x*x;
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

double l(double *x, double t,int n){
  int i, j;
  double z, r;
  r = 0;
  for(i = 0;i < n;i++){
      z = 1.;
      for(j = 0;j < n;j++){
          if (i != j){
              z = z*((t - x[j]) / (x[i] - x[j]));
          } 
        }
      r = r + f(x[i])*z;
    }
  return r;
}

int main(void){
    //FILE *input;
    FILE *output;
    int n,i,j,k,m;
    double **A, *a, s, *x, *b, a1_k, x1k, c, d, h;
    //input = fopen("input.txt","r");
    output = fopen("output.txt","w");
    //fscanf(input,"%d",&n);
    printf("%s\n","vvedite n:");
    scanf("%d %lf %lf",&n, &c, &d);
    h = (d-c)/((double)(n-1));
    A = (double**)malloc(sizeof(double*)*n);
    a = (double*)malloc(sizeof(double)*n*n);
    b = (double*)malloc(sizeof(double)*n);
    x = (double*)malloc(sizeof(double)*n);
    A[0] = &a[0];
    for (i = 1; i < n; i++)
    {
        A[i] = A[i-1]+n;
    }

    //zapolnenie
    for(i=0;i<n;i++){
        x[i] = c +i*h;
        b[i] = f(x[i]);
        A[i][0] = 1.;
        for(j=1;j<n;j++){
            A[i][j] = A[i][j-1]*x[i];
        }
    }

    //print matrix
    printf ("Matrix:\n");
    for(i = 0; i<n;i++){
        for(j=0;j<n;j++){
            printf("%lf %s",A[i][j],"  ");
        }
        printf("\n");
    }
    //otrazhenie
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
        for(i = 0;i < n;i++){
            s = 0.;
            for(k = 0;k < n;k++){
                s = s + A[k][i]*x[k];
            }
            for(k = 0; k < n; k++){
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
        for(i = j + 1;i < n;i++){
             A[i][j] = 0;
        }
    }
    x[n-1] = b[n-1]/A[n-1][n-1];
    for(i = n - 2;i >= 0; i--){
            x[i] = b[i];
            for(j = n - 1;j > i;j--){
                x[i] = x[i] - A[i][j]*x[j];
            }
            x[i] = x[i]/A[i][i];
        }
    printf ("Coef:\n");
    for(i = 0;i < n; i++){
        printf ("%lf",x[i]);
    }
    printf("\n");
    for(i = 0;i < n;i++){
         b[i] = c + i*h;
    }
    h = h/3.;
    fprintf(output ,"x \t\t p(x)\t\t\t l(x)\t\t\t f(x)\t\t  p(x)-f(x) \t\t\t   l(x)-f(x) \t\t   p(x)-l(x)\r\n");
    for (i = 0 ;i < 3*n - 2 ;i++){
        s = c + i*h;
        fprintf (output ,"%e %e %e %e %e %e %e\r\n", s , p(x,s,n) , l(b,s,n), f(s) , fabs(p(x,s,n) - f(s)), fabs(l(b,s,n) - f(s)), fabs(p(x,s,n) - l(b,s,n)));
    }
    //%25.18lf



    free(A);
    free(a);
    free(b);
    free(x);
    return 0;
}