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
int sign(double x);
int otraz(double **A, double *x, double *b, int n);

int sign(double x){
    if(x>EPS){
        return 1;
    }
    else if(x<-EPS){
        return 0;
    }
    else{
        return 0;
    }
}

double f(double x){
    return fabs(x)+x*x+exp(x);
}

double p(double *a, double t,int n){
    int i;
    double z,r;
    z = 0.;
    r = 1.;
    for(i = 0;i < n-1;i++){
        z = z + a[i]*r;
        r = r*t;
    }
    return z;
}

int cmpfunc(const void * a, const void * b){
   return ( *(int*)a - *(int*)b );
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
            //for(i = j + 1;i < n;i++){
        //      A[i][j] = 0;
        // }
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


int main(void){
    FILE *input;
    FILE *output;
    int n,m,i,j,k,*rn,r,tmp,t, ind, N;
    double **A, *a, s, *x, *b,  c, d, h, h_max, *xN, *fN, hf , h_i, h1, h2;
    input = fopen("input.txt","w");
    output = fopen("output.txt","w");
    //fscanf(input,"%d",&n);
    printf("%s\n","vvedite N,m,c,d:");
    //n<N-2
    scanf("%d %d %lf %lf",&N,&n, &c, &d);
    
    A = (double**)malloc(sizeof(double*)*(n+2));
    a = (double*)malloc(sizeof(double)*(n+2)*(n+2));
    b = (double*)malloc(sizeof(double)*(n+2));
    x = (double*)malloc(sizeof(double)*(n+2));
    xN = (double*)malloc(sizeof(double)*N);
    fN = (double*)malloc(sizeof(double)*N);
    rn = (int*)malloc(sizeof(int)*(n+2));

    A[0] = &a[0];
    for (i = 1; i < n+2; i++)
    {
        A[i] = A[i-1]+n+2;
    }
    //fprintf(input ,"%d %d %lf %lf\r\n", N,n, c, d);

    //zap xN fN
    h = (d-c)/((double)(N-1));
    for(i = 0;i<N;i++){
        xN[i] = c +i*h;
        fN[i] = f(xN[i]);
    }

    //zap random
    srand(time(NULL));
    i=1;
    rn[0] = rand()%N;
    while(i<n+2){
        tmp = 0;
        r = rand()%N;
        for(j=0;j<i;j++){
            if(rn[j] == r){
                tmp = tmp +1;
            }
        }
        if(tmp<1){
            rn[i] = r;
            i = i+1;
        }
    }
    //sort index
    qsort(rn, n+2, sizeof(int), cmpfunc);

    t = 1;
    while(t>0){
        printf("%d \n",t);
        //zapolnenie
        for(i=0;i<n+2;i++){
            x[i] = c+h*rn[i];
            b[i] = f(x[i]);
            //printf("%lf %lf \n", x[i],b[i]);
            A[i][0] = 1.;
            for(j=1;j<n+1;j++){
                A[i][j] = A[i][j-1]*x[i];
            }
            A[i][n+1] = pow((-1),i);
            //printf("%lf %lf \n", x[i],b[i]);
        }

        
    //otrazhenie
        otraz(A,x,b,n+2);
        //
        hf = fabs(x[n+1]);
        ind = 0;
        h_max = 0;
        for(i=0;i<N;i++){
            k = 0;
            for(j = 0; j < n+2; j++){
                if(fabs(xN[i] - xN[rn[j]])>EPS){
                    k = k + 1;
                }
            }
            if(k == n+2){
                h_i = fabs(p(x,xN[i],n + 2)-fN[i]);
                if(h_max < h_i){
                    h_max = h_i;
                    ind = i;
                }
            }
        }
        printf("%lf %lf %d \n",hf, h_max, i);
        
        if(h_max < hf + EPS){
            printf("h_max: %25.18lf\n", hf); 
            break;
        }

        h_i = fN[ind] - p(x,xN[ind],n + 2);
        if(ind < rn[0]){
            h1 = fN[rn[0]] - p(x,xN[rn[0]],n + 2);
            if(h_i * h1 >0){
                rn[0] = ind;
            }
            else{
                for(i = n + 1; i > 0; i--){
                    rn[i] = rn[i-1];
                }
                rn[0] = ind;
            }
        }
        else if(rn[n+1] < ind){
            h1 = fN[rn[n+1]] - p(x,xN[rn[n+1]],n + 2);
            if(h_i*h1>0){
                rn[n+1] = ind;
            }
            else{
                for(i = 0; i < n + 1; i++){
                    rn[i] = rn[i+1];
                }
                rn[n+1] = ind;
            }
        }
        else{
            for(i = 0; i< n + 1; i++){
                if((rn[i]<ind)&&(ind< rn[i+1])){
                    break;
                }
            }
            h1 = fN[rn[i]] - p(x,xN[rn[i]],n + 2);
            if(h_i*h1 > 0){
                rn[i] = ind;
            }
            else{
                rn[i+1] = ind;
            }
        }
        t = t+1;
        
    }
    /*
    for(i = 0; i< N; i++){
        printf("%lf %lf %lf %lf \n",xN[i],p(x,xN[i],n+2),fN[i],fabs(p(x,xN[i],n+2)-fN[i]));
    }
    */
    printf("Krasava \n");
    //%20.18lf



    free(A);
    free(a);
    free(b);
    free(x);
    free(xN);
    free(fN);
    return 0;
}