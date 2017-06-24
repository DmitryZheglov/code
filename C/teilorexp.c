#include <math.h>
#include <stdlib.h>
#include <stdio.h>
const double eps=2e-16;


double factorial(int n);
double power(double t, int n);


double factorial(int n){
	double prod = 1;
	int i = 1;
	for(i;i<n+1;i=i+1){
		prod = prod*i;
	}
	return prod;
}

double power(double t, int n){
	int i = 1;
	double prod = 1;
	for(i; i < n+1; i = i+1){
		prod = prod*t;
	}
	return prod;
}

int main(void){
	double sum = 1;
	double sum2 = 0;
	double prod =1;
	int n;
	double t;
	double exponent;
	scanf("%lf", &t);
	scanf("%d", &n);
	double prodd = 1;
	int k = n;
	int i = 1;
	while(fabs(prod*t/(double)(i))>eps ){//(sum < sum +prod*t/(double)i)||(sum > sum +prod*t/(double)i)
		printf("%d\n",i);
		prod = prod*t/(double)i;
		sum = sum + prod;
		i++;
	}

	for(k; k>=0; k--){
		printf("%d\n",k);
		sum2 = sum2 + power(t,k)/factorial(k);
	}

	printf("%25.18f\n",sum);
	printf("%25.18f\n",sum2);
	exponent = exp(t);
	printf("%25.18f\n",exponent);
	printf("%25.18f\n",(exponent-sum)/exponent);
	printf("%25.18f\n",(exponent-sum2)/exponent);
	return(sum);
	

}
