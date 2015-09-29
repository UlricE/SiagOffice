
#include <math.h>

#ifdef qgamma
/* It was reported that mips-sgi-irix6.5 has a weird and conflicting define
   for qgamma.  See bug 1689.  */
#warning "Your <math.h> is somewhat broken; we'll work around that."
#undef qgamma
#endif

/* Make up for a few deficient headers.  */
#ifndef M_LN2
#define M_LN2 0.69314718055994530942
#endif
#ifndef M_LN10
#define M_LN10 2.30258509299404568402
#endif

/* ------------------------------------------------------------------------- */

int range_sum (const double *xs, int n, double *res);
int range_product (const double *xs, int n, double *res);

int range_sumsq (const double *xs, int n, double *res);
int range_avedev (const double *xs, int n, double *res);

int range_average (const double *xs, int n, double *res);
int range_harmonic_mean (const double *xs, int n, double *res);
int range_geometric_mean (const double *xs, int n, double *res);

int range_min (const double *xs, int n, double *res);
int range_max (const double *xs, int n, double *res);

int range_devsq (const double *xs, int n, double *res);
int range_var_pop (const double *xs, int n, double *res);
int range_var_est (const double *xs, int n, double *res);
int range_stddev_pop (const double *xs, int n, double *res);
int range_stddev_est (const double *xs, int n, double *res);
int range_skew_pop (const double *xs, int n, double *res);
int range_skew_est (const double *xs, int n, double *res);
int range_kurtosis_m3_pop (const double *xs, int n, double *res);
int range_kurtosis_m3_est (const double *xs, int n, double *res);

int range_covar (const double *xs, const double *ys, int n, double *res);
int range_correl_pop (const double *xs, const double *ys, int n, double *res);
int range_correl_est (const double *xs, const double *ys, int n, double *res);
int range_rsq_pop (const double *xs, const double *ys, int n, double *res);
int range_rsq_est (const double *xs, const double *ys, int n, double *res);

/* ------------------------------------------------------------------------- */

double bessel_i (double x, double alpha, double expo);
double bessel_k (double x, double alpha, double expo);

/* "d": density.  */
/* "p": distribution function.  */
/* "q": inverse distribution function.  */

/* The normal distribution.  */
double dnorm (double x, double mu, double sigma);
double pnorm (double x, double mu, double sigma);
double qnorm (double p, double mu, double sigma);

/* The log-normal distribution.  */
double plnorm (double x, double logmean, double logsd);
double qlnorm (double x, double logmean, double logsd);

/* The gamma distribution.  */
double dgamma (double x, double shape, double scale);
double pgamma (double x, double p, double scale);
double qgamma (double p, double alpha, double scale);

/* The beta distribution.  */
double pbeta (double x, double pin, double qin);
double qbeta (double alpha, double p, double q);

/* The t distribution.  */
double pt (double x, double n);
double qt (double p, double ndf);

/* The F distribution.  */
double pf (double x, double n1, double n2);
double qf (double x, double n1, double n2);

/* The chi-squared distribution.  */
double pchisq (double x, double df);
double qchisq (double p, double df);

/* The Weibull distribution.  */
double dweibull (double x, double shape, double scale);
double pweibull (double x, double shape, double scale);

/* The Poisson distribution.  */
double dpois (double x, double lambda);
double ppois (double x, double lambda);

/* The exponential distribution.  */
double dexp (double x, double scale);
double pexp (double x, double scale);

/* Binomial distribution.  */
double dbinom (double x, double n, double p);
double pbinom (double x, double n, double p);
double qbinom (double x, double n, double p);

/* Random number generation. */
double random_01          (void);
double random_poisson     (double lambda);
double random_binomial    (double p, int trials);
double random_negbinom    (double p, int f);
double random_exponential (double b);
double random_bernoulli   (double p);
double random_normal      (void);

/* ------------------------------------------------------------------------- */

/* Matrix functions. */
double mdeterm (double *A, int dim);
int     minverse(double *A, int dim, double *res);
void    mmult   (double *A, double *B, int cols_a, int rows_a, int cols_b,
		 double *product);

/* ------------------------------------------------------------------------- */

/* Misc. */
double     gpow10                 (int n);
int         gcd                    (int a, int b);


/* ------------------------------------------------------------------------- */

/* Optimization methods for the Solver tool. */


/* Affine scaling */

typedef void (*affscale_callback_fun_t) (int iter, double *x, 
					 double bv, double cx, 
					 int n_variables, void *data);

int affine_init (double *A, double *b, double *c, int n_constraints,
		      int n_variables, double *x);
int affine_scale (double *A, double *b, double *c, double *x,
		       int n_constraints, int n_variables, int max_flag,
		       double e, int max_iter,
		       affscale_callback_fun_t fun, void *data);

int branch_and_bound (double *A, double *b, double *c, double *xx,
			   int n_constraints, int n_variables, int n_original,
			   int max_flag, double e, int max_iter,
			   int *int_r,
			   affscale_callback_fun_t fun, void *data,
			   double *best);

