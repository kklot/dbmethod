#include <TMB.hpp>
#include "ktools.hpp"

template<class Type>
Type objective_function<Type>::operator() ()
{
  parallel_accumulator<Type> dll(this);

  // data
  DATA_VECTOR(afs);
  DATA_IVECTOR(age);
  DATA_IVECTOR(event);
  DATA_IVECTOR(yob);

  // priors
  DATA_VECTOR(sd_beta);
  DATA_VECTOR(sd_yob);
  DATA_VECTOR(sd_age);
  
  DATA_SCALAR(age_order);
  DATA_SCALAR(yob_order);

  DATA_VECTOR(shape_prior);
  DATA_VECTOR(skew_prior);

  DATA_MATRIX(R_age);
  DATA_MATRIX(R_yob);

  // Data model - log-logistic parameters
  PARAMETER(intercept);

  Type prior = 0.0;
  prior -= dnorm(intercept, sd_beta(0), sd_beta(1), true);

  // Shape
  PARAMETER(log_shape); 
  prior -= dnorm(log_shape, shape_prior(0), shape_prior(1), true);
  Type shape = exp(log_shape);

  // Skewness *
  PARAMETER(log_skew);
  Type skew = exp(log_skew);
  prior -= dnorm(log_skew, skew_prior(0), skew_prior(1), true);

  // yob rw2
  PARAMETER_VECTOR (yob_rw2);
  PARAMETER        (log_yob_rw2_e);

  Type yob_rw2_e = exp(log_yob_rw2_e);
  prior -= ktools::pc_prec(yob_rw2_e, sd_yob(0), sd_yob(1));
  prior += ktools::rw(yob_rw2, R_yob, yob_rw2_e, yob_order);

  // age rw2
  PARAMETER_VECTOR (age_rw2);
  PARAMETER        (log_age_rw2_e);

  Type age_rw2_e = exp(log_age_rw2_e);
  prior -= ktools::pc_prec(age_rw2_e, sd_age(0), sd_age(1));
  prior += ktools::rw(age_rw2, R_age, age_rw2_e, age_order);

  // Data likelihood
  for (int i = 0; i < afs.size(); i++) 
  {
    Type eta = intercept + yob_rw2(yob(i)) + age_rw2(age(i));
    Type scale = exp(eta);
    if (event(i)) 
		{
      dll -= log(ktools::ft_llogisI(afs(i), shape, scale, skew) );
    } 
		else {
      dll -= log(ktools::St_llogisI(afs(i), shape, scale, skew));
    }
  }
  dll += prior;
  return dll;
}
