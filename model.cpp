#include <TMB.hpp>
#include "ktools.hpp"

template<class Type>
Type objective_function<Type>::operator() ()
{
  parallel_accumulator<Type> dll(this);

  // data
  DATA_VECTOR(afs); DATA_UPDATE(afs); // afs is updatable
  DATA_VECTOR(afs_u);
  DATA_VECTOR(afs_l);
  DATA_IVECTOR(age);
  DATA_IVECTOR(event);
  DATA_IVECTOR(yob);

  DATA_VECTOR(svw);

	// meta data
	DATA_INTEGER(to_skew);
	DATA_INTEGER(age_term);
	DATA_INTEGER(yob_term);
	DATA_INTEGER(smooth_age);
	DATA_INTEGER(smooth_yob);
	DATA_INTEGER(interval_censor);
	DATA_IVECTOR(to_fit);

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
	Type skew = 1.;
	if (to_skew) {
		prior -= dnorm(log_skew, skew_prior(0), skew_prior(1), true);
		// Skewness real
		/* a = exp(skew - 1.1*log_shape); */
		skew = exp(log_skew);
	}

  // yob rw2
	PARAMETER 			 (beta_yob);
  PARAMETER_VECTOR (yob_rw2);
  PARAMETER        (log_yob_rw2_e);

	if (yob_term) 
	{
		if (smooth_yob) {
			Type yob_rw2_e = exp(log_yob_rw2_e);
			prior -= ktools::pc_prec(yob_rw2_e, sd_yob(0), sd_yob(1));
			prior += ktools::rw(yob_rw2, R_yob, yob_rw2_e, yob_order);
		} else {
			prior -= dnorm(beta_yob, sd_beta(0), sd_beta(1), true);
		}
	}

  // age rw2
	PARAMETER 			 (beta_age);
  PARAMETER_VECTOR (age_rw2);
  PARAMETER        (log_age_rw2_e);

	if (age_term)
	{
		if (smooth_age) {
			Type age_rw2_e = exp(log_age_rw2_e);
			prior -= ktools::pc_prec(age_rw2_e, sd_age(0), sd_age(1));
			prior += ktools::rw(age_rw2, R_age, age_rw2_e, age_order);
		} else {
			prior -= dnorm(beta_age, sd_beta(0), sd_beta(1), true);
		}
	}

  // Data likelihood
	vector<Type> logliki(afs.size());

  for (int i = 0; i < afs.size(); i++) {

    Type eta = intercept;

		if (yob_term) 
		{
			if (smooth_yob)
				eta += yob_rw2(yob(i));
			else
				eta += beta_yob * yob(i);
		}

		if (age_term)
		{
			if (smooth_age)
				eta += age_rw2(age(i));
			else
				eta += beta_age * age(i);
		}

    Type scale = exp(eta);

    if (event(i)) 
		{
			if (interval_censor)
				logliki(i) = log( svw(i) * (
							ktools::St_llogisI(afs_l(i), shape, scale, skew) - 
							ktools::St_llogisI(afs_u(i), shape, scale, skew) ));
			else
				logliki(i) = log( svw(i) * ktools::ft_llogisI(afs(i), shape, scale, skew) );
    } 
		else {
      logliki(i) = log(svw(i) * ktools::St_llogisI(afs(i), shape, scale, skew));
    }
		if (to_fit(i))
			dll -= logliki(i);
  }
  dll += prior;
	REPORT(logliki);
  return dll;
}
