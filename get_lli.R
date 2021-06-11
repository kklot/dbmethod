get_lli = function(to_skew = 1,
									 yob_term = 0,
									 age_term = 0,
									 smooth_age = 0,
									 smooth_yob = 0,
									 age_order = 1,
									 yob_order = 1,
									 weightv = "none",
									 interval_censor = 0,
									 data, K = 10, S = 1000, verbose = F, check = F)
{
	data = data %>% mutate(grp = sample(1:K, n(), TRUE)) 

	if (weightv == "none") 
		data$weight = 1
	if (weightv == "kish") 
	{
		data %<>%
			group_by(svlab) %>% 
			mutate(neff = sum(w.ori)^2/sum(w.ori^2)) %>% 
			mutate(weight = w.ori / sum(w.ori) * neff)  %>%
			select(-neff) %>% 
			ungroup()
	}

	# initial parameters
	parameters = list(intercept     = -3,
										log_shape     = log(10),
										log_skew      = log(1),
										beta_yob      = .01,
										yob_rw2       = rep(0, length(unique(data$yob))),
										log_yob_rw2_e = log(.1),
										beta_age      = .01,
										age_rw2       = rep(0, length(unique(data$age))),
										log_age_rw2_e = log(.1)
	)

	ll = NULL

	for (fold in 1:K) 
	{
		cat(fold,"...")
		# out of sample subset to get get pointwise LL
		oos = which(data$grp == fold)
		# all data and meta data
		idata = list(afs             = data$afs,
								afs_u           = data$afs + 1,
								afs_l           = data$afs,
								age             = data$age-min(data$age),
								event           = data$event,
								yob             = data$yob-min(data$yob),
								svw             = data$weight,
								to_skew         = to_skew,
								age_term 				= age_term,
								smooth_age      = smooth_age,
								yob_term        = yob_term,
								smooth_yob      = smooth_yob,
								interval_censor = 0,
								to_fit          = as.integer(data$grp != fold),
								sd_beta         = c(0, 1),
								sd_yob          = c(1, 1e-3),
								sd_age          = c(1, 1e-3),
								age_order       = age_order,
								yob_order       = yob_order,
								shape_prior     = c(0, 1),
								skew_prior      = c(0, 1),
								R_age           = INLA:::inla.rw(length(unique(data$age)), 1, F, T),
								R_yob           = INLA:::inla.rw(length(unique(data$yob)), 1, F, T))

		# change this depending on model, fixed parameters that are not estimated
		which_ones = NULL

		if (!to_skew)
			which_ones = c(which_ones, "log_skew")

		if (!yob_term)
			which_ones = c(which_ones, char(beta_yob, yob_rw2, log_yob_rw2_e))
		else {
			if (smooth_yob)
				which_ones = c(which_ones, "beta_yob")
			else
				which_ones = c(which_ones, char(yob_rw2, log_yob_rw2_e))
		}

		if (!age_term)
			which_ones = c(which_ones, char(beta_age, age_rw2, log_age_rw2_e))
		else {
			if (smooth_age)
				which_ones = c(which_ones, "beta_age")
			else
				which_ones = c(which_ones, char(age_rw2, log_age_rw2_e))
		}

		# set random effect
		re = NULL
		fe_only = TRUE
		re_only = FALSE
		if (smooth_age | smooth_yob) 
		{
			re = char(intercept, log_shape, log_skew, beta_yob, beta_age, yob_rw2, age_rw2)
			fe_only = FALSE
			re_only = TRUE
		}

		# model configs
		obj = MakeADFun(data 			 = idata, parameters = parameters,
										random 		 = re, map = tmb_fixit(parameters, which_ones),
										silent 		 = !verbose, DLL = 'model')
		fit = nlminb(obj$par, obj$fn, obj$gr)
		if (check) browser()
		if (fold == 1) 
			cat(head(names(fit$par), 10), "\n")
		smp = sample_tmb(list(obj = obj, fit = fit), S, fe_only, re_only)
		llk = apply(smp, 1, function(p) obj$report(p)$logliki)
		ll = rbind(ll, llk[oos, ])
	} # end fold
	cat("\n")
	ll
}
