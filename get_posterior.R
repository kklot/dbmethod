get_posterior <- function(age_order = 1,
                          yob_order = 1,
                          data,
                          sample_size,
                          K = 1000, S = 100, verbose = F, check = F) {

    loop_fn <- function(svysmp) 
    {
        cat(svysmp, "...")
        # Generate a survey dataset

        fitdt <- data %>%
            group_by(svy) %>%
            mutate(take = id %in% sample(id, sample_size, prob = sampling_weight)) %>%
            filter(take) %>%
            select(svy, biased_afs, age, event, yob)

        # all data and meta data
        idata <- list(
            afs = fitdt$biased_afs,
            age = fitdt$age - min(fitdt$age),
            yob = fitdt$yob - min(fitdt$yob),
            event = fitdt$event,
            sd_beta = c(0, 1),
            sd_yob = c(1, 1e-1), # pc-prior
            sd_age = c(1, 1e-1), # pc-prior
            age_order = age_order,
            yob_order = yob_order,
            shape_prior = c(0, 1),
            skew_prior = c(0, 1),
            R_age = INLA:::inla.rw(length(unique(fitdt$age)), age_order, F, T),
            R_yob = INLA:::inla.rw(length(unique(fitdt$yob)), yob_order, F, T)
        )

        # initial parameters
        parameters <- list(
            intercept = -3,
            log_shape = log(10),
            log_skew = log(1),
            yob_rw2 = rep(0, length(unique(fitdt$yob))),
            age_rw2 = rep(0, length(unique(fitdt$age))),
            log_yob_rw2_e = log(1),
            log_age_rw2_e = log(1)
        )

        # model configs
        openmp(1)
        options(tape.parallel = FALSE, DLL = "model")
        
        obj <- MakeADFun(
            data = idata, parameters = parameters,
            random = char(yob_rw2, age_rw2),
            silent = !verbose, DLL = "model"
        )
  
        fit <- nlminb(obj$par, obj$fn, obj$gr)
  
        # sample posterior samples
        smp <- sample_tmb(list(obj = obj, fit = fit), S, FALSE, FALSE)

        attributes(smp)$yob <- sort(unique(fitdt$yob))
        attributes(smp)$age <- sort(unique(fitdt$age))
        attributes(smp)$age_id <- grep("age_rw2$", colnames(smp))
        attributes(smp)$yob_id <- grep("yob_rw2$", colnames(smp))
        
        if (check) {
            browser()
        }
        smp
    } # end a sample

    posterior <- parallel::mclapply(1:K, loop_fn)
    posterior
}
