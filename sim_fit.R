library(abind)
library(TMB)
library(data.table)
library(tidyverse)
library(ktools)

set.seed(123)
options(mc.cores = parallel::detectCores()-2)

# cluster stuffs
task_id <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
base_dir <- "/scratch/fuchs/fias/knguyen/"
save_to  <- paste0(base_dir, "bias_on_scale/")

dir.create(save_to, FALSE)

scenarios <- crossing(
    nsv = 2:5,
    sample_size = 1000,
    bias = char(none, men, women),
    trend = char(increase, decrease, none),
    theK = 100
)

scenarios

params <- as.list(scenarios[task_id, ])
params

myname <- unlist(params) %>%
    paste0(names(.), .) %>%
    paste0(collapse = "_") %>%
    paste0(".rds")
mypath <- paste0(save_to, myname)

## Pool population
N <- 10^6
birth_cohorts <- 1940:2005
wanted_cohort <- 1985:2005
elig_age      <- 15:49 # age eligible for including in the year of surveys

# take age distribution of SZ
sz = data.frame(
    age = c(
        15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,
        33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49
    ),
    n = c(
        525, 566, 514, 469, 443, 464, 351, 385, 321, 316, 262, 301, 273, 241,
        222, 245, 211, 188, 184, 185, 151, 184, 164, 235, 123, 151, 136, 144,
        109, 116, 108, 136, 124, 136, 81
    )
)
sz %>% {approxfun(.[,1], .[,2]/sum(.[,2]), yleft=0, yright=0)} -> age_weight

my_pop <- data.table(id  = 1:N, yob = sample(birth_cohorts, N, T), afs=0)

# choose survey year 5 year apart starting backward from 2020
# maximum 5
chosen_svy <- crossing(svy = 1990:2020, bch = wanted_cohort) %>%
    mutate(age = svy - bch) %>%
    filter(age >= 15) %>%
    group_by(svy) %>%
    summarise(min = min(age), max = max(age), miny = min(bch), maxy = max(bch)) %>%
    arrange(desc(svy)) %>%
    mutate(id = 1:nrow(.) %% 5) %>%
    filter(svy == 2020 | id == 0) %>%
    slice_head(n = params$nsv) %>%
    pull(svy)
chosen_svy

sk_scale <- function(median = 16, q = 0.5, shape = 10, skew = 1.5) {
    # Skew log logistic median
    # clipr::write_clip(
        # Ryacas::as_r(Ryacas::yac_str("Solve(y==1/scale*(-1+0.5^(-1/skew))^(-1/shape),scale)"))
    # )
    (0.5^((-1) / skew) - 1)^((-1) / shape) / median
  }

# AFS parameters and sampling
# Following skew log-logistic distribution, at the begining, year 1900
ref = list(scale = sk_scale(17), shape = 10, skew  = 1.5)
(min_scale = sk_scale(15))
(max_scale = sk_scale(19))

if (params$trend == "none") {
	pdata = tibble(yob = birth_cohorts, scale = ref$scale, skew = ref$skew, shape = ref$shape)
} else if (params$trend == "increase") {
	scalev = seq(min_scale, max_scale, length.out = length(birth_cohorts))
	pdata  = tibble(yob = birth_cohorts, scale = scalev, skew = ref$skew, shape = ref$shape)
} else if (params$trend == "decrease") {
	scalev = seq(max_scale, min_scale, length.out = length(birth_cohorts))
	pdata  = tibble(yob = birth_cohorts, scale = scalev, skew = ref$skew, shape = ref$shape)
}

pdata %<>% mutate(median = qskewlogis(.5, scale, shape, skew))

bias_lgt <- function(age, max = 4, r = .5, mid = 23) max / (1 + exp(-r * (age - mid))) - max / 2

# Generate biases
bias_f <- switch(params$bias,
    "none" = function(age) 0,
    "norm" = function(age, sd = 0.3) rnorm(1, 0, sd),
    "logis" = bias_lgt,
    "women" = function(age) .5 - bias_lgt(age, max = 3, r = .5),
    "men" = function(age) .5 + bias_lgt(age, max = 4, r = .35)
)

bias_f(15:49) %>% plott()

# Generate pooled and survey data
## Generate true afs
afsd = my_pop %>%
    # add reference parameters
    left_join(pdata, "yob") %>%
    arrange(yob)

afsd <- afsd %>%
    # generate surveys sample
    uncount(params$nsv, .id = "svy") %>%
    mutate(
        svy = chosen_svy[svy],
        age = svy - yob
    ) %>%
    # who in the eligible ages
    filter(age %in% 15:49) 

# generate bias 
afsd <- afsd %>%
    mutate(
        # generate biased in term of median age 
        true_median = qskewlogis(.5, scale, shape, skew),
        biased_median = true_median + bias_f(age) + rnorm(n(), 0, 1),
        # and convert back to biased in scale
        biased_scale = sk_scale(biased_median),
        biased_afs = rskewlogis(n(), biased_scale, shape, skew),
        afs = rskewlogis(n(), scale, shape, skew),
        sampling_weight = age_weight(age),
        # survival data format
        event = if_else(biased_afs <= age, 1, 0),
        # for censored obs
        biased_afs = if_else(event == 0, as.double(age), biased_afs),
        afs = if_else(event == 0, as.double(age), afs)
    )

# bias form in median
afsd %>%
    mutate(diff = biased_median - true_median) %>%
    group_by(svy, age) %>%
    summarise(med=median(diff))  %>%
    ggplot(aes(age, med)) + geom_line()

# bias form in scale
afsd %>%
    mutate(diff = biased_scale - scale) %>%
    group_by(svy, age) %>%
    summarise(med=median(diff))  %>%
    ggplot(aes(age, med, color=factor(svy))) + geom_line()

# Fit model
source("get_posterior.R")

# Load dll
# we do multiple fit on parallel so avoiding using it in TMB
openmp(1)
options(mc.cores = 1)
compile("model.cpp")
dyn.load(dynlib("model"))
invisible(config(tape.parallel = FALSE, DLL = "model"))
source("tmb_sampling.R")

# parallel within this
post = get_posterior(
    data = afsd, 
    sample_size = params$sample_size, 
    K = params$theK, 
    S = 50
)

attributes(post)$ref_par = ref 

saveRDS(post, mypath)
