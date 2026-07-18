############### Benjamin Wildermuth brms ##################
# code for the models of the manuscript "Linking arthropod community assembly and biomass change along real-world and experimental grassland gradients"
# confidential, do not share

library(brms)

######## Exploratories  #############

dat1 <- read.csv2("Input_explos.csv", dec = ("."), stringsAsFactors = F)

# biomass priors to ensure convergence & transitions 
# (weak priors, see manuscript & supplement)

priors_bm <- c(
  
  ## -----------------------
  ## Intercepts
  ## -----------------------
  prior(normal(-430, 300), class = "Intercept", resp = "SREL"),
  prior(normal( 400, 300), class = "Intercept", resp = "SREG"),
  prior(normal(  58, 200), class = "Intercept", resp = "SIEL"),
  prior(normal( -70, 200), class = "Intercept", resp = "SIEG"),
  prior(normal(  12, 500), class = "Intercept", resp = "CDE"),
  
  ## -----------------------
  ## Fixed effects (ALL responses)
  ## -----------------------
  prior(normal(0, 100), class = "b", resp = "SREL"),
  prior(normal(0, 100), class = "b", resp = "SREG"),
  prior(normal(0,  70), class = "b", resp = "SIEL"),
  prior(normal(0,  70), class = "b", resp = "SIEG"),
  prior(normal(0,  40), class = "b", resp = "CDE"),
  
  ## -----------------------
  ## Residual SDs (ALL responses)
  ## -----------------------
  prior(student_t(3, 0, 400), class = "sigma", resp = "SREL"),
  prior(student_t(3, 0, 400), class = "sigma", resp = "SREG"),
  prior(student_t(3, 0, 300), class = "sigma", resp = "SIEL"),
  prior(student_t(3, 0, 300), class = "sigma", resp = "SIEG"),
  prior(student_t(3, 0, 700), class = "sigma", resp = "CDE"),
  
  ## -----------------------
  ## Degrees of freedom
  ## -----------------------
  prior(gamma(2, 0.1), class = "nu"),
  
  ## -----------------------
  ## Residual correlations
  ## -----------------------
  prior(lkj(2), class = "rescor"),
  
  ## -----------------------
  ## Random-effect SDs (FULLY specified for all 3 grouping factors)
  ## -----------------------
  # plot.x
  prior(student_t(3, 0, 250), class = "sd", group = "plot.x", resp = "SREL"),
  prior(student_t(3, 0, 250), class = "sd", group = "plot.x", resp = "SREG"),
  prior(student_t(3, 0, 200), class = "sd", group = "plot.x", resp = "SIEL"),
  prior(student_t(3, 0, 200), class = "sd", group = "plot.x", resp = "SIEG"),
  prior(student_t(3, 0, 300), class = "sd", group = "plot.x", resp = "CDE"),
  
  # plot.y
  prior(student_t(3, 0, 250), class = "sd", group = "plot.y", resp = "SREL"),
  prior(student_t(3, 0, 250), class = "sd", group = "plot.y", resp = "SREG"),
  prior(student_t(3, 0, 200), class = "sd", group = "plot.y", resp = "SIEL"),
  prior(student_t(3, 0, 200), class = "sd", group = "plot.y", resp = "SIEG"),
  prior(student_t(3, 0, 300), class = "sd", group = "plot.y", resp = "CDE"),
  
  # year.x (new grouping factor; use slightly smaller scales expecting less variation)
  prior(student_t(3, 0, 200), class = "sd", group = "year.x", resp = "SREL"),
  prior(student_t(3, 0, 200), class = "sd", group = "year.x", resp = "SREG"),
  prior(student_t(3, 0, 150), class = "sd", group = "year.x", resp = "SIEL"),
  prior(student_t(3, 0, 150), class = "sd", group = "year.x", resp = "SIEG"),
  prior(student_t(3, 0, 250), class = "sd", group = "year.x", resp = "CDE")
)

# model fit biomass change

fit_mv_explo <- brm(
  mvbind(SRE.L, SRE.G, SIE.L, SIE.G, CDE) ~ # Price components defined in manuscript
    scale(diff) * PM_cat + # diff = delta LUI, PM_cat = land-use type
    (1 | year.x) + (1 | plot.x) + (1 | plot.y),
  data = dat1,
  family = student(),
  prior = priors_bm,
  chains = 4, cores = 4, iter = 6000, warmup = 2000,
  control = list(adapt_delta = 0.99, max_treedepth = 14))

# species richness priors

priors_sr <- c(
  
  ## -----------------------
  ## Intercepts
  ## -----------------------
  prior(normal(-13, 7), class = "Intercept", resp = "SLrich"),
  prior(normal( 12, 7), class = "Intercept", resp = "SGrich"),
  
  ## -----------------------
  ## Fixed effects
  ## -----------------------
  prior(normal(0, 1.3), class = "b", resp = "SLrich"),
  prior(normal(0, 1.3), class = "b", resp = "SGrich"),
  
  ## -----------------------
  ## Residual SDs
  ## -----------------------
  prior(student_t(3, 0, 4.7), class = "sigma", resp = "SLrich"),
  prior(student_t(3, 0, 4.7), class = "sigma", resp = "SGrich"),
  
  ## -----------------------
  ## Degrees of freedom
  ## -----------------------
  prior(gamma(2, 0.1), class = "nu"),
  
  ## -----------------------
  ## Residual correlations
  ## -----------------------
  prior(lkj(2), class = "rescor"),
  
  ## -----------------------
  ## Random-effect SDs (plot.x)
  ## -----------------------
  prior(student_t(3, 0, 3), class = "sd",
        group = "plot.x", coef = "Intercept", resp = "SLrich"),
  prior(student_t(3, 0, 3), class = "sd",
        group = "plot.x", coef = "Intercept", resp = "SGrich"),
  
  ## -----------------------
  ## Random-effect SDs (plot.y)
  ## -----------------------
  prior(student_t(3, 0, 3), class = "sd",
        group = "plot.y", coef = "Intercept", resp = "SLrich"),
  prior(student_t(3, 0, 3), class = "sd",
        group = "plot.y", coef = "Intercept", resp = "SGrich")
)

# model fit species richness change

fit_mv_explo_rich <- brm(
  mvbind(
    SL.rich, SG.rich # species loss & gain
  ) ~ scale(diff) * PM_cat + # diff = delta LUI, PM_cat = land-use type
    (1 |  year.x) + (1 | plot.x) + (1 | plot.y),
  data = dat1,
  family = student(),
  prior = priors_sr,
  chains = 4, cores = 4, iter = 6000, warmup = 2000,
  control = list(adapt_delta = 0.99, max_treedepth = 14))

########### Jena Experiment #################

dat2 <- read.csv2("Input_jena.csv", dec = ("."), stringsAsFactors = F)

# model fit biomass (no priors needed)

fit_mv_jena <- brm(
  mvbind(
    SRE.L, SRE.G, SIE.L, SIE.G, CDE # Price components defined in manuscript
  ) ~ scale(-diff) * scale(FG_diff) + # diff = delta PSR, FG_diff = delta plant functional group richness
    (1 |  year.x) + (1 | plot.x) + (1 | plot.y),
  data = dat2,
  family = student(),
  chains = 4, cores = 4, iter = 4000, warmup = 1000,
  control = list(adapt_delta = 0.99, max_treedepth = 12))

# model fit species richness change

fit_mv_jena_rich <- brm(
  mvbind(
    SL.rich, SG.rich # species loss & gain
  ) ~ scale(diff) * scale(FG_diff) +  # diff = delta PSR, FG_diff = delta plant functional group richness
    (1 |  year.x) + (1 | plot.x) + (1 | plot.y),
  data = dat2,
  family = student(),
  chains = 4, cores = 4, iter = 4000, warmup = 1000,
  control = list(adapt_delta = 0.99, max_treedepth = 12))