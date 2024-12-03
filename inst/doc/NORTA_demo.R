## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
library(simdata)
library(fitdistrplus)
library(dplyr)
library(ggplot2)
library(patchwork)
library(ggcorrplot)

knitr::opts_chunk$set(
  error = TRUE,
  eval = requireNamespace("NHANES", quietly = TRUE)
)

## ----eval=TRUE, echo=TRUE, message=FALSE, warning=FALSE-----------------------
if (requireNamespace("NHANES", quietly = TRUE)) {
    df = NHANES::NHANES %>% 
        filter(SurveyYr == "2011_12") %>%
        select(Gender, Age, Race = Race1, Weight, 
               BMI, BPsys = BPSys1, BPdia = BPDia1) %>% 
        filter(complete.cases(.)) %>% 
        filter(Age > 18) %>% 
        mutate(Gender = if_else(Gender == "male", 1, 2), 
               Race = as.numeric(Race))
    
    print(head(df))
} else {
    message("Package 'NHANES' not available.")
}

## ----echo=TRUE----------------------------------------------------------------
cor_target = cor(df)
ggcorrplot::ggcorrplot(cor_target, lab = TRUE)

## ----echo=TRUE, results='hide', warning=FALSE---------------------------------
dist_auto = quantile_functions_from_data(df, n_small = 15) 

## ----echo=TRUE, results='hide', warning=FALSE---------------------------------
dist = list()

# gender
dist[["Gender"]] = function(x) qbinom(x, size = 1, prob = 0.5)

# age
dens = stats::density(df$Age, cut = 1) # cut defines how to deal with boundaries
# integrate
int_dens = cbind(Age = dens$x, cdf = cumsum(dens$y))
# normalize to obtain cumulative distribution function
int_dens[, "cdf"] = int_dens[, "cdf"] / max(int_dens[, "cdf"])
# derive quantile function
# outside the defined domain retun minimum and maximum age, respectively
dist[["Age"]] = stats::approxfun(int_dens[, "cdf"], int_dens[, "Age"], 
                          yleft = min(int_dens[, "Age"]), 
                          yright = max(int_dens[, "Age"]))

# race
dist[["Race"]] = function(x) 
    cut(x, breaks = c(0, 0.112, 0.177, 0.253, 0.919, 1), 
        labels = 1:5)

# weight
fit = fitdistrplus::fitdist(as.numeric(df$Weight), "gamma")
summary(fit)
dist[["Weight"]] = function(x) qgamma(x, shape = 16.5031110, rate = 0.2015375)

# bmi
fit = fitdistrplus::fitdist(as.numeric(df$BMI), "lnorm")
summary(fit)
dist[["BMI"]] = function(x) qlnorm(x, meanlog = 3.3283118, sdlog = 0.2153347)

# systolic blood pressure
fit = fitdistrplus::fitdist(as.numeric(df$BPsys), "lnorm")
summary(fit)
dist[["BPsys"]] = function(x) qlnorm(x, meanlog = 4.796213, sdlog = 0.135271)

# diastolic blood pressure
fit = fitdistrplus::fitdist(as.numeric(df %>% 
                                         filter(BPdia > 0) %>% 
                                         pull(BPdia)), "norm")
summary(fit)
dist[["BPdia"]] = function(x) qnorm(x, mean = 71.75758, sd = 11.36352)

## -----------------------------------------------------------------------------
# use automated specification
dsgn_auto = simdata::simdesign_norta(cor_target_final = cor_target, 
                                     dist = dist_auto, 
                                     transform_initial = data.frame,
                                     names_final = names(dist), 
                                     seed_initial = 1)

simdf_auto = simdata::simulate_data(dsgn_auto, nrow(df), seed = 2)

# use manual specification
dsgn = simdata::simdesign_norta(cor_target_final = cor_target, 
                                dist = dist, 
                                transform_initial = data.frame,
                                names_final = names(dist), 
                                seed_initial = 1)

simdf = simdata::simulate_data(dsgn, nrow(df), seed = 2)

## ----message=FALSE, warning=FALSE---------------------------------------------
summary(df)
summary(simdf_auto)
summary(simdf)

## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
ggcorrplot::ggcorrplot(cor(df), title = "Original", lab = TRUE)
ggcorrplot::ggcorrplot(cor(simdf_auto), title = "Simulated (automated)", lab = TRUE)
ggcorrplot::ggcorrplot(cor(simdf), title = "Simulated (manual)", lab = TRUE)

## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
vars = c("Age", "Weight", "BMI", "BPsys", "BPdia")
limits = list(
    Age = c(10, 90), 
    Weight = c(0, 225), 
    BMI = c(10, 80),
    BPsys = c(65, 240), 
    BPdia = c(0, 140)
)
plist = list()
for (i in seq_along(vars)) for (j in seq_along(vars)) {
    vari = vars[i]
    varj = vars[j]
    if (i == j) {
        p = ggplot(df, aes_string(x = vari)) + 
            geom_density() + 
            geom_density(data = simdf, color = "red") + 
            coord_cartesian(xlim = limits[[vari]])
    } else if (i < j) {
        p = ggplot(df[1:1000, ], aes_string(x = vari, y = varj)) + 
            geom_point(alpha = 0.04) + 
            coord_cartesian(xlim = limits[[vari]], ylim = limits[[varj]])
    } else {
        p = ggplot(simdf_auto[1:1000, ], aes_string(x = varj, y = vari)) + 
            geom_point(color = "red", alpha = 0.04) + 
            coord_cartesian(xlim = limits[[varj]], ylim = limits[[vari]])
    }
    plist = append(plist, list(p + theme_bw(base_size = 7)))
}

p = patchwork::wrap_plots(plist) + 
    patchwork::plot_annotation(title = "Simulated (automated)")
print(p)

plist = list()
for (i in seq_along(vars)) for (j in seq_along(vars)) {
    vari = vars[i]
    varj = vars[j]
    if (i == j) {
        p = ggplot(df, aes_string(x = vari)) + 
            geom_density() + 
            geom_density(data = simdf, color = "red") + 
            coord_cartesian(xlim = limits[[vari]])
    } else if (i < j) {
        p = ggplot(df[1:1000, ], aes_string(x = vari, y = varj)) + 
            geom_point(alpha = 0.04) + 
            coord_cartesian(xlim = limits[[vari]], ylim = limits[[varj]])
    } else {
        p = ggplot(simdf[1:1000, ], aes_string(x = varj, y = vari)) + 
            geom_point(color = "red", alpha = 0.04) + 
            coord_cartesian(xlim = limits[[varj]], ylim = limits[[vari]])
    }
    plist = append(plist, list(p + theme_bw(base_size = 7)))
}

p = patchwork::wrap_plots(plist) +
    patchwork::plot_annotation(title = "Simulated (manual)")
print(p)

## ----echo=FALSE---------------------------------------------------------------
sessionInfo()

