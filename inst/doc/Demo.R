## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
library(simdata)
library(ggplot2)
library(reshape2)
library(GGally)
library(knitr)
library(doParallel)
library(doRNG)

## ----message=FALSE, warning=FALSE---------------------------------------------
correlation_matrix = diag(1, nrow = 5)

sim_design = simdata::simdesign_mvtnorm(relations = correlation_matrix, 
                                        transform_initial = data.frame,
                                        prefix_final = "variable")
sim_data = simdata::simulate_data(sim_design, n_obs = 100, seed = 25897165)

knitr::kable(head(sim_data))

p = GGally::ggpairs(sim_data, 
                    upper = list(continuous = "points"), 
                    progress = FALSE) +
  theme_bw()
print(p)

## ----message=TRUE, warning=TRUE-----------------------------------------------
correlation_matrix = diag(1, nrow = 5)
transformation = simdata::function_list(
  "v1" = function(x) x[, 1],
  "v2*2" = function(x) x[, 2] * 2, 
  "v3^2" = function(x) x[, 3]^2,
  "v4+v5" = function(x) x[, 4] + x[, 5], 
  check.names = FALSE # pass columnnames exactly as specified here
)

sim_design = simdata::simdesign_mvtnorm(
  relations = correlation_matrix,
  transform_initial = transformation, 
  prefix_final = NULL
)
sim_data = simdata::simulate_data(sim_design, n_obs = 100, seed = 25897165)

knitr::kable(head(sim_data))

p = GGally::ggpairs(sim_data, upper = list(continuous = "points"), 
                    progress = FALSE) +
  theme_bw()
print(p)

## -----------------------------------------------------------------------------
correlation_matrix = simdata::cor_from_upper(
  15, 
  rbind(c(1,2,0.8), c(1,9,0.3), 
        c(3,5,0.3), c(3,9,-0.5), 
        c(4,6,-0.5), c(4,7,-0.3),
        c(5,6,-0.3), c(5,12,0.5),
        c(6,7,0.5), c(6,11,0.5), c(6,14,0.3),
        c(7,11,0.3), c(7,14,0.3),
        c(8,9,-0.3), c(8,11,0.3),
        c(11,14,0.5)))

transformation = simdata::function_list(
  v1 = function(z) floor(10 * z[,1] + 55), 
  v2 = function(z) z[,2] < 0.6, 
  v3 = function(z) exp(0.4 * z[,3] + 3),
  v4 = function(z) z[,4] >= -1.2,
  v5 = function(z) z[,4] >= 0.75,
  v6 = function(z) exp(0.5 * z[,5] + 1.5),
  v7 = function(z) floor(pmax(0, 100 * exp(z[,6]) - 20)),
  v8 = function(z) floor(pmax(0, 80 * exp(z[,7]) - 20)),
  v9 = function(z) z[,8] < -0.35,
  v10 = function(z) (z[,9] >= 0.5) & (z[,9] < 1.5),
  v11 = function(z) z[,9] >= 1.5,
  v12 = function(z) 0.01*floor(100 * (z[,10] + 4)^2),
  v13 = function(z) floor(10 * z[,11] + 55),
  v14 = function(z) floor(10 * z[,12] + 55),
  v15 = function(z) floor(10 * z[,13] + 55),
  v16 = function(z) z[,14] < 0,
  v17 = function(z) z[,15] < 0
)

sim_design = simdata::simdesign_mvtnorm(
  relations = correlation_matrix,
  transform_initial = transformation,
  prefix_final = NULL
)

## ----echo=FALSE---------------------------------------------------------------
plotdf = melt(sim_design$cor_initial)

ggplot(plotdf, aes(x = factor(Var1), y = factor(Var2), fill = value)) + 
    geom_tile() + 
    geom_text(aes(label = value)) + 
    scale_x_discrete(position = "top") +
    scale_y_discrete(limits = rev) +
    xlab(expr(paste("Initial variable ", z[i]))) + 
    ylab(expr(paste("Initial variable ", z[i]))) +
    coord_equal() +
    scale_fill_gradient2(
        "Correlation",
        limits = c(-1, 1),
        low = "#377eb8", mid = "white", high = "#e41a1c") +
    theme_bw()

## -----------------------------------------------------------------------------
simdata::plot_cor_network(sim_design, seed = 1)

## -----------------------------------------------------------------------------
sim_data = simdata::simulate_data(sim_design, n_obs = 100, seed = 25897165)

## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
numeric_vars = names(sim_design$types_final)[sim_design$types_final == "numeric"]
plotdf = melt(sim_data[, numeric_vars], measure.vars = numeric_vars)

p = ggplot(plotdf, aes(x = "", y = value)) +
    geom_violin() +
    geom_dotplot(binaxis = "y", stackdir = "center") + 
    facet_wrap(~ variable, scales = "free", nrow = 3) +
    theme_bw() + 
    ylab("Value") +
    theme(axis.title.x = element_blank())
print(p)

discrete_vars = names(sim_design$types_final)[sim_design$types_final != "numeric"]
plotdf = melt(sim_data[, discrete_vars], measure.vars = discrete_vars)

p = ggplot(plotdf, aes(x = variable, fill = value, color = value)) +
    geom_bar(alpha = 0.5, size = 1) +
    scale_fill_brewer(palette = "Set1") +
    scale_color_brewer(palette = "Set1") +
    ylab("Proportion of total samplesize (%)") +
    theme_bw() +
    theme(axis.title.x = element_blank())
print(p)

## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
plotdf = cor(sim_data, method = "s", use = "p")
plotdf = melt(plotdf) 

ggplot(plotdf, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() + 
    geom_text(aes(label = round(value, 2)), size = 2) +
    scale_fill_gradient2(
        "Correlation",
        limits = c(-1, 1),
        low = "#377eb8", mid = "white", high = "#e41a1c") +
    coord_equal() +
    scale_x_discrete(position = "top") +
        scale_y_discrete(limits = rev) +
    xlab(expr(paste("Final variable ", v[i]))) + 
    ylab(expr(paste("Final variable ", v[i]))) +
    theme_bw() + 
    theme(text = element_text(size = 10))

## ----message=FALSE, warning=FALSE---------------------------------------------
# draw full network
simdata::plot_estimated_cor_network(sim_design, 
                                    cor_cutoff = NULL, 
                                    edge_label_function = NULL,
                                    edge_width_function = function(x) x*25,
                                    use_edge_weights = TRUE, 
                                    edge.color = "clipped-ramp",
                                    seed = 2321673)

## ----message=FALSE, warning=FALSE---------------------------------------------
# simplify by using cor_cutoff
simdata::plot_estimated_cor_network(sim_design, 
                                    seed = 2)

# set correlation type
simdata::plot_estimated_cor_network(sim_design, 
                                    cor_type = "spearman", 
                                    seed = 2321673)

# set various parameters
simdata::plot_estimated_cor_network(sim_design, seed = 2321673, 
                                    edge.color = "red-blue", 
                                    axes = TRUE, cor_type = "s", 
                                    edge_width_function = function(x) var(x)*200,
                                    show_categorical = FALSE, 
                                    mar = c(2, 2, 0, 0))

## -----------------------------------------------------------------------------
sim_design$process_final = list(
    process_truncate_by_iqr = list(
        truncate_multipliers = c(v6 = 2, v7 = 2, v8 = 2)
    )
)

sim_data = simdata::simulate_data(sim_design, n_obs = 100, seed = 25897165)

## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
numeric_vars = names(sim_design$types_final)[sim_design$types_final == "numeric"]
plotdf = melt(sim_data[, numeric_vars])

p = ggplot(plotdf, aes(x = "", y = value)) +
    geom_violin() +
    geom_dotplot(binaxis = "y", stackdir = "center") + 
    facet_wrap(~ variable, scales = "free", nrow = 3) +
    theme_bw() + 
    ylab("Value") +
    theme(axis.title.x = element_blank())
print(p)

## -----------------------------------------------------------------------------
sim_design$process_final = list(
    process_truncate_by_threshold = list(
        truncate_upper = c(v8 = 200, v7 = 300),
        truncate_lower = c(v6 = 2)
    )
)

sim_data = simdata::simulate_data(sim_design, n_obs = 100, seed = 25897165)

## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
numeric_vars = names(sim_design$types_final)[sim_design$types_final == "numeric"]
plotdf = melt(sim_data[, numeric_vars])

ggplot(plotdf, aes(x = "", y = value)) +
    geom_violin() +
    geom_dotplot(binaxis = "y", stackdir = "center") + 
    facet_wrap(~ variable, scales = "free", nrow = 3) +
    theme_bw() + 
    ylab("Value") +
    theme(axis.title.x = element_blank())

## -----------------------------------------------------------------------------
sim_design$process_final = list(
    process_truncate_by_iqr = list(
        truncate_multipliers = c(v6 = 2, v7 = 2, v8 = 2)
    ), 
    scale = list(), 
    data.frame = list()
)

sim_data = simdata::simulate_data(sim_design, n_obs = 100, seed = 25897165)

## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
numeric_vars = names(sim_design$types_final)[sim_design$types_final == "numeric"]
plotdf = melt(sim_data[, numeric_vars])

ggplot(plotdf, aes(x = "", y = value)) +
    geom_violin() +
    geom_dotplot(binaxis = "y", stackdir = "center") + 
    facet_wrap(~ variable, scales = "free", nrow = 3) +
    theme_bw() + 
    ylab("Value") +
    theme(axis.title.x = element_blank())

## -----------------------------------------------------------------------------
correlation_matrix = diag(1, nrow = 2)
transformation = simdata::function_list("v1" = function(x) x[, 1],
                                        "v1*2" = function(x) x[, 1] * 2, 
                                        check.names = FALSE)

sim_design = simdata::simdesign_mvtnorm(
  relations = correlation_matrix,        
  transform_initial = transformation, 
  prefix_final = NULL
)

# ignoring the collinearity
sim_data = simdata::simulate_data(sim_design, n_obs = 100, seed = 2)
knitr::kable(cor(sim_data))

# rejecting collinear matrices
sim_data = simdata::simulate_data_conditional(sim_design,
                                              n_obs = 100, seed = 2,
                                              reject = is_collinear,
                                              reject_max_iter = 3,
                                              return_tries = TRUE)

sim_data

## -----------------------------------------------------------------------------
correlation_matrix = diag(1, nrow = 3)
transformation = simdata::function_list(
  "v1" = function(x) x[, 1],
  "might_be_collinear" = function(x) {
    if (rbinom(1, 1, 0.5)) {
      return(x[, 1] * 2)
    } else return(x[, 2])
  }, 
  "might_be_constant" = function(x) {
    if (rbinom(1, 1, 0.5)) {
      return(0)
    } else return(x[, 3])
  },
  check.names = FALSE)

sim_design = simdata::simdesign_mvtnorm(
  relations = correlation_matrix,        
  transform_initial = transformation,
  prefix_final = NULL
)

sim_data = simdata::simulate_data_conditional(
  sim_design,
  n_obs = 100, seed = 3,
  reject = simdata::function_list(is_collinear, 
                                  contains_constant),
  reject_max_iter = 3, 
  return_tries = TRUE)

sprintf("Number of tries: %d", sim_data[[2]])
knitr::kable(head(sim_data[[1]]))

## -----------------------------------------------------------------------------
binomial_simdesign <- function(size = 1, prob = 0.5, ...) {
  
  # define generator function
  # make sure it returns a two-dimensional array
  generator = function(n) matrix(rbinom(n, size = size, prob = prob), ncol = 1)
  
  # setup simdesign object
  # make sure to pass generator function and ...
  # all other information passed is optional
  dsgn = simdata::simdesign(
    generator = generator, 
    size = size, 
    prob = prob,
    ...
  )
  
  # extend the class attribute 
  class(dsgn) = c("binomial_simdesign", class(dsgn))
  
  # return the object
  dsgn
}

## -----------------------------------------------------------------------------
sim_design = binomial_simdesign(size = 1, prob = 0.7)
sim_data = simdata::simulate_data(sim_design, 100, seed = 1)
knitr::kable(table(sim_data))

## -----------------------------------------------------------------------------
realdata_simdesign <- function(dataset, ...) {
  
  # define generator function
  # make sure it returns a two-dimensional array
  generator = function(n) dataset[sample(1:nrow(dataset), n, replace = TRUE), ,
                                  drop = FALSE]
  
  # setup simdesign object
  # make sure to pass generator function and ...
  # all other information passed is optional
  dsgn = simdata::simdesign(
    generator = generator, 
    dataset = dataset,
    ...
  )
  
  # extend the class attribute 
  class(dsgn) = c("realdata_simdesign", class(dsgn))
  
  # return the object
  dsgn
}

## -----------------------------------------------------------------------------
data(iris)
sim_design = realdata_simdesign(iris, prefix_final = NULL)
sim_data = simdata::simulate_data(sim_design, 100, seed = 1)
knitr::kable(head(sim_data))

## -----------------------------------------------------------------------------
correlation_matrix = simdata::cor_from_upper(
  100,
  entries = rbind(
    expand.grid(1:30, 1:30, 0.5), 
    expand.grid(31:50, 31:50, 0.2))
)

# create list of transformation functions programmatically
# For the first 60 variables: 
# odd varibles will be translated
# even variables will be scaled
transformation = list()
for (i in 1:60) {
  if (i %% 2) {
    transformation[[i]] = substitute(function(x) x[, i] * 5, list(i = i))
  } else transformation[[i]] = substitute(function(x) x[, i] - 10, list(i = i))
}
# the remaining are returned as they are
transformation[[61]] = function(x) x[, 61:100]
# construct single transformation function from the list
transformation = simdata::as_function_list(transformation)

# create simulation design
sim_design = simdata::simdesign_mvtnorm(
  relations = correlation_matrix, 
  transform_initial = transformation
)

## ----echo=FALSE---------------------------------------------------------------
plotdf = melt(sim_design$cor_initial)

ggplot(plotdf, aes(x = factor(Var1), y = factor(Var2), fill = value)) + 
    geom_tile() + 
    scale_x_discrete(position = "top") +
    scale_y_discrete(limits = rev) + 
    xlab(expr(paste("Initial variable ", z[i]))) + 
    ylab(expr(paste("Initial variable ", z[i]))) +
    coord_equal() +
    scale_fill_gradient2(
        "Correlation",
        limits = c(-1, 1),
        low = "#377eb8", mid = "white", high = "#e41a1c") +
    theme_bw() + 
    theme(axis.text = element_blank())

## -----------------------------------------------------------------------------
simdata::plot_cor_network(sim_design, seed = 2,  
                          vertex_labels = NA,
                          edge_label_function = NULL,
                          edge_width_function = function(x) 0.01,
                          edge_weight_function = function(x) 0.25 * x, 
                          use_edge_weights = TRUE, 
                          edge.color = "clipped-ramp", 
                          vertex.size = 3)

## -----------------------------------------------------------------------------
sim_data = simdata::simulate_data(sim_design, n_obs = 50, seed = 5)

## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
ggplot(melt(sim_data), aes(x = value, color = variable)) + 
    geom_density() + 
    scale_color_viridis_d(option = "plasma") +
    guides(color = "none") +
    xlab("Value") + ylab("Density") +
    theme_bw()

## ----eval=FALSE, include=TRUE-------------------------------------------------
#  correlation_matrix = diag(1, nrow = 5)
#  transformation = simdata::function_list(
#      "v1" = function(x) x[, 1],
#      "v2*2" = function(x) x[, 2] * 2,
#      "v3^2" = function(x) x[, 3]^2,
#      "v4+v5" = function(x) x[, 4] + x[, 5],
#      check.names = FALSE # pass columnnames exactly as specified here
#    )
#  
#  sim_design = simdata::simdesign_mvtnorm(
#    relations = correlation_matrix,
#    transform_initial = transformation,
#    prefix_final = NULL
#  )
#  
#  # parallelisation
#  cl = parallel::makeCluster(1)
#  doParallel::registerDoParallel(cl)
#  res = foreach(
#    i = 1:10,
#    .packages = c("simdata"),
#    .options.RNG = 1 # note that the seed is passed here
#  ) %dorng% {
#    simulate_data(sim_design, n_obs = 100)
#  
#    # do some task with the simulated data
#  }
#  parallel::stopCluster(cl)
#  
#  knitr::kable(purrr::map(res[1:2], summary))

## -----------------------------------------------------------------------------
# parameter
mult = 2
transformation = simdata::function_list(
  "v1" = function(x) x[, 1] * mult # dangerous, depends on global variable!
)

sim_design = simdata::simdesign_mvtnorm(
        relations = diag(1), 
        transform_initial = transformation
)

sample1 = simdata::simulate_data(sim_design, n_obs = 5, seed = 25897165)

# change value of global variable
mult = 4
sample2 = simdata::simulate_data(sim_design, n_obs = 5, seed = 25897165)

# note the different values in both columns
knitr::kable(cbind(sample1, sample2))

## -----------------------------------------------------------------------------
# parameter
mult = 2
transformation = simdata::function_list(
    # specify function as partial 
    "v1" = simdata::partial(function(x, mult) x[, 1] * mult, mult = mult)
)

sim_design = simdata::simdesign_mvtnorm(
        relations = diag(1), 
        transform_initial = transformation
)

sample1 = simdata::simulate_data(sim_design, n_obs = 5, seed = 25897165)

# change value of global variable
mult = 4
sample2 = simdata::simulate_data(sim_design, n_obs = 5, seed = 25897165)

# note both columns are equal now, as expected
knitr::kable(cbind(sample1, sample2))

## ----echo=FALSE---------------------------------------------------------------
sessionInfo()

