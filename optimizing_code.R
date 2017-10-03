# code for lecture/exercise in quant 3
# improving and optimizing R code
# Adam Lauretig, October 4, 2017
rm(list = ls())
options(stringsAsFactors = FALSE)
seed_to_use <- 216
set.seed(seed_to_use)

# What are the basic types of data you'll encounter in R?
# Everything is a vector, and vector size is determined at creation
vector_1 <- seq_len(100)
# this creates a vector of length 100, and populates with a sequence of integers

# What can we put in vectors? There are a variety of variable types (modes)
# Integers: positive counting numbers
a <- as.integer(1)
a2 <- 1L
# Numerics: Integers, plus decimals and negative numbers
b <- as.numeric(1.1)
b2 <- 1.1
# character vectors: text values
c <- as.character(1)
c2 <- "1" 

# every value can have only one type (mode)
# use str() to find out the mode
str(a)
str(b)
str(c)

# why does this matter?
a <- as.integer(seq(from = 1, to = 1000000, by = 1))
b <- as.numeric(seq(from = 1, to = 1000000, by = 1))
c <- as.character(seq(from = 1, to = 1000000, by = 1))

object.size(a)
object.size(b)
object.size(c)


# R coereces to the "broadest" category
d <- c(1L, 2.1, "3")

# what other data types can we use?
# a matrix: a vector with additional attributes -- number of rows and columns
mat_a <- matrix(data = as.integer(seq(from = 1, to = 1000, by = 1)), 
  nrow = 10, ncol = 100)
# we'll see the same sorts of memory issues here
mat_b <-  matrix(data = as.numeric(seq(from = 1, to = 1000, by = 1)), 
  nrow = 10, ncol = 100)
mat_c <-  matrix(data = as.character(seq(from = 1, to = 1000, by = 1)), 
  nrow = 10, ncol = 100)

object.size(mat_a)
object.size(mat_b)
object.size(mat_c)


# but what if we want to include labels?
# let's make some toy labels
var_labels <- letters[1:10]
labels_mat <- cbind(var_labels, mat_a) #this turns everything into a character
# we could use a list, that lets us combine different modes
matrix_list <- list(labels_to_use= var_labels, mat_a)
# or make a data frame
labels_df <- data.frame(var_labels, mat_a)
# how does size vary
object.size(mat_a)
object.size(labels_mat)
object.size(matrix_list)
object.size(labels_df)

#####
# Iterating: going through a vector and doing things to each element
#####
# we'll demonstrate the efficiency of various approaches with the "system.time()"
# command, which tells us how much time the computer used to do something

# the slow way: "grow" a vector
# example: multiply every number from 1 to 10000 by 2

# system.time: a command to measure how long various commands take

output1 <- c()
system.time(
for(i in 1:1000000){
  output1[i] <- i * 2
}
)


# but what if we pre-allocate memory?
output2 <- rep(NA, 1000000)
system.time(
for(i in 1:1000000){
  output2[i] <- i * 2
}
)


# 3 outputs:
# user: total user time
# system: total cpu time
# elapsed: wall-clock time


# we can also vectorize our code: apply a function "*" to a vector directly
vec_1 <- seq( 1:1000000)
output_3 <- c()
system.time(output_3 <- vec_1 * 2)

output_4 <- rep(NA, 1000000)
system.time(output_4 <- vec_1 * 2)

# we can also vectorize code over matrices/dataframes

# compare
mat_cols_for <- rep(NA, ncol(mat_a))
system.time(
for(i in 1:ncol(mat_a)){
 mat_cols_for[i] <- mean(mat_a[,i])
}
)
system.time(
mat_cols_vec <- colMeans(mat_a)
)

# or operations applied to a matrix

system.time(
for(i in 1:ncol(mat_a)){
  return(mat_a[,i] * 2)
}
)

system.time(
mat_a *2
)



######
# So what? - when would I use this?
######
# Ex:
# bootstrapping a model
# we'll do two things: 
# vectorizing, and comparing applying a function to a for loop


rows_test <- 300
cols_test <- 10

# what does this function do?
create_matching_data <- function(rows = 1000, columns = 5, nonlinear = TRUE){
  simulated_data <- as.data.frame(matrix(rnorm(rows*columns), rows, columns))
  if(nonlinear == TRUE){
    simulated_data[, (floor(columns*.4):columns)] <- 
      simulated_data[, (floor(columns*.4):columns)] + sin(
      simulated_data[, (floor(columns*.4):columns)])^2
  }
  as.matrix(simulated_data)
}

sample_data1 <- create_matching_data(
  rows = rows_test, columns = cols_test, nonlinear = TRUE)
sample_data1 <- cbind(1, sample_data1)
colnames(sample_data1) <- paste0("A", seq(1:ncol(sample_data1)))
covariates <- as.matrix(rnorm(ncol(sample_data1), 0, 5))
y <- sample_data1 %*% covariates + rnorm(rows_test, 0, 3)
sample_data <- data.frame(y = y, sample_data1[, 2:ncol(sample_data1)])

# formula for our equation
formula1 <- as.formula(paste("y ~", paste(colnames(sample_data)[2:ncol(sample_data)], 
  collapse = " + ")))

m1 <- lm(formula1, data = sample_data)


# pre_allocate the space
model_out <- matrix(NA, nrow = 10000, ncol = cols_test + 1)
system.time(
for(i in 1:10000){
  set.seed(i)
  df <- sample_data[ sample.int(n = nrow(sample_data), size = nrow(sample_data), replace = TRUE), ]
  m_boot <- lm(formula1, data = df)
  model_out[i,] <- m_boot$coefficients
  }
)
# now, we'll write a function for bootstrapping
boot_glm <- function(j, data_to_use = sample_data, form_to_use = formula1){
  set.seed(i)
  df <- data_to_use[ sample.int(n = nrow(data_to_use), 
    size = nrow(data_to_use), replace = TRUE), ]
  m_boot <- lm(form_to_use, data = df)
  m_boot$coefficients
}
system.time(
  model_out_2 <- do.call(rbind, lapply(1:10000, boot_glm, 
    data_to_use = sample_data, form_to_use = formula1))
)


## Now, we'll turn to some tools to speed up the implementation of our code
# we can compile our function, to make it even faster
library(compiler)
enableJIT(level = 3)
boot_glm_c <- cmpfun(boot_glm)
system.time(
model_out_3 <- do.call(rbind, lapply(1:10000, boot_glm_c, 
  data_to_use = sample_data, form_to_use = formula1))
)

# we can also use the data.table package, which is an incredibly fast 
# version of data frame for huge datasets (I use it in a lot of my work)

library(data.table)
sample_data_dt <- as.data.table(sample_data)
boot_glm_dt <- function(j, data_to_use = sample_data_dt, form_to_use = formula1){
  set.seed(j)
  dt <- data_to_use[sample.int(n = nrow(data_to_use), 
    size = nrow(data_to_use), replace = TRUE), ]
  m_boot <- lm(formula1, data = dt)
  data.table(m_boot$coefficients)
}
system.time(
model_out_4 <-rbindlist(lapply(1:10000, boot_glm_dt, 
  data_to_use = sample_data_dt, form_to_use = formula1))
)
# Here, no performance gain, but we can use data.table to speed up & 
# simplify calculating means and other grouped operations
# calculate descriptives by species from the iris data set
data("iris")
iris_dt <- data.table(iris)
iris_dt[,.(SL = mean(Sepal.Length), SW = mean(Sepal.Width), PL = mean(Petal.Length), 
  PW = mean(Petal.Width)), by = Species]

# There's a very useful cheatsheet here: 
# https://s3.amazonaws.com/assets.datacamp.com/blog_assets/datatable_Cheat_Sheet_R.pdf

# Finally, if we really need a speed boost, we can distribute a function to 
# multiple cores, so that each core will only need to do a fraction of the iterations

library(parallel) # mac/unix package
# windows users, go here: https://stackoverflow.com/questions/23926334/how-do-i-parallelize-in-r-on-windows-example

# get the number of cores to use with detectCores()
detectCores()
system.time(
model_out_5 <- do.call(rbind, mclapply(1:10000, boot_glm_c, 
  data_to_use = sample_data, form_to_use = formula1,
  mc.cores = detectCores()))
)

# When can we parallelize?
# What Hadley Wickham calls "split-apply-combine"
# Split: divide the data according to some group membership (what is it here)
# Apply: apply the function you want to apply
# Combine: at the end - put all the computations back together


# What if this isn't enough? Then, we go to the cluster

# We can also use Rcpp-- calling c++ code in R
## Eddelbuettel, book available as pdf from library
## https://link.springer.com/book/10.1007%2F978-1-4614-6868-4

### Other resources
# Norman Matloff: Art of R programming
# Hadley Wickham: Advanced R http://adv-r.had.co.nz/
# Parallel processing in Caret: https://topepo.github.io/caret/parallel-processing.html
