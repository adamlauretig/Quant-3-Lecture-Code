# code for lecture/exercise in quant 3
# improving and optimizing R code
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
# Numerics: Integers, plus decimals and negative numbers
b <- as.numeric(1.1)
# character vectors: text values
c <- as.character(1)
c <- "1" # same thing

# every value can have only one type (mode)
# use str() to find out the mode
str(a)
str(b)
str(c)

# why does this matter?
a <- as.integer(seq(from = 1, to = 1000000, by = 1))
b <- as.numeric(seq(from = 1, to = 1000000, by = 1))
c <- as.character(seq(from = 1, to = 1000000, by = 1))

# R coereces to the "broadest" category
d <- c(1L, 2.1, "3")

# what other data types can we use?
# a matrix: a vector with additional attributes -- number of rows and columns
mat_a <- matrix(data = as.integer(seq(from = 1, to = 1000, by = 1)), nrow = 10, ncol = 100)
# but what if we want to include labels?
var_labels <- letters[1:10]
labels_mat <- cbind(var_labels, mat_a) #this turns everything into a character
# we could use a list, that let's us combine different mode
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

# we can also vectorize our code: apply a function "*" to a vector directly
vec_1 <- seq( 1:1000000)
output_3 <- c()
system.time(output_3 <- vec_1 * 2)

output_4 <- rep(NA, 1000000)
system.time(output_4 <- vec_1 * 2)

# we can also vectorize code over matrices/dataframes
colMeans(mat_a)
rowMeans(mat_a)

######
# So what? - when would I use this?
######
# Ex:
# bootstrapping a model
# we'll do two things: 
# vectorizing, and comparing applying a function to a for loop
data(iris)
iris$binarize <- ifelse(iris$Species == "setosa", 1, 0)
m1 <- glm(binarize ~ 
    Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris)

# pre_allocate the space
model_out <- matrix(NA, nrow = 10000, ncol = 5)
system.time(
for(i in 1:10000){
  set.seed(i)
  df <- iris[ sample.int(n = nrow(iris), size = nrow(iris), replace = TRUE), ]
  m_boot <- glm(binarize ~ 
    Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = df)
  model_out[i,] <- m_boot$coefficients
  }
)
# now, we'll write a function for bootstrapping
boot_glm <- function(j, data_to_use = iris){
  set.seed(i)
  df <- iris[ sample.int(n = nrow(iris), size = nrow(iris), replace = TRUE), ]
  m_boot <- glm(binarize ~ 
    Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = df)
  m_boot$coefficients
}
system.time(
  model_out_2 <- do.call(rbind, lapply(1:10000, boot_glm, data_to_use = iris))
)


## Now, we'll turn to some tools to speed up the implementation of our code
# we can compile our function, to make it even faster
library(compiler)
enableJIT(level = 3)
boot_glm_c <- cmpfun(boot_glm)
system.time(
model_out_3 <- do.call(rbind, lapply(1:10000, boot_glm_c, data_to_use = iris))
)

# we can also use the data.table package, which is an incredibly fast 
# version of data frame (I use it in a lot of my work)

library(data.table)
iris_dt <- as.data.table(iris)
boot_glm_dt <- function(j, data_to_use = iris_dt){
  set.seed(i)
  dt <- iris[ sample.int(n = nrow(iris), size = nrow(iris), replace = TRUE), ]
  m_boot <- glm(binarize ~ 
    Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = dt)
  data.table(m_boot$coefficients)
}
system.time(
model_out_4 <-rbindlist(lapply(1:10000, boot_glm_dt, data_to_use = iris))
)