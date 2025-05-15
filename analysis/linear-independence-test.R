### quick test of linear independence of vector, relative
### to other vectors
### 

#### resources
# https://www.geeksforgeeks.org/linear-independence/#steps-to-determine-linear-independence
# https://www.mathbootcamps.com/determine-vector-linear-combination-vectors/

#### step 1: make a matrix of the regressors
mat <- matrix(c(1, 1, -1, 0, 1, 1, 1, 0, 1), ncol = 3,
              byrow = FALSE)

det(mat) # +ve, suggests linear independence

###### triple checking by row reducing and then
## taking the determinant

#### step 2: row reduce the matrix
## row 1 already has a 1 at the start so leave
## switch row 2 & 3
mat[c(2,3),] <- mat[c(3,2),]
# add row 1 to row 2
mat[2,] <- mat[2,] + mat[1,]
# subtract row 1 from row 3
mat[3,] <- mat[3,] - mat[1,] 
# subtract row 2 from row 3
mat[3,] <- mat[3,] - mat[2,]
# multiply row 3 by 1/3
mat[3,] <- mat[3,] * -1/3

#### step 3: take the determinant. Is it positive?
det(mat)

## test using my RDMs ----------------------------------------------------------
## https://stackoverflow.com/questions/532029/how-to-check-if-m-n-sized-vectors-are-linearly-independent
## https://www.wikihow.com/Reduce-a-Matrix-to-Row-Echelon-Form

stim_vec <- c(1, 0, 1, 0, 1, 0, 1, 0,
              0, 1, 0, 1, 0, 1, 0, 1,
              1, 0, 1, 0, 1, 0, 1, 0,
              0, 1, 0, 1, 0, 1, 0, 1,
              1, 0, 1, 0, 1, 0, 1, 0,
              0, 1, 0, 1, 0, 1, 0, 1,
              1, 0, 1, 0, 1, 0, 1, 0,
              0, 1, 0, 1, 0, 1, 0, 1)
# matrix(stim_vec, 8)
resp_vec <- c(1, 0, 0, 1, 1, 0, 0, 1, 
              0, 1, 1, 0, 0, 1, 1, 0,
              0, 1, 1, 0, 0, 1, 1, 0,
              1, 0, 0, 1, 1, 0, 0, 1,
              1, 0, 0, 1, 1, 0, 0, 1, 
              0, 1, 1, 0, 0, 1, 1, 0,
              0, 1, 1, 0, 0, 1, 1, 0,
              1, 0, 0, 1, 1, 0, 0, 1)
# matrix(resp_vec, 8, 8)
rule_vec <- c(1, 1, 0, 0, 1, 1, 0, 0,
              1, 1, 0, 0, 1, 1, 0, 0,
              0, 0, 1, 1, 0, 0, 1, 1,
              0, 0, 1, 1, 0, 0, 1, 1,
              1, 1, 0, 0, 1, 1, 0, 0,
              1, 1, 0, 0, 1, 1, 0, 0,
              0, 0, 1, 1, 0, 0, 1, 1,
              0, 0, 1, 1, 0, 0, 1, 1)
# matrix(rule_vec, 8, 8)
val_vec <- c(1, 1, 1, 1, 0, 0, 0, 0,
             1, 1, 1, 1, 0, 0, 0, 0,
             1, 1, 1, 1, 0, 0, 0, 0,
             1, 1, 1, 1, 0, 0, 0, 0,
             0, 0, 0, 0, 1, 1, 1, 1,
             0, 0, 0, 0, 1, 1, 1, 1,
             0, 0, 0, 0, 1, 1, 1, 1,
             0, 0, 0, 0, 1, 1, 1, 1)
# matrix(val_vec, 8)
conj_vec <- as.vector(diag(1, 8, 8))
# diag(1, 8, 8)

(mat <- matrix(c(stim_vec, resp_vec, rule_vec, val_vec, conj_vec), nrow=5, byrow=T))

## convert to row echelon form
## switch row 2 & 3
mat[c(2,3),] <- mat[c(3,2),]
mat[, 1:5]
# subtract row 1 from row 2
mat[2,] <- mat[2,] - mat[1,]
mat[, 1:5]
# subtract row 1 from row 3
mat[3,] <- mat[3,] - mat[1,]
mat[, 1:5]
# subtract row 1 from 4
mat[4,] <- mat[4,] - mat[1,]
mat[, 1:5]
# subtract row 1 from 5
mat[5,] <- mat[5,] - mat[1,]
mat[, 1:5]
# subtract row 2  from 4
mat[4,] <- mat[4,] - mat[2,]
mat[, 1:5]
# multiply row 3 by -1
mat[3,] <- mat[3,] * -1
mat[, 1:5]
# add row 5 to row 4
mat[4,] <- mat[4,] + mat[5,]
mat[, 1:5]
# add row 3 to row 5
mat[5,] <- mat[5,] + mat[3,]
mat[, 1:5]
# add row 4 to row 5
mat[5,] <- mat[5,] + mat[4,]
mat[, 1:5]
# multiply row 5 by -1/3
mat[5,] <- mat[5,] * -1/3
mat[, 1:5]
mat # no columns or rows that are all-zero, so therefore independent? 



# ## repeat with only lower portion of matrices
# stim_vec <- c(1, 0, 1, 0, 1, 0, 1, 0,
#               1, 0, 1, 0, 1, 0, 1,
#               1, 0, 1, 0, 1, 0,
#               1, 0, 1, 0, 1,
#               1, 0, 1, 0,
#               1, 0, 1,
#               1, 0,
#               1)
# # matrix(stim_vec, 8)
# resp_vec <- c(1, 0, 0, 1, 1, 0, 0, 1, 
#               1, 1, 0, 0, 1, 1, 0,
#               1, 0, 0, 1, 1, 0,
#               1, 1, 0, 0, 1,
#               1, 0, 0, 1, 
#               1, 1, 0,
#               1, 0,
#               1)
# # matrix(resp_vec, 8, 8)
# rule_vec <- c(1, 1, 0, 0, 1, 1, 0, 0,
#               1, 0, 0, 1, 1, 0, 0,
#               1, 1, 0, 0, 1, 1,
#               1, 0, 0, 1, 1,
#               1, 1, 0, 0,
#               1, 0, 0,
#               1, 1,
#               1)
# # matrix(rule_vec, 8, 8)
# val_vec <- c(1, 1, 1, 1, 0, 0, 0, 0,
#              1, 1, 1, 0, 0, 0, 0,
#              1, 1, 0, 0, 0, 0,
#              1, 0, 0, 0, 0,
#              1, 1, 1, 1,
#              1, 1, 1,
#              1, 1,
#              1)
# 
# (mat <- matrix(c(stim_vec, resp_vec, rule_vec, val_vec), ncol=length(stim_vec)))

