# Code for Figure 6

library(corrplot)
library(knitr) 
library(kableExtra) 

## Define the Solvency correlation matrix
table_matrix = matrix(c(1, 0.25, 0.25, 0.25, 0.25,
                        0.25, 1, 0.25, 0.25, 0.5,
                        0.25, 0.25, 1, 0.25, 0,
                        0.25, 0.25, 0.25, 1, 0,
                        0.25, 0.5, 0, 0, 1), nrow = 5, byrow = TRUE)

## Create the row and column names
rownames(table_matrix) = c("Market", "Default", "Life", "Health", "Non-Life")
colnames(table_matrix) = c("Market", "Default", "Life", "Health", "Non-Life")

## Display the correlation matrix
corrplot(table_matrix, method = "number", type = "full", tl.col = "black", 
         col = "black", addCoef.col = "black", cl.pos = "n", order = "original")

# Code for Figure 7

## Define the stress-test correlation matrix
stress_matrix = table_matrix
## Modify some off-diagonal values
stress_matrix[4, 1] = 0.7  
stress_matrix[4, 2] = 0.7  
stress_matrix[4, 3] = 0.7  
stress_matrix[1, 4] = 0.7  
stress_matrix[2, 4] = 0.7  
stress_matrix[3, 4] = 0.7  
## Highlight modified values in red
color_matrix = ifelse(stress_matrix < 0.7, "red", "black")
## Display the correlation matrix with modified colors
corrplot(stress_matrix, method = "number", type = "full", tl.col = "black", 
         col = color_matrix, addCoef.col = "black", cl.pos = "n", order = "original")
## Calculate the eigenvalues
eigen_values = eigen(stress_matrix)$values
eigenvalues = t(eigen_values) #transpose of eigenvalues

# Code for Table 1

## Set eigenvalues column names
colnames(eigenvalues) = c("Eigenvalue 1", "Eigenvalue 2", "Eigenvalue 3", 
                          "Eigenvalue 4", "Eigenvalue 5")
## Print eigenvalues table 
kbl(eigenvalues, align="c", booktabs=TRUE, caption="Eigenvalues - stress test 1.") %>%
  kable_classic(full_width = FALSE) %>%
  kableExtra::kable_styling(latex_options ="striped") %>%
  kableExtra::kable_styling(latex_options = "hold_position")

# Check that a 3x3 matrix with reasonable entries is not a correlation matrix

A = matrix(c(1, 0.17, 0.62, 0.17, 1, 0.88, 0.62, 0.88, 1), nrow = 3, byrow = TRUE)
eigen(A)$values

# Figure 8

### FIRST STRESSED MATRIX
rho1=0.75 # new correlation value
## Modify some off-diagonal values
stress_matrix[1, 5] = rho1  
stress_matrix[2, 5] = rho1  
stress_matrix[5, 1] = rho1
stress_matrix[5, 2] = rho1 
## Highlight modified values in red
color_matrix = ifelse(stress_matrix == rho1, "red", "black")
## Display the correlation matrix with modified colors
par(mfrow = c(1, 2), pty="s", font.main=3)
corrplot(stress_matrix, method = "number", type = "full", tl.col = "black", 
         col = color_matrix, addCoef.col = "black", cl.pos = "n", order = "original")

## Calculate eigenvalues to check that at least one of them is negative
eigen(stress_matrix)$values

### SECOND STRESSED MATRIX
stress_matrix2 = stress_matrix # second correlation matrix
rho2=0.80 # new correlation value
## Modify some off-diagonal values
stress_matrix2[1, 2] = rho2
stress_matrix2[1, 4] = rho2
stress_matrix2[2, 1] = rho2
stress_matrix2[4, 1] = rho2  
## Highlight modified values in red
color_matrix = ifelse(stress_matrix2 < rho2, "red", "black")
## Display the correlation matrix with modified colors
corrplot(stress_matrix2, method = "number", type = "full", tl.col = "black", 
         col = color_matrix, addCoef.col = "black", cl.pos = "n", order = "original")
## Calculate eigenvalues to check that at least one of them is negative
eigen(stress_matrix2)$values