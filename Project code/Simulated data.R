set.seed(123)

n = 200
p = 60
alpha = 2

X_star <- matrix(rnorm(n*p), nrow = n, ncol=p)
Z <- matrix(rnorm(n), nrow = n, ncol = 1) #noise variable
X <- X_star + matrix(rep(Z, p), nrow = n)

#sparse  β（true coefficients）
beta <- rep(0, p)
beta[c(16, 18, 20)] <- 1  # weak signal
beta[c(31, 33, 35)] <- 2  # 
beta[c(46, 48, 50)] <- 3  # strong signal
print(beta)

tau = 1/4
sigma = 1/sqrt(tau)
eps=rnorm(n, mean = 0, sd = sigma)
y = alpha + as.vector(X%*%beta + eps)

#data frame
colnames(X) <- paste0("X", 1:p)
data <- as.data.frame(X)
data$Y <- as.numeric(y)
head(data)

beta_df <- data.frame(Variable = paste0("X", 1:p), Beta = beta)
head(beta_df)



