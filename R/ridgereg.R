ridgereg <- setRefClass("ridgereg",
                      fields = list(result ="list",
                                    data = "data.frame",
                                    df_name ="character",
                                    formula = "formula",
                                    beta_hat = "matrix",
                                    y_hat ="matrix",
                                    X = "matrix",
                                    y = "numeric",
                                    lambda="numeric",
                                    scale_data="data.frame",
                                    new_data="data.frame",
                                    X_new="matrix",
                                    Y_new="matrix"
                                    ),
                      methods = list(
                        initialize = function(formula, data, lambda=0){
                          lambda <<- lambda
                          formula <<- formula
                          df_name <<- as.character(substitute(data))
                          
                          scale_d <- data
                          for(i in 1:ncol(data)){
                            if(is.numeric(data[,i])){
                              scale_d[,i] <- scale(data[,i])
                            }
                          }
                          scale_data <<- scale_d
                          
                          X <<- model.matrix(formula, scale_data)               # Independen variable(s)
                          y <<- data[,all.vars(formula)[1]]                     # Dependent variable
                          
                          # Beta (Regression coefficients)
                          result$beta_hat <<- solve(t(X)%*%X+lambda*diag(ncol(X)))%*%t(X)%*%y
                          
                          # Fitted values 
                          result$y_hat <<- X%*%result$beta_hat
                        },
                        print = function(){
                          
                          cat("Call:\nridgereg(formula = ", format(formula), ", data = ", df_name,", lambda = ",lambda,")\n\n", sep="")
                          temp <- as.data.frame(t(data.frame(result$beta_hat)))
                          rownames(temp) <- ""
                          colnames(temp) <- colnames(X)
                          cat("Coefficients:\n")
                          print.data.frame(temp)
                        },
                        predict = function(new_data){
                          #X_new <<- model.matrix(formula, new_data)
                          new_X <- cbind(1, new_data)
                          Y_new <<- as.matrix(new_X)%*%result$beta_hat
                          return(Y_new)
                        },
                        coef = function(){
                          return(result$beta_hat)
                        }
                      )
)
example <- ridgereg(data=iris, formula=Petal.Length~Petal.Width+Sepal.Width, lambda=0)
example$print()
example$predict(new_data=data.frame(Petal.Width=1, Sepal.Width=200))
example$predict()
example$coef()

test <- cbind(1, data.frame(Petal.Width=4))
as.matrix(test)%*%example$coef()

new_data <-
new_data <- as.matrix(cbind(1, new_data))
test <- example$coef()
new_data %*% test




lambda<-1
lambda*diag(ncol(X))

data <- iris
formula <- Petal.Length~Species
lambda <- 3
result <- list()
# 
solve(t(X)%*%X+lambda*diag(ncol(X)))%*%t(X)%*%y

X
cbind(X[,1], scale(X[,-1]))

scale_data <- data
for(i in 1:ncol(data)){
  if(is.numeric(data[,i])){
    temp <- scale(data[,i])
  } else {
    temp <- data[,i]
  }
  scale_data[,i] <- temp
}
scale_data


lm_mod <- lm(data=iris, formula=Petal.Length~Species)
predict(lm_mod, data.frame(Species="versicolor"))



