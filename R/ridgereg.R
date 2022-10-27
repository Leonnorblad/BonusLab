#' Ridge regression
#' 
#' Performs ridge regression
#' 
#' @param Data A data.frame
#' @param formula A formula
#' @param lambda Regularization parameter
#' @param new_data New data to be predicted in the \code{predict()} function
#' 
#' @return
#' \code{print()} Returns the function call and estimated coefficients
#'
#' \code{predict()} Returns predictions for new data
#' 
#' \code{coef()} Returns a vector of estimated coefficients
#' 
#' 
#' @examples
#' data(iris)
#' example <- ridgereg(data=iris, formula=Petal.Length~Petal.Width+Sepal.Width+Species, lambda=0)
#' example$print()
#' example$predict(new_data=data.frame(Petal.Width=c(1,2,3),
#'                                    Sepal.Width=c(2,3,4),
#'                                    Species=c("versicolor", "virginica", "versicolor")))
#' @export ridgereg
#' 

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
                        },
                        show = function(){
                          
                          cat("Call:\nridgereg(formula = ", format(formula), ", data = ", df_name,", lambda = ",lambda,")\n\n", sep="")
                          temp <- as.data.frame(t(data.frame(result$beta_hat)))
                          rownames(temp) <- ""
                          colnames(temp) <- colnames(X)
                          cat("Coefficients:\n")
                          print.data.frame(temp)
                        },
                        predict = function(new_data){
                          if(!all(attr(terms(formula), which = "term.labels") %in% colnames(new_data))){
                            stop("Invalid new_data input")}
                          # Data.frame of reoderd new_data
                          X_model_matrix <- data.frame()
                          
                          # First column=Intercept
                          X_model_matrix[1:nrow(new_data),1] <- rep(1, nrow(new_data))
                          colName <- c("(Intercept)")
                          
                          # Set counter for colums in new_data
                          k<-1
                          # Operates over all colums of new_data
                          for(i in 1:ncol(new_data)){
                            # If the coloum is a character
                            if(is.character(new_data[,i])){
                              # Operates over all levels of new_data
                              for(j in 1:length(unique(new_data[,i]))){
                                # Moves to the next column (first one 2 due to the intercept)
                                k <- k+1
                                # The colum name is the variable name + variable level (like in model.matrix)
                                colName[k]<- paste(colnames(new_data)[i], unique(new_data[,i]),sep="")[j]
                                # TRUE if the colum value belongs to the level of the variable, FALSE otherwise
                                X_model_matrix[,k] <- new_data[,i]==unique(new_data[j,i])
                                # Setts TRUE to 1 and FALSE to 0
                                X_model_matrix[,k] <- as.integer(as.logical(X_model_matrix[,k]))
                              }
                              # If not character
                            } else { 
                              # Moves to the next row
                              k <- k+1
                              # The values is the values from new_data
                              X_model_matrix[,k] <- new_data[,i]
                              # The colum name is the same as in new_data
                              colName[k] <- colnames(new_data)[i]
                            }
                          }
                          # Sets colum names
                          colnames(X_model_matrix)<-colName
                          # Calculate predicted value by new_data times the corresponding beta parameter
                          Y_new <<- as.matrix(X_model_matrix[match(rownames(result$beta_hat),colnames(X_model_matrix), nomatch=0)])%*%as.matrix(result$beta_hat)
                          return(data.frame(Predictions=Y_new))
                        },
                        coef = function(){
                          return(result$beta_hat)
                        }
                      )
)


