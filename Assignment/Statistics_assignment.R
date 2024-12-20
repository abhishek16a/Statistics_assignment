install.packages("ggplot2")
install.packages("matlib")
install.packages("rsample")
install.packages("plotly")
install.packages("Hmisc")
install.packages("tidyr")
install.packages("dplyr")
install.packages("data.table")
install.packages("ggExtra")
install.packages("MASS")

library(ggExtra)
library(matlib)
library(ggplot2)
library(rsample)
library(plotly)
library(Hmisc)
library(tidyr)
library(dplyr)
library(data.table)


Brainwave_file = read_excel("C:/brainwave.xlsx")

df = data.frame(cbind(brainwave))
df[1:5,]

describe(df["Time"])
describe(df["x2"])
describe(df["x1"])


#-----------------------------Task 1 initiation------------------------------
#1.1.1 (Time series comparison of Input Signals with Time)
plot_ly(df) %>%
  add_trace( x=~Time, y=~x1,type = "scatter",mode = "lines", name = "x1") %>% 
  add_trace( x=~Time, y=~x3,type = "scatter",mode = "lines", name = "x3") %>% 
  add_trace( x=~Time, y=~x4,type = "scatter",mode = "lines", name = "x4") %>% 
  add_trace( x=~Time, y=~x5,type = "scatter",mode = "lines", name = "x5") %>%
  layout(plot_bgcolor='#e5ecf6', 
         title="Time series comparison of Input signals with time", 
         xaxis = list(title="Time"), yaxis = list(title="Input Signals"))

#1.1.2 (Time series analysis of input signals(x1,x3,x4,x5) with time)
fig1 = plot_ly(df, x=~Time, y=~x1,type = "scatter",mode = "lines", name = "x1")
fig2 = plot_ly(df, x=~Time, y=~x3,type = "scatter",mode = "lines", name = "x3")
fig3 = plot_ly(df, x=~Time, y=~x4,type = "scatter",mode = "lines", name = "x4")
fig4 = plot_ly(df, x=~Time, y=~x5,type = "scatter",mode = "lines", name = "x5")
x_fig = plotly :: subplot(fig1, fig2, fig3, fig4, nrows = 4, shareX = TRUE,titleX = TRUE, 
        titleY = TRUE) %>% layout(plot_bgcolor='#e5ecf6',
        title="Time series analysis of input signal with time", 
        xaxis = list(title="Time"))
x_fig
#1.1.3 Time series analysis of Output Signal(x2) with Time)
fig1 = plot_ly(df, x=~Time, y=~x2,type = "scatter",mode = "lines", name = "x2")
y_fig = plotly::subplot(fig1, nrows = 1,titleX =TRUE, titleY = TRUE)%>%
                        layout(plot_bgcolor='#e5ecf6',
                        title="Time series analysis of Output signal(x2) with time", 
                        xaxis = list(title="Time"), 
                        yaxis = list(title="Output Signal (x2)"))
y_fig

# Distribution of input x1
gg_X1 = ggplot(df, aes(x = x1)) +   
geom_histogram(aes(y = after_stat(density)), bins=10,fill = "#195f90")+
               stat_density(geom="line") +
               geom_rug() 
               ggplotly(gg_X1) %>% layout(plot_bgcolor='#e5ecf6', 
               title="Distribution of X1", xaxis= list(title="x1 Signal"), 
               yaxis = list(title="Density"))

# Distribution of input x3               
gg_X3 = ggplot(df, aes(x = x3)) +   
geom_histogram(aes(y = after_stat(density)), bins=10,fill = "#195f90")+
               stat_density(geom="line") +
               geom_rug() 
               ggplotly(gg_X3) %>% layout(plot_bgcolor='#e5ecf6', 
               title="Distribution of X3", xaxis= list(title="x3 Signal"), 
               yaxis = list(title="Density"))

# Distribution of input x4                             
gg_X4 = ggplot(df, aes(x = x4)) +    
geom_histogram(aes(y = after_stat(density)), bins=10,fill = "#195f90")+
                 stat_density(geom="line") +
                 geom_rug() 
                 ggplotly(gg_X4) %>% layout(plot_bgcolor='#e5ecf6', 
                 title="Distribution of X4", xaxis= list(title="x4 Signal"), 
                yaxis = list(title="Density"))
                 
 # Distribution of input x5
gg_X5 = ggplot(df, aes(x = x5)) +    
geom_histogram(aes(y = after_stat(density)), bins=10,fill = "#195f90")+
                 stat_density(geom="line") +
                 geom_rug() 
                 ggplotly(gg_X5) %>% layout(plot_bgcolor='#e5ecf6', 
                 title="Distribution of X5", xaxis= list(title="x5 Signal"), 
                yaxis = list(title="Density"))
                 
 # Distribution of output x2
gg_X2 = ggplot(df, aes(x = x2)) +    
geom_histogram(aes(y = after_stat(density)), bins=10,fill = "#195f90")+
                 stat_density(geom="line") +
                 geom_rug() 
                 ggplotly(gg_X2) %>% layout(plot_bgcolor='#e5ecf6', 
                 title="Distribution of output (x2)", 
                 xaxis= list(title="Output(x2) Signal"), 
                 yaxis = list(title="Density"))
                 


#Disribution of all inputsignals

X_df = data.frame(rbind(data.frame(values = df$x1, Inputs = "x1"),
                       data.frame(values = df$x3, Inputs = "x3"),
                       data.frame(values = df$x4, Inputs = "x4"),
                      data.frame(values = df$x5, Inputs = "x5")))


p <- ggplot(X_df, aes(x = values)) +
 geom_histogram(aes(x=values, y = after_stat(density), fill=Inputs),
 bins = 10, alpha=0.5) + stat_density(aes(x=values, 
  y = after_stat(density), color=Inputs),geom="line") + geom_rug()
  fig <- ggplotly(p) %>% layout(plot_bgcolor='#e5ecf6',
      title="Distribution of input Signal", 
      xaxis=list(title="Input Signal(x1,x3,x4,x5)"), 
      yaxis = list(title="Density")) 
  fig

  #Correlation of x1 input to x2 output
  fig = plot_ly(df, x=~x1, y=~x2, type="scatter", mode="markers") %>%
  layout(plot_bgcolor='#e5ecf6', title="Correlation of x1 input to x2 output", 
         xaxis =list(title="X1 input"), yaxis = list(title="Output Signal (X2)"))
  fig
  
  #correlation of x3 input to x2 output
  fig = plot_ly(df, x=~x3, y=~x2, type="scatter", mode="markers") %>%
  layout(plot_bgcolor='#e5ecf6', title="Correlation of x3 input to x2 output", 
  xaxis =list(title="X3 input"), yaxis = list(title="Output Signal (X2)"))
  fig
  
  #correlation of x4 input to x2 output
  fig = plot_ly(df, x=~x4, y=~x2, type="scatter", mode="markers") %>%
  layout(plot_bgcolor='#e5ecf6', title="Correlation of x4 input to x2 output", 
  xaxis =list(title="x4 input"), yaxis = list(title="Output Signal (x2)"))
  fig
  
  #correlation of x5 input to x2 output
  fig = plot_ly(df, x=~x5, y=~x2, type="scatter", mode="markers") %>%
  layout(plot_bgcolor='#e5ecf6', title="Correlation of x5 input to x2 output", 
  xaxis =list(title="x5 input"), yaxis = list(title="Output Signal (x2)"))
  fig
  
  #correlation of all inputs(x1,x3,x4,x5) to X2 output
  plot_ly(df) %>%
    add_trace(x=~x1, y=~x2, type = "scatter", mode = "markers", name = "x1") %>% 
    add_trace(x=~x3, y=~x2, type = "scatter", mode = "markers", name = "x3") %>%
    add_trace(x=~x4, y=~x2, type = "scatter", mode = "markers", name = "x4") %>% 
    add_trace(x=~x5, y=~x2, type = "scatter", mode = "markers", name = "x5") %>%
    layout(plot_bgcolor='#e5ecf6',title="Correlation of all inputs with output(x2)", 
    xaxis = list(title="inputs(x1,x3,x4,x5)"), yaxis = list(title="output(x2)"))
#--------------------------------Task 1 ending------------------------------------
  
#-----------------------------Task 2 initiation-----------------------------------
# Task2.0-------model representation of all models--------------------------------
  generateModel1 <- function(df){
    set.seed(100)
    ones = matrix(1, length(df$x1),1)
    theta_bias = runif(1, -1, 1) * ones
    return (cbind(df$x4, df$x3^2, theta_bias))
  }
  
  generateModel2 <- function(df){
    set.seed(100)
    ones = matrix(1, length(df$x1),1)
    theta_bias = runif(1, -1, 1) * ones
    return (cbind(df$x4, df$x3^2, df$x5, theta_bias))
  }
  
  generateModel3 <- function(df){
    set.seed(100)
    ones = matrix(1, length(df$x1),1)
    return (cbind(df$x3, df$x4, df$x5^3))
  }
  
  generateModel4 <- function(df){
    set.seed(100)
    ones = matrix(1, length(df$x1),1)
    theta_bias = runif(1, -1, 1) * ones
    return (cbind(df$x4, df$x3^2, df$x5^3, theta_bias))
  }
  
  generateModel5 <- function(df){
    set.seed(100)
    ones = matrix(1, length(df$x1),1)
    theta_bias = runif(1, -1, 1) * ones
    return (cbind(df$x4, df$x1^2, df$x3^2, theta_bias))
  }
  
#task 2.1------Estimating model parameters using least square method
  
  thetaHat <- function(model, x2){
  return (solve(t(model) %*% model) %*% t(model) %*% df$x2)
    }

  Model1 = generateModel1(df)
  Model1_theta_hat = thetaHat(Model1,df$x2)
  cat(("Model 1 Theta hat"),"\n",(Model1_theta_hat[,1]))
  x2_Hat_Model1 = Model1 %*% Model1_theta_hat
  cat(("Model 1 x2 hat"),"\n",(x2_Hat_Model1[1:5,]))
  
  Model2 = generateModel2(df)
  Model2_theta_hat = thetaHat(Model2,df$x2)
  cat(("Model 2 Theta hat"),"\n",(Model2_theta_hat[,1]))
  x2_Hat_Model2 = Model2 %*% Model2_theta_hat
  cat(("Model 2 x2 hat"),"\n",(x2_Hat_Model2[1:5,]))
  
  Model3 = generateModel3(df)
  Model3_theta_hat = thetaHat(Model3,df$x2)
  cat(("Model 3 Theta hat"),"\n",(Model3_theta_hat[,1]))
  x2_Hat_Model3 = Model3 %*% Model3_theta_hat
  cat(("Model 3 x2 hat"),"\n",(x2_Hat_Model3[1:5,]))
  
  Model4 = generateModel4(df)
  Model4_theta_hat = thetaHat(Model4,df$x2)
  cat(("Model 4 Theta hat"),"\n",(Model4_theta_hat[,1]))
  x2_Hat_Model4 = Model4 %*% Model4_theta_hat
  cat(("Model 4 x2 hat"),"\n",(x2_Hat_Model4[1:5,]))
  
  Model5 = generateModel5(df)
  Model5_theta_hat = thetaHat(Model5,df$x2)
  cat(("Model 5 Theta hat"),"\n",(Model5_theta_hat[,1]))
  x2_Hat_Model5 = Model5 %*% Model5_theta_hat
  cat(("Model 5 x2 hat"),"\n",(x2_Hat_Model5[1:5,]))
  
#Task 2.2 Residual Sum of squares (RSS)
  calculateRSS<- function(x2,x2_hat_model){
    return(sum(x2-x2_hat_model)^2)
  }
  
  RSS_Model1 = calculateRSS(df$x2, x2_Hat_Model1)
  RSS_Model2 = calculateRSS(df$x2, x2_Hat_Model2)
  RSS_Model3 = calculateRSS(df$x2, x2_Hat_Model3)
  RSS_Model4 = calculateRSS(df$x2, x2_Hat_Model4)
  RSS_Model5 = calculateRSS(df$x2, x2_Hat_Model5)
  cat(print("RSS"),"\n",c("(Model 1)",RSS_Model1,"\n", "(Model 2)",RSS_Model2,
                          "\n","(Model 3)", RSS_Model3,"\n",
                          "(Model 4)",RSS_Model4,"\n", "(Model 5)",RSS_Model5))
  
#Task 2.3 Likelihood function
  calculateVariance <- function(N, rss_model){
    return (rss_model/(N-1))
  }
  calculateLikelihood <- function(N, variance_model, rss_model) {
    return (-(N/2)*(log(2*pi))-(N/2)*(log(variance_model))-(1/(2*variance_model))*rss_model)
  }
  N = length(df$x2)
  Variance_Model1 = calculateVariance(N, RSS_Model1) 
  Likelihood_1 = calculateLikelihood(N, Variance_Model1, RSS_Model1)
  
  N = length(df$x2)
  Variance_Model1 = calculateVariance(N, RSS_Model1)
  Variance_Model2 = calculateVariance(N, RSS_Model2)
  Variance_Model3 = calculateVariance(N, RSS_Model3)
  Variance_Model4 = calculateVariance(N, RSS_Model4)
  Variance_Model5 = calculateVariance(N, RSS_Model5) 
 cat(print("Variance"),"\n", c("(Model 1)", Variance_Model1,"\n","(Model 2)",Variance_Model2,
                               "\n", "(Model 3)",Variance_Model3,
                               "\n","(Model 4)", Variance_Model4,
                               "\n","(Model 5)",Variance_Model5))
  
  s
  Likelihood_1 = calculateLikelihood(N, Variance_Model1, RSS_Model1)
  Likelihood_2 = calculateLikelihood(N, Variance_Model2, RSS_Model2)
  Likelihood_3 = calculateLikelihood(N, Variance_Model3, RSS_Model3)
  Likelihood_4 = calculateLikelihood(N, Variance_Model4, RSS_Model4)
  Likelihood_5 = calculateLikelihood(N, Variance_Model5, RSS_Model5)
  
 cat(print("likelihood"),"\n", c("(Model 1)",Likelihood_1,"\n","(Model 2)", Likelihood_2,
                                 "\n","(Model 3)",Likelihood_3,
                                 "\n","(Model 4)", Likelihood_4,
                                 "\n","(Model 5)", Likelihood_5))
 
 #Task 2.4 (AIC &BIC)
 calculateAIC <- function(N, model_thetahat, likelihood_model){
   k_model = length(model_thetahat)
   return (2*k_model - 2*likelihood_model)
 }
 
 calculateBIC <- function(N, model_thetahat, likelihood_model){
   k_model = length(model_thetahat)
   return (k_model*log(N) - 2*likelihood_model)
 }
 
 AIC_Model1 = calculateAIC(N, Model1_theta_hat, Likelihood_1)
 BIC_Model1 = calculateBIC(N, Model1_theta_hat, Likelihood_1)
 c(AIC_Model1, BIC_Model1)
 
 AIC_Model2 = calculateAIC(N, Model2_theta_hat, Likelihood_2)
 BIC_Model2 = calculateBIC(N, Model2_theta_hat, Likelihood_2)
 c(AIC_Model2, BIC_Model2)
 
 AIC_Model3 = calculateAIC(N, Model3_theta_hat, Likelihood_3)
 BIC_Model3 = calculateBIC(N, Model3_theta_hat, Likelihood_3)
 c(AIC_Model3, BIC_Model3)
 
 AIC_Model4 = calculateAIC(N, Model4_theta_hat, Likelihood_4)
 BIC_Model4 = calculateBIC(N, Model4_theta_hat, Likelihood_4)
 c(AIC_Model4, BIC_Model4)
 
 AIC_Model5 = calculateAIC(N, Model5_theta_hat, Likelihood_5)
 BIC_Model5 = calculateBIC(N, Model5_theta_hat, Likelihood_5)
 c(AIC_Model5, BIC_Model5)
 
 list_models = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5")
 list_aic = c(AIC_Model1, AIC_Model2, AIC_Model3, AIC_Model4, AIC_Model5)
 list_bic = c(BIC_Model1, BIC_Model2, BIC_Model3, BIC_Model4, BIC_Model5)
 list_rss = c(RSS_Model1, RSS_Model2, RSS_Model3, RSS_Model4, RSS_Model5)
 list_likelihood = c(Likelihood_1, Likelihood_2, Likelihood_3, Likelihood_4, Likelihood_5)
 list_variance = c(Variance_Model1, Variance_Model2, Variance_Model3, Variance_Model4, 
                   Variance_Model5)
 
 
 data.frame(Models = list_models, AIC = list_aic)
 
 data.frame(Models = list_models, BIC = list_bic)
 
 data.frame(Models = list_models, RSS = list_rss)
 
 data.frame(Models = list_models, Variance = list_variance, Likelihood = list_likelihood)
 
#Task 2.5 Distribution of prediction errors 
 calculateError <- function(x2, x2_hat){
   return (x2 - x2_hat)
 }
 
 plotQQ <- function(model_error, title){
   error_fig = ggplot(data.frame(model_error), aes(sample=model_error)) +
     geom_qq(color = "#195") +
     geom_qq_line(color = "red")
   
   return (ggplotly(error_fig) %>% layout(plot_bgcolor='#e5ecf6', title=title, 
                                          xaxis=list(title="Theoritical Quantities"), 
                                          yaxis = list(title="Sample Quantities")))
 }
 
 Model1_Error = calculateError(df$x2, x2_Hat_Model1)
 plotQQ(Model1_Error, "QQ plot of Model 1")
 
 Model2_Error = calculateError(df$x2, x2_Hat_Model2)
 plotQQ(Model2_Error, "QQ plot of Model 2")
 
 Model3_Error = calculateError(df$x2, x2_Hat_Model3)
 plotQQ(Model3_Error, "QQ plot of Model 3")
 
 Model4_Error = calculateError(df$x2, x2_Hat_Model4)
 plotQQ(Model4_Error, "QQ plot of Model 4")
 
 Model5_Error = calculateError(df$x2, x2_Hat_Model5)
 plotQQ(Model5_Error, "QQ plot of Model 5")
 
 #2.6 Model selection
 data.frame(Models = list_models, RSS = list_rss, AIC = list_aic, BIC = list_bic,
            Likelihood = list_likelihood)
 
 #2.7 Evaluation of model
 # Set seed for reproducibility
 set.seed(100)
 
 # Split the data into training and testing sets (70% training, 30% testing)
 Split_Data = initial_split(df, prop = .7)
 training_set = training(Split_Data)
 testing_set = testing(Split_Data)
 
 # Prepare the training and testing data for Model 2
 X_training_model2 = generateModel2(training_set)
 X_testing_model2 = generateModel2(testing_set)
 
 # Calculate thetaHat for Model 2 using the training data
 training_thetaHat2 = thetaHat(X_training_model2, training_set$x2)
 
 # Predict the values of y for the training and testing sets
 Y_testing_hat = X_testing_model2 %*% training_thetaHat2
 Y_training_hat = X_training_model2 %*% training_thetaHat2
 
 #Calculation of Confidence Interval
 result=t.test(Y_training_hat,Y_testing_hat,mu=500,alternative = "two.sided",
               conf.level = 0.95)
 result
 
 # calculation of Standard error
 C_I1 = result$conf.int[1]
 C_I2 = result$conf.int[2]
 S_ERROR = result$stderr
 S_ERROR
 
 
 
 # Create a density plot for the response variable in the training set
 training_plot <- ggplot(training_set, aes(x = x2)) +
   stat_density(geom = "line", color = "#195f90") +
   geom_vline(xintercept = C_I1, linetype = "dashed", color = "red") +
   geom_vline(xintercept = C_I2, linetype = "dashed", color = "red") +
   geom_vline(xintercept = S_ERROR, linetype = "dashed", color = "black") +
   labs(title = "Distribution of Training Data with Confidence Intervals", x = "Y_training_hat",
        y = "Density") + theme(plot.background = element_rect(fill = "#e5ecf6"))
 
 ggplotly(training_plot)
 
 # Create a density plot for the response variable in the testing set
 testing_plot <- ggplot(testing_set, aes(x = x2)) +
   stat_density(geom = "line", color = "#195f90") +
   geom_vline(xintercept = C_I1, linetype = "dashed", color = "red") +
   geom_vline(xintercept = C_I2, linetype = "dashed", color = "red") +
   geom_vline(xintercept = S_ERROR, linetype = "dashed", color = "black") +
   labs(title = "Distribution of Testing Data with Confidence Intervals", x = "Y_testing_hat",
        y = "Density") + theme(plot.background = element_rect(fill = "#e5ecf6"))
 
 ggplotly(testing_plot)
 
 # calculate density plot of x2 signal
 dis_test=density(training_set$x2)
 plot(dis_test,main = "Density plot of x2 Signal")
 
 z = 1.96  # (95%) Confidential interval
 error = (as.matrix(testing_set$x2) - Y_testing_hat)
 
 #Calculate the standard error of the errors
 n_len = length(Y_testing_hat)
 sd_error = sqrt(sum(error^2) / (n_len - 1))
 
 # Calculate the standard error of the mean
 se_mean = sd_error / sqrt(n_len) 
 
 # Calculate the lower and upper bounds of the 95% confidence interval
 
 C_I_lower = mean(error_model2) - z * se_mean   
 C_I_upper = mean(error_model2) + z * se_mean  
 
 # Print the confidence intervals and standard error
 cat("Confidence Interval Lower Bound:", C_I_lower, "\n")
 cat("Confidence Interval Upper Bound:", C_I_upper, "\n")
 cat("Standard Error:", sd_error, "\n")
 
 ----------------------------------------------------------
#Task 3 Approximate Bayesian Computation(ABC) 
 
 arr_1=0
 arr_2=0
 f_value=0
 s_value=0
 
 thetaone <- Model2_theta_hat[1,] #chosen parameter
 thetathree <- Model2_theta_hat[3,] # chosen parameter
 thetabias <- Model2_theta_hat[4,]
 thetatwo <- Model2_theta_hat[2,]

 epsilon <- RSS_Model2 * 2
 num <- 100
 
 for (i in 1:num) {
   
   p1 <- runif(1,-abs(thetaone), abs(thetaone))
   p2 <- runif(1,-abs(thetathree), abs(thetathree))
   abc_thetahat <- matrix(c(p1,p2,thetabias, thetatwo))
   abc_Y_Hat <- Model2 %*% abc_thetahat 
   abc_RSS <- sum((df$x2-abc_Y_Hat)^2)
   
   if (abc_RSS > epsilon){
     
     arr_1[i] <- p1
     arr_2[i] <- p2
     
     counter = counter+1
     
     f_value <- matrix(arr_1)
     s_value <- matrix(arr_2)
     
   }
   
 }
 abc_results = data.frame(f_value,s_value)
 abc_results
 abc_results$variable <- rep(c("f_value", "s_value"), each = nrow(abc_results) / 2)
 plot_ly(abc_results, x=~f_value, y=~s_value, type="scatter", 
         mode="markers",color = ~factor(variable),colors = c("red", "blue")) %>%
  layout(plot_bgcolor='#e5ecf6', 
       title="Joint and Marginal Posterior Distribution",
       xaxis =list(title="ABC  θ₁ x₄"), yaxis = list(title="ABC θ₃ x₅"))
 

 
 
 
 
 
 
 
 
 
 
 
 
 
 