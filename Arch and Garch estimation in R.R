'''
Arch and Garch estimation in R
'''
library(quantmod)
library(fGarch)
Symb = '^GSPC'
# doenload the SP500 adjusted close price from yahoo finance
P = getSymbols(Symbols = Symb, src = "yahoo", from =c("2009-01-01") ,
              to = Sys.Date(), auto.assign = F,periodicity = "daily")[,6]
log_return = diff(log(P))*100
Zero_mean_return= log_return - mean(as.vector(log_return),na.rm = TRUE)


# Garch modeling ARCH(1)
garch01=garchFit(formula = ~garch(1,0), data =na.omit(Zero_mean_return), 
                               include.mean = FALSE)
names(garch01@fit)
garch01@fit$coef
garch01@fit$llh
plot(garch01)

garch40=garchFit(formula = ~garch(4,0), data =na.omit(Zero_mean_return), 
                 include.mean = FALSE)
garch40@fit$coef
garch40@fit$llh

garch41=garchFit(formula = ~garch(4,1), data =na.omit(Zero_mean_return), 
                 include.mean = FALSE)
garch41@fit$coef
garch41@fit$llh

garch11=garchFit(formula = ~garch(1,1), data =na.omit(Zero_mean_return), 
                 include.mean = FALSE)
garch11@fit$coef
garch11@fit$llh

garch11_t=garchFit(formula = ~garch(1,1), data =na.omit(Zero_mean_return), 
          include.mean = FALSE, cond.dist = "std",trace= F)
garch11_t@fit$coef
garch11_t@fit$llh

# GARCH(1,1):  (sigma_t)^2= omega + alpha_1 * (return_t-1)^2 + Beta_1 * (sigma_t-1)^2
garch11_s_t=garchFit(formula = ~garch(1,1), data =na.omit(Zero_mean_return), 
                   include.mean = FALSE, cond.dist = "sstd",trace= F)
garch11_s_t@fit$coef
garch11_s_t@fit$llh
plot(garch11_s_t)


# Advaned ARCH and GARCH estimation in R

#APARCH :  (sig_t+1)^delta = omega + SUM(i=1 to L_1)[alpha_i *(|return_t-i| - gama *(return_t-i))]
#                              + SUM(i=1 to L_2) [Beta_j * (sig_t-j)^delta ]
# normal APARCH(1,1)
aparch_11=garchFit(formula= ~aparch(1,1), data=na.omit(Zero_mean_return)
         ,include.mean = FALSE,trace = F)
# fix delta at 2
aparch_11=garchFit(formula= ~aparch(1,1), data=na.omit(Zero_mean_return)
                   ,include.mean = FALSE,include.delta = F,delta = 2,trace = F).predict()
aparch_11@fit$coef
garchFit()

# normal APARCH(1,1)
aparch_11=garchFit(formula= ~aparch(1,1), data=na.omit(Zero_mean_return)
         ,include.mean = FALSE,trace = F)
# fix delta = 2
aparch_delta2_11=garchFit(formula= ~aparch(1,1), data=na.omit(Zero_mean_return)
                   ,include.mean = FALSE,include.delta = F,delta = 2,trace = F)
aparch_delta2_11@fit$coef

aparch_student_t_11 = garchFit(formula = ~aparch(1,1),data = na.omit(Zero_mean_return)
                               ,include.mean = FALSE, cond.dist = 'std',trace = F)
aparch_student_t_11@fit$coef

# Normal Aparch(2,2)
aparch_22 = garchFit(formula = ~aparch(2,2), data = na.omit(Zero_mean_return), 
                     include.mean = FALSE ,trace=F)
aparch_22@fit$coef
