#install.packages("deSolve")
library (deSolve)

#####BUILDING A NVPDZ model###########

# The model (i. e. the right hand side of the ODE)
model = function(t, state, parameters) {
  with(as.list(c(state, parameters)),{
    dN <-   
    dP <- 
    dV <- 
    dZ <- 
    dD <- 
    list(c(dN, dP, dV, dZ, dD))         #returns a list of equations
  })
}



# Parameters and their values
parameter<- c( theta=
                 
                 
) 

initial_condition = c(N=, P=, V=, Z=, D=) #####initial conditions of state variables


# Numerical method
numerical_method = "lsoda" # = solver for ordinary differential equations

time = seq(0, 1000, by=1)

require(deSolve)

solution = ode(initial_condition, time, model, parameter, numerical_method)

plot(solution)



# check for mass balance
solutiondf <-as.data.frame(solution)
N<-solutiondf$N
P<-solutiondf$P
V<-solutiondf$V
Z<-solutiondf$Z
D<-solutiondf$D
massbalance = N+P+Z+V+D
plot(time, massbalance)





