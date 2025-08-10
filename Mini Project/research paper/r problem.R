#install.packages("deSolve")
library (deSolve)

#####BUILDING A NVPDZ model###########

# The model (i. e. the right hand side of the ODE)
model = function(t, state, parameters) {
  with(as.list(c(state, parameters)),{
    dN <- e*D - mu*P*(N/(N+kN))
    dP <- mu*P*(N/(N+kN)) - I*P*V - gz*P*Z - mp*P
    dV <- deltaV*I*P*V - mv*V
    dZ <- gz*P*Z - Z*mz
    dD <- mp*P+ Z*mz + V*mv + (1-deltaV)*I*P*V - e*D
    list(c(dN, dP, dV, dZ, dD))         #returns a list of equations
  })
}

# Parameters and their values
parameter<- c( e=0.8 ,      #Remineralization rate
               mu=0.9 ,     #Phytoplankton maximum growth rate
               kN=0.2 ,     #Half-saturation constant
               mp=0.05 ,    #Phytoplankton mortality
               I=0. ,     #infection rate
               gz=0.0 ,    #grazing rate 
               deltaV=0.0 ,#virus production
               mv=0.001 ,   #Virus mortality
               mz=0.01      #Zooplankton mortality
               )

initial_condition = c(N=97.499, P=2, V=0.00, Z=0., D=0) #####initial conditions of state variables


# Numerical method
numerical_method = "lsoda"

time = seq(0, 350, by=1)

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

