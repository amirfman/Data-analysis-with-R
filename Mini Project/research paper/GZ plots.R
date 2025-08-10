#install.packages("deSolve")
library (deSolve)
library(ggplot2)
library(gridExtra)

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
parameter<- c( e=0.3 ,      #Remineralization rate
                mu=0.8 ,     #Phytoplankton maximum growth rate
                kN=0.4 ,     #Half-saturation constant
                mp=0.01 ,    #Phytoplankton mortality
                I=0.5  ,     #infection rate
                gz=0.1 ,    #grazing rate 
                deltaV=0.01 ,#virus production
                mv=0.001 ,   #Virus mortality
                mz=0.03      #Zooplankton mortality
)

parameter2<- c( e=0.3 ,      #Remineralization rate
                mu=0.8 ,     #Phytoplankton maximum growth rate
                kN=0.4 ,     #Half-saturation constant
                mp=0.01 ,    #Phytoplankton mortality
                I=0.5  ,     #infection rate
                gz=0.0 ,    #grazing rate 
                deltaV=0.01 ,#virus production
                mv=0.001 ,   #Virus mortality
                mz=0.03      #Zooplankton mortality
)

parameter3<- c( e=0.3 ,      #Remineralization rate
                mu=0.8 ,     #Phytoplankton maximum growth rate
                kN=0.4 ,     #Half-saturation constant
                mp=0.01 ,    #Phytoplankton mortality
                I=0.5  ,     #infection rate
                gz=0.69 ,    #grazing rate 
                deltaV=0.01 ,#virus production
                mv=0.001 ,   #Virus mortality
                mz=0.03      #Zooplankton mortality
)




initial_condition = c(N=97.499, P=2, V=0.001, Z=0.0, D=0) #####initial conditions of state variables


# Numerical method
numerical_method = "lsoda"

time = seq(0, 18000, by=0.1)

require(deSolve)

solution1 = ode(initial_condition, time, model, parameter, numerical_method)


plot(solution1)


# check for mass balance
solutiondf1 <-as.data.frame(solution1)
N<-solutiondf1$N
P<-solutiondf1$P
V<-solutiondf1$V
Z<-solutiondf1$Z
D<-solutiondf1$D
massbalance = N+P+Z+V+D
plot(time, massbalance)



solution2 = ode(initial_condition, time, model, parameter2, numerical_method)

plot(solution2)



# check for mass balance
solutiondf2 <-as.data.frame(solution2)
N<-solutiondf2$N
P<-solutiondf2$P
V<-solutiondf2$V
Z<-solutiondf2$Z
D<-solutiondf2$D
massbalance = N+P+Z+V+D
plot(time, massbalance)

solution3 = ode(initial_condition, time, model, parameter3, numerical_method)

plot(solution3)



# check for mass balance
solutiondf3 <-as.data.frame(solution3)
N<-solutiondf3$N
P<-solutiondf3$P
V<-solutiondf3$V
Z<-solutiondf3$Z
D<-solutiondf3$D
massbalance = N+P+Z+V+D
plot(time, massbalance)

######plotting one model output and all components together
all<-ggplot()+ geom_path(data=solutiondf2, aes(x=time, y=V, col = "Virus"),na.rm = TRUE) +
  geom_path(data=solutiondf2, aes(x=time, y=P, col ="Phytoplankton"),na.rm = TRUE)+ 
  geom_path(data=solutiondf2, aes(x=time, y=Z, col ="Zooplankton"),na.rm = TRUE)+
  geom_path(data=solutiondf2, aes(x=time, y=N, col ="Nutrients"),na.rm = TRUE)+
  geom_path(data=solutiondf2, aes(x=time, y=D, col ="Detritus"),na.rm = TRUE)+
  xlim(values=c(1000,18000))+
  #ylim(values=c(0,20))+
  labs(x = "Time (d)", y = "Concentration (umol N l-1)",color = "Legend", title= "All components")






#####plotting different model outputs ....e.g. growth rates of phytoplankton from 0.1 to 1.2

b1<-ggplot()+ geom_path(data=solutiondf1, aes(x=time, y=V, col = "ref run"),na.rm = TRUE)+ geom_path(data=solutiondf2, aes(x=time, y=V, col ="GZ nul"),na.rm = TRUE)+ geom_path(data=solutiondf3, aes(x=time, y=V, col ="GZ high"),na.rm = TRUE)+
  xlim(values=c(15000,18000))+
  #ylim(values=c(0,20))+
  labs(x = "Time (d)", y = "Concentration (umol N l-1)",color = "Legend", title= "Virus")

b2<-ggplot()+ geom_path(data=solutiondf1, aes(x=time, y=P, col = "ref run"),na.rm = TRUE)+ geom_path(data=solutiondf2, aes(x=time, y=P, col ="GZ nul"),na.rm = TRUE)+ geom_path(data=solutiondf3, aes(x=time, y=P, col ="GZ high"),na.rm = TRUE)+
  xlim(values=c(15000,18000))+
  #ylim(values=c(0,20))+
  labs(x = "Time (d)", y = "Concentration (umol N l-1)",color = "Legend", title= "Phytoplankton")

b3<-ggplot()+ geom_path(data=solutiondf1, aes(x=time, y=Z, col = "ref run"),na.rm = TRUE)+ geom_path(data=solutiondf2, aes(x=time, y=Z, col ="GZ nul"),na.rm = TRUE)+ geom_path(data=solutiondf3, aes(x=time, y=Z, col ="GZ high"),na.rm = TRUE)+
  xlim(values=c(15000,18000))+
  #ylim(values=c(0,50))+
  labs(x = "Time (d)", y = "Concentration (umol N l-1)",color = "Legend", title= "Zooplankton")


b4<-ggplot()+ geom_path(data=solutiondf1, aes(x=time, y=N, col = "ref run"),na.rm = TRUE)+ geom_path(data=solutiondf2, aes(x=time, y=N, col ="GZ nul"),na.rm = TRUE)+ geom_path(data=solutiondf3, aes(x=time, y=N, col ="GZ high"),na.rm = TRUE)+
  xlim(values=c(15000,18000))+
  #ylim(values=c(0,20))+
  labs(x = "Time (d)", y = "Concentration (umol N l-1)",color = "Legend", title= "Nutrients")

b5<-ggplot()+ geom_path(data=solutiondf1, aes(x=time, y=D, col = "ref run"),na.rm = TRUE)+ geom_path(data=solutiondf2, aes(x=time, y=D, col ="GZ nul"),na.rm = TRUE)+ geom_path(data=solutiondf3, aes(x=time, y=D, col ="GZ high"),na.rm = TRUE)+
  xlim(values=c(15000,18000))+
  #ylim(values=c(0,10))+
  labs(x = "Time (d)", y = "Concentration (umol N l-1)",color = "Legend", title= "Detritus")

grid.arrange(b1,b2,b3, b4, b5, nrow =3)

