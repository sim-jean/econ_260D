rm(list = ls(all = TRUE)) 

library(ggplot2)   # Data visualization
library(tidyr)   # Tidy data management
library(dplyr)
library(cowplot)

### 0. Set parameters ####

magnitude = 100
magn_tweak = 3

sizex = magnitude*magn_tweak

graph = T

Horizon = 50

delta = .9
r = .8
K = 50
theta = 1600
beta = 1

small = K/10000

sgrid = seq(small, K, length.out = sizex)

### I. Certain case ####
### A. Define functions ####
growth = function(y){
  return(y + r*y*(1 - y/K))
}

current_payoff = function(y, s){
  z = (theta*(log(s) - log(y)) + beta/3*y^3)
  return(-z)
}

cost = function(x){
  return(theta/x)
}

damage = function(x){
  return(beta*x^2)
}

current_payoff_int = function(y,s){
z = integrate(cost, y, s) 
d = integrate(damage, 0, y)

return((z$value + d$value))
}

# Compare payoff functions : 
current_payoff(1,2)
current_payoff_int(1,2)

value = function(y, s, V){
  xnext = growth(y)
  Vnext = spline(x=sgrid,y=V,xout=xnext)
  out = (current_payoff_int(y, s) + delta*Vnext$y) 
  return(out)
}


### B. Set and do VFI #####
DFall = data.frame()
Vnext = vector()
V = seq(0,0,length.out=sizex)

# Check if value function works:
value(1,2,V)

for(t in Horizon:1)
{
  print(t)
  for(i in 1:sizex)
  {
    s = sgrid[i]
    guess = s/2 
    low = small/100 #lower bound on harvest
    high = s #upper bound on harvest
    Thing = optim(par=guess,
                  fn=value,
                  lower=low,
                  upper=high,
                  V=V,
                  s=s,
                  method='L-BFGS-B')
    hstar = Thing$par
    Vstar = Thing$value
    Vnext[i] = Vstar
    DFnow = data.frame(time=t,s=s,Y_star=hstar,Vstar=Vstar)
    DFall = bind_rows(DFall,DFnow)
  }
  V = Vnext
}

DFall = DFall %>% mutate(control = s - Y_star)

### C. Plot results #####
graph = T

Ph = DFall  %>% subset(time < 10) %>% ggplot() +
  geom_line(aes(x=s,y=Y_star,color=factor(time)),linewidth=1.3) +
  xlab("Stock, x") +
  ylab("Remaining stock, Y") +
  scale_color_discrete(name="Year") +
  theme_bw() #+
  #theme(legend.position = "none") +
  #ylim(c(0,100))
if(graph == T){
  Ph
}

Ph = DFall%>%  ggplot() +
  geom_line(aes(x=s,y=control,color=factor(time)),linewidth=1.3) +
  xlab("Stock, x") +
  ylab("Treatment") +
  scale_color_discrete(name="Year") +
  theme_bw() #+
#theme(legend.position = "none") +
#ylim(c(0,100))
if(graph == T){
  Ph
}


PV = ggplot(data=DFall) +
  geom_path(aes(x=s,y=Vstar,color=factor(time)),linewidth=1.3) +
  xlab("Stock, x") +
  ylab("Value Function, V") +
  scale_color_discrete(name="Year") +
  theme_bw()+ 
  theme(legend.position = "none") 
if(graph == T){
  PV
}

# Store policy function for comparison with uncertain case
Storage = DFall %>%
  subset(time==1) %>% 
  select(s, Vstar, Y_star)

### II. Uncertainty #####
# Set shock parameter and distribution
a = .75
p = 0.5

growth = function(y){
  return(y + r*y*(1 - y/K))
}
stoch_growth = function(y,z){
  w = (1-a)*z*growth(y) + (1+a)*(1-z)*growth(y)
  return(w)
}
values = data.frame()


current_payoff = function(y, s){
  z = (theta*(log(s) - log(y)) + beta/3*y^3)
  return(-z)
}

cost = function(x){
  return(theta/x)
}

damage = function(x){
  return(beta*x^2)
}

current_payoff_int = function(y,s){
  z = integrate(cost, y, s) 
  d = integrate(damage, 0, y)
  
  return((z$value + d$value))
}

# Compare payoff functions : 
current_payoff(1,2)
current_payoff_int(1,2)

value = function(y, s, V){
  xnext = (1-a)*growth(y)
  Vnext_b = spline(x=sgrid,y=V,xout=xnext)
  
  xnexta = (1+a)*growth(y)
  Vnext_a = spline(x=sgrid, y=V, xout=xnexta)
  
  out = (current_payoff_int(y, s) + delta*(p*Vnext_b$y + (1-p)*Vnext_a$y)) 
  return(out)
}

DFall = data.frame()
Vnext = vector()
V = seq(0,0,length.out=sizex)

for(t in Horizon:1)
{
  print(t)
  for(i in 1:sizex)
  {
    s = sgrid[i]
    guess = s/2 
    low = small/100 #lower bound on harvest
    high = s #upper bound on harvest
    Thing = optim(par=guess,
                  fn=value,
                  lower=low,
                  upper=high,
                  V=V,
                  s=s,
                  method='L-BFGS-B')
    hstar = Thing$par
    Vstar = Thing$value
    Vnext[i] = Vstar
    DFnow = data.frame(time=t,s=s,Y_star=hstar,Vstar=Vstar)
    DFall = bind_rows(DFall,DFnow)
  }
  V = Vnext
}

DFall = DFall %>% mutate(control = s - Y_star)

Storage = Storage %>% 
  mutate(V_uncertain = DFall %>% subset(time==1) %>% select(Vstar) %>% pull(),
         Y_uncertain = DFall %>% subset(time==1) %>% select(Y_star) %>% pull())
colnames(Storage) = c('stock', 'V_certain', 'Y_certain', 'V_uncertain', 'Y_uncertain')

Ph = DFall  %>% subset(time < 10) %>% ggplot() +
  geom_line(aes(x=s,y=Y_star,color=factor(time)),linewidth=1.3) +
  xlab("Stock, x") +
  ylab("Remaining stock, Y") +
  scale_color_discrete(name="Year") +
  theme_bw() #+
#theme(legend.position = "none") +
#ylim(c(0,100))

if(graph == T){
  Ph
}


Storage %>%
  ggplot(aes(x=stock))+
  geom_line(aes(y=V_certain, colour = 'Certain'))+
  geom_line(aes(y=V_uncertain, colour = 'Uncertain'))

Storage %>% 
  ggplot(aes(x=stock))+
  geom_line(aes(y=Y_certain, colour = 'Certain'))+
  geom_line(aes(y=Y_uncertain, colour = 'Uncertain'))

# Sensitivity analysis

params = expand.grid(r= c(.5, .6, .7, .8,.9),
delta = c(0.8, 0.85, .9, .95, .99),
a = c(.01, .3 ,.5, .7, .8, .99 ))
