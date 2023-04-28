rm(list = ls(all = TRUE)) 
setwd()

library(ggplot2)   # Data visualization
library(tidyr)   # Tidy data management
library(dplyr)
library(cowplot)


sizex = 100 #size of the state grid

T=20 #time horizon for backward induction

a=20
b=.1
delta=1/1.2
r=.8
K=100
small=K/1000

xgrid = seq(small,K,length.out=sizex)

f = function(h,x)
{
  xnext = x + r*x*(1-x/K) - h
}

pi = function(h)
{
  profit = a*h-b*h^2
}

Payoff = function(h,x,V)
{
  xnext = f(h,x)
  Vnext = spline(x=xgrid,y=V,xout=xnext)
  negout = -(pi(h) + delta*Vnext$y) 
  return(negout)
}

DFall = data.frame()
Vnext = vector()
V = seq(0,0,length.out=sizex)

#Try payoff function
z=Payoff(.17992,.1,V)

for(t in T:1)
{
  print(t)
  for(i in 1:sizex)
  {
    x = xgrid[i]
    guess = x/2 
    low = 0 #lower bound on harvest
    high = x + r*x*(1-x/K) #upper bound on harvest
    Thing = optim(par=guess,
                  fn=Payoff,
                  lower=low,
                  upper=high,
                  x=x,
                  V=V,
                  method='L-BFGS-B')
    hstar = Thing$par
    Vstar = -Thing$value
    Vnext[i] = Vstar
    DFnow = data.frame(time=t,x=x,hstar=hstar,Vstar=Vstar)
    DFall = bind_rows(DFall,DFnow)
  }
  V = Vnext
}
write.csv(DFall, "C:/Users/jean/PycharmProjects/costello_class/results_with_r.csv")
### Test while ####
tol = 1
diff = 1000
step = 0

DFall = data.frame()
Vnext = vector()
V = seq(0,0,length.out=sizex)

while(diff > tol){
  print(step)
  for(i in 1:sizex)
  {
    x = xgrid[i]
    
    if (step ==0){
      guess = x/2 
    } else {
      guess = DFall$hstar[20*(step-1)+i]
    }
    
    low = 0 #lower bound on harvest
    high = x + r*x*(1-x/K) #upper bound on harvest
    
    Thing = optim(par=guess,
                  fn=Payoff,
                  lower=low,
                  upper=high,
                  x=x,
                  V=V,
                  method='L-BFGS-B')
    
    hstar = Thing$par
    Vstar = -Thing$value
    Vnext[i] = Vstar
    
    DFnow = data.frame(time=step,x=x,hstar=hstar,Vstar=Vstar)
    DFall = bind_rows(DFall,DFnow)
  
  }
  V = Vnext
  if (step>1){
    diff = DFall %>% subset(time == step) %>% pull() -  DFall %>% subset(time == step-1) %>% pull()
    diff = sum(diff^2)
  }
  step = step + 1
  }
######

Ph = ggplot(data=DFall) +
  geom_line(aes(x=x,y=hstar,color=factor(time)),linewidth=1.3) +
  xlab("Stock, x") +
  ylab("Harvest, h") +
  scale_color_discrete(name="Year") +
  theme_bw() +
  theme(legend.position = "none") +
  ylim(c(0,100))
Ph

PV = ggplot(data=DFall) +
  geom_path(aes(x=x,y=Vstar,color=factor(time)),size=1.3) +
  xlab("Stock, x") +
  ylab("Value Function, V") +
  scale_color_discrete(name="Year") +
  theme_bw()
PV

#Forward Simulation
DFopt = DFall %>% filter(time==1)
hpol = DFopt$hstar
xpol = DFopt$x
xsim=vector()
hsim=vector()

xsim[1]=K/10
Tsim = seq(1,20)

for(tt in Tsim)
{
  Thing = spline(x=xpol,y=hpol,xout=xsim[tt])
  hsim[tt] = Thing$y
  if(tt<max(Tsim))
  {
     xsim[tt+1] = f(h=hsim[tt],x=xsim[tt]) 
  }

}

DFsim = data.frame(time=Tsim,xsim=xsim,hsim=hsim)

Pxsim = ggplot(data=DFsim) +
  geom_line(aes(x=time,y=xsim),color="skyblue3",size=1.5) +
  geom_point(aes(x=time,y=xsim),color="black") +
  xlab("Time") +
  ylab("Stock, x") +
  theme_bw()
#Pxsim

Phsim = ggplot(data=DFsim) +
  geom_line(aes(x=time,y=hsim),color="skyblue3",size=1.5) +
  geom_point(aes(x=time,y=hsim),color="black") +
  xlab("Time") +
  ylab("Harvest, h") +
  theme_bw()
#Phsim

Pall = plot_grid(Ph,PV,Pxsim,Phsim,ncol=2,nrow=2)
Pall

ggsave(filename="../Fig1.png",plot=Pall,width=6,height=5,unit="in")



