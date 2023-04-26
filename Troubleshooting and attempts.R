# Solve the value function for both states of the world

# 1. z = 1, i.e good state

for (z in c(0,1)){
  if(z==0){
    print('Good state')
  } else {
    print('Bad state')
  }
  
  current_payoff_int = function(y,s){
    k = integrate(cost, y, s) 
    j = integrate(damage, 0, y)
    
    return((k$value + j$value))
  }
  
  
  value = function(y, s, V){
    xnext = stoch_growth(y,z)
    Vnext = spline(x=sgrid,y=V,xout=xnext)
    out = (current_payoff_int(y, s) + delta*Vnext$y) 
    return(out)
  }
  
  DFall = data.frame()
  Vnext = vector()
  V = seq(0,0,length.out=sizex)
  
  
  tol = 0.1
  diff = 1000
  step = 0
  while(diff>tol)
  {
    print(step)
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
      DFnow = data.frame(time=step,s=s,Y_star=hstar,Vstar=Vstar)
      DFall = bind_rows(DFall,DFnow)
    }
    V = Vnext
    if(step>1){
      diff_a = DFall %>% subset(time == step) %>% select(Vstar) %>% pull()
      diff_b = DFall %>% subset(time == step-1) %>% select(Vstar) %>% pull()
      diff = sum((diff_a - diff_b)^2)
    }
    step = step +1
    print(diff)
  }
  Storage = data.frame(DFall%>%subset(time == step -1) %>% select(Vstar), rep(z,sizex), sgrid)
  values = rbind(values, Storage )
  rm(Storage)
}

colnames(values) = c('Value','Sow', "s")
values %>% ggplot(aes(x=s, y=Value, group=Sow))+
  geom_line(aes(colour=factor(Sow)))

# Now use that to solve the probabilistic problem
# Value in t+1 is the expectation of the value functions so
# change the delta *V by *.5*V_bad + .5*V_good