#### Section II #####

delta = .95
alpha = .8
C = 100
gamma = 100
theta = 8

growth = function(t){
  return(t^alpha)
}
growth_deriv = function(t){
  return(alpha*t^(alpha-1))
}

P_linear = function(t){
  return(gamma - theta*t)
}

P_deriv = -theta

elasticity = function(t){
  return(gamma*t/P_linear(t))
}

t1 = seq(.1,200,by=0.01)

### Attempt 1: 
current_profit = function(t){
  y = P_linear(growth(t))*growth(t) - C
  return(y)
}

common_part = function(t){
  y = log(delta)*current_profit(t)
  return(y)
}

monop_part = function(t){
  y = (1-delta^t)*(growth_deriv(t)*(P_deriv * growth(t) + P_linear(t)))
  return(y)
}

comp_part = function(t){
  y = (1-delta^t)*growth_deriv(t)*P_linear(t)
  return(y)
}

value = function(t){
  y = delta^t/(1-delta^t)*profit(t)
  return(y)
}
results = data.frame(t1, common = common_part(t1), monop = monop_part(t1), comp = comp_part(t1))%>%
  mutate(eq_monop = common + monop, 
         eq_comp = common + comp)

results %>% subset(eq_comp<0.01 & eq_comp >-.01)
t_comp = 4.835
value(t_comp)

results %>% subset(eq_monop < 0.05 & eq_monop > -.05)
t_monop = 3.175
value(t_monop)

results_tot_og = data.frame(t_comp, value_comp = value(t_comp), t_monop, value_monop = value(t_monop), delta = delta, 
                            theta = theta, spec = 'OG')

#### Sensitivity analysis ####


delta = .95
alpha = .8
C = 100
gamma = 100
theta = 2

growth = function(t){
  return(t^alpha)
}
growth_deriv = function(t){
  return(alpha*t^(alpha-1))
}

P_linear = function(t){
  return(gamma - theta*t)
}

P_deriv = -theta

elasticity = function(t){
  return(gamma*t/P_linear(t))
}

t1 = seq(.1,200,by=0.01)

### Attempt 1: 
current_profit = function(t){
  y = P_linear(growth(t))*growth(t) - C
  return(y)
}

common_part = function(t){
  y = log(delta)*current_profit(t)
  return(y)
}

monop_part = function(t){
  y = (1-delta^t)*(growth_deriv(t)*(P_deriv * growth(t) + P_linear(t)))
  return(y)
}

comp_part = function(t){
  y = (1-delta^t)*growth_deriv(t)*P_linear(t)
  return(y)
}

value = function(t){
  y = delta^t/(1-delta^t)*profit(t)
  return(y)
}
results = data.frame(t1, common = common_part(t1), monop = monop_part(t1), comp = comp_part(t1))%>%
  mutate(eq_monop = common + monop, 
         eq_comp = common + comp)

results %>% subset(eq_comp<0.01 & eq_comp >-.01)
t_comp = 4.745
value(t_comp)

results %>% subset(eq_monop < 0.05 & eq_monop > -.05)
t_monop = 4.135
value(t_monop)

results_tot = data.frame(t_comp, value_comp = value(t_comp), t_monop, value_monop = value(t_monop), delta = delta, 
                         theta = theta, spec = 'sensitivity delta')
results_tot_og = rbind(results_tot_og, results_tot)

#### Sensitivity analysis ####


delta = .98
alpha = .8
C = 100
gamma = 100
theta = 8

growth = function(t){
  return(t^alpha)
}
growth_deriv = function(t){
  return(alpha*t^(alpha-1))
}

P_linear = function(t){
  return(gamma - theta*t)
}

P_deriv = -theta

elasticity = function(t){
  return(gamma*t/P_linear(t))
}

t1 = seq(.1,200,by=0.01)

### Attempt 1: 
current_profit = function(t){
  y = P_linear(growth(t))*growth(t) - C
  return(y)
}

common_part = function(t){
  y = log(delta)*current_profit(t)
  return(y)
}

monop_part = function(t){
  y = (1-delta^t)*(growth_deriv(t)*(P_deriv * growth(t) + P_linear(t)))
  return(y)
}

comp_part = function(t){
  y = (1-delta^t)*growth_deriv(t)*P_linear(t)
  return(y)
}

value = function(t){
  y = delta^t/(1-delta^t)*profit(t)
  return(y)
}
results = data.frame(t1, common = common_part(t1), monop = monop_part(t1), comp = comp_part(t1))%>%
  mutate(eq_monop = common + monop, 
         eq_comp = common + comp)

results %>% subset(eq_comp<0.01 & eq_comp >-.01)
t_comp = 5.375
value(t_comp)

results %>% subset(eq_monop < 0.05 & eq_monop > -.05)
t_monop = 3.305
value(t_monop)

results_tot = data.frame(t_comp, value_comp = value(t_comp), t_monop, value_monop = value(t_monop), delta = delta, 
                         theta = theta, spec = 'sensitivity delta')
results_tot_og = rbind(results_tot_og, results_tot)

#### New attempt : 

delta <- c(0.9, 0.95, 0.99)
alpha = c(0.8, 0.85, 0.9)
gamma = c(100, 150, 200)
theta = c(1, 3, 10)

params = data.frame(delta, alpha, gamma, theta) %>% expand.grid()

t1 = seq(.1,200,by=0.01)

results_tot_og = data.frame()
for (i in 1:nrow(params)){
  delta = params[i, 1]
  alpha = params[i, 2]
  gamma = params[i, 3]
  theta = params[i, 4]
  
  # Reset function values
  growth = function(t){
    return(t^alpha)
  }
  growth_deriv = function(t){
    return(alpha*t^(alpha-1))
  }
  P_linear = function(t){
    return(gamma - theta*t)
  }
  P_deriv = -theta
  elasticity = function(t){
    return(gamma*t/P_linear(t))
  }
  
  current_profit = function(t){
    y = P_linear(growth(t))*growth(t) - C
    return(y)
  }
  
  common_part = function(t){
    y = log(delta)*current_profit(t)
    return(y)
  }
  
  monop_part = function(t){
    y = (1-delta^t)*(growth_deriv(t)*(P_deriv * growth(t) + P_linear(t)))
    return(y)
  }
  
  comp_part = function(t){
    y = (1-delta^t)*growth_deriv(t)*P_linear(t)
    return(y)
  }
  
  value = function(t){
    y = delta^t/(1-delta^t)*profit(t)
    return(y)
  }
  
  results = data.frame(t1, common = common_part(t1), monop = monop_part(t1), comp = comp_part(t1))%>%
    mutate(eq_monop = common + monop, 
           eq_comp = common + comp)
  
  candidat_eq = results %>% subset(eq_comp<0.1 & eq_comp >-.1)
  for (j in 2:nrow(candidat_eq)){
    if ((candidat_eq[j-1, 6] < 0 & candidat_eq[j, 6] > 0) |
        (candidat_eq[j-1, 6] > 0 & candidat_eq[j, 6] < 0)) break
  }
  t_comp = (candidat_eq[j-1, 1] + candidat_eq[j, 1])/2
  
  candidat_eq = results %>% subset(eq_monop<0.1 & eq_monop>-.1)
  for (j in 2:nrow(candidat_eq)){
    if ((candidat_eq[j-1, 6] < 0 & candidat_eq[j, 6] > 0) |
        (candidat_eq[j-1, 6] > 0 & candidat_eq[j, 6] < 0)) break
  }
  t_monop = (candidat_eq[j-1, 1] + candidat_eq[j, 1])/2
  
  
  
  
  results_tot_og = rbind(results_tot_og, data.frame(delta, alpha, gamma, theta, t_comp, t_monop, value_monop = value(t_monop), value_comp = value(t_comp)))
}

data.frame(q = seq(1,gamma/theta)) %>% mutate(elasticity = - elasticity(q))%>%
  ggplot(aes(x=q, y= elasticity))+geom_line()

results_valid = results_tot_og %>% 
  subset(value_monop >0) %>%
  mutate(longer = t_comp - t_monop)

