import numpy as np
import pandas as pd
import os
import scipy


size_x = 100
T = 20

# Define parameters
delta = .9
beta = 1
theta = 1600
r = .8
K = 50

# Define state space
state = np.arange(0.1, K+1, K/size_x)
# Define functions
def mc_s(x, theta=theta):
    return(theta/x)

def growth(y, r=r, K=K):
    return (y + r*y*(1-y/K))

def k_s(x, beta=beta):
    return beta*(x**2)

def integral_mc_s(y,S, theta=theta):
    return theta*(np.log(S) - np.log(y))

def integral_k_s(y, beta=beta):
    return (beta/3)*(y**3)

def instant_payoff(y,S):
    return (integral_k_s(y) + integral_mc_s(y, S))

def payoff(y,S,V):
    x_next = growth(y)
    V_next = scipy.interpolate.CubicSpline(x=state, y=V)
    V_next = V_next(x_next)
    return(instant_payoff(y,S) + delta*V_next)

for t in range(T,-1, -1):
    print(t)
    for i in range(len(state)):
        x = state[i]
        guess = x / 2
        low = 0  # lower bound on harvest
        high = growth(x)  # upper bound on harvest

        Thing = optim(par=guess, fn=Payoff, lower=low, upper=high, x=x, V=V, method='L-BFGS-B')

        hstar = Thing$par
        Vstar = -Thing$value
        Vnext[i] = Vstar
        DFnow = data.frame(time=t, x=x, hstar=hstar, Vstar=Vstar)
        DFall = bind_rows(DFall, DFnow)
        }
        V = Vnext
    }