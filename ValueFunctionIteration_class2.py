#region Comments
# This is annoying : why is it not working?
#endregion


import numpy as np
import pandas as pd
import scipy
import seaborn as sns
import matplotlib.pyplot as plt

#region Define parameters
size_x = 100

T = 20

a = 20
b = .1
delta = 1/1.2
r = .8
K = 100
small = K/1000


x_grid = np.linspace(small, K, size_x)
#endregion

#region Define functions
def growth(h,x, r=r, K=K):
    y = x + r*x*(1- x/K) - h
    return y

def profit(h, a=a, b=b):
    y = a*h - b*(h**2)
    return y

def Payoff(h, x, V):
    x_next = growth(h,x)
    V_next = scipy.interpolate.CubicSpline(x_grid,V)
    V_next = V_next(x_next)
    return - (profit(h)+delta*V_next)
#endregion

DF_test = pd.DataFrame(x_grid,Payoff(x_grid, 5, V)).reset_index()

#region Initiate problem:
V = np.repeat(0,size_x)
V_next = [0]*size_x
DF_all = pd.DataFrame(columns = ['t', 'x', 'h_star', 'V_star'])

Payoff(.17992, 0.1, V)
#endregion

#region Routine over a fixed planning horizon
for t in range(T,0,-1):
    print(t)
    for i in range(len(x_grid)):
        guess = x_grid[i]/2
        bnds = scipy.optimize.Bounds((0),(growth(0,x_grid[i])))
        Opti = scipy.optimize.minimize(Payoff,
                                       args = (x_grid[i],V),
                                       x0 = guess,
                                       bounds = bnds,
                                       method = 'L-BFGS-B')
        if Opti.success == False:
            print('Problemo at ' + str(i) + ' in period t=' + str(t))
        Now = [t, x_grid[i], Opti.x[0], -Opti.fun]
        DF_all.loc[len(DF_all)] = Now
        V_next[i] = -Opti.fun
    V = V_next
#endregion
sns.lineplot(DF_all, x='x', y= 'h_star', hue = 't')
plt.show()

sns.lineplot(DF_all, x='x', y= 'V_star', hue = 't')
plt.show()
for t in range(1, 21):
    h_star_now = np.array(DF_all[DF_all['t']==t].h_star)
    h_star_past = np.array(DF_all[DF_all['t']==t-1].h_star)

    score = np.sum((h_star_now-h_star_past)**2)
    print(score)



#region Routine until convergence
V = np.repeat(0,size_x)
V_next = [0]*size_x
DF_all = pd.DataFrame(columns = ['t', 'x', 'h_star', 'V_star'])

metric = 10000
tol = 0.001
step = 0
while metric>tol:
    for i in range(len(x_grid)):
        guess = x_grid[i]/2
        bnds = scipy.optimize.Bounds((0),(growth(0,x_grid[i])))
        Opti = scipy.optimize.minimize(Payoff,
                                       args = (x_grid[i],V),
                                       x0 = guess,
                                       bounds = bnds,
                                       method = 'L-BFGS-B')
        Now = [step, x_grid[i], Opti.x[0], -Opti.fun]
        DF_all.loc[len(DF_all)] = Now
        V_next[i] = -Opti.fun
    V = V_next
    if step >=1:
        h_star_now = np.array(DF_all[DF_all['t']==step].h_star)
        h_star_t_1 = np.array(DF_all[DF_all['t']==(step-1)].h_star)
        metric = np.sum((h_star_now-h_star_t_1)**2)
    step +=1
    print('At step ' + str(step) +', euclidian distance is ' + str(metric))
#endregion

