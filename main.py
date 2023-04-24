#Packages
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import itertools

# Define parameters
alpha = [0.8, .85, .9]
gamma = [100, 150, 200]
theta = [1, 3, 10]
delta = [0.9, .95, .99]

C = [100, 200, 300]

param = list(itertools.product(alpha, gamma, theta, delta, C))
# Define functions
results_tot = pd.DataFrame()
t1 = np.arange(0.5,100,0.1)
for i in range(len(param)):
    def growth(t):
        return t ** param[i][0]

    def growth_deriv(t):
        return param[i][0] * t ** (param[i][0] - 1)

    def P_linear(t):
        return (param[i][1] - param[i][2] * t)

    P_deriv = - param[i][2]

    def elasticity(t):
        return -param[i][2]*t/P_linear(t)

    def current_profit(t):
        y = P_linear(growth(t))*growth(t) - param[i][-1]
        return y

    def common_part(t):
        y = np.log(param[i][3])*current_profit(t)
        return y

    def monop_part(t):
        y = (1-param[i][3]**t)*growth_deriv(t)*(P_linear(t) + P_deriv * growth(t))
        return y
    def comp_part(t):
        y = (1-(param[i][3]**t))*growth_deriv(t)*P_linear(t)
        return y

    def value(t):
        y = (param[i][3]**t)/(1-param[i][3]**t)*current_profit(t)
        return y

    results = pd.DataFrame(t1)
    results['common'] = common_part(t1)
    results['monop'] = monop_part(t1)
    results['comp'] = comp_part(t1)
    results['eq_comp'] = results['common'] + results['comp']
    results['eq_monop'] = results['common'] + results ['monop']
    results['verif_m'] = np.sign(results.eq_monop).diff().ne(0)
    results['verif_c'] = np.sign(results.eq_comp).diff().ne(0)

    time_m = results[results['verif_m']==True][0].tolist()[-1]
    time_c = results[results['verif_c']==True][0].tolist()[-1]

    row = list(param[i])
    row.extend([time_m, time_c, value(time_m), value(time_c)])
    row = pd.DataFrame(row).transpose()

    results_tot = pd.concat([results_tot, row])
results_tot.columns = ['alpha', 'gamma', 'theta', 'delta', 'C', 't_m', 't_c', 'v_m', 'v_c']
