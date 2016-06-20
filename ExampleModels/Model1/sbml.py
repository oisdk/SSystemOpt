def methods(m):
    for method in dir(m):
        print(method)
from pylab import *
from scipy import *
from SloppyCell.ReactionNetworks import *
n = IO.from_SBML_file('sbml.xml','net1')
import data
m = Model([data.expt], [n])
params = n.GetParameters()
# print(m.cost(params))
# params = Optimization.fmin_lm_log_params(m, params, maxiter=2)
# print(params)
j = m.jacobian_log_params_sens(params)
with open('jacobian', 'w') as file:
    file.write(str(j))
# print(methods(n))

# figure()
# Plotting.plot_model_results(m)
# show()
# print(m.cost(n.GetParameters()))
# prams = Optimization.fmin_lm_log_params(m,n.GetParameters(),maxiter=2)
# print(prams)
