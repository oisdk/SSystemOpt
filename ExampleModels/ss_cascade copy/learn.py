from pylab import *
from scipy import *
from SloppyCell.ReactionNetworks import *

net = IO.from_SBML_file('sbml.xml', 'exp1')

import data

m = Model([data.expt], [net])
params = m.get_params()
j = m.jacobian_log_params_sens(log(params))
jtj = dot(transpose(j), j)


