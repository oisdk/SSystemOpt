from pylab import *
from scipy import *
from SloppyCell.ReactionNetworks import *

def methds(object):
    for prop in dir(object):
        print(prop)

net = IO.from_SBML_file('sbml.xml', 'exp1')

import data

m = Model([data.expt], [net])
params = KeyedList([('e', 1)])
print m.cost(params)

# methds(m)
# methds(net)
# print(net.calculate())

# j = m.jacobian_log_params_sens(log(params))
# jtj = dot(transpose(j), j)


