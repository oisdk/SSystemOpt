from SloppyCell.ReactionNetworks import *
n = IO.from_SBML_file('sbml.xml','net1')
import data
m = Model([data.expt], [n])
