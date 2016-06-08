from SloppyCell.ReactionNetworks import *
n = IO.from_SBML_file('/Users/oisinkidney/Developer/SSystemOpt/Example Models/sbml.txt','')
# print [method for method in dir(n) if callable(getattr(n, method))]
IO.eqns_TeX_file(n)
