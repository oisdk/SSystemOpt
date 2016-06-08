from SloppyCell.ReactionNetworks import *
n = IO.from_SBML_file('/Users/oisinkidney/Developer/SSystemOpt/ExampleModels/Model1/sbml.xml','')
# n = IO.eqns_TeX_file(n)
print [method for method in dir(n) if callable(getattr(n, method))]