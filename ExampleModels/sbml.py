from SloppyCell.ReactionNetworks import *
n = IO.from_SBML_file('/Users/oisinkidney/Developer/SSystemOpt/ExampleModels/Model1/sbml.xml','')
# IO.eqns_TeX_file(n)
n = n.integrateSensitivity(range(100), returnEvents=True)
n.to_file('sensitivities')
print [method for method in dir(n) if callable(getattr(n, method))]
