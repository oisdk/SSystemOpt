from SloppyCell.ReactionNetworks import *
n = IO.from_SBML_file('/Users/oisinkidney/Developer/SSystemOpt/ExampleModels/Model1/sbml.xml','')
m = n.integrateSensitivity(range(100), returnEvents=True)
# print [method for method in dir(n) if callable(getattr(n, method))]
