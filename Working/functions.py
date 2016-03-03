from scipy.stats import uniform
import numpy as np
import subprocess
import sys

def my_simulation(v):

    exeloc = '/Users/oisinkidney/Desktop/Code/SSystems/.stack-work/install/x86_64-osx/lts-5.1/7.10.3/bin/simulate'
    proc = subprocess.Popen([exeloc, "/Users/oisinkidney/Desktop/Code/SSystems/Working/model"], stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    outs, _ = proc.communicate(','.join(name + ' ' + str(val) for name, val in v.items()) + '\n')
    res = np.atleast_2d([[float(n) for n in row.split()] for row in outs.splitlines()])
    return res


def my_prior(par, func=False):

    gap = par['max'] - par['min']
    pdf = uniform(loc=par['min'], scale=gap)
    return pdf if func else pdf.rvs()


def my_distance(d2, p):

    mean_obs = np.mean(p['dataset1'])
    std_obs = np.std(p['dataset1'])

    gmean = abs((mean_obs - np.mean(d2)) / mean_obs)
    gstd = abs((std_obs - np.std(d2)) / std_obs)

    rho = gmean + gstd

    return np.atleast_1d(rho)
