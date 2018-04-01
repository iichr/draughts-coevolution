import argparse
import numpy as np
import matplotlib.pyplot as plt
from scipy.interpolate import splrep, splev
import itertools


ap = argparse.ArgumentParser()
ap.add_argument("-p", required=True, type=str, help = "input folder path")
args = vars(ap.parse_args())

filepath = args["p"]
print("The path containing the co-evolutionary stat .csv files is {}".format(filepath))

def parse_data(file):
    with open(file,"rb") as f_in:
        population = np.genfromtxt(itertools.islice(f_in,0,None,2), delimiter=',',dtype= None, names=['gen','mean','sd','min','max'])
        f_in.seek(0)
        opponents = np.genfromtxt(itertools.islice(f_in,1,None,2), delimiter=',',dtype= None, names=['gen','mean','sd','min','max']) 
        
        # changes the first value of an iterable to the current count, beginning from 1 up to length
        # used to renumber the populations
        for i,element in enumerate(population,1):
        	element[0] = i

        for i,element in enumerate(opponents,1):
            element[0] = i

        print(population)
        print(opponents)

        pop_X = population['gen']
        opp_X = opponents['gen']
        np.testing.assert_array_equal(pop_X,opp_X)

        # round means and SDs
        pop_mean = np.round(population['mean'],4)
        pop_sd = np.round(population['sd'],4)

        opp_mean = np.round(opponents['mean'],4)
        opp_sd = np.round(opponents['sd'],4)

        # get minimum values
        pop_min = population['min']
        opp_min = opponents['min']

        # get maximum values
        pop_max = population['max']
        opp_max = opponents['max']

        # plotting

        # x coords - same for both populations
        xnew = np.linspace(pop_X[0],pop_X[-1])
        # define the curve, y = f(x)
        tck_pop_mean = splrep(pop_X,pop_mean,s=3)
         # splev - array of points to return the value AT; the spline
        y_pop_mean = splev(xnew,tck_pop_mean)
        plt.plot(xnew,y_pop_mean)

        tck_opp_mean = splrep(opp_X,opp_mean,s=3)
        y_opp_mean = splev(xnew, tck_opp_mean)
        plt.plot(xnew, y_opp_mean)


        plt.savefig("test1.png")