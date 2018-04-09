import argparse
import os
import glob
import csv
import numpy as np
import matplotlib.pyplot as plt
from scipy.interpolate import splrep, splev
import itertools

ap = argparse.ArgumentParser()
ap.add_argument("-p", required=True, type=str, help = "input folder path")

args = vars(ap.parse_args())

filepath = args["p"]
print("The path you've specified is {}".format(filepath))

# Run in the given folder
begin(filepath)


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

        # print(population)
        # print(opponents)

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

        # PLOT BEGIN
        
        # x coords - same for both populations
        xnew = np.linspace(pop_X[0],pop_X[-1])
        # splrep define the curve, y = f(x)
        # splev - array of points to return the value AT; the spline
        
        # MEAN VALUES
        # tck_pop_mean = splrep(pop_X,pop_mean,s=1)
        # y_pop_mean = splev(xnew,tck_pop_mean)
        # plt.plot(xnew,y_pop_mean, 'g-', label='Player mean')

        # tck_opp_mean = splrep(opp_X,opp_mean,s=1)
        # y_opp_mean = splev(xnew, tck_opp_mean)
        # plt.plot(xnew, y_opp_mean, 'r-', label='Opponent mean')

        # STANDARD DEVIATIONS
        # tck_pop_sd = splrep(pop_X,pop_sd,s=1)
        # y_pop_sd = splev(xnew,tck_pop_sd)
        # plt.plot(xnew, y_pop_sd, 'g-', label='Player standard deviation')

        # tck_opp_sd = splrep(opp_X,opp_sd,s=1)
        # y_opp_sd = splev(xnew,tck_opp_sd)
        # plt.plot(xnew, y_opp_sd, 'r-', label='Opponent standard deviation')

        # MINIMUM VALUES
        tck_pop_min = splrep(pop_X,pop_min,s=10)
        y_pop_min = splev(xnew,tck_pop_min)
        plt.plot(xnew, y_pop_min, 'g-.', label='Player min')

        tck_opp_min = splrep(opp_X,opp_min,s=10)
        y_opp_min = splev(xnew,tck_opp_min)
        plt.plot(xnew, y_opp_min, 'r-.', label='Opponent min')

        # MAXIMUM VALUES
        # tck_pop_max = splrep(pop_X,pop_max,s=20)
        # y_pop_max = splev(xnew,tck_pop_max)
        # plt.plot(xnew, y_pop_max, 'g--', label='Player max')

        # tck_opp_max = splrep(opp_X,opp_max,s=20)
        # y_opp_max = splev(xnew,tck_opp_max)
        # plt.plot(xnew, y_opp_max, 'r--', label='Opponent max')


# Executes and saves the plot to a file
def begin(fpath):
    for file in glob.glob(os.path.join(fpath, '*.csv')):
        print(file)
        parse_data(file)
    plt.ylim(0,10)
    plt.xlim(1,200)
    plt.grid()
    plt.xticks([1,20,40,60,80,100,120,140,160,180,200])
    plt.xlabel('Generation')
    plt.ylabel('Fitness values')

    # Remove duplicates from legend when plotting mutliple at once
    handles, labels = plt.gca().get_legend_handles_labels()
    i = 1
    while i<len(labels):
        if labels[i] in labels[:i]:
            del(labels[i])
            del(handles[i])
        else:
            i +=1

    # Legend on top with shadow, increase ylim so that it does not cover data points
    plt.legend(handles, labels, loc='upper center', bbox_to_anchor=(0.5, 1.15),
          ncol=3, fancybox=True, shadow=True)

    plt.savefig("05_min_opp.pdf")
    plt.close()


def test_read_datafromfile(file):
    with open(file,"rb") as f_in:
        # only population is printed with (0,None,2) = 1,3,5,7...399
        # only opponents are printed with (1,None,2) = 2,4,6,8...400
        arr = np.genfromtxt(itertools.islice(f_in,1,None,2), delimiter=',',dtype= None, names=['gen','mean','sd','min','max'])
        return arr