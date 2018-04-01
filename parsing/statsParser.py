import argparse

ap = argparse.ArgumentParser()
ap.add_argument("-p", required=True, type=str, help = "input folder path")
args = vars(ap.parse_args())

filepath = args["p"]
print("The path containing the co-evolutionary stat .csv files is {}".format(filepath))
