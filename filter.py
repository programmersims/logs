# -*- coding: utf-8 -*-
import sys
import pandas as pd
import numpy as np
import os

fildes = open(sys.argv[1], "r")
lines = fildes.readlines()
output = []
for l in lines:
  sp = l.split("\"")
  if len(sp) > 1:
    output.append(sp[1].split(","))

output = pd.DataFrame(output)
output.to_csv("output.csv")
fildes.close()
os.system("Rscript avg.R")
