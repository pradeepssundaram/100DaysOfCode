#imports
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

bikesharing = pd.read_csv("./Data/hour.csv")
print(f"Shape of data: {bikesharing.shape}")
print(f"Number of missing values in the data:{bikesharing.isnull().sum().sum()}")
bikesharing.describe()
