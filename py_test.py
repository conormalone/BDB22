# -*- coding: utf-8 -*-
"""
Created on Tue Nov 16 14:35:10 2021

@author: conny
"""

import numpy as np
import tensorflow
import spektral
from tensorflow.keras.layers import Dense
from tensorflow.keras.models import Model
from spektral.data import Dataset, BatchLoader, Graph
from spektral.transforms.normalize_adj import NormalizeAdj
from tensorflow.keras.layers import Dropout
from spektral.layers import GCNConv, GlobalSumPool
from sklearn.model_selection import GroupShuffleSplit

################################################################################
# Config
################################################################################
learning_rate = 1e-2  # Learning rate
epochs = 100  # Number of training epochs
es_patience = 10  # Patience for early stopping
batch_size = 32  # Batch size
local_data = r.dataset
################################################################################
# Test Train Split
################################################################################
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.1, stratify=df['Unique ID'].values)
################################################################################
# Load data
################################################################################

class GraphDataset(Dataset):
    def __init__(self, n_samples, df, n_colors=120, **kwargs):
        self.n_samples = n_samples
        self.df = df  
        self.n_colors = n_colors  
        super().__init__(**kwargs)

    def read(self):
        output = []
        #df_subset_x = self.df[1]
        #df_subset_a = self.df[2]
        #df_subset_y = self.df[0]
        for i in range(self.n_samples):
            # Node features
            iter_x = self.df["x"][i].copy()
            x = np.array(iter_x).reshape(22,6)

            # Edges
            iter_a =  self.df["a"][i].copy()
            a = np.array(iter_a).reshape(22,22)

            # 
            y = np.zeros((120,))
            y_index = int(19+self.df["y"][i])
            y[y_index:] = 1
           
            output.append(Graph(x=x, a=a, y=y))
            print(i)
        return(output)

        # We must return a list of Graph objects

 #
# Train/valid/test split
len_test = len(local_test["x"])
len_val = len(local_val["x"])
data_te = GraphDataset(n_samples = len_test, df=local_test, transforms=NormalizeAdj())
data_va = GraphDataset(n_samples = len_val, df = local_val, transforms=NormalizeAdj())
# Data loaders
loader_te = BatchLoader(data_te, batch_size=batch_size)
loader_va = BatchLoader(data_va, batch_size=batch_size)
################################################################################
# Build model
################################################################################
class MyFirstGNN(Model):

    def __init__(self, n_hidden, n_labels):
        super().__init__()
        self.graph_conv = GCNConv(n_hidden)
        self.pool = GlobalSumPool()
        self.dropout = Dropout(0.5)
        self.dense = Dense(n_labels, 'sigmoid')

    def call(self, inputs):
        out = self.graph_conv(inputs)
        out = self.dropout(out)
        out = self.pool(out)
        out = self.dense(out)

        return out


model = MyFirstGNN(32, 120)
model.compile('adam', "mean_squared_error")

class LOOGraphDataset(Dataset):
    def __init__(self, df, **kwargs):
        self.df = df  
        super().__init__(**kwargs)

    def read(self):
        output = []
        #df_subset_x = self.df[1]
        #df_subset_a = self.df[2]
        #df_subset_y = self.df[0]
        for i in range(len(self.df["loo_x"])):
            # Node features
            iter_x = self.df["loo_x"][i].copy()
            x = np.array(iter_x).reshape(21,6)
            y = np.zeros((120,))
            y_index = int(19+self.df["y"][i])
            y[y_index:] = 1
            for j in range(self.df["loo_a"].shape[1]):
                # Edges
                iter_a =  self.df["loo_a"][i][j].copy()
                a = np.array(iter_a).reshape(21,21)

            
                output.append(Graph(x=x, a=a, y=y))
        len_all = self.df["loo_a"].shape[0] * self.df["loo_a"].shape[1] 
        output_compiler = GraphDataset(n_samples = len_all, df = output, transforms=NormalizeAdj())
        loo_loader = BatchLoader(output_compiler, batch_size=batch_size)
        predictions = model.predict(loo_loader)
        return(predictions)        