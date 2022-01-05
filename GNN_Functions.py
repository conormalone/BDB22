# -*- coding: utf-8 -*-
import numpy as np
import tensorflow
import spektral
from tensorflow.keras.layers import Dense
from tensorflow.keras.models import Model
from spektral.data import Dataset, BatchLoader, Graph
from spektral.transforms.normalize_adj import NormalizeAdj
from tensorflow.keras.layers import Dropout
from spektral.layers import GCNConv, GlobalSumPool

################################################################################
# Config
################################################################################
learning_rate = 1e-2  # Learning rate
epochs = 100  # Number of training epochs
es_patience = 10  # Patience for early stopping
batch_size = 21  # Batch size
local_all = r.all_data
local_test = r.test
local_train = r.train
local_val = r.validate
local_loo_vis = r.loo_vis
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
        for i in range(self.n_samples):
            # Node features
            iter_x = self.df["train_x"][i].copy()
            x = np.array(iter_x).reshape(21,6)

            # Edges
            iter_a =  self.df["train_a"][i].copy()
            a = np.array(iter_a).reshape(21,21)

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
len_train = len(local_train["train_x"])
len_val = len(local_val["train_x"])
len_test = len(local_test["train_x"])
len_all = len(local_all["train_x"])
len_loo_vis= len(local_loo_vis["train_x"])
data_tr = GraphDataset(n_samples = len_train, df = local_train, transforms=NormalizeAdj())
data_va = GraphDataset(n_samples = len_val, df = local_val, transforms=NormalizeAdj())
data_te = GraphDataset(n_samples = len_test, df=local_test, transforms=NormalizeAdj())
data_all = GraphDataset(n_samples = len_all, df=local_all, transforms=NormalizeAdj())
data_loo_vis = GraphDataset(n_samples = len_loo_vis, df=local_loo_vis, transforms=NormalizeAdj())
# Data loaders
loader_tr = BatchLoader(data_tr, batch_size=batch_size, epochs=epochs)
loader_va = BatchLoader(data_va, batch_size=batch_size)
loader_te = BatchLoader(data_te, batch_size=batch_size)
loader_all = BatchLoader(data_all, batch_size=batch_size)
loader_loo_vis = BatchLoader(data_loo_vis, batch_size=batch_size)


################################################################################
# Build model
################################################################################
class BDB22GNN(Model):

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


model = BDB22GNN(21, 120)
model.compile('adam', "mean_absolute_error")

model.fit(loader_tr.load(), validation_data= loader_va.load(), steps_per_epoch=loader_tr.steps_per_epoch,
    validation_steps=loader_va.steps_per_epoch, epochs=100)

test_loss = model.evaluate(loader_te.load(), steps=loader_te.steps_per_epoch)

print('Test loss: {}'.format(test_loss))

len_all = 13663
#for each blocker in each play creates graph for which predictions are made
class LooGraphDataset(Dataset):
    def __init__(self, loo_n_samples, loo_df, **kwargs):
        self.loo_n_samples = loo_n_samples
        self.loo_df = loo_df  
        super().__init__(**kwargs)

    def read(self):
        output = []
        for i in range(self.loo_n_samples):
            # Node features
            y = np.zeros((120,))
            y_index = int(19+self.loo_df["y"][i])
            y[y_index:] = 1
            for j in range(10):
                iter_x = self.loo_df["loo_x"][i][j].copy()
                x = np.array(iter_x).reshape(21,6)
                # Edges
                iter_a = self.loo_df["loo_a"][i][j].copy()
                a = np.array(iter_a).reshape(21,21)
            
                output.append(Graph(x=x, a=a, y=y))
        return(output)        
        
output_compiler = LooGraphDataset(loo_n_samples = len_all, loo_df = local_all, transforms=NormalizeAdj())
loo_loader = BatchLoader(output_compiler, batch_size=batch_size)
predictions = model.predict(loo_loader.load(), steps =loo_loader.steps_per_epoch)

loo_vis_output_compiler = LooGraphDataset(loo_n_samples = len_loo_vis, loo_df = local_loo_vis, transforms=NormalizeAdj())
loo_vis_loader = BatchLoader(loo_vis_output_compiler, batch_size=batch_size)
loo_vis_predictions = model.predict(loo_vis_loader.load(), steps =loo_vis_loader.steps_per_epoch)

