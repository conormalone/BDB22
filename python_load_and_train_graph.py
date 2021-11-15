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
batch_size = 32  # Batch size
local_train = r.train
local_test = r.test
local_val = r.validate
################################################################################
# Load data
################################################################################

class GraphDataset(Dataset):
    def __init__(self, n_samples, df, n_colors=120, **kwargs):
        self.n_samples = n_samples
        self.n_colors = n_colors  
        self.df = df    
        super().__init__(**kwargs)

    def read(self):
        output = []
        for i in range(self.n_samples):
            # Node features
            df_subset = self.df[i]
            iter_x = df_subset[1]
            x = np.array(iter_x).reshape(22,4)

            # Edges
            iter_a =  df_subset[2]
            a = np.array(iter_a).reshape(22,22)

            # 
            y = np.zeros((120,))
            y_index = int(19+df_subset[0])
            y[y_index:] = 1
           
            output.append(Graph(x=x, a=a, y=y))
            print(i)
        return(output)

        # We must return a list of Graph objects

 #
# Train/valid/test split
data_tr = GraphDataset(len(local_train[1]), df=local_train, transforms=NormalizeAdj())
data_va = GraphDataset(len(local_val[1]), df=local_val, transforms=NormalizeAdj())
data_te = GraphDataset(len(local_test[1]), df=local_test, transforms=NormalizeAdj())

# Data loaders
loader_tr = BatchLoader(data_tr, batch_size=batch_size, epochs=epochs)
loader_va = BatchLoader(data_va, batch_size=batch_size)
loader_te = BatchLoader(data_te, batch_size=batch_size)

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

model.fit(loader_tr.load(), validation_data= loader_va.load(), steps_per_epoch=loader_tr.steps_per_epoch,
    validation_steps=loader_va.steps_per_epoch, epochs=100)

test_loss = model.evaluate(loader_te.load(), steps=loader_te.steps_per_epoch)

print('Test loss: {}'.format(test_loss))


#predictions = model.predict(loader_te.load(), steps = loader_te.steps_per_epoch)