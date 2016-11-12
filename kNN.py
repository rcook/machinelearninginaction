from numpy import *
import operator

def create_data_set():
  group = array([[1.0,1.1],[1.0,1.0],[0,0],[0,0.1]])
  labels = ["A", "A", "B", "B"]
  return group, labels

def classify0(in_x, data_set, labels, k):
  data_set_size = data_set.shape[0]
  diff_mat = tile(in_x, (data_set_size, 1)) – data_set

group, labels = create_data_set()
classify0([0, 0], group, labels, 3)

