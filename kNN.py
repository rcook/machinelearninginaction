from numpy import *
import operator

def create_data_set():
  group = array([[1.0, 1.1], [1.0, 1.0], [0, 0], [0, 0.1]])
  labels = ["A", "A", "B", "B"]
  return group, labels

def classify0(in_x, data_set, labels, k):
  data_set_size = data_set.shape[0]
  diff_mat = tile(in_x, (data_set_size, 1)) - data_set
  sq_diff_mat = diff_mat ** 2
  sq_distances = sq_diff_mat.sum(axis=1)
  distances = sq_distances ** 0.5
  sorted_dist_indices = distances.argsort()
  class_count = {}
  for i in range(k):
    label = labels[sorted_dist_indices[i]]
    class_count[label] = class_count.get(label, 0) + 1
  sorted_class_count = sorted(class_count.iteritems(), key=operator.itemgetter(1), reverse=True)
  return sorted_class_count[0][0]

group, labels = create_data_set()
print(classify0([0, 0], group, labels, 3))
print(classify0([1, 1.2], group, labels, 3))
