import matplotlib
import matplotlib.pyplot as plt
from numpy import *
import operator
import os

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
  print(class_count)
  sorted_class_count = sorted(class_count.iteritems(), key=operator.itemgetter(1), reverse=True)
  return sorted_class_count[0][0]

def file2matrix(path):
  classes = {}
  lines = open(path, "rt").readlines()
  rowCount = len(lines)
  columnCount = len(lines[0].split('\t'))
  m = zeros((rowCount, columnCount - 1))
  labels = []
  for i, line in enumerate(lines):
    line = line.strip()
    columns = line.split('\t')
    m[i, :] = columns[: columnCount - 1]
    class_ = columns[-1]
    class_id = classes.get(class_)
    if class_id is None:
      class_id = len(classes) + 1
      classes[class_] = class_id
    labels.append(class_id)
  return m, labels

def main():
  """
  datingDataMat, datingLabels = file2matrix(os.path.expanduser("~/Desktop/machinelearninginaction/Ch02/datingTestSet.txt"))
  fig = plt.figure()
  ax = fig.add_subplot(111)
  ax.scatter(datingDataMat[:, 0], datingDataMat[:, 1], 15.0 * array(datingLabels), 15.0 * array(datingLabels))
  plt.show()
  """
  group, labels = create_data_set()
  r = classify0([0, 0], group, labels, 3)
  print(r)

if __name__ == "__main__":
  main()
