# -*- coding: utf-8 -*-
# Guochen Dai

import numpy as np
import random as rd
import matplotlib.pyplot as plt

x1= [rd.gauss(5,1) for i in range(100)]+[rd.gauss(0,1) for i in range(100)]+[rd.gauss(11,1) for i in range(100)]
x2= [rd.gauss(5,1) for i in range(100)]+[rd.gauss(0,1) for i in range(100)]+[rd.gauss(1,1) for i in range(100)]
data= np.transpose(np.array(x1+x2).reshape((2,300)))

plt.scatter(data[:,0],data[:,1])

def return_cluster(sample, center_list):
    temp = [sum((sample-c)**2) for c in center_list]
    return temp.index(min(temp))
  
def init_center(data, k):  
    sample_num = data.shape[0]
    return data[rd.sample(xrange(sample_num),k),:]

# k-means cluster  
def kmeans(data, k):  
    sample_num = data.shape[0]  
    cluster_label = np.zeros(sample_num)  
    cluster_change = True  
  
    # step 1: init centroids  
    centers = init_center(data, k)  
    
    while cluster_change:  
        total_cost=0
        cluster_change = False
        # step 2: caculate cluster
        for i in xrange(sample_num):
            new_label = return_cluster(data[i,:], centers)
            total_cost += sum((data[i,:]- centers[new_label])**2)
            if cluster_label[i] != new_label:
                cluster_label[i] = new_label
                cluster_change = True
            
    # step 3: update centers  
    for j in range(k):
        centers[j, :] = np.mean(data[cluster_label == j,:],axis=0)
        
    return centers, cluster_label, total_cost

def plot_cluster(data, k):  
    if k<=7:
        sample_num = data.shape[0]
        res = kmeans(data, k)
        centers = res[0]
        cluster_label = res[1]
           
        mark = ['or', 'ob', 'og', 'ok', 'oc', 'om', 'oy']   
        # draw all samples
        for i in xrange(sample_num):  
            plt.plot(data[i, 0], data[i, 1], mark[int(cluster_label[i])])        
        # draw the centroids
        mark = ['*r', '*b', '*g', '*k', '*c', '*m', '*y']
        for i in range(k):  
            plt.plot(centers[i, 0], centers[i, 1], mark[i], markersize = 12)   
    else:
        return False
            
        
plot_cluster(data, 3)

#plot cost function value - numbers of k
plt.plot(range(1,7),np.mean([[kmeans(data, i)[2] for i in range(1,7)] for j in range(30)],axis=0))



