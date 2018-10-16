import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import numpy as np
import csv


def f(x,y):
    print(x)
    return np.array(z_dirty[y][x])

fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')
ax.set_xlabel("point")
ax.set_ylabel("time")
ax.set_zlabel("temp")
x_dirty = []
y_dirty = []
z_dirty = []
i=0
with open('result.csv',newline='') as csvfile:
    spamreader = csv.reader(csvfile,delimiter=';')
    for row in spamreader:
        i+=1
        if(x_dirty==[]):
            x_dirty = [c for c in range(0,len(row))]
        z_dirty.append(row)
y_dirty = [c for c in range(0,i)]
    

X = np.array(x_dirty,int)
Y = np.array(y_dirty,int)

print(X)
Z = np.array(z_dirty,float)
X_new, Y_new = np.meshgrid(X, Y)


# _, Z = np.meshgrid(X_new, Z[0])

ax.plot_wireframe(X_new,Y_new,Z)
plt.show()