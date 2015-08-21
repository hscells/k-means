import matplotlib.pyplot as plt
import numpy as np
import ast, random

def col():
    color = '#{:02x}{:02x}{:02x}'.format(*map(lambda x: random.randint(0, 255), range(3)))
    return color

f = open("k-means.txt","r").read()
f = f.replace(" ", ",");
s = ast.literal_eval(f)

fig = plt.figure()
ax = fig.add_subplot(1, 1, 1)

print s
for data in s:
    print data
    x = [i[0] for i in data]
    y = [i[1] for i in data]
    ax.scatter(x, y, color = col())
plt.show()
