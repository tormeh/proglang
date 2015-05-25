import numpy as np
import matplotlib.pyplot as plt

N=4
ind = np.arange(N)
width = 0.8

plt.bar(ind, [4.797, 2.399, 0.002, 0.179], width)

plt.ylabel('Completion time in seconds')
plt.title('Performance of Execution Model')
plt.xticks(ind+width/2., ('unsynchronized', 'original', 'unsynchronizedNoPrint', 'originalNoPrint') )
plt.yticks(np.arange(0,5,0.5))

plt.show()
