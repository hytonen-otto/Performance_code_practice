import matplotlib.pyplot as plt

file =  open('output.dat','r')

lines = file.readlines()

time = [] 
trouts=[]

for row in lines:
    row = row.split(' ')
    time.append(int(row[0]))
    trouts.append(float(row[1]))

plt.figure()
plt.plot(time,trouts)
plt.xlabel('Time in years')
plt.ylabel('Trout population')
plt.show()