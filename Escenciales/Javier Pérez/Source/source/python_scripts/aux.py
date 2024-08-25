# Re-defining the data and creating a line plot with markers as the code execution state was reset

import matplotlib.pyplot as plt
import numpy as np

# First matrix diagonal values
matrix1_diagonal = [1096938, 1109052, 1095563, 1067225, 1056496, 1049110, 1014716, 1023944, 1045464, 1034114]
# Second matrix diagonal values
# Define labels
labels = [2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019]
x = np.arange(len(labels))  # the label locations

# Plot as line plot with markers
fig, ax = plt.subplots(figsize=(10, 6))
line1 = ax.plot(x, matrix1_diagonal, marker='o', linestyle='-', color='#277A7A', label='Producción Hospitalaria')

# Adding value labels to each point
for i, txt in enumerate(matrix1_diagonal):
    ax.annotate(format(txt, ','), (x[i], matrix1_diagonal[i]), textcoords="offset points", xytext=(0,10), ha='center', fontsize=12)

# Setting labels and title
ax.set_xlabel('Años', fontsize=12)
ax.set_ylabel('Valor', fontsize=12)
ax.set_title('Producción Hospitalaria - Chile', fontsize=12)
ax.set_xticks(x)
ax.set_xticklabels(labels, rotation=45, ha='right')
ax.legend()
ax.set_ylim(750000, 1500000)

fig.tight_layout()

plt.show()
