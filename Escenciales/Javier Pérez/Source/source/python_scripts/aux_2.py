# Recreating the bar chart with the provided data after session reset

import matplotlib.pyplot as plt
import numpy as np

# Data
categories = ["Intervenciones Quirúrgicas", "Subtítulo 21", "Egreso Hospitalario"]
values = [21, 12, 3]

x_pos = np.arange(len(categories))  # the label locations

fig, ax = plt.subplots(figsize=(10, 6))
bars = ax.bar(x_pos, values, color='#277A7A')  # using the same color as before

# Add text for labels, title and custom x-axis tick labels, etc.
ax.set_xlabel('Variables')
ax.set_ylabel('Valor')
ax.set_title('Determinantes Eficiencia técnica - Malmquist')
ax.set_xticks(x_pos)
ax.set_xticklabels(categories, rotation=45, ha='right')

fig.tight_layout()

plt.show()