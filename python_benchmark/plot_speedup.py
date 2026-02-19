import matplotlib.pyplot as plt
import numpy as np

# Data from benchmark results
record_sizes = [1, 5, 10, 15, 20]
speedups = [21.67, 23.91, 46.66, 58.50, 77.26]
errors = [0.98, 0.82, 1.99, 2.95, 4.78]

# Set style
plt.style.use('seaborn-v0_8-darkgrid')
fig, ax = plt.subplots(figsize=(12, 7), facecolor='white')

# Create gradient background
ax.set_facecolor('#f8f9fa')

# Main plot with enhanced styling
line = ax.errorbar(record_sizes, speedups, yerr=errors, 
                    marker='o', markersize=12, 
                    capsize=6, capthick=2.5, 
                    linewidth=3, 
                    color='#0066cc', 
                    ecolor='#cc0066',
                    elinewidth=2,
                    markerfacecolor='#0066cc',
                    markeredgecolor='white',
                    markeredgewidth=2,
                    label='arity-raising speedup',
                    zorder=3)

# Add filled area under the curve
ax.fill_between(record_sizes, 0, speedups, alpha=0.15, color='#0066cc', zorder=1)

# Customize the plot with better typography
ax.set_xlabel('Record Size', fontsize=15, fontweight='bold', color='#2c3e50', labelpad=10)
ax.set_ylabel('Speedup Factor (×)', fontsize=15, fontweight='bold', color='#2c3e50', labelpad=10)
ax.set_title('Performance Improvement: konradbausch/arity-raising vs main', 
             fontsize=17, fontweight='bold', color='#1a252f', pad=20)

# Enhanced grid
ax.grid(True, alpha=0.3, linestyle='-', linewidth=0.8, color='#bdc3c7', zorder=0)
ax.set_axisbelow(True)

# Set axis limits and ticks
ax.set_xlim(-1, 22)
ax.set_ylim(0, max(speedups) + max(errors) + 10)
ax.set_xticks([0, 5, 10, 15, 20])
ax.tick_params(labelsize=12, colors='#34495e', width=1.5, length=6)

# Add value labels with better styling
for size, speedup, error in zip(record_sizes, speedups, errors):
    ax.annotate(f'{speedup:.1f}×', 
                xy=(size, speedup), 
                xytext=(0, 12), 
                textcoords='offset points',
                ha='center',
                fontsize=11,
                fontweight='bold',
                color='#2c3e50',
                bbox=dict(boxstyle='round,pad=0.5', 
                         facecolor='white', 
                         edgecolor='#0066cc',
                         linewidth=2,
                         alpha=0.95),
                zorder=4)

# Enhanced legend
legend = ax.legend(fontsize=12, frameon=True, shadow=True, 
                   fancybox=True, framealpha=0.95, 
                   edgecolor='#34495e', loc='upper left')
legend.get_frame().set_facecolor('white')

# Add subtle border
for spine in ax.spines.values():
    spine.set_edgecolor('#95a5a6')
    spine.set_linewidth(1.5)

plt.tight_layout()
plt.savefig('speedup_comparison.png', dpi=300, bbox_inches='tight', facecolor='white')
print("✓ Plot saved as 'speedup_comparison.png'")
plt.show()
