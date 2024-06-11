import numpy as np
import matplotlib.pyplot as plt
np.random.seed(3)

def matrixplot():
    m = np.random.choice([0,1], size=(5,8))
    colnames = list(range(1,8)) + ['...']
    rownames = [f'LLM-{i}' for i in range(1,5)] + ['⋮']
    fig, ax = plt.subplots()
    ax.matshow(m, cmap='Greys')
    ax.set_xticklabels([''] + colnames, fontweight='bold')
    ax.set_yticklabels([''] + rownames, fontweight='bold')
    # add lines to separate rows and columns
    for i in range(1, m.shape[0]):
        ax.axhline(i-0.5, color='white', linewidth=0.5)
    for i in range(1, m.shape[1]):
        ax.axvline(i-0.5, color='white', linewidth=0.5)
    # remove bottom ticks, and set font size
    ax.tick_params(bottom=False, axis='both', which='major', labelsize=18)
    # larger margins
    plt.subplots_adjust(left=0.2, right=0.8, top=0.8, bottom=0.2)
    # save the plot
    plt.savefig('p-matrix.png', dpi=300)

def scoretable(m, suffix='', right=False):
    fig, ax = plt.subplots(figsize=(10, 6))
    rownames=['ARC', 'GSM8K', 'HellaSwag', '⋮', 'Mean']
    colnames = [f'M{i}' for i in range(1,8)] + ['...']
    # plot matrix with numbers instead of color
    ax.matshow(m, cmap='Greys')
    ax.set_xticklabels([''] + colnames, fontweight='bold')
    ax.set_yticklabels([''] + rownames, fontweight='bold')
    # add lines to separate rows and columns
    for i in range(1, m.shape[0]):
        ax.axhline(i-0.5, color='white', linewidth=0.5)
    for i in range(1, m.shape[1]):
        ax.axvline(i-0.5, color='white', linewidth=0.5)
    # add extra thick line for penultimate row
    ax.axhline(m.shape[0]-1-0.5, color='white', linewidth=4)
    # remove bottom ticks, 
    ax.tick_params(bottom=False, axis='both', which='major')
    # set font size for rows only
    ax.tick_params(axis='both', which='major', labelsize=18)
    # larger margins
    plt.subplots_adjust(left=0.2, right=0.8, top=0.8, bottom=0.2)
    # optionally move ylabels and ticks to the right
    if right:
        ax.yaxis.tick_right()
        ax.yaxis.set_label_position('right')
    # save the plot
    plt.savefig(f'p-scoretable-{suffix}.png', dpi=300)

def tableplot():
    corr = np.random.uniform(0.5, 0.85, size=(4, 4))
    m_orig = np.random.multivariate_normal(0.6 * np.ones(4), corr, size=8).T
    m = 1 / (1 + np.exp(-m_orig))
    m1 = np.vstack([m, np.mean(m, axis=0).reshape(1, -1)])
    noise = np.random.uniform(-0.2, 0.2, size=(4, 8))
    m2 = np.vstack([m_orig + noise, np.mean(m_orig + noise, axis=0).reshape(1, -1)])
    scoretable(m1, 'original')
    scoretable(m2, 'noisy', right=True)

def parameterplot(ax):
    # slightly correlated point cloud
    x = np.random.normal(size=100)
    y = x + np.random.normal(size=100)
    ax.scatter(x, y, color='black', s=100)
    # add labels
    ax.set_xlabel(r'$\delta$', fontsize=30)
    ax.set_ylabel(r'$\alpha$', fontsize=30)
    # more space between axes and labels
    ax.yaxis.labelpad = 20
    ax.xaxis.labelpad = 20
    # remove ticks
    ax.set_xticks([])
    ax.set_yticks([])
    # larger margins
    # plt.subplots_adjust(left=0.2, right=0.8, top=0.8, bottom=0.2)
    return ax

def infoplot(ax, n=7):
    x = np.linspace(-3, 3, 100)
    y1 = np.exp(-x**2)
    y2 = np.exp(-(x-1)**2) * 0.5
    y3 = np.exp(-(x+1)**2/2) * 0.8
    y4 = np.exp(-x**2/4) * 0.1
    y5 = np.exp(-(x-1.3)**2/2) * 0.15
    y6 = np.exp(-(x+1.8)**2/2.5) * 0.13
    y7 = np.exp(-(x+1.1)**2/2.7) * 0.11

    for y in [y1, y2, y3, y4, y5, y6, y7][:n]:
        ax.plot(x, y, color='black', linestyle='dashed')
    ax.set_xticks([])
    ax.set_yticks([])
    ax.xaxis.labelpad = 20
    ax.yaxis.labelpad = 20
    ax.set_xlabel(r'$\vartheta$', fontsize=50)
    ax.set_ylabel(r'$I(\vartheta)$', fontsize=50)
    # make the box around the plot thicker
    for spine in ax.spines.values():
        spine.set_linewidth(6)
    return ax

def doubleinfo():
    fig, axs = plt.subplots(2, 1, figsize=(10, 18))
    infoplot(axs[0])
    infoplot(axs[1], n=3)
    plt.subplots_adjust(hspace=0.6)
    plt.savefig('p-info.png', dpi=300)


def main():
    doubleinfo()
    tableplot()


if __name__ == '__main__':
    main()
