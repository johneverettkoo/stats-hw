import os
import sys
import numpy as np
from matplotlib import pyplot as plt

# --- CONSTANTS --- #

# number of folds
K = 10

# how to subsample for learning curves
LEARNING_CURVE_STEP = .1

# go to specified directory
PARENT_DIR = os.path.expanduser(str(sys.argv[1]))

# subdirectory where the data should live
DATA_SUBDIR = 'sentiment labelled sentences'

# data names
DATA_FILES = ['amazon_cells_labelled.txt',
              'imdb_labelled.txt',
              'yelp_labelled.txt']

# where the plots go
PLOT_SUBDIR = 'plots'

# --- PREPARE THE DIRECTORIES --- #

# try going to the directory
try:
    os.chdir(PARENT_DIR)
except:
    sys.exit(f'could not go to directory {PARENT_DIR}')

print(os.listdir())

# check if the data exist
try:
    os.chdir(DATA_SUBDIR)
    if all(file in os.listdir('.') for file in DATA_FILES):
        # go back to parent dir
        os.chdir('..')
    else:
        sys.exit(f'no data found in subdirectory {DATA_SUBDIR}')
except:
    sys.exit(f'could not find subdirectory {DATA_SUBDIR}')

# check if functions exist
try:
    from pp1_functions import open_reviews, learning_curve
except:
    sys.exit('could not find pp1_functions.py')

# if plot subdirectory doesn't exist, make it
if PLOT_SUBDIR not in os.listdir('.'):
    os.mkdir(PLOT_SUBDIR)
os.chdir(PLOT_SUBDIR)
if 'experiment-1' not in os.listdir('.'):
    os.mkdir('experiment-1')
if 'experiment-2' not in os.listdir('.'):
    os.mkdir('experiment-2')
os.chdir('..')

# --- EXPERIMENT 1 --- #

print('starting experiment 1 ...')

# for each dataset
for data_file in DATA_FILES:
    print(f'dataset: {data_file} ...')
    dataset_name = data_file.split('_')[0]

    # read the data
    os.chdir(os.path.join(PARENT_DIR, DATA_SUBDIR))
    reviews, ratings = open_reviews(data_file)

    # for each value of m specified by the assignment
    # compute the accuracy curves
    fig = plt.figure()
    for m in [0, 1]:
        # compute accuracies for each subsample proportion and fold
        accuracies = learning_curve(reviews, ratings,
                                    m,
                                    K, LEARNING_CURVE_STEP)

        # compute the mean and std accuracy for each subsample proportion
        mean_accs = [np.mean(accuracy) for accuracy in accuracies]
        std_accs = [np.std(accuracy) for accuracy in accuracies]

        # plot
        proportions = np.arange(LEARNING_CURVE_STEP, 1 + LEARNING_CURVE_STEP,
                                LEARNING_CURVE_STEP)
        plt.errorbar(proportions, mean_accs,
                     yerr=[2 * s for s in std_accs],
                     uplims=True, lolims=True,
                     label=f'm={m}')
    plt.legend(loc='upper right')
    plt.xlabel('subsample proportion')
    plt.ylabel('accuracy')
    plt.title(dataset_name)
    plt.savefig(os.path.join(PARENT_DIR, PLOT_SUBDIR, 'experiment-1',
                             dataset_name + '.png'))

# --- EXPERIMENT 2 --- #

print('starting experiment 2 ...')

# values of m specified by the assignment
m_values = [0, .1, .2, .3, .4, .5, .6, .7, .8, .9,
            1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
# colors for plotting
colormap = plt.cm.viridis(np.array(m_values))

# for each dataset
for data_file in DATA_FILES:
    print(f'dataset: {data_file} ...')
    dataset_name = data_file.split('_')[0]

    # read the data
    os.chdir(os.path.join(PARENT_DIR, DATA_SUBDIR))
    reviews, ratings = open_reviews(data_file)

    # for each value of m specified by the assignment
    # compute the accuracy curves
    fig = plt.figure(figsize=(16, 10))
    for i, m in enumerate(m_values):
        # compute accuracies for each subsample proportion and fold
        accuracies = learning_curve(reviews, ratings,
                                    m,
                                    K, LEARNING_CURVE_STEP)

        # compute the mean and std accuracy for each subsample proportion
        mean_accs = [np.mean(accuracy) for accuracy in accuracies]
        std_accs = [np.std(accuracy) for accuracy in accuracies]

        # plot
        proportions = np.arange(LEARNING_CURVE_STEP, 1 + LEARNING_CURVE_STEP,
                                LEARNING_CURVE_STEP)
        plt.errorbar(proportions, mean_accs,
                     yerr=[2 * s for s in std_accs],
                     uplims=True, lolims=True,
                     label=m,
                     c=colormap[i])
    plt.legend(loc='lower right')
    plt.xlabel('subsample proportion')
    plt.ylabel('accuracy')
    plt.title(dataset_name)
    plt.savefig(os.path.join(PARENT_DIR, PLOT_SUBDIR, 'experiment-2',
                             dataset_name + '.png'))