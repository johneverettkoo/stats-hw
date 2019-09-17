import numpy as np
import re
from collections import Counter
import math


def open_reviews(filepath):
    """read reviews and return output as list of reviews and list of ratings"""

    # split each line into the review text and review rating
    # also strip punctuation and cast to lowercase
    reviews = [re.split('. *\t', line.strip('\n')) for line in
               open(filepath)]
    review_text = [re.sub(r'[^\w\s]', '', review[0].lower())
                   for review in reviews]
    review_rating = [int(review[1]) for review in reviews]

    return review_text, review_rating


def construct_dictionary(reviews):
    """ construct a dictionary of words in a list of reviews"""

    # split each review into individual words
    reviews = [review.split() for review in reviews]

    # flatten the list of list of words
    dictionary = [word for review in reviews for word in review]

    # drop duplicates
    dictionary = set(dictionary)

    return dictionary


def construct_term_freq_dict(reviews, relative=False):
    """construct a dictionary of term frequencies"""

    # split each review into individual words
    reviews = [review.split() for review in reviews]

    # flatten list of reviews
    words = [word for review in reviews for word in review]

    # construct term frequency dictionary
    term_freq_dict = dict(Counter(words))

    # normalize if desired
    if relative:
        n = len(words)
        term_freq_dict = {k: v / n for k, v in term_freq_dict.items()}

    return term_freq_dict


def construct_sentiment_term_freq_dicts(reviews, ratings, relative=False):
    """construct dictionaries of term frequencies by sentiment"""

    # separate reviews to positive and negative
    positive_reviews = [review for review, rating in zip(reviews, ratings)
                        if rating == 1]
    negative_reviews = [review for review, rating in zip(reviews, ratings)
                        if rating == 0]

    # construct term frequency dictionaries
    positive_term_freq = construct_term_freq_dict(positive_reviews, relative)
    negative_term_freq = construct_term_freq_dict(negative_reviews, relative)

    return positive_term_freq, negative_term_freq


def compute_loglik(review, term_freq, prior_prob=.5,
                   method='maximum likelihood', alpha=1.):
    """compute the log likelihood for one review"""

    # set alpha=1 if not using prior
    if method == 'maximum likelihood' or method == 'ml':
        alpha = 0
    elif method != 'maximum a posteriori' and method != 'map':
        raise(f'{method} is not a valid method')

    # how many words are there in total
    n = sum(term_freq.values()) + alpha * len(term_freq)

    # remove punctuation and cast to lowercase
    review = re.sub(r'[^\w\s]', '', review.lower())

    # split review into words
    review = review.split()

    # compute log likelihood one word at a time
    loglik = np.log(prior_prob)
    for word in review:
        if word in term_freq:
            loglik += np.log((term_freq[word] + alpha))
        else:
            loglik += np.log(alpha)
            n += 1
    loglik -= np.log(n) * len(review)

    return loglik


def predict_sentiment(review, positive_term_freq, negative_term_freq,
                      positive_weight=.5,
                      method='maximum likelihood', alpha=1.):
    """predict the sentiment of a word by comparing log likelihoods"""

    # compute the log likelihood for each class
    pos_loglik = compute_loglik(review, positive_term_freq, positive_weight,
                                method, alpha)
    neg_loglik = compute_loglik(review, negative_term_freq, 1 - positive_weight,
                                method, alpha)

    # predict the sentiment by comparing log likelihoods
    if math.isinf(pos_loglik) and math.isinf(neg_loglik):
        # if they are both -inf then pick one randomly
        p = np.random.uniform()
        if p > positive_weight:
            return 1
        else:
            return 0
    else:
        if pos_loglik > neg_loglik:
            return 1
        else:
            return 0


def create_predictor_fn(reviews, ratings,
                        method='maximum likelihood', alpha=1.):
    """returns a function that predicts the sentiment of a new review"""

    # proportion of positive/negative reviews
    prop_pos = np.mean(ratings)

    # construct term frequency dictionaries
    positive_term_freq, negative_term_freq = \
        construct_sentiment_term_freq_dicts(reviews, ratings)

    def predictor_fn(review):
        return predict_sentiment(review,
                                 positive_term_freq, negative_term_freq,
                                 prop_pos,
                                 method, alpha)

    return predictor_fn



def assign_fold_indices(reviews, ratings, k=10):
    """assign fold 1..k to each review"""

    fold_indices = list(range(1, k + 1))

    # separate reviews to positive and negative
    positive_reviews = [review for review, rating in zip(reviews, ratings)
                        if rating == 1]
    negative_reviews = [review for review, rating in zip(reviews, ratings)
                        if rating == 0]

    # count number of reviews for each class
    n_pos = len(positive_reviews)
    n_neg = len(negative_reviews)

    # assign folds 1..k and shuffle
    pos_folds = np.repeat(fold_indices, math.ceil(n_pos / k))
    neg_folds = np.repeat(fold_indices, math.ceil(n_neg / k))
    np.random.shuffle(pos_folds)
    np.random.shuffle(neg_folds)
    pos_folds = pos_folds[range(n_pos)]
    neg_folds = neg_folds[range(n_neg)]

    # recombine
    reviews = positive_reviews + negative_reviews
    ratings = np.concatenate([np.repeat(1, n_pos),
                              np.repeat(0, n_neg)])
    folds = np.concatenate([pos_folds, neg_folds])

    return reviews, ratings, folds

def kfold_cv(reviews, ratings,
             proportion=1.,
             k=10,
             method='maximum likelihood', alpha=1.):
    """return results of k-fold cross validation"""

    # get fold assignments
    reviews, ratings, folds = \
        assign_fold_indices(reviews, ratings, k)

    # preallocate memory for accuracies
    accuracies = np.repeat(np.nan, k)

    for i in range(1, k + 1):
        # split the data into train/val
        train_reviews = [review for review, fold in zip(reviews, folds)
                         if fold != i]
        train_ratings = [rating for rating, fold in zip(ratings, folds)
                         if fold != i]
        val_reviews = [review for review, fold in zip(reviews, folds)
                       if fold == i]
        val_ratings = [rating for rating, fold in zip(ratings, folds)
                       if fold == i]

        # subsample the training data
        if proportion < 1.:
            train_ind = np.arange(len(train_reviews))
            np.random.shuffle(train_ind)
            index_max = int(len(train_reviews) * proportion)
            train_ind = train_ind[0:index_max]
            train_reviews = [train_reviews[i] for i in train_ind]
            train_ratings = [train_ratings[i] for i in train_ind]

        # fit model and construct prediction function
        model = create_predictor_fn(train_reviews, train_ratings, method, alpha)

        # predict on validation set
        pred_ratings = [model(review) for review in val_reviews]

        # compute accuracy
        accuracies[i - 1] = np.mean([val_rating == pred_rating
                                     for val_rating, pred_rating
                                     in zip(val_ratings, pred_ratings)])

    return accuracies


def experiment(path,
               m=0., k=10, learning_curve_step=.1,
               verbose=True,
               plot_path=None):
    """
    function to run the experiments described in assignment
    testing a range of smoothing parameters
    this only does one dataset, specified by path
    """

    # get the data
    reviews, ratings = open_reviews(os.path.join(path))

    # initialize list of accuracies
    accuracies = []

    # for each subset proportion, try k-fold CV and store the accuracy metrics
    proportions = np.arange(learning_curve_step, 1 + learning_curve_step,
                            learning_curve_step)
    for proportion in proportions:
        accuracies.append(kfold_cv(reviews, ratings,
                                   proportion,
                                   k=k,
                                   method='map', alpha=m))

    acc_mean = [np.mean(accuracy) for accuracy in accuracies]
    acc_std = [np.std(accuracy) for accuracy in accuracies]

    # if verbose, print out the means and standard deviations
    if verbose:
        print('subsample', 'mean acc, std acc')
        print(np.stack([proportions,
                        acc_mean,
                        acc_std]).T)

    # plot
    if plot_path is not None:
        flattened_accs = [accuracy for sub_accs in accuracies
                          for accuracy in sub_accs]
        flattened_props = np.repeat(proportions, k)
        plt.scatter(x=flattened_props, y=flattened_accs)
        # plt.plot(proportions,
        #          [np.mean(accuracy) for accuracy in accuracies])
        plt.errorbar(proportions, acc_mean,
                     yerr=[2 * s for s in acc_std],
                     c='red')
        plt.xlabel('subsample proportion')
        plt.ylabel('accuracy')
        plt.show()
        plt.savefig(plot_path)

    return accuracies

def learning_curve(reviews, ratings, alpha=0., k=10, learning_curve_step=.1):
    """compute accuracies from k-fold CV with subsampling"""

    # initialize list of accuracies
    accuracies = []

    # for each subset proportion, try k-fold CV and store accuracies
    proportions = np.arange(learning_curve_step, 1 + learning_curve_step,
                            learning_curve_step)
    for proportion in proportions:
        accuracies.append(kfold_cv(reviews, ratings,
                                   proportion,
                                   k=k,
                                   method='map', alpha=alpha))

    return accuracies