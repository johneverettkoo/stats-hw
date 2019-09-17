# Programming Project 1

## Naive Bayes Classifier for Sentiment Analysis

Author: John Koo

URL: https://github.com/johneverettkoo/stats-hw/tree/master/csci-b555/homework/pp1

To run: `python main.py [parent_directory]` in the terminal

The file structure is assumed to be:

```
parent_directory
    ↳ README.md
    ↳ pp1_functions.py
    ↳ main.py
    ↳ sentiment labelled sentences
        ↳ amazon_cells_labelled.txt
        ↳ imdb_labelled.txt
        ↳ yelp_labelled.txt
    ↳ plots
```

The subdirectory `plots` is generated when `main.py` is run. The rest are 
assumed to be preexisting.

Some notes:

* `pp1_functions.py` contains all the necessary functions 
* The data are contained in a subdirectory `sentiment labelled sentences`
* Plots are generated and saved to the subdirectory `plots`
* *k=10* is chosen in *k*-fold cross-validation for all experiments
* If the log likelihoods are -inf for both classes, then the predicted class is 
chosen randomly based on the proportion of classes in the training set
* The write-up is in `write-up.ipynb`. This was converted into a .pdf for 
submission.