# higgsboson
Package for SM1/SC1 group project. Used to analyse the simulated data from the ATLAS experiment at CERN to optimise the analysis of the Higgs Boson.


## Installation
This package can be installed using devtools and the command
```{r}
devtools::install_github("hsansford1/higgsboson")
```

## Introduction 
The Atlas experiment at CERN provided simulated data that was used to optimise the analysis of the Higgs boson. In the Large Hadron Collider (LHC), proton bunches are accelerated in both directions on a circular trajectory. This results in some of the protons colliding as the bunches cross the ATLAS detector (called an \textit{event}), which produces hundreds of millions of proton-proton collisions per second. The particles resulting from each event are detected by sensors and, from this raw data, certain real-valued features are estimated.

Most of the uninteresting events (call the \textit{background}) are discarded using a real-time multi-stage cascade classfier. However, many of the remaining events represent known processes (also called \textit{background}). Our aim is to find the region of the feature space in which there is a significant excess of events (called \textit{signal}) compared to what known background processes can explain.


## Data
To load the data, use the function `data` with either `training` or `test` as input, i.e.
```{r}
data(training)
data(test)
```
Both datasets are very large with 30 feature columns, a weight column and a label column. The training data has 250,000 rows and the test data has 550,000 rows.  
