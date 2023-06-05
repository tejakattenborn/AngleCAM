# AngleCAM

## Contents

[Introduction](#Introduction)

[Approach and evaluation](#approach-and-evaluation)

[Use AngleCam and how to contribute](#Use-AngleCam-and-how-to-contribute)


## Introduction

<img align="left" width="200" height="200" src="https://github.com/tejakattenborn/AngleCAM/blob/main/illustrations_small.png">
Vertical leaf surface angles are relevant for various applications (e.g., modelling radiative transfer in plants, producitvity, or Earth systems). Measuring or tracking leaf angles through time is, hence, of vital importance for various disciplines. AngleCam is a deep learning-based method to predict leaf angle distributions from horizontal photographs. AngleCam can be applied on single photographs or time series. AngleCam was evaluated over various species, growth forms and for predicting time series over several months at 3-min intervals under field conditions.
The underlying CNN models of AngleCam were trained with the Brinno TLC-200 Pro camera. The latter is low-cost and outdoor ready and enables to acquire time series up to several month. AngleCam may also be applicable to other cameras as long as their properties are comparable (Field of view, etc.).<br/>

<br/>

![diurnal](https://github.com/tejakattenborn/AngleCAM/blob/main/result_small_mod.gif)

*Animation of the AngleCam input (image frames) and output (leaf angle estimates) for a Acer pseudoplatanus crown. Note that here the output (leaf angle distribution) is for simplicity integrated to average leaf angles. The animation shows that during a sunny day Tilia cordata tends to oscilate its leaf angles. The estimates show a relatively high variance, mostly caused by small changes in leaf orientation due to wind. Despite considerable variation in illumination conditions, the predictions show a relatively stable course during the day.*

## Approach and evaluation

AngleCam is based on Convolutional Neural Networks (at current stage with TensorFlow and the EfficientNet backbone). We trained the networks with several thousands reference samples that were generated from visual interpretation of invidiual image frames. For each image frame, we sampled 20 leaves, which were then converted to a leaf angle distribution (beta distribution). The CNN models were, hence, trained to predict a leaf angle distribution for each individual image. The model accuracy was estimated from independent holdouts. Additionally, we performed a independent validation using terrestrial laser scanning and the [TLSLeAF method by Atticus Stovall](https://github.com/aestovall/TLSLeAF).

![val](https://github.com/tejakattenborn/AngleCAM/blob/main/AngleCam_val.png)

*Model evaluation based on training data, test data and terrestrial laser scanning. A manuscript describing the method and its evaluation is currently in review.*

![tls validation](https://github.com/tejakattenborn/AngleCAM/blob/main/tlsleaf_anglecam_comparison.png) 

*Comparison of AngleCam and TLSLeAF for predicting leaf angle distributions.*


## Use AngleCam and how to contribute

* R-Scripts for running AngleCam can be found in [code_run_AngleCam](https://github.com/tejakattenborn/AngleCAM/tree/main/code_run_AngleCam).

* The mandatory model object (hdf5) and example data can be downloaded from [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.8007723.svg)](https://doi.org/10.5281/zenodo.8007723)

* Further details on the method and its evaluation are published (2022) in Methods in Ecology and Evolution [![DOI](https://img.shields.io/badge/DOI-10.1111%252F2041----210X.13968-red)](https://doi.org/10.1111/2041-210X.13968)



The code requires a running TensorFlow installation (see script for some help ressources). 
Please contact me if you find any bugs or have problems getting the models running:
https://rsc4earth.de/authors/tkattenborn/     https://twitter.com/TejaKattenborn

Current evaluations indicate the transferability of the approach across scence conditions, species and plant forms. However, we cannot eventually state how well the models perform on your datasets (which may be composed of very different species, leaf forms or cameras). Additional labels (reference data) may be helpful to tune the model towards you application scenario. A [R-script](https://github.com/tejakattenborn/AngleCAM/blob/main/code_manuscript/01_labelling_leaf_angles.R) for producing new reference data is included in this repository. We would be very thankful if you would share these samples with us, so we can continuously improve the model performance and transferability. In return, we provide you a model object that was optimized for your data. AngleCam is truly in a alpha-phase and it success also depends on your help. Contributors will ofcourse be involved in upcoming analysis and scientific output.
