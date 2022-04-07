# AngleCAM

## Contents

[Introduction](#Introduction)
[Approach and evaluation](#approach-and-evaluation)
[Use AngleCam and how to contribute](#Use-AngleCam-and-how-to-contribute)


## Introduction

Leaf angles are relevant for various applications (e.g., modelling radiative transfer in plants, producitvity, or Earth systems). Measuring or tracking leaf angles through time is, hence, of vital importance for various disciplines. AngleCam is a deep learning-based method to predict leaf angle distributions from horizontal photographs. AngleCam can be applied on single photographs or time series. The underlying CNN models of AngleCam were trained with the Brinno TLC-200 Pro camera. The latter is low-cost and outdoor ready and enables to acquire time series up to several month. AngleCam was evaluated over various species, growth forms and for predicting time series over several months at 3-min intervals under field conditions (the manuscript describing the method is currently in review).

![diurnal](https://github.com/tejakattenborn/AngleCAM/blob/main/result_small_mod.gif)
*Animation of the AngleCam input and output. Note that the output (leaf angle distribution) is for simplicity integrated to average leaf angles. The animation shows that during a sunny day Tilia cordata tends to oscilate its leaf angles. The estimates show a relatively high variance, mostly caused by small changes in leaf orientation due to wind. Despite considerable variation in illumination conditions, the predictions show a relatively stable course during the day.*

## Approach and evaluation

AngleCam is based on Convolutional Neural Networks (at current stage with the EfficientNet backbone). We trained the models with several thousand reference samples that were generated from visual interpretation of invidiual image frames. For each image frame, we sampled 20 leaves, which were then converted to a leaf angle distribution (beta distribution). The CNN models were, hence, trained to predict a leaf angle distribution for each individual image. The model accuracy was estimated from independent holdouts. Additionally, we performed a independent validation using terrestrial laser scanning and the [TLSLeAF method by Atticus Stovall](https://github.com/aestovall/TLSLeAF).

![val](https://github.com/tejakattenborn/AngleCAM/blob/main/AngleCam_val.png)

## Use AngleCam and how to contribute

The trained models are available in ... and come with a minimalistic code. The code requires a running tensorflow and keras implementation in R (see).
The models can be either trained on individual image files in a input directory or from Brinno-AVI-files.
Please contact me if you find any bugs or have problems in getting the models running.

Current evaluations indicate the transferability of the approach across scence conditions, species and plant forms. However, we cannot eventually state how well the models perform on your datasets (which may be composed of very different species, leaf forms or cameras). Additional labels (reference data) may be helpful to tune the model towards you application scenario. A script for producing new reference data can be found here:
We would be very thankful if you would share these samples with us, so we can continuously improve the model performance and transferability. AngleCam is truly in a alpha-phase and it success also depends on your help. Contributors will ofcourse be involved in upcoming analysis and scientific output.
