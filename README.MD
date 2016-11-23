# README
## Getting and Cleaning Data Course Project

This repository contains the course project for the Getting and Cleaning Data course. It contains:

- An R script, `run_analysis.R`.
- Means for each measurement in the tidied UCI HAR dataset, `cleaningdata_means.txt`
- A codebook, `codebook.MD`. This describes the variables in the dataset produced by `run_alanysis.R`.

### run_analysis.R
This script takes the files from the [UCI Human Activity Recognition dataset](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones), cleans the data, and produces a tidy data set with clearly labeled subjects, activity types, and measurements.

It also produces a text file containing the means for each measurement by subject and activity.