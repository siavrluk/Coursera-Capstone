# Coursera-Capstone

This app is the [Data Science
Capstone]( "https://www.coursera.org/learn/data-science-project/home/welcome")
project for the [Data Science
Specialization]( "https://www.coursera.org/specializations/data-science-statistics-machine-learning")
offered by John Hopkins University.

The ultimate goal of the project is to create a Shiny App that suggest
next word of a phrase entered by the user. The data used for building
the model is provided by SwiftKey and can be found
[here]( "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip").

## Model

The app is built by using an [**n-gram
model**]( "https://en.wikipedia.org/wiki/N-gram"). This is a
probabilistic language model for predicting the next item in a sequence
of words conditioned on the previous *n* words. Sequences of 1, 2, 3, or
4 words, i.e. **uni-**, **bi-**, **tri**-, and **quadrigrams**, resp,
have been used. E.g., if a user enters 3 words, a fourth word is
returned based on the frequencies of all quadrigrams. If a user enters
more than 3 words, only the last 3 are considered for the prediction.

## Additional resources

-   The R for data preprocessing and modeling is available at
-   A short deck about the app can be found at
