# Diabetic-Patients-Readmission

In this project, I have analyzed the relationship between diabetes patient readmission and the various patient attributes.
The dataset was obtained from one of our professors at Northern Illinois University who is working on Statistical Modeling.

Performed initial data exploration and data cleansing using Excel (Pivot tables, Slicer and other advanced Excel functions).
The cleansed data file can be found in this repository named med_data.csv

### Project_V4.R 

Holds the R code used to perform data analysis, data type conversions, data transformations etc.

Splitted the data into training and test sets and declared the reference value for categorical variables.

Using the training set, a sample was picked and logistic regression model was implemented.

Using the test set, predictions were made. Computed the accuracy of the model and built ROC curve.
