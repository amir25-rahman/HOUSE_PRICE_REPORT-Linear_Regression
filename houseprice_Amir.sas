proc import datafile="HousePrices_Canada.csv" out= House_Price replace;
delimiter=',';
getnames=yes;
run;

TITLE "Frequency table";
proc freq 
data = House_Price;

run; 

data House_Price;
set House_Price;

num_bedroom_1 = (bedrooms=1);
num_bedroom_2 = (bedrooms=2);
num_bedroom_3 = (bedrooms=3);
num_bedroom_4 = (bedrooms=4);
num_bedroom_5 = (bedrooms=5);
num_bedroom_6 = (bedrooms=6);

num_bathroom_1 = (bathrooms=1);
num_bathroom_2 = (bathrooms=2);
num_bathroom_3 = (bathrooms=3);
num_bathroom_4 = (bathrooms=4);

num_stories_1 = (stories=1);
num_stories_2 = (stories=2);
num_stories_3 = (stories=3);
num_stories_4 = (stories=4);

num_driveway = (driveway="yes");
num_recreation = (recreation="yes"); 
num_fullbase = (fullbase="yes"); 
num_gasheat = (gasheat="ye"); 
num_aircon = (aircon="yes");  

num_garage_0 = (garage=0);
num_garage_1 = (garage=1);
num_garage_2 = (garage=2);
num_garage_3 = (garage=3);

num_prefer = (prefer="ye");

*interaction variable;
sq_height = lotsize*stories*num_fullbase;
sq_loc = lotsize*num_prefer;
run;

*Printing the dataset;
TITLE "Dataset";
PROC PRINT data= House_Price;
RUN;

*Histogram;
TITLE "Price House Histogram";
PROC UNIVARIATE normal;
VAR price;
HISTOGRAM / normal (mu = est sigma = est);
RUN;

*Scatter Table;
TITLE "Scatter Plot";
PROC gplot data= House_Price;
plot price * (lotsize bedrooms bathrooms stories driveway recreation fullbase gasheat aircon garage prefer sq_height sq_loc);
run;

*Correlation Table;
TITLE "Pearson";
PROC CORR;
VAR price lotsize bedrooms stories num_driveway num_recreation num_fullbase 
num_gasheat num_aircon garage num_prefer;
RUN;

*Transform;
DATA House_Price;
SET House_Price;
ln_price =log(price);

*Check new variable ln_price;
PROC PRINT data=House_Price;
RUN;

*log Histogram;
TITLE "LOG Price Transform";
PROC UNIVARIATE normal;
VAR ln_price;
HISTOGRAM / normal (mu = est sigma = est);
RUN;

*Full Model;
TITLE "Full Model";
PROC REG;
MODEL ln_price = lotsize num_bedroom_1 num_bedroom_2 num_bedroom_3 num_bedroom_4 num_bedroom_5 num_bedroom_6 
num_bathroom_1 num_bathroom_2 num_bathroom_3 num_bathroom_4 num_stories_1 num_stories_2 num_stories_3 num_stories_4 
num_driveway num_recreation num_fullbase num_gasheat num_aircon num_garage_0 num_garage_1 num_garage_2 
num_garage_3 num_prefer sq_height sq_loc;
PLOT residual.*(lotsize);
plot student.*predicted.;
plot npp.*student.;
run;

*multicollinearity Issue VIF > 10?;
TITLE "Full model multicollinearity";
PROC REG;
MODEL ln_price = lotsize num_bedroom_1 num_bedroom_2 num_bedroom_3 num_bedroom_4 num_bedroom_5 num_bedroom_6 
num_bathroom_1 num_bathroom_2 num_bathroom_3 num_bathroom_4 num_stories_1 num_stories_2 num_stories_3 num_stories_4 
num_driveway num_recreation num_fullbase num_gasheat num_aircon num_garage_0 num_garage_1 num_garage_2 
num_garage_3 num_prefer sq_height sq_loc/vif tol;
run;

*Full Model Outliers;
TITLE "Full Model Outliers";
PROC REG;
MODEL ln_price = lotsize num_bedroom_1 num_bedroom_2 num_bedroom_3 num_bedroom_4 num_bedroom_5 num_bedroom_6 
num_bathroom_1 num_bathroom_2 num_bathroom_3 num_bathroom_4 num_stories_1 num_stories_2 num_stories_3 num_stories_4 
num_driveway num_recreation num_fullbase num_gasheat num_aircon num_garage_0 num_garage_1 num_garage_2 
num_garage_3 num_prefer sq_height sq_loc/ stb influence r;
run;


TITLE "Split Data 80/20";
PROC surveyselect data = House_Price out = XV_ALL seed = 764895
samprate = 0.8 outall;
RUN;

TITLE "Train Data";
data XV_ALL;
set XV_ALL;
if selected then new_y = ln_price;
run;

TITLE "Test Data";
data XV_ALL;
set XV_ALL;
if not selected then new_y_test = ln_price;
run;

TITLE "Print both Test/Train Data";
proc print data = XV_ALL;
run;




*Backward;
TITLE "Backward Method";
proc reg data = XV_ALL;
MODEL new_y = lotsize num_bedroom_1 num_bedroom_2 num_bedroom_3 num_bedroom_4 num_bedroom_5 num_bedroom_6 
num_bathroom_1 num_bathroom_2 num_bathroom_3 num_bathroom_4 num_stories_1 num_stories_2 num_stories_3 num_stories_4 
num_driveway num_recreation num_fullbase num_gasheat num_aircon num_garage_0 num_garage_1 num_garage_2 
num_garage_3 num_prefer sq_height sq_loc/selection = backward;
output out=outm1(where=(new_y=.)) p=yhat;
run;

proc print data = outm1;
run;

*Backward RMSE;
TITLE "Backward Method with RMSE ADJ Adj-Sq";
proc reg data = XV_ALL;
MODEL new_y = lotsize num_bedroom_2  
num_bathroom_1 num_bathroom_2 num_stories_1 num_stories_2 
num_driveway num_recreation num_fullbase num_gasheat num_aircon num_garage_1 num_garage_2 num_prefer;
run;

/* summarize the results of the cross-validations for model-1*/
title "Backward Difference between Observed and Predicted in Test Set";
data outm1_sum;
set outm1;
d= new_y_test - yhat; *d is the difference between observed and predicted values in test set;
absd=abs(d);
run;

/* computes predictive statistics: root mean square error (rmse)
and mean absolute error (mae)*/
proc summary data=outm1_sum;
var d absd;
output out=outm1_stats std(d)=rmse mean(absd)=mae ;
run;
proc print data=outm1_stats;
title 'Validation statistics for Model';
run;
*computes correlation of observed and predicted values in test set;
proc corr data=outm1;
var new_y_test yhat;
run;


*Stepwise;
TITLE "Stepwise Method";
proc reg data = XV_ALL;
MODEL new_y = lotsize num_bedroom_1 num_bedroom_2 num_bedroom_3 num_bedroom_4 num_bedroom_5 num_bedroom_6 
num_bathroom_1 num_bathroom_2 num_bathroom_3 num_bathroom_4 num_stories_1 num_stories_2 num_stories_3 num_stories_4 
num_driveway num_recreation num_fullbase num_gasheat num_aircon num_garage_0 num_garage_1 num_garage_2 
num_garage_3 num_prefer sq_height sq_loc/selection = stepwise;
output out=outm2(where=(new_y=.)) p=yhat;
run;

*Stepwise;
TITLE "Stepwise Final MODEL with RMSE ADJ Adj-Sq";
proc reg data = XV_ALL;
MODEL new_y = lotsize num_bedroom_2   
num_bathroom_1 num_bathroom_2 num_stories_1 num_stories_2 num_stories_3 num_stories_4 
num_driveway num_recreation num_fullbase num_gasheat num_aircon num_garage_0 num_garage_2 
num_prefer;
run;

proc print data = outm2;
run;

/* summarize the results of the cross-validations for model-1*/
title "Stepwise Difference between Observed and Predicted in Test Set";
data outm2_sum;
set outm2;
d= new_y_test - yhat; *d is the difference between observed and predicted values in test set;
absd=abs(d);
run;

/* computes predictive statistics: root mean square error (rmse)
and mean absolute error (mae)*/
proc summary data=outm2_sum;
var d absd;
output out=outm2_stats std(d)=rmse mean(absd)=mae ;
run;
proc print data=outm2_stats;
title 'Validation statistics for Model';
run;
*computes correlation of observed and predicted values in test set;
proc corr data=outm2;
var new_y_test yhat;
run;


TITLE "Forward Method";
proc reg data = XV_ALL;
MODEL new_y = lotsize num_bedroom_1 num_bedroom_2 num_bedroom_3 num_bedroom_4 num_bedroom_5 num_bedroom_6 
num_bathroom_1 num_bathroom_2 num_bathroom_3 num_bathroom_4 num_stories_1 num_stories_2 num_stories_3 num_stories_4 
num_driveway num_recreation num_fullbase num_gasheat num_aircon num_garage_0 num_garage_1 num_garage_2 
num_garage_3 num_prefer sq_height sq_loc/selection = forward;
output out=outm3(where=(new_y=.)) p=yhat;
run;

TITLE "Forward Method with RMSE ADJ Adj-Sq";
proc reg data = XV_ALL;
MODEL new_y = lotsize num_bedroom_1 num_bedroom_2 num_bedroom_4 num_bedroom_6 
num_bathroom_1 num_bathroom_2 num_bathroom_3 num_stories_2 num_stories_3 num_stories_4 
num_driveway num_recreation num_fullbase num_gasheat num_aircon num_garage_0 num_garage_1 num_garage_2 
num_prefer;
run;

proc print data = outm3;
run;

/* summarize the results of the cross-validations for model-1*/
title "Forward Difference between Observed and Predicted in Test Set";
data outm3_sum;
set outm3;
d= new_y_test - yhat; *d is the difference between observed and predicted values in test set;
absd=abs(d);
run;

/* computes predictive statistics: root mean square error (rmse)
and mean absolute error (mae)*/
proc summary data=outm3_sum;
var d absd;
output out=outm3_stats std(d)=rmse mean(absd)=mae ;
run;
proc print data=outm3_stats;
title 'Validation statistics for Model';
run;
*computes correlation of observed and predicted values in test set;
proc corr data=outm3;
var new_y_test yhat;
run;

TITLE "CP Method";
proc reg data = XV_ALL;
MODEL new_y = lotsize num_bedroom_1 num_bedroom_2 num_bedroom_3 num_bedroom_4 num_bedroom_5 num_bedroom_6 
num_bathroom_1 num_bathroom_2 num_bathroom_3 num_bathroom_4 num_stories_1 num_stories_2 num_stories_3 num_stories_4 
num_driveway num_recreation num_fullbase num_gasheat num_aircon num_garage_0 num_garage_1 num_garage_2 
num_garage_3 num_prefer sq_height sq_loc/selection = cp;
output out=outm4(where=(new_y=.)) p=yhat;
run;

TITLE "CP Method with RMSE ADJ Adj-Sq";
proc reg data = XV_ALL;
MODEL new_y = lotsize num_bedroom_2 num_bathroom_1 num_bathroom_2 num_stories_1 num_stories_2 num_driveway num_recreation 
num_fullbase num_gasheat num_aircon num_garage_0 num_garage_2 num_prefer;
run; 

proc print data = outm4;
run;

/* summarize the results of the cross-validations for model-1*/
title "CP Difference between Observed and Predicted in Test Set";
data outm4_sum;
set outm4;
d= new_y_test - yhat; *d is the difference between observed and predicted values in test set;
absd=abs(d);
run;

/* computes predictive statistics: root mean square error (rmse)
and mean absolute error (mae)*/
proc summary data=outm4_sum;
var d absd;
output out=outm4_stats std(d)=rmse mean(absd)=mae ;
run;
proc print data=outm4_stats;
title 'Validation statistics for Model';
run;
*computes correlation of observed and predicted values in test set;
proc corr data=outm4;
var new_y_test yhat;
run;

TITLE "ADJRSQ Method";
proc reg data = XV_ALL;
MODEL new_y = lotsize num_bedroom_1 num_bedroom_2 num_bedroom_3 num_bedroom_4 num_bedroom_5 num_bedroom_6 
num_bathroom_1 num_bathroom_2 num_bathroom_3 num_bathroom_4 num_stories_1 num_stories_2 num_stories_3 num_stories_4 
num_driveway num_recreation num_fullbase num_gasheat num_aircon num_garage_0 num_garage_1 num_garage_2 
num_garage_3 num_prefer sq_height sq_loc/selection = ADJRSQ;
output out=outm5(where=(new_y=.)) p=yhat;
run;

TITLE "ADJRSQ Method with RMSE ADJ Adj-Sq";
proc reg data = XV_ALL;
MODEL new_y = lotsize num_bedroom_2 num_bedroom_6 num_bathroom_1 num_bathroom_2 num_bathroom_3 num_stories_1 num_stories_2 num_driveway 
num_recreation num_fullbase num_gasheat num_aircon num_garage_1 num_garage_2 num_prefer;
run;

proc print data = outm5;
run;

/* summarize the results of the cross-validations for model-1*/
title "ADJRSQ Difference between Observed and Predicted in Test Set";
data outm5_sum;
set outm5;
d= new_y_test - yhat; *d is the difference between observed and predicted values in test set;
absd=abs(d);
run;

/* computes predictive statistics: root mean square error (rmse)
and mean absolute error (mae)*/
proc summary data=outm5_sum;
var d absd;
output out=outm5_stats std(d)=rmse mean(absd)=mae ;
run;
proc print data=outm5_stats;
title 'Validation statistics for Model';
run;
*computes correlation of observed and predicted values in test set;
proc corr data=outm5;
var new_y_test yhat;
run;




TITLE "FINAL MODEL WITHOUT multicollinearity(FORWARD)";
PROC REG data = XV_ALL;
MODEL new_y = lotsize num_bedroom_1 num_bedroom_2 num_bedroom_4 num_bedroom_6 
num_bathroom_1 num_bathroom_2 num_bathroom_3 num_stories_2 num_stories_3 num_stories_4 
num_driveway num_recreation num_fullbase num_gasheat num_aircon num_garage_0 num_garage_1 num_garage_2 
num_prefer;
run; 


TITLE "multicollinearity Final MODEL & Residuals";
PROC REG data = XV_ALL;
MODEL new_y = lotsize num_bedroom_1 num_bedroom_2 num_bedroom_4 num_bedroom_6 
num_bathroom_2 num_bathroom_3 num_stories_2 num_stories_3 num_stories_4 
num_driveway num_recreation num_fullbase num_gasheat num_aircon num_garage_1 num_garage_2 
num_prefer/vif tol stb;
PLOT student.*(lotsize);
PLOT student.*predicted.;
PLOT npp.*student.;
run;
*removel num_bathroom_1;
*remove num_garage_0;


*outliers;
TITLE "Outlier";
proc reg data = XV_ALL;
model new_y = lotsize num_bedroom_1 num_bedroom_2 num_bedroom_4 num_bedroom_6 
num_bathroom_2 num_bathroom_3 num_stories_2 num_stories_3 num_stories_4 
num_driveway num_recreation num_fullbase num_gasheat num_aircon num_garage_1 num_garage_2 
num_prefer/influence r;
run;



TITLE "Prediction";
data new_data;
input lotsize num_bedroom_1 num_bedroom_2 num_bedroom_4 num_bedroom_6 
num_bathroom_2 num_bathroom_3 num_stories_2 num_stories_3 num_stories_4 
num_driveway num_recreation num_fullbase num_gasheat num_aircon num_garage_1 num_garage_2 num_prefer;
datalines;
3200 1 0 0 0 1 0 0 0 1 0 1 1 1 1 1 0 1
4000 0 0 0 1 0 1 1 0 0 1 1 1 1 1 0 1 1  
;
run;
proc print;
run;


*Merge prediction;
TITLE "New Data with Merge";
data prediction;
set new_data XV_ALL;
run;
proc print data = prediction;
run;

*Run with data;
TITLE "Run with new data";
PROC REG data = prediction;
MODEL new_y = lotsize num_bedroom_1 num_bedroom_2 num_bedroom_4 num_bedroom_6 
num_bathroom_2 num_bathroom_3 num_stories_2 num_stories_3 num_stories_4 
num_driveway num_recreation num_fullbase num_gasheat num_aircon num_garage_1 num_garage_2 
num_prefer/clb cli clm;
OUTPUT OUT = pred;
run;
PROC PRINT data = pred;
RUN;




/* apply 5-fold crossvalidation with stepwise selection 
and 25% of data removed for testing; */

title "5-fold crossvalidation + 25% testing set Forward";
proc glmselect data=House_Price
	plots=(asePlot Criteria);
	*partition defines a test set (25% of data) to validate model on new data;
	partition fraction(test=0.25);
	* selection=stepwise uses stepwise selection method;
	* stop=cv: minimizes prediction residual sum of squares for variable selection;
	model ln_price = lotsize num_bedroom_1 num_bedroom_2 num_bedroom_3 num_bedroom_4 num_bedroom_5 num_bedroom_6 
num_bathroom_1 num_bathroom_2 num_bathroom_3 num_bathroom_4 num_stories_1 num_stories_2 num_stories_3 num_stories_4 
num_driveway num_recreation num_fullbase num_gasheat num_aircon num_garage_0 num_garage_1 num_garage_2 
num_garage_3 num_prefer sq_height sq_loc/selection=forward(stop=cv) cvMethod=split(5) cvDetails=all;
run;

title "5-fold crossvalidation + 25% testing set Stepwise";
proc glmselect data= House_Price
	plots=(asePlot Criteria);
	*partition defines a test set (25% of data) to validate model on new data;
	partition fraction(test=0.25);
	* selection=stepwise uses stepwise selection method;
	* stop=cv: minimizes prediction residual sum of squares for variable selection;
	model ln_price = lotsize num_bedroom_1 num_bedroom_2 num_bedroom_3 num_bedroom_4 num_bedroom_5 num_bedroom_6 
num_bathroom_1 num_bathroom_2 num_bathroom_3 num_bathroom_4 num_stories_1 num_stories_2 num_stories_3 num_stories_4 
num_driveway num_recreation num_fullbase num_gasheat num_aircon num_garage_0 num_garage_1 num_garage_2 
num_garage_3 num_prefer sq_height sq_loc/selection=stepwise(stop=cv) cvMethod=split(5) cvDetails=all;
run;

title "5-fold crossvalidation + 25% testing set Backward";
proc glmselect data=House_Price
	plots=(asePlot Criteria);
	*partition defines a test set (25% of data) to validate model on new data;
	partition fraction(test=0.25);
	* selection=stepwise uses stepwise selection method;
	* stop=cv: minimizes prediction residual sum of squares for variable selection;
	model ln_price = lotsize num_bedroom_1 num_bedroom_2 num_bedroom_3 num_bedroom_4 num_bedroom_5 num_bedroom_6 
num_bathroom_1 num_bathroom_2 num_bathroom_3 num_bathroom_4 num_stories_1 num_stories_2 num_stories_3 num_stories_4 
num_driveway num_recreation num_fullbase num_gasheat num_aircon num_garage_0 num_garage_1 num_garage_2 
num_garage_3 num_prefer sq_height sq_loc/selection=backward(stop=cv) cvMethod=split(5) cvDetails=all;
run;


