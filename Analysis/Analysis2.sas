proc print data=house;
run;
/*Creating Features*/
data house2;
set house;
InteriorSF=GrLivArea+TotalBsmtSF;
LISF=log(InteriorSF);
LSP=log(SalePrice);
lYearBuilt=log(YearBuilt);
lYrSold=log(YrSold);
YrDiff=YrSold-YearBuilt;
lYrDiff=log(YrDiff);
run;
proc print data=house2;
run;
/*Examining Relationships*/
proc sgscatter data=house2;
matrix LSP LISF OverallCond OverallQual;
run;
/*Model Selection*/
proc glmselect data=house2 plots=all seed=;
class MSSubClass(ref='20') Neighborhood OverallCond(ref='1') OverallQual MSZoning;
partition fraction(test = .2);
model LSP=LISF LYearBuilt lYrSold MSSubClass Neighborhood OverallCond OverallQual MSZoning;
run;
/*Examining Relationships*/
proc sgscatter data=house2;
matrix LSP LISF lYearBuilt lYrSold YrDiff;
run;
/*Model Selection and Cross-Validation*/
proc glmselect data=house2 plots=all;
class MSSubClass(ref='20') Neighborhood OverallCond(ref='1') OverallQual MSZoning;
partition fraction(test = .2);
model LSP=LISF lYearBuilt Neighborhood OverallCond OverallQual MSZoning;
run;

/*Forward, Backward, and Stepwise Selection and Cross-Validation*/
proc glmselect data=house2 plots=all;
class MSSubClass(ref='20') Neighborhood OverallCond(ref='1') OverallQual MSZoning;
partition fraction(test = .2);
model LSP=LISF LYearBuilt lYrSold YrDiff MSSubClass Neighborhood OverallCond OverallQual MSZoning/selection=forward;
run;
proc glmselect data=house2 plots=all;
class MSSubClass(ref='20') Neighborhood OverallCond(ref='1') OverallQual MSZoning;
partition fraction(test = .2);
model LSP=LISF LYearBuilt lYrSold YrDiff MSSubClass Neighborhood OverallCond OverallQual MSZoning/selection=backward;
run;
proc glmselect data=house2 plots=all;
class MSSubClass(ref='20') Neighborhood OverallCond(ref='1') OverallQual MSZoning;
partition fraction(test = .2);
model LSP=LISF LYearBuilt lYrSold YrDiff MSSubClass Neighborhood OverallCond OverallQual MSZoning/selection=stepwise;
run;

/*Running the model*/
proc glm data=house2 plots=all;
class Neighborhood OverallCond OverallQual MSZoning;
model LSP=LISF YrDiff Neighborhood OverallCond OverallQual MSZoning/solution clparm;
run;
