/*
         Author: SK JANE ALAM
         Institution: Delhi School of Economics, New Delhi
         Purpose; To demonstrate data cleaning, running regression and visualisation
		 and emthusiasm to work as a predoc.
*/
			
    view "D:\Practice stata\Log File.smcl"
   
// Opening log file to save and track back into operations that I did. 
    log using "D:\Practice stata\Log File2.smcl"
   
// Importing the csv file.
    import delimited "D:\Practice stata\New Variables.csv", clear
   
// Merging the new csv file with main dataset
    merge 1:1 uniqueid using "D:\Practice stata\Main Dataset.dta"
   
/* Appending the file of new observations, if the data is arriving daily or 
   weekly or monthly
*/

    append using "D:\Practice stata\New Observations.dta"
/* Summarising the Income, age and surveytime with deatil,
   where mean of weekly income is 2505 rupees with median of 2455 rupees 
   which depicts people are earning loewr than usual and their age is around 45
   years.
   the mean of the age is little annoying which is 334 we will work on it later.
*/
     sum income age surveytime, d
   
// Summarising the table in publishable format.
	estpost tabstat income age surveytime, s(mean sd median) c(s)
	eststo DescTot
	esttab DescTot using table1a.tex, replace booktabs align(rrr) nodep nonum noparen label ///
	cells("mean(fmt(2) label(Mean)) sd(fmt(2) label(SD)) p50(fmt(2) label(Median))") /// 
	coefl(income "Income" age "Age" survetime "Survey Time") title("Descriptive Statistics") ///
	mtitle("") note("Source Nishith sir's Test")
// The table is saved in latex format with codes in my github account.


//Checking uniqueness of our variables, where household Id has two variable
// extra....
	 duplicates report
	 duplicates report hhid
/// Dropping extra variables
	 duplicates drop

/* 
   To find missing values, I'm summarising it..., where only 7 variables have missing values
   which are own two wheeler, which household members were the victim of this
   crime, robbery of the two wheeler, Have you ever visited police station in past
   12 months, which household members were the victims of this type of crime,
   which household member were the victim of cycle theft, own 4 wheeler, wchich
   household member were the victim of 4 wheeler theft.
 */

     misstable summarize
     misstable tree, frequency
	
	
// installing missing plot  to analyse missing values garphically
    ssc install missingplot
    missingplot, labels mlabcolor(blue ..)
   
// setting directory to save the graphs
    cd "D:\Practice stata"
	
// saving the graph
    graph export "Missing_plot.pdf",replace
   
   
/*
   CLEANING the data; In the column of surveyor there are people with names like
   Benjamin and Jane, so replacing there name with unique id such as 1,2,3 etc,.
*/

    replace surveyor = "1" if surveyor == "Benjamin"
    replace surveyor = "5" if surveyor == "John"
    replace surveyor = "4" if surveyor == "Mary"
    replace surveyor = "3" if surveyor == "Anna"
    replace surveyor = "2" if surveyor == "Peter"
    replace surveyor = "9" if surveyor == "Sam"
    replace surveyor = "8" if surveyor == "David"
    replace surveyor = "7" if surveyor == "Joseph"
    replace surveyor = "6" if surveyor == "Caroline"
    replace surveyor = "11" if surveyor == "Jane"
    replace surveyor = "10" if surveyor == "Grace"
   
   
/*
Since, the mean of the age was 334, why this was happening because, some people
had age as 1996 which was in terms year. so to solve it I'm going to there
year of birth with actual age.
*/

    replace age = 54 if age == 1981
    replace age = 54 if age == 1971
    replace age = 53 if age == 1980
    replace age = 53 if age == 1970
    replace age = 50 if age == 1973
    replace age = 49 if age == 1972
    replace age = 48 if age == 1975
    replace age = 47 if age == 1976
    replace age = 47 if age == 1974
    replace age = 46 if age == 1978
    replace age = 46 if age == 1977
    replace age = 45 if age == 1978
    replace age = 44 if age == 1982
    replace age = 42 if age == 1979
    replace age = 40 if age == 1983
    replace age = 39 if age == 1984
    replace age = 38 if age == 1985
    replace age = 37 if age == 1986
    replace age = 34 if age == 1989
    replace age = 33 if age == 1990
    replace age = 30 if age == 1987
    replace age = 29 if age == 1988
   
    sum age
  
// now the mean of age is 44.28 years which make sense.

/*
  If you brows the data you will find there some negative values such as -999
  or -555, these 
  are missing values but as of provides no information,
  so recoding these values
  as...
*/

    recode burglaryyn vandalismyn trespassingyn (-999 = .r) // don't know
    recode burglaryyn vandalismyn trespassingyn (-997 = .d)// refusal
    recode burglaryyn vandalismyn trespassingyn (-777 = .b)// blank
    recode burglaryyn vandalismyn trespassingyn (-666 = .)// ''''
    recode burglaryyn vandalismyn trespassingyn (-555 = .n)// not applicable
    sum age
	
// cleaning ends up here

/*
   Insatalling new data for research methodology..
   where the question is the effect of increasing in tenure security through
   land ownership rights in the household labour supply.
*/

    use "D:\Practice stata\analysis_sample.dta", clear
// to find uniqueness of each id

    ssc install unique
    unique lotsize
   
// to get to know full names of each variables
     describe
/*

   Using DID to the difference in labour supply of program beneficiary and
   non beneficiary households in early neighbourhoods to the difference 
   between beneficiary and non beneficiary households in the late neighbourhoods
   i.e, future program - after the survey..
   
*/

    diff pctmale , t(hastitle) p(enter)
/*
   fraction of adult household who are male has increased by 0.027 points to 
   the people who are program
   beneficiary compared to control group of those who doen't have property title.
*/

    diff members, t(hastitle) p(enter)
/*
   total number of household members of any age has decreased by 0.361 points to 
   the people who are program
   beneficiary compared to control group of those who doen't have property title. 
*/

    diff elemhd, t(hastitle) p(enter)
/*
   high grade attaintment of hh head elementary school has increased by 0.017
   point to the people who are program
   beneficiary compared to control group of those who doen't have property title.  

*/
    diff avgage, t(hastitle) p(enter)
/*
   the average age of household members has decreased by 0.571 points
   to the people who are program
   beneficiary compared to control group of those who doen't have property title.  
   
*/
    diff agehd, t(hastitle) p(enter)
/*
   the age of hh head have also decreased by 0.738 points to 
   the people who are program
    beneficiary compared to control group of those who doen't have property title.
*/

//  NOTE; All the results were insignificant ***

/*
   Now some plottings/Graphs----- All the graphs have been saved if you want to
   look at it, you can contact me at sjalam@gmail.com... I have some beutiful 
   graphs with heading, labelling and super rich colour where I have used 
   amazing vg_bright schemes..
*/


/*
   kdensity graph,,, all the density plots are reght skewd..
*/
    kdensity totwkhrs if hastitle==0 & enter==0, title("No program, Not titled") xtitle("Comparison group") ytitle("Total working hours")
    graph export "kdensity_totwkhrs_hastitle_1.pdf", replace
   
    kdensity totwkhrs if hastitle==1 & enter==1, title("Program, Title") xtitle("Target group") ytitle("Total working hours")
    graph export "kdensity_totwkhrs_hastitle_2.pdf", replace
    
    kdensity totwkhrs if enter==0 & squat==1, title("No Program, Squatter") xtitle("Comparison group") ytitle("Total working hours")
    graph export "kdensity_totwkhrs_hastitle_3.pdf", replace
   
    kdensity totwkhrs if enter==1 & squat==1, title("Program, Squatter") xtitle("Comparisom group") ytitle("Total working hours")
    graph export "kdensity_totwkhrs_hastitle_4.pdf", replace

/*
   Regression analysis of age of household head to various variables
   and making a publishable table in latex format by using esstab..
   where only elemhd is significant to note while others are insignificant.
*/

    eststo: quietly regress agehd elemhd
    eststo: quietly regress agehd elemhd enteryr_fut
    eststo: quietly regress agehd elemhd enteryr_fut hw
    eststo: quietly regress agehd elemhd enteryr_fut hw lotsize
    eststo: quietly regress agehd elemhd enteryr_fut hw lotsize tenuresquat
    esttab
    esttab, se ar2
   
// extracting the table in latex and rtf format

    esttab using example.tex, label title(Regression table\label{tab1})
    esttab using example.rtf, replace nogaps title({\b Table 2.} {\i This is the 2{\super nd} table})
   
/*
   Two way scatter plot and bar graph.....
*/

    twoway scatter agehd tenure, xtitle("Tenure") ytitle("Age")
    twoway (scatter agehd tenure)(lfit agehd tenure), xtitle("Tenure") ytitle("Age")
    graph export "scatterplot_agehd_tenure.pdf",replace
// here the relationship of household age and their tenure is positive i.e,
// as age increases of hh head his tenure is also increases..

    twoway (scatter agehd totwkhrs )(lfit agehd totwkhrs ), xtitle("Total Working Hours") ytitle("Age")
    graph export "scatterplot_agehd_totwkhrs.pdf",replace
// scatter plot with lfit line...

    tab agehd
/*
   Interesting part atleast for me ...I want to know which age group is working more 
   and which is not...the range of age is (30,92) 
   for that I have created four subpart variables such as 1 for
   the people under 30 years , 2 for people between 30 to 50, 3 for the people of 
   50 to 70 and 4 for people above 70 
*/
	 gen age = 1 if agehd <= 30
	 replace age = 2 if agehd > 30 & agehd <= 50
	 replace age = 3 if agehd > 50 & agehd <= 70
	 replace age = 4 if agehd > 70 & agehd <= 92
	
// labelling four categories as young, mid young, late young  & old

    label define agelab 1 "Young" 2 "Mid Young" 3 "Late Young" 4 "Old"
    label values age agelab
    label variable age "age(four catogories)"
   
// normal graph...
    graph bar, over(age) 
    graph bar, over(age)
    graph export "Bar_graph_age_1.pdf",replace
   
// setting up  schemes for bright and vibrant colours...
    set scheme vg_brite
   
    graph bar, over(age) asyvar
// here the graph is showing mid young people working the most which is 
// around 48 %

    graph export "Bar_graph_age_2.pdf",replace
// I want to know more ....so making a pie graph would help

    graph pie, over(age)
    graph export "Pie_chart_age.pdf",replace
   
    graph pie, over(age) pie(1 ,explode) plabel(_all percent)
    graph export "Pie_chart_with%_age.pdf",replace
/*
   Here in this pie graph I'm getting exact values of proportion of people
   where 49.02% of people are mid young which very close to my previous
   result ...but not only that young people are about 4.72%, late young are
   about 39.3% and old people are 6.963%  ... which depicts the working force is
   very hardworking as they are from 30 to 70 years old......
*/
// log-file is available on request..
/*
**************************THANK YOU FOR YOUR TIME.******************************
*/
