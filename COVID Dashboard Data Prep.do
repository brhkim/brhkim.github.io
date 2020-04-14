/**********************************************************************

	Name: COVID Dashboard Data Prep.do
	Created: April, 2020
	Modified: April, 2020
	Author: Brian Heseung Kim (twitter/github: @brhkim)

	Purpose: This do-file prepares data in a format useable by the Nudge4
	Solutions COVID-19 Health Workforce Recruiting Dashboard
	( https://www.nudge4.org/covid19 ). To do so, this file pulls together:
	
		1. Most recent COVID case data from Virginia Department of Health
			(county-level)
		2. Historical COVID case data from VDH (manually constructed from
			archives of #1) (county-level)
		3. Hospital bed data from Urban Institute (Blavin & Arnos, 2020)
			(county-level)
		4. Population data from ACS 2018 to transform Urban's beds-per-1000-pop
			data into raw counts
		5. ICU bed data from Kaiser Health News (Schulte, Lucas, Rau, Szabo, 
			& Hancock, 2020) (county-level)
		6. Our output tables from our recent-graduates and near-completers
			analysis with VCCS
		7. A crosswalk of VCCS service regions to VA counties to merge together
			COVID, Urban, and KHN data. Manually created by BK from VEDP data:
			http://gis.vedp.org/datasets/6a24e3e31e414611a7338b0b3682ef10_13
	
	Contents:
		#setup - Setup file and prepare globals
		#healthdata - Process all the health data from outside sources
		#studentdata - Process all the student data from our analysis
		#finalprep - Final cleaning steps, and cleaning validation steps
		#distortion - Analyze the extent of data distortion due to cell size suppression
		
***********************************************************************/

/***********************************************************************

	#setup - Setup file and prepare globals

***********************************************************************/
	
//Basics
	clear all
	set more off
	
//Set up logging
	global sysdate=c(current_date)
	global sysdate=subinstr("$sysdate", " ", "", .)
	capture log close
	log using "logs/covid_cases_converter_log_$sysdate.log", replace
		
//Set globals for settings, etc.
	global updatecasedata=1
		//Set to 0 if no need to pull new data, set to 1 to download
	global distortionanalysis=0
		//Set whether you want to run an analysis of distortions based on our 
		//replacement of values <5 with 5
	global pastdate=3
		//Set how many days back to display for past COVID case count data

//Set globals for filenames and filepaths
	global covidhealthdataarchive="COVID Health Data Archive"
		//Archive folder for all days of pulled covid health data so far
	global healthdataname="healthdata.csv"
		//Filename for most current health data
	global pasthealthdataname="pasthealthdata.csv"
		//Filename for appended past health data over time
	global censusdata="acs_county_population_2018.csv"
		//Filename for current ACS population estimates to transform Urban
		//hospital bed data
	global hospitalbeds="hospital_beds_us_counties.csv"
		//Urban hospital bed data
	global icubeds="icu_beds_us_counties.csv"
		//Kaiser Health News ICU bed data
	global countycrosswalk "VCCS County Crosswalk.csv"
		//Manually created crosswalk between VCCS college coverage and counties
	global recentgradcounts "../recent_graduates_college_by_program_counts.csv"
		//Analytic data for counts of health science recent grads
	global nearcompletercounts "../near_halfway_completers_college_by_program_counts.csv"
		//Analytic data for counts of health science near completers
	global outputname "vccscollegecovidcases.csv"
		//Final save file name
		
	// Upload final csv data file here: https://github.com/brhkim/brhkim.github.io
		

/***********************************************************************

	#healthdata - Process all the health data from outside sources

***********************************************************************/
	
//Grab health data
	if $updatecasedata==1 {
		copy http://www.vdh.virginia.gov/content/uploads/sites/182/2020/03/VDH-COVID-19-PublicUseDataset-Cases.csv $healthdataname, replace
			/* Automatically download updated county-level COVID data from VDH.
			Saves in the current working directory as $healthdataname.
			Note from the website: This page is updated daily before 10 AM. 
			Numbers are preliminary and close out at 5 PM the day before posting. 
			Case counts reflect what has been reported to VDH by healthcare providers 
			and laboratories. */
	}

	import delimited "$healthdataname", clear
	capture rename ïreportdate reportdate
	
	//Save the health data in a format appropriate for archiving first
		gen newdate = date(reportdate, "MDY", 2030)
		format newdate %td
		gen month = month(newdate)
		gen day = day(newdate)
		gen year = year(newdate)
		sum newdate, detail
		local currentdate=r(max)
		local pastdate=(`currentdate'-${pastdate})
		
		//Get nicely formatted days
		if strlen(string(day))==1 {
			local day = "0" + string(day)
		}
		if strlen(string(day))==2 {
			local day = string(day)
		}
		
		//Get nicely formatted months
		if strlen(string(month))==1 {
			local month = "0" + string(month)
		}
		if strlen(string(month))==2 {
			local month = string(month)
		}
			
		drop newdate month day year
		
		export delimited using "${covidhealthdataarchive}/20-`month'-`day'_healthdata.csv", replace
	
		tempfile currenthealthdata
		save `currenthealthdata', replace
	
	//Append the current health data to the archived health data, since it's stored
	//day by day without an archive accessible
		preserve
			import delimited "${covidhealthdataarchive}/${pasthealthdataname}", clear
			gen newdate = date(reportdate, "MDY", 2030)
			sum newdate, detail
			local archivedate=r(max)
			drop newdate
			
			append using `currenthealthdata'
			
			if `archivedate'<`currentdate' {
			export delimited using "${covidhealthdataarchive}/${pasthealthdataname}", replace
			}
			
			gen newdate=date(reportdate, "MDY", 2030)
			keep if newdate==`pastdate'
			
			keep locality totalcases
			rename totalcases priorcases
			
			tempfile pasthealthdata
			save `pasthealthdata', replace
		restore
		
	//Now get past health data
	merge 1:1 locality using `pasthealthdata'
	
	//Ensure merge worked correctly
	qui sum _merge
	local mergecheck = r(mean)
	assert `mergecheck'==3
		//If this fails, it means something didn't merge properly between the old
		//health data and the newest set
	
	drop _merge
	
	
//Grab hospital bed data
	preserve
	
		import delimited "${covidhealthdataarchive}/${censusdata}", clear
		keep if stname=="Virginia"
		keep if year==11
		keep if agegrp==0
		
		gen fips=string(county)
		replace fips="00" + fips if strlen(fips)==1
		replace fips="0" + fips if strlen(fips)==2
		replace fips="51"+fips
		
		keep fips tot_pop ctyname
		
		tempfile popdata
		save `popdata', replace

		import delimited "${covidhealthdataarchive}/${hospitalbeds}", clear
		
		//Mild cleaning to prepare for merge
		keep if statename=="Virginia"
		gen fips = string(id)
		keep fips beds countyname
		
		merge 1:1 fips using `popdata'
	
		//Ensure merge worked correctly
		qui sum _merge
		local mergecheck = r(mean)
		assert `mergecheck'==3
			//If this fails, it means something didn't merge properly between the 
			//population data and the Urban hospital beds data
	
		drop _merge
		
		gen hospitalbeds=beds*(tot_pop/1000)
		
		keep fips hospitalbeds
		destring fips, replace
		
		tempfile hospitalbeds
		save `hospitalbeds', replace
	restore

	//Merge in hospital beds data
	merge 1:1 fips using `hospitalbeds'
	
	//Ensure merge worked correctly
	qui sum _merge
	local mergecheck = r(mean)
	assert `mergecheck'==3
		//If this fails, it means something didn't merge properly between the old
		//health data and the newest set
	
	drop _merge
	

//Grab ICU bed data
	preserve
		import delimited "${covidhealthdataarchive}/${icubeds}", clear
		
		//Mild cleaning to prepare for merge
		keep state county icubeds populationaged60
		keep if state=="Virginia"
		
		//Prep county names for name merge
		gen county_new = county
		replace county_new = substr(county_new, 1, strlen(county_new)-5) if substr(county_new, strlen(county_new)-3, .)=="City"
		replace county=county_new if county_new!="Buena Vista" & county_new!="Fairfax" & ///
			county_new!="Franklin" & county_new!="Manassas" & county_new!="Richmond" & ///
			county_new!="Roanoke" & county_new!="James" & county_new!="Charles"
			
		replace county=county+" County" if county=="Franklin" | county=="Richmond" | county=="Roanoke"
			
		rename county locality
		rename populationaged60 pop60
		drop county_new state
		
		tempfile icubeds
		save `icubeds', replace
	restore
	
	//Merge ICU beds data back in
	merge 1:1 locality using `icubeds'
	
	//Ensure merge worked correctly
	qui sum _merge
	local mergecheck = r(mean)
	assert `mergecheck'==3
		//If this fails, it means something didn't merge properly between the old
		//health data and the newest set
	
	drop _merge
	
	tempfile healthdata
	save `healthdata', replace
	
	
//Prep crosswalk
	preserve
		import delimited "$countycrosswalk", clear
		capture rename ïfips fips
		tempfile temp
		save `temp', replace
	restore
	
	preserve
		use `temp', clear
		keep locality vccscollege
		sort vccscollege locality
		
		//Number up all the counties served by each college
		egen countynum=seq(), by(vccscollege)
		qui sum countynum
		local maxcounties = r(max)
		
		//Reshape to wide so we can concatenate counties served by each college
		reshape wide locality, i(vccscollege) j(countynum)
		
		//Generate a simple list of localities served. Use semicolons so we can save as csv
		gen serving=locality1
		forvalues i=2/`maxcounties' {
			replace serving = serving + "; " + locality`i'
			
			//Get rid of extraneous semicolons from shorter locality lists
			replace serving = regexr(serving, "; ; ", "")
		}

		//Get rid of trailing "; "'s from the sequence
		replace serving=substr(serving, 1, strlen(serving)-2) if substr(serving, strlen(serving)-1, 2)=="; "
		
		keep vccscollege serving
		rename vccscollege college
		
		tempfile temp2
		save `temp2', replace
	restore

	
//Merge in VCCS college data for each county in the health data
	merge 1:m locality using `temp'
	rename vccscollege college
	
	//Ensure merge worked correctly
	qui sum _merge
	local mergecheck = r(mean)
	assert `mergecheck'==3
		//If this fails, it means something didn't merge properly between the health data
		//and the VCCS-county coverage crosswalk
	
	drop _merge
	
//Adjust for counties split between colleges
	gen currentcasemod = totalcases * share
	gen priorcasemod = priorcases * share
	gen hospitalmod = hospitalbeds * share
	gen icumod = icubeds * share
	gen pop60mod = pop60 * share
	
//Grab current date of health report
	local date = reportdate[1]

//Collapse down to the college level for cases
	collapse (sum) currentcasemod priorcasemod hospitalmod icumod pop60mod, by(college)
	rename currentcasemod totalcases
	rename priorcasemod priorcases
	rename hospitalmod hospitalbeds
	rename icumod icubeds
	rename pop60mod pop60
	
//Generate new date variable after collapse
	gen date = "`date'"

//Merge in service area county names
	merge m:1 college using `temp2'
	
	//Ensure merge worked correctly
	qui sum _merge
	local mergecheck = r(mean)
	assert `mergecheck'==3
		//If this fails, it means something didn't merge properly between the college
		//area county names and the health data
	
	drop _merge
	
	tempfile coviddata
	save `coviddata', replace
	clear
	

/***********************************************************************

	#studentdata - Process all the student data from our analysis

***********************************************************************/
	
//Prep student counts data for recentgrads
	preserve
		import delimited "$recentgradcounts", clear

		//Drop out never employed individuals, per BC's suggestion for clarity
			//drop if never_employed==1 Add them back in, BK 20-04-08
		
		//Flatten across employment status types
			collapse (sum) count, by(college acadplan_deglvl curr_text last2years)
		
		//Basic data cleaning
			gen recentgrad=1
			gen last3to5years=.
			replace last3to5years=1 if last2years==0
			replace last3to5years=0 if last2years==1
		
		//Merge in student counts data for recentgrads
			merge m:1 college using `coviddata'
	
		//Ensure merge worked correctly
			qui sum _merge
			local mergecheck = r(mean)
			assert `mergecheck'==3
				//If this fails, it means something didn't merge properly between the college
				//names in the health data/crosswalk above and the recent grad counts data we produced
				//from our analysis
		
			drop _merge
			
		tempfile recentgrads
		save `recentgrads', replace
	restore

//Prep student counts data for near completers
	preserve
		import delimited "$nearcompletercounts", clear
		
		drop if near_completer==0
		
		//Merge in student counts data for recentgrads
			merge m:1 college using `coviddata'
	
		//Ensure merge worked correctly
			qui sum _merge
			local mergecheck = r(mean)
			assert `mergecheck'==3
				//If this fails, it means something didn't merge properly between the college
				//names in the health data/crosswalk above and the recent grad counts data we produced
				//from our analysis
		
			drop _merge
			
		tempfile nearcompleters
		save `nearcompleters', replace
	restore

	
//Append student counts data for near-completers
	append using `recentgrads' `nearcompleters'
	
	
/***********************************************************************

	#finalprep - Final cleaning steps, and cleaning validation steps, to
				 prepare data for upload

***********************************************************************/
	
//Basic cleaning of data to prepare for dashboard compatibility
	//Drop out unnecessary variables
		drop acadplan_dipl_descr acadplan curr collnum
		capture drop cip halfway_completer
	
	//Clean up idiosyncratic degree level categories and rename variable for clarity
		gen deglvl=""
		replace deglvl="AAS" if acadplan_deglvl=="AAS"
		replace deglvl="AAS" if acadplan_deglvl=="AA&S"
		
		replace deglvl="CERT" if acadplan_deglvl=="CERT"
		
		replace deglvl="CSC" if acadplan_deglvl=="CSC"
		replace deglvl="CSC" if acadplan_deglvl=="PRE"
		replace deglvl="CSC" if acadplan_deglvl==""
			//Careful here; this may not always be true!
		
		drop acadplan_deglvl
		
	//Clean up group status
		gen group=""
		replace group="Recent Graduate" if recentgrad==1
		replace group="Near Completer" if near_completer==1
		drop recentgrad near_completer

	//Clean up employment status
		/* Remove for now, BK 20-04-07
		gen empstatus=""
		replace empstatus="No Recent Employment, Never in Health" if emp_norecent_neverhealth==1
		replace empstatus="No Recent Employment, Prior Employment in Health" if emp_norecent_priorhealth==1
		replace empstatus="Recent Employment, Not in Health" if emp_recent_nothealth==1
		drop emp_norecent_neverhealth emp_norecent_priorhealth emp_recent_nothealth never_employed
		*/
		
//Run data validity checks before saving final file
	//Check degree level
		count 
		local total=r(N)
		
		count if missing(deglvl)
		local degreemissing=r(N)
		
		assert `degreemissing'==0
			//If this fails, some observations are missing a valid degree status
		
	//Check group type
		count if missing(group)
		local groupmissing=r(N)
		
		assert `groupmissing'==0
			//If this fails, some observations are missing a valid group status

	/* Remove for now, BK 20-04-07
	//Check employment type
		count if (missing(empstatus) & group=="Recent Graduate")
		local empmissing=r(N)
		
		assert `empmissing'==0
			//If this fails, some observations are missing a valid group status
	*/
			
	//Check program name
		count if missing(curr_text)
		local currmissing=r(N)
		
		assert `currmissing'==0
			//If this fails, some observations are missing a valid program name status
			
	//Check counts for covid and our analysis
		count if (missing(totalcases) | missing(count))
		local countsmissing=r(N)
		
		assert `countsmissing'==0
			//If this fails, some observations are missing a valid COVID or count status

	//Just see what the latest date of the data are
		codebook date

		
/***********************************************************************

	#distortion - Analyze the extent of data distortion due to cell size suppression

***********************************************************************/
		
if $distortionanalysis == 1 {
	//NOTE TO SELF: Maybe try dropping out cells 2 and below?
	preserve
		global viewtype "college curr_text"
		global distortion "2"
	
		gen countround1 = count
		replace countround1 = 5 if countround1 ==1 | countround1==2 | countround1==3 | countround1==4
			//Scheme where all values below 5 are rounded to 5
			
		gen countround2 = count
		replace countround2 = 0 if countround2 ==1 | countround2==2
		replace countround2 = 5 if countround2 ==3 | countround2==4
			//Scheme where all values below 2 are rounded to 0, and 3-4 are rounded to 5
			
		//Find the value of the error in native units
		gen distortion1 = countround1 - count
		gen distortion2 = countround2 - count
		
		//Summarize the mean cell size of cells affected by rounding
		sum count if countround1 != count
		
		//Summarize the mean error of cells affected by rounding
		sum distortion1 if countround1 != count, detail
		sum distortion2 if countround1 != count, detail
		
		//Generate a % error for cells
		gen error1 = distortion1/count		
		gen error2 = distortion2/count
		gen abserror2 = abs(error2)
		
		//Summarize the % error only for cells affected by rounding
		sum error1 if count!=countround1, detail
		sum error2 if count!=countround1, detail
		sum abserror2 if count!=countround1, detail
		
		//Examine distortion by set values
		egen sum_group = total(count), by(${viewtype})
		egen grouptag = tag(${viewtype})
		
		egen sumd${distortion}_group = total(distortion${distortion}), by(${viewtype})
		if ($distortion == 2) {
			egen abssumd${distortion}_group = total(abserror2), by(${viewtype})
		}
		
		gen errord${distortion}_group = sumd${distortion}_group/sum_group
		gen abserrord${distortion}_group = abs(errord${distortion}_group)
		
		sum sumd${distortion}_group if grouptag==1, detail
		if ($distortion == 2) {
			sum abssumd${distortion}_group if grouptag==1, detail
		}
		
		sum errord${distortion}_group if grouptag==1, detail
		
		sum abserrord${distortion}_group if grouptag==1, detail
	
	restore
}		
		
	//Abide by minimum cell size
		replace count = 0 if count==1 | count==2
		replace count = 5 if count==3 | count==4
			
//Save
	export delimited using "$outputname", replace


