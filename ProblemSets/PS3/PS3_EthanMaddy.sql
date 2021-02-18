-- A: step 1: read in dataset by creating a table with all variables
CREATE TABLE datfl(policyID,statecode,county,eq_site_limit,hu_site_limit,
             fl_site_limit,fr_site_limit,tiv_2011,tiv_2012,eq_site_deductible,
             hu_site_deductible,fl_site_deductible,fr_site_deductible,
             point_latitude,point_longitude,line,construction,point_granularity);
--step 2: read in dataset by setting file type
.mode csv
-- import file
.import DScourseS21/ProblemSets/PS3/FL_insurance_sample.csv datfl

-- B: limit to show number of rows
SELECT * FROM datfl LIMIT 10;

-- C: 'distinct' function to list unique values
SELECT DISTINCT county FROM datfl;

--D: 'avg' function to compute average
SELECT AVG (tiv_2012 - tiv_2011) FROM datfl;
--398118.230

--E: create one-way frequency table to list amount categories
SELECT construction, COUNT(*) FROM datfl GROUP BY construction;
