use adss_2020t
go

-- Age Specific Mortality Rates

--if OBJECT_ID('cfb..ASMR','U') is not null
--drop table cfb..ASMR
--go 

--SELECT  [Years]
--      ,[AgeGroup]
--      ,[Ordered]
--      ,[MidYearPop]
--      ,[Freq]
--      ,[ASMR]
--  into cfb..ASMR
--  FROM [ADSS].[dbo].[cfb_CHIDO_ASMR]


-- Frequency or counts of births
  if OBJECT_ID('cfb..Birth','U') is not null
drop table cfb..Birth
go 

select *
into cfb..Birth
from cfb_CHIDO_Birth

--Crude rates for mortality, birth, deaths, inmigration, outmigration

if OBJECT_ID('cfb..cruderates','U') is not null
drop table cfb..cruderates
go 

select *
into cfb..cruderates
from cfb_CHIDO_cruderates

--Frequency or counts for births, deaths, inmigration, outmigration

if OBJECT_ID('cfb..freq','U') is not null
drop table cfb..freq
go 

select *
into cfb..freq
from cfb_CHIDO_freq
go


--Population counts by Years 
if OBJECT_ID('cfb..pop','U') is not null
drop table cfb..pop
go 



with a as (
select Years
       ,VillNo
	   ,PoliticalVillage
	   ,AgeGroup
	   ,Ordered
	   ,MalePop
	   ,FemalePop
	   ,MidYearPop
from cfb_PVill_MidYearPop_bySex_AgeGRoup
union
select Years
       ,80 as VillNo
	   ,'Original' as PoliticalVillage
	   ,AgeGroup
	   ,Ordered
	   ,MalePop
	   ,FemalePop
	   ,MidYearPop
from cfb_Site_MidYearPop_bySex_AgeGRoup
union
select Years
       ,90 as VillNo
	   ,'Current' as PoliticalVillage
	   ,AgeGroup
	   ,Ordered
	   ,MalePop
	   ,FemalePop
	   ,MidYearPop
from cfb_Site_MidYearPop_bySex_AgeGRoup_Current
)
select *
into cfb..pop
from a


--Age Specific Fertility Rates

if OBJECT_ID('cfb..Birth_ASFR','U') is not null
drop table cfb..Birth_ASFR
go 



with a as (
select Years
       ,VillNo
	   ,PoliticalVillage
	   ,AgeGroup
	   ,ASFR
from cfb_PVill_Birth_ASFR
union
select Years
       ,80 as VillNo
	   ,'Original' as PoliticalVillage
	   ,AgeGroup
	   ,ASFR
from cfb_Site_Birth_ASFR
union
select Years
       ,90 as VillNo
	   ,'Current' as PoliticalVillage
	   ,AgeGroup
	   ,ASFR
from cfb_Site_Birth_ASFR_Current
)
select *
into cfb..Birth_ASFR
from a

--Current education by Years

if OBJECT_ID('cfb..CurrentEdu','U') is not null
drop table cfb..CurrentEdu
go 

with a as (
select Year as Years
       ,Village as VillNo
	   ,VillName as PoliticalVillage
	   ,Age
	   ,PreSchool
	   ,PreSchool_Percent *100 as PreSchool_Percent
	   ,Grade1to4
	   ,Grade1to4_Percent * 100 as Grade1to4_Percent
	   ,Grade5to7 
	   ,Grade5to7_Percent *100 as Grade5to7_Percent
	   ,Grade8to12
	   ,Grade8to12_Percent * 100 as Grade8to12_Percent
	   ,Incomplete_Tertiary
	   ,Incomplete_Tertiary_Percent *100 as Incomplete_Tertiary_Percent
	   ,Complete_Tertiary
	   ,Complete_Tertiary_Percent *100 as Complete_Tertiary_Percent
	   ,Total_Pop
from cfb_PVill_CurrentEdu
union
select Year
       , 80 as Village
	   ,'Original' as VillName
	   ,Age
	   ,PreSchool
	   ,PreSchool_Percent * 100 as PreSchool_Percent
	   , Grade1to4
	   ,Grade1to4_Percent * 100 as Grade1to4_Percent
	   ,Grade5to7
	   ,Grade5to7_Percent * 100 as Grade5to7_Percent
	   ,Grade8to12
	   ,Grade8to12_Percent *100 as Grade8to12_Percent
	   ,Incomplete_Tertiary
	   ,Incomplete_Tertiary_Percent * 100 as Incomplete_Tertiary_Percent
	   ,Complete_Tertiary
	   ,Complete_Tertiary_Percent * 100 as Complete_Tertiary_Percent
	   ,Total_Pop
from cfb_Site_CurrentEdu
union
select Year
      ,90 as Village
	  ,'Current' as VillName
	  ,Age 
	  ,PreSchool
	  ,PreSchool_Percent * 100 as PreSchool_Percent
	  ,Grade1to4
	  ,Grade1to4_Percent * 100 as Grade1to4_Percent
	  ,Grade5to7
	  ,Grade5to7_Percent * 100 as Grade5to7_Percent
	  ,Grade8to12
	  ,Grade8to12_Percent * 100 as Grade8to12_Percent
	  ,Incomplete_Tertiary
	  ,Incomplete_Tertiary_Percent * 100 as Incomplete_Tertiary_Percent
	  ,Complete_Tertiary
	  ,Complete_Tertiary_Percent * 100 as Complete_Tertiary_Percent
	  ,Total_Pop
from cfb_Site_CurrentEdu_Current
)
select *
into cfb..CurrentEdu
from a

--Cause of Death from VA

if OBJECT_ID('cfb..CoDVA','U') is not null
drop table cfb..CoDVA
go 
with a as (
SELECT  Year as Years
      ,[AgeGroup]
      ,Cause 
      ,CntAll
      ,Rnk
	  , case when AgeGroup = 'Neonate' then 1 when AgeGroup = '<1' then 2 when AgeGroup = '1-4' then 3 when AgeGroup = '5-19' then 4
	  when AgeGroup = '20-49' then 5 when AgeGroup = '50-64' then 6 when  AgeGroup = '65+' then 7 else null end Ordered
  FROM adss2020..cfb_Site_VA_CoD
  ), b as (
  SELECT [Years]
	  , case when age < 1 then 2 
			WHEN age BETWEEN 1  AND 5  THEN 3
		    WHEN age BETWEEN 5  AND 19 THEN 4
		    WHEN age BETWEEN 20 AND 49 THEN 5	
		    WHEN age BETWEEN 50 AND 64 THEN 6	    			    	    
		    WHEN age >=65 THEN 7 END AS AgeGroup	
      ,sum([TotalDeaths]) as Total
  FROM [ADSS_2020t].[dbo].[cfb_Site_DeathCount_bySexAge_Current]
  group by [Years]
	  , case when age < 1 then 2 
			WHEN age BETWEEN 1  AND 5  THEN 3
		    WHEN age BETWEEN 5  AND 19 THEN 4
		    WHEN age BETWEEN 20 AND 49 THEN 5	
		    WHEN age BETWEEN 50 AND 64 THEN 6	    			    	    
		    WHEN age >=65 THEN 7 END )
		
select a.Years, a.AgeGroup, a.Ordered, a.CntAll, a.Cause, a.[Rnk], b.Total
into cfb..CoDVA	
from a 
join b on a.Years= b.Years and a.Ordered=b.AgeGroup

--Diead At Place
  if OBJECT_ID('cfb..DiedAt','U') is not null
drop table cfb..DiedAt
go 

SELECT Years
      ,DiedAt
      ,Under5 
      ,[Age5-14]
      ,[Age15-49]
	  ,[Age50-64]
	  ,[65AndOlder]
	  ,Age_Unknown
	  ,TOTALDEATH
  into cfb..DiedAt
  FROM [cfb_Site_DiedAt_Current]

  -- Water Avalilability
if OBJECT_ID('cfb..WaterAvailabl','U') is not null
drop table cfb..WaterAvailabl
go 

with a as (
select [Variable]
      ,[Year]
      ,[VillNo]
      ,[PoliticalVillage]
      ,[Households]
      ,[Always]
      ,[Always%]
      ,[Most of the time]
      ,[Most of the time%]
      ,[Few hours a day]
      ,[Few hours a day%]
      ,[Irregular not every day]
      ,[Irregular not every day%]
      ,[Very irregular]
      ,[Very irregular%]
      ,[Unknown]
      ,[Unknown%]
from cfb_PVill_WaterAvailabl
union
select [Variable]
      ,[Year]
      ,80 as [VillNo]
      ,'Original' as [PoliticalVillage]
      ,[Households]
      ,[Always]
      ,[Always%]
      ,[Most of the time]
      ,[Most of the time%]
      ,[Few hours a day]
      ,[Few hours a day%]
      ,[Irregular not every day]
      ,[Irregular not every day%]
      ,[Very irregular]
      ,[Very irregular%]
      ,[Unknown]
      ,[Unknown%]
from cfb_Site_WaterAvailabl
union
select [Variable]
      ,[Year]
      ,90 as [VillNo]
      ,'Current' as [PoliticalVillage]
      ,[Households]
      ,[Always]
      ,[Always%]
      ,[Most of the time]
      ,[Most of the time%]
      ,[Few hours a day]
      ,[Few hours a day%]
      ,[Irregular not every day]
      ,[Irregular not every day%]
      ,[Very irregular]
      ,[Very irregular%]
      ,[Unknown]
      ,[Unknown%]
from cfb_Site_WaterAvailabl_Current
)
select *
into cfb..WaterAvailabl
from a


-- Water Supply
if OBJECT_ID('cfb..WaterSupply','U') is not null
drop table cfb..WaterSupply
go 

with a as (
select [Variable]
      ,[Year]
      ,[VillNo]
	  ,PoliticalVillage
      ,Household as [Households]
      ,[Tap in House] 
      ,[Tap in House%] 
      ,[Tap in Yard] 
      ,[Tap in Yard%] 
      ,[Tap in Street] 
      ,[Tap in Street%] 
      ,[Truck]
      ,[Truck%] 
      ,[Cement well] 
      ,[Cement well%] 
      ,[Traditional well] 
      ,[Traditional well%] 
      ,[Pond]
      ,[Pond %] 
      ,[River]
      ,[River %] 
      ,[Dam]
      ,[Dam %]
      ,[Rainwater tank] 
      ,[Rainwater tank %] 
      ,[Other]
      ,[Other %] 
      ,[Unknown]
      ,[Unknown %] 
from cfb_PVil_WaterSupply
union
select [Variable]
      ,[Year]
      ,80 as [VillNo]
	  ,'Original' as PoliticalVillage
      ,Household as [Households]
 ,[Tap in House] 
      ,[Tap in House%] 
      ,[Tap in Yard] 
      ,[Tap in Yard%] 
      ,[Tap in Street] 
      ,[Tap in Street%] 
      ,[Truck]
      ,[Truck%] 
      ,[Cement well] 
      ,[Cement well%] 
      ,[Traditional well] 
      ,[Traditional well%] 
      ,[Pond]
      ,[Pond %] 
      ,[River]
      ,[River %] 
      ,[Dam]
      ,[Dam %] 
      ,[Rainwater tank] 
      ,[Rainwater tank %] 
      ,[Other]
      ,[Other %] 
      ,[Unknown]
      ,[Unknown %] 
from cfb_Site_WaterSupply
union
select [Variable]
      ,[Year]
      ,90 as [VillNo]
	  ,'Current' as PoliticalVillage
      ,Household as [Households]
,[Tap in House] 
      ,[Tap in House%] 
      ,[Tap in Yard] 
      ,[Tap in Yard%] 
      ,[Tap in Street] 
      ,[Tap in Street%] 
      ,[Truck]
      ,[Truck%] 
      ,[Cement well] 
      ,[Cement well%] 
      ,[Traditional well] 
      ,[Traditional well%] 
      ,[Pond]
      ,[Pond %] 
      ,[River]
      ,[River %] 
      ,[Dam]
      ,[Dam %] 
      ,[Rainwater tank]
      ,[Rainwater tank %] 
      ,[Other]
      ,[Other %] 
      ,[Unknown]
      ,[Unknown %] 
from cfb_Site_WaterSupply_Current
)
, b as (
select *
from a

UNPIVOT 
(
  NoofHouseholds FOR Source IN ([Tap in House], [Tap in Yard], [Tap in Street],[Truck],[Cement well],[Traditional well],[Pond],[Dam],[River], [Rainwater tank], [Other],[Unknown])
) AS up
UNPIVOT 
(
  PercofHouseholds FOR Source1 IN ([Tap in House%], [Tap in Yard%], [Tap in Street%],[Truck%],[Cement well%],[Traditional well%],[Pond %],[Dam %],[River %], [Rainwater tank %], [Other %],[Unknown %])
) AS ep
)

select Variable, Year, VillNo, PoliticalVillage, Households,Source, NoofHouseholds, PercofHouseholds 
into cfb..WaterSupply
from b
where Source like Source1
go
/*
select *
from cfb..WaterSupply
where VillNo in (80,90)
order by Year
*/

-- Education by Gender aged 5-19
if OBJECT_ID('cfb..EducGender','U') is not null
drop table cfb..EducGender
go 

with a as (
SELECT   case when CensusRound =25 then 2019 else YEAR(o.ObservationDate) end   AS Year_P
		,c.Village AS Village_P, c.Name as VillName_P
		,e.Id
		,i.Gender
		,CASE WHEN Education IN ('C','R') THEN 1 ELSE 0 END AS PreSchool_P
		,CASE WHEN Education IN ('A','A1','B','1','2',' M1','M2','M3','M4')THEN 1 ELSE 0 END AS Grade1to4_P
		,CASE WHEN Education IN ('A2','A3','3','4','5','M5','M6','M7') THEN 1 ELSE 0 END AS Grade5to7_P
		,CASE WHEN Education IN ('A4','6','7','8','9','0','N1','M8','M9','M10','M11','M12') THEN 1 ELSE 0 END AS Grade8to12_P
		,CASE WHEN Education IN ('H','L1','L2','T1','T2','U1') THEN 1 ELSE 0 END AS Incomplete_Tertiary_P
		,CASE WHEN Education IN ('U2') THEN 1 ELSE 0 END AS Complete_Tertiary_P
		,CASE WHEN e.Id IS NOT NULL THEN 1 ELSE 0 END AS Total_Pop_P
		,FLOOR(DATEDIFF(DAY,i.DoB,o.ObservationDate)/365.25)  as Age
FROM EducationStatus e
	JOIN Individuals i ON e.Id = i.Id
	JOIN Observations o ON o.Observation = e.Observation
	JOIN Locations l ON l.Location=o.Location
	JOIN Villages c ON c.Village=l.PoliticalVillage
WHERE l.Village NOT IN ('00','32','33','34','35','38')
--	   AND FLOOR(DATEDIFF(DAY,i.DoB,o.ObservationDate)/365.25) BETWEEN 1 AND 21
--	   AND  YEAR(ObservationDate) IN (2017)
--GROUP BY YEAR(o.ObservationDate),c.Village ,c.Name
)
,b as (
select Year_P
       ,Village_P
	   ,VillName_P
	   ,Gender
	   ,sum(PreSchool_P) as PreSchool
	   ,sum(Grade1to4_P) as Grade1to4
	   ,sum(Grade5to7_P) as Grade5to7
	   ,sum(Grade8to12_P) as Grade8to12
	   ,sum(Incomplete_Tertiary_P) as Incomplete_Tertiary
	   ,sum(Complete_Tertiary_P) as Complete_Tertiary
	   ,sum(Total_Pop_P) as Total_Pop
from a
where Age between 5 and 19
group by Year_P, Village_P, VillName_P, Gender
--order by Year_P, Village_P, VillName_P, Gender
)
, c as (
select Year_P as Year
       ,Village_P as Village
	   ,VillName_P as VillName
	   ,Gender
	   ,PreSchool + Grade1to4 + Grade5to7 + Grade8to12 + Incomplete_Tertiary as [Total learners]
	   ,Total_Pop
from b
)
,[Site] as (
select c.Year, 80 as Village, 'Original' as Villname, Gender, sum([Total learners]) [Total learners] , sum(Total_Pop) Total_Pop
from c
join cfb_PolVillage_Original p on p.VilNo=c.Village
group by c.Year, Gender
)
, [Current] as (
select c.Year, 90 as Village, 'Current' as Villname, Gender, sum([Total learners]) [Total learners] , sum(Total_Pop) Total_Pop
from c
join cfb_PolVillage_Current p on p.VilNo=c.Village
group by c.Year, Gender
)
select c.*
      ,cast([Total learners] as float)/Total_Pop *100 as Perc
into cfb..EducGender
from c
union
select Year, Village,VillName, Gender, [Total learners], Total_Pop,cast([Total learners] as float)/Total_Pop *100 as Perc
from [Site]
union 
select Year, Village,VillName, Gender, [Total learners], Total_Pop, cast([Total learners] as float)/Total_Pop *100 as Perc
from [Current]
order by Year, Village



--Unemployment

drop table if exists cfb..UnemploymentRate_NoAGe
go

select *
into cfb..UnemploymentRate_NoAGe
from cfb_PVill_UnemploymentRate_NoAGe
union 
select Years, 80 as VillNo, 'Original' as PoliticalVillage, MaleUnemployed, FemaleUnemployed, MaleEmployed, FemaleEmployed, TotalEmployed, TotalUnemployed, LabourForce, Rate
from cfb_Site_UnemploymentRate_NoAGe
union 
select Years, 90 as VillNo, 'Current' as PoliticalVillage, MaleUnemployed, FemaleUnemployed, MaleEmployed, FemaleEmployed, TotalEmployed, TotalUnemployed, LabourForce, Rate
from cfb_Site_UnemploymentRate_NoAGe
go

drop table if exists cfb..UnemploymentRate_byAGe
go

select *
into cfb..UnemploymentRate_byAGe
from cfb_PVill_UnemploymentRate_byAGe
union 
select Years, 80 as VillNo, 'Original' as PoliticalVillage,AgeGroup, MaleUnemployed, FemaleUnemployed, MaleEmployed, FemaleEmployed, TotalEmployed, TotalUnemployed, LabourForce, Rate
from cfb_Site_UnemploymentRate_byAGe
union 
select Years, 90 as VillNo, 'Current' as PoliticalVillage,AgeGroup, MaleUnemployed, FemaleUnemployed, MaleEmployed, FemaleEmployed, TotalEmployed, TotalUnemployed, LabourForce, Rate
from cfb_Site_UnemploymentRate_byAGe_Current
go

select *
from cfb..UnemploymentRate_byAGe

--Total population by sex

drop table if exists cfb..TotPopBySex
go

select *
 into cfb..TotPopBySex
  FROM [cfb_PVill_MidYearPop_bySex]
  union
SELECT Years, 80 as VillNo, 'Original' as PoliticalVillage, MalePop, FemalePop, MidYearPop
  FROM [cfb_Site_MidYearPop_bySex]
  union
select Years, 90 as VillNo, 'Current' as PoliticalVillage, MalePop, FemalePop, MidYearPop
  FROM [cfb_Site_MidYearPop_bySex_Current]


--Households
drop table if exists cfb..Households
go

select *
into cfb..Households
from cfb_PVill_HouseholdFreq
union 
select Years, 80 as VillNo, 'Original' as PoliticalVillage, HouseholdCnt
from cfb_Site_HouseholdFreq
union
select Years, 90 as VillNo, 'Current' as PoliticalVillage, HouseholdCnt
from cfb_Site_HouseholdFreq_Current


--Table 1

drop table if exists cfb..Table1
go

select h.Years, h.VillNo, h.PoliticalVillage, h.HouseholdCnt, t.MidYearPop as Pop, t.MalePop , t.FemalePop , p.MidYearPop as Under5 , e.[Total learners] SchoolGoing
into cfb..Table1
from cfb..Households h 
join cfb..TotPopBySex t on t.Years=h.Years and t.VillNo=h.VillNo
join cfb..Pop p on p.Years=h.Years and p.VillNo=h.VillNo and p.Ordered=1
join (select Year, Village, sum([Total learners]) as [Total learners]
from cfb..EducGender
group by Year, Village) e on e.Year=h.Years and cast(e.Village as int)=h.VillNo


--Education

drop table if exists cfb..curedu
go


select Years, VillNo,Age, PoliticalVillage, School_level, Count, Prop
into cfb..curedu
from cfb..CurrentEdu c
unpivot(
Count
for School_level in (PreSchool, Grade1to4, Grade5to7, Grade8to12, Incomplete_Tertiary, Complete_Tertiary)
) as p
unpivot(
Prop
for School in (PreSchool_Percent, Grade1to4_Percent, Grade5to7_Percent, Grade8to12_Percent, Incomplete_Tertiary_Percent, Complete_Tertiary_percent)
) as p
where School_level  = substring(School,1, CHARINDEX ('_', School) - 1)

--DiedAt 

drop table if exists cfb..VA_CoDp
go

SELECT  [Years]
      ,[DiedAt]
      ,[Under5]
      ,[Age5-14]
      ,[Age15-49]
      ,[Age50-64]
      ,[65AndOlder]
      ,[TOTALDEATH]
	into cfb..VA_CoDp
  FROM [ADSS_2020t].[dbo].[cfb_Site_DiedAt_Current]


