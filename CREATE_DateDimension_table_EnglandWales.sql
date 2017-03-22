/*=====================================================================
*	Creating a date dimension or calendar table in SQL Server
*	English and Wales
*	This script will need to be updated for any extra Bank holidays announced by the UK Govt (Royal Weddings, Royal Deaths, Royal Anniversaries etc)
*
*	Hat Tip Aaron Bertrand
*	https://www.mssqltips.com/sqlservertip/4054/creating-a-date-dimension-or-calendar-table-in-sql-server/?utm_source=dailynewsletter&utm_medium=email&utm_content=headline&utm_campaign=20170321
*/
USE xStuff;
GO

DECLARE @StartDate DATE = '20000101', @NumberOfYears INT = 30;

-- prevent set or regional settings from interfering with 
-- interpretation of dates / literals

SET DATEFIRST 7;	--SUNDAY is first day of week
SET DATEFORMAT DMY;
--SET LANGUAGE US_ENGLISH;
SET LANGUAGE British;

DECLARE @CutoffDate DATE = DATEADD(YEAR, @NumberOfYears, @StartDate);

-- this is just a holding table for intermediate calculations:

IF OBJECT_ID('tempdb..#dim') IS NOT NULL
	DROP TABLE #dim;
CREATE TABLE #dim
(
  [date]       DATE PRIMARY KEY, 
  [day]        AS DATEPART(DAY,      [date]),
  [month]      AS DATEPART(MONTH,    [date]),
  FirstOfMonth AS CONVERT(DATE, DATEADD(MONTH, DATEDIFF(MONTH, 0, [date]), 0)),
  [MonthName]  AS DATENAME(MONTH,    [date]),
  [week]       AS DATEPART(WEEK,     [date]),
  [ISOweek]    AS DATEPART(ISO_WEEK, [date]),
  [DayOfWeek]  AS DATEPART(WEEKDAY,  [date]),
  [quarter]    AS DATEPART(QUARTER,  [date]),
  [year]       AS DATEPART(YEAR,     [date]),
  FirstOfYear  AS CONVERT(DATE, DATEADD(YEAR,  DATEDIFF(YEAR,  0, [date]), 0)),
  Style112     AS CONVERT(CHAR(8),   [date], 112),
  Style101     AS CONVERT(CHAR(10),  [date], 101)
);

-- use the catalog views to generate as many rows as we need

INSERT #dim([date]) 
SELECT d
FROM
(
  SELECT d = DATEADD(DAY, rn - 1, @StartDate)
  FROM 
  (
    SELECT TOP (DATEDIFF(DAY, @StartDate, @CutoffDate)) 
      rn = ROW_NUMBER() OVER (ORDER BY s1.[object_id])
    FROM sys.all_objects AS s1
    CROSS JOIN sys.all_objects AS s2
    -- on my system this would support > 5 million days
    ORDER BY s1.[object_id]
  ) AS x
) AS y;
GO


IF OBJECT_ID('dbo.DateDimension') IS NULL
BEGIN
	--DROP TABLE dbo.DateDimension
	CREATE TABLE dbo.DateDimension(
		DateKey             INT         NOT NULL CONSTRAINT PK_DateDimension PRIMARY KEY,
		[Date]              DATE        NOT NULL,
		[Day]               TINYINT     NOT NULL,
		DaySuffix           CHAR(2)     NOT NULL,
		[Weekday]           TINYINT     NOT NULL,
		WeekDayName         VARCHAR(10) NOT NULL,
		IsWeekend           BIT         NOT NULL,
		IsHoliday           BIT         NOT NULL,
		IsBankHoliday       BIT         NOT NULL,
		HolidayText         VARCHAR(64) SPARSE,
		DOWInMonth          TINYINT     NOT NULL,
		[DayOfYear]         SMALLINT    NOT NULL,
		WeekOfMonth         TINYINT     NOT NULL,
		WeekOfYear          TINYINT     NOT NULL,
		ISOWeekOfYear       TINYINT     NOT NULL,
		[Month]             TINYINT     NOT NULL,
		[MonthName]         VARCHAR(10) NOT NULL,
		[Quarter]           TINYINT     NOT NULL,
		QuarterName         VARCHAR(6)  NOT NULL,
		[Year]              INT         NOT NULL,
		MMYYYY              CHAR(6)     NOT NULL,
		MonthYear           CHAR(7)     NOT NULL,
		FirstDayOfMonth     DATE        NOT NULL,
		LastDayOfMonth      DATE        NOT NULL,
		FirstDayOfQuarter   DATE        NOT NULL,
		LastDayOfQuarter    DATE        NOT NULL,
		FirstDayOfYear      DATE        NOT NULL,
		LastDayOfYear       DATE        NOT NULL,
		FirstDayOfNextMonth DATE        NOT NULL,
		FirstDayOfNextYear  DATE        NOT NULL
	);
END;
GO

IF NOT EXISTS ( SELECT 1 FROM dbo.DateDimension)
BEGIN
	INSERT dbo.DateDimension WITH (TABLOCKX)
	SELECT
	  DateKey       = CONVERT(INT, Style112),
	  [Date]        = [date],
	  [Day]         = CONVERT(TINYINT, [day]),
	  DaySuffix     = CONVERT(CHAR(2), CASE WHEN [day] / 10 = 1 THEN 'th' ELSE 
					  CASE RIGHT([day], 1) WHEN '1' THEN 'st' WHEN '2' THEN 'nd' 
					  WHEN '3' THEN 'rd' ELSE 'th' END END),
	  [Weekday]     = CONVERT(TINYINT, [DayOfWeek]),
	  [WeekDayName] = CONVERT(VARCHAR(10), DATENAME(WEEKDAY, [date])),
	  [IsWeekend]   = CONVERT(BIT, CASE WHEN [DayOfWeek] IN (1,7) THEN 1 ELSE 0 END),
	  [IsHoliday]   = CONVERT(BIT, 0),
	  [IsBankHoliday]   = CONVERT(BIT, 0),
	  HolidayText   = CONVERT(VARCHAR(64), NULL),
	  [DOWInMonth]  = CONVERT(TINYINT, ROW_NUMBER() OVER 
					  (PARTITION BY FirstOfMonth, [DayOfWeek] ORDER BY [date])),
	  [DayOfYear]   = CONVERT(SMALLINT, DATEPART(DAYOFYEAR, [date])),
	  WeekOfMonth   = CONVERT(TINYINT, DENSE_RANK() OVER 
					  (PARTITION BY [year], [month] ORDER BY [week])),
	  WeekOfYear    = CONVERT(TINYINT, [week]),
	  ISOWeekOfYear = CONVERT(TINYINT, ISOWeek),
	  [Month]       = CONVERT(TINYINT, [month]),
	  [MonthName]   = CONVERT(VARCHAR(10), [MonthName]),
	  [Quarter]     = CONVERT(TINYINT, [quarter]),
	  QuarterName   = CONVERT(VARCHAR(6), CASE [quarter] WHEN 1 THEN 'First' 
					  WHEN 2 THEN 'Second' WHEN 3 THEN 'Third' WHEN 4 THEN 'Fourth' END), 
	  [Year]        = [year],
	  MMYYYY        = CONVERT(CHAR(6), LEFT(Style101, 2)    + LEFT(Style112, 4)),
	  MonthYear     = CONVERT(CHAR(7), LEFT([MonthName], 3) + LEFT(Style112, 4)),
	  FirstDayOfMonth     = FirstOfMonth,
	  LastDayOfMonth      = MAX([date]) OVER (PARTITION BY [year], [month]),
	  FirstDayOfQuarter   = MIN([date]) OVER (PARTITION BY [year], [quarter]),
	  LastDayOfQuarter    = MAX([date]) OVER (PARTITION BY [year], [quarter]),
	  FirstDayOfYear      = FirstOfYear,
	  LastDayOfYear       = MAX([date]) OVER (PARTITION BY [year]),
	  FirstDayOfNextMonth = DATEADD(MONTH, 1, FirstOfMonth),
	  FirstDayOfNextYear  = DATEADD(YEAR,  1, FirstOfYear)
	FROM #dim
	OPTION (MAXDOP 1);
END;
GO

/*===========================
*	IsHoliday Updates
============================*/

;WITH x AS 
(
  SELECT DateKey, [Date], IsHoliday, HolidayText, FirstDayOfYear, IsBankHoliday, [Year]
    ,DOWInMonth, [MonthName], [WeekDayName], [Day],
    LastDOWInMonth = ROW_NUMBER() OVER 
    (
      PARTITION BY FirstDayOfMonth, [Weekday] 
      ORDER BY [Date] DESC
    )
	,FirstDOWInMonth = ROW_NUMBER() OVER 
    (
      PARTITION BY FirstDayOfMonth, [Weekday] 
      ORDER BY [Date] ASC
    )
  FROM dbo.DateDimension
)
/*=================================
*	UK Public Holidays
=================================*/
UPDATE x 
SET
	 IsHoliday = 1
	,HolidayText = CASE
					  WHEN ([Date] = FirstDayOfYear) 
						THEN 'New Year''s Day'
					  WHEN ([DAY] = 29 AND [MonthName] = 'April' AND [Year] = 2011)
						THEN 'Royal Wedding Bank Holiday'			-- (Royal Wedding)
					  WHEN ([FirstDOWInMonth] = 1 AND [MonthName] = 'May' AND [WeekDayName] = 'Monday')
						THEN 'Early May Bank Holiday'				-- (First Monday in May)
					  WHEN ([LastDOWInMonth] = 1 AND [MonthName] = 'May'   AND [WeekDayName] = 'Monday') AND [Year] <> 2012
						THEN 'Spring Bank Holiday'					-- (Last Monday in May)
					  WHEN ([FirstDOWInMonth] = 1 AND [MonthName] = 'June'   AND [WeekDayName] = 'Monday') AND [Year] = 2012
						THEN 'Spring Bank Holiday'					-- (Spring bank holiday (substitute day - Queen’s Diamond Jubilee year)
					  WHEN ([FirstDOWInMonth] = 1 AND [MonthName] = 'June'   AND [WeekDayName] = 'Tuesday') AND [Year] = 2012
						THEN 'Queen’s Diamond Jubilee Bank Holiday'	-- (Queen’s Diamond Jubilee (extra bank holiday)
					  WHEN ([LastDOWInMonth] = 1 AND [MonthName] = 'August'   AND [WeekDayName] = 'Monday')
						THEN 'Summer Bank Holiday'					-- (Last Monday in August)
					  WHEN ([MonthName] = 'December' AND [Day] = 25)
						THEN 'Christmas Day'
					  WHEN ([MonthName] = 'December' AND [Day] = 26)
						THEN 'Boxing Day'
					  END
WHERE 
	([Date] = FirstDayOfYear)
	OR ([DAY] = 29 AND [MonthName] = 'April' AND [Year] = 2011)
	OR ([FirstDOWInMonth] = 1 AND [MonthName] = 'May'	AND [WeekDayName] = 'Monday')
	OR ([LastDOWInMonth] = 1 AND [MonthName] = 'May'   AND [WeekDayName] = 'Monday')  AND [Year] <> 2012
	OR ([FirstDOWInMonth] = 1 AND [MonthName] = 'June'   AND [WeekDayName] = 'Monday') AND [Year] = 2012
	OR ([FirstDOWInMonth] = 1 AND [MonthName] = 'June'   AND [WeekDayName] = 'Tuesday') AND [Year] = 2012
	OR ([LastDOWInMonth] = 1 AND [MonthName] = 'August'	AND [WeekDayName] = 'Monday')
	OR ([MonthName] = 'December' AND [Day] = 25)
	OR ([MonthName] = 'December' AND [Day] = 26);
GO


/*============================
*	New Years Bank Holiday
*	Calculate Substitute day
=============================*/
;WITH x
AS(
	SELECT
		Date, Day, WeekDay, WeekDayName, IsWeekEnd, IsHoliday, IsBankHoliday
		,CASE
			WHEN
				WeekDayName IN ( 'Saturday')
					THEN DATEADD(DAY, 2, [Date])
			WHEN
				WeekDayName IN ( 'Sunday')
					THEN DATEADD(DAY, 1, [Date])
			ELSE
				[Date]
		 END		'NewYearBankHoliday'
	FROM
		dbo.DateDimension
	WHERE
		DayOfYear = 1
)
UPDATE
	D
SET
	 IsHoliday = 1
	,IsBankHoliday = 1
	,HolidayText = 'New Year''s Bank Holiday'
FROM
	dbo.DateDimension	D
INNER JOIN
	x	ON X.NewYearBankHoliday = D.Date;
GO


/*===========================
*	Calculate Days of Easter
============================*/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'dbo.GetEasterHolidays') AND type in (N'FN', N'IF', N'TF', N'FS', N'FT'))
DROP FUNCTION dbo.GetEasterHolidays;
GO
CREATE FUNCTION dbo.GetEasterHolidays(@year INT) 
RETURNS TABLE
WITH SCHEMABINDING
AS 
RETURN 
(
	WITH x AS 
	(
	SELECT [Date] = CONVERT(DATE, RTRIM(@year) + '0' + RTRIM([Month]) 
		+ RIGHT('0' + RTRIM([Day]),2))
		FROM (SELECT [Month], [Day] = DaysToSunday + 28 - (31 * ([Month] / 4))
		FROM (SELECT [Month] = 3 + (DaysToSunday + 40) / 44, DaysToSunday
		FROM (SELECT DaysToSunday = paschal - ((@year + @year / 4 + paschal - 13) % 7)
		FROM (SELECT paschal = epact - (epact / 28)
		FROM (SELECT epact = (24 + 19 * (@year % 19)) % 30) 
		AS epact) AS paschal) AS dts) AS m) AS d
	)
	SELECT [Date], HolidayName = 'Easter Sunday' FROM x
	UNION ALL SELECT DATEADD(DAY,-2,[Date]), 'Good Friday'   FROM x
	UNION ALL SELECT DATEADD(DAY, 1,[Date]), 'Easter Monday' FROM x
);
GO


/*===========================
*	Easter
*	Calculate Bank Holidays
============================*/
;WITH x AS 
(
	SELECT d.[Date], d.IsHoliday, d.HolidayText, h.HolidayName, IsBankHoliday
	FROM dbo.DateDimension AS d
	CROSS APPLY dbo.GetEasterHolidays(d.[Year]) AS h
	WHERE d.[Date] = h.[Date]
)
UPDATE
	x
SET
	 IsHoliday = 1
	,IsBankHoliday = CASE
						WHEN HolidayName IN ( 'Good Friday', 'Easter Monday') 
							THEN 1
						ELSE
							0
					 END
	,HolidayText = HolidayName;
GO


/*=========================
*	Update IsBankHoliday
*	for Bank Holidays
=========================*/
UPDATE
	dbo.DateDimension
SET
	IsBankHoliday = 1
WHERE
	HolidayText LIKE '%Bank Holiday';
GO

/*=========================================
*	Christmas 
*	Calculate Substitute bank holiday
==========================================*/
;WITH X
AS(
	SELECT
		Date, Day, WeekDay, WeekDayName, IsWeekEnd, IsHoliday, HolidayText, IsBankHoliday
		,CASE
			WHEN
				(WeekDayName IN ( 'Saturday') AND ([MonthName] = 'December' AND [Day] = 25))
					THEN DATEADD(DAY, 2, [Date])
			WHEN
				(WeekDayName IN ( 'Sunday') AND ([MonthName] = 'December' AND [Day] = 25))
					THEN DATEADD(DAY, 1, [Date])
			ELSE
				[Date]
		 END		'XMas_BankHoliday'
	FROM
		dbo.DateDimension
	WHERE
		([MonthName] = 'December' AND [Day] = 25)
)
UPDATE
	D
SET
	 IsHoliday = 1
	,IsBankHoliday = 1
	,HolidayText = 'Christmas Day Bank Holiday'
FROM
	dbo.DateDimension	D
INNER JOIN
	X	ON X.XMas_BankHoliday = D.Date ;
GO

/*=========================================
*	Boxing Day 
*	Calculate Substitute bank holiday
==========================================*/
;WITH X
AS(
	SELECT
		Date, Day, WeekDay, WeekDayName, IsWeekEnd, IsHoliday, HolidayText, IsBankHoliday
		,CASE
			WHEN
				(WeekDayName IN ( 'Saturday') AND ([MonthName] = 'December' AND [Day] = 26))
					THEN DATEADD(DAY, 2, [Date])
			WHEN
				(WeekDayName IN ( 'Sunday') AND ([MonthName] = 'December' AND [Day] = 26))
					THEN DATEADD(DAY, 2, [Date])
			WHEN	--to cater for Christmas Day Substitute bank holiday
				(WeekDayName IN ( 'Monday') AND ([MonthName] = 'December' AND [Day] = 26))
					THEN DATEADD(DAY, 1, [Date])
			ELSE
				[Date]
		 END		'BoxDay_BankHoliday'
	FROM
		dbo.DateDimension
	WHERE
		([MonthName] = 'December' AND [Day] = 26)
)
UPDATE
	D
SET
	 IsHoliday = 1
	,IsBankHoliday = 1
	,HolidayText = 'Boxing Day Bank Holiday'
FROM
	dbo.DateDimension	D
INNER JOIN
	X	ON X.BoxDay_BankHoliday = D.Date;
GO


SELECT * FROM dbo.DateDimension WHERE HolidayText IS NOT NULL AND [Year] = 2008 ;
--SELECT * FROM dbo.DateDimension WHERE [Year] = 2011 AND [MonthName] = 'April' ;
--SELECT * FROM dbo.DateDimension WHERE IsBankHoliday = 1 AND [Year] = 2013 ;
--DROP TABLE dbo.DateDimension

IF OBJECT_ID('tempdb..#dim') IS NOT NULL
	DROP TABLE #dim;
GO
