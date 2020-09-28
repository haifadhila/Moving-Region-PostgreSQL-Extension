-- SQL commands to test moving region extension

CREATE TABLE data_rain(
	id integer primary key,
	name VARCHAR(50),
	country VARCHAR(50),
	rain mregion
)

-- to data_rain table RAIN 1
UPDATE data_rain SET rain = something FROM (SELECT append_intervalregion(tc3.inv) AS something FROM tc3 WHERE tc3.id=1) as som WHERE data_rain.id = 1;
UPDATE data_rain SET rain = something FROM (SELECT append_intervalregion(d.rain, tc3.inv) AS something FROM tc3, data_rain AS d WHERE tc3.id=2 AND d.id=1) AS som WHERE data_rain.id = 1;
UPDATE data_rain SET rain = something FROM (SELECT append_intervalregion(d.rain, tc3.inv) AS something FROM tc3, data_rain AS d WHERE tc3.id=3 AND d.id=1) AS som WHERE data_rain.id = 1;
UPDATE data_rain SET rain = something FROM (SELECT append_intervalregion(d.rain, tc3.inv) AS something FROM tc3, data_rain AS d WHERE tc3.id=4 AND d.id=1) AS som WHERE data_rain.id = 1;
UPDATE data_rain SET rain = something FROM (SELECT append_intervalregion(d.rain, tc3.inv) AS something FROM tc3, data_rain AS d WHERE tc3.id=5 AND d.id=1) AS som WHERE data_rain.id = 1;
UPDATE data_rain SET rain = something FROM (SELECT append_intervalregion(d.rain, tc3.inv) AS something FROM tc3, data_rain AS d WHERE tc3.id=6 AND d.id=1) AS som WHERE data_rain.id = 1;


-- to data_mregion table RAIN 2
UPDATE data_rain SET rain = something FROM (SELECT append_intervalregion(tc3.inv) AS something FROM tc3 WHERE tc3.id=11) as som WHERE data_rain.id = 2;
UPDATE data_rain SET rain = something FROM (SELECT append_intervalregion(d.rain, tc3.inv) AS something FROM tc3, data_rain AS d WHERE tc3.id=21 AND d.id=2) AS som WHERE data_rain.id = 2;
UPDATE data_rain SET rain = something FROM (SELECT append_intervalregion(d.rain, tc3.inv) AS something FROM tc3, data_rain AS d WHERE tc3.id=31 AND d.id=2) AS som WHERE data_rain.id = 2;


-- to data_mregion table RAIN 3
UPDATE data_rain SET rain = something FROM (SELECT append_intervalregion(tc3.inv) AS something FROM tc3 WHERE tc3.id=12) as som WHERE data_rain.id = 3;
UPDATE data_rain SET rain = something FROM (SELECT append_intervalregion(d.rain, tc3.inv) AS something FROM tc3, data_rain AS d WHERE tc3.id=22 AND d.id=3) AS som WHERE data_rain.id = 3;
UPDATE data_rain SET rain = something FROM (SELECT append_intervalregion(d.rain, tc3.inv) AS something FROM tc3, data_rain AS d WHERE tc3.id=32 AND d.id=3) AS som WHERE data_rain.id = 3;
UPDATE data_rain SET rain = something FROM (SELECT append_intervalregion(d.rain, tc3.inv) AS something FROM tc3, data_rain AS d WHERE tc3.id=42 AND d.id=3) AS som WHERE data_rain.id = 3;


-- TO CHECK IF INSERTION WENT WELL
SELECT ST_Union(get_snapshots(rain)) FROM data_rain;
SELECT traversed(rain) FROM data_rain