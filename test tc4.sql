-- SQL commands to test moving region extension

CREATE TABLE data_storm(
	id integer primary key,
	name VARCHAR(50),
	storm mregion
)

-- to data_rain table STORM NANA
UPDATE data_storm SET storm = something FROM (SELECT append_intervalregion(tc4.inv) AS something FROM tc4 WHERE tc4.id=1) as som WHERE data_storm.id = 1;
UPDATE data_storm SET storm = something FROM  (SELECT append_intervalregion(d.storm, tc4.inv) AS something FROM tc4, data_storm AS d WHERE tc4.id=2 AND d.id=1) AS som WHERE data_storm.id = 1;
UPDATE data_storm SET storm = something FROM  (SELECT append_intervalregion(d.storm, tc4.inv) AS something FROM tc4, data_storm AS d WHERE tc4.id=3 AND d.id=1) AS som WHERE data_storm.id = 1;
UPDATE data_storm SET storm = something FROM  (SELECT append_intervalregion(d.storm, tc4.inv) AS something FROM tc4, data_storm AS d WHERE tc4.id=4 AND d.id=1) AS som WHERE data_storm.id = 1;
UPDATE data_storm SET storm = something FROM  (SELECT append_intervalregion(d.storm, tc4.inv) AS something FROM tc4, data_storm AS d WHERE tc4.id=5 AND d.id=1) AS som WHERE data_storm.id = 1;
UPDATE data_storm SET storm = something FROM  (SELECT append_intervalregion(d.storm, tc4.inv) AS something FROM tc4, data_storm AS d WHERE tc4.id=6 AND d.id=1) AS som WHERE data_storm.id = 1;
UPDATE data_storm SET storm = something FROM  (SELECT append_intervalregion(d.storm, tc4.inv) AS something FROM tc4, data_storm AS d WHERE tc4.id=7 AND d.id=1) AS som WHERE data_storm.id = 1;
UPDATE data_storm SET storm = something FROM  (SELECT append_intervalregion(d.storm, tc4.inv) AS something FROM tc4, data_storm AS d WHERE tc4.id=8 AND d.id=1) AS som WHERE data_storm.id = 1;
UPDATE data_storm SET storm = something FROM  (SELECT append_intervalregion(d.storm, tc4.inv) AS something FROM tc4, data_storm AS d WHERE tc4.id=9 AND d.id=1) AS som WHERE data_storm.id = 1;

-- to data_mregion table STORM OMAR
UPDATE data_storm SET storm = something FROM (SELECT append_intervalregion(tc4_2.inv) AS something FROM tc4_2 WHERE tc4_2.id=1) as som WHERE data_storm.id = 2;
UPDATE data_storm SET storm = something FROM  (SELECT append_intervalregion(d.storm, tc4_2.inv) AS something FROM tc4_2, data_storm AS d WHERE tc4_2.id=2 AND d.id=2) AS som WHERE data_storm.id = 2;
UPDATE data_storm SET storm = something FROM  (SELECT append_intervalregion(d.storm, tc4_2.inv) AS something FROM tc4_2, data_storm AS d WHERE tc4_2.id=3 AND d.id=2) AS som WHERE data_storm.id = 2;
UPDATE data_storm SET storm = something FROM  (SELECT append_intervalregion(d.storm, tc4_2.inv) AS something FROM tc4_2, data_storm AS d WHERE tc4_2.id=4 AND d.id=2) AS som WHERE data_storm.id = 2;
UPDATE data_storm SET storm = something FROM  (SELECT append_intervalregion(d.storm, tc4_2.inv) AS something FROM tc4_2, data_storm AS d WHERE tc4_2.id=5 AND d.id=2) AS som WHERE data_storm.id = 2;
UPDATE data_storm SET storm = something FROM  (SELECT append_intervalregion(d.storm, tc4_2.inv) AS something FROM tc4_2, data_storm AS d WHERE tc4_2.id=6 AND d.id=2) AS som WHERE data_storm.id = 2;
UPDATE data_storm SET storm = something FROM  (SELECT append_intervalregion(d.storm, tc4_2.inv) AS something FROM tc4_2, data_storm AS d WHERE tc4_2.id=7 AND d.id=2) AS som WHERE data_storm.id = 2;
UPDATE data_storm SET storm = something FROM  (SELECT append_intervalregion(d.storm, tc4_2.inv) AS something FROM tc4_2, data_storm AS d WHERE tc4_2.id=8 AND d.id=2) AS som WHERE data_storm.id = 2;
UPDATE data_storm SET storm = something FROM  (SELECT append_intervalregion(d.storm, tc4_2.inv) AS something FROM tc4_2, data_storm AS d WHERE tc4_2.id=9 AND d.id=2) AS som WHERE data_storm.id = 2;
UPDATE data_storm SET storm = something FROM  (SELECT append_intervalregion(d.storm, tc4_2.inv) AS something FROM tc4_2, data_storm AS d WHERE tc4_2.id=10 AND d.id=2) AS som WHERE data_storm.id = 2;
UPDATE data_storm SET storm = something FROM  (SELECT append_intervalregion(d.storm, tc4_2.inv) AS something FROM tc4_2, data_storm AS d WHERE tc4_2.id=11 AND d.id=2) AS som WHERE data_storm.id = 2;
UPDATE data_storm SET storm = something FROM  (SELECT append_intervalregion(d.storm, tc4_2.inv) AS something FROM tc4_2, data_storm AS d WHERE tc4_2.id=12 AND d.id=2) AS som WHERE data_storm.id = 2;
UPDATE data_storm SET storm = something FROM  (SELECT append_intervalregion(d.storm, tc4_2.inv) AS something FROM tc4_2, data_storm AS d WHERE tc4_2.id=13 AND d.id=2) AS som WHERE data_storm.id = 2;
UPDATE data_storm SET storm = something FROM  (SELECT append_intervalregion(d.storm, tc4_2.inv) AS something FROM tc4_2, data_storm AS d WHERE tc4_2.id=14 AND d.id=2) AS som WHERE data_storm.id = 2;
UPDATE data_storm SET storm = something FROM  (SELECT append_intervalregion(d.storm, tc4_2.inv) AS something FROM tc4_2, data_storm AS d WHERE tc4_2.id=15 AND d.id=2) AS som WHERE data_storm.id = 2;
UPDATE data_storm SET storm = something FROM  (SELECT append_intervalregion(d.storm, tc4_2.inv) AS something FROM tc4_2, data_storm AS d WHERE tc4_2.id=16 AND d.id=2) AS som WHERE data_storm.id = 2;
UPDATE data_storm SET storm = something FROM  (SELECT append_intervalregion(d.storm, tc4_2.inv) AS something FROM tc4_2, data_storm AS d WHERE tc4_2.id=17 AND d.id=2) AS som WHERE data_storm.id = 2;
UPDATE data_storm SET storm = something FROM  (SELECT append_intervalregion(d.storm, tc4_2.inv) AS something FROM tc4_2, data_storm AS d WHERE tc4_2.id=18 AND d.id=2) AS som WHERE data_storm.id = 2;
UPDATE data_storm SET storm = something FROM  (SELECT append_intervalregion(d.storm, tc4_2.inv) AS something FROM tc4_2, data_storm AS d WHERE tc4_2.id=19 AND d.id=2) AS som WHERE data_storm.id = 2;


-- TO CHECK IF INSERTION WENT WELL
SELECT ST_Union(get_snapshots(storm)) FROM data_storm;
SELECT traversed(storm) FROM data_storm