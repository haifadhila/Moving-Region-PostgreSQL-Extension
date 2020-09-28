-- SQL commands to test moving region extension

CREATE TABLE data_mregion(
	id integer primary key,
	name VARCHAR(50),
	the_mregion mregion
)

-- DATA INSERTION
-- to tc2 intervalregion table
INSERT INTO tc2 VALUES(1, intervalregion(1, 50, ST_GeomFromText('POINT(110.072225 -7.397931)', 4326), ST_Polygon('LINESTRING(110.032802 -7.456366, 110.014020 -7.473366, 110.015498 -7.516727, 110.101650 -7.554951,110.140042 -7.540164,110.135237 -7.495852,110.095237 -7.495852,110.032802  -7.456366)', 4326)));
INSERT INTO tc2 VALUES(2, intervalregion(50, 150, ST_Polygon('LINESTRING(110.032802 -7.456366, 110.014020 -7.473366, 110.015498 -7.516727, 110.101650 -7.554951,110.140042 -7.540164,110.135237 -7.495852,110.095237 -7.495852,110.032802  -7.456366)', 4326), ST_Polygon('LINESTRING(110.112802 -7.620034, 110.064020 -7.653366, 110.065498 -7.716727, 110.131650 -7.774951,110.190042 -7.740164,110.185237 -7.695852,110.112802  -7.620034)', 4326)));
INSERT INTO tc2 VALUES(3, intervalregion(150, 200, ST_Polygon('LINESTRING(110.112802 -7.620034, 110.064020 -7.653366, 110.065498 -7.716727, 110.131650 -7.774951,110.190042 -7.740164,110.185237 -7.695852,110.112802  -7.620034)', 4326), ST_Polygon('LINESTRING(110.312802 -7.770034, 110.264020 -7.803366, 110.265498 -7.866727, 110.331650 -7.924951,110.390042 -7.890164,110.385237 -7.845852,110.312802 -7.770034)', 4326)));
INSERT INTO tc2 VALUES(4, intervalregion(200, 300, ST_Polygon('LINESTRING(110.312802 -7.770034, 110.264020 -7.803366, 110.265498 -7.866727, 110.331650 -7.924951,110.390042 -7.890164,110.385237 -7.845852,110.312802 -7.770034)', 4326), ST_Polygon('LINESTRING(110.580095 -7.932058, 110.531343 -7.953140, 110.554689 -8.009579,110.627474 -8.015019,110.667299 -7.969460,110.617861 -7.948379,110.580095 -7.932058)', 4326)));
INSERT INTO tc2 VALUES(5, intervalregion(300, 400, ST_Polygon('LINESTRING(110.580095 -7.932058, 110.531343 -7.953140, 110.554689 -8.009579,110.627474 -8.015019,110.667299 -7.969460,110.617861 -7.948379,110.580095 -7.932058)', 4326), ST_Polygon('LINESTRING(110.760095 -7.992058, 110.701343 -7.983140, 110.777474 -8.015019,110.817299 -7.969460,110.800299 -7.975460,110.787861 -7.968379,110.760095 -7.992058)', 4326)));
INSERT INTO tc2 VALUES(6, intervalregion(400, 500, ST_Polygon('LINESTRING(110.760095 -7.992058, 110.701343 -7.983140, 110.777474 -8.015019,110.817299 -7.969460,110.800299 -7.975460,110.787861 -7.968379,110.760095 -7.992058)', 4326), ST_GeomFromText('POINT(110.890968 -7.914784 )', 4326)));

-- to data_mregion table
UPDATE data_mregion SET the_mregion = something FROM (SELECT append_intervalregion(tc2.inv) AS something FROM tc2 WHERE tc2.id=1) as som WHERE data_mregion.id = 5;
UPDATE data_mregion SET the_mregion = something FROM (SELECT append_intervalregion(d.the_mregion, tc2.inv) AS something FROM tc2, data_mregion AS d WHERE tc2.id=2 AND d.id=5) AS som WHERE data_mregion.id = 5;
UPDATE data_mregion SET the_mregion = something FROM (SELECT append_intervalregion(d.the_mregion, tc2.inv) AS something FROM tc2, data_mregion AS d WHERE tc2.id=3 AND d.id=5) AS som WHERE data_mregion.id = 5;
UPDATE data_mregion SET the_mregion = something FROM (SELECT append_intervalregion(d.the_mregion, tc2.inv) AS something FROM tc2, data_mregion AS d WHERE tc2.id=4 AND d.id=5) AS som WHERE data_mregion.id = 5;
UPDATE data_mregion SET the_mregion = something FROM (SELECT append_intervalregion(d.the_mregion, tc2.inv) AS something FROM tc2, data_mregion AS d WHERE tc2.id=5 AND d.id=5) AS som WHERE data_mregion.id = 5;
UPDATE data_mregion SET the_mregion = something FROM (SELECT append_intervalregion(d.the_mregion, tc2.inv) AS something FROM tc2, data_mregion AS d WHERE tc2.id=6 AND d.id=5) AS som WHERE data_mregion.id = 5;

-- TO CHECK IF INSERTION WENT WELL
SELECT ST_Union(get_snapshots(the_mregion)) FROM data_mregion where id = 5;
SELECT traversed(the_mregion) FROM data_mregion where id = 5;

-- TEST COMMANDS FOR: PROJECTION OPERATIONS
-- deftime
SELECT deftime(the_mregion) 
FROM data_mregion 
WHERE id = 5;

-- traversed
SELECT traversed(the_mregion) 
FROM data_mregion 
WHERE id = 5;

-- inst
SELECT inst(atinstant(the_mregion,257)) 
FROM data_mregion 
WHERE id = 5;

-- val
SELECT val(atinstant(the_mregion,257)) 
FROM data_mregion 
WHERE id = 5;

-- TEST COMMANDS FOR: INTERACTION OPERATIONS
-- atinstant
SELECT atinstant(the_mregion,257) 
FROM data_mregion 
WHERE id = 5;

-- atperiods
SELECT atperiods(the_mregion, period(200,365)) 
FROM data_mregion 
WHERE id = 5;

SELECT traversed(atperiods(the_mregion, period(200,365))) 
FROM data_mregion 
WHERE id = 5;

-- passes
SELECT p.name 
FROM province p, data_mregion m
WHERE p.admin = 'Indonesia' 
AND m.id = 5 
AND passes(m.the_mregion, p.geom);

-- at
SELECT at(m.the_mregion, p.geom)
FROM province p, data_mregion m
WHERE p.name = 'Yogyakarta' 
AND m.id = 5;

SELECT deftime(m.the_mregion, p.geom)
FROM province p, data_mregion m
WHERE p.name = 'Yogyakarta' 
AND m.id = 5;

-- initial/ final
SELECT val(initial(at(m.the_mregion, p.geom)))
FROM province p, data_mregion m
WHERE p.name = 'Yogyakarta' 
AND m.id = 5;

SELECT final(at(m.the_mregion, p.geom))
FROM province p, data_mregion m
WHERE p.name = 'Yogyakarta' 
AND m.id = 5;

-- atmin/ atmax
SELECT atmax(the_mregion)
FROM data_mregion 
WHERE id = 5;

SELECT deftime(atmax(the_mregion)) 
FROM data_mregion 
WHERE id = 5;

SELECT traversed(atmax(the_mregion)) 
FROM data_mregion 
WHERE id = 5;

-- present
SELECT deftime(the_mregion) 
FROM data_mregion;

SELECT * 
FROM data_mregion 
WHERE present(the_mregion, 350);