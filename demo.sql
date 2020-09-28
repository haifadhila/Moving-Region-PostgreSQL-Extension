-- SQL commands to test moving region extension

CREATE EXTENSION moving_region;

-- TEST COMMANDS FOR: PROJECTION OPERATIONS
-- deftime
SELECT id, name, deftime1(storm)
FROM data_storm

SELECT id, name, to_timestamp(deftime1(storm))
FROM data_storm

-- traversed
SELECT traversed(the_mregion)
FROM data_mregion 
WHERE name='tc1'

SELECT name, traversed(rain)
FROM data_rain


-- TEST COMMANDS FOR: INTERACTION OPERATIONS
-- atinstant
SELECT atinstant(storm, ts_unix_id(2020,9,3,12,45,0))
FROM data_storm
WHERE name='NANA'

SELECT val(atinstant(storm, ts_unix_id(2020,9,3,12,45,0)))
FROM data_storm
WHERE name='NANA'

-- atperiods
SELECT name, atperiods(storm, day(2020,9,3))
FROM data_storm
WHERE name='NANA' OR name='OMAR'

SELECT traversed(atperiods(storm, day(2020,9,3)))
FROM data_storm
WHERE name='NANA' OR name='OMAR'


-- initial/ final
SELECT name, initial(atperiods(storm, day(2020,9,3)))
FROM data_storm
WHERE name='OMAR'

SELECT val(initial(atperiods(storm, day(2020,9,3))))
FROM data_storm
WHERE name='OMAR'

SELECT name, final(atperiods(storm, day(2020,9,3)))
FROM data_storm
WHERE name='OMAR'

SELECT val(final(atperiods(storm, day(2020,9,3))))
FROM data_storm
WHERE name='OMAR'

-- ADA TABEL PROVINSI!

-- passes & at

-- TC1 MREGION
SELECT p.name 
FROM province p, data_mregion m
WHERE p.admin = 'Indonesia' 
AND m.name= 'tc1'
AND passes(m.the_mregion, p.geom)


SELECT traversed(at(m.the_mregion, p.geom))
FROM province p, data_mregion m
WHERE p.name = 'Yogyakarta' 
AND m.name = 'tc1';

--TC3 STORM
SELECT DISTINCT p.admin 
FROM province p, data_storm s
WHERE s.name = 'NANA'
AND passes(s.storm, p.geom)

SELECT p.name, p.geom
FROM province p, data_storm s
WHERE s.name = 'NANA'
AND p.admin ='Jamaica'
AND passes(s.storm, p.geom)

SELECT to_timestamp(deftime1(at(s.storm, p.geom)))
FROM province p, data_storm s
WHERE s.name = 'NANA'
AND p.admin ='Jamaica'
AND p.name ='Clarendon'

-- present
SELECT name, to_timestamp(deftime1(rain))
FROM data_rain;

SELECT *
FROM data_rain
WHERE present(rain, ts_unix_id(2020,9,3,16,36,0))


-- atmin/ atmax
SELECT name, atmax(rain) 
FROM data_rain
WHERE name = 'rain_3';

SELECT traversed(atmax(rain))
FROM data_rain
WHERE name = 'rain_3';

SELECT name, atmin(rain) 
FROM data_rain
WHERE name = 'rain_3';

SELECT traversed(atmin(rain))
FROM data_rain
WHERE name = 'rain_3';


-- atmin and atmax STORMS
SELECT name, traversed(atmin(storm))
FROM data_storm
WHERE name = 'OMAR';

SELECT name, to_timestamp(deftime1(atmin(storm)))
FROM data_storm
WHERE name = 'OMAR';

SELECT name, traversed(atmax(storm))
FROM data_storm
WHERE name = 'OMAR';

SELECT name, to_timestamp(deftime1(atmax(storm)))
FROM data_storm
WHERE name = 'OMAR';

-- inst
SELECT inst(atinstant(the_mregion, 250))
FROM data_mregion 
WHERE name='tc1'

-- val
SELECT val(atinstant(the_mregion, 250))
FROM data_mregion 
WHERE name='tc1'

SELECT at(m.the_mregion, p.geom)
FROM province p, data_mregion m
WHERE p.name = 'Yogyakarta' 
AND m.name = 'tc1';