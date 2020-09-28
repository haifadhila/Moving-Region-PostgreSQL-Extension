-- SQL commands to test moving region extension
CREATE TABLE data_mregion(
	id integer primary key,
	name VARCHAR(50),
	the_mregion mregion
)



select traversed(the_mregion) from data_mregion where id=4

SELECT val(atinstant(atperiods(the_mregion, period(200,400)),368)) from data_mregion

SELECT p.name 
FROM province p, data_mregion m
WHERE p.admin = 'Indonesia' 
AND m.id = 4 
AND passes(atperiods(m.the_mregion, period(200,400)), p.geom)

SELECT p.name 
FROM province p, data_mregion m
WHERE p.admin = 'Indonesia' 
AND m.id = 4 
AND passes(m.the_mregion, p.geom)


SELECT traversed(at(atperiods(m.the_mregion, period(200,400)), p.geom))
FROM province p, data_mregion m
WHERE p.name = 'Yogyakarta' 
AND m.id = 4 

SELECT deftime(at(atperiods(m.the_mregion, period(200,400)), p.geom))
FROM province p, data_mregion m
WHERE p.name = 'Yogyakarta' 
AND m.id = 4 

SELECT traversed(at(m.the_mregion, p.geom))
FROM province p, data_mregion m
WHERE p.name = 'Yogyakarta' 
AND m.id = 4 


SELECT val(atinstant(atperiods(the_mregion, period(200,400)),368)) from data_mregion

SELECT (val(atinstant(atperiods(the_mregion, period(45,99)),45))) from data_mregion where id=4;
--SELECT (val(atinstant(the_mregion,245))) from data_mregion where id=4
