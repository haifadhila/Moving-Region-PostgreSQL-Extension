--complain if script is sourced in psql, rather than via CREATE EXTENSION

-- /usr/local/share/postgresql/extension/moving_region
\echo Use "CREATE EXTENSION moving_region" to load this file. \quit

-------------------------------------------------------------------------
-------------- DATA TYPE DECLARATIONS AND CREATE FUNCTIONS --------------
-------------------------------------------------------------------------

-- 2D POINT
CREATE DOMAIN Point2D geometry(POINT, 4326);

-- 3D POINT
CREATE DOMAIN Point3D geometry(POINTZ, 4326);

-- SEGMENT
-- CREATE TABLE tes1(idd serial primary key, the_segment segment);
-- insert into tes1(the_segment) values(ST_GeomFromText('LINESTRING(-71 42,-71 43)', 4326));
-- Kasih syarat cuma bisa 2 titik membentuk linestring ini
CREATE DOMAIN segment geometry(LINESTRING, 4326);

-- MOVING SEGMENT
-- CREATE TABLE tes2(idd serial primary key, the_msegment msegment);
-- insert into tes2(the_msegment) values(msegment(ST_GeomFromText('POINT(0 0 0)',4326),ST_GeomFromText('POINT(0 2 3)',4326), ST_GeomFromText('POINT(1 4 3)',4326)));
CREATE TYPE msegment AS (
    mseg_a Point3D,
    mseg_b Point3D,
    mseg_c Point3D
); 

CREATE OR REPLACE FUNCTION public.msegment(a Point3D, b Point3D, c Point3D)
RETURNS msegment AS $$
DECLARE the_msegment msegment;
BEGIN
    the_msegment.mseg_a := a;
    the_msegment.mseg_b := b;
    the_msegment.mseg_c := c;
    RETURN the_msegment;
END
$$ LANGUAGE plpgsql;


-- CYCLE
-- CREATE TABLE tes3(idd serial primary key, the_cycle cycle);
-- insert into tes3(the_cycle) values(ST_Polygon('LINESTRING(0 0, 77 29, 30 32, 0 0)', 4326));
CREATE DOMAIN cycle geometry(POLYGON, 4326);

-- REGION
-- CREATE TABLE tes4(idd serial primary key, the_region region);
-- insert into tes4(the_region) values(ST_GeometryFromText('MULTIPOLYGON(((0 0,4 0,4 4,0 4,0 0),(1 1,2 1,2 2,1 2,1 1)), ((-1 -1,-1 -2,-2 -2,-2 -1,-1 -1)))', 4326))
CREATE DOMAIN region geometry(MULTIPOLYGON, 4326);

-- INTERVAL REGION
-- CREATE TABLE tes5(idd serial primary key, the_intervalregion intervalregion);
-- insert into tes5(the_intervalregion) values(intervalregion(1,100,(ST_Polygon('LINESTRING(0 0, 4 -2, 8 0, 8 8, 4 10, 0 8, 0 0)', 4326)),(ST_Polygon('LINESTRING(0 0, 8 0, 8 8, 0 8, 0 0)', 4326))));
CREATE TYPE intervalregion AS (
    src_time float,
    dest_time float,
    src_reg geometry,
    dest_reg geometry,
    moving_segments msegment[]
);

CREATE OR REPLACE FUNCTION public.intervalregion(src_time float, dest_time float, src_reg geometry, dest_reg geometry)
RETURNS intervalregion AS $$
DECLARE the_intervalregion intervalregion;
BEGIN
    the_intervalregion.src_time = src_time;
    the_intervalregion.dest_time = dest_time;
    the_intervalregion.src_reg = src_reg;
    the_intervalregion.dest_reg = dest_reg;
   -- SELECT create_msegments(src_time, dest_time, src_reg, dest_reg) INTO the_intervalregion.moving_segments;
    RETURN the_intervalregion;
END
$$ LANGUAGE plpgsql;

-- MOVING REGION
CREATE TYPE mregion AS (
    mreg_id int,
    interval_regions intervalregion[]
);

CREATE OR REPLACE FUNCTION public.msegment(mreg_id int, interval_regions intervalregion[])
RETURNS mregion AS $$
DECLARE the_mregion mregion;
BEGIN
    the_mregion.mreg_id := mreg_id;
    the_mregion.interval_regions := interval_regions;
    RETURN the_mregion;
END
$$ LANGUAGE plpgsql;

-------------------------------------------------------------------------
----------------------- FUNCTIONS AND PROCEDURES ------------------------
-------------------------------------------------------------------------

-- FUNGSI A
CREATE OR REPLACE FUNCTION public.MR(the_line geometry)
RETURNS FLOAT AS $$
DECLARE the_length FLOAT;
BEGIN
    SELECT ST_Length(the_line) INTO the_length;
    RETURN the_length;
END
$$ LANGUAGE plpgsql;

-- FUNGSI B
CREATE OR REPLACE FUNCTION public.MR2(the_line geometry)
RETURNS FLOAT AS $$
DECLARE the_length FLOAT;
BEGIN
    SELECT ST_Length(the_line) INTO the_length;
    RETURN the_length+20;
END
$$ LANGUAGE plpgsql;