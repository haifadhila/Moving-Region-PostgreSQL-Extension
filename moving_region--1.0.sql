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
    SELECT create_msegments(src_time, dest_time, src_reg, dest_reg) INTO the_intervalregion.moving_segments;
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

-- FUNGSI CREATE MSEGMENTS
CREATE OR REPLACE FUNCTION public.create_msegments(src_time float, dest_time float, src_reg geometry, dest_reg geometry)
RETURNS msegment[] AS $$
DECLARE the_msegments msegment[];
BEGIN
    SELECT interpolate_convex_simple(src_time, dest_time, src_reg, dest_reg) INTO the_msegments;
    return the_msegments;
END
$$ LANGUAGE plpgsql;

-------------------------------------------------------------------------
------------------------------- HELPERS ---------------------------------
-------------------------------------------------------------------------

-- FUNGSI PROGRESS ANGLE
-- SELECT calculate_progress_angle(ST_GeomFromText('LINESTRING(3 3,0 0)', 4326))
CREATE OR REPLACE FUNCTION public.calculate_progress_angle(seg segment)
RETURNS FLOAT AS $$
DECLARE 
	progress_angle float;
	primary_endpoint geometry(POINT, 4326);
	secondary_endpoint geometry(POINT, 4326);
BEGIN
    -- progress angle calculation algorithm here
    /* angle formed by rotating an imaginary ray shooting in 
    the negative y direction counterclockwise around the primary 
    endpoint of s until its collinear and overlapping with s */
    SELECT ST_PointN(seg, 1) INTO primary_endpoint;
    SELECT ST_PointN(seg, 2) INTO secondary_endpoint;
    SELECT 360 - degrees(ST_Azimuth(secondary_endpoint, primary_endpoint)) INTO progress_angle;
    return progress_angle;
END
$$ LANGUAGE plpgsql;

-- FUNGSI GET SEGMENT KE N DARI SIMPLE CONVEX REGION
-- SELECT ST_AsText(get_segment_n(1, (ST_Polygon('LINESTRING(0 0, 77 29, 30 32, 0 0)', 4326))))
CREATE OR REPLACE FUNCTION public.get_segment_n(n integer, the_region cycle)
RETURNS segment AS $$
DECLARE 
    segmentN segment;
BEGIN
    SELECT ST_MakeLine(ST_PointN(ST_Boundary(the_region),n) , ST_PointN(ST_Boundary(the_region),n+1) )
    INTO segmentN;
    RETURN segmentN;
END
$$ LANGUAGE plpgsql;

-- FUNGSI CREATE 3D POINT FROM A 2D POINT
-- SELECT create_3d_point(ST_GeomFromText('POINT(2 2)',4326),100)
CREATE OR REPLACE FUNCTION public.create_3d_point(xy_index Point2D, z_index float)
RETURNS Point3D AS $$
DECLARE point_3d Point3D;
BEGIN
    SELECT ST_SetSRID(ST_MakePoint(ST_X(xy_index),ST_Y(xy_index),z_index),4326) INTO point_3d;
    RETURN point_3d;
END
$$ LANGUAGE plpgsql;

-- FUNGSI CREATE MSEGMENT FROM POINT AND SEGMENT
-- SELECT create_msegment1(ST_GeomFromText('LINESTRING(3 3,0 0)', 4326), 0, ST_GeomFromText('POINT(2 2)',4326), 5)
CREATE OR REPLACE FUNCTION public.create_msegment1(seg segment, z_index_seg float, p Point2D, z_index_p float)
RETURNS msegment AS $$
    SELECT  msegment(
                create_3d_point(ST_PointN(seg,1), z_index_seg),
                create_3d_point(ST_PointN(seg,2), z_index_seg),
                create_3d_point(p, z_index_p)
            );
$$ LANGUAGE SQL;

-- FUNGSI ST_AsText UNTUK MSEGMENT
CREATE OR REPLACE FUNCTION public.ST_AsText(mseg msegment)
RETURNS text AS $$
BEGIN
    RETURN CONCAT('(',ST_AsText(mseg.mseg_a),',',ST_AsText(mseg.mseg_b),',',ST_AsText(mseg.mseg_c),')');
END
$$ LANGUAGE plpgsql;

-- FUNGSI INTERPOLASI CONVEX SIMPLE REGIONS
-- SELECT interpolate_convex_simple(1, 100, ST_Polygon('LINESTRING(0 0, 16 0, 8 8, 0 0)', 4326), ST_Polygon('LINESTRING(0 0, 8 0, 4 4, 0 0)', 4326))
-- select interpolate_convex_simple(1, 100, ST_Polygon('LINESTRING(0 0, 4 -2, 8 0, 8 8, 4 10, 0 8, 0 0)', 4326), ST_Polygon('LINESTRING(0 0, 8 0, 8 8, 0 8, 0 0)', 4326))
CREATE OR REPLACE FUNCTION public.interpolate_convex_simple(src_time float, dest_time float, src_reg geometry, dest_reg geometry)
RETURNS msegment[] AS $$
DECLARE 
    the_msegments msegment[];
    r_current segment;  d_current segment;
    r_last segment;     d_last segment;
    r_prev segment;     d_prev segment;
    r_angle float;      d_angle float;
    rn integer;        	dn integer;     an integer;
BEGIN
    -- Interpolation algorithm here
    -- find least segments s from src reg and d from dest reg (idx = 0)
    -- assign initial values
    rn = 1;
    dn = 1;
    an = 1;
    SELECT  get_segment_n(rn,src_reg), 
            get_segment_n(-2,src_reg),
            get_segment_n(dn,dest_reg), 
            get_segment_n(-2,dest_reg)
    INTO r_current, r_last, d_current, d_last;

    SELECT ST_GeomFromText('LINESTRING(0 0, 0 0)', 4326) into r_prev;
    SELECT ST_GeomFromText('LINESTRING(0 0, 0 0)', 4326) into d_prev;

    SELECT  calculate_progress_angle(r_current), 
            calculate_progress_angle(d_current)
    INTO r_angle, d_angle;

    -- begin for loop to create delta triangles (moving segments)
    LOOP
        IF ((NOT ST_Equals(d_current,d_prev)) AND (NOT ST_Equals(r_current,r_prev))) THEN
            IF (r_angle < d_angle) THEN
                -- append a moving segment: r to d's primary point
                SELECT array_append(the_msegments, create_msegment1(r_current, src_time, ST_PointN(d_current,1), dest_time)) into the_msegments;
                RAISE NOTICE 'msegment added: %',ST_AsText(the_msegments[an]);
                an = an +1;
                -- append a moving segment: d to r's secondary point
                SELECT array_append(the_msegments, create_msegment1(d_current, dest_time, ST_PointN(r_current,2), src_time)) into the_msegments;
                RAISE NOTICE 'msegment added: %',ST_AsText(the_msegments[an]);
                an = an +1;
            ELSE
                -- append a moving segment: d to r's primary point
                SELECT array_append(the_msegments, create_msegment1(d_current, dest_time, ST_PointN(r_current,1), src_time)) into the_msegments;
                RAISE NOTICE 'msegment added: %',ST_AsText(the_msegments[an]);
                an = an +1;
                -- append a moving segment: r to d's secondary point
                SELECT array_append(the_msegments, create_msegment1(r_current, src_time, ST_PointN(d_current,2), dest_time)) into the_msegments;
                RAISE NOTICE 'msegment added: %',ST_AsText(the_msegments[an]);
                an = an +1;
            END IF;
        ELSE
            IF (NOT ST_Equals(d_current, d_prev)) THEN
                -- append a moving segment: d to r's secondary point
                SELECT array_append(the_msegments, create_msegment1(d_current, dest_time, ST_PointN(r_current,2), src_time)) into the_msegments;
                RAISE NOTICE 'msegment added: %',ST_AsText(the_msegments[an]);
                an = an +1;
            END IF;

            IF (NOT ST_Equals(r_current, r_prev)) THEN
                -- append a moving segment: r to d's secondary point
                SELECT array_append(the_msegments, create_msegment1(r_current, src_time, ST_PointN(d_current,2), dest_time)) into the_msegments;
                RAISE NOTICE 'msegment added: %',ST_AsText(the_msegments[an]);
                an = an +1;
            END IF;
        END IF;

        -- proceed to the next segment in the region
        r_prev = r_current;
        d_prev = d_current;
        SELECT calculate_progress_angle(get_segment_n(rn+1,src_reg)) INTO r_angle;
        SELECT calculate_progress_angle(get_segment_n(dn+1,dest_reg)) INTO d_angle;
        
        IF (NOT ST_Equals(r_current, r_last)) AND (r_angle < d_angle OR ST_Equals(d_current, d_last)) THEN
            rn = rn + 1;
            SELECT  get_segment_n(rn,src_reg) INTO r_current;
        ELSIF NOT ST_Equals(d_current, d_last) THEN
            dn = dn + 1;
            SELECT  get_segment_n(dn,dest_reg) INTO d_current;
        END IF;

    -- Exit loop when reach the end of each region
    EXIT WHEN ( ST_Equals(r_current,r_last) AND ST_Equals(r_current,r_prev) AND ST_Equals(d_current,d_last) AND ST_Equals(d_current,d_prev) );
    END LOOP;
    RETURN the_msegments;
END
$$ LANGUAGE plpgsql;
