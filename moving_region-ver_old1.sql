--complain if script is sourced in psql, rather than via CREATE EXTENSION
\echo Use "CREATE EXTENSION moving_region" to load this file. \quit

-- DATA TYPE DECLARATIONS AND CREATE FUNCTIONS

-- SEGMENT
-- insert into a values(1,segment(1,ST_GeomFromText('POINT(0 0)',4362),ST_GeomFromText('POINT(0 2)',4362)));
CREATE TYPE segment AS (
    seg_id int,
    seg_p geometry(POINT, 4362),
    seg_q geometry(POINT, 4362)
); 

CREATE OR REPLACE FUNCTION public.segment(id int, p geometry, q geometry)
RETURNS segment AS $$
DECLARE the_segment segment;
BEGIN
    the_segment.seg_id = id;
    the_segment.seg_p = p;
    the_segment.seg_q = q;
    RETURN the_segment;
END
$$ LANGUAGE plpgsql;

-- MOVING SEGMENT
--insert into test_segment values(1,msegment(1,ST_GeomFromText('POINT(0 0 0)',4362),ST_GeomFromText('POINT(0 2 3)',4362), ST_GeomFromText('POINT(1 4 3)',4362)));
CREATE TYPE msegment AS (
    mseg_id int,
    mseg_a geometry(POINTZ, 4362),
    mseg_b geometry(POINTZ, 4362),
    mseg_c geometry(POINTZ, 4362)
); 

CREATE OR REPLACE FUNCTION public.msegment(id int, a geometry, b geometry, c geometry)
RETURNS msegment AS $$
DECLARE the_msegment msegment;
BEGIN
    the_msegment.mseg_id = id;
    the_msegment.mseg_a = a;
    the_msegment.mseg_b = b;
    the_msegment.mseg_c = c;
    RETURN the_msegment;
END
$$ LANGUAGE plpgsql;

-- CYCLE
CREATE TYPE cycle AS (
    cycle_id int,
    cycle_elmt segment[]
);

CREATE OR REPLACE FUNCTION public.cycle(id int, elmt segment[])
RETURNS cycle AS $$
DECLARE the_cycle cycle;
BEGIN
    the_cycle.cycle_id = id;
    the_cycle.cycle_elmt = elmt;
    RETURN the_cycle;
END
$$ LANGUAGE plpgsql;

-- INTERVAL REGION
CREATE TYPE intervalregion AS (
    intvlreg_id int,
    src_time float,
    dest_time float,
    src_region geometry(POLYGON, 4362),
    dest_region geometry(POLYGON, 4362),
    moving_segments msegment[]
); 

-- MOVING REGION
CREATE TYPE mregion AS (
    mreg_id int,
    interval_regions int[]
); 

CREATE OR REPLACE FUNCTION public.msegment(id int, a geometry, b geometry, c geometry)
RETURNS msegment AS $$
DECLARE the_msegment msegment;
BEGIN
    the_msegment.mseg_id = id;
    the_msegment.mseg_a = a;
    the_msegment.mseg_b = b;
    the_msegment.mseg_c = c;
    RETURN the_msegment;
END
$$ LANGUAGE plpgsql;





-- FUNCTIONS AND PROCEDURES
CREATE OR REPLACE FUNCTION public.MR(the_line geometry)
RETURNS FLOAT AS $$
DECLARE the_length FLOAT;
BEGIN
    SELECT ST_Length(the_line) INTO the_length;
    RETURN the_length;
END
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.MR2(the_line geometry)
RETURNS FLOAT AS $$
DECLARE the_length FLOAT;
BEGIN
    SELECT ST_Length(the_line) INTO the_length;
    RETURN the_length+20;
END
$$ LANGUAGE plpgsql;


-- FUNGSI CREATE MSEGMENT FROM POINT AND SEGMENT
-- SELECT create_msegment_custom(ST_GeomFromText('LINESTRING(3 3,0 0)', 4326), 0, ST_GeomFromText('POINT(2 2)',4326), 5)
CREATE OR REPLACE FUNCTION public.create_msegment_custom(seg segment, z_index_seg float, p Point2D, z_index_p float)
RETURNS msegment AS $$
DECLARE the_mseg msegment;
BEGIN
    drop table tesmm;
    CREATE TABLE tesmm(idd serial primary key, the_msegment msegment);
    insert into tesmm(the_msegment) 
    SELECT  msegment(
                create_3d_point(ST_PointN(seg,1), z_index_seg),
                create_3d_point(ST_PointN(seg,2), z_index_seg),
                create_3d_point(p, z_index_p)
            );
    RAISE NOTICE 'OMG SAMPE G YYY';
    SELECT the_msegment.msegment INTO the_mseg FROM tesmm WHERE idd=1;
    RETURN the_mseg;
END
$$ LANGUAGE plpgsql;


-- FUNGSI INTERPOLASI CONVEX SIMPLE REGIONS
-- SELECT interpolate_convex_simple(1, 100, ST_Polygon('LINESTRING(0 0, 16 0, 8 8, 0 0)', 4326), ST_Polygon('LINESTRING(0 0, 8 0, 4 4, 0 0)', 4326))
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
            RAISE NOTICE 'mashook if pertama';
            IF (r_angle < d_angle) THEN
                RAISE NOTICE 'mashook if pertama terus if kedua';
                -- append a moving segment: r to d's primary point
                SELECT array_append(the_msegments, create_msegment1(r_current, src_time, ST_PointN(d_current,1), dest_time)) into the_msegments;
                RAISE NOTICE 'this: %',ST_AsText(the_msegments[an]);
                an = an +1;
                RAISE NOTICE 'SUDAH APPEND % KALI', an-1;
                -- append a moving segment: d to r's secondary point
                SELECT array_append(the_msegments, create_msegment1(d_current, dest_time, ST_PointN(r_current,2), src_time)) into the_msegments;
                RAISE NOTICE 'this: %',ST_AsText(the_msegments[an]);
                an = an +1;
                RAISE NOTICE 'SUDAH APPEND % KALI', an-1;
            ELSE
                RAISE NOTICE 'mashook if pertama terus else';
                -- append a moving segment: d to r's primary point
                SELECT array_append(the_msegments, create_msegment1(d_current, dest_time, ST_PointN(r_current,1), src_time)) into the_msegments;
                RAISE NOTICE 'this: %',ST_AsText(the_msegments[an]);
                an = an +1;
                RAISE NOTICE 'SUDAH APPEND % KALI', an-1;
                -- append a moving segment: r to d's secondary point
                SELECT array_append(the_msegments, create_msegment1(r_current, src_time, ST_PointN(d_current,2), dest_time)) into the_msegments;
                RAISE NOTICE 'this: %',ST_AsText(the_msegments[an]);
                an = an +1;
                RAISE NOTICE 'SUDAH APPEND % KALI', an-1;
            END IF;
        ELSE
            RAISE NOTICE 'mashook else';
            IF (NOT ST_Equals(d_current, d_prev)) THEN
                RAISE NOTICE 'mashook else pertama terus if yang d';
                -- append a moving segment: d to r's secondary point
                SELECT array_append(the_msegments, create_msegment1(d_current, dest_time, ST_PointN(r_current,2), src_time)) into the_msegments;
                RAISE NOTICE 'this: %',ST_AsText(the_msegments[an]);
                an = an +1;
                RAISE NOTICE 'SUDAH APPEND % KALI', an-1;
            END IF;

            IF (NOT ST_Equals(r_current, r_prev)) THEN
                RAISE NOTICE 'mashook else pertama terus if yang r';
                -- append a moving segment: r to d's secondary point
                SELECT array_append(the_msegments, create_msegment1(r_current, src_time, ST_PointN(d_current,2), dest_time)) into the_msegments;
                RAISE NOTICE 'this: %',ST_AsText(the_msegments[an]);
                an = an +1;
                RAISE NOTICE 'SUDAH APPEND % KALI', an-1;
            END IF;
        END IF;

        -- proceed to the next segment in the region
        r_prev = r_current;
        d_prev = d_current;
        RAISE NOTICE 'VAR r_prev %', ST_AsText(r_prev);
        RAISE NOTICE 'VAR d_prev %', ST_AsText(d_prev);
        SELECT calculate_progress_angle(get_segment_n(rn+1,src_reg)) INTO r_angle;
        SELECT calculate_progress_angle(get_segment_n(dn+1,dest_reg)) INTO d_angle;

        RAISE NOTICE 'VAR r_angle %', r_angle;
        RAISE NOTICE 'VAR d_angle %', d_angle;
        RAISE NOTICE 'last 1';
        
        IF (NOT ST_Equals(r_current, r_last)) AND (r_angle < d_angle OR ST_Equals(d_current, d_last)) THEN
            rn = rn + 1;
            SELECT  get_segment_n(rn,src_reg) INTO r_current;
            RAISE NOTICE 'last 2';
        ELSIF NOT ST_Equals(d_current, d_last) THEN
            dn = dn + 1;
            SELECT  get_segment_n(dn,dest_reg) INTO d_current;
            RAISE NOTICE 'last 3';
        END IF;

    -- Exit loop when reach the end of each region
    EXIT WHEN ( ST_Equals(r_current,r_last) AND ST_Equals(r_current,r_prev) AND ST_Equals(d_current,d_last) AND ST_Equals(d_current,d_prev) );
    END LOOP;
    RAISE NOTICE 'LAST';
    RETURN the_msegments;
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

-- MOVING REGION
-- CREATE TABLE tes5(idd serial primary key, the_mregion mregion);
/* update tes_mreg 
SET the_mregion = (1,something)
FROM (Select append_intervalregion(the_intervalregion) as something from test_intvl where idd=1) as som
*/
CREATE TYPE mregion AS (
    mreg_id int,
    interval_regions intervalregion[]
);

CREATE OR REPLACE FUNCTION public.mregion(mreg_id int, interval_regions intervalregion[])
RETURNS mregion AS $$
DECLARE the_mregion mregion;
BEGIN
    the_mregion.mreg_id := mreg_id;
    the_mregion.interval_regions := interval_regions;
    RETURN the_mregion;
END
$$ LANGUAGE plpgsql;


-- COMPARE ORIGINAL ARRAY AND CONVEX HULL ARRAY. RETURN: NEW ORIGINAL PG_SEGMENT
CREATE OR REPLACE FUNCTION public.get_new_pg_ori(ori_seg segment[], chull pg_segment)
RETURNS float[] AS $$
DECLARE
    ori_pg float[];
    chull_seg segment[];
    chull_pg float[];
    idx integer;
    cidx integer;
BEGIN 
    RAISE NOTICE 'LEWAT SINI GAIS';
    
    SELECT get_arr_pg(chull), get_arr_segment(chull) INTO chull_pg, chull_seg;
    RAISE NOTICE 'pg chull: %', chull_pg;
    idx = 1;
    cidx = 1;
    LOOP
        IF (ST_Equals(ori_seg[idx], chull_seg[cidx])) THEN
            SELECT array_append(ori_pg, chull_pg[cidx]) INTO ori_pg;
            cidx = cidx + 1;
        ELSE
            SELECT array_append(ori_pg, chull_pg[cidx]) INTO ori_pg;
            IF (ST_Equals(ST_PointN(ori_seg[idx],2), ST_PointN(chull_seg[cidx],2))) THEN
                cidx = cidx + 1;
            END IF;
        END IF;
        idx = idx + 1;

    EXIT WHEN (idx > array_length(ori_seg,1));
    END LOOP;
    RAISE NOTICE 'pg ori: %', ori_pg;
    RETURN ori_pg;
END
$$ LANGUAGE plpgsql;






ALTER DATABASE semangat SET postgis.backend = sfcgal;
CREATE EXTENSION postgis_sfcgal; 

SELECT ST_AsText(ST_3DIntersection(ST_POLYGON('LINESTRING(0 0 1, 16 0 1, 8 0 100, 0 0 1)', 4326),ST_POLYGON('LINESTRING(0 0 50, 0 200 50, 200 200 50, 200 0 50, 0 0 50)',4326)));
SELECT ST_POLYGON('LINESTRING(-400 -400, 400 -400, 400 400, -400 400, -400 -400)',4326);

SELECT ST_AsText(ST_3DIntersection(ST_POLYGON('LINESTRING(0 0 1, 4 -2 1, 0 0 100, 0 0 1)', 4326),ST_POLYGON('LINESTRING(-400 -400 5, 400 -400 5, 400 400 5, -400 400 5, -400 -400 5)',4326)));

-- convex convex tc1
-- tc2 nya dia lanjut ke 2 array of intvlregion
select val(atinstant(atperiods(mregion(intervalregion(1,100,(ST_Polygon('LINESTRING(0 0, 4 -2, 8 0, 8 8, 4 10, 0 8, 0 0)', 4326)),(ST_Polygon('LINESTRING(0 0, 8 0, 8 8, 0 8, 0 0)', 4326)))), period(70,100)), 79))

-- convex nonconvex tc3
select val(atinstant(atperiods(mregion(intervalregion(1,100,(ST_Polygon('LINESTRING(0 0, 4 2, 8 0, 8 8, 4 10, 0 8, 0 0)', 4326)),(ST_Polygon('LINESTRING(0 0, 8 0, 8 8, 0 8, 0 0)', 4326)))), period(70,100)), 79))

-- nonconvex nonconvex tc3
select mregion(intervalregion(1,100,(ST_Polygon('LINESTRING(0 0, 4 2, 8 0, 8 8, 4 10, 0 8, 0 0)', 4326)),(ST_Polygon('LINESTRING(0 0, 8 0, 8 4, 4 4, 4 8, 0 8, 0 0)', 4326))))

-- nonconvex nonconvex tc3
mregion(intervalregion(1,100,(ST_Polygon('LINESTRING(0 0, 4 2, 8 0, 8 8, 4 10, 0 8, 0 0)', 4326)),(ST_Polygon('LINESTRING(0 0, 6 0, 6 2, 4 2, 4 4, 2 4, 2 6, 0 6, 0 0)', 4326))))

-- tes 3dintersection >:(
SELECT ST_AsText(ST_3DIntersection(ST_GeomFromText('LINESTRING(8 0 100,13 5 200)', 4326),ST_POLYGON('LINESTRING(-400 -400 151, 400 -400 151, 400 400 151, -400 400 151, -400 -400 151)',4326)));

-- tes fungsi fungsi
select ST_Equals(val(initial(atperiods(the_mregion, period(34, 79)))), val(atinstant(the_mregion, 34))) from tes5 where idd=2




UPDATE data_mregion
SET the_mregion = something
FROM 
(Select append_intervalregion(the_mregion::intervalregion[], 
							 intervalregion(100,200,(ST_Polygon('LINESTRING(0 0, 8 0, 8 8, 0 8, 0 0)', 4326)),(ST_Polygon('LINESTRING(5 5, 13 5, 13 13, 5 13, 5 5)', 4326)))) 
 as something from data_mregion where id=2) as som


 CREATE TABLE data_mregion(
	id integer primary key,
	name VARCHAR(50),
	the_mregion mregion
)


INSERT INTO data_mregion
VALUES(3,
	  'convex-convex-2',
	  mregion(intervalregion(1,100,(ST_Polygon('LINESTRING(0 0, 4 -2, 8 0, 8 8, 4 10, 0 8, 0 0)', 4326)),(ST_Polygon('LINESTRING(0 0, 8 0, 8 8, 0 8, 0 0)', 4326)))))

CREATE TABLE tc1(
	id integer primary key,
	inv intervalregion
)



INSERT INTO tc1
VALUES(2,
	  intervalregion(100, 200, ST_Polygon('LINESTRING(
109.89 -7.577,
109.880 -7.576,
109.870 -7.564,
109.8615 -7.565,
109.859 -7.596,
109.862 -7.602,
109.878 -7.620,
109.915 -7.630,
109.915 -7.577, 
109.90 -7.565, 
109.89 -7.577
)', 
4326),  ST_Polygon('LINESTRING(
110.112802 -7.620034, 
110.064020 -7.653366, 
 110.065498 -7.716727, 
110.131650 -7.774951,
110.190042 -7.740164,
110.185237 -7.695852,
110.112802  -7.620034
)', 
4326)))


update data_mregion
SET the_mregion = something
FROM (Select append_intervalregion(d.the_mregion, tc1.inv) as something from tc1, data_mregion as d where tc1.id=4 and d.id=4) as som
WHERE data_mregion.id = 4

update data_mregion
SET the_mregion = something
FROM (Select append_intervalregion(tc1.inv) as something from tc1 where tc1.id=12) as som
WHERE data_mregion.id = 4


select traversed((mregion(intervalregion(100, 200,ST_Polygon('LINESTRING(
110.062802 -7.436366, 
110.014020 -7.453366, 
110.015498 -7.516727, 
110.081650 -7.574951,
110.140042 -7.540164,
110.135237 -7.495852,
110.095237 -7.495852,
110.062802  -7.436366
)', 
4326),  ST_Polygon('LINESTRING(
110.112802 -7.620034, 
110.064020 -7.653366, 
110.065498 -7.716727, 
110.131650 -7.774951,
110.190042 -7.740164,
110.185237 -7.695852,
110.112802  -7.620034
)', 
4326)))))



select traversed(the_mregion) from data_mregion where id=4

SELECT val(atinstant(atperiods(the_mregion, period(200,400)),368)) from data_mregion

SELECT p.name 
FROM province1 p, data_mregion m
WHERE p.admin = 'Indonesia' 
AND m.id = 4 
AND passes(atperiods(m.the_mregion, period(200,400)), p.geom)


SELECT traversed(at(atperiods(m.the_mregion, period(200,400)), p.geom))
FROM province1 p, data_mregion m
WHERE p.name = 'Yogyakarta' 
AND m.id = 4 

SELECT deftime(at(atperiods(m.the_mregion, period(200,400)), p.geom))
FROM province1 p, data_mregion m
WHERE p.name = 'Yogyakarta' 
AND m.id = 4 


SELECT val(atinstant(atperiods(the_mregion, period(200,400)),368)) from data_mregion




-- DEFTIME
-- DEFTIME
CREATE OR REPLACE FUNCTION public.deftime(mreg mregion)
RETURNS period AS $$
BEGIN
    SELECT get_src_time(mreg[1]) INTO tstart;
    SELECT get_dest_time(mreg[array_length(mreg, 1)]) INTO tend;
    RETURN period(tstart,tend);
END
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.to_ts(per period)
RETURNS period_tsz AS $$
DECLARE
    tstart timestamptz;
    tend timestamptz;
BEGIN
    SELECT to_timestamp(get_tstart(per)) INTO tstart;
    SELECT to_timestamp(get_tend(per)) INTO tend;
    RETURN period_tsz(tstart,tend);
END
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.to_timestamp(per period[])
RETURNS period_tsz[] AS $$
DECLARE
    tstart timestamptz;
    tend timestamptz;
    arr period_tsz[];
	p period;
BEGIN
    FOREACH p IN ARRAY per
    LOOP
        SELECT to_timestamp(get_tstart(p)) INTO tstart;
        SELECT to_timestamp(get_tend(p)) INTO tend;
        SELECT array_append(arr, period_tsz(tstart,tend)) INTO arr;
    END LOOP;
    RETURN arr;
END
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.deftime_tsz(mreg mregion)
RETURNS period_tsz AS $$
DECLARE
    tstart timestamptz;
    tend timestamptz;
BEGIN
    SELECT to_timestamp(get_src_time(mreg[1])) INTO tstart;
    SELECT to_timestamp(get_dest_time(mreg[array_length(mreg, 1)])) INTO tend;
    RETURN period_tsz(tstart,tend);
END
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.deftime_ts(mreg mregion)
RETURNS period_ts AS $$
DECLARE
    tstart timestamp;
    tend timestamp;
BEGIN
    SELECT to_timestamp(get_src_time(mreg[1])) INTO tstart;
    SELECT to_timestamp(get_dest_time(mreg[array_length(mreg, 1)])) INTO tend;
    RETURN period_ts(tstart,tend);
END
$$ LANGUAGE plpgsql;
