--complain if script is sourced in psql, rather than via CREATE EXTENSION

-- /usr/local/share/postgresql/extension/moving_region
\echo Use "CREATE EXTENSION moving_region" to load this file. \quit

-------------------------------------------------------------------------
-------------- DATA TYPE DECLARATIONS AND CONSTRUCTORS ------------------
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
-- CREATE TABLE test_intvl(idd serial primary key, the_intervalregion intervalregion);
-- insert into test_intvl(the_intervalregion) values(intervalregion(1,100,(ST_Polygon('LINESTRING(0 0, 4 -2, 8 0, 8 8, 4 10, 0 8, 0 0)', 4326)),(ST_Polygon('LINESTRING(0 0, 8 0, 8 8, 0 8, 0 0)', 4326))));
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
    IF (ST_GeometryType(src_reg) = 'ST_Point' AND ST_GeometryType(dest_reg) = 'ST_Point' AND src_time=dest_time) THEN
        the_intervalregion.moving_segments = NULL;
    ELSE
        SELECT create_msegments(src_time, dest_time, src_reg, dest_reg) INTO the_intervalregion.moving_segments;
    END IF;
    
    RETURN the_intervalregion;
END
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.intervalregion(intvlreg intervalregion, src_time float, dest_time float, src_reg geometry, dest_reg geometry)
RETURNS intervalregion AS $$
DECLARE the_intervalregion intervalregion;
BEGIN
    the_intervalregion.src_time = src_time;
    the_intervalregion.dest_time = dest_time;
    the_intervalregion.src_reg = src_reg;
    the_intervalregion.dest_reg = dest_reg;
    SELECT intvlreg.moving_segments INTO the_intervalregion.moving_segments;
    
    RETURN the_intervalregion;
END
$$ LANGUAGE plpgsql;

-- Getter for interval region attributes
CREATE OR REPLACE FUNCTION public.get_src_time(the_intvl intervalregion)
RETURNS float AS $$
    SELECT the_intvl.src_time;
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION public.get_dest_time(the_intvl intervalregion)
RETURNS float AS $$
    SELECT the_intvl.dest_time;
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION public.get_src_region(the_intvl intervalregion)
RETURNS geometry AS $$
    SELECT the_intvl.src_reg;
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION public.get_dest_region(the_intvl intervalregion)
RETURNS geometry AS $$
    SELECT the_intvl.dest_reg;
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION public.get_moving_segments(the_intvl intervalregion)
RETURNS msegment[] AS $$
    SELECT the_intvl.moving_segments;
$$ LANGUAGE SQL;

--------------------- TYPE CONSTRUCTOR HELPERS ------------------------

-- FUNGSI CREATE MSEGMENTS
CREATE OR REPLACE FUNCTION public.create_msegments(src_time float, dest_time float, src_reg geometry, dest_reg geometry)
RETURNS msegment[] AS $$
DECLARE the_msegments msegment[];
BEGIN
    SELECT interpolate_nonconvex_simple(src_time, dest_time, src_reg, dest_reg) INTO the_msegments;
    return the_msegments;
END
$$ LANGUAGE plpgsql;

-- FUNGSI CREATE ARRAY OF INTERVAL REGIONS
CREATE OR REPLACE FUNCTION public.append_intervalregion(the_intervalregion intervalregion)
RETURNS intervalregion[] AS $$
DECLARE the_array intervalregion[];
BEGIN
    SELECT array_append(the_array, the_intervalregion) INTO the_array;
    RETURN the_array;
END
$$ LANGUAGE plpgsql;

-- FUNGSI APPEND ARRAY OF INTERVAL REGIONS
CREATE OR REPLACE FUNCTION public.append_intervalregion(the_array intervalregion[], the_intervalregion intervalregion)
RETURNS intervalregion[] AS $$
BEGIN
    SELECT array_append(the_array, the_intervalregion) INTO the_array;
    RETURN the_array;
END
$$ LANGUAGE plpgsql;

-- MOVING REGION
-- CREATE TABLE tes5(idd serial primary key, the_mregion mregion);
/* update tes5
SET the_mregion = (1,something)
FROM (Select append_intervalregion(the_intervalregion) as something from test_intvl where idd=1) as som
*/
CREATE DOMAIN mregion intervalregion[];

CREATE OR REPLACE FUNCTION public.mregion(intvlreg intervalregion)
RETURNS mregion AS $$
    SELECT append_intervalregion(intvlreg)::mregion;
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION public.mregion(intvlregs intervalregion[])
RETURNS mregion AS $$
    SELECT intvlregs::mregion;
$$ LANGUAGE SQL;

-- PERIOD
CREATE TYPE period AS (
    tstart float,
    tend float
);

CREATE OR REPLACE FUNCTION public.period(tstart float, tend float)
RETURNS period AS $$
DECLARE the_period period;
BEGIN
    the_period.tstart := tstart;
    the_period.tend := tend;
    RETURN the_period;
END
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.second(yr integer, mon integer, dy integer, hr integer, mnt integer, sec integer)
RETURNS period AS $$
DECLARE 
    the_period period;
BEGIN
    the_period.tstart := extract(epoch from make_timestamp(yr, mon, dy, hr, mnt, sec))::float;
    the_period.tend := extract(epoch from make_timestamp(yr, mon, dy, hr, mnt, sec))::float;
    RETURN the_period;
END
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.minute(yr integer, mon integer, dy integer, hr integer, mnt integer)
RETURNS period AS $$
DECLARE 
    the_period period;
BEGIN
    the_period.tstart := extract(epoch from make_timestamp(yr, mon, dy, hr, mnt, 0))::float;
    the_period.tend := extract(epoch from make_timestamp(yr, mon, dy, hr, mnt, 59))::float;
    RETURN the_period;
END
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.hour(yr integer, mon integer, dy integer, hr integer)
RETURNS period AS $$
DECLARE 
    the_period period;
BEGIN
    the_period.tstart := extract(epoch from make_timestamp(yr, mon, dy, hr, 0, 0))::float;
    the_period.tend := extract(epoch from make_timestamp(yr, mon, dy, hr, 59, 59))::float;
    RETURN the_period;
END
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.day(yr integer, mon integer, dy integer)
RETURNS period AS $$
DECLARE 
    the_period period;
BEGIN
    the_period.tstart := extract(epoch from make_timestamp(yr, mon, dy, 0, 0, 0))::float;
    the_period.tend := extract(epoch from make_timestamp(yr, mon, dy, 23, 59, 59))::float;
    RETURN the_period;
END
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.month(yr integer, mon integer)
RETURNS period AS $$
DECLARE 
    the_period period;
    day_end integer;
BEGIN
    the_period.tstart := extract(epoch from make_timestamp(yr, mon, 1, 0, 0, 0))::float;
    IF (mon = 1 OR mon = 3 OR mon = 5 OR mon = 7 OR mon = 8 OR mon = 10 OR mon = 12) THEN
        the_period.tend := extract(epoch from make_timestamp(yr, mon, 31, 23, 59, 59))::float;
    ELSIF (mon = 2 AND yr%4 = 0) THEN
        the_period.tend := extract(epoch from make_timestamp(yr, mon, 29, 23, 59, 59))::float;
    ELSIF (mon = 2 AND yr%4 != 0) THEN
        the_period.tend := extract(epoch from make_timestamp(yr, mon, 28, 23, 59, 59))::float;
    ELSE
        the_period.tend := extract(epoch from make_timestamp(yr, mon, 30, 23, 59, 59))::float;
    END IF;
    RETURN the_period;
END
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.year(yr integer)
RETURNS period AS $$
DECLARE 
    the_period period;
BEGIN
    the_period.tstart := extract(epoch from make_timestamp(yr, 1, 1, 0, 0, 0))::float;
    the_period.tend := extract(epoch from make_timestamp(yr, 12, 31, 23, 59, 59))::float;
    RETURN the_period;
END
$$ LANGUAGE plpgsql;

-- Get tstart in a period
CREATE OR REPLACE FUNCTION public.get_tstart(the_period period)
RETURNS float AS $$
    SELECT the_period.tstart;
$$ LANGUAGE SQL;

-- Get tend in a period
CREATE OR REPLACE FUNCTION public.get_tend(the_period period)
RETURNS float AS $$
    SELECT the_period.tend;
$$ LANGUAGE SQL;


-- INTIME
CREATE TYPE intime AS (
    inst float,
    val geometry
);

CREATE OR REPLACE FUNCTION public.intime(inst float, val geometry)
RETURNS intime AS $$
DECLARE the_intime intime;
BEGIN
    the_intime.inst := inst;
    the_intime.val := val;
    RETURN the_intime;
END
$$ LANGUAGE plpgsql;

CREATE TYPE pg_segment AS(
    arr_pg float[],
    arr_segment segment[]
);

-- pg_segment constructor
CREATE OR REPLACE FUNCTION public.pg_segment(arr_pg float[], arr_segment segment[])
RETURNS pg_segment AS $$
DECLARE the_pg_segment pg_segment;
BEGIN
    the_pg_segment.arr_pg := arr_pg;
    the_pg_segment.arr_segment := arr_segment;
    RETURN the_pg_segment;
END
$$ LANGUAGE plpgsql;

-- arr_pg getter
CREATE OR REPLACE FUNCTION public.get_arr_pg(elmt pg_segment)
RETURNS float[] AS $$
    SELECT elmt.arr_pg;
$$ LANGUAGE SQL;

-- arr_segment getter
CREATE OR REPLACE FUNCTION public.get_arr_segment(elmt pg_segment)
RETURNS segment[] AS $$
    SELECT elmt.arr_segment;
$$ LANGUAGE SQL;

----------------------------------------------------------------------------------
---------------------- INTERPOLATION ALGORITHM HELPERS ---------------------------
----------------------------------------------------------------------------------

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


-- CHECK IF POLYGON IS CONVEX
CREATE OR REPLACE FUNCTION public.is_convex(reg geometry)
RETURNS boolean AS $$
    SELECT ST_Equals(reg, ST_ConvexHull(reg));
$$ LANGUAGE SQL;

-- CREATE ARRAY OF PROGRESS ANGLE DAN ARRAY OF LINESTRINGS OF IT (ORIGINAL)
CREATE OR REPLACE FUNCTION public.create_array_pg(reg geometry)
RETURNS pg_segment AS $$
DECLARE
    arr_pg float[];
    arr_segment segment[];
    r_current segment;
    r_last segment;
    idx integer;   
BEGIN  
    idx = 1;
    SELECT get_segment_n(-2,reg)
    INTO r_last;
    
    LOOP
        SELECT get_segment_n(idx, reg) INTO r_current;
        SELECT array_append(arr_segment, r_current) INTO arr_segment;
        SELECT array_append(arr_pg, calculate_progress_angle(r_current)) INTO arr_pg;
        RAISE NOTICE 'segment: %',st_astext(r_current);
        RAISE NOTICE 'angle: %', calculate_progress_angle(r_current); 
        idx := idx + 1;
    EXIT WHEN (ST_Equals(r_current, r_last));
    END LOOP;

    RETURN pg_segment(arr_pg, arr_segment);
END
$$ LANGUAGE plpgsql;

-- COMPARE ORIGINAL ARRAY AND CONVEX HULL ARRAY. RETURN: NEW ORIGINAL PG_SEGMENT
CREATE OR REPLACE FUNCTION public.get_new_pg_ori2(ori_seg segment[], chull pg_segment)
RETURNS float[] AS $$
DECLARE
    ori_pg float[];
    chull_seg segment[];
    chull_pg float[];
    idx integer;
    cidx integer;
BEGIN 
    RAISE NOTICE 'LEWAT SINI GAIS YG KEDUA TAPIIII';
    SELECT get_arr_pg(chull), get_arr_segment(chull) INTO chull_pg, chull_seg;
    RAISE NOTICE 'pg chull: %', chull_pg;
    idx = 1;
    cidx = 1;
    FOR i IN 1..array_length(ori_seg, 1)
    LOOP
        SELECT find_pg_idx(ori_seg, i, chull_seg) INTO idx;
        IF (idx = -1) THEN
            SELECT array_append(ori_pg, ori_pg[i-1]) INTO ori_pg;
        ELSE
            SELECT array_append(ori_pg, chull_pg[idx]) INTO ori_pg;
        END IF;
    END LOOP;
    RAISE NOTICE 'pg ori: %', ori_pg;
    RETURN ori_pg;
END
$$ LANGUAGE plpgsql;

-- COMPARE ORIGINAL ARRAY AND CONVEX HULL ARRAY. RETURN: NEW ORIGINAL PG_SEGMENT
CREATE OR REPLACE FUNCTION public.find_pg_idx(ori_seg segment[], ori_idx integer, chull_seg segment[])
RETURNS integer AS $$
DECLARE
    idx integer;
    i integer;
    found boolean;
BEGIN 
    found = false;
    idx = 0;
    -- Find idx in chull array where ori_seg is located (to assign progress angle(pg))
    FOR i IN 1..array_length(chull_seg, 1)
    LOOP
        IF ST_Equals(ori_seg[ori_idx], chull_seg[i]) THEN
            idx = i;
            found = true;
        END IF;
    EXIT WHEN found;        
    END LOOP;
    -- if found, return.
    IF (idx != 0) THEN
        RETURN idx;
    -- if not found, it's in concavity. so, look for it's closing segment
    ELSE
        FOR i IN 1..array_length(chull_seg, 1)
        LOOP
            IF ST_Equals(ST_PointN(ori_seg[ori_idx], 1), ST_PointN(chull_seg[i], 1)) THEN
                idx = i;
                found = true;
            ELSIF ST_Equals(ST_PointN(ori_seg[ori_idx], 1), ST_PointN(chull_seg[i], 1)) THEN 
                idx = i;
                found = true;
            END IF;
        EXIT WHEN found;        
        END LOOP;
    END IF;
    -- if found, return
    IF (idx != 0) THEN
        RETURN idx;
    -- if still not found, it's in 'deeper' concavity where none of its point shares with closing segment.
    -- look for closing segment by comparing its previous segment.
    ELSE
        IF (ori_idx != 1) THEN
            -- it means just assign the previous progress angle in the ori segments
            idx = -1;
        ELSE
            SELECT find_pg_idx_compare(ori_seg, ori_idx, chull_seg) INTO idx;
        END IF;
    END IF;
    RETURN idx;
END
$$ LANGUAGE plpgsql;

-- COMPARE ORIGINAL ARRAY AND CONVEX HULL ARRAY. RETURN: NEW ORIGINAL PG_SEGMENT
CREATE OR REPLACE FUNCTION public.find_pg_idx_compare(ori_seg segment[], ori_idx integer, chull_seg segment[])
RETURNS integer AS $$
DECLARE
    idx integer;
    i integer;
    found boolean;
BEGIN 
    idx = 0;
    i = ori_idx;
    found = false;
    LOOP 
        IF (idx != 0) THEN
            found = true;
        ELSE
            IF (i = 1) THEN  
                SELECT array_length(ori_seg, 1) INTO i;
            ELSE
                i = i - 1;
            END IF;
        END IF;
        SELECT find_pg_idx_simple(ori_seg[i], chull_seg) INTO idx;
    EXIT WHEN found;
    END LOOP;
    -- after found, return the closing segment idx (idx + 1)
    IF (idx = array_length(chull_seg,1)) THEN
        idx = 1;
    ELSE
        idx = idx + 1;
    END IF;

    RETURN idx;
END
$$ LANGUAGE plpgsql;

-- COMPARE ORIGINAL ARRAY AND CONVEX HULL ARRAY. RETURN: NEW ORIGINAL PG_SEGMENT
CREATE OR REPLACE FUNCTION public.find_pg_idx_simple(ori_seg segment, chull_seg segment[])
RETURNS integer AS $$
DECLARE
    idx integer;
    found boolean;
BEGIN 
    found = false;
    idx = 0;
    -- Find idx in chull array where ori_seg is located (to assign progress angle(pg))
    FOR i IN 1..array_length(chull_seg, 1)
    LOOP
        IF ST_Equals(ori_seg, chull_seg[i]) THEN
            idx = i;
            found = true;
        END IF;
    EXIT WHEN found;        
    END LOOP;
    RETURN idx;
END
$$ LANGUAGE plpgsql;


-------------------------------------------------------------------------------------
-------------------- INTERPOLATION ALGORITHMS (SIMPLE REGION) -----------------------
-------------------------------------------------------------------------------------

-- select st_astext(ST_ForcePolygonCCW(st_convexhull(ST_Polygon('LINESTRING(0 0, 4 2, 8 0, 8 8, 4 10, 0 8, 0 0)', 4326))))
-- select interpolate_nonconvex_simple(1,100,(ST_Polygon('LINESTRING(0 0, 4 2, 8 0, 8 8, 4 10, 0 8, 0 0)', 4326)),(ST_Polygon('LINESTRING(0 0, 8 0, 8 8, 0 8, 0 0)', 4326)))
-- select mregion(intervalregion(1,100,(ST_Polygon('LINESTRING(0 0, 4 2, 8 0, 8 8, 4 10, 0 8, 0 0)', 4326)),(ST_Polygon('LINESTRING(0 0, 8 0, 8 4, 4 4, 4 8, 0 8, 0 0)', 4326))))

-- INTERPOLASI SIMPLE NONCONVEX (AND CONVEX)
CREATE OR REPLACE FUNCTION public.interpolate_nonconvex_simple(src_time float, dest_time float, src_reg geometry, dest_reg geometry)
RETURNS msegment[] AS $$
DECLARE 
    src segment[]; -- array of src geom's linestrings
    dest segment[];
    src_chull geometry[]; -- array of src geom's convex hull linestrings
    dest_chull geometry[];
    src_pg float[]; -- array of src geom's linestrings' progress angles
    dest_pg float[];
    src_pg_chull float[]; -- array of src geom's convex hull linestrings' progress angles
    dest_pg_chull float[];
    src_reg_chull geometry;
    dest_reg_chull geometry;
BEGIN
    -- Interpolation algorithm here
    IF (ST_GeometryType(src_reg) = 'ST_Point') THEN
        SELECT get_arr_segment(create_array_pg(dest_reg)) INTO dest;
        RETURN interpolate_src_point(src_time, dest_time, src_reg, dest);
    ELSIF (ST_GeometryType(dest_reg) = 'ST_Point') THEN
        SELECT get_arr_segment(create_array_pg(src_reg)) INTO src;
        RETURN interpolate_dest_point(src_time, dest_time, src, dest_reg);
    ELSE
        SELECT  get_arr_segment(create_array_pg(src_reg)), 
                get_arr_segment(create_array_pg(dest_reg))
        INTO src, dest;
        -- If convex
        IF (is_convex(src_reg) AND is_convex(dest_reg)) THEN
            -- progress angle = unchanged
            SELECT  get_arr_pg(create_array_pg(src_reg)), 
                    get_arr_pg(create_array_pg(dest_reg))
            INTO src_pg, dest_pg;
            RETURN interpolate_simple(src_time, dest_time, src, dest, src_pg, dest_pg);
        -- If nonconvex
        ELSE
            -- create convex hull of regions and equalize their directions with original regions
            IF (ST_IsPolygonCW(src_reg)) THEN 
                SELECT ST_ForcePolygonCW(ST_ConvexHull(src_reg)) INTO src_reg_chull;
            ELSE 
                SELECT ST_ForcePolygonCCW(ST_ConvexHull(src_reg)) INTO src_reg_chull;
            END IF;
            IF (ST_IsPolygonCW(dest_reg)) THEN 
                SELECT ST_ForcePolygonCW(ST_ConvexHull(dest_reg)) INTO dest_reg_chull;
            ELSE 
                SELECT ST_ForcePolygonCCW(ST_ConvexHull(dest_reg)) INTO dest_reg_chull;
            END IF;
            -- intialize array values of convex hull regions
            SELECT  get_arr_segment(create_array_pg(src_reg_chull)), 
                    get_arr_segment(create_array_pg(dest_reg_chull)),
                    get_arr_pg(create_array_pg(src_reg_chull)), 
                    get_arr_pg(create_array_pg(dest_reg_chull))
            INTO src_chull, dest_chull, src_pg_chull, dest_pg_chull;
            -- progress angle = changed based in convexhull progress angle
            SELECT get_new_pg_ori2(src, pg_segment(src_pg_chull, src_chull)) INTO src_pg;
            SELECT get_new_pg_ori2(dest, pg_segment(dest_pg_chull, dest_chull)) INTO dest_pg;

            RETURN interpolate_simple(src_time, dest_time, src, dest, src_pg, dest_pg);
        END IF;
    END IF;
END
$$ LANGUAGE plpgsql;

-- BASIC INTERPOLATION ALGORITHM
CREATE OR REPLACE FUNCTION public.interpolate_simple(src_time float, dest_time float, src segment[], dest segment[], src_pg float[], dest_pg float[])
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
    SELECT  src[rn], 
            src[array_length(src,1)],
            dest[dn], 
            dest[array_length(dest,1)]
    INTO r_current, r_last, d_current, d_last;

    SELECT ST_GeomFromText('LINESTRING(0 0, 0 0)', 4326) into r_prev;
    SELECT ST_GeomFromText('LINESTRING(0 0, 0 0)', 4326) into d_prev;

    SELECT  src_pg[rn], dest_pg[dn]
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
        SELECT  src_pg[rn+1], dest_pg[dn+1]
        INTO r_angle, d_angle;
        
        IF (NOT ST_Equals(r_current, r_last)) AND (r_angle < d_angle OR ST_Equals(d_current, d_last)) THEN
            rn = rn + 1;
            SELECT  src[rn] INTO r_current;
        ELSIF NOT ST_Equals(d_current, d_last) THEN
            dn = dn + 1;
            SELECT  dest[dn] INTO d_current;
        END IF;

    -- Exit loop when reach the end of each region
    EXIT WHEN ( ST_Equals(r_current,r_last) AND ST_Equals(r_current,r_prev) AND ST_Equals(d_current,d_last) AND ST_Equals(d_current,d_prev) );
    END LOOP;
    RETURN the_msegments;
END
$$ LANGUAGE plpgsql;

-- select interpolate_nonconvex_simple(1, 100, ST_GeomFromText('POINT(100 100)', 4326), ST_POLYGON('LINESTRING(0 0, 8 0, 4 4, 8 8, 0 8, 0 0)', 4326))
-- select st_astext(st_normalize(st_convexhull(ST_POLYGON('LINESTRING(0 0, 8 0, 4 4, 8 8, 6 4, 4 8, 2 4, 0 8, 0 0)', 4326))))

-- INTERPOLATE SIMPLE REGION WITH DEST = POINT
CREATE OR REPLACE FUNCTION public.interpolate_dest_point(src_time float, dest_time float, src segment[], dest_point geometry)
RETURNS msegment[] AS $$
DECLARE
    the_msegments msegment[];
    r_seg segment;  
    rn integer;
BEGIN
    -- Interpolation algorithm here
    -- each segment must project triangle to the src point
    rn = 1;
    -- begin for loop to create delta triangles (moving segments)
    FOREACH r_seg IN ARRAY src
    LOOP
        SELECT array_append(the_msegments, create_msegment1(r_seg, src_time, dest_point, dest_time)) into the_msegments;
        RAISE NOTICE 'msegment added: %',ST_AsText(the_msegments[rn]);
        rn = rn + 1;
    END LOOP;
    RETURN the_msegments;
END
$$ LANGUAGE plpgsql;


-- INTERPOLATE SIMPLE REGION WITH SRC = POINT
CREATE OR REPLACE FUNCTION public.interpolate_src_point(src_time float, dest_time float, src_point geometry, dest segment[])
RETURNS msegment[] AS $$
DECLARE
    the_msegments msegment[];
    d_seg segment;  
    dn integer;
BEGIN
    -- Interpolation algorithm here
    -- each segment must project triangle to the src point
    dn = 1;
    -- begin for loop to create delta triangles (moving segments)
    FOREACH d_seg IN ARRAY dest
    LOOP
        SELECT array_append(the_msegments, create_msegment1(d_seg, dest_time, src_point, src_time)) into the_msegments;
        RAISE NOTICE 'msegment added: %',ST_AsText(the_msegments[dn]);
        dn = dn + 1;
    END LOOP;
    RETURN the_msegments;
END
$$ LANGUAGE plpgsql;

-- GET VALUE MAX
CREATE OR REPLACE FUNCTION public.get_array_max(arr float[])
RETURNS float AS $$
DECLARE 
    max float;
    elmt float;
BEGIN
    max = arr[1];
    FOREACH elmt in ARRAY arr 
    LOOP
        IF (elmt > max) THEN
            max = elmt;
        END IF;
    END LOOP;
    RETURN max;
END
$$ LANGUAGE plpgsql;

-- GET VALUE MIN
CREATE OR REPLACE FUNCTION public.get_array_min(arr float[])
RETURNS float AS $$
DECLARE 
    min float;
    elmt float;
BEGIN
    min = arr[1];
    FOREACH elmt in ARRAY arr 
    LOOP
        IF (elmt < min) THEN
            min = elmt;
        END IF;
    END LOOP;
    RETURN min;
END
$$ LANGUAGE plpgsql;

-- GET IDX MAX
CREATE OR REPLACE FUNCTION public.get_array_max_idx(arr float[])
RETURNS integer[] AS $$
DECLARE 
    max float;
    idx_arr integer[];
    i integer;
    elmt float;
BEGIN
    SELECT get_array_max(arr) INTO max;
    i = 1;
    FOREACH elmt in ARRAY arr 
    LOOP
        IF (elmt = max) THEN
            SELECT array_append(idx_arr, i) INTO idx_arr;
        END IF;
        i = i + 1;
    END LOOP;
    
    RETURN idx_arr;
END
$$ LANGUAGE plpgsql;

-- GET IDX MIN
CREATE OR REPLACE FUNCTION public.get_array_min_idx(arr float[])
RETURNS integer[] AS $$
DECLARE 
    min float;
    idx_arr integer[];
    i integer;
    elmt float;
BEGIN
    SELECT get_array_min(arr) INTO min;
    i = 1;
    FOREACH elmt IN ARRAY arr 
    LOOP
        IF (elmt = min) THEN
            SELECT array_append(idx_arr, i) INTO idx_arr;
        END IF;
        i = i + 1;
    END LOOP;
    
    RETURN idx_arr;
END
$$ LANGUAGE plpgsql;

------------------------------------------------------------------------------------
--------------------- FUNGSI PROYEKSI (PADA DOMAIN DAN RANGE -----------------------
------------------------------------------------------------------------------------
-- DEFTIME
CREATE OR REPLACE FUNCTION public.deftime(mreg mregion)
RETURNS period AS $$
DECLARE
    tstart float;
    tend float;
BEGIN
    SELECT get_src_time(mreg[1]) INTO tstart;
    SELECT get_dest_time(mreg[array_length(mreg, 1)]) INTO tend;
    RETURN period(tstart,tend);
END
$$ LANGUAGE plpgsql;

-- DEFTIME
CREATE OR REPLACE FUNCTION public.deftime1(mreg mregion)
RETURNS period[] AS $$
DECLARE
    tstart float;
    tend float;
    srctime_arr float[];
    destime_arr float[];
    intvlreg intervalregion;
    i integer;
    periods period[];
BEGIN
    IF mreg IS NULL THEN
        RETURN NULL;
    END IF;
    FOREACH intvlreg IN ARRAY mreg::intervalregion[]
    LOOP
        SELECT array_append(srctime_arr, get_src_time(intvlreg)) INTO srctime_arr;
        SELECT array_append(destime_arr, get_dest_time(intvlreg)) INTO destime_arr;
    END LOOP;

    i = 1;
    tstart = srctime_arr[i];
    LOOP
        IF (destime_arr[i] != srctime_arr[i+1]) OR (destime_arr[i] != srctime_arr[i+1] IS NULL) THEN
            tend = destime_arr[i];
            SELECT array_append(periods, period(tstart, tend)) INTO periods;
            tstart = srctime_arr[i+1];
        END IF;
        i = i + 1;
    EXIT WHEN (i > array_length(destime_arr, 1));
    END LOOP;

    RETURN periods;
END
$$ LANGUAGE plpgsql;

-- TRAVERSED
CREATE OR REPLACE FUNCTION public.traversed(mreg mregion)
RETURNS geometry AS $$
DECLARE
    intvlreg intervalregion;
    mseg msegment;
    result geometry;
    i integer;
BEGIN
    SELECT get_src_region(atinstant_intvlreg(mreg,inst(initial(mreg)))) INTO result;

    FOREACH intvlreg IN ARRAY mreg::intervalregion[]
    LOOP
        FOR i in get_src_time(intvlreg)..get_dest_time(intvlreg)
        LOOP
            SELECT ST_Union(result,val(atinstant(mreg,i))) INTO result;
            RAISE NOTICE 'i: %',i;
            RAISE NOTICE 'val: %',st_astext(val(atinstant(mreg,i)));
        END LOOP;
    END LOOP;

    RETURN result;
END
$$ LANGUAGE plpgsql;

-- INST: Get time instant in an intime
CREATE OR REPLACE FUNCTION public.inst(the_intime intime)
RETURNS float AS $$
    SELECT the_intime.inst;
$$ LANGUAGE SQL;

-- VAL: Get region value in an intime
CREATE OR REPLACE FUNCTION public.val(the_intime intime)
RETURNS geometry AS $$
    SELECT the_intime.val;
$$ LANGUAGE SQL;


------------------------------------------------------------------------------------
--------------------- FUNGSI INTERAKSI (PADA DOMAIN DAN RANGE ----------------------
------------------------------------------------------------------------------------

-- ATINSTANT
CREATE OR REPLACE FUNCTION public.atinstant(mreg mregion, inst float)
RETURNS intime AS $$
DECLARE
    arr_multipoints geometry[]; arr_points geometry[];
    val geometry;
    intvlreg intervalregion; intvlreg_idx float;
    msegments msegment[];
    mseg msegment;
    delta_triangle geometry; --for debugging
    intersection_result geometry;
    infinite_inst_plane geometry;
    src_time float; dest_time float;
BEGIN
    -- If time instant is present in mregion
    IF present(mreg, inst) THEN
        SELECT  get_src_time(atinstant_intvlreg(mreg, inst)),
                get_dest_time(atinstant_intvlreg(mreg, inst))
        INTO src_time, dest_time;

        raise notice 'sr time: %',src_time;
        raise notice 'dest time: %',dest_time;

        IF (src_time = inst) THEN 
            SELECT get_src_region(atinstant_intvlreg(mreg, inst)) INTO val;
        ELSIF (dest_time = inst) THEN
            SELECT get_dest_region(atinstant_intvlreg(mreg, inst)) INTO val;
        ELSE
            -- Get msegments in interval region in which the time instant is present
            SELECT get_moving_segments(atinstant_intvlreg(mreg, inst)) INTO msegments;
            -- Get infinite instant plane to intersect with delta triangle
            SELECT create_infinite_instant_plane(inst) INTO infinite_inst_plane;
            -- Traverse through all delta triangles to intersect it with infinite instant plane
            -- to get linestring and then create polygon
            FOREACH mseg IN ARRAY msegments
            LOOP
                RAISE NOTICE 'The msegment? %', ST_AsText(mseg);
                SELECT create_triangle_msegment(mseg) INTO delta_triangle; -- for debugging
                RAISE NOTICE 'The triangle: %', ST_AsText(delta_triangle);

                SELECT segarray_3DIntersection(create_segarray_msegment(mseg), infinite_inst_plane) INTO intersection_result;
                SELECT array_append(arr_multipoints, intersection_result) INTO arr_multipoints;
                RAISE NOTICE 'The intersection: %', ST_AsText(intersection_result);
            END LOOP;
            SELECT ST_PrintText(arr_multipoints) INTO arr_points;
            SELECT get_sorted_points(sort_multipoint_array(arr_multipoints)) INTO arr_points;
            SELECT ST_PrintText(arr_points) INTO arr_points;
            SELECT ST_SimplifyPreserveTopology(ST_SetSRID(ST_MakePolygon(ST_MakeLine(arr_points)),4326),0) INTO val;
            RAISE NOTICE 'DONE YAY';
        END IF;
    ELSE 
        RETURN NULL;
    END IF;

    RETURN intime(inst,ST_Force2D(val));
END
$$ LANGUAGE plpgsql;

-- ATPERIODS
-- select traversed(atperiods(the_mregion,period(67,125))) from tes5
CREATE OR REPLACE FUNCTION public.atperiods(mreg mregion, per period)
RETURNS mregion AS $$
DECLARE
    mreg_result intervalregion[];
    idx_tstart integer;
    idx_tend integer;
    reg_tstart geometry;
    reg_tend geometry;
    intvlreg intervalregion;
    i integer;
    mreg_arr intervalregion[];
BEGIN
    SELECT mreg::intervalregion[] INTO mreg_arr;
    IF present(mreg, per) THEN
        SELECT  atinstant_intvlreg_idx(mreg, get_tstart(per)),
                atinstant_intvlreg_idx(mreg, get_tend(per)),
                val(atinstant(mreg,get_tstart(per))),
                val(atinstant(mreg,get_tend(per)))
        INTO idx_tstart, idx_tend, reg_tstart, reg_tend;
        -- In the same interval region
        IF (idx_tstart = idx_tend) THEN
            SELECT append_intervalregion(mreg_result,intervalregion(mreg_arr[idx_tstart],get_tstart(per), get_tend(per), reg_tstart, reg_tend))
            INTO mreg_result;
            RETURN mreg_result;
        -- In different interval region
        ELSE
            --SELECT atinstant_intvlreg(mreg, get_tstart(per)) INTO intvlreg;
            SELECT append_intervalregion(mreg_result,intervalregion(mreg_arr[idx_tstart], get_tstart(per), get_dest_time(atinstant_intvlreg(mreg, get_tstart(per))), reg_tstart, get_dest_region(atinstant_intvlreg(mreg, get_tstart(per)))))
            INTO mreg_result;

            IF ((idx_tend - idx_tstart) > 1) THEN
                FOR i IN 1..(idx_tend-idx_tstart-1)
                LOOP
                    SELECT append_intervalregion(mreg_result, mreg[i+idx_tstart]) INTO mreg_result;
                END LOOP;
            END IF;

            --SELECT atinstant_intvlreg(mreg, get_tend(per)) INTO intvlreg;
            SELECT append_intervalregion(mreg_result, intervalregion(mreg_arr[idx_t], get_src_time(atinstant_intvlreg(mreg, get_tend(per))), get_tend(per), get_src_region(atinstant_intvlreg(mreg, get_tend(per))), reg_tend))
            INTO mreg_result;

            RETURN mreg_result;
        END IF;
    ELSE
        RETURN NULL;
    END IF;  
END
$$ LANGUAGE plpgsql;


-- INITIAL
CREATE OR REPLACE FUNCTION public.initial(mreg mregion)
RETURNS intime AS $$
DECLARE
    inst float;
    val geometry;
BEGIN
    SELECT get_src_time(mreg[1]) INTO inst;
    SELECT get_src_region(mreg[1]) INTO val;

    RETURN intime(inst,val);
END
$$ LANGUAGE plpgsql;

-- FINAL
CREATE OR REPLACE FUNCTION public.final(mreg mregion)
RETURNS intime AS $$
DECLARE
    inst float;
    val geometry;
BEGIN
    SELECT get_dest_time(mreg[array_length(mreg, 1)]) INTO inst;
    SELECT get_dest_region(mreg[array_length(mreg, 1)]) INTO val;

    RETURN intime(inst,val);
END
$$ LANGUAGE plpgsql;

-- PRESENT (time: instant)
CREATE OR REPLACE FUNCTION public.present(mreg mregion, inst float)
RETURNS boolean AS $$
DECLARE
    is_present boolean;
    per period;
BEGIN
    is_present = false;
    IF mreg IS NULL THEN
        RETURN false;
	END IF;
    FOREACH per IN ARRAY deftime1(mreg) 
    LOOP
        IF (get_tstart(per) <= inst AND get_tend(per) >= inst) THEN
            is_present = true;
            RETURN is_present;
        END IF;
    END LOOP;
    RETURN is_present;
END
$$ LANGUAGE plpgsql;

-- PRESENT (time: period)
CREATE OR REPLACE FUNCTION public.present(mreg mregion, per period)
RETURNS boolean AS $$
DECLARE
    is_present boolean;
    pdeftime period;
BEGIN
    is_present = false;
    IF mreg IS NULL THEN
        RETURN false;
	END IF;
    FOREACH pdeftime IN ARRAY deftime1(mreg) 
    LOOP
        IF (get_tstart(pdeftime) <= get_tstart(per) AND get_tend(pdeftime) >= get_tend(per)) THEN
            is_present = true;
            RETURN is_present;
        END IF;
    END LOOP;
    RETURN is_present;
END
$$ LANGUAGE plpgsql;

-- AT
CREATE OR REPLACE FUNCTION public.at(mreg mregion, geom geometry)
RETURNS mregion AS $$
DECLARE
    tstart float;
    tend float;
    tstart_found boolean;
    tend_found boolean;
    both_found boolean;
	intvlreg intervalregion;
    time_last float;
BEGIN
    tstart_found = false;
    tend_found = false;
    both_found = false;
    FOREACH intvlreg IN ARRAY mreg::intervalregion[]
    LOOP
        FOR i in get_src_time(intvlreg)..get_dest_time(intvlreg)
        LOOP
            -- look for tstart in which mreg initially passes the 'geom'
            IF (NOT tstart_found) THEN
                IF (ST_Intersects(geom, val(atinstant(mreg, i)))) THEN
                    tstart = i;
                    tstart_found = true;
                END IF;
            -- if tstart found, look for the tend
            ELSE
                IF (NOT ST_Intersects(geom, val(atinstant(mreg, i)))) THEN
                    tend = i-1;
                    tend_found = true;
                END IF;
            END IF;
        EXIT WHEN (tend_found);
        END LOOP;
        time_last = get_dest_time(intvlreg);
    END LOOP;

    IF (NOT tend_found) THEN
        tend = time_last;
    END IF; 
    RETURN atperiods(mreg, period(tstart, tend));
END
$$ LANGUAGE plpgsql;

-- GET SNAPSHOTS FROM MREGION
CREATE OR REPLACE FUNCTION public.get_snapshots(mreg mregion)
RETURNS geometry[] AS $$
DECLARE 
    snapshots geometry[];
	i integer;
	intvlreg intervalregion;
BEGIN
    i = 1;
    FOREACH intvlreg IN ARRAY mreg::intervalregion[]
    LOOP
        SELECT array_append(snapshots, get_src_region(intvlreg)) INTO snapshots;
        IF (i = array_length(mreg::intervalregion[], 1)) THEN
            SELECT array_append(snapshots, get_dest_region(intvlreg)) INTO snapshots;
        END IF;
        i = i + 1;
    END LOOP;
    RETURN snapshots;
END
$$ LANGUAGE plpgsql;

-- GET SNAPSHOT TIMES FROM MREGION
CREATE OR REPLACE FUNCTION public.get_snapshot_times(mreg mregion)
RETURNS float[] AS $$
DECLARE 
    timee float[];
	i integer;
	intvlreg intervalregion;
BEGIN
    i = 1;
    FOREACH intvlreg IN ARRAY mreg::intervalregion[]
    LOOP
        SELECT array_append(timee, get_src_time(intvlreg)) INTO timee;
        IF (i = array_length(mreg::intervalregion[], 1)) THEN
            SELECT array_append(timee, get_dest_time(intvlreg)) INTO timee;
        END IF;
        i = i + 1;
    END LOOP;
    RETURN timee;
END
$$ LANGUAGE plpgsql;


-- ATMIN
CREATE OR REPLACE FUNCTION public.atmin(mreg mregion)
RETURNS mregion AS $$
DECLARE 
    snapshot geometry;
    snapshots geometry[];
    snapshot_times float[];
    snapshot_areas float[];

    idx_min_arr integer[];
    idx_min integer;

    new_mreg intervalregion[];
    mreg1 intervalregion[];

    i integer;
    appended_idx integer;

BEGIN
    SELECT mreg::intervalregion[] INTO mreg1;

    SELECT get_snapshots(mreg) INTO snapshots;
    SELECT get_snapshot_times(mreg) INTO snapshot_times;

    FOREACH snapshot IN ARRAY snapshots
    LOOP
        SELECT array_append(snapshot_areas, round(CAST(ST_Area(snapshot) AS NUMERIC), 10)::float) INTO snapshot_areas;
        
    END LOOP;

    SELECT get_array_min_idx(snapshot_areas) INTO idx_min_arr;

    -- min area mregion: just in 1 snapshot
    IF array_length(idx_min_arr,1) = 1 THEN
        idx_min = idx_min_arr[1];
        RETURN mregion(intervalregion( snapshot_times[idx_min], snapshot_times[idx_min], snapshots[idx_min], snapshots[idx_min] ));
    -- min area mregion: just in 1 snapshot
    ELSE
        i = 1;
        appended_idx = 0;
        LOOP
            -- if snapshots are in order (therefore: in the same intvlreg)
            IF (idx_min_arr[i]+1 = idx_min_arr[i+1]) AND ((idx_min_arr[i]+1 = idx_min_arr[i+1]) IS NOT NULL) THEN
                SELECT array_append(new_mreg, mreg1[idx_min_arr[i]]) INTO new_mreg;
                appended_idx = i+1;
            ELSE
                IF (i != appended_idx) THEN
                    SELECT array_append(new_mreg, intervalregion( 
                                            snapshot_times[idx_min_arr[i]], 
                                            snapshot_times[idx_min_arr[i]], 
                                            snapshots[idx_min_arr[i]], 
                                            snapshots[idx_min_arr[i]] ))
                    INTO new_mreg;
                END IF;
            END IF;
            i = i + 1;
        EXIT WHEN (i > array_length(idx_min_arr,1));
        END LOOP;
        RETURN mregion(new_mreg);
    END IF;
END
$$ LANGUAGE plpgsql;

-- ATMAX
CREATE OR REPLACE FUNCTION public.atmax(mreg mregion)
RETURNS mregion AS $$
DECLARE 
    snapshot geometry;
    snapshots geometry[];
    snapshot_times float[];
    snapshot_areas float[];

    idx_max_arr integer[];
    idx_max integer;

    new_mreg intervalregion[];
    mreg1 intervalregion[];

    i integer;
    appended_idx integer;

BEGIN
    SELECT mreg::intervalregion[] INTO mreg1;

    SELECT get_snapshots(mreg) INTO snapshots;
    SELECT get_snapshot_times(mreg) INTO snapshot_times;

    FOREACH snapshot IN ARRAY snapshots
    LOOP
        SELECT array_append(snapshot_areas, round(CAST(ST_Area(snapshot) AS NUMERIC), 10)::float) INTO snapshot_areas;
    END LOOP;

    SELECT get_array_max_idx(snapshot_areas) INTO idx_max_arr;

    -- max area mregion: just in 1 snapshot
    IF array_length(idx_max_arr,1) = 1 THEN
        idx_max = idx_max_arr[1];
        RETURN mregion(intervalregion( snapshot_times[idx_max], snapshot_times[idx_max], snapshots[idx_max], snapshots[idx_max] ));
    -- max area mregion: in several snapshots
    ELSE
        i = 1;
        appended_idx = 0;
        LOOP
            -- if snapshots are in order (therefore: in the same intvlreg)
            IF (idx_max_arr[i]+1 = idx_max_arr[i+1]) AND ((idx_max_arr[i]+1 = idx_max_arr[i+1]) IS NOT NULL) THEN
                SELECT array_append(new_mreg, mreg1[idx_max_arr[i]]) INTO new_mreg;
                appended_idx = i+1;
            ELSE
                IF (i != appended_idx) THEN
                    SELECT array_append(new_mreg, intervalregion( 
                                            snapshot_times[idx_max_arr[i]], 
                                            snapshot_times[idx_max_arr[i]], 
                                            snapshots[idx_max_arr[i]], 
                                            snapshots[idx_max_arr[i]] ))
                    INTO new_mreg;
                END IF;
            END IF;
            i = i + 1;
        EXIT WHEN (i > array_length(idx_max_arr,1));
        END LOOP;
        RETURN mregion(new_mreg);
    END IF;
END
$$ LANGUAGE plpgsql;

-- PASSES
CREATE OR REPLACE FUNCTION public.passes(mreg mregion, geom geometry)
RETURNS boolean AS $$
DECLARE 
    trav geometry;
    passed boolean;
	intvlreg intervalregion;
BEGIN
    FOREACH intvlreg IN ARRAY mreg::intervalregion[]
    LOOP
        IF ST_Intersects(get_src_region(intvlreg), geom) OR ST_Intersects(get_dest_region(intvlreg), geom) THEN
            passed = true;
        END IF;
    END LOOP;
    IF (NOT passed) THEN
        SELECT traversed(mreg) INTO trav;
        SELECT ST_Intersects(trav, geom) INTO passed;
    END IF; 
    RETURN passed;
END
$$ LANGUAGE plpgsql;

-------------------------------------------------------------------------
------------------------ HELPERS FOR ATINSTANT --------------------------
-------------------------------------------------------------------------

-- FUNGSI CREATE INFINITE INSTANT PLANE
--SELECT ST_AsText(ST_3DIntersection(ST_POLYGON('LINESTRING(0 0 1, 16 0 1, 8 0 100, 0 0 1)', 4326),create_infinite_instant_plane(50)));
CREATE OR REPLACE FUNCTION public.create_infinite_instant_plane(inst float)
RETURNS geometry AS $$
DECLARE
    arr_point geometry[];

BEGIN
    SELECT array_append(arr_point, ST_MakePoint(-400,-400,inst)) INTO arr_point;
    SELECT array_append(arr_point, ST_MakePoint(400,-400,inst)) INTO arr_point;
    SELECT array_append(arr_point, ST_MakePoint(400,400,inst)) INTO arr_point;
    SELECT array_append(arr_point, ST_MakePoint(-400,400,inst)) INTO arr_point;
    SELECT array_append(arr_point, ST_MakePoint(-400,-400,inst)) INTO arr_point;
    RETURN ST_SetSRID(ST_MakePolygon(ST_MakeLine(arr_point)),4326);
END
$$ LANGUAGE plpgsql;

-- CREATE TRIANGLE OF LINESTRING FROM MOVING SEGMENT
-- function is called for debugging purpose
--select create_triangle_msegment(msegment(ST_GeomFromText('POINT(0 0 0)',4326),ST_GeomFromText('POINT(0 2 3)',4326), ST_GeomFromText('POINT(1 4 3)',4326)))
CREATE OR REPLACE FUNCTION public.create_triangle_msegment(mseg msegment)
RETURNS geometry AS $$
DECLARE
    arr_point geometry[];
    triangle geometry;
BEGIN
    SELECT array_append(arr_point, mseg.mseg_a::geometry) INTO arr_point;
    SELECT array_append(arr_point, mseg.mseg_b::geometry) INTO arr_point;
    SELECT array_append(arr_point, mseg.mseg_c::geometry) INTO arr_point;
    SELECT array_append(arr_point, mseg.mseg_a::geometry) INTO arr_point;

    SELECT ST_SetSRID(ST_MakeLine(arr_point),4326) INTO triangle;
    return triangle;
END
$$ LANGUAGE plpgsql;

-- CREATE ARRAY OF TRIANGLE LINESTRINGS FROM MOVING SEGMENT
--select create_segarray_msegment(msegment(ST_GeomFromText('POINT(0 0 0)',4326),ST_GeomFromText('POINT(0 2 3)',4326), ST_GeomFromText('POINT(1 4 3)',4326)))
CREATE OR REPLACE FUNCTION public.create_segarray_msegment(mseg msegment)
RETURNS geometry[] AS $$
DECLARE
    arr_segment geometry[];
BEGIN
    SELECT array_append(arr_segment, ST_SetSRID(ST_MakeLine(mseg.mseg_a::geometry, mseg.mseg_b::geometry),4326)) INTO arr_segment;
    SELECT array_append(arr_segment, ST_SetSRID(ST_MakeLine(mseg.mseg_b::geometry, mseg.mseg_c::geometry),4326)) INTO arr_segment;
    SELECT array_append(arr_segment, ST_SetSRID(ST_MakeLine(mseg.mseg_c::geometry, mseg.mseg_a::geometry),4326)) INTO arr_segment;

    RETURN arr_segment;
END
$$ LANGUAGE plpgsql;

-- ARRAY OF SEGMENTS 3D INTERSECTION WITH INFINITE PLANE
-- select st_astext(segarray_3DIntersection(create_segarray_msegment(msegment(ST_GeomFromText('POINT(13 5 20)',4326),ST_GeomFromText('POINT(13 13 200)',4326), ST_GeomFromText('POINT(8 0 100)',4326))), ST_POLYGON('LINESTRING(-400 -400 151, 400 -400 151, 400 400 151, -400 400 151, -400 -400 151)',4326)))
CREATE OR REPLACE FUNCTION public.segarray_3DIntersection(arr_segment geometry[], infinite_plane geometry)
RETURNS geometry AS $$
DECLARE
    arr_point geometry[];
    intersection_point geometry;
    seg geometry;
BEGIN
    FOREACH seg IN ARRAY arr_segment
    LOOP 
        SELECT ST_3DIntersection(seg, infinite_plane) INTO intersection_point;
        IF NOT ST_IsEmpty(intersection_point) THEN
            SELECT array_append(arr_point, ST_SnapToGrid(intersection_point, 0.000001)) INTO arr_point;
        END IF;
    END LOOP;
   
    RETURN ST_Collect(arr_point);
END
$$ LANGUAGE plpgsql;

-- FUNGSI GET INTERVAL REGION BY INSTANT
CREATE OR REPLACE FUNCTION public.atinstant_intvlreg(mreg mregion, inst float)
RETURNS intervalregion AS $$
DECLARE
    intvlreg intervalregion;
BEGIN
    IF present(mreg, inst) THEN
        FOREACH intvlreg IN ARRAY mreg::intervalregion[]
        LOOP
            IF (inst >= get_src_time(intvlreg) AND inst <= get_dest_time(intvlreg)) THEN
                RETURN intvlreg;
            END IF;
        END LOOP;
    ELSE 
		RETURN NULL;
	END IF;
END
$$ LANGUAGE plpgsql;

-- FUNGSI GET INTERVAL REGION IDX BY INSTANT
CREATE OR REPLACE FUNCTION public.atinstant_intvlreg_idx(mreg mregion, inst float)
RETURNS integer AS $$
DECLARE
    intvlreg intervalregion[];
	i integer;
BEGIN
    SELECT mreg::intervalregion[] INTO intvlreg;
	FOR i IN 1..array_length(intvlreg,1)
	LOOP
		IF (inst >= get_src_time(intvlreg[i]) AND inst <= get_dest_time(intvlreg[i])) THEN
			RETURN i;
		END IF;
	END LOOP;
	RETURN NULL;
END
$$ LANGUAGE plpgsql;

-- SORT MULTIPOINT ARRAY
CREATE OR REPLACE FUNCTION public.sort_multipoint_array(arr_multipoint geometry[])
RETURNS geometry[] AS $$
DECLARE
    arr_new geometry[];
    switched_multipoint geometry;
BEGIN
    RAISE NOTICE 'SORT IDX PERTAMA';
    IF (ST_Distance(ST_GeometryN(arr_multipoint[1],2),ST_GeometryN(arr_multipoint[2],1)) < 0.000001) THEN
        SELECT array_append(arr_new, arr_multipoint[1]) INTO arr_new;
    ELSE
        SELECT ST_Collect(ST_GeometryN(arr_multipoint[1],2), ST_GeometryN(arr_multipoint[1],1)) INTO switched_multipoint;
        SELECT array_append(arr_new, switched_multipoint) INTO arr_new;
    END IF;
    FOR i IN 2..array_length(arr_multipoint,1)
        LOOP
            RAISE NOTICE 'SORT IDX BERIKUTNYA';
            RAISE NOTICE 'INI %', st_astext(ST_GeometryN(arr_new[i-1],2));
            RAISE NOTICE 'INI %', st_astext(ST_GeometryN(arr_multipoint[i],1));
            IF ST_Distance(ST_GeometryN(arr_multipoint[i],1),ST_GeometryN(arr_new[i-1],2)) < 0.000001 THEN
                RAISE NOTICE 'SAMA NIH';
                SELECT array_append(arr_new, arr_multipoint[i]) INTO arr_new;
            ELSE
                RAISE NOTICE 'BEDA NIH';
                SELECT ST_Collect(ST_GeometryN(arr_multipoint[i],2), ST_GeometryN(arr_multipoint[i],1)) INTO switched_multipoint;
                SELECT array_append(arr_new, switched_multipoint) INTO arr_new;
            END IF;
        END LOOP;
		RETURN arr_new;
END
$$ LANGUAGE plpgsql;

-- GET ARRAY OF POINTS FROM SORTED MULTIPOINT ARRAY
CREATE OR REPLACE FUNCTION public.get_sorted_points(arr_multipoint geometry[])
RETURNS geometry[] AS $$
DECLARE
    arr_new geometry[];
BEGIN
    FOR i IN 1..array_length(arr_multipoint,1)
        LOOP
            IF i=1 THEN
                SELECT array_append(arr_new, ST_GeometryN(arr_multipoint[i],1)) INTO arr_new;
                SELECT array_append(arr_new, ST_GeometryN(arr_multipoint[i],2)) INTO arr_new;
            ELSE
               SELECT array_append(arr_new, ST_GeometryN(arr_multipoint[i],2)) INTO arr_new;             
            END IF;
        END LOOP;
    IF NOT ST_Equals(arr_new[array_length(arr_new, 1)], arr_new[1]) THEN
        arr_new[array_length(arr_new, 1)] = arr_new[1];
	END IF;
    RETURN arr_new;
END
$$ LANGUAGE plpgsql;


-- GET ARRAY OF POINTS FROM SORTED MULTIPOINT ARRAY
CREATE OR REPLACE FUNCTION public.create_multi_arr(lol integer)
RETURNS geometry[] AS $$
DECLARE
    the_array geometry[];
BEGIN   
    SELECT array_append(the_array, ST_Collect(ST_MakePoint(0,0),ST_MakePoint(8,0))) INTO the_array;
    SELECT array_append(the_array, ST_Collect(ST_MakePoint(8,0),ST_MakePoint(8,8))) INTO the_array;
    SELECT array_append(the_array, ST_Collect(ST_MakePoint(8,8),ST_MakePoint(4,10))) INTO the_array;
    SELECT array_append(the_array, ST_Collect(ST_MakePoint(4,10),ST_MakePoint(0,8))) INTO the_array;
    SELECT array_append(the_array, ST_Collect(ST_MakePoint(0,8),ST_MakePoint(0,0))) INTO the_array;
    return the_array;
END
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.ST_PrintText(arr geometry[])
RETURNS geometry[] AS $$
DECLARE
    the_array geometry[];
    elmt geometry;
BEGIN  
    the_array := arr;
    FOREACH elmt in ARRAY arr
    LOOP
        RAISE NOTICE 'THE ELMT: %', ST_AsText(elmt);
    END LOOP;
    return the_array;
END
$$ LANGUAGE plpgsql;