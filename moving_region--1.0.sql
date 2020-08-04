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
    SELECT create_msegments(src_time, dest_time, src_reg, dest_reg) INTO the_intervalregion.moving_segments;
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

-- MOVING REGION
-- CREATE TABLE tes5(idd serial primary key, the_mregion mregion);
/* update tes5
SET the_mregion = (1,something)
FROM (Select append_intervalregion(the_intervalregion) as something from test_intvl where idd=1) as som
*/
CREATE DOMAIN mregion intervalregion[];

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

CREATE TYPE pg_segment AS{
    arr_pg float[];
    arr_segment segment[];
}

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


-------------------------------------------------------------------------
--------------------------- FUNGSI PROYEKSI -----------------------------
-------------------------------------------------------------------------

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

-- TRAVERSED
CREATE OR REPLACE FUNCTION public.traversed(mreg mregion)
RETURNS geometry AS $$
DECLARE
    intvlreg intervalregion;
    mseg msegment;
    result geometry;
    i integer;
BEGIN
    SELECT get_src_region(atinstant_intvlreg(mreg,get_tstart(deftime(mreg)))) INTO result;

    FOREACH intvlreg IN ARRAY mreg::intervalregion[]
    LOOP
        FOR i in get_src_time(intvlreg)..get_dest_time(intvlreg)
        LOOP
            -- When the z index % 5 is not 0, n error encountered with 3DIntersect function.
            -- Should be fixed soon 
            IF ((i%5 = 0)) THEN
                SELECT ST_Union(result,val(atinstant(mreg,i))) INTO result;
                RAISE NOTICE 'i: %',i;
                RAISE NOTICE 'val: %',st_astext(val(atinstant(mreg,i)));
            END IF;
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


-------------------------------------------------------------------------
-------------------------- FUNGSI INTERAKSI -----------------------------
-------------------------------------------------------------------------

-- ATINSTANT
CREATE OR REPLACE FUNCTION public.atinstant(mreg mregion, inst float)
RETURNS intime AS $$
DECLARE
    arr_multipoints geometry[]; arr_points geometry[];
    val geometry;
    intvlreg intervalregion; intvlreg_idx float;
    msegments msegment[];
    mseg msegment;
    delta_triangle geometry;
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
                SELECT create_triangle_msegment(mseg) INTO delta_triangle;
                RAISE NOTICE 'The triangle: %', ST_AsText(delta_triangle);
                SELECT array_append(arr_multipoints,ST_3DIntersection(delta_triangle,infinite_inst_plane)) INTO arr_multipoints;
                RAISE NOTICE 'The intersection: %', ST_AsText(ST_LineFromMultiPoint(ST_3DIntersection(delta_triangle,infinite_inst_plane)));
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
BEGIN
    IF present(mreg, per) THEN
        SELECT  atinstant_intvlreg_idx(mreg, get_tstart(per)),
                atinstant_intvlreg_idx(mreg, get_tend(per)),
                val(atinstant(mreg,get_tstart(per))),
                val(atinstant(mreg,get_tend(per)))
        INTO idx_tstart, idx_tend, reg_tstart, reg_tend;
        -- In the same interval region
        IF (idx_tstart = idx_tend) THEN
            SELECT append_intervalregion(mreg_result,intervalregion(get_tstart(per), get_tend(per), reg_tstart, reg_tend))
            INTO mreg_result;
            RETURN mreg_result;
        -- In different interval region
        ELSE
            --SELECT atinstant_intvlreg(mreg, get_tstart(per)) INTO intvlreg;
            SELECT append_intervalregion(mreg_result,intervalregion(get_tstart(per), get_dest_time(atinstant_intvlreg(mreg, get_tstart(per))), reg_tstart, get_dest_region(atinstant_intvlreg(mreg, get_tstart(per)))))
            INTO mreg_result;

            IF ((idx_tend - idx_tstart) > 1) THEN
                FOR i IN 1..(idx_tend-idx_tstart-1)
                LOOP
                    SELECT append_intervalregion(mreg_result, mreg[i+idx_start]) INTO mreg_result;
                END LOOP;
            END IF;

            --SELECT atinstant_intvlreg(mreg, get_tend(per)) INTO intvlreg;
            SELECT append_intervalregion(mreg_result, intervalregion(get_src_time(atinstant_intvlreg(mreg, get_tend(per))), get_tend(per), get_src_region(atinstant_intvlreg(mreg, get_tend(per))), reg_tend))
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
    SELECT get_tstart(deftime(mreg)) <= inst 
    AND get_tend(deftime(mreg)) >= inst;
$$ LANGUAGE SQL;

-- PRESENT (time: period)
CREATE OR REPLACE FUNCTION public.present(mreg mregion, per period)
RETURNS boolean AS $$
    SELECT get_tstart(deftime(mreg)) <= get_tstart(per) 
    AND get_tend(deftime(mreg)) >= get_tend(per);
$$ LANGUAGE SQL;

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
            -- When the z index % 5 is not 0, n error encountered with 3DIntersect function.
            -- Should be fixed soon 
            IF (i%5 = 0) THEN
                -- look for tstart in which mreg initially passes the 'geom'
                IF (NOT tstart_found) THEN
                    IF (ST_Intersects(geom, val(atinstant(mreg, i)))) THEN
                        tstart = i;
                        tstart_found = true;
                    END IF;
                -- if tstart found, look for the tend
                ELSE
                    IF (NOT ST_Intersects(geom, val(atinstant(mreg, i)))) THEN
                        tend = i;
                        tend_found = true;
                    END IF;
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

-- ATMIN

-- ATMAX

-- PASSES
CREATE OR REPLACE FUNCTION public.passes(mreg mregion, geom geometry)
RETURNS boolean AS $$
    SELECT ST_Intersects(traversed(mreg), geom);
$$ LANGUAGE SQL;

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
    SELECT array_append(arr_new, arr_multipoint[1]) INTO arr_new;
    FOR i IN 2..array_length(arr_multipoint,1)
        LOOP
            RAISE NOTICE 'SORT IDX BERIKUTNYA';
            RAISE NOTICE 'INI %', st_astext(ST_GeometryN(arr_new[i-1],2));
            RAISE NOTICE 'INI %', st_astext(ST_GeometryN(arr_multipoint[i],1));
            IF ST_Distance(ST_GeometryN(arr_multipoint[i],1),ST_GeometryN(arr_new[i-1],2)) < 0.00000000001 THEN
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

-------------------------------------------------------------------------
--------------------- INTERPOLASI SIMPLE NONCONVEX ----------------------
-------------------------------------------------------------------------

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



-- INTERPOLASI SIMPLE NONCONVEX
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
    -- intialize array values of original regions
        SELECT  get_arr_segment(create_array_pg(src_reg)), 
                get_arr_segment(create_array_pg(dest_reg))
        INTO src, dest;

    -- If convex
    IF (is_convex(src_reg) AND is_convex(src_reg)) THEN
        -- progress angle = unchanged
        SELECT  get_arr_pg(create_array_pg(src_reg)), 
                get_arr_pg(create_array_pg(dest_reg))
        INTO src_pg, dest_pg;
        RETURN interpolate_simple(src_time, dest_time, src, dest, src_pg, dest_pg);
    -- If nonconvex
    ELSE
        -- intialize array values of convex hull regions
        SELECT  get_arr_segment(create_array_pg(ST_ConvexHull(src_reg))), 
                get_arr_segment(create_array_pg(ST_ConvexHull(dest_reg))),
                get_arr_pg(create_array_pg(ST_ConvexHull(src_reg))), 
                get_arr_pg(create_array_pg(ST_ConvexHull(dest_reg)))
        INTO src_chull, dest_chull, src_pg_chull, dest_pg_chull;
        -- progress angle = changed based in convexhull progress angle
        SELECT get_new_pg_ori(src, pg_segment(src_pg_chull, src_chull)) INTO src_pg;
        SELECT get_new_pg_ori(dest, pg_segment(dest_pg_chull, dest_chull)) INTO dest_pg;

        RETURN interpolate_simple(src_time, dest_time, src, dest, src_pg, dest_pg);
    END IF;
END
$$ LANGUAGE plpgsql;


-- INTERPOLATE SIMPLE REGION
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
