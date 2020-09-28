
# Moving Region PostgreSQL Extension

An extension of PostgreSQL to query and process Moving Region data. Built using PostgreSQL features with PostGIS extension using PL/pgSQL Language.

## To Use Moving Region Extension

- #### Files needed
	- *moving_region.control*
	- *moving_region--1.0.sql*
	> The file ***moving_region.control*** must be located in your `SHAREDIR/extension` directory, for example `/usr/local/share/postgresql/extension`
	> The file ***moving_region--1.0.sql*** must be located in `SHAREDIR/extension/moving_region` directory.
	
- #### Command
	 `CREATE EXTENSION moving_region` 
	> Apply this command to your database
	