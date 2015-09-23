DROP TABLE IF EXISTS stations.locations;
CREATE TABLE stations.locations (
STATION		VARCHAR(100)
,LON		VARCHAR
,LAT		VARCHAR
);
COPY stations.locations(station, lon, lat) FROM 'C:\Users\Jleach1\Documents\oyster\data\stations.csv' DELIMITER ',' QUOTE '"' HEADER CSV;

/*
select * from stations.locations
*/