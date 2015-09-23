DROP TABLE IF EXISTS stations.locations;
CREATE TABLE stations.locations (
STATION		VARCHAR(100)
,LAT		numeric(10,8)
,LON		numeric(10,8)
);
COPY stations.locations(station, lat, lon) FROM 'C:\Users\Jleach1\Documents\oyster\data\stations.csv' DELIMITER ',' CSV;