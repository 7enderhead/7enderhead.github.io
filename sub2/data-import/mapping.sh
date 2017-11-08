SERVER=192.168.0.26 &&
USER=root &&
PASSWORD=MySql123 && 
DB=map &&
TABLE=mapping && # MySql access data
mysql -h $SERVER -u $USER -p$PASSWORD --local-infile -e " \
CREATE DATABASE IF NOT EXISTS $DB \
  DEFAULT CHARACTER SET utf8 \
  DEFAULT COLLATE utf8_general_ci; \
USE $DB; \
DROP TABLE IF EXISTS $TABLE; \
CREATE TABLE $TABLE ( \
  route_id BIGINT, \
  stop_id BIGINT \
); \
LOAD DATA LOCAL INFILE 'mapping.csv' INTO TABLE $DB.$TABLE \
  FIELDS TERMINATED BY '|' \
  LINES TERMINATED BY '\n' \
  IGNORE 1 LINES;"
