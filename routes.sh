SERVER=192.168.0.26 &&
USER=root &&
PASSWORD=MySql123 && 
DB=map &&
TABLE=route && # MySql access data
mysql -h $SERVER -u $USER -p$PASSWORD -e " \
CREATE DATABASE IF NOT EXISTS $DB \
  DEFAULT CHARACTER SET utf8 \
  DEFAULT COLLATE utf8_general_ci; \
USE $DB; \
DROP TABLE IF EXISTS $TABLE; \
CREATE TABLE $TABLE ( \
  id BIGINT NOT NULL PRIMARY KEY, \
  number VARCHAR(10), \
  type VARCHAR(10), \
  start VARCHAR(255), \
  end VARCHAR(255)
);" && # setup database (if necessary) and table
FIFO_BASE=`mktemp` && FIFO=$FIFO_BASE.fifo && # setup named pipe used for MySql LOAD DATA input instead of file
mkfifo $FIFO && ( \
awk -F", |: | => " -v OFS="," 'FNR > 1 {print $1, $2, $3, $4}' routes.csv | # ignoring the first header line, split into id, combined type/number, start and end
awk -F"," -v OFS="," '{split($2, a, " "); print $1, a[1] "," a[2], $3, $4}' > $FIFO & # split the combined type/number field and redirect results into named pipe and wait in background for data consumption
mysql -h $SERVER -u $USER -p$PASSWORD --local-infile -e " \
LOAD DATA LOCAL INFILE '$FIFO' INTO TABLE $DB.$TABLE \
  FIELDS TERMINATED BY ',' \
  LINES TERMINATED BY '\n';" \
) && # load data from named pipe into table
\rm -f $FIFO_BASE ; \rm -f $FIFO # clean up temp files
