cd /usr/local/hadoop

sbin/start-dfs.sh

bin/hadoop fs -ls /

bin/hadoop fs -put /
bin/hadoop fs -get /

sbin/stop-dfs.sh

sbin/start-yarn.sh
sbin/stop-yarn.sh