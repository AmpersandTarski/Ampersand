FROM mariadb:10.1

ENV MYSQL_ROOT_PASSWORD abc
ENV MYSQL_USER ampersand
ENV MYSQL_PASSWORD ampersand

RUN echo "REVOKE ALL PRIVILEGES ON *.* FROM 'ampersand'@'%'; GRANT ALL PRIVILEGES ON *.* TO 'ampersand'@'%' REQUIRE NONE WITH GRANT OPTION MAX_QUERIES_PER_HOUR 0 MAX_CONNECTIONS_PER_HOUR 0 MAX_UPDATES_PER_HOUR 0 MAX_USER_CONNECTIONS 0;" >> /docker-entrypoint-initdb.d/01_grant_ampersand.sql \
 && echo "SET GLOBAL sql_mode = 'ANSI,TRADITIONAL';" >> /docker-entrypoint-initdb.d/02_set_sql_mode.sql
