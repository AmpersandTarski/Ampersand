-- Grant root access from any host with empty password
GRANT ALL PRIVILEGES ON *.* TO 'root'@'%' IDENTIFIED BY '' WITH GRANT OPTION;
FLUSH PRIVILEGES;
