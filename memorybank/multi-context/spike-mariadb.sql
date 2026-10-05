DROP DATABASE IF EXISTS mc_spike_registry;
DROP DATABASE IF EXISTS mc_spike_permits;
CREATE DATABASE mc_spike_registry;
CREATE DATABASE mc_spike_permits;
CREATE TABLE mc_spike_registry.address (person VARCHAR(20), address VARCHAR(20)) ENGINE=InnoDB;
CREATE TABLE mc_spike_permits.applicant (permit VARCHAR(20), person VARCHAR(20)) ENGINE=InnoDB;
INSERT INTO mc_spike_registry.address VALUES ('Peter', 'Main Street 1');
INSERT INTO mc_spike_permits.applicant VALUES ('permit1', 'Peter'), ('permit2', 'Anna');
-- 1. one query over two databases: the violations of the rule "located"
SELECT 'cross-database query' AS test, a.permit, a.person
FROM mc_spike_permits.applicant a LEFT JOIN mc_spike_registry.address r ON a.person = r.person
WHERE r.person IS NULL;
-- 2. one transaction over two databases, rolled back
START TRANSACTION;
INSERT INTO mc_spike_registry.address VALUES ('Anna', 'Side Street 2');
INSERT INTO mc_spike_permits.applicant VALUES ('permit3', 'Anna');
ROLLBACK;
SELECT 'after rollback' AS test,
  (SELECT COUNT(*) FROM mc_spike_registry.address) AS registry_rows,
  (SELECT COUNT(*) FROM mc_spike_permits.applicant) AS permits_rows;
-- 3. grants: a user with all rights on permits and select on registry
DROP USER IF EXISTS 'mc_permits'@'%';
CREATE USER 'mc_permits'@'%' IDENTIFIED BY 'x';
GRANT ALL ON mc_spike_permits.* TO 'mc_permits'@'%';
GRANT SELECT ON mc_spike_registry.* TO 'mc_permits'@'%';
