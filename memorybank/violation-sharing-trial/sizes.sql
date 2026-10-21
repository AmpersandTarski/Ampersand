SELECT table_schema, COUNT(*) AS tables, ROUND(SUM(data_length + index_length) / 1048576) AS mb,
       SUM(table_rows) AS approx_rows
FROM information_schema.tables
WHERE table_schema NOT IN ('mysql', 'information_schema', 'performance_schema', 'sys')
GROUP BY table_schema;
