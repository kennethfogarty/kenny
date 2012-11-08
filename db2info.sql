--
-- THIS QUERY CAN BE USED TO GIVE YOU THE CURRENT DB2 LEVEL AND MODE OF A SUBSYSTEM
--
SELECT
   DATE(CURRENT_TIMESTAMP) AS DATE,
   TIME(CURRENT_TIMESTAMP) AS TIME,
   CAST(GETVARIABLE('SYSIBM.SYSTEM_NAME') AS CHAR(4)) AS SYSTEM,
   CASE CAST(SUBSTR(GETVARIABLE(SYSIBM.VERSION'),4,2) AS CHAR(2)
      WHEN '08' THEN 'DB2 V8'
      WHEN '09' THEN 'DB2 V9'
      WHEN '10' THEN 'DB2 10'
      ELSE 'CHECK QUERY'
   END AS VERSION,
   CASE(CAST(SUBSTR(GETVARIABLE(SYSIBM.VERSION),8,1) AS INT)
      WHEN 0 THEN 'COMPATIBILITY MODE'
      WHEN 1 THEN 'COMPATIBILITY MODE'
      WHEN 2 THEN 'COMPATIBILITY MODE'
      WHEN 3 THEN 'COMPATIBILITY MODE'
      WHEN 4 THEN 'COMPATIBILITY MODE'
      ELSE 'NEW FUNCTION MODE'
   END AS MODE
   FROM
   SYSIBM.SYSDUMMY1;
   COMMIT;
