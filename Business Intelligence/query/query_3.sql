-- Individuazione dei corsi di laurea ad elevato tasso di commitment, 
-- ovvero appelli di esami diversi ma del medesimo corso di laurea che
-- si sono svolti nello stesso giorno

----------------------------
--- TABELLA  NORMALIZZATA---
----------------------------
SELECT 
	   cds.cds,
       count( DISTINCT app.adcod ) AS Numero_Appelli,
       app.dtappello
  FROM cds
       JOIN
       appelli AS app ON cds.cdscod = app.cdscod
 GROUP BY cds.cds,
          app.dtappello
 HAVING Numero_Appelli > 1
 ORDER BY Numero_Appelli DESC;


-----------------------------
-- TABELLA DENORMALIZZATA ---
-----------------------------
SELECT
	   bd.CdS,
       count(DISTINCT Ad) AS Numero_Appelli,
       bd.DtAppello
  FROM bos_denormalizzato AS bd
 GROUP BY bd.CdS,
          bd.DtAppello
 HAVING Numero_Appelli > 1
 ORDER BY Numero_Appelli DESC;







