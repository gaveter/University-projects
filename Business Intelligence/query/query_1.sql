-- Distribuzione del numero degli studenti iscritti nei vari appelli, suddivisa per anni e per corso di laurea

----------------------------
--- TABELLA  NORMALIZZATA---
----------------------------
SELECT cds.cds,
       app.Anno_accademico,
       count(isc.appcod) AS Numero_Studenti
  FROM cds
       JOIN
       appelli AS app ON cds.cdscod = app.cdscod
       JOIN
       iscrizioni AS isc ON app.appcod = isc.appcod
 GROUP BY cds.cds, app.Anno_accademico
 ORDER BY Numero_Studenti;


-----------------------------
-- TABELLA DENORMALIZZATA ---
-----------------------------
SELECT Cds, Anno_accademico, count(*) as Numero_Studenti
from bos_denormalizzato as bos
group by Anno_accademico, Cds
ORDER BY Numero_Studenti;





