--Normalizzato
-- voti più bassi

WITH iscr
AS
(
SELECT *, count(*) as Iscritti
  FROM cds
       JOIN
       appelli AS app ON cds.cdscod = app.cdscod
       JOIN
       ad ON ad.adcod = app.adcod
       JOIN
       iscrizioni AS isc ON isc.appcod = app.appcod
 GROUP BY cds.cds,
				 ad.ad
 HAVING Iscritti > 3
),

bottom
AS
(
SELECT cds.cds as corso,
       ad.ad,
       round(avg(isc.Voto),2) AS media_voti
  FROM cds
       JOIN
       appelli AS app ON cds.cdscod = app.cdscod
       JOIN
       ad ON ad.adcod = app.adcod
       JOIN
       iscrizioni AS isc ON isc.appcod = app.appcod
 WHERE isc.Voto IS NOT NULL
 GROUP BY cds.cds,
          ad.ad
),

final AS (
SELECT iscr.cds, bottom.ad, bottom.media_voti, row_number() OVER (PARTITION BY iscr.cds ORDER BY bottom.media_voti) as Rownum
FROM bottom JOIN
		  iscr ON bottom.ad = iscr.ad AND bottom.corso = iscr.cds
ORDER BY cds, media_voti

)
SELECT *
FROM final
WHERE Rownum <= 3
ORDER BY cds, media_voti;


--Più alti
WITH iscr
AS
(
SELECT *, count(*) as Iscritti
  FROM cds
       JOIN
       appelli AS app ON cds.cdscod = app.cdscod
       JOIN
       ad ON ad.adcod = app.adcod
       JOIN
       iscrizioni AS isc ON isc.appcod = app.appcod
 GROUP BY cds.cds,
				 ad.ad
 HAVING Iscritti > 3
),

top
AS
(
SELECT cds.cds as corso,
       ad.ad,
       round(avg(isc.Voto),2) AS media_voti
  FROM cds
       JOIN
       appelli AS app ON cds.cdscod = app.cdscod
       JOIN
       ad ON ad.adcod = app.adcod
       JOIN
       iscrizioni AS isc ON isc.appcod = app.appcod
 WHERE isc.Voto IS NOT NULL
 GROUP BY cds.cds,
          ad.ad
),

final AS (
SELECT iscr.cds, top.ad, top.media_voti, row_number() OVER (PARTITION BY iscr.cds ORDER BY top.media_voti DESC) as Rownum
FROM top JOIN
		  iscr ON top.ad = iscr.ad AND top.corso = iscr.cds
ORDER BY cds, media_voti

)

SELECT *
FROM final
WHERE Rownum <= 3
ORDER BY cds, media_voti DESC;


-- Denormalizzato
-- voti più bassi

WITH iscr
AS
(
SELECT *, count(*) as Iscritti
  FROM bos_denormalizzato
 GROUP BY cds,
				 ad
 HAVING Iscritti > 3
),

bottom
AS
(
SELECT cds as corso,
       ad,
       round(avg(Voto),2) AS media_voti
  FROM bos_denormalizzato
 WHERE Voto IS NOT NULL
 GROUP BY cds,
				 ad
),

final AS (
SELECT iscr.cds, bottom.ad, bottom.media_voti, row_number() OVER (PARTITION BY iscr.cds ORDER BY bottom.media_voti) as Rownum
FROM bottom JOIN
		  iscr ON bottom.ad = iscr.ad AND bottom.corso = iscr.cds
ORDER BY cds, media_voti

)

SELECT *
FROM final
WHERE Rownum <= 3
ORDER BY cds, media_voti;

-- voti più alti

WITH iscr
AS
(
SELECT *, count(*) as Iscritti
  FROM bos_denormalizzato
 GROUP BY cds,
				 ad
 HAVING Iscritti > 3
),

top
AS
(
SELECT cds as corso,
            ad,
			round(avg(Voto),2) AS media_voti
  FROM bos_denormalizzato
 WHERE Voto IS NOT NULL
 GROUP BY cds,
				 ad
),

final AS (
SELECT iscr.cds, top.ad, top.media_voti, row_number() OVER (PARTITION BY iscr.cds ORDER BY top.media_voti DESC) as Rownum
FROM top JOIN
		  iscr ON top.ad = iscr.ad AND top.corso = iscr.cds
ORDER BY cds, media_voti

)

SELECT *
FROM final
WHERE Rownum <= 3
ORDER BY cds, media_voti DESC;