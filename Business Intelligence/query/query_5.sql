-- Calcolare la distribuzione degli studenti “fast&furious” per corso di studi, 
-- ovvero studenti con il rapporto “votazione media riportata negli esami superati” su “periodo di attività” maggiore.
-- Per periodo di attività si intende il numero di giorni trascorsi tra il primo appello sostenuto (non necessariamente superato) e l’ultimo

----------------------------
--- TABELLA  NORMALIZZATA---
----------------------------

WITH calcolo_media AS (
SELECT cds.cds, isc.studente, round(avg(isc.Voto),2) as Media, count(isc.Voto) as Numero_esami
FROM cds 
		 JOIN
		 appelli as app on cds.cdscod = app.cdscod
		 JOIN
		 iscrizioni as isc on app.appcod = isc.appcod
WHERE isc.Voto NOTNULL
GROUP BY isc.studente, cds.cds
),

calcolo_giorni AS (
SELECT studente, julianday(max(dtappello)) - julianday(min(dtappello)) as day
FROM cds 
		 JOIN
		 appelli as app on cds.cdscod = app.cdscod
		 JOIN
		 iscrizioni as isc on app.appcod = isc.appcod
GROUP BY isc.studente, cds.cds
)

SELECT cds, calcolo_media.studente, round(media/day, 4) as Result, calcolo_media.numero_esami
FROM calcolo_media 
		 JOIN 
		 calcolo_giorni on calcolo_media.studente = calcolo_giorni.studente
WHERE day != 0.0 AND day > 30  and calcolo_media.numero_esami  > 3
ORDER BY calcolo_media.numero_esami DESC;
		 
-----------------------------
-- TABELLA DENORMALIZZATA ---
-----------------------------
WITH calcolo_media AS (
SELECT cds, studente, round(avg(Voto),2) as Media, count(Voto) as numero_esami
FROM bos_denormalizzato
WHERE Voto NOTNULL 
GROUP BY studente, cds
),

calcolo_giorni AS (
SELECT studente, julianday(max(dtappello)) - julianday(min(dtappello)) as day
FROM bos_denormalizzato
GROUP BY studente, cds
)

SELECT cds, calcolo_media.studente, round(media/day, 4) as Result, numero_esami
FROM calcolo_media 
		 JOIN 
		 calcolo_giorni on calcolo_media.studente = calcolo_giorni.studente
WHERE Result != 0.0 AND day != 0.0 AND day > 30  and calcolo_media.numero_esami  > 3
ORDER BY calcolo_media.numero_esami DESC;
