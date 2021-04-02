-- Normalizzato complessivo (intesa come tutta l'università)
--- Dato un esame, il rispettivo valore trial&error è dato dalla media del numero di tentativi (bocciature) di ogni studente per quell'esame

WITH bocciatura AS (
SELECT *,  (1.0*(SUM(isc.Ritiro) + SUM(isc.Insufficienza)))/count(*) as trial_and_error_stud
FROM cds JOIN
		  appelli as app on cds.cdscod = app.cdscod 
		  JOIN ad on ad.adcod = app.adcod 
		  JOIN iscrizioni as isc on isc.appcod = app.appcod
WHERE isc.Assenza = 0
GROUP BY cds.cds, ad.ad, isc.studente
),

media AS (
SELECT cds, ad,  avg(trial_and_error_stud) as trial_and_error, count(*) as x,
			row_number() OVER (PARTITION BY cds ORDER BY avg(trial_and_error_stud) DESC) as Rownum
FROM bocciatura
GROUP BY cds, ad
HAVING x > 3
)

SELECT cds, ad, trial_and_error, Rownum
FROM media
WHERE Rownum <= 3
ORDER BY cds, trial_and_error DESC;


--- Denormalizzato

WITH bocciatura AS (
SELECT *,  (1.0*(SUM(Ritiro) + SUM(Insufficienza)))/count(*) as trial_and_error_stud
FROM bos_denormalizzato
WHERE Assenza = 0
GROUP BY cds, ad, studente
),

media AS (
SELECT cds, ad,  avg(trial_and_error_stud) as trial_and_error, count(*) as x,
			row_number() OVER (PARTITION BY cds ORDER BY avg(trial_and_error_stud) DESC) as Rownum
FROM bocciatura
GROUP BY cds, ad
HAVING x > 3
)

SELECT cds, ad, trial_and_error, Rownum
FROM media
WHERE Rownum <= 3
ORDER BY cds, trial_and_error DESC;






