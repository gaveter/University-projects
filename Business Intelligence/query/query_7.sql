-- Scoprire  l'appello (mese) in cui è più facile superare un determinato esame --

----------------------------
--- TABELLA  NORMALIZZATA---
----------------------------
WITH promossi as
(
	SELECT c.Cds,a.AD,strftime('%m',ap.dtappello) as mese, COUNT(Superamento) as superati
	FROM cds as c JOIN appelli as ap
	ON c.cdscod = ap.cdscod
	JOIN ad as a
	ON a.adcod=ap.adcod
	JOIN iscrizioni as i
	ON ap.appcod=i.appcod
	WHERE Superamento == 1
	GROUP BY c.Cds,a.AD,mese
	ORDER BY c.Cds,a.AD,superati
),
iscritti as
(
	SELECT c.Cds,a.AD,strftime('%m',ap.dtappello) as mese, COUNT(Iscrizione) as iscr
	FROM cds as c JOIN appelli as ap
	ON c.cdscod = ap.cdscod
	JOIN ad as a
	ON a.adcod=ap.adcod
	JOIN iscrizioni as i
	ON ap.appcod=i.appcod
	GROUP BY c.Cds,a.AD,mese
	HAVING iscr > 3
	ORDER BY c.Cds,a.AD
)
SELECT p.Cds,p.AD,p.mese,p.superati,i.iscr, ROUND(cast(p.superati as float) / cast(i.iscr as float),2) AS percentuale
FROM promossi as p JOIN iscritti as i
ON p.Cds=i.CdS AND p.AD=i.AD AND p.mese=i.mese
ORDER BY p.Cds,p.AD,percentuale


-----------------------------
-- TABELLA DENORMALIZZATA ---
-----------------------------
WITH promossi as
(
	SELECT CdS,AD,strftime('%m',dtAppello) AS mese,COUNT(Superamento) AS superati
	from bos_denormalizzato
	WHERE Superamento == 1
	GROUP BY CdS,AD,strftime('%m', dtAppello)
),
iscritti as
(
	SELECT CdS,AD,strftime('%m', dtAppello) as mese,COUNT(Iscrizione) as iscr
	FROM bos_denormalizzato
	GROUP BY CdS,AD,strftime('%m', dtAppello)
	HAVING iscr > 3
)
SELECT p.Cds,p.AD,p.mese,p.superati,i.iscr, ROUND(cast(p.superati as float) / cast(i.iscr as float),2) AS percentuale
FROM promossi as p JOIN iscritti as i
			ON p.Cds=i.CdS AND p.AD=i.AD AND p.mese=i.mese
ORDER BY p.Cds,p.AD,percentuale