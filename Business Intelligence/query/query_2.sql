--Normalizzato

WITH iscritti_

AS
(
SELECT cds.cds, ad.ad, count(*) as iscritti, app.Anno_accademico
FROM cds JOIN appelli as app on cds.cdscod = app.cdscod join ad on ad.adcod = app.adcod join iscrizioni as isc on isc.appcod = app.appcod
GROUP BY cds.cdscod, ad.adcod, app.Anno_accademico
ORDER BY cds.cds
),

 passati_
AS
(
SELECT cds.cds, ad.ad, count(*) as passati,  app.Anno_accademico
FROM cds JOIN appelli as app on cds.cdscod = app.cdscod join ad on ad.adcod = app.adcod join iscrizioni as isc on isc.appcod = app.appcod
WHERE isc.Superamento == 1
GROUP BY cds.cds, ad.ad, app.Anno_accademico
),

complex

AS
(
SELECT iscritti_.cds,
	   iscritti_.ad,
	   iscritti_.Anno_accademico,
	   round(passati_.passati*1.0/iscritti_.iscritti, 4) * 100 as Tasso,
	   row_number() OVER (PARTITION BY iscritti_.cds, iscritti_.Anno_accademico ORDER BY round(passati_.passati*1.0/iscritti_.iscritti, 4) * 100 ) as Rownum_cds
FROM iscritti_ JOIN passati_ on iscritti_.cds = passati_.cds AND iscritti_.ad = passati_.ad AND iscritti_.Anno_accademico = passati_.Anno_accademico
WHERE iscritti_.iscritti > 3
)

SELECT cds, ad, Anno_accademico, Tasso, Rownum_cds
FROM complex
WHERE Rownum_cds <=10
ORDER BY complex.cds, complex.Anno_accademico, complex.Tasso;

-- Denormalizzato

WITH promossiCorso as
(
	SELECT CdS,AD,COUNT(Superamento) as promossi, Anno_accademico
	FROM bos_denormalizzato
	WHERE Superamento =  1
	GROUP BY CdS, AD, Anno_accademico
),
partecipanti as
(
SELECT cds, cdscod, ad, adcod, count(*) as iscritti, Anno_accademico
FROM bos_denormalizzato
GROUP BY cds, ad, Anno_accademico
ORDER BY cds
),
promossiPartecipanti as
(
	SELECT pr.CDS,pr.AD,pr.promossi,par.iscritti, pr.Anno_accademico, ROUND(CAST(pr.promossi as float) / CAST(par.iscritti as float), 4)*100 AS tasso
	FROM promossiCorso as pr
	JOIN partecipanti as par
	ON pr.Cds=par.CdS AND pr.AD=par.AD AND pr.Anno_accademico = par.Anno_accademico
	WHERE par.iscritti > 3
	GROUP BY pr.CdS, pr.AD, pr.Anno_accademico
),
tassoEsami as
(
SELECT pp.CDS,pp.AD, pp.Anno_accademico, pp.tasso,
	row_number() OVER (
	PARTITION BY CdS, Anno_accademico
	ORDER BY tasso) ranktasso
FROM promossiPartecipanti as pp
group by CdS,AD, pp.Anno_accademico
)

SELECT *
FROM tassoEsami
WHERE ranktasso <= 10
ORDER BY cds, Anno_accademico, tasso;