# Elforbrug i danske kommuner

### En analyse af kommunale forskelle i privat elforbrug i 2024

Hvorfor er det registrerede private elforbrug pr. indbygger højere i nogle danske kommuner end i andre?

Dette projekt undersøger sammenhængen mellem elforbrug og udvalgte demografiske, økonomiske og boligmæssige karakteristika i Danmarks 98 kommuner.

Analysen er gennemført i **R** og dokumenteret i en **Quarto-rapport**.

**[Læs den fulde analyserapport](https://istplum.github.io/Elforbrug/report.html)**

## Datagrundlag

Analysen anvender kommunale data fra 2024, herunder:

- Registreret privat elforbrug pr. indbygger
- Andelen af indbyggere på 65 år eller derover
- Gennemsnitlig disponibel familieindkomst
- Befolkningstæthed
- Elbilernes andel af personbilbestanden
- Andelen af boliger med elbaseret opvarmning
- Antallet af fritidshuse pr. 1.000 fastboende

Data stammer fra offentlige datakilder, herunder Danmarks Statistik og Energi Data Service.

## Metode

De kommunale forskelle undersøges ved hjælp af multipel lineær regression (OLS).

Analysen omfatter:

- Indsamling og bearbejdning af kommunale data i R
- Konstruktion og operationalisering af forklaringsvariable
- Estimation af regressionsmodeller
- Modeldiagnostik og sensitivitetsanalyser
- HC3-robuste standardfejl
- Standardisering af forklaringsvariable til sammenligning af partielle sammenhænge
- Visualisering og formidling af resultater i Quarto

## Hovedresultater

Analysen viser en særlig stærk positiv sammenhæng mellem antallet af fritidshuse pr. fastboende og det registrerede private elforbrug pr. indbygger.

Der findes desuden positive partielle sammenhænge med elbaseret opvarmning, elbilandel, andelen af ældre og disponibel indkomst, mens befolkningstæthed har en negativ partiel sammenhæng med elforbruget.

Resultaterne er statistiske sammenhænge og kan ikke fortolkes som kausale effekter.

En væsentlig metodisk begrænsning er, at fritidshusenes elforbrug kan indgå i det registrerede forbrug, selvom fritidshusenes brugere ikke nødvendigvis indgår i kommunens fastboende befolkning.

## Projektets filer

- `R/get_data.R` – dataindsamling
- `R/analysis.R` – databehandling, regressionsmodeller og modeldiagnostik
- `report.qmd` – Quarto-rapportens kildefil
- `report.html` – den færdige rapport
- `data/municipality_data.csv` – det samlede kommunale analysedatasæt

## Reproducerbarhed

Projektet er udviklet i R og RStudio. Koden og datagrundlaget er tilgængelige i dette repository.

Rapporten er genereret med Quarto, så analyseresultater, tabeller og figurer kan opdateres direkte fra R-koden.

## Brug af AI i projektet

Under arbejdet med projektet har jeg anvendt ChatGPT som læringsværktøj og faglig sparringspartner. AI er blandt andet blevet brugt til at forklare metoder og R-kode, hjælpe med fejlfinding, diskutere statistiske problemstillinger og understøtte udformningen af kode og rapporttekst.

Arbejdsprocessen har været tilrettelagt med fokus på læring og selvstændig forståelse frem for automatiseret analyse. Jeg har løbende arbejdet med at forstå, afprøve og vurdere de foreslåede løsninger og har selv taget stilling til analysens metodiske valg, fortolkninger og konklusioner.

AI har således været et hjælpemiddel i analyse- og skriveprocessen, mens ansvaret for projektets indhold og resultater er mit eget.