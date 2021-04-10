# hpaie

Generate a [hledger](http://hledger.org/)-formatted file from a tab-separated flat list of expenses, distributing those over various keys.

## Usage

* Demander au comptable le grand livre à la date de clôture souhaitée au format Excel
  Le grand-livre doit avoir les champs suivants:

  ```
  Compte	Journal	Date	Piece	Libelle	RefLibelle	Reference	Debit	Credit	Solde
  10100000	AN	01/10/2018		A-nouveaux au 01/10/2018	Capital		0	4000	-4000
  10610000	OD	16/03/2019	49	AGO 16.03.19	Réserve légale		0	1780.2	-1780.2
  ```

    * NOTE: Le délimiteur utilisé peut-être des TABs ou des points-virgules. Dans tous les cas il est probable que l'extension soit `.csv` ce qui n'a pas de sens mais bon...
* Transfomer le grand-livre en un autre fichier `.csv` permettant de procéder à la répartition des charges et produits, en appliquant des règles de répartitions pré-définies pour simplifier le travail:

  ```
  $ assign-keys -i grand-livre-input.tsv -r rules -o grand-livre-output.tsv
  ```

  Cette étape génère un autre fichier `tsv` possédant la structure suivante, avec les mêmes écritures:

  ```
  Date	compte	libelle	sens	montant	keys	Description
  24/01/2019	60600000:Achats non stockés de matières et fournitures	UPDOWN	D	5,00	ALL
  ```

  La colonne `keys` contient soit le nom des personnes auxquelles assigner l'écriture (séparées par des virgules), soit `ALL` si l'écriture concerne tout le monde
* Le programme `assign-keys` attend un fichier encodé en UTF-8, et il fort probable que le grand livre initial soit encodé en ISO-8859-1 (Windows...):
  ```
  $ assign-keys -i grand-livre-input.tsv -r rules -o grand-livre-output.tsv
  assign-keys: user error (parse error (Failed reading: conversion error: Cannot decode byte '\xe9': Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream) at
  40110000;ACH;30/04/2020;;Mois Avril 2020;Fournisseurs;;0;966.38;-966.38
  40110000;OD;30/04/2020;;Moi (truncated))
  ```
  Dans ce cas, il est nécessaire de procéder à une conversion, par exemple en utilisant [iconv](https://linux.die.net/man/1/iconv):
  ```
  $ iconv -f iso-8859-1 -t utf-8 exportGranLivreGenerale\ \(22\).csv > export-grand-livre.csv
  ```
  
* Par défaut, le programme `assign-keys` attend un fichier délimité par des tabulations. Pour adapter le caractère de délimitation des champs, utiliser l'option `-d`:
  ```
  $ assign-keys -i export-grand-livre.csv -o export-assign.csv -r rules -d ';'
  ```
* Procéder à la répartition, en uploadant le fichier `export-assign.csv`  sur Google Drive parce que c'est plus simple pour éditer
  Notes: seules les écritures de charges et produits (comptes 6 et 7) doivent être réparties
* télécharger le résultat et appliquer la transformation au format ledger:

  ```
  $ gen-ledger -i grand-livre-output.tsv -o grand-livre.ledger
  ```

  le programme affiche par ailleurs le compte de résultat détaillé, avec la répartition par compte analytique.

* Générer un fichier `CSV` à partir de ces écritures et le transmettre au comptable

  ```
  $ hledger -f grand-livre.ledger register -o journal-analytique.csv
  ```
