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

* Télécharger le grand-livre au format `tsv`
* Transfomer le grand-livre en un fichier `tsv` permettant de procéder à la répartition des charges et produits:

  ```
  $ assign-keys grand-livre-input.tsv rules grand-livre-output.tsv
  ```

  Cette étape génère un autre fichier `tsv` possédant la structure suivante, avec les mêmes écritures:

  ```
  Date	compte	libelle	sens	montant	keys	Description
  24/01/2019	60600000:Achats non stockés de matières et fournitures	UPDOWN	D	5,00	ALL
  ```

  La colonne `keys` contient soit le nom des personnes auxquelles assigner l'écriture (séparées par des virgules), soit `ALL` si l'écriture concerne tout le monde
* Procéder à la répartition, en uploadant le fichier `grand-livre-output.tsv`  sur Google Drive parce que c'est plus simple pour éditer
  Notes: seules les écritures de charges et produits (comptes 6 et 7) doivent être répartis
* télécharger le résultat et appliquer la transformation au format ledger:

  ```
  $ gen-ledger -i grand-livre-output.tsv -o grand-livre.ledger
  ```

  le programme affiche par ailleurs le compte de résultat détaillé, avec la répartition par compte analytique.

* Générer un fichier `CSV` à partir de ces écritures et le transmettre au comptable

  ```
  $ hledger -f grand-livre.ledger register -o journal-analytique.csv
  ```
