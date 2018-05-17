# hpaie

Generate a [hledger](http://hledger.org/)-formatted file from a CSV-formatted flat list of expenses, distributing those over various keys.

## Usage

Given the following input file named `in.csv`:

```
Date;compte;libelle;sens;montant
2018-05-14;612000:KPMG;Frais tenu de comptes;D;120,00
2018-05-15;63000:Resto;Resto;D;180,00
```

when run, it outputs:

```
$ hpaie-exe in.csv out.ledger 801:Arnaud 802:Fred 803:Bernard
              120.00  612000:KPMG
              180.00  63000:Resto
             -100.00  801:Arnaud
             -100.00  802:Fred
             -100.00  803:Bernard
--------------------
                   0
```

The `out.ledger` file contains:

```
2018-05-14 Frais tenu de comptes
    612000:KPMG          120.00
    801:Arnaud           -40.00
    802:Fred             -40.00
    803:Bernard          -40.00
2018-05-15 Resto
    63000:Resto          180.00
    801:Arnaud           -60.00
    802:Fred             -60.00
    803:Bernard          -60.00
```


## TODO

* [x] ensure all transactions are balanced
