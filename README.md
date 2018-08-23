# hpaie

Generate a [hledger](http://hledger.org/)-formatted file from a CSV-formatted flat list of expenses, distributing those over various keys.

## Usage

Given the following input file named `in.csv`:

```
Date;compte;libelle;sens;montant
2018-05-14;612000:KPMG;Frais tenu de comptes;D;120,00;ALL
2018-05-15;63000:Resto;Resto;D;180,00;ALL
```

when run, it outputs:

```
$ stack exec hpaie-exe in.csv out.ledger 
              120.00  612000:KPMG
              180.00  63000:Resto
               30.00  63001:Hotel
               20.00  63002:Train
             -120.00  801000:Arnaud
             -130.00  802000:Bernard
             -100.00  803000:Fred
--------------------
                   0
```

The `out.ledger` file contains:

```
2018-05-14 Frais tenu de comptes
    612000:KPMG                                        120.00
    801000:Arnaud                                      -40.00
    802000:Bernard                                     -40.00
    803000:Fred                                        -40.00
2018-05-15 Resto d'equipe
    63000:Resto                                        180.00
    801000:Arnaud                                      -60.00
    802000:Bernard                                     -60.00
    803000:Fred                                        -60.00
2018-05-16 Mission pour Tesla
    63001:Hotel                                        30.00
    802000:Bernard                                     -30.00
2018-05-17 Mission pour Google
    63002:Train                                        20.00
    801000:Arnaud                                      -20.00

```


## TODO

* [x] ensure all transactions are balanced
