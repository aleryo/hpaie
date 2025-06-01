# legal

## Usage

Generate a pair of simple keys to interact with the chain:

```sh
> cardano-cli address key-gen --verification-key-file test.vk --signing-key-file test.sk
```

Compute the corresponding address (here on Preprod network):

```sh
> cardano-cli conway address build --testnet-magic 1 --payment-verification-key-file test.vk | tee test.addr
addr_test1vr69qqlwgw7jty8m2wqvyxytnntufxhcjjur44rqd3t4hfgfq2ne0
```

Send this address some funds, e.g through a [Testnet Faucet]() or from an existing wallet.
This address should hold at least one UTxO with enough funds (~10 ADAs for `Hello, World!` contract). This can be checked with:

```sh
> cardano-cli conway query utxo --address addr_test1vr69qqlwgw7jty8m2wqvyxytnntufxhcjjur44rqd3t4hfgfq2ne0 --testnet-magic 1 --socket-path node.socket
2178565983a3125c608f7dea381d5b70d064be6b21879de86d4765e8ad3361f1     0        10000000 lovelace + TxOutDatumNone
```

assuming a `cardano-node` is started and exposing a local socket at path `node.socket`.

### Locking funds into smart contract

Generate script file for use by cardano-cli:

```sh
> aiken blueprint convert > hello.script
```

> [!WARNING]
> The `compiledCode` field in the blueprint file `plutus.json` cannot be used as-is because the cardano-cli uses a C BOR-in-CBOR encoding wrapped in a simple `TextEnvelope` JSON file. `aiken blueprint convert` correctly encodes the script.

Compute script's address:

```sh
> cardano-cli address build --testnet-magic 1 --payment-script-file hello.script | tee hello.addr
addr_test1wqd988jgwwa5kjc2q4e03rrnrvqvlwz6c7wlyazymhd87mc2x3pjs
```

Compute public key hash that will be authorized to unlock the funds at the contract and is passed as _datum_:

```sh
cardano-cli conway address build --testnet-magic 1 --payment-verification-key-file test.vk | cardano-address address inspect | jq -r .spending_key_hash | tee test.hash
f45003ee43bd2590fb5380c2188b9cd7c49af894b83ad4606c575ba5
```

Create datum JSON file, following the datum schema from the blueprint:

```sh
> jq -c '{constructor:0,fields:[{bytes:.}]}' < test.hash
{"constructor":0,"fields":[{"bytes":"f45003ee43bd2590fb5380c2188b9cd7c49af894b83ad4606c575ba5"}]}
```

Build the locking transaction, tying together the various pieces above:

```sh
> cardano-cli conway transaction build \
      --tx-in 2178565983a3125c608f7dea381d5b70d064be6b21879de86d4765e8ad3361f1#0 \
      --tx-out $(cat hello.addr)+1100000 \
      --tx-out-inline-datum-file datum.json \
      --change-address $(cat test.addr) \
      --socket-path node.socket --testnet-magic 1 \
      --out-file tx.lock.raw
```

> [!NOTICE]
> Every UTxO requires a minimum Ada value to prevent users from abusing storage capabilities offered by the system. The 1100000 locked value in the output is a slight over approximation of the actual amount which is computed by the `build` command.

Then sign and submit it:

```sh
> cardano-cli conway transaction sign --tx-file tx.lock.raw --out-file tx.lock.signed --signing-key-file test.sk
> cardano-cli conway transaction submit --tx-file tx.lock.signed --socket-path node.socket --testnet-magic 1
```

### Unlocking funds

Compute the redeemer JSON file:

```sh
> jq -c '{constructor:0,fields:[{bytes:.}]}' <<< "\"$(echo 'Hello, World!' | xxd -g1 | cut -d ' ' -f2-14  | tr -d ' ')\"" | tee redeemer.json
{"constructor":0,"fields":[{"bytes":"48656c6c6f2c20576f726c6421"}]}
```

Check available UTxOs at spending address:

```sh
> cardano-cli conway query utxo --address addr_test1vr69qqlwgw7jty8m2wqvyxytnntufxhcjjur44rqd3t4hfgfq2ne0 --testnet-magic 1 --socket-path node.socket
25cf8f4e1e269672cdecb81f8576df3097a07198052ad1692548353304ccd1db     1        8654387 lovelace + TxOutDatumNone
```

Check UTxO locked at script's address:

```sh
> cardano-cli conway query utxo --address addr_test1vr69qqlwgw7jty8m2wqvyxytnntufxhcjjur44rqd3t4hfgfq2ne0 --testnet-magic 1 --socket-path node.socket --output-json
{
  "2842d9914aa07933fee6ac539be51ed19aa8f4d65473bf1e7afc30ba16536f79#0": {
    "address": "addr_test1wqd988jgwwa5kjc2q4e03rrnrvqvlwz6c7wlyazymhd87mc2x3pjs",
    "datum": null,
    "inlineDatum": {
      "constructor": 0,
      "fields": [
        {
          "bytes": "f45003ee43bd2590fb5380c2188b9cd7c49af894b83ad4606c575ba5"
        }
      ]
    },
    "inlineDatumRaw": "d8799f581c52ad33a108902a09926474a08bbd42bd8ab5155f3c007a6ff6747d37ff",
    "inlineDatumhash": "8a27f6e6fd3b1c07f86480306e3edfced1f799bea8c3215dd89eb16bbca386b0",
    "referenceScript": null,
    "value": {
      "lovelace": 1100000
    }
  }
}
```

Extract protocol parameters from node:

```sh
cardano-cli conway query protocol-parameters --testnet-magic 1 --socket-path node.socket > pparams.json
```

The protocol parameters are indispensable for:
1. computing the transaction fees
2. adding corresponding hash to the transaction for double-checking purpose (ag. avoid `PPViewHashesDontMatch` error)

Compute the execution units for unlocking (executing) the validator:

```sh
> aiken check
    Compiling pankzsoft/legal 0.0.0 (.)
    Compiling aiken-lang/stdlib v2.2.0 (./build/packages/aiken-lang-stdlib)
   Collecting all tests scenarios across all modules
      Testing ...

    ┍━ hello_world ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    │ PASS [mem: 32451, cpu: 11921833] hello_world_example
    │ · with traces
    │ | redeemer: Hello, World!
    ┕━━━━━━━━━━━━━━━━━━━━━━━ 1 tests | 1 passed | 0 failed
```

Build unlocking transaction, this time using `build-raw` command:

```sh
> cardano-cli conway transaction build-raw \
  --tx-in 25cf8f4e1e269672cdecb81f8576df3097a07198052ad1692548353304ccd1db#1 \
  --tx-in 2842d9914aa07933fee6ac539be51ed19aa8f4d65473bf1e7afc30ba16536f79#0 \
  --tx-in-collateral 25cf8f4e1e269672cdecb81f8576df3097a07198052ad1692548353304ccd1db#1 \
  --tx-in-script-file hello.script \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file redeemer.json \
  --tx-in-execution-units '(12000000,34000)' \
  --tx-out addr_test1vpf26vappzgz5zvjv362pzaag27c4dg4tu7qq7n07e686dsy5669m+$((8654387 + 1100000 - 1000000)) \
  --fee 1000000 \
  --protocol-params-file pparams.json \
  --out-file tx.unlock.raw \
  --required-signer ~/projects/cardano-wallet/scripts/tx-gen/txgen.sk
```

> [!IMPORTANT]
> We cannot use `build` command here because it will try to evaluate the script to compute the execution units and transaction fees, which will fail because the transaction is not yet signed and therefore the script will fail to check the transaction's signatories match its datum

The computed fee are almost certainly way too high, compute the minimum fees from the transaction:

```sh
cardano-cli conway transaction calculate-min-fee --tx-body-file tx.unlock.raw --protocol-params-file pparams.json --witness-count 1
198028 Lovelace
```

Then rerun the above command adjusting the fees and the change in the second output.

Finally, sign and submit the transaction:

```sh
> cardano-cli conway transaction sign --tx-file tx.unlock.raw --out-file tx.unlock.signed --signing-key-file test.sk
> cardano-cli conway transaction submit --tx-file tx.unlock.signed --socket-path node.socket --testnet-magic 1
```

## Coding Validators

See [validators](./validators/) directory.

### Building

```sh
aiken build
```

### Configuring

**aiken.toml**
```toml
[config.default]
network_id = 41
```

Or, alternatively, write conditional environment modules under `env`.

### Testing

You can write tests in any module using the `test` keyword. For example:

```aiken
use config

test foo() {
  config.network_id + 1 == 42
}
```

To run all tests, simply do:

```sh
aiken check
```

To run only tests matching the string `foo`, do:

```sh
aiken check -m foo
```

## Documentation

If you're writing a library, you might want to generate an HTML documentation for it.

Use:

```sh
aiken docs
```

## Resources

Find more on the [Aiken's user manual](https://aiken-lang.org).
