#!/usr/bin/env bash
# Generate "base address" identical to what Keystone generates 
# given some address index.
# The address includes both payment and stake parts
# Mnemonics are read from the stdin, address is output to stdout

INDEX=0

if [[ $# -eq 1 ]]; then
  INDEX=$1
fi

ROOT_SK=$(cardano-address key from-recovery-phrase Shelley)
STAKE_VK=$(cardano-address key child 1852H/1815H/0H/2/0 <<< ${ROOT_SK} | cardano-address key public --without-chain-code)
ADDR_VK=$(cardano-address key child 1852H/1815H/0H/0/${INDEX} <<< ${ROOT_SK} | cardano-address key public --without-chain-code)
PAYMENT_ADDR=$(cardano-address address payment --network-tag mainnet <<< ${ADDR_VK})
cardano-address address delegation ${STAKE_VK} <<< ${PAYMENT_ADDR}

