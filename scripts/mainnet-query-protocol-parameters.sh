#!/bin/bash
cardano-cli query protocol-parameters \
    --mainnet \
    --out-file "mainnet-protocol-parameters.json"
