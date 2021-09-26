#!/bin/bash
set -eux

baseDir=dirname "$0"


# arguments:
#   utxo
#   wallet address file
#   signing key file


# export CARDANO_NODE_SOCKET_PATH="/Users/jonathanfischoff/Library/Application Support/Daedalus Mainnet/cardano-node.socket"

bodyFile=nft-tx-body.01
outFile=nft-tx.01
nftPolicyFile="nft-mint-policy.plutus"
nftPolicyId=$($baseDir/policyid.sh $nftPolicyFile)
value="1 $nftPolicyId.$4"
walletAddr=$2

echo "utxo: $1"
echo "bodyFile: $bodyFile"
echo "outFile: $outFile"
echo "nftPolicyFile: $nftPolicyFile"
echo "nftPolicyId: $nftPolicyId"
echo "value: $value"
echo "walletAddress: $walletAddr"
echo "signing key file: $3"
echo

echo "querying protocol parameters"
$baseDir/mainnet-query-protocol-parameters.sh

echo


cardano-cli transaction build \
    --alonzo-era \
    --mainnet \
    --tx-in $1 \
    --tx-in-collateral $1 \
    --tx-out "$walletAddr + 1724100 lovelace + $value" \
    --tx-out-datum-hash 45b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0 \
    --mint "$value" \
    --mint-script-file $nftPolicyFile \
    --mint-redeemer-value [] \
    --change-address $walletAddr \
    --protocol-params-file mainnet-protocol-parameters.json \
    --out-file $bodyFile

echo "saved transaction to $bodyFile"

cardano-cli transaction sign \
    --tx-body-file $bodyFile \
    --signing-key-file $3 \
    --mainnet \
    --out-file $outFile

echo "signed transaction and saved as $outFile"

cardano-cli transaction submit \
    --mainnet \
    --tx-file $outFile

echo "submitted transaction"

echo
