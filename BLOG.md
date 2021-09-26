# Taking Plutus for a Spin

Recently the Cardano cryptocurrency project released their Alonzo version with support for smart contracts. Cardano is primarily built on Haskell and their smart contract system, Plutus, is as well.

Cardano is a large cryptocurrency project, currently the third largest by market capitialization after Bitcoin and Etherum. Beyond market capitialization the project is possibly the most actively developed Haskell project. It is designed to compete with Etherum's smart contract abilities, but because Cardano uses Proof of Stake, instead of Proof of Work, it is more energy efficent and requires dramatically lower fees.

It is common for Ethereum smart contract transactions to cost over $60 dollars in fees. However, this has not stopped adoption Ethereum, which has surged this year as NFTs have become more mainstream.

Cardano promises to do all the things Ethereum can do, but better and cheaper, and it uses Haskell. Ethereum uses Solidity, I don't know Solidity, but with 12 years of Haskell I thought I would check it out.

## Buy Ada

To actually deploy and interact with a Plutus smart contract you need to create transaction on the Cardano blockchain. This will require spending the native currency Ada. You don't need a lot to play around with Plutus. A few bucks in Ada will be enough.

The easiest way to buy Ada is with Coinbase. If you don't have a Coinbase account yet, you'll want to install the app and set one up. Have fun.

## Install Nix

The first thing you are going to want to do is install the Nix package manager system.

You can do that with this command:

```
sh <(curl -L https://nixos.org/nix/install) --daemon
```

If you have Nix installed already you will see instructions on how to uninstall it.

I'm on a Mac and thus required a few extra steps to uninstall that were not listed sadly.

First I had to use the Disk Utility to remove my Nix partition. Then I needed to delete the `/nix` line from the `/etc/exports` file.

After restarting my computer I could install Nix ... yay.

### 🚨🚨🚨 VERY IMPORTANT 🚨🚨🚨

You must configure the IOHK nix caches otherwise you will end up building GHC from source. Follow the instructions here: https://github.com/input-output-hk/plutus#iohk-binary-cache

## Install Daedalus

Daedalus is the Cardano GUI wallet system. You could probably use `cardano-node` instead but I got things to work with Daedalus so ... that is direction we will go.

Download it here: https://daedaluswallet.io/en/download/.

After installing it start and let it sync. This will take like half a day.

## Build the `cardano-cli`

We are going to need the `cardano-cli` to interact with the Cardano Block chain. These instructions are based on the documentation here: https://docs.cardano.org/plutus/Plutus-transactions.

Clone the repo and build the `cardano-cli`

```
git clone https://github.com/input-output-hk/cardano-node
cd cardano-node
git fetch --all --recurse-submodules --tags
git checkout tags/1.29.0
```

Create a `plutux-tutorial.nix` file with the following content:

```
{ version ? "mainnet", pkgs ? import <nixpkgs> { }}:
let
  cardano-node-repo = import ./. { };

in pkgs.mkShell {
  buildInputs = with pkgs; [
    libsodium
    cabal-install
    zlib
    haskell.compiler.ghc8107
    haskellPackages.haskell-language-server

    cardano-node-repo.scripts."${version}".node
    cardano-node-repo.cardano-cli
  ];

  CARDANO_NODE_SOCKET_PATH = "${builtins.toString ./.}/state-node-${version}/node.socket";

}
```

Call

```
nix-shell plutus-tutorial.nix
```

Wait ten minutes.

Once everything finishes building it's time to create a wallet address.

Run the following in the currently active nix shell.

```
cardano-cli address key-gen --verification-key-file ~/plutus_test.vkey --signing-key-file ~/plutus_test.skey
cardano-cli address build --payment-verification-key-file ~/plutus_test.vkey --out-file ~/plutus_test.addr
```

Now transfer 2 Ada to this address.

## Creating a NFT

NFTs are way to make unique digital assets. Cardano has had the ability to create a limited form of NFTs before Alonzo, but with Plutus, Cardano can make NFTs that rival the Ethereum based NFTs.

I've extracted the NFT creation code from the Lobster Challenge repo.

The NFT smart contract enforces uniqueness by requiring the transaction input is a specific UTxO on the blockchain. UTxO's are blockchain nodes that can be used as inputs to new transactions. However once they are used in a transaction, they cannot be used again (this would allow "double spending").

What this means is the smart contract must be recompiled for every NFT. It can only be used once.

To compile the smart contract we must first get the id for the UTxO where our Ada is stored.

First make sure Daedalus is running and it is synced up with the network. Then set the

```
export CARDANO_NODE_SOCKET_PATH="/Users/YOUR_USER_NAME/Library/Application Support/Daedalus Mainnet/cardano-node.socket"
```

Now we can get the UTxO associated with our wallet address.

```
cardano-cli query utxo --address $(cat ~/plutus_test.addr) --mainnet

                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
ea2fe8519d45c33c21de2fc95664075c1dfd42af232f3a51809ed0ffad223164     0        4200000 lovelace + TxOutDatumHashNone
```

In this example the id we need is `ea2fe8519d45c33c21de2fc95664075c1dfd42af232f3a51809ed0ffad223164#0`.

We can pass this along with the "token name" of our NFT :

```
cabal run create-nft-script -- ea2fe8519d45c33c21de2fc95664075c1dfd42af232f3a51809ed0ffad223164#0 AwesomeNFT
```

This will write the `nft-mint-policy.plutus` to the `scripts` directory.

We can now create our NFT minting transaction.

```
scripts/mint_nft.sh ea2fe8519d45c33c21de2fc95664075c1dfd42af232f3a51809ed0ffad223164#0 $(cat ~/plutus_test.addr) ~/plutus_test.skey AwesomeNFT
```

After about 30 seconds query UTxO at your wallet address

```
cardano-cli query utxo --address $(cat ~/plutus_test.addr) --mainnet
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
8579dd0a31cd1b54e5b91320ff90806e1bdd65ebe3278a93e638429b262ecd8b     0        2070331 lovelace + TxOutDatumHashNone
8579dd0a31cd1b54e5b91320ff90806e1bdd65ebe3278a93e638429b262ecd8b     1        1724100 lovelace + 1 369e5bad71475274d99a1c3c8272df1b159e677b49b86d220961e3c4.AwesomeNFT + TxOutDatumHash ScriptDataInAlonzoEra "45b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0"
```

Which should show your NFT.

Congrats!

## Closing Thoughts

We created an NFT but it is not that useful. The NFTs on Ethereum also store metadata, like urls to IPFS which we need to do as well if we wanted to make something more useful. I believe this would require a validator script to preserve the metadata in addition to the minting policy script.

However, it is a start. I'm pretty busy building my third attempt at a consumer social app (only 10 more attempts to go before one of them might work), but I might try to extending the NFT functionality. Pull requests welcome ;).
