{-# LANGUAGE NoImplicitPrelude #-}
module Cardano.Plutus.Nft (nftMint) where

import           Cardano.Api.Shelley      (PlutusScript (..), PlutusScriptV1)
import           Codec.Serialise
import qualified Data.ByteString.Lazy     as LB
import qualified Data.ByteString.Short    as SBS
import           Ledger                   hiding (singleton)
import qualified Ledger.Typed.Scripts     as Scripts
import           Ledger.Value             as Value
import qualified PlutusTx
import           PlutusTx.Prelude         hiding (Semigroup (..), unless)

{-# INLINABLE mkNFTPolicy #-}
mkNFTPolicy :: TokenName -> TxOutRef -> BuiltinData -> ScriptContext -> Bool
mkNFTPolicy tn utxo _ ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                            traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == utxo) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, tn', amt)] -> tn' == tn && amt == 1
        _               -> False

nftPolicy :: TokenName -> TxOutRef -> Scripts.MintingPolicy
nftPolicy theTokenName utxo = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \tn utxo' -> Scripts.wrapMintingPolicy $ mkNFTPolicy tn utxo' ||])
    `PlutusTx.applyCode`
     PlutusTx.liftCode theTokenName
    `PlutusTx.applyCode`
     PlutusTx.liftCode utxo

nftPlutusScript :: TokenName ->  TxOutRef -> Script
nftPlutusScript theTokenName = unMintingPolicyScript . nftPolicy theTokenName

nftValidator :: TokenName -> TxOutRef -> Validator
nftValidator theTokenName = Validator . nftPlutusScript theTokenName

nftScriptAsCbor :: TokenName -> TxOutRef -> LB.ByteString
nftScriptAsCbor theTokenName = serialise . nftValidator theTokenName

nftMint :: TokenName -> TxOutRef -> PlutusScript PlutusScriptV1
nftMint theTokenName
  = PlutusScriptSerialised
  . SBS.toShort
  . LB.toStrict
  . nftScriptAsCbor theTokenName
