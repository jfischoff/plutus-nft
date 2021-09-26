import Cardano.Api                         hiding (TxId)
import Data.String                         (IsString (..))
import Ledger
import Ledger.Bytes                        (getLedgerBytes)
import System.Environment                  (getArgs)

import Cardano.Plutus.Nft

main :: IO ()
main = do
    [tokenName, utxo'] <- getArgs
    let utxo            = parseUTxO utxo'
        nftPolicyFile   = "scripts/nft-mint-policy.plutus"

    nftPolicyResult <- writeFileTextEnvelope nftPolicyFile Nothing $ nftMint (fromString tokenName) utxo
    case nftPolicyResult of
        Left err -> print $ displayError err
        Right () -> putStrLn $ "wrote NFT policy to file " ++ nftPolicyFile

parseUTxO :: String -> TxOutRef
parseUTxO s =
  let
    (x, y) = span (/= '#') s
  in
    TxOutRef (TxId $ getLedgerBytes $ fromString x) $ read $ tail y
