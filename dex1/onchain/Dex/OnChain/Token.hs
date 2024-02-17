{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS -fno-strictness -fno-spec-constr -fno-specialise #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Dex.OnChain.Token (
    mkTestTokenPolicy,
    MyTestDatum (..),
    mkCoin,
    mkCoin',
) where

import           PlutusLedgerApi.V2
import           PlutusLedgerApi.V1.Value    (flattenValue)
import qualified PlutusTx
import           PlutusTx.Prelude          as PlutusTx
import Dex.OnChain.Uniswap.Types


data MyTestDatum = MyTestDatum
  { mtdVals        :: [Integer]
  }
PlutusTx.unstableMakeIsData ''MyTestDatum

{-# INLINABLE mkTestTokenPolicy #-}
mkTestTokenPolicy :: Integer -> TokenName -> TxOutRef -> BuiltinData -> BuiltinData -> ()
mkTestTokenPolicy amt tn utxo _ ctx'
    | hasn'tUTxO  = traceError "UTxO not consumed"
    | tn /= tn'   = traceError "wrong token"
    | amt /= amt' = traceError "wrong amount"
    | otherwise   = ()
  where
    ctx :: ScriptContext
    ctx = unsafeFromBuiltinData ctx'

    info :: TxInfo
    info = scriptContextTxInfo ctx

    [(_, tn', amt')] = flattenValue $ txInfoMint info

    hasn'tUTxO :: Bool
    hasn'tUTxO = all (\i -> txInInfoOutRef i /= utxo) $ txInfoInputs info
