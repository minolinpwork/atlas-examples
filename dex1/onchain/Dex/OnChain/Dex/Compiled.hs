{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}

module Dex.OnChain.Dex.Compiled (
    originalTestTokenPolicy,
    MyTestDatum (..),
    mkCoin,
    mkCoin',
) where

import qualified PlutusLedgerApi.V2   as V2
import qualified PlutusLedgerApi.V1.Scripts

import qualified PlutusTx

import           Dex.OnChain.Token

import           PlutusCore.Version    (plcVersion100)

originalTestTokenPolicy
    :: Integer          -- ^ count
    -> V2.TokenName        -- ^ token name (e.g. @GOLD@)
    -> V2.TxOutRef         -- ^ utxo to base token on
    -> PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
originalTestTokenPolicy count tn utxo = 
    $$(PlutusTx.compile [|| mkTestTokenPolicy ||])
    `PlutusTx.unsafeApplyCode`
     PlutusTx.liftCode plcVersion100 count
    `PlutusTx.unsafeApplyCode`
     PlutusTx.liftCode plcVersion100 tn
    `PlutusTx.unsafeApplyCode`
     PlutusTx.liftCode plcVersion100 utxo