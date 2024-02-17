{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}

module Dex.OnChain.Uniswap.Uniswap.Compiled (
    uniswapValidator,
    Uniswap(..),
    Coin(..),
    PoolState,
    UniswapDatum (..),
    unitValue,
    isUnity,
    liquidityPolicy,
    LiquidityPool(..),
    Liquidity,
    A, B, U,
    valueOf,
    Amount(..), UniswapAction (..)
) where

import qualified PlutusLedgerApi.V2 as V2  
import qualified PlutusLedgerApi.V1.Scripts

import qualified PlutusTx

import Dex.OnChain.Uniswap.Types
    ( isUnity, unitValue, Coin(..), PoolState, Uniswap(..), UniswapDatum(..), Amount, Liquidity, 
    LiquidityPool(..),
    A, B, U,
    valueOf,
    Amount(unAmount), UniswapAction (..)
    
    )
import           Dex.OnChain.Uniswap.OnChain
import           Dex.OnChain.Uniswap.Pool
import           PlutusCore.Version    (plcVersion100)


-- | Generates validator given params.
uniswapValidator :: Uniswap -> Coin PoolState -> PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
uniswapValidator us c = 
    $$(PlutusTx.compile [|| mkUniswapValidator' ||]) 
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 us
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 c

liquidityPolicy :: Uniswap -> V2.TokenName -> PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
liquidityPolicy us poolStateTokenName = 
    $$(PlutusTx.compile [|| validateLiquidityMinting' ||])
        `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 us
        `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 poolStateTokenName
