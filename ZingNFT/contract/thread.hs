{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Thread where

import Plutus.V2.Ledger.Api   (ScriptContext (scriptContextTxInfo), TxOutRef (TxOutRef), TxInfo (txInfoMint),
                              txInInfoOutRef, txInfoInputs, BuiltinData, UnsafeFromData (unsafeFromBuiltinData),
                              mkMintingPolicyScript, ToData (toBuiltinData), MintingPolicy, TokenName (unTokenName), 
                              TxId (TxId))
import PlutusTx.Prelude       (traceIfFalse, find, map, (&&), Bool (False), Eq ((==)), ($), isJust)
import Utilities              (wrapPolicy, writeCodeToFile)
import PlutusTx               (compile, CompiledCode, applyCode, liftCode)
import Plutus.V1.Ledger.Value (flattenValue)
import Prelude                (IO)

{-# INLINABLE policy #-}
policy :: TxOutRef -> () -> ScriptContext -> Bool
policy ref _ ctx = traceIfFalse "Expected output was not consumed" consumesInput &&
                   traceIfFalse "TokenName incorrect"              correctName
    where
        txInfo :: TxInfo
        txInfo = scriptContextTxInfo ctx

        consumesInput :: Bool
        consumesInput = isJust $ find (== ref) (map txInInfoOutRef (txInfoInputs txInfo)) 
        
        correctName :: Bool
        correctName = case flattenValue $ txInfoMint txInfo of
            [(_, tn, _)] -> unTokenName tn == "thread"
            _          -> False


{-# INLINABLE wrappedPolicy #-}
wrappedPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedPolicy hash idx = wrapPolicy $ policy oref
    where
        oref :: TxOutRef
        oref = TxOutRef (TxId $ unsafeFromBuiltinData hash) (unsafeFromBuiltinData idx)


{-# INLINABLE compiledPolicyCode #-}
compiledPolicyCode :: CompiledCode(BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
compiledPolicyCode = $$(compile [|| wrappedPolicy ||])

-- compiledPolicy :: TxOutRef -> MintingPolicy 
-- compiledPolicy out = mkMintingPolicyScript ($$(compile [|| wrappedPolicy ||]) `applyCode` liftCode (toBuiltinData out)) 

writeToFile :: IO ()
writeToFile = writeCodeToFile "./assets/thread.plutus" compiledPolicyCode