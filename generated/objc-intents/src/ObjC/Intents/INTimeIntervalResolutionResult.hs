{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INTimeIntervalResolutionResult@.
module ObjC.Intents.INTimeIntervalResolutionResult
  ( INTimeIntervalResolutionResult
  , IsINTimeIntervalResolutionResult(..)
  , successWithResolvedTimeInterval
  , confirmationRequiredWithTimeIntervalToConfirm
  , successWithResolvedTimeIntervalSelector
  , confirmationRequiredWithTimeIntervalToConfirmSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ successWithResolvedTimeInterval:@
successWithResolvedTimeInterval :: CDouble -> IO (Id INTimeIntervalResolutionResult)
successWithResolvedTimeInterval resolvedTimeInterval =
  do
    cls' <- getRequiredClass "INTimeIntervalResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedTimeInterval:") (retPtr retVoid) [argCDouble (fromIntegral resolvedTimeInterval)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithTimeIntervalToConfirm:@
confirmationRequiredWithTimeIntervalToConfirm :: CDouble -> IO (Id INTimeIntervalResolutionResult)
confirmationRequiredWithTimeIntervalToConfirm timeIntervalToConfirm =
  do
    cls' <- getRequiredClass "INTimeIntervalResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithTimeIntervalToConfirm:") (retPtr retVoid) [argCDouble (fromIntegral timeIntervalToConfirm)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedTimeInterval:@
successWithResolvedTimeIntervalSelector :: Selector
successWithResolvedTimeIntervalSelector = mkSelector "successWithResolvedTimeInterval:"

-- | @Selector@ for @confirmationRequiredWithTimeIntervalToConfirm:@
confirmationRequiredWithTimeIntervalToConfirmSelector :: Selector
confirmationRequiredWithTimeIntervalToConfirmSelector = mkSelector "confirmationRequiredWithTimeIntervalToConfirm:"

