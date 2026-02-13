{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INTimeIntervalResolutionResult@.
module ObjC.Intents.INTimeIntervalResolutionResult
  ( INTimeIntervalResolutionResult
  , IsINTimeIntervalResolutionResult(..)
  , successWithResolvedTimeInterval
  , confirmationRequiredWithTimeIntervalToConfirm
  , confirmationRequiredWithTimeIntervalToConfirmSelector
  , successWithResolvedTimeIntervalSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ successWithResolvedTimeInterval:@
successWithResolvedTimeInterval :: CDouble -> IO (Id INTimeIntervalResolutionResult)
successWithResolvedTimeInterval resolvedTimeInterval =
  do
    cls' <- getRequiredClass "INTimeIntervalResolutionResult"
    sendClassMessage cls' successWithResolvedTimeIntervalSelector resolvedTimeInterval

-- | @+ confirmationRequiredWithTimeIntervalToConfirm:@
confirmationRequiredWithTimeIntervalToConfirm :: CDouble -> IO (Id INTimeIntervalResolutionResult)
confirmationRequiredWithTimeIntervalToConfirm timeIntervalToConfirm =
  do
    cls' <- getRequiredClass "INTimeIntervalResolutionResult"
    sendClassMessage cls' confirmationRequiredWithTimeIntervalToConfirmSelector timeIntervalToConfirm

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedTimeInterval:@
successWithResolvedTimeIntervalSelector :: Selector '[CDouble] (Id INTimeIntervalResolutionResult)
successWithResolvedTimeIntervalSelector = mkSelector "successWithResolvedTimeInterval:"

-- | @Selector@ for @confirmationRequiredWithTimeIntervalToConfirm:@
confirmationRequiredWithTimeIntervalToConfirmSelector :: Selector '[CDouble] (Id INTimeIntervalResolutionResult)
confirmationRequiredWithTimeIntervalToConfirmSelector = mkSelector "confirmationRequiredWithTimeIntervalToConfirm:"

