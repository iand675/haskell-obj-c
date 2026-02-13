{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INCallRecordResolutionResult@.
module ObjC.Intents.INCallRecordResolutionResult
  ( INCallRecordResolutionResult
  , IsINCallRecordResolutionResult(..)
  , successWithResolvedCallRecord
  , disambiguationWithCallRecordsToDisambiguate
  , confirmationRequiredWithCallRecordToConfirm
  , confirmationRequiredWithCallRecordToConfirmSelector
  , disambiguationWithCallRecordsToDisambiguateSelector
  , successWithResolvedCallRecordSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ successWithResolvedCallRecord:@
successWithResolvedCallRecord :: IsINCallRecord resolvedCallRecord => resolvedCallRecord -> IO (Id INCallRecordResolutionResult)
successWithResolvedCallRecord resolvedCallRecord =
  do
    cls' <- getRequiredClass "INCallRecordResolutionResult"
    sendClassMessage cls' successWithResolvedCallRecordSelector (toINCallRecord resolvedCallRecord)

-- | @+ disambiguationWithCallRecordsToDisambiguate:@
disambiguationWithCallRecordsToDisambiguate :: IsNSArray callRecordsToDisambiguate => callRecordsToDisambiguate -> IO (Id INCallRecordResolutionResult)
disambiguationWithCallRecordsToDisambiguate callRecordsToDisambiguate =
  do
    cls' <- getRequiredClass "INCallRecordResolutionResult"
    sendClassMessage cls' disambiguationWithCallRecordsToDisambiguateSelector (toNSArray callRecordsToDisambiguate)

-- | @+ confirmationRequiredWithCallRecordToConfirm:@
confirmationRequiredWithCallRecordToConfirm :: IsINCallRecord callRecordToConfirm => callRecordToConfirm -> IO (Id INCallRecordResolutionResult)
confirmationRequiredWithCallRecordToConfirm callRecordToConfirm =
  do
    cls' <- getRequiredClass "INCallRecordResolutionResult"
    sendClassMessage cls' confirmationRequiredWithCallRecordToConfirmSelector (toINCallRecord callRecordToConfirm)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedCallRecord:@
successWithResolvedCallRecordSelector :: Selector '[Id INCallRecord] (Id INCallRecordResolutionResult)
successWithResolvedCallRecordSelector = mkSelector "successWithResolvedCallRecord:"

-- | @Selector@ for @disambiguationWithCallRecordsToDisambiguate:@
disambiguationWithCallRecordsToDisambiguateSelector :: Selector '[Id NSArray] (Id INCallRecordResolutionResult)
disambiguationWithCallRecordsToDisambiguateSelector = mkSelector "disambiguationWithCallRecordsToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithCallRecordToConfirm:@
confirmationRequiredWithCallRecordToConfirmSelector :: Selector '[Id INCallRecord] (Id INCallRecordResolutionResult)
confirmationRequiredWithCallRecordToConfirmSelector = mkSelector "confirmationRequiredWithCallRecordToConfirm:"

