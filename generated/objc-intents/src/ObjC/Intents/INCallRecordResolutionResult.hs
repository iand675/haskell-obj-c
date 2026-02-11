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
  , successWithResolvedCallRecordSelector
  , disambiguationWithCallRecordsToDisambiguateSelector
  , confirmationRequiredWithCallRecordToConfirmSelector


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

-- | @+ successWithResolvedCallRecord:@
successWithResolvedCallRecord :: IsINCallRecord resolvedCallRecord => resolvedCallRecord -> IO (Id INCallRecordResolutionResult)
successWithResolvedCallRecord resolvedCallRecord =
  do
    cls' <- getRequiredClass "INCallRecordResolutionResult"
    withObjCPtr resolvedCallRecord $ \raw_resolvedCallRecord ->
      sendClassMsg cls' (mkSelector "successWithResolvedCallRecord:") (retPtr retVoid) [argPtr (castPtr raw_resolvedCallRecord :: Ptr ())] >>= retainedObject . castPtr

-- | @+ disambiguationWithCallRecordsToDisambiguate:@
disambiguationWithCallRecordsToDisambiguate :: IsNSArray callRecordsToDisambiguate => callRecordsToDisambiguate -> IO (Id INCallRecordResolutionResult)
disambiguationWithCallRecordsToDisambiguate callRecordsToDisambiguate =
  do
    cls' <- getRequiredClass "INCallRecordResolutionResult"
    withObjCPtr callRecordsToDisambiguate $ \raw_callRecordsToDisambiguate ->
      sendClassMsg cls' (mkSelector "disambiguationWithCallRecordsToDisambiguate:") (retPtr retVoid) [argPtr (castPtr raw_callRecordsToDisambiguate :: Ptr ())] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithCallRecordToConfirm:@
confirmationRequiredWithCallRecordToConfirm :: IsINCallRecord callRecordToConfirm => callRecordToConfirm -> IO (Id INCallRecordResolutionResult)
confirmationRequiredWithCallRecordToConfirm callRecordToConfirm =
  do
    cls' <- getRequiredClass "INCallRecordResolutionResult"
    withObjCPtr callRecordToConfirm $ \raw_callRecordToConfirm ->
      sendClassMsg cls' (mkSelector "confirmationRequiredWithCallRecordToConfirm:") (retPtr retVoid) [argPtr (castPtr raw_callRecordToConfirm :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedCallRecord:@
successWithResolvedCallRecordSelector :: Selector
successWithResolvedCallRecordSelector = mkSelector "successWithResolvedCallRecord:"

-- | @Selector@ for @disambiguationWithCallRecordsToDisambiguate:@
disambiguationWithCallRecordsToDisambiguateSelector :: Selector
disambiguationWithCallRecordsToDisambiguateSelector = mkSelector "disambiguationWithCallRecordsToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithCallRecordToConfirm:@
confirmationRequiredWithCallRecordToConfirmSelector :: Selector
confirmationRequiredWithCallRecordToConfirmSelector = mkSelector "confirmationRequiredWithCallRecordToConfirm:"

