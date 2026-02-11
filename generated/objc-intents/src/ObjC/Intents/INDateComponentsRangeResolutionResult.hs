{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INDateComponentsRangeResolutionResult@.
module ObjC.Intents.INDateComponentsRangeResolutionResult
  ( INDateComponentsRangeResolutionResult
  , IsINDateComponentsRangeResolutionResult(..)
  , successWithResolvedDateComponentsRange
  , disambiguationWithDateComponentsRangesToDisambiguate
  , confirmationRequiredWithDateComponentsRangeToConfirm
  , successWithResolvedDateComponentsRangeSelector
  , disambiguationWithDateComponentsRangesToDisambiguateSelector
  , confirmationRequiredWithDateComponentsRangeToConfirmSelector


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

-- | @+ successWithResolvedDateComponentsRange:@
successWithResolvedDateComponentsRange :: IsINDateComponentsRange resolvedDateComponentsRange => resolvedDateComponentsRange -> IO (Id INDateComponentsRangeResolutionResult)
successWithResolvedDateComponentsRange resolvedDateComponentsRange =
  do
    cls' <- getRequiredClass "INDateComponentsRangeResolutionResult"
    withObjCPtr resolvedDateComponentsRange $ \raw_resolvedDateComponentsRange ->
      sendClassMsg cls' (mkSelector "successWithResolvedDateComponentsRange:") (retPtr retVoid) [argPtr (castPtr raw_resolvedDateComponentsRange :: Ptr ())] >>= retainedObject . castPtr

-- | @+ disambiguationWithDateComponentsRangesToDisambiguate:@
disambiguationWithDateComponentsRangesToDisambiguate :: IsNSArray dateComponentsRangesToDisambiguate => dateComponentsRangesToDisambiguate -> IO (Id INDateComponentsRangeResolutionResult)
disambiguationWithDateComponentsRangesToDisambiguate dateComponentsRangesToDisambiguate =
  do
    cls' <- getRequiredClass "INDateComponentsRangeResolutionResult"
    withObjCPtr dateComponentsRangesToDisambiguate $ \raw_dateComponentsRangesToDisambiguate ->
      sendClassMsg cls' (mkSelector "disambiguationWithDateComponentsRangesToDisambiguate:") (retPtr retVoid) [argPtr (castPtr raw_dateComponentsRangesToDisambiguate :: Ptr ())] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithDateComponentsRangeToConfirm:@
confirmationRequiredWithDateComponentsRangeToConfirm :: IsINDateComponentsRange dateComponentsRangeToConfirm => dateComponentsRangeToConfirm -> IO (Id INDateComponentsRangeResolutionResult)
confirmationRequiredWithDateComponentsRangeToConfirm dateComponentsRangeToConfirm =
  do
    cls' <- getRequiredClass "INDateComponentsRangeResolutionResult"
    withObjCPtr dateComponentsRangeToConfirm $ \raw_dateComponentsRangeToConfirm ->
      sendClassMsg cls' (mkSelector "confirmationRequiredWithDateComponentsRangeToConfirm:") (retPtr retVoid) [argPtr (castPtr raw_dateComponentsRangeToConfirm :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedDateComponentsRange:@
successWithResolvedDateComponentsRangeSelector :: Selector
successWithResolvedDateComponentsRangeSelector = mkSelector "successWithResolvedDateComponentsRange:"

-- | @Selector@ for @disambiguationWithDateComponentsRangesToDisambiguate:@
disambiguationWithDateComponentsRangesToDisambiguateSelector :: Selector
disambiguationWithDateComponentsRangesToDisambiguateSelector = mkSelector "disambiguationWithDateComponentsRangesToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithDateComponentsRangeToConfirm:@
confirmationRequiredWithDateComponentsRangeToConfirmSelector :: Selector
confirmationRequiredWithDateComponentsRangeToConfirmSelector = mkSelector "confirmationRequiredWithDateComponentsRangeToConfirm:"

