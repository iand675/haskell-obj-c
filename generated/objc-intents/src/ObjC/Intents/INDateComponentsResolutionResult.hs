{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INDateComponentsResolutionResult@.
module ObjC.Intents.INDateComponentsResolutionResult
  ( INDateComponentsResolutionResult
  , IsINDateComponentsResolutionResult(..)
  , successWithResolvedDateComponents
  , disambiguationWithDateComponentsToDisambiguate
  , confirmationRequiredWithDateComponentsToConfirm
  , successWithResolvedDateComponentsSelector
  , disambiguationWithDateComponentsToDisambiguateSelector
  , confirmationRequiredWithDateComponentsToConfirmSelector


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

-- | @+ successWithResolvedDateComponents:@
successWithResolvedDateComponents :: IsNSDateComponents resolvedDateComponents => resolvedDateComponents -> IO (Id INDateComponentsResolutionResult)
successWithResolvedDateComponents resolvedDateComponents =
  do
    cls' <- getRequiredClass "INDateComponentsResolutionResult"
    withObjCPtr resolvedDateComponents $ \raw_resolvedDateComponents ->
      sendClassMsg cls' (mkSelector "successWithResolvedDateComponents:") (retPtr retVoid) [argPtr (castPtr raw_resolvedDateComponents :: Ptr ())] >>= retainedObject . castPtr

-- | @+ disambiguationWithDateComponentsToDisambiguate:@
disambiguationWithDateComponentsToDisambiguate :: IsNSArray dateComponentsToDisambiguate => dateComponentsToDisambiguate -> IO (Id INDateComponentsResolutionResult)
disambiguationWithDateComponentsToDisambiguate dateComponentsToDisambiguate =
  do
    cls' <- getRequiredClass "INDateComponentsResolutionResult"
    withObjCPtr dateComponentsToDisambiguate $ \raw_dateComponentsToDisambiguate ->
      sendClassMsg cls' (mkSelector "disambiguationWithDateComponentsToDisambiguate:") (retPtr retVoid) [argPtr (castPtr raw_dateComponentsToDisambiguate :: Ptr ())] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithDateComponentsToConfirm:@
confirmationRequiredWithDateComponentsToConfirm :: IsNSDateComponents dateComponentsToConfirm => dateComponentsToConfirm -> IO (Id INDateComponentsResolutionResult)
confirmationRequiredWithDateComponentsToConfirm dateComponentsToConfirm =
  do
    cls' <- getRequiredClass "INDateComponentsResolutionResult"
    withObjCPtr dateComponentsToConfirm $ \raw_dateComponentsToConfirm ->
      sendClassMsg cls' (mkSelector "confirmationRequiredWithDateComponentsToConfirm:") (retPtr retVoid) [argPtr (castPtr raw_dateComponentsToConfirm :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedDateComponents:@
successWithResolvedDateComponentsSelector :: Selector
successWithResolvedDateComponentsSelector = mkSelector "successWithResolvedDateComponents:"

-- | @Selector@ for @disambiguationWithDateComponentsToDisambiguate:@
disambiguationWithDateComponentsToDisambiguateSelector :: Selector
disambiguationWithDateComponentsToDisambiguateSelector = mkSelector "disambiguationWithDateComponentsToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithDateComponentsToConfirm:@
confirmationRequiredWithDateComponentsToConfirmSelector :: Selector
confirmationRequiredWithDateComponentsToConfirmSelector = mkSelector "confirmationRequiredWithDateComponentsToConfirm:"

