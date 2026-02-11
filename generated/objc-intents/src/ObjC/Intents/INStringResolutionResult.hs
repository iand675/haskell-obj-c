{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INStringResolutionResult@.
module ObjC.Intents.INStringResolutionResult
  ( INStringResolutionResult
  , IsINStringResolutionResult(..)
  , successWithResolvedString
  , disambiguationWithStringsToDisambiguate
  , confirmationRequiredWithStringToConfirm
  , successWithResolvedStringSelector
  , disambiguationWithStringsToDisambiguateSelector
  , confirmationRequiredWithStringToConfirmSelector


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

-- | @+ successWithResolvedString:@
successWithResolvedString :: IsNSString resolvedString => resolvedString -> IO (Id INStringResolutionResult)
successWithResolvedString resolvedString =
  do
    cls' <- getRequiredClass "INStringResolutionResult"
    withObjCPtr resolvedString $ \raw_resolvedString ->
      sendClassMsg cls' (mkSelector "successWithResolvedString:") (retPtr retVoid) [argPtr (castPtr raw_resolvedString :: Ptr ())] >>= retainedObject . castPtr

-- | @+ disambiguationWithStringsToDisambiguate:@
disambiguationWithStringsToDisambiguate :: IsNSArray stringsToDisambiguate => stringsToDisambiguate -> IO (Id INStringResolutionResult)
disambiguationWithStringsToDisambiguate stringsToDisambiguate =
  do
    cls' <- getRequiredClass "INStringResolutionResult"
    withObjCPtr stringsToDisambiguate $ \raw_stringsToDisambiguate ->
      sendClassMsg cls' (mkSelector "disambiguationWithStringsToDisambiguate:") (retPtr retVoid) [argPtr (castPtr raw_stringsToDisambiguate :: Ptr ())] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithStringToConfirm:@
confirmationRequiredWithStringToConfirm :: IsNSString stringToConfirm => stringToConfirm -> IO (Id INStringResolutionResult)
confirmationRequiredWithStringToConfirm stringToConfirm =
  do
    cls' <- getRequiredClass "INStringResolutionResult"
    withObjCPtr stringToConfirm $ \raw_stringToConfirm ->
      sendClassMsg cls' (mkSelector "confirmationRequiredWithStringToConfirm:") (retPtr retVoid) [argPtr (castPtr raw_stringToConfirm :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedString:@
successWithResolvedStringSelector :: Selector
successWithResolvedStringSelector = mkSelector "successWithResolvedString:"

-- | @Selector@ for @disambiguationWithStringsToDisambiguate:@
disambiguationWithStringsToDisambiguateSelector :: Selector
disambiguationWithStringsToDisambiguateSelector = mkSelector "disambiguationWithStringsToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithStringToConfirm:@
confirmationRequiredWithStringToConfirmSelector :: Selector
confirmationRequiredWithStringToConfirmSelector = mkSelector "confirmationRequiredWithStringToConfirm:"

