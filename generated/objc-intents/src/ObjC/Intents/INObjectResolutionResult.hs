{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INObjectResolutionResult@.
module ObjC.Intents.INObjectResolutionResult
  ( INObjectResolutionResult
  , IsINObjectResolutionResult(..)
  , successWithResolvedObject
  , disambiguationWithObjectsToDisambiguate
  , confirmationRequiredWithObjectToConfirm
  , successWithResolvedObjectSelector
  , disambiguationWithObjectsToDisambiguateSelector
  , confirmationRequiredWithObjectToConfirmSelector


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

-- | @+ successWithResolvedObject:@
successWithResolvedObject :: IsINObject resolvedObject => resolvedObject -> IO (Id INObjectResolutionResult)
successWithResolvedObject resolvedObject =
  do
    cls' <- getRequiredClass "INObjectResolutionResult"
    withObjCPtr resolvedObject $ \raw_resolvedObject ->
      sendClassMsg cls' (mkSelector "successWithResolvedObject:") (retPtr retVoid) [argPtr (castPtr raw_resolvedObject :: Ptr ())] >>= retainedObject . castPtr

-- | @+ disambiguationWithObjectsToDisambiguate:@
disambiguationWithObjectsToDisambiguate :: IsNSArray objectsToDisambiguate => objectsToDisambiguate -> IO (Id INObjectResolutionResult)
disambiguationWithObjectsToDisambiguate objectsToDisambiguate =
  do
    cls' <- getRequiredClass "INObjectResolutionResult"
    withObjCPtr objectsToDisambiguate $ \raw_objectsToDisambiguate ->
      sendClassMsg cls' (mkSelector "disambiguationWithObjectsToDisambiguate:") (retPtr retVoid) [argPtr (castPtr raw_objectsToDisambiguate :: Ptr ())] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithObjectToConfirm:@
confirmationRequiredWithObjectToConfirm :: IsINObject objectToConfirm => objectToConfirm -> IO (Id INObjectResolutionResult)
confirmationRequiredWithObjectToConfirm objectToConfirm =
  do
    cls' <- getRequiredClass "INObjectResolutionResult"
    withObjCPtr objectToConfirm $ \raw_objectToConfirm ->
      sendClassMsg cls' (mkSelector "confirmationRequiredWithObjectToConfirm:") (retPtr retVoid) [argPtr (castPtr raw_objectToConfirm :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedObject:@
successWithResolvedObjectSelector :: Selector
successWithResolvedObjectSelector = mkSelector "successWithResolvedObject:"

-- | @Selector@ for @disambiguationWithObjectsToDisambiguate:@
disambiguationWithObjectsToDisambiguateSelector :: Selector
disambiguationWithObjectsToDisambiguateSelector = mkSelector "disambiguationWithObjectsToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithObjectToConfirm:@
confirmationRequiredWithObjectToConfirmSelector :: Selector
confirmationRequiredWithObjectToConfirmSelector = mkSelector "confirmationRequiredWithObjectToConfirm:"

