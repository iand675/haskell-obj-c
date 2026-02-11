{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSBindingSelectionMarker@.
module ObjC.AppKit.NSBindingSelectionMarker
  ( NSBindingSelectionMarker
  , IsNSBindingSelectionMarker(..)
  , init_
  , setDefaultPlaceholder_forMarker_onClass_withBinding
  , defaultPlaceholderForMarker_onClass_withBinding
  , initSelector
  , setDefaultPlaceholder_forMarker_onClass_withBindingSelector
  , defaultPlaceholderForMarker_onClass_withBindingSelector


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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSBindingSelectionMarker nsBindingSelectionMarker => nsBindingSelectionMarker -> IO (Id NSBindingSelectionMarker)
init_ nsBindingSelectionMarker  =
  sendMsg nsBindingSelectionMarker (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ setDefaultPlaceholder:forMarker:onClass:withBinding:@
setDefaultPlaceholder_forMarker_onClass_withBinding :: (IsNSBindingSelectionMarker marker, IsNSString binding) => RawId -> marker -> Class -> binding -> IO ()
setDefaultPlaceholder_forMarker_onClass_withBinding placeholder marker objectClass binding =
  do
    cls' <- getRequiredClass "NSBindingSelectionMarker"
    withObjCPtr marker $ \raw_marker ->
      withObjCPtr binding $ \raw_binding ->
        sendClassMsg cls' (mkSelector "setDefaultPlaceholder:forMarker:onClass:withBinding:") retVoid [argPtr (castPtr (unRawId placeholder) :: Ptr ()), argPtr (castPtr raw_marker :: Ptr ()), argPtr (unClass objectClass), argPtr (castPtr raw_binding :: Ptr ())]

-- | @+ defaultPlaceholderForMarker:onClass:withBinding:@
defaultPlaceholderForMarker_onClass_withBinding :: (IsNSBindingSelectionMarker marker, IsNSString binding) => marker -> Class -> binding -> IO RawId
defaultPlaceholderForMarker_onClass_withBinding marker objectClass binding =
  do
    cls' <- getRequiredClass "NSBindingSelectionMarker"
    withObjCPtr marker $ \raw_marker ->
      withObjCPtr binding $ \raw_binding ->
        fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "defaultPlaceholderForMarker:onClass:withBinding:") (retPtr retVoid) [argPtr (castPtr raw_marker :: Ptr ()), argPtr (unClass objectClass), argPtr (castPtr raw_binding :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @setDefaultPlaceholder:forMarker:onClass:withBinding:@
setDefaultPlaceholder_forMarker_onClass_withBindingSelector :: Selector
setDefaultPlaceholder_forMarker_onClass_withBindingSelector = mkSelector "setDefaultPlaceholder:forMarker:onClass:withBinding:"

-- | @Selector@ for @defaultPlaceholderForMarker:onClass:withBinding:@
defaultPlaceholderForMarker_onClass_withBindingSelector :: Selector
defaultPlaceholderForMarker_onClass_withBindingSelector = mkSelector "defaultPlaceholderForMarker:onClass:withBinding:"

