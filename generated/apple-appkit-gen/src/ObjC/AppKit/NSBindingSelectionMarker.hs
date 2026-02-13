{-# LANGUAGE DataKinds #-}
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
  , multipleValuesSelectionMarker
  , noSelectionMarker
  , notApplicableSelectionMarker
  , defaultPlaceholderForMarker_onClass_withBindingSelector
  , initSelector
  , multipleValuesSelectionMarkerSelector
  , noSelectionMarkerSelector
  , notApplicableSelectionMarkerSelector
  , setDefaultPlaceholder_forMarker_onClass_withBindingSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSBindingSelectionMarker nsBindingSelectionMarker => nsBindingSelectionMarker -> IO (Id NSBindingSelectionMarker)
init_ nsBindingSelectionMarker =
  sendOwnedMessage nsBindingSelectionMarker initSelector

-- | @+ setDefaultPlaceholder:forMarker:onClass:withBinding:@
setDefaultPlaceholder_forMarker_onClass_withBinding :: (IsNSBindingSelectionMarker marker, IsNSString binding) => RawId -> marker -> Class -> binding -> IO ()
setDefaultPlaceholder_forMarker_onClass_withBinding placeholder marker objectClass binding =
  do
    cls' <- getRequiredClass "NSBindingSelectionMarker"
    sendClassMessage cls' setDefaultPlaceholder_forMarker_onClass_withBindingSelector placeholder (toNSBindingSelectionMarker marker) objectClass (toNSString binding)

-- | @+ defaultPlaceholderForMarker:onClass:withBinding:@
defaultPlaceholderForMarker_onClass_withBinding :: (IsNSBindingSelectionMarker marker, IsNSString binding) => marker -> Class -> binding -> IO RawId
defaultPlaceholderForMarker_onClass_withBinding marker objectClass binding =
  do
    cls' <- getRequiredClass "NSBindingSelectionMarker"
    sendClassMessage cls' defaultPlaceholderForMarker_onClass_withBindingSelector (toNSBindingSelectionMarker marker) objectClass (toNSString binding)

-- | @+ multipleValuesSelectionMarker@
multipleValuesSelectionMarker :: IO (Id NSBindingSelectionMarker)
multipleValuesSelectionMarker  =
  do
    cls' <- getRequiredClass "NSBindingSelectionMarker"
    sendClassMessage cls' multipleValuesSelectionMarkerSelector

-- | @+ noSelectionMarker@
noSelectionMarker :: IO (Id NSBindingSelectionMarker)
noSelectionMarker  =
  do
    cls' <- getRequiredClass "NSBindingSelectionMarker"
    sendClassMessage cls' noSelectionMarkerSelector

-- | @+ notApplicableSelectionMarker@
notApplicableSelectionMarker :: IO (Id NSBindingSelectionMarker)
notApplicableSelectionMarker  =
  do
    cls' <- getRequiredClass "NSBindingSelectionMarker"
    sendClassMessage cls' notApplicableSelectionMarkerSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSBindingSelectionMarker)
initSelector = mkSelector "init"

-- | @Selector@ for @setDefaultPlaceholder:forMarker:onClass:withBinding:@
setDefaultPlaceholder_forMarker_onClass_withBindingSelector :: Selector '[RawId, Id NSBindingSelectionMarker, Class, Id NSString] ()
setDefaultPlaceholder_forMarker_onClass_withBindingSelector = mkSelector "setDefaultPlaceholder:forMarker:onClass:withBinding:"

-- | @Selector@ for @defaultPlaceholderForMarker:onClass:withBinding:@
defaultPlaceholderForMarker_onClass_withBindingSelector :: Selector '[Id NSBindingSelectionMarker, Class, Id NSString] RawId
defaultPlaceholderForMarker_onClass_withBindingSelector = mkSelector "defaultPlaceholderForMarker:onClass:withBinding:"

-- | @Selector@ for @multipleValuesSelectionMarker@
multipleValuesSelectionMarkerSelector :: Selector '[] (Id NSBindingSelectionMarker)
multipleValuesSelectionMarkerSelector = mkSelector "multipleValuesSelectionMarker"

-- | @Selector@ for @noSelectionMarker@
noSelectionMarkerSelector :: Selector '[] (Id NSBindingSelectionMarker)
noSelectionMarkerSelector = mkSelector "noSelectionMarker"

-- | @Selector@ for @notApplicableSelectionMarker@
notApplicableSelectionMarkerSelector :: Selector '[] (Id NSBindingSelectionMarker)
notApplicableSelectionMarkerSelector = mkSelector "notApplicableSelectionMarker"

