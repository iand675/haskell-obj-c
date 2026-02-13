{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSAccessibilityElement@.
module ObjC.AppKit.NSAccessibilityElement
  ( NSAccessibilityElement
  , IsNSAccessibilityElement(..)
  , accessibilityElementWithRole_frame_label_parent
  , accessibilityAddChildElement
  , accessibilityFrameInParentSpace
  , setAccessibilityFrameInParentSpace
  , accessibilityAddChildElementSelector
  , accessibilityElementWithRole_frame_label_parentSelector
  , accessibilityFrameInParentSpaceSelector
  , setAccessibilityFrameInParentSpaceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @+ accessibilityElementWithRole:frame:label:parent:@
accessibilityElementWithRole_frame_label_parent :: (IsNSString role_, IsNSString label) => role_ -> NSRect -> label -> RawId -> IO RawId
accessibilityElementWithRole_frame_label_parent role_ frame label parent =
  do
    cls' <- getRequiredClass "NSAccessibilityElement"
    sendClassMessage cls' accessibilityElementWithRole_frame_label_parentSelector (toNSString role_) frame (toNSString label) parent

-- | @- accessibilityAddChildElement:@
accessibilityAddChildElement :: (IsNSAccessibilityElement nsAccessibilityElement, IsNSAccessibilityElement childElement) => nsAccessibilityElement -> childElement -> IO ()
accessibilityAddChildElement nsAccessibilityElement childElement =
  sendMessage nsAccessibilityElement accessibilityAddChildElementSelector (toNSAccessibilityElement childElement)

-- | @- accessibilityFrameInParentSpace@
accessibilityFrameInParentSpace :: IsNSAccessibilityElement nsAccessibilityElement => nsAccessibilityElement -> IO NSRect
accessibilityFrameInParentSpace nsAccessibilityElement =
  sendMessage nsAccessibilityElement accessibilityFrameInParentSpaceSelector

-- | @- setAccessibilityFrameInParentSpace:@
setAccessibilityFrameInParentSpace :: IsNSAccessibilityElement nsAccessibilityElement => nsAccessibilityElement -> NSRect -> IO ()
setAccessibilityFrameInParentSpace nsAccessibilityElement value =
  sendMessage nsAccessibilityElement setAccessibilityFrameInParentSpaceSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @accessibilityElementWithRole:frame:label:parent:@
accessibilityElementWithRole_frame_label_parentSelector :: Selector '[Id NSString, NSRect, Id NSString, RawId] RawId
accessibilityElementWithRole_frame_label_parentSelector = mkSelector "accessibilityElementWithRole:frame:label:parent:"

-- | @Selector@ for @accessibilityAddChildElement:@
accessibilityAddChildElementSelector :: Selector '[Id NSAccessibilityElement] ()
accessibilityAddChildElementSelector = mkSelector "accessibilityAddChildElement:"

-- | @Selector@ for @accessibilityFrameInParentSpace@
accessibilityFrameInParentSpaceSelector :: Selector '[] NSRect
accessibilityFrameInParentSpaceSelector = mkSelector "accessibilityFrameInParentSpace"

-- | @Selector@ for @setAccessibilityFrameInParentSpace:@
setAccessibilityFrameInParentSpaceSelector :: Selector '[NSRect] ()
setAccessibilityFrameInParentSpaceSelector = mkSelector "setAccessibilityFrameInParentSpace:"

