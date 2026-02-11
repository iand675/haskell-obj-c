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
  , accessibilityElementWithRole_frame_label_parentSelector
  , accessibilityAddChildElementSelector
  , accessibilityFrameInParentSpaceSelector
  , setAccessibilityFrameInParentSpaceSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
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
    withObjCPtr role_ $ \raw_role_ ->
      withObjCPtr label $ \raw_label ->
        fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "accessibilityElementWithRole:frame:label:parent:") (retPtr retVoid) [argPtr (castPtr raw_role_ :: Ptr ()), argNSRect frame, argPtr (castPtr raw_label :: Ptr ()), argPtr (castPtr (unRawId parent) :: Ptr ())]

-- | @- accessibilityAddChildElement:@
accessibilityAddChildElement :: (IsNSAccessibilityElement nsAccessibilityElement, IsNSAccessibilityElement childElement) => nsAccessibilityElement -> childElement -> IO ()
accessibilityAddChildElement nsAccessibilityElement  childElement =
withObjCPtr childElement $ \raw_childElement ->
    sendMsg nsAccessibilityElement (mkSelector "accessibilityAddChildElement:") retVoid [argPtr (castPtr raw_childElement :: Ptr ())]

-- | @- accessibilityFrameInParentSpace@
accessibilityFrameInParentSpace :: IsNSAccessibilityElement nsAccessibilityElement => nsAccessibilityElement -> IO NSRect
accessibilityFrameInParentSpace nsAccessibilityElement  =
  sendMsgStret nsAccessibilityElement (mkSelector "accessibilityFrameInParentSpace") retNSRect []

-- | @- setAccessibilityFrameInParentSpace:@
setAccessibilityFrameInParentSpace :: IsNSAccessibilityElement nsAccessibilityElement => nsAccessibilityElement -> NSRect -> IO ()
setAccessibilityFrameInParentSpace nsAccessibilityElement  value =
  sendMsg nsAccessibilityElement (mkSelector "setAccessibilityFrameInParentSpace:") retVoid [argNSRect value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @accessibilityElementWithRole:frame:label:parent:@
accessibilityElementWithRole_frame_label_parentSelector :: Selector
accessibilityElementWithRole_frame_label_parentSelector = mkSelector "accessibilityElementWithRole:frame:label:parent:"

-- | @Selector@ for @accessibilityAddChildElement:@
accessibilityAddChildElementSelector :: Selector
accessibilityAddChildElementSelector = mkSelector "accessibilityAddChildElement:"

-- | @Selector@ for @accessibilityFrameInParentSpace@
accessibilityFrameInParentSpaceSelector :: Selector
accessibilityFrameInParentSpaceSelector = mkSelector "accessibilityFrameInParentSpace"

-- | @Selector@ for @setAccessibilityFrameInParentSpace:@
setAccessibilityFrameInParentSpaceSelector :: Selector
setAccessibilityFrameInParentSpaceSelector = mkSelector "setAccessibilityFrameInParentSpace:"

