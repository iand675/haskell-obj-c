{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCustomTouchBarItem@.
module ObjC.AppKit.NSCustomTouchBarItem
  ( NSCustomTouchBarItem
  , IsNSCustomTouchBarItem(..)
  , view
  , setView
  , viewController
  , setViewController
  , customizationLabel
  , setCustomizationLabel
  , viewSelector
  , setViewSelector
  , viewControllerSelector
  , setViewControllerSelector
  , customizationLabelSelector
  , setCustomizationLabelSelector


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

-- | @- view@
view :: IsNSCustomTouchBarItem nsCustomTouchBarItem => nsCustomTouchBarItem -> IO (Id NSView)
view nsCustomTouchBarItem  =
  sendMsg nsCustomTouchBarItem (mkSelector "view") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setView:@
setView :: (IsNSCustomTouchBarItem nsCustomTouchBarItem, IsNSView value) => nsCustomTouchBarItem -> value -> IO ()
setView nsCustomTouchBarItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsCustomTouchBarItem (mkSelector "setView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- viewController@
viewController :: IsNSCustomTouchBarItem nsCustomTouchBarItem => nsCustomTouchBarItem -> IO (Id NSViewController)
viewController nsCustomTouchBarItem  =
  sendMsg nsCustomTouchBarItem (mkSelector "viewController") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setViewController:@
setViewController :: (IsNSCustomTouchBarItem nsCustomTouchBarItem, IsNSViewController value) => nsCustomTouchBarItem -> value -> IO ()
setViewController nsCustomTouchBarItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsCustomTouchBarItem (mkSelector "setViewController:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- customizationLabel@
customizationLabel :: IsNSCustomTouchBarItem nsCustomTouchBarItem => nsCustomTouchBarItem -> IO (Id NSString)
customizationLabel nsCustomTouchBarItem  =
  sendMsg nsCustomTouchBarItem (mkSelector "customizationLabel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCustomizationLabel:@
setCustomizationLabel :: (IsNSCustomTouchBarItem nsCustomTouchBarItem, IsNSString value) => nsCustomTouchBarItem -> value -> IO ()
setCustomizationLabel nsCustomTouchBarItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsCustomTouchBarItem (mkSelector "setCustomizationLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @view@
viewSelector :: Selector
viewSelector = mkSelector "view"

-- | @Selector@ for @setView:@
setViewSelector :: Selector
setViewSelector = mkSelector "setView:"

-- | @Selector@ for @viewController@
viewControllerSelector :: Selector
viewControllerSelector = mkSelector "viewController"

-- | @Selector@ for @setViewController:@
setViewControllerSelector :: Selector
setViewControllerSelector = mkSelector "setViewController:"

-- | @Selector@ for @customizationLabel@
customizationLabelSelector :: Selector
customizationLabelSelector = mkSelector "customizationLabel"

-- | @Selector@ for @setCustomizationLabel:@
setCustomizationLabelSelector :: Selector
setCustomizationLabelSelector = mkSelector "setCustomizationLabel:"

