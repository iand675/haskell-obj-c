{-# LANGUAGE DataKinds #-}
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
  , customizationLabelSelector
  , setCustomizationLabelSelector
  , setViewControllerSelector
  , setViewSelector
  , viewControllerSelector
  , viewSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- view@
view :: IsNSCustomTouchBarItem nsCustomTouchBarItem => nsCustomTouchBarItem -> IO (Id NSView)
view nsCustomTouchBarItem =
  sendMessage nsCustomTouchBarItem viewSelector

-- | @- setView:@
setView :: (IsNSCustomTouchBarItem nsCustomTouchBarItem, IsNSView value) => nsCustomTouchBarItem -> value -> IO ()
setView nsCustomTouchBarItem value =
  sendMessage nsCustomTouchBarItem setViewSelector (toNSView value)

-- | @- viewController@
viewController :: IsNSCustomTouchBarItem nsCustomTouchBarItem => nsCustomTouchBarItem -> IO (Id NSViewController)
viewController nsCustomTouchBarItem =
  sendMessage nsCustomTouchBarItem viewControllerSelector

-- | @- setViewController:@
setViewController :: (IsNSCustomTouchBarItem nsCustomTouchBarItem, IsNSViewController value) => nsCustomTouchBarItem -> value -> IO ()
setViewController nsCustomTouchBarItem value =
  sendMessage nsCustomTouchBarItem setViewControllerSelector (toNSViewController value)

-- | @- customizationLabel@
customizationLabel :: IsNSCustomTouchBarItem nsCustomTouchBarItem => nsCustomTouchBarItem -> IO (Id NSString)
customizationLabel nsCustomTouchBarItem =
  sendMessage nsCustomTouchBarItem customizationLabelSelector

-- | @- setCustomizationLabel:@
setCustomizationLabel :: (IsNSCustomTouchBarItem nsCustomTouchBarItem, IsNSString value) => nsCustomTouchBarItem -> value -> IO ()
setCustomizationLabel nsCustomTouchBarItem value =
  sendMessage nsCustomTouchBarItem setCustomizationLabelSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @view@
viewSelector :: Selector '[] (Id NSView)
viewSelector = mkSelector "view"

-- | @Selector@ for @setView:@
setViewSelector :: Selector '[Id NSView] ()
setViewSelector = mkSelector "setView:"

-- | @Selector@ for @viewController@
viewControllerSelector :: Selector '[] (Id NSViewController)
viewControllerSelector = mkSelector "viewController"

-- | @Selector@ for @setViewController:@
setViewControllerSelector :: Selector '[Id NSViewController] ()
setViewControllerSelector = mkSelector "setViewController:"

-- | @Selector@ for @customizationLabel@
customizationLabelSelector :: Selector '[] (Id NSString)
customizationLabelSelector = mkSelector "customizationLabel"

-- | @Selector@ for @setCustomizationLabel:@
setCustomizationLabelSelector :: Selector '[Id NSString] ()
setCustomizationLabelSelector = mkSelector "setCustomizationLabel:"

