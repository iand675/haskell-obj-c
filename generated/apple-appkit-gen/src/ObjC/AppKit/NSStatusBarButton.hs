{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Status bar buttons are the visual representation of @NSStatusItem@s, and are primarily displayed on the right side of the menu bar. When a template image is set as the @image@ property of the status bar button, it is rendered with the correct menu bar style. This guarantees that the button will look correct in various button states and appearances (such as dark menu bar).
--
-- Generated bindings for @NSStatusBarButton@.
module ObjC.AppKit.NSStatusBarButton
  ( NSStatusBarButton
  , IsNSStatusBarButton(..)
  , appearsDisabled
  , setAppearsDisabled
  , appearsDisabledSelector
  , setAppearsDisabledSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | If @YES@, the status bar icon has a disabled/off appearance while still being functional, such as allowing selection and actions. Defaults to @NO@.
--
-- ObjC selector: @- appearsDisabled@
appearsDisabled :: IsNSStatusBarButton nsStatusBarButton => nsStatusBarButton -> IO Bool
appearsDisabled nsStatusBarButton =
  sendMessage nsStatusBarButton appearsDisabledSelector

-- | If @YES@, the status bar icon has a disabled/off appearance while still being functional, such as allowing selection and actions. Defaults to @NO@.
--
-- ObjC selector: @- setAppearsDisabled:@
setAppearsDisabled :: IsNSStatusBarButton nsStatusBarButton => nsStatusBarButton -> Bool -> IO ()
setAppearsDisabled nsStatusBarButton value =
  sendMessage nsStatusBarButton setAppearsDisabledSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @appearsDisabled@
appearsDisabledSelector :: Selector '[] Bool
appearsDisabledSelector = mkSelector "appearsDisabled"

-- | @Selector@ for @setAppearsDisabled:@
setAppearsDisabledSelector :: Selector '[Bool] ()
setAppearsDisabledSelector = mkSelector "setAppearsDisabled:"

