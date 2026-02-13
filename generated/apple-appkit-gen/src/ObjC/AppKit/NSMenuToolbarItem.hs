{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSMenuToolbarItem@.
module ObjC.AppKit.NSMenuToolbarItem
  ( NSMenuToolbarItem
  , IsNSMenuToolbarItem(..)
  , menu
  , setMenu
  , showsIndicator
  , setShowsIndicator
  , menuSelector
  , setMenuSelector
  , setShowsIndicatorSelector
  , showsIndicatorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- menu@
menu :: IsNSMenuToolbarItem nsMenuToolbarItem => nsMenuToolbarItem -> IO (Id NSMenu)
menu nsMenuToolbarItem =
  sendMessage nsMenuToolbarItem menuSelector

-- | @- setMenu:@
setMenu :: (IsNSMenuToolbarItem nsMenuToolbarItem, IsNSMenu value) => nsMenuToolbarItem -> value -> IO ()
setMenu nsMenuToolbarItem value =
  sendMessage nsMenuToolbarItem setMenuSelector (toNSMenu value)

-- | @- showsIndicator@
showsIndicator :: IsNSMenuToolbarItem nsMenuToolbarItem => nsMenuToolbarItem -> IO Bool
showsIndicator nsMenuToolbarItem =
  sendMessage nsMenuToolbarItem showsIndicatorSelector

-- | @- setShowsIndicator:@
setShowsIndicator :: IsNSMenuToolbarItem nsMenuToolbarItem => nsMenuToolbarItem -> Bool -> IO ()
setShowsIndicator nsMenuToolbarItem value =
  sendMessage nsMenuToolbarItem setShowsIndicatorSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @menu@
menuSelector :: Selector '[] (Id NSMenu)
menuSelector = mkSelector "menu"

-- | @Selector@ for @setMenu:@
setMenuSelector :: Selector '[Id NSMenu] ()
setMenuSelector = mkSelector "setMenu:"

-- | @Selector@ for @showsIndicator@
showsIndicatorSelector :: Selector '[] Bool
showsIndicatorSelector = mkSelector "showsIndicator"

-- | @Selector@ for @setShowsIndicator:@
setShowsIndicatorSelector :: Selector '[Bool] ()
setShowsIndicatorSelector = mkSelector "setShowsIndicator:"

