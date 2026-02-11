{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSMenuToolbarItem@.
module ObjC.AppKit.NSMenuToolbarItem
  ( NSMenuToolbarItem
  , IsNSMenuToolbarItem(..)
  , showsIndicator
  , setShowsIndicator
  , showsIndicatorSelector
  , setShowsIndicatorSelector


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

-- | @- showsIndicator@
showsIndicator :: IsNSMenuToolbarItem nsMenuToolbarItem => nsMenuToolbarItem -> IO Bool
showsIndicator nsMenuToolbarItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMenuToolbarItem (mkSelector "showsIndicator") retCULong []

-- | @- setShowsIndicator:@
setShowsIndicator :: IsNSMenuToolbarItem nsMenuToolbarItem => nsMenuToolbarItem -> Bool -> IO ()
setShowsIndicator nsMenuToolbarItem  value =
  sendMsg nsMenuToolbarItem (mkSelector "setShowsIndicator:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @showsIndicator@
showsIndicatorSelector :: Selector
showsIndicatorSelector = mkSelector "showsIndicator"

-- | @Selector@ for @setShowsIndicator:@
setShowsIndicatorSelector :: Selector
setShowsIndicatorSelector = mkSelector "setShowsIndicator:"

