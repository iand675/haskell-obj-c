{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSFileWrapper@.
module ObjC.AppKit.NSFileWrapper
  ( NSFileWrapper
  , IsNSFileWrapper(..)
  , icon
  , setIcon
  , iconSelector
  , setIconSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- icon@
icon :: IsNSFileWrapper nsFileWrapper => nsFileWrapper -> IO (Id NSImage)
icon nsFileWrapper =
  sendMessage nsFileWrapper iconSelector

-- | @- setIcon:@
setIcon :: (IsNSFileWrapper nsFileWrapper, IsNSImage value) => nsFileWrapper -> value -> IO ()
setIcon nsFileWrapper value =
  sendMessage nsFileWrapper setIconSelector (toNSImage value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @icon@
iconSelector :: Selector '[] (Id NSImage)
iconSelector = mkSelector "icon"

-- | @Selector@ for @setIcon:@
setIconSelector :: Selector '[Id NSImage] ()
setIconSelector = mkSelector "setIcon:"

