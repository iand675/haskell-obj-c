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

-- | @- icon@
icon :: IsNSFileWrapper nsFileWrapper => nsFileWrapper -> IO (Id NSImage)
icon nsFileWrapper  =
  sendMsg nsFileWrapper (mkSelector "icon") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIcon:@
setIcon :: (IsNSFileWrapper nsFileWrapper, IsNSImage value) => nsFileWrapper -> value -> IO ()
setIcon nsFileWrapper  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsFileWrapper (mkSelector "setIcon:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @icon@
iconSelector :: Selector
iconSelector = mkSelector "icon"

-- | @Selector@ for @setIcon:@
setIconSelector :: Selector
setIconSelector = mkSelector "setIcon:"

