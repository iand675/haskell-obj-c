{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCoder@.
module ObjC.AppKit.NSCoder
  ( NSCoder
  , IsNSCoder(..)
  , decodeNXColor
  , decodeNXColorSelector


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

-- | @- decodeNXColor@
decodeNXColor :: IsNSCoder nsCoder => nsCoder -> IO (Id NSColor)
decodeNXColor nsCoder  =
  sendMsg nsCoder (mkSelector "decodeNXColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @decodeNXColor@
decodeNXColorSelector :: Selector
decodeNXColorSelector = mkSelector "decodeNXColor"

