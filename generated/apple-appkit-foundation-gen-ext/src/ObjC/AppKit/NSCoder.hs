{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- decodeNXColor@
decodeNXColor :: IsNSCoder nsCoder => nsCoder -> IO (Id NSColor)
decodeNXColor nsCoder =
  sendMessage nsCoder decodeNXColorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @decodeNXColor@
decodeNXColorSelector :: Selector '[] (Id NSColor)
decodeNXColorSelector = mkSelector "decodeNXColor"

