{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKLookAroundSnapshot@.
module ObjC.MapKit.MKLookAroundSnapshot
  ( MKLookAroundSnapshot
  , IsMKLookAroundSnapshot(..)
  , image
  , imageSelector


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

import ObjC.MapKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- image@
image :: IsMKLookAroundSnapshot mkLookAroundSnapshot => mkLookAroundSnapshot -> IO (Id NSImage)
image mkLookAroundSnapshot  =
  sendMsg mkLookAroundSnapshot (mkSelector "image") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @image@
imageSelector :: Selector
imageSelector = mkSelector "image"

