{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKMapSnapshot@.
module ObjC.MapKit.MKMapSnapshot
  ( MKMapSnapshot
  , IsMKMapSnapshot(..)
  , image
  , appearance
  , imageSelector
  , appearanceSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- image@
image :: IsMKMapSnapshot mkMapSnapshot => mkMapSnapshot -> IO (Id NSImage)
image mkMapSnapshot  =
    sendMsg mkMapSnapshot (mkSelector "image") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- appearance@
appearance :: IsMKMapSnapshot mkMapSnapshot => mkMapSnapshot -> IO (Id NSAppearance)
appearance mkMapSnapshot  =
    sendMsg mkMapSnapshot (mkSelector "appearance") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @image@
imageSelector :: Selector
imageSelector = mkSelector "image"

-- | @Selector@ for @appearance@
appearanceSelector :: Selector
appearanceSelector = mkSelector "appearance"

