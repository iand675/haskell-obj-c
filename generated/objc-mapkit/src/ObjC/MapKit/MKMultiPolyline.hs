{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKMultiPolyline@.
module ObjC.MapKit.MKMultiPolyline
  ( MKMultiPolyline
  , IsMKMultiPolyline(..)
  , initWithPolylines
  , polylines
  , initWithPolylinesSelector
  , polylinesSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- initWithPolylines:@
initWithPolylines :: (IsMKMultiPolyline mkMultiPolyline, IsNSArray polylines) => mkMultiPolyline -> polylines -> IO (Id MKMultiPolyline)
initWithPolylines mkMultiPolyline  polylines =
withObjCPtr polylines $ \raw_polylines ->
    sendMsg mkMultiPolyline (mkSelector "initWithPolylines:") (retPtr retVoid) [argPtr (castPtr raw_polylines :: Ptr ())] >>= ownedObject . castPtr

-- | @- polylines@
polylines :: IsMKMultiPolyline mkMultiPolyline => mkMultiPolyline -> IO (Id NSArray)
polylines mkMultiPolyline  =
  sendMsg mkMultiPolyline (mkSelector "polylines") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPolylines:@
initWithPolylinesSelector :: Selector
initWithPolylinesSelector = mkSelector "initWithPolylines:"

-- | @Selector@ for @polylines@
polylinesSelector :: Selector
polylinesSelector = mkSelector "polylines"

