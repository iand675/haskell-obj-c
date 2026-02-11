{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKMultiPolygon@.
module ObjC.MapKit.MKMultiPolygon
  ( MKMultiPolygon
  , IsMKMultiPolygon(..)
  , initWithPolygons
  , polygons
  , initWithPolygonsSelector
  , polygonsSelector


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

-- | @- initWithPolygons:@
initWithPolygons :: (IsMKMultiPolygon mkMultiPolygon, IsNSArray polygons) => mkMultiPolygon -> polygons -> IO (Id MKMultiPolygon)
initWithPolygons mkMultiPolygon  polygons =
withObjCPtr polygons $ \raw_polygons ->
    sendMsg mkMultiPolygon (mkSelector "initWithPolygons:") (retPtr retVoid) [argPtr (castPtr raw_polygons :: Ptr ())] >>= ownedObject . castPtr

-- | @- polygons@
polygons :: IsMKMultiPolygon mkMultiPolygon => mkMultiPolygon -> IO (Id NSArray)
polygons mkMultiPolygon  =
  sendMsg mkMultiPolygon (mkSelector "polygons") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPolygons:@
initWithPolygonsSelector :: Selector
initWithPolygonsSelector = mkSelector "initWithPolygons:"

-- | @Selector@ for @polygons@
polygonsSelector :: Selector
polygonsSelector = mkSelector "polygons"

