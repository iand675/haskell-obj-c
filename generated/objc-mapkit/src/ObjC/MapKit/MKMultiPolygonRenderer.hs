{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKMultiPolygonRenderer@.
module ObjC.MapKit.MKMultiPolygonRenderer
  ( MKMultiPolygonRenderer
  , IsMKMultiPolygonRenderer(..)
  , initWithMultiPolygon
  , multiPolygon
  , initWithMultiPolygonSelector
  , multiPolygonSelector


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

-- | @- initWithMultiPolygon:@
initWithMultiPolygon :: (IsMKMultiPolygonRenderer mkMultiPolygonRenderer, IsMKMultiPolygon multiPolygon) => mkMultiPolygonRenderer -> multiPolygon -> IO (Id MKMultiPolygonRenderer)
initWithMultiPolygon mkMultiPolygonRenderer  multiPolygon =
withObjCPtr multiPolygon $ \raw_multiPolygon ->
    sendMsg mkMultiPolygonRenderer (mkSelector "initWithMultiPolygon:") (retPtr retVoid) [argPtr (castPtr raw_multiPolygon :: Ptr ())] >>= ownedObject . castPtr

-- | @- multiPolygon@
multiPolygon :: IsMKMultiPolygonRenderer mkMultiPolygonRenderer => mkMultiPolygonRenderer -> IO (Id MKMultiPolygon)
multiPolygon mkMultiPolygonRenderer  =
  sendMsg mkMultiPolygonRenderer (mkSelector "multiPolygon") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMultiPolygon:@
initWithMultiPolygonSelector :: Selector
initWithMultiPolygonSelector = mkSelector "initWithMultiPolygon:"

-- | @Selector@ for @multiPolygon@
multiPolygonSelector :: Selector
multiPolygonSelector = mkSelector "multiPolygon"

