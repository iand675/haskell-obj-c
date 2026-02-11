{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKMultiPolylineRenderer@.
module ObjC.MapKit.MKMultiPolylineRenderer
  ( MKMultiPolylineRenderer
  , IsMKMultiPolylineRenderer(..)
  , initWithMultiPolyline
  , multiPolyline
  , initWithMultiPolylineSelector
  , multiPolylineSelector


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

-- | @- initWithMultiPolyline:@
initWithMultiPolyline :: (IsMKMultiPolylineRenderer mkMultiPolylineRenderer, IsMKMultiPolyline multiPolyline) => mkMultiPolylineRenderer -> multiPolyline -> IO (Id MKMultiPolylineRenderer)
initWithMultiPolyline mkMultiPolylineRenderer  multiPolyline =
withObjCPtr multiPolyline $ \raw_multiPolyline ->
    sendMsg mkMultiPolylineRenderer (mkSelector "initWithMultiPolyline:") (retPtr retVoid) [argPtr (castPtr raw_multiPolyline :: Ptr ())] >>= ownedObject . castPtr

-- | @- multiPolyline@
multiPolyline :: IsMKMultiPolylineRenderer mkMultiPolylineRenderer => mkMultiPolylineRenderer -> IO (Id MKMultiPolyline)
multiPolyline mkMultiPolylineRenderer  =
  sendMsg mkMultiPolylineRenderer (mkSelector "multiPolyline") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMultiPolyline:@
initWithMultiPolylineSelector :: Selector
initWithMultiPolylineSelector = mkSelector "initWithMultiPolyline:"

-- | @Selector@ for @multiPolyline@
multiPolylineSelector :: Selector
multiPolylineSelector = mkSelector "multiPolyline"

