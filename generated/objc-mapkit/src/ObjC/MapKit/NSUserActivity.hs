{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUserActivity@.
module ObjC.MapKit.NSUserActivity
  ( NSUserActivity
  , IsNSUserActivity(..)
  , mapItem
  , setMapItem
  , mapItemSelector
  , setMapItemSelector


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

-- | @- mapItem@
mapItem :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO (Id MKMapItem)
mapItem nsUserActivity  =
  sendMsg nsUserActivity (mkSelector "mapItem") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMapItem:@
setMapItem :: (IsNSUserActivity nsUserActivity, IsMKMapItem value) => nsUserActivity -> value -> IO ()
setMapItem nsUserActivity  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsUserActivity (mkSelector "setMapItem:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mapItem@
mapItemSelector :: Selector
mapItemSelector = mkSelector "mapItem"

-- | @Selector@ for @setMapItem:@
setMapItemSelector :: Selector
setMapItemSelector = mkSelector "setMapItem:"

