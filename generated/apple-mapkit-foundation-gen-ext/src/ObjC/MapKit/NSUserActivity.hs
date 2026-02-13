{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- mapItem@
mapItem :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO (Id MKMapItem)
mapItem nsUserActivity =
  sendMessage nsUserActivity mapItemSelector

-- | @- setMapItem:@
setMapItem :: (IsNSUserActivity nsUserActivity, IsMKMapItem value) => nsUserActivity -> value -> IO ()
setMapItem nsUserActivity value =
  sendMessage nsUserActivity setMapItemSelector (toMKMapItem value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mapItem@
mapItemSelector :: Selector '[] (Id MKMapItem)
mapItemSelector = mkSelector "mapItem"

-- | @Selector@ for @setMapItem:@
setMapItemSelector :: Selector '[Id MKMapItem] ()
setMapItemSelector = mkSelector "setMapItem:"

