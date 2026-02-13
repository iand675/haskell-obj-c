{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKLocalSearchResponse@.
module ObjC.MapKit.MKLocalSearchResponse
  ( MKLocalSearchResponse
  , IsMKLocalSearchResponse(..)
  , mapItems
  , mapItemsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- mapItems@
mapItems :: IsMKLocalSearchResponse mkLocalSearchResponse => mkLocalSearchResponse -> IO (Id NSArray)
mapItems mkLocalSearchResponse =
  sendMessage mkLocalSearchResponse mapItemsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mapItems@
mapItemsSelector :: Selector '[] (Id NSArray)
mapItemsSelector = mkSelector "mapItems"

