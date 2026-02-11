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

-- | @- mapItems@
mapItems :: IsMKLocalSearchResponse mkLocalSearchResponse => mkLocalSearchResponse -> IO (Id NSArray)
mapItems mkLocalSearchResponse  =
  sendMsg mkLocalSearchResponse (mkSelector "mapItems") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mapItems@
mapItemsSelector :: Selector
mapItemsSelector = mkSelector "mapItems"

