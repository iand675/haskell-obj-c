{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSpatialEventTrigger@.
module ObjC.Intents.INSpatialEventTrigger
  ( INSpatialEventTrigger
  , IsINSpatialEventTrigger(..)
  , initWithPlacemark_event
  , placemark
  , event
  , initWithPlacemark_eventSelector
  , placemarkSelector
  , eventSelector

  -- * Enum types
  , INSpatialEvent(INSpatialEvent)
  , pattern INSpatialEventUnknown
  , pattern INSpatialEventArrive
  , pattern INSpatialEventDepart

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

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithPlacemark:event:@
initWithPlacemark_event :: (IsINSpatialEventTrigger inSpatialEventTrigger, IsCLPlacemark placemark) => inSpatialEventTrigger -> placemark -> INSpatialEvent -> IO (Id INSpatialEventTrigger)
initWithPlacemark_event inSpatialEventTrigger  placemark event =
withObjCPtr placemark $ \raw_placemark ->
    sendMsg inSpatialEventTrigger (mkSelector "initWithPlacemark:event:") (retPtr retVoid) [argPtr (castPtr raw_placemark :: Ptr ()), argCLong (coerce event)] >>= ownedObject . castPtr

-- | @- placemark@
placemark :: IsINSpatialEventTrigger inSpatialEventTrigger => inSpatialEventTrigger -> IO (Id CLPlacemark)
placemark inSpatialEventTrigger  =
  sendMsg inSpatialEventTrigger (mkSelector "placemark") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- event@
event :: IsINSpatialEventTrigger inSpatialEventTrigger => inSpatialEventTrigger -> IO INSpatialEvent
event inSpatialEventTrigger  =
  fmap (coerce :: CLong -> INSpatialEvent) $ sendMsg inSpatialEventTrigger (mkSelector "event") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPlacemark:event:@
initWithPlacemark_eventSelector :: Selector
initWithPlacemark_eventSelector = mkSelector "initWithPlacemark:event:"

-- | @Selector@ for @placemark@
placemarkSelector :: Selector
placemarkSelector = mkSelector "placemark"

-- | @Selector@ for @event@
eventSelector :: Selector
eventSelector = mkSelector "event"

