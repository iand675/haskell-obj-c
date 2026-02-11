{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INTicketedEvent@.
module ObjC.Intents.INTicketedEvent
  ( INTicketedEvent
  , IsINTicketedEvent(..)
  , init_
  , initWithCategory_name_eventDuration_location
  , category
  , name
  , eventDuration
  , location
  , initSelector
  , initWithCategory_name_eventDuration_locationSelector
  , categorySelector
  , nameSelector
  , eventDurationSelector
  , locationSelector

  -- * Enum types
  , INTicketedEventCategory(INTicketedEventCategory)
  , pattern INTicketedEventCategoryUnknown
  , pattern INTicketedEventCategoryMovie

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

-- | @- init@
init_ :: IsINTicketedEvent inTicketedEvent => inTicketedEvent -> IO (Id INTicketedEvent)
init_ inTicketedEvent  =
  sendMsg inTicketedEvent (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithCategory:name:eventDuration:location:@
initWithCategory_name_eventDuration_location :: (IsINTicketedEvent inTicketedEvent, IsNSString name, IsINDateComponentsRange eventDuration, IsCLPlacemark location) => inTicketedEvent -> INTicketedEventCategory -> name -> eventDuration -> location -> IO (Id INTicketedEvent)
initWithCategory_name_eventDuration_location inTicketedEvent  category name eventDuration location =
withObjCPtr name $ \raw_name ->
  withObjCPtr eventDuration $ \raw_eventDuration ->
    withObjCPtr location $ \raw_location ->
        sendMsg inTicketedEvent (mkSelector "initWithCategory:name:eventDuration:location:") (retPtr retVoid) [argCLong (coerce category), argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_eventDuration :: Ptr ()), argPtr (castPtr raw_location :: Ptr ())] >>= ownedObject . castPtr

-- | @- category@
category :: IsINTicketedEvent inTicketedEvent => inTicketedEvent -> IO INTicketedEventCategory
category inTicketedEvent  =
  fmap (coerce :: CLong -> INTicketedEventCategory) $ sendMsg inTicketedEvent (mkSelector "category") retCLong []

-- | @- name@
name :: IsINTicketedEvent inTicketedEvent => inTicketedEvent -> IO (Id NSString)
name inTicketedEvent  =
  sendMsg inTicketedEvent (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- eventDuration@
eventDuration :: IsINTicketedEvent inTicketedEvent => inTicketedEvent -> IO (Id INDateComponentsRange)
eventDuration inTicketedEvent  =
  sendMsg inTicketedEvent (mkSelector "eventDuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- location@
location :: IsINTicketedEvent inTicketedEvent => inTicketedEvent -> IO (Id CLPlacemark)
location inTicketedEvent  =
  sendMsg inTicketedEvent (mkSelector "location") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCategory:name:eventDuration:location:@
initWithCategory_name_eventDuration_locationSelector :: Selector
initWithCategory_name_eventDuration_locationSelector = mkSelector "initWithCategory:name:eventDuration:location:"

-- | @Selector@ for @category@
categorySelector :: Selector
categorySelector = mkSelector "category"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @eventDuration@
eventDurationSelector :: Selector
eventDurationSelector = mkSelector "eventDuration"

-- | @Selector@ for @location@
locationSelector :: Selector
locationSelector = mkSelector "location"

