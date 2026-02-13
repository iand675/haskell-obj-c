{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CalAttendee@.
module ObjC.CalendarStore.CalAttendee
  ( CalAttendee
  , IsCalAttendee(..)
  , address
  , commonName
  , status
  , addressSelector
  , commonNameSelector
  , statusSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CalendarStore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- address@
address :: IsCalAttendee calAttendee => calAttendee -> IO (Id NSURL)
address calAttendee =
  sendMessage calAttendee addressSelector

-- | @- commonName@
commonName :: IsCalAttendee calAttendee => calAttendee -> IO (Id NSString)
commonName calAttendee =
  sendMessage calAttendee commonNameSelector

-- | @- status@
status :: IsCalAttendee calAttendee => calAttendee -> IO (Id NSString)
status calAttendee =
  sendMessage calAttendee statusSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @address@
addressSelector :: Selector '[] (Id NSURL)
addressSelector = mkSelector "address"

-- | @Selector@ for @commonName@
commonNameSelector :: Selector '[] (Id NSString)
commonNameSelector = mkSelector "commonName"

-- | @Selector@ for @status@
statusSelector :: Selector '[] (Id NSString)
statusSelector = mkSelector "status"

