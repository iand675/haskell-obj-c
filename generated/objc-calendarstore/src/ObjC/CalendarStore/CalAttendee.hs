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

import ObjC.CalendarStore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- address@
address :: IsCalAttendee calAttendee => calAttendee -> IO (Id NSURL)
address calAttendee  =
  sendMsg calAttendee (mkSelector "address") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- commonName@
commonName :: IsCalAttendee calAttendee => calAttendee -> IO (Id NSString)
commonName calAttendee  =
  sendMsg calAttendee (mkSelector "commonName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- status@
status :: IsCalAttendee calAttendee => calAttendee -> IO (Id NSString)
status calAttendee  =
  sendMsg calAttendee (mkSelector "status") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @address@
addressSelector :: Selector
addressSelector = mkSelector "address"

-- | @Selector@ for @commonName@
commonNameSelector :: Selector
commonNameSelector = mkSelector "commonName"

-- | @Selector@ for @status@
statusSelector :: Selector
statusSelector = mkSelector "status"

