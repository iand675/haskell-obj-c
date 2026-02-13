{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that contains a phone number that the data detection system matches.
--
-- The DataDetection framework returns a phone number match in a @DDMatchPhoneNumber@ object, which contains a phone number, and optionally a label that categorizes the phone number.
--
-- Generated bindings for @DDMatchPhoneNumber@.
module ObjC.DataDetection.DDMatchPhoneNumber
  ( DDMatchPhoneNumber
  , IsDDMatchPhoneNumber(..)
  , phoneNumber
  , label
  , labelSelector
  , phoneNumberSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.DataDetection.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | A string that represents a phone number.
--
-- ObjC selector: @- phoneNumber@
phoneNumber :: IsDDMatchPhoneNumber ddMatchPhoneNumber => ddMatchPhoneNumber -> IO (Id NSString)
phoneNumber ddMatchPhoneNumber =
  sendMessage ddMatchPhoneNumber phoneNumberSelector

-- | A string that categorizes a phone number, such as Home or Work.
--
-- ObjC selector: @- label@
label :: IsDDMatchPhoneNumber ddMatchPhoneNumber => ddMatchPhoneNumber -> IO (Id NSString)
label ddMatchPhoneNumber =
  sendMessage ddMatchPhoneNumber labelSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @phoneNumber@
phoneNumberSelector :: Selector '[] (Id NSString)
phoneNumberSelector = mkSelector "phoneNumber"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

