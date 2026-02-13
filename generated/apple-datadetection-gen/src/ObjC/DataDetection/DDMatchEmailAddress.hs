{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that contains an email address that the data detection system matches.
--
-- The DataDetection framework returns an email match in a @DDMatchEmailAddress@ object, which includes an email address, and optionally a label that categorizes the email address.
--
-- Generated bindings for @DDMatchEmailAddress@.
module ObjC.DataDetection.DDMatchEmailAddress
  ( DDMatchEmailAddress
  , IsDDMatchEmailAddress(..)
  , emailAddress
  , label
  , emailAddressSelector
  , labelSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.DataDetection.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | A string that represents an email address.
--
-- ObjC selector: @- emailAddress@
emailAddress :: IsDDMatchEmailAddress ddMatchEmailAddress => ddMatchEmailAddress -> IO (Id NSString)
emailAddress ddMatchEmailAddress =
  sendMessage ddMatchEmailAddress emailAddressSelector

-- | A string that categorizes an email address, such as Home or Work.
--
-- ObjC selector: @- label@
label :: IsDDMatchEmailAddress ddMatchEmailAddress => ddMatchEmailAddress -> IO (Id NSString)
label ddMatchEmailAddress =
  sendMessage ddMatchEmailAddress labelSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @emailAddress@
emailAddressSelector :: Selector '[] (Id NSString)
emailAddressSelector = mkSelector "emailAddress"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

