{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PHCloudIdentifier@.
module ObjC.Photos.PHCloudIdentifier
  ( PHCloudIdentifier
  , IsPHCloudIdentifier(..)
  , initWithStringValue
  , notFoundIdentifier
  , stringValue
  , initWithStringValueSelector
  , notFoundIdentifierSelector
  , stringValueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Photos.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | For use in serialization
--
-- ObjC selector: @- initWithStringValue:@
initWithStringValue :: (IsPHCloudIdentifier phCloudIdentifier, IsNSString stringValue) => phCloudIdentifier -> stringValue -> IO (Id PHCloudIdentifier)
initWithStringValue phCloudIdentifier stringValue =
  sendOwnedMessage phCloudIdentifier initWithStringValueSelector (toNSString stringValue)

-- | DEPRECATED: If there is a failure to determine the global identifier for a local identifier, the notFoundIdentifier is provided in that array slot.
--
-- ObjC selector: @+ notFoundIdentifier@
notFoundIdentifier :: IO (Id PHCloudIdentifier)
notFoundIdentifier  =
  do
    cls' <- getRequiredClass "PHCloudIdentifier"
    sendClassMessage cls' notFoundIdentifierSelector

-- | @- stringValue@
stringValue :: IsPHCloudIdentifier phCloudIdentifier => phCloudIdentifier -> IO (Id NSString)
stringValue phCloudIdentifier =
  sendMessage phCloudIdentifier stringValueSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithStringValue:@
initWithStringValueSelector :: Selector '[Id NSString] (Id PHCloudIdentifier)
initWithStringValueSelector = mkSelector "initWithStringValue:"

-- | @Selector@ for @notFoundIdentifier@
notFoundIdentifierSelector :: Selector '[] (Id PHCloudIdentifier)
notFoundIdentifierSelector = mkSelector "notFoundIdentifier"

-- | @Selector@ for @stringValue@
stringValueSelector :: Selector '[] (Id NSString)
stringValueSelector = mkSelector "stringValue"

