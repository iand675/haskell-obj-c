{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CSPerson@.
module ObjC.CoreSpotlight.CSPerson
  ( CSPerson
  , IsCSPerson(..)
  , initWithDisplayName_handles_handleIdentifier
  , displayName
  , handles
  , handleIdentifier
  , contactIdentifier
  , setContactIdentifier
  , contactIdentifierSelector
  , displayNameSelector
  , handleIdentifierSelector
  , handlesSelector
  , initWithDisplayName_handles_handleIdentifierSelector
  , setContactIdentifierSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreSpotlight.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithDisplayName:handles:handleIdentifier:@
initWithDisplayName_handles_handleIdentifier :: (IsCSPerson csPerson, IsNSString displayName, IsNSArray handles, IsNSString handleIdentifier) => csPerson -> displayName -> handles -> handleIdentifier -> IO (Id CSPerson)
initWithDisplayName_handles_handleIdentifier csPerson displayName handles handleIdentifier =
  sendOwnedMessage csPerson initWithDisplayName_handles_handleIdentifierSelector (toNSString displayName) (toNSArray handles) (toNSString handleIdentifier)

-- | @- displayName@
displayName :: IsCSPerson csPerson => csPerson -> IO (Id NSString)
displayName csPerson =
  sendMessage csPerson displayNameSelector

-- | @- handles@
handles :: IsCSPerson csPerson => csPerson -> IO (Id NSArray)
handles csPerson =
  sendMessage csPerson handlesSelector

-- | @- handleIdentifier@
handleIdentifier :: IsCSPerson csPerson => csPerson -> IO (Id NSString)
handleIdentifier csPerson =
  sendMessage csPerson handleIdentifierSelector

-- | @- contactIdentifier@
contactIdentifier :: IsCSPerson csPerson => csPerson -> IO (Id NSString)
contactIdentifier csPerson =
  sendMessage csPerson contactIdentifierSelector

-- | @- setContactIdentifier:@
setContactIdentifier :: (IsCSPerson csPerson, IsNSString value) => csPerson -> value -> IO ()
setContactIdentifier csPerson value =
  sendMessage csPerson setContactIdentifierSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDisplayName:handles:handleIdentifier:@
initWithDisplayName_handles_handleIdentifierSelector :: Selector '[Id NSString, Id NSArray, Id NSString] (Id CSPerson)
initWithDisplayName_handles_handleIdentifierSelector = mkSelector "initWithDisplayName:handles:handleIdentifier:"

-- | @Selector@ for @displayName@
displayNameSelector :: Selector '[] (Id NSString)
displayNameSelector = mkSelector "displayName"

-- | @Selector@ for @handles@
handlesSelector :: Selector '[] (Id NSArray)
handlesSelector = mkSelector "handles"

-- | @Selector@ for @handleIdentifier@
handleIdentifierSelector :: Selector '[] (Id NSString)
handleIdentifierSelector = mkSelector "handleIdentifier"

-- | @Selector@ for @contactIdentifier@
contactIdentifierSelector :: Selector '[] (Id NSString)
contactIdentifierSelector = mkSelector "contactIdentifier"

-- | @Selector@ for @setContactIdentifier:@
setContactIdentifierSelector :: Selector '[Id NSString] ()
setContactIdentifierSelector = mkSelector "setContactIdentifier:"

