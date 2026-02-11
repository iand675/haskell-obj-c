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
  , initWithDisplayName_handles_handleIdentifierSelector
  , displayNameSelector
  , handlesSelector
  , handleIdentifierSelector
  , contactIdentifierSelector
  , setContactIdentifierSelector


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

import ObjC.CoreSpotlight.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithDisplayName:handles:handleIdentifier:@
initWithDisplayName_handles_handleIdentifier :: (IsCSPerson csPerson, IsNSString displayName, IsNSArray handles, IsNSString handleIdentifier) => csPerson -> displayName -> handles -> handleIdentifier -> IO (Id CSPerson)
initWithDisplayName_handles_handleIdentifier csPerson  displayName handles handleIdentifier =
withObjCPtr displayName $ \raw_displayName ->
  withObjCPtr handles $ \raw_handles ->
    withObjCPtr handleIdentifier $ \raw_handleIdentifier ->
        sendMsg csPerson (mkSelector "initWithDisplayName:handles:handleIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_displayName :: Ptr ()), argPtr (castPtr raw_handles :: Ptr ()), argPtr (castPtr raw_handleIdentifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- displayName@
displayName :: IsCSPerson csPerson => csPerson -> IO (Id NSString)
displayName csPerson  =
  sendMsg csPerson (mkSelector "displayName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- handles@
handles :: IsCSPerson csPerson => csPerson -> IO (Id NSArray)
handles csPerson  =
  sendMsg csPerson (mkSelector "handles") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- handleIdentifier@
handleIdentifier :: IsCSPerson csPerson => csPerson -> IO (Id NSString)
handleIdentifier csPerson  =
  sendMsg csPerson (mkSelector "handleIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- contactIdentifier@
contactIdentifier :: IsCSPerson csPerson => csPerson -> IO (Id NSString)
contactIdentifier csPerson  =
  sendMsg csPerson (mkSelector "contactIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setContactIdentifier:@
setContactIdentifier :: (IsCSPerson csPerson, IsNSString value) => csPerson -> value -> IO ()
setContactIdentifier csPerson  value =
withObjCPtr value $ \raw_value ->
    sendMsg csPerson (mkSelector "setContactIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDisplayName:handles:handleIdentifier:@
initWithDisplayName_handles_handleIdentifierSelector :: Selector
initWithDisplayName_handles_handleIdentifierSelector = mkSelector "initWithDisplayName:handles:handleIdentifier:"

-- | @Selector@ for @displayName@
displayNameSelector :: Selector
displayNameSelector = mkSelector "displayName"

-- | @Selector@ for @handles@
handlesSelector :: Selector
handlesSelector = mkSelector "handles"

-- | @Selector@ for @handleIdentifier@
handleIdentifierSelector :: Selector
handleIdentifierSelector = mkSelector "handleIdentifier"

-- | @Selector@ for @contactIdentifier@
contactIdentifierSelector :: Selector
contactIdentifierSelector = mkSelector "contactIdentifier"

-- | @Selector@ for @setContactIdentifier:@
setContactIdentifierSelector :: Selector
setContactIdentifierSelector = mkSelector "setContactIdentifier:"

