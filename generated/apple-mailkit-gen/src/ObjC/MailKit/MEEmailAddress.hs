{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Contain information about an email address. This can include both valid and invalid email addresses.
--
-- Generated bindings for @MEEmailAddress@.
module ObjC.MailKit.MEEmailAddress
  ( MEEmailAddress
  , IsMEEmailAddress(..)
  , new
  , init_
  , initWithRawString
  , rawString
  , addressString
  , addressStringSelector
  , initSelector
  , initWithRawStringSelector
  , newSelector
  , rawStringSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MailKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MEEmailAddress)
new  =
  do
    cls' <- getRequiredClass "MEEmailAddress"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsMEEmailAddress meEmailAddress => meEmailAddress -> IO (Id MEEmailAddress)
init_ meEmailAddress =
  sendOwnedMessage meEmailAddress initSelector

-- | @- initWithRawString:@
initWithRawString :: (IsMEEmailAddress meEmailAddress, IsNSString rawString) => meEmailAddress -> rawString -> IO (Id MEEmailAddress)
initWithRawString meEmailAddress rawString =
  sendOwnedMessage meEmailAddress initWithRawStringSelector (toNSString rawString)

-- | The raw string for the email address.
--
-- ObjC selector: @- rawString@
rawString :: IsMEEmailAddress meEmailAddress => meEmailAddress -> IO (Id NSString)
rawString meEmailAddress =
  sendMessage meEmailAddress rawStringSelector

-- | The simple address string portion of the raw string if it is valid. For example, the  @addressString@ of "John Appleseed <j.appleseed\@example.com>" will be "j.appleseed\@example.com".
--
-- ObjC selector: @- addressString@
addressString :: IsMEEmailAddress meEmailAddress => meEmailAddress -> IO (Id NSString)
addressString meEmailAddress =
  sendMessage meEmailAddress addressStringSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MEEmailAddress)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MEEmailAddress)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithRawString:@
initWithRawStringSelector :: Selector '[Id NSString] (Id MEEmailAddress)
initWithRawStringSelector = mkSelector "initWithRawString:"

-- | @Selector@ for @rawString@
rawStringSelector :: Selector '[] (Id NSString)
rawStringSelector = mkSelector "rawString"

-- | @Selector@ for @addressString@
addressStringSelector :: Selector '[] (Id NSString)
addressStringSelector = mkSelector "addressString"

