{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The user defaults for contacts.
--
-- Note: This class is not thread safe.
--
-- Generated bindings for @CNContactsUserDefaults@.
module ObjC.Contacts.CNContactsUserDefaults
  ( CNContactsUserDefaults
  , IsCNContactsUserDefaults(..)
  , sharedDefaults
  , sortOrder
  , countryCode
  , sharedDefaultsSelector
  , sortOrderSelector
  , countryCodeSelector

  -- * Enum types
  , CNContactSortOrder(CNContactSortOrder)
  , pattern CNContactSortOrderNone
  , pattern CNContactSortOrderUserDefault
  , pattern CNContactSortOrderGivenName
  , pattern CNContactSortOrderFamilyName

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

import ObjC.Contacts.Internal.Classes
import ObjC.Contacts.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ sharedDefaults@
sharedDefaults :: IO (Id CNContactsUserDefaults)
sharedDefaults  =
  do
    cls' <- getRequiredClass "CNContactsUserDefaults"
    sendClassMsg cls' (mkSelector "sharedDefaults") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- sortOrder@
sortOrder :: IsCNContactsUserDefaults cnContactsUserDefaults => cnContactsUserDefaults -> IO CNContactSortOrder
sortOrder cnContactsUserDefaults  =
  fmap (coerce :: CLong -> CNContactSortOrder) $ sendMsg cnContactsUserDefaults (mkSelector "sortOrder") retCLong []

-- | @- countryCode@
countryCode :: IsCNContactsUserDefaults cnContactsUserDefaults => cnContactsUserDefaults -> IO (Id NSString)
countryCode cnContactsUserDefaults  =
  sendMsg cnContactsUserDefaults (mkSelector "countryCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedDefaults@
sharedDefaultsSelector :: Selector
sharedDefaultsSelector = mkSelector "sharedDefaults"

-- | @Selector@ for @sortOrder@
sortOrderSelector :: Selector
sortOrderSelector = mkSelector "sortOrder"

-- | @Selector@ for @countryCode@
countryCodeSelector :: Selector
countryCodeSelector = mkSelector "countryCode"

