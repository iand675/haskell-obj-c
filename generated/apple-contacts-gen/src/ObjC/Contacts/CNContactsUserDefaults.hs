{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , countryCodeSelector
  , sharedDefaultsSelector
  , sortOrderSelector

  -- * Enum types
  , CNContactSortOrder(CNContactSortOrder)
  , pattern CNContactSortOrderNone
  , pattern CNContactSortOrderUserDefault
  , pattern CNContactSortOrderGivenName
  , pattern CNContactSortOrderFamilyName

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' sharedDefaultsSelector

-- | @- sortOrder@
sortOrder :: IsCNContactsUserDefaults cnContactsUserDefaults => cnContactsUserDefaults -> IO CNContactSortOrder
sortOrder cnContactsUserDefaults =
  sendMessage cnContactsUserDefaults sortOrderSelector

-- | @- countryCode@
countryCode :: IsCNContactsUserDefaults cnContactsUserDefaults => cnContactsUserDefaults -> IO (Id NSString)
countryCode cnContactsUserDefaults =
  sendMessage cnContactsUserDefaults countryCodeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedDefaults@
sharedDefaultsSelector :: Selector '[] (Id CNContactsUserDefaults)
sharedDefaultsSelector = mkSelector "sharedDefaults"

-- | @Selector@ for @sortOrder@
sortOrderSelector :: Selector '[] CNContactSortOrder
sortOrderSelector = mkSelector "sortOrder"

-- | @Selector@ for @countryCode@
countryCodeSelector :: Selector '[] (Id NSString)
countryCodeSelector = mkSelector "countryCode"

