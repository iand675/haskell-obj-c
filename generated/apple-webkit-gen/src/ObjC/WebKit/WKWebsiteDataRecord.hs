{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A WKWebsiteDataRecord represents website data, grouped by domain name using the public suffix list.
--
-- Generated bindings for @WKWebsiteDataRecord@.
module ObjC.WebKit.WKWebsiteDataRecord
  ( WKWebsiteDataRecord
  , IsWKWebsiteDataRecord(..)
  , displayName
  , dataTypes
  , dataTypesSelector
  , displayNameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The display name for the data record. This is usually the domain name.
--
-- ObjC selector: @- displayName@
displayName :: IsWKWebsiteDataRecord wkWebsiteDataRecord => wkWebsiteDataRecord -> IO (Id NSString)
displayName wkWebsiteDataRecord =
  sendMessage wkWebsiteDataRecord displayNameSelector

-- | The various types of website data that exist for this data record.
--
-- ObjC selector: @- dataTypes@
dataTypes :: IsWKWebsiteDataRecord wkWebsiteDataRecord => wkWebsiteDataRecord -> IO (Id NSSet)
dataTypes wkWebsiteDataRecord =
  sendMessage wkWebsiteDataRecord dataTypesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @displayName@
displayNameSelector :: Selector '[] (Id NSString)
displayNameSelector = mkSelector "displayName"

-- | @Selector@ for @dataTypes@
dataTypesSelector :: Selector '[] (Id NSSet)
dataTypesSelector = mkSelector "dataTypes"

