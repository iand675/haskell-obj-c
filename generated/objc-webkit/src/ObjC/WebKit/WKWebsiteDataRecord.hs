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
  , displayNameSelector
  , dataTypesSelector


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

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The display name for the data record. This is usually the domain name.
--
-- ObjC selector: @- displayName@
displayName :: IsWKWebsiteDataRecord wkWebsiteDataRecord => wkWebsiteDataRecord -> IO (Id NSString)
displayName wkWebsiteDataRecord  =
  sendMsg wkWebsiteDataRecord (mkSelector "displayName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The various types of website data that exist for this data record.
--
-- ObjC selector: @- dataTypes@
dataTypes :: IsWKWebsiteDataRecord wkWebsiteDataRecord => wkWebsiteDataRecord -> IO (Id NSSet)
dataTypes wkWebsiteDataRecord  =
  sendMsg wkWebsiteDataRecord (mkSelector "dataTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @displayName@
displayNameSelector :: Selector
displayNameSelector = mkSelector "displayName"

-- | @Selector@ for @dataTypes@
dataTypesSelector :: Selector
dataTypesSelector = mkSelector "dataTypes"

