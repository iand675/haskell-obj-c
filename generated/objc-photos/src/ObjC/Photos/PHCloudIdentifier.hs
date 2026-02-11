{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PHCloudIdentifier@.
module ObjC.Photos.PHCloudIdentifier
  ( PHCloudIdentifier
  , IsPHCloudIdentifier(..)
  , initWithStringValue
  , stringValue
  , initWithStringValueSelector
  , stringValueSelector


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

import ObjC.Photos.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | For use in serialization
--
-- ObjC selector: @- initWithStringValue:@
initWithStringValue :: (IsPHCloudIdentifier phCloudIdentifier, IsNSString stringValue) => phCloudIdentifier -> stringValue -> IO (Id PHCloudIdentifier)
initWithStringValue phCloudIdentifier  stringValue =
withObjCPtr stringValue $ \raw_stringValue ->
    sendMsg phCloudIdentifier (mkSelector "initWithStringValue:") (retPtr retVoid) [argPtr (castPtr raw_stringValue :: Ptr ())] >>= ownedObject . castPtr

-- | @- stringValue@
stringValue :: IsPHCloudIdentifier phCloudIdentifier => phCloudIdentifier -> IO (Id NSString)
stringValue phCloudIdentifier  =
  sendMsg phCloudIdentifier (mkSelector "stringValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithStringValue:@
initWithStringValueSelector :: Selector
initWithStringValueSelector = mkSelector "initWithStringValue:"

-- | @Selector@ for @stringValue@
stringValueSelector :: Selector
stringValueSelector = mkSelector "stringValue"

