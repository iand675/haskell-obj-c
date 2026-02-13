{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDataTypeAtomicAttributeStatusStruct@.
module ObjC.Matter.MTRDataTypeAtomicAttributeStatusStruct
  ( MTRDataTypeAtomicAttributeStatusStruct
  , IsMTRDataTypeAtomicAttributeStatusStruct(..)
  , attributeID
  , setAttributeID
  , statusCode
  , setStatusCode
  , attributeIDSelector
  , setAttributeIDSelector
  , setStatusCodeSelector
  , statusCodeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- attributeID@
attributeID :: IsMTRDataTypeAtomicAttributeStatusStruct mtrDataTypeAtomicAttributeStatusStruct => mtrDataTypeAtomicAttributeStatusStruct -> IO (Id NSNumber)
attributeID mtrDataTypeAtomicAttributeStatusStruct =
  sendMessage mtrDataTypeAtomicAttributeStatusStruct attributeIDSelector

-- | @- setAttributeID:@
setAttributeID :: (IsMTRDataTypeAtomicAttributeStatusStruct mtrDataTypeAtomicAttributeStatusStruct, IsNSNumber value) => mtrDataTypeAtomicAttributeStatusStruct -> value -> IO ()
setAttributeID mtrDataTypeAtomicAttributeStatusStruct value =
  sendMessage mtrDataTypeAtomicAttributeStatusStruct setAttributeIDSelector (toNSNumber value)

-- | @- statusCode@
statusCode :: IsMTRDataTypeAtomicAttributeStatusStruct mtrDataTypeAtomicAttributeStatusStruct => mtrDataTypeAtomicAttributeStatusStruct -> IO (Id NSNumber)
statusCode mtrDataTypeAtomicAttributeStatusStruct =
  sendMessage mtrDataTypeAtomicAttributeStatusStruct statusCodeSelector

-- | @- setStatusCode:@
setStatusCode :: (IsMTRDataTypeAtomicAttributeStatusStruct mtrDataTypeAtomicAttributeStatusStruct, IsNSNumber value) => mtrDataTypeAtomicAttributeStatusStruct -> value -> IO ()
setStatusCode mtrDataTypeAtomicAttributeStatusStruct value =
  sendMessage mtrDataTypeAtomicAttributeStatusStruct setStatusCodeSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @attributeID@
attributeIDSelector :: Selector '[] (Id NSNumber)
attributeIDSelector = mkSelector "attributeID"

-- | @Selector@ for @setAttributeID:@
setAttributeIDSelector :: Selector '[Id NSNumber] ()
setAttributeIDSelector = mkSelector "setAttributeID:"

-- | @Selector@ for @statusCode@
statusCodeSelector :: Selector '[] (Id NSNumber)
statusCodeSelector = mkSelector "statusCode"

-- | @Selector@ for @setStatusCode:@
setStatusCodeSelector :: Selector '[Id NSNumber] ()
setStatusCodeSelector = mkSelector "setStatusCode:"

