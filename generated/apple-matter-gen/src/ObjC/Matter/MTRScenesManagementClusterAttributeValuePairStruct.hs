{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRScenesManagementClusterAttributeValuePairStruct@.
module ObjC.Matter.MTRScenesManagementClusterAttributeValuePairStruct
  ( MTRScenesManagementClusterAttributeValuePairStruct
  , IsMTRScenesManagementClusterAttributeValuePairStruct(..)
  , attributeID
  , setAttributeID
  , valueUnsigned8
  , setValueUnsigned8
  , valueSigned8
  , setValueSigned8
  , valueUnsigned16
  , setValueUnsigned16
  , valueSigned16
  , setValueSigned16
  , valueUnsigned32
  , setValueUnsigned32
  , valueSigned32
  , setValueSigned32
  , valueUnsigned64
  , setValueUnsigned64
  , valueSigned64
  , setValueSigned64
  , attributeIDSelector
  , setAttributeIDSelector
  , setValueSigned16Selector
  , setValueSigned32Selector
  , setValueSigned64Selector
  , setValueSigned8Selector
  , setValueUnsigned16Selector
  , setValueUnsigned32Selector
  , setValueUnsigned64Selector
  , setValueUnsigned8Selector
  , valueSigned16Selector
  , valueSigned32Selector
  , valueSigned64Selector
  , valueSigned8Selector
  , valueUnsigned16Selector
  , valueUnsigned32Selector
  , valueUnsigned64Selector
  , valueUnsigned8Selector


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
attributeID :: IsMTRScenesManagementClusterAttributeValuePairStruct mtrScenesManagementClusterAttributeValuePairStruct => mtrScenesManagementClusterAttributeValuePairStruct -> IO (Id NSNumber)
attributeID mtrScenesManagementClusterAttributeValuePairStruct =
  sendMessage mtrScenesManagementClusterAttributeValuePairStruct attributeIDSelector

-- | @- setAttributeID:@
setAttributeID :: (IsMTRScenesManagementClusterAttributeValuePairStruct mtrScenesManagementClusterAttributeValuePairStruct, IsNSNumber value) => mtrScenesManagementClusterAttributeValuePairStruct -> value -> IO ()
setAttributeID mtrScenesManagementClusterAttributeValuePairStruct value =
  sendMessage mtrScenesManagementClusterAttributeValuePairStruct setAttributeIDSelector (toNSNumber value)

-- | @- valueUnsigned8@
valueUnsigned8 :: IsMTRScenesManagementClusterAttributeValuePairStruct mtrScenesManagementClusterAttributeValuePairStruct => mtrScenesManagementClusterAttributeValuePairStruct -> IO (Id NSNumber)
valueUnsigned8 mtrScenesManagementClusterAttributeValuePairStruct =
  sendMessage mtrScenesManagementClusterAttributeValuePairStruct valueUnsigned8Selector

-- | @- setValueUnsigned8:@
setValueUnsigned8 :: (IsMTRScenesManagementClusterAttributeValuePairStruct mtrScenesManagementClusterAttributeValuePairStruct, IsNSNumber value) => mtrScenesManagementClusterAttributeValuePairStruct -> value -> IO ()
setValueUnsigned8 mtrScenesManagementClusterAttributeValuePairStruct value =
  sendMessage mtrScenesManagementClusterAttributeValuePairStruct setValueUnsigned8Selector (toNSNumber value)

-- | @- valueSigned8@
valueSigned8 :: IsMTRScenesManagementClusterAttributeValuePairStruct mtrScenesManagementClusterAttributeValuePairStruct => mtrScenesManagementClusterAttributeValuePairStruct -> IO (Id NSNumber)
valueSigned8 mtrScenesManagementClusterAttributeValuePairStruct =
  sendMessage mtrScenesManagementClusterAttributeValuePairStruct valueSigned8Selector

-- | @- setValueSigned8:@
setValueSigned8 :: (IsMTRScenesManagementClusterAttributeValuePairStruct mtrScenesManagementClusterAttributeValuePairStruct, IsNSNumber value) => mtrScenesManagementClusterAttributeValuePairStruct -> value -> IO ()
setValueSigned8 mtrScenesManagementClusterAttributeValuePairStruct value =
  sendMessage mtrScenesManagementClusterAttributeValuePairStruct setValueSigned8Selector (toNSNumber value)

-- | @- valueUnsigned16@
valueUnsigned16 :: IsMTRScenesManagementClusterAttributeValuePairStruct mtrScenesManagementClusterAttributeValuePairStruct => mtrScenesManagementClusterAttributeValuePairStruct -> IO (Id NSNumber)
valueUnsigned16 mtrScenesManagementClusterAttributeValuePairStruct =
  sendMessage mtrScenesManagementClusterAttributeValuePairStruct valueUnsigned16Selector

-- | @- setValueUnsigned16:@
setValueUnsigned16 :: (IsMTRScenesManagementClusterAttributeValuePairStruct mtrScenesManagementClusterAttributeValuePairStruct, IsNSNumber value) => mtrScenesManagementClusterAttributeValuePairStruct -> value -> IO ()
setValueUnsigned16 mtrScenesManagementClusterAttributeValuePairStruct value =
  sendMessage mtrScenesManagementClusterAttributeValuePairStruct setValueUnsigned16Selector (toNSNumber value)

-- | @- valueSigned16@
valueSigned16 :: IsMTRScenesManagementClusterAttributeValuePairStruct mtrScenesManagementClusterAttributeValuePairStruct => mtrScenesManagementClusterAttributeValuePairStruct -> IO (Id NSNumber)
valueSigned16 mtrScenesManagementClusterAttributeValuePairStruct =
  sendMessage mtrScenesManagementClusterAttributeValuePairStruct valueSigned16Selector

-- | @- setValueSigned16:@
setValueSigned16 :: (IsMTRScenesManagementClusterAttributeValuePairStruct mtrScenesManagementClusterAttributeValuePairStruct, IsNSNumber value) => mtrScenesManagementClusterAttributeValuePairStruct -> value -> IO ()
setValueSigned16 mtrScenesManagementClusterAttributeValuePairStruct value =
  sendMessage mtrScenesManagementClusterAttributeValuePairStruct setValueSigned16Selector (toNSNumber value)

-- | @- valueUnsigned32@
valueUnsigned32 :: IsMTRScenesManagementClusterAttributeValuePairStruct mtrScenesManagementClusterAttributeValuePairStruct => mtrScenesManagementClusterAttributeValuePairStruct -> IO (Id NSNumber)
valueUnsigned32 mtrScenesManagementClusterAttributeValuePairStruct =
  sendMessage mtrScenesManagementClusterAttributeValuePairStruct valueUnsigned32Selector

-- | @- setValueUnsigned32:@
setValueUnsigned32 :: (IsMTRScenesManagementClusterAttributeValuePairStruct mtrScenesManagementClusterAttributeValuePairStruct, IsNSNumber value) => mtrScenesManagementClusterAttributeValuePairStruct -> value -> IO ()
setValueUnsigned32 mtrScenesManagementClusterAttributeValuePairStruct value =
  sendMessage mtrScenesManagementClusterAttributeValuePairStruct setValueUnsigned32Selector (toNSNumber value)

-- | @- valueSigned32@
valueSigned32 :: IsMTRScenesManagementClusterAttributeValuePairStruct mtrScenesManagementClusterAttributeValuePairStruct => mtrScenesManagementClusterAttributeValuePairStruct -> IO (Id NSNumber)
valueSigned32 mtrScenesManagementClusterAttributeValuePairStruct =
  sendMessage mtrScenesManagementClusterAttributeValuePairStruct valueSigned32Selector

-- | @- setValueSigned32:@
setValueSigned32 :: (IsMTRScenesManagementClusterAttributeValuePairStruct mtrScenesManagementClusterAttributeValuePairStruct, IsNSNumber value) => mtrScenesManagementClusterAttributeValuePairStruct -> value -> IO ()
setValueSigned32 mtrScenesManagementClusterAttributeValuePairStruct value =
  sendMessage mtrScenesManagementClusterAttributeValuePairStruct setValueSigned32Selector (toNSNumber value)

-- | @- valueUnsigned64@
valueUnsigned64 :: IsMTRScenesManagementClusterAttributeValuePairStruct mtrScenesManagementClusterAttributeValuePairStruct => mtrScenesManagementClusterAttributeValuePairStruct -> IO (Id NSNumber)
valueUnsigned64 mtrScenesManagementClusterAttributeValuePairStruct =
  sendMessage mtrScenesManagementClusterAttributeValuePairStruct valueUnsigned64Selector

-- | @- setValueUnsigned64:@
setValueUnsigned64 :: (IsMTRScenesManagementClusterAttributeValuePairStruct mtrScenesManagementClusterAttributeValuePairStruct, IsNSNumber value) => mtrScenesManagementClusterAttributeValuePairStruct -> value -> IO ()
setValueUnsigned64 mtrScenesManagementClusterAttributeValuePairStruct value =
  sendMessage mtrScenesManagementClusterAttributeValuePairStruct setValueUnsigned64Selector (toNSNumber value)

-- | @- valueSigned64@
valueSigned64 :: IsMTRScenesManagementClusterAttributeValuePairStruct mtrScenesManagementClusterAttributeValuePairStruct => mtrScenesManagementClusterAttributeValuePairStruct -> IO (Id NSNumber)
valueSigned64 mtrScenesManagementClusterAttributeValuePairStruct =
  sendMessage mtrScenesManagementClusterAttributeValuePairStruct valueSigned64Selector

-- | @- setValueSigned64:@
setValueSigned64 :: (IsMTRScenesManagementClusterAttributeValuePairStruct mtrScenesManagementClusterAttributeValuePairStruct, IsNSNumber value) => mtrScenesManagementClusterAttributeValuePairStruct -> value -> IO ()
setValueSigned64 mtrScenesManagementClusterAttributeValuePairStruct value =
  sendMessage mtrScenesManagementClusterAttributeValuePairStruct setValueSigned64Selector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @attributeID@
attributeIDSelector :: Selector '[] (Id NSNumber)
attributeIDSelector = mkSelector "attributeID"

-- | @Selector@ for @setAttributeID:@
setAttributeIDSelector :: Selector '[Id NSNumber] ()
setAttributeIDSelector = mkSelector "setAttributeID:"

-- | @Selector@ for @valueUnsigned8@
valueUnsigned8Selector :: Selector '[] (Id NSNumber)
valueUnsigned8Selector = mkSelector "valueUnsigned8"

-- | @Selector@ for @setValueUnsigned8:@
setValueUnsigned8Selector :: Selector '[Id NSNumber] ()
setValueUnsigned8Selector = mkSelector "setValueUnsigned8:"

-- | @Selector@ for @valueSigned8@
valueSigned8Selector :: Selector '[] (Id NSNumber)
valueSigned8Selector = mkSelector "valueSigned8"

-- | @Selector@ for @setValueSigned8:@
setValueSigned8Selector :: Selector '[Id NSNumber] ()
setValueSigned8Selector = mkSelector "setValueSigned8:"

-- | @Selector@ for @valueUnsigned16@
valueUnsigned16Selector :: Selector '[] (Id NSNumber)
valueUnsigned16Selector = mkSelector "valueUnsigned16"

-- | @Selector@ for @setValueUnsigned16:@
setValueUnsigned16Selector :: Selector '[Id NSNumber] ()
setValueUnsigned16Selector = mkSelector "setValueUnsigned16:"

-- | @Selector@ for @valueSigned16@
valueSigned16Selector :: Selector '[] (Id NSNumber)
valueSigned16Selector = mkSelector "valueSigned16"

-- | @Selector@ for @setValueSigned16:@
setValueSigned16Selector :: Selector '[Id NSNumber] ()
setValueSigned16Selector = mkSelector "setValueSigned16:"

-- | @Selector@ for @valueUnsigned32@
valueUnsigned32Selector :: Selector '[] (Id NSNumber)
valueUnsigned32Selector = mkSelector "valueUnsigned32"

-- | @Selector@ for @setValueUnsigned32:@
setValueUnsigned32Selector :: Selector '[Id NSNumber] ()
setValueUnsigned32Selector = mkSelector "setValueUnsigned32:"

-- | @Selector@ for @valueSigned32@
valueSigned32Selector :: Selector '[] (Id NSNumber)
valueSigned32Selector = mkSelector "valueSigned32"

-- | @Selector@ for @setValueSigned32:@
setValueSigned32Selector :: Selector '[Id NSNumber] ()
setValueSigned32Selector = mkSelector "setValueSigned32:"

-- | @Selector@ for @valueUnsigned64@
valueUnsigned64Selector :: Selector '[] (Id NSNumber)
valueUnsigned64Selector = mkSelector "valueUnsigned64"

-- | @Selector@ for @setValueUnsigned64:@
setValueUnsigned64Selector :: Selector '[Id NSNumber] ()
setValueUnsigned64Selector = mkSelector "setValueUnsigned64:"

-- | @Selector@ for @valueSigned64@
valueSigned64Selector :: Selector '[] (Id NSNumber)
valueSigned64Selector = mkSelector "valueSigned64"

-- | @Selector@ for @setValueSigned64:@
setValueSigned64Selector :: Selector '[Id NSNumber] ()
setValueSigned64Selector = mkSelector "setValueSigned64:"

