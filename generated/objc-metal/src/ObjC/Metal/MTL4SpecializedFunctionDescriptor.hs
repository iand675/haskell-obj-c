{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Groups together properties to configure and create a specialized function by passing it to a factory method.
--
-- You can pass an instance of this class to any methods that accept a ``MTL4FunctionDescriptor`` parameter to provide extra configuration, such as function constants or a name.
--
-- Generated bindings for @MTL4SpecializedFunctionDescriptor@.
module ObjC.Metal.MTL4SpecializedFunctionDescriptor
  ( MTL4SpecializedFunctionDescriptor
  , IsMTL4SpecializedFunctionDescriptor(..)
  , functionDescriptor
  , setFunctionDescriptor
  , specializedName
  , setSpecializedName
  , constantValues
  , setConstantValues
  , functionDescriptorSelector
  , setFunctionDescriptorSelector
  , specializedNameSelector
  , setSpecializedNameSelector
  , constantValuesSelector
  , setConstantValuesSelector


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

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Provides a descriptor that corresponds to a base function that the specialization applies to.
--
-- ObjC selector: @- functionDescriptor@
functionDescriptor :: IsMTL4SpecializedFunctionDescriptor mtL4SpecializedFunctionDescriptor => mtL4SpecializedFunctionDescriptor -> IO (Id MTL4FunctionDescriptor)
functionDescriptor mtL4SpecializedFunctionDescriptor  =
  sendMsg mtL4SpecializedFunctionDescriptor (mkSelector "functionDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Provides a descriptor that corresponds to a base function that the specialization applies to.
--
-- ObjC selector: @- setFunctionDescriptor:@
setFunctionDescriptor :: (IsMTL4SpecializedFunctionDescriptor mtL4SpecializedFunctionDescriptor, IsMTL4FunctionDescriptor value) => mtL4SpecializedFunctionDescriptor -> value -> IO ()
setFunctionDescriptor mtL4SpecializedFunctionDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtL4SpecializedFunctionDescriptor (mkSelector "setFunctionDescriptor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Assigns an optional name to the specialized function.
--
-- ObjC selector: @- specializedName@
specializedName :: IsMTL4SpecializedFunctionDescriptor mtL4SpecializedFunctionDescriptor => mtL4SpecializedFunctionDescriptor -> IO (Id NSString)
specializedName mtL4SpecializedFunctionDescriptor  =
  sendMsg mtL4SpecializedFunctionDescriptor (mkSelector "specializedName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Assigns an optional name to the specialized function.
--
-- ObjC selector: @- setSpecializedName:@
setSpecializedName :: (IsMTL4SpecializedFunctionDescriptor mtL4SpecializedFunctionDescriptor, IsNSString value) => mtL4SpecializedFunctionDescriptor -> value -> IO ()
setSpecializedName mtL4SpecializedFunctionDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtL4SpecializedFunctionDescriptor (mkSelector "setSpecializedName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Configures optional function constant values to associate with the function.
--
-- ObjC selector: @- constantValues@
constantValues :: IsMTL4SpecializedFunctionDescriptor mtL4SpecializedFunctionDescriptor => mtL4SpecializedFunctionDescriptor -> IO (Id MTLFunctionConstantValues)
constantValues mtL4SpecializedFunctionDescriptor  =
  sendMsg mtL4SpecializedFunctionDescriptor (mkSelector "constantValues") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Configures optional function constant values to associate with the function.
--
-- ObjC selector: @- setConstantValues:@
setConstantValues :: (IsMTL4SpecializedFunctionDescriptor mtL4SpecializedFunctionDescriptor, IsMTLFunctionConstantValues value) => mtL4SpecializedFunctionDescriptor -> value -> IO ()
setConstantValues mtL4SpecializedFunctionDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtL4SpecializedFunctionDescriptor (mkSelector "setConstantValues:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @functionDescriptor@
functionDescriptorSelector :: Selector
functionDescriptorSelector = mkSelector "functionDescriptor"

-- | @Selector@ for @setFunctionDescriptor:@
setFunctionDescriptorSelector :: Selector
setFunctionDescriptorSelector = mkSelector "setFunctionDescriptor:"

-- | @Selector@ for @specializedName@
specializedNameSelector :: Selector
specializedNameSelector = mkSelector "specializedName"

-- | @Selector@ for @setSpecializedName:@
setSpecializedNameSelector :: Selector
setSpecializedNameSelector = mkSelector "setSpecializedName:"

-- | @Selector@ for @constantValues@
constantValuesSelector :: Selector
constantValuesSelector = mkSelector "constantValues"

-- | @Selector@ for @setConstantValues:@
setConstantValuesSelector :: Selector
setConstantValuesSelector = mkSelector "setConstantValues:"

