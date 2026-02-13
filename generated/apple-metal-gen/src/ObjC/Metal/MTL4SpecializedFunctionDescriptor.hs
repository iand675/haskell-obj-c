{-# LANGUAGE DataKinds #-}
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
  , constantValuesSelector
  , functionDescriptorSelector
  , setConstantValuesSelector
  , setFunctionDescriptorSelector
  , setSpecializedNameSelector
  , specializedNameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Provides a descriptor that corresponds to a base function that the specialization applies to.
--
-- ObjC selector: @- functionDescriptor@
functionDescriptor :: IsMTL4SpecializedFunctionDescriptor mtL4SpecializedFunctionDescriptor => mtL4SpecializedFunctionDescriptor -> IO (Id MTL4FunctionDescriptor)
functionDescriptor mtL4SpecializedFunctionDescriptor =
  sendMessage mtL4SpecializedFunctionDescriptor functionDescriptorSelector

-- | Provides a descriptor that corresponds to a base function that the specialization applies to.
--
-- ObjC selector: @- setFunctionDescriptor:@
setFunctionDescriptor :: (IsMTL4SpecializedFunctionDescriptor mtL4SpecializedFunctionDescriptor, IsMTL4FunctionDescriptor value) => mtL4SpecializedFunctionDescriptor -> value -> IO ()
setFunctionDescriptor mtL4SpecializedFunctionDescriptor value =
  sendMessage mtL4SpecializedFunctionDescriptor setFunctionDescriptorSelector (toMTL4FunctionDescriptor value)

-- | Assigns an optional name to the specialized function.
--
-- ObjC selector: @- specializedName@
specializedName :: IsMTL4SpecializedFunctionDescriptor mtL4SpecializedFunctionDescriptor => mtL4SpecializedFunctionDescriptor -> IO (Id NSString)
specializedName mtL4SpecializedFunctionDescriptor =
  sendMessage mtL4SpecializedFunctionDescriptor specializedNameSelector

-- | Assigns an optional name to the specialized function.
--
-- ObjC selector: @- setSpecializedName:@
setSpecializedName :: (IsMTL4SpecializedFunctionDescriptor mtL4SpecializedFunctionDescriptor, IsNSString value) => mtL4SpecializedFunctionDescriptor -> value -> IO ()
setSpecializedName mtL4SpecializedFunctionDescriptor value =
  sendMessage mtL4SpecializedFunctionDescriptor setSpecializedNameSelector (toNSString value)

-- | Configures optional function constant values to associate with the function.
--
-- ObjC selector: @- constantValues@
constantValues :: IsMTL4SpecializedFunctionDescriptor mtL4SpecializedFunctionDescriptor => mtL4SpecializedFunctionDescriptor -> IO (Id MTLFunctionConstantValues)
constantValues mtL4SpecializedFunctionDescriptor =
  sendMessage mtL4SpecializedFunctionDescriptor constantValuesSelector

-- | Configures optional function constant values to associate with the function.
--
-- ObjC selector: @- setConstantValues:@
setConstantValues :: (IsMTL4SpecializedFunctionDescriptor mtL4SpecializedFunctionDescriptor, IsMTLFunctionConstantValues value) => mtL4SpecializedFunctionDescriptor -> value -> IO ()
setConstantValues mtL4SpecializedFunctionDescriptor value =
  sendMessage mtL4SpecializedFunctionDescriptor setConstantValuesSelector (toMTLFunctionConstantValues value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @functionDescriptor@
functionDescriptorSelector :: Selector '[] (Id MTL4FunctionDescriptor)
functionDescriptorSelector = mkSelector "functionDescriptor"

-- | @Selector@ for @setFunctionDescriptor:@
setFunctionDescriptorSelector :: Selector '[Id MTL4FunctionDescriptor] ()
setFunctionDescriptorSelector = mkSelector "setFunctionDescriptor:"

-- | @Selector@ for @specializedName@
specializedNameSelector :: Selector '[] (Id NSString)
specializedNameSelector = mkSelector "specializedName"

-- | @Selector@ for @setSpecializedName:@
setSpecializedNameSelector :: Selector '[Id NSString] ()
setSpecializedNameSelector = mkSelector "setSpecializedName:"

-- | @Selector@ for @constantValues@
constantValuesSelector :: Selector '[] (Id MTLFunctionConstantValues)
constantValuesSelector = mkSelector "constantValues"

-- | @Selector@ for @setConstantValues:@
setConstantValuesSelector :: Selector '[Id MTLFunctionConstantValues] ()
setConstantValuesSelector = mkSelector "setConstantValues:"

