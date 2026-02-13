{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Base interface for other function-derived interfaces.
--
-- Generated bindings for @MTL4BinaryFunctionDescriptor@.
module ObjC.Metal.MTL4BinaryFunctionDescriptor
  ( MTL4BinaryFunctionDescriptor
  , IsMTL4BinaryFunctionDescriptor(..)
  , name
  , setName
  , functionDescriptor
  , setFunctionDescriptor
  , options
  , setOptions
  , functionDescriptorSelector
  , nameSelector
  , optionsSelector
  , setFunctionDescriptorSelector
  , setNameSelector
  , setOptionsSelector

  -- * Enum types
  , MTL4BinaryFunctionOptions(MTL4BinaryFunctionOptions)
  , pattern MTL4BinaryFunctionOptionNone
  , pattern MTL4BinaryFunctionOptionPipelineIndependent

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Associates a string that uniquely identifies a binary function.
--
-- You can use this property to look up a corresponding binary function by name in a ``MTL4Archive`` instance.
--
-- ObjC selector: @- name@
name :: IsMTL4BinaryFunctionDescriptor mtL4BinaryFunctionDescriptor => mtL4BinaryFunctionDescriptor -> IO (Id NSString)
name mtL4BinaryFunctionDescriptor =
  sendMessage mtL4BinaryFunctionDescriptor nameSelector

-- | Associates a string that uniquely identifies a binary function.
--
-- You can use this property to look up a corresponding binary function by name in a ``MTL4Archive`` instance.
--
-- ObjC selector: @- setName:@
setName :: (IsMTL4BinaryFunctionDescriptor mtL4BinaryFunctionDescriptor, IsNSString value) => mtL4BinaryFunctionDescriptor -> value -> IO ()
setName mtL4BinaryFunctionDescriptor value =
  sendMessage mtL4BinaryFunctionDescriptor setNameSelector (toNSString value)

-- | Provides the function descriptor corresponding to the function to compile into a binary function.
--
-- ObjC selector: @- functionDescriptor@
functionDescriptor :: IsMTL4BinaryFunctionDescriptor mtL4BinaryFunctionDescriptor => mtL4BinaryFunctionDescriptor -> IO (Id MTL4FunctionDescriptor)
functionDescriptor mtL4BinaryFunctionDescriptor =
  sendMessage mtL4BinaryFunctionDescriptor functionDescriptorSelector

-- | Provides the function descriptor corresponding to the function to compile into a binary function.
--
-- ObjC selector: @- setFunctionDescriptor:@
setFunctionDescriptor :: (IsMTL4BinaryFunctionDescriptor mtL4BinaryFunctionDescriptor, IsMTL4FunctionDescriptor value) => mtL4BinaryFunctionDescriptor -> value -> IO ()
setFunctionDescriptor mtL4BinaryFunctionDescriptor value =
  sendMessage mtL4BinaryFunctionDescriptor setFunctionDescriptorSelector (toMTL4FunctionDescriptor value)

-- | Configure the options to use at binary function creation time.
--
-- ObjC selector: @- options@
options :: IsMTL4BinaryFunctionDescriptor mtL4BinaryFunctionDescriptor => mtL4BinaryFunctionDescriptor -> IO MTL4BinaryFunctionOptions
options mtL4BinaryFunctionDescriptor =
  sendMessage mtL4BinaryFunctionDescriptor optionsSelector

-- | Configure the options to use at binary function creation time.
--
-- ObjC selector: @- setOptions:@
setOptions :: IsMTL4BinaryFunctionDescriptor mtL4BinaryFunctionDescriptor => mtL4BinaryFunctionDescriptor -> MTL4BinaryFunctionOptions -> IO ()
setOptions mtL4BinaryFunctionDescriptor value =
  sendMessage mtL4BinaryFunctionDescriptor setOptionsSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @functionDescriptor@
functionDescriptorSelector :: Selector '[] (Id MTL4FunctionDescriptor)
functionDescriptorSelector = mkSelector "functionDescriptor"

-- | @Selector@ for @setFunctionDescriptor:@
setFunctionDescriptorSelector :: Selector '[Id MTL4FunctionDescriptor] ()
setFunctionDescriptorSelector = mkSelector "setFunctionDescriptor:"

-- | @Selector@ for @options@
optionsSelector :: Selector '[] MTL4BinaryFunctionOptions
optionsSelector = mkSelector "options"

-- | @Selector@ for @setOptions:@
setOptionsSelector :: Selector '[MTL4BinaryFunctionOptions] ()
setOptionsSelector = mkSelector "setOptions:"

