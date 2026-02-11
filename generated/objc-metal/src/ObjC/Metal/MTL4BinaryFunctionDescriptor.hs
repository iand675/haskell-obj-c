{-# LANGUAGE PatternSynonyms #-}
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
  , nameSelector
  , setNameSelector
  , functionDescriptorSelector
  , setFunctionDescriptorSelector
  , optionsSelector
  , setOptionsSelector

  -- * Enum types
  , MTL4BinaryFunctionOptions(MTL4BinaryFunctionOptions)
  , pattern MTL4BinaryFunctionOptionNone
  , pattern MTL4BinaryFunctionOptionPipelineIndependent

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
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Associates a string that uniquely identifies a binary function.
--
-- You can use this property to look up a corresponding binary function by name in a ``MTL4Archive`` instance.
--
-- ObjC selector: @- name@
name :: IsMTL4BinaryFunctionDescriptor mtL4BinaryFunctionDescriptor => mtL4BinaryFunctionDescriptor -> IO (Id NSString)
name mtL4BinaryFunctionDescriptor  =
  sendMsg mtL4BinaryFunctionDescriptor (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Associates a string that uniquely identifies a binary function.
--
-- You can use this property to look up a corresponding binary function by name in a ``MTL4Archive`` instance.
--
-- ObjC selector: @- setName:@
setName :: (IsMTL4BinaryFunctionDescriptor mtL4BinaryFunctionDescriptor, IsNSString value) => mtL4BinaryFunctionDescriptor -> value -> IO ()
setName mtL4BinaryFunctionDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtL4BinaryFunctionDescriptor (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Provides the function descriptor corresponding to the function to compile into a binary function.
--
-- ObjC selector: @- functionDescriptor@
functionDescriptor :: IsMTL4BinaryFunctionDescriptor mtL4BinaryFunctionDescriptor => mtL4BinaryFunctionDescriptor -> IO (Id MTL4FunctionDescriptor)
functionDescriptor mtL4BinaryFunctionDescriptor  =
  sendMsg mtL4BinaryFunctionDescriptor (mkSelector "functionDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Provides the function descriptor corresponding to the function to compile into a binary function.
--
-- ObjC selector: @- setFunctionDescriptor:@
setFunctionDescriptor :: (IsMTL4BinaryFunctionDescriptor mtL4BinaryFunctionDescriptor, IsMTL4FunctionDescriptor value) => mtL4BinaryFunctionDescriptor -> value -> IO ()
setFunctionDescriptor mtL4BinaryFunctionDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtL4BinaryFunctionDescriptor (mkSelector "setFunctionDescriptor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Configure the options to use at binary function creation time.
--
-- ObjC selector: @- options@
options :: IsMTL4BinaryFunctionDescriptor mtL4BinaryFunctionDescriptor => mtL4BinaryFunctionDescriptor -> IO MTL4BinaryFunctionOptions
options mtL4BinaryFunctionDescriptor  =
  fmap (coerce :: CULong -> MTL4BinaryFunctionOptions) $ sendMsg mtL4BinaryFunctionDescriptor (mkSelector "options") retCULong []

-- | Configure the options to use at binary function creation time.
--
-- ObjC selector: @- setOptions:@
setOptions :: IsMTL4BinaryFunctionDescriptor mtL4BinaryFunctionDescriptor => mtL4BinaryFunctionDescriptor -> MTL4BinaryFunctionOptions -> IO ()
setOptions mtL4BinaryFunctionDescriptor  value =
  sendMsg mtL4BinaryFunctionDescriptor (mkSelector "setOptions:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @functionDescriptor@
functionDescriptorSelector :: Selector
functionDescriptorSelector = mkSelector "functionDescriptor"

-- | @Selector@ for @setFunctionDescriptor:@
setFunctionDescriptorSelector :: Selector
setFunctionDescriptorSelector = mkSelector "setFunctionDescriptor:"

-- | @Selector@ for @options@
optionsSelector :: Selector
optionsSelector = mkSelector "options"

-- | @Selector@ for @setOptions:@
setOptionsSelector :: Selector
setOptionsSelector = mkSelector "setOptions:"

