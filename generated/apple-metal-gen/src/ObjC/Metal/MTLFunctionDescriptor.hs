{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLFunctionDescriptor@.
module ObjC.Metal.MTLFunctionDescriptor
  ( MTLFunctionDescriptor
  , IsMTLFunctionDescriptor(..)
  , functionDescriptor
  , name
  , setName
  , specializedName
  , setSpecializedName
  , constantValues
  , setConstantValues
  , options
  , setOptions
  , binaryArchives
  , setBinaryArchives
  , functionDescriptorSelector
  , nameSelector
  , setNameSelector
  , specializedNameSelector
  , setSpecializedNameSelector
  , constantValuesSelector
  , setConstantValuesSelector
  , optionsSelector
  , setOptionsSelector
  , binaryArchivesSelector
  , setBinaryArchivesSelector

  -- * Enum types
  , MTLFunctionOptions(MTLFunctionOptions)
  , pattern MTLFunctionOptionNone
  , pattern MTLFunctionOptionCompileToBinary
  , pattern MTLFunctionOptionStoreFunctionInMetalPipelinesScript
  , pattern MTLFunctionOptionStoreFunctionInMetalScript
  , pattern MTLFunctionOptionFailOnBinaryArchiveMiss
  , pattern MTLFunctionOptionPipelineIndependent

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

-- | functionDescriptor
--
-- Create an autoreleased function descriptor
--
-- ObjC selector: @+ functionDescriptor@
functionDescriptor :: IO (Id MTLFunctionDescriptor)
functionDescriptor  =
  do
    cls' <- getRequiredClass "MTLFunctionDescriptor"
    sendClassMsg cls' (mkSelector "functionDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | name
--
-- The name of the @visible@ function to find.
--
-- ObjC selector: @- name@
name :: IsMTLFunctionDescriptor mtlFunctionDescriptor => mtlFunctionDescriptor -> IO (Id NSString)
name mtlFunctionDescriptor  =
    sendMsg mtlFunctionDescriptor (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | name
--
-- The name of the @visible@ function to find.
--
-- ObjC selector: @- setName:@
setName :: (IsMTLFunctionDescriptor mtlFunctionDescriptor, IsNSString value) => mtlFunctionDescriptor -> value -> IO ()
setName mtlFunctionDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtlFunctionDescriptor (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | specializedName
--
-- An optional new name for a @visible@ function to allow reuse with different specializations.
--
-- ObjC selector: @- specializedName@
specializedName :: IsMTLFunctionDescriptor mtlFunctionDescriptor => mtlFunctionDescriptor -> IO (Id NSString)
specializedName mtlFunctionDescriptor  =
    sendMsg mtlFunctionDescriptor (mkSelector "specializedName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | specializedName
--
-- An optional new name for a @visible@ function to allow reuse with different specializations.
--
-- ObjC selector: @- setSpecializedName:@
setSpecializedName :: (IsMTLFunctionDescriptor mtlFunctionDescriptor, IsNSString value) => mtlFunctionDescriptor -> value -> IO ()
setSpecializedName mtlFunctionDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtlFunctionDescriptor (mkSelector "setSpecializedName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | constantValues
--
-- The set of constant values assigned to the function constants. Compilation fails if you do not provide valid constant values for all required function constants.
--
-- ObjC selector: @- constantValues@
constantValues :: IsMTLFunctionDescriptor mtlFunctionDescriptor => mtlFunctionDescriptor -> IO (Id MTLFunctionConstantValues)
constantValues mtlFunctionDescriptor  =
    sendMsg mtlFunctionDescriptor (mkSelector "constantValues") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | constantValues
--
-- The set of constant values assigned to the function constants. Compilation fails if you do not provide valid constant values for all required function constants.
--
-- ObjC selector: @- setConstantValues:@
setConstantValues :: (IsMTLFunctionDescriptor mtlFunctionDescriptor, IsMTLFunctionConstantValues value) => mtlFunctionDescriptor -> value -> IO ()
setConstantValues mtlFunctionDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtlFunctionDescriptor (mkSelector "setConstantValues:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | options
--
-- The options to use for this new @MTLFunction@.
--
-- ObjC selector: @- options@
options :: IsMTLFunctionDescriptor mtlFunctionDescriptor => mtlFunctionDescriptor -> IO MTLFunctionOptions
options mtlFunctionDescriptor  =
    fmap (coerce :: CULong -> MTLFunctionOptions) $ sendMsg mtlFunctionDescriptor (mkSelector "options") retCULong []

-- | options
--
-- The options to use for this new @MTLFunction@.
--
-- ObjC selector: @- setOptions:@
setOptions :: IsMTLFunctionDescriptor mtlFunctionDescriptor => mtlFunctionDescriptor -> MTLFunctionOptions -> IO ()
setOptions mtlFunctionDescriptor  value =
    sendMsg mtlFunctionDescriptor (mkSelector "setOptions:") retVoid [argCULong (coerce value)]

-- | binaryArchives
--
-- The array of archives to be searched.
--
-- Binary archives to be searched for precompiled functions during the compilation of this function.
--
-- ObjC selector: @- binaryArchives@
binaryArchives :: IsMTLFunctionDescriptor mtlFunctionDescriptor => mtlFunctionDescriptor -> IO (Id NSArray)
binaryArchives mtlFunctionDescriptor  =
    sendMsg mtlFunctionDescriptor (mkSelector "binaryArchives") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | binaryArchives
--
-- The array of archives to be searched.
--
-- Binary archives to be searched for precompiled functions during the compilation of this function.
--
-- ObjC selector: @- setBinaryArchives:@
setBinaryArchives :: (IsMTLFunctionDescriptor mtlFunctionDescriptor, IsNSArray value) => mtlFunctionDescriptor -> value -> IO ()
setBinaryArchives mtlFunctionDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtlFunctionDescriptor (mkSelector "setBinaryArchives:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @functionDescriptor@
functionDescriptorSelector :: Selector
functionDescriptorSelector = mkSelector "functionDescriptor"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

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

-- | @Selector@ for @options@
optionsSelector :: Selector
optionsSelector = mkSelector "options"

-- | @Selector@ for @setOptions:@
setOptionsSelector :: Selector
setOptionsSelector = mkSelector "setOptions:"

-- | @Selector@ for @binaryArchives@
binaryArchivesSelector :: Selector
binaryArchivesSelector = mkSelector "binaryArchives"

-- | @Selector@ for @setBinaryArchives:@
setBinaryArchivesSelector :: Selector
setBinaryArchivesSelector = mkSelector "setBinaryArchives:"

