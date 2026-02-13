{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , binaryArchivesSelector
  , constantValuesSelector
  , functionDescriptorSelector
  , nameSelector
  , optionsSelector
  , setBinaryArchivesSelector
  , setConstantValuesSelector
  , setNameSelector
  , setOptionsSelector
  , setSpecializedNameSelector
  , specializedNameSelector

  -- * Enum types
  , MTLFunctionOptions(MTLFunctionOptions)
  , pattern MTLFunctionOptionNone
  , pattern MTLFunctionOptionCompileToBinary
  , pattern MTLFunctionOptionStoreFunctionInMetalPipelinesScript
  , pattern MTLFunctionOptionStoreFunctionInMetalScript
  , pattern MTLFunctionOptionFailOnBinaryArchiveMiss
  , pattern MTLFunctionOptionPipelineIndependent

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

-- | functionDescriptor
--
-- Create an autoreleased function descriptor
--
-- ObjC selector: @+ functionDescriptor@
functionDescriptor :: IO (Id MTLFunctionDescriptor)
functionDescriptor  =
  do
    cls' <- getRequiredClass "MTLFunctionDescriptor"
    sendClassMessage cls' functionDescriptorSelector

-- | name
--
-- The name of the @visible@ function to find.
--
-- ObjC selector: @- name@
name :: IsMTLFunctionDescriptor mtlFunctionDescriptor => mtlFunctionDescriptor -> IO (Id NSString)
name mtlFunctionDescriptor =
  sendMessage mtlFunctionDescriptor nameSelector

-- | name
--
-- The name of the @visible@ function to find.
--
-- ObjC selector: @- setName:@
setName :: (IsMTLFunctionDescriptor mtlFunctionDescriptor, IsNSString value) => mtlFunctionDescriptor -> value -> IO ()
setName mtlFunctionDescriptor value =
  sendMessage mtlFunctionDescriptor setNameSelector (toNSString value)

-- | specializedName
--
-- An optional new name for a @visible@ function to allow reuse with different specializations.
--
-- ObjC selector: @- specializedName@
specializedName :: IsMTLFunctionDescriptor mtlFunctionDescriptor => mtlFunctionDescriptor -> IO (Id NSString)
specializedName mtlFunctionDescriptor =
  sendMessage mtlFunctionDescriptor specializedNameSelector

-- | specializedName
--
-- An optional new name for a @visible@ function to allow reuse with different specializations.
--
-- ObjC selector: @- setSpecializedName:@
setSpecializedName :: (IsMTLFunctionDescriptor mtlFunctionDescriptor, IsNSString value) => mtlFunctionDescriptor -> value -> IO ()
setSpecializedName mtlFunctionDescriptor value =
  sendMessage mtlFunctionDescriptor setSpecializedNameSelector (toNSString value)

-- | constantValues
--
-- The set of constant values assigned to the function constants. Compilation fails if you do not provide valid constant values for all required function constants.
--
-- ObjC selector: @- constantValues@
constantValues :: IsMTLFunctionDescriptor mtlFunctionDescriptor => mtlFunctionDescriptor -> IO (Id MTLFunctionConstantValues)
constantValues mtlFunctionDescriptor =
  sendMessage mtlFunctionDescriptor constantValuesSelector

-- | constantValues
--
-- The set of constant values assigned to the function constants. Compilation fails if you do not provide valid constant values for all required function constants.
--
-- ObjC selector: @- setConstantValues:@
setConstantValues :: (IsMTLFunctionDescriptor mtlFunctionDescriptor, IsMTLFunctionConstantValues value) => mtlFunctionDescriptor -> value -> IO ()
setConstantValues mtlFunctionDescriptor value =
  sendMessage mtlFunctionDescriptor setConstantValuesSelector (toMTLFunctionConstantValues value)

-- | options
--
-- The options to use for this new @MTLFunction@.
--
-- ObjC selector: @- options@
options :: IsMTLFunctionDescriptor mtlFunctionDescriptor => mtlFunctionDescriptor -> IO MTLFunctionOptions
options mtlFunctionDescriptor =
  sendMessage mtlFunctionDescriptor optionsSelector

-- | options
--
-- The options to use for this new @MTLFunction@.
--
-- ObjC selector: @- setOptions:@
setOptions :: IsMTLFunctionDescriptor mtlFunctionDescriptor => mtlFunctionDescriptor -> MTLFunctionOptions -> IO ()
setOptions mtlFunctionDescriptor value =
  sendMessage mtlFunctionDescriptor setOptionsSelector value

-- | binaryArchives
--
-- The array of archives to be searched.
--
-- Binary archives to be searched for precompiled functions during the compilation of this function.
--
-- ObjC selector: @- binaryArchives@
binaryArchives :: IsMTLFunctionDescriptor mtlFunctionDescriptor => mtlFunctionDescriptor -> IO (Id NSArray)
binaryArchives mtlFunctionDescriptor =
  sendMessage mtlFunctionDescriptor binaryArchivesSelector

-- | binaryArchives
--
-- The array of archives to be searched.
--
-- Binary archives to be searched for precompiled functions during the compilation of this function.
--
-- ObjC selector: @- setBinaryArchives:@
setBinaryArchives :: (IsMTLFunctionDescriptor mtlFunctionDescriptor, IsNSArray value) => mtlFunctionDescriptor -> value -> IO ()
setBinaryArchives mtlFunctionDescriptor value =
  sendMessage mtlFunctionDescriptor setBinaryArchivesSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @functionDescriptor@
functionDescriptorSelector :: Selector '[] (Id MTLFunctionDescriptor)
functionDescriptorSelector = mkSelector "functionDescriptor"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

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

-- | @Selector@ for @options@
optionsSelector :: Selector '[] MTLFunctionOptions
optionsSelector = mkSelector "options"

-- | @Selector@ for @setOptions:@
setOptionsSelector :: Selector '[MTLFunctionOptions] ()
setOptionsSelector = mkSelector "setOptions:"

-- | @Selector@ for @binaryArchives@
binaryArchivesSelector :: Selector '[] (Id NSArray)
binaryArchivesSelector = mkSelector "binaryArchives"

-- | @Selector@ for @setBinaryArchives:@
setBinaryArchivesSelector :: Selector '[Id NSArray] ()
setBinaryArchivesSelector = mkSelector "setBinaryArchives:"

