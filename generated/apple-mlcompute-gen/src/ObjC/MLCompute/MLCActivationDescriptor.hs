{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCActivationDescriptor
--
-- The MLCActivationDescriptor specifies a neuron descriptor.
--
-- Generated bindings for @MLCActivationDescriptor@.
module ObjC.MLCompute.MLCActivationDescriptor
  ( MLCActivationDescriptor
  , IsMLCActivationDescriptor(..)
  , new
  , init_
  , descriptorWithType
  , descriptorWithType_a
  , descriptorWithType_a_b
  , descriptorWithType_a_b_c
  , activationType
  , a
  , b
  , c
  , aSelector
  , activationTypeSelector
  , bSelector
  , cSelector
  , descriptorWithTypeSelector
  , descriptorWithType_aSelector
  , descriptorWithType_a_bSelector
  , descriptorWithType_a_b_cSelector
  , initSelector
  , newSelector

  -- * Enum types
  , MLCActivationType(MLCActivationType)
  , pattern MLCActivationTypeNone
  , pattern MLCActivationTypeReLU
  , pattern MLCActivationTypeLinear
  , pattern MLCActivationTypeSigmoid
  , pattern MLCActivationTypeHardSigmoid
  , pattern MLCActivationTypeTanh
  , pattern MLCActivationTypeAbsolute
  , pattern MLCActivationTypeSoftPlus
  , pattern MLCActivationTypeSoftSign
  , pattern MLCActivationTypeELU
  , pattern MLCActivationTypeReLUN
  , pattern MLCActivationTypeLogSigmoid
  , pattern MLCActivationTypeSELU
  , pattern MLCActivationTypeCELU
  , pattern MLCActivationTypeHardShrink
  , pattern MLCActivationTypeSoftShrink
  , pattern MLCActivationTypeTanhShrink
  , pattern MLCActivationTypeThreshold
  , pattern MLCActivationTypeGELU
  , pattern MLCActivationTypeHardSwish
  , pattern MLCActivationTypeClamp
  , pattern MLCActivationTypeCount

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MLCompute.Internal.Classes
import ObjC.MLCompute.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MLCActivationDescriptor)
new  =
  do
    cls' <- getRequiredClass "MLCActivationDescriptor"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsMLCActivationDescriptor mlcActivationDescriptor => mlcActivationDescriptor -> IO (Id MLCActivationDescriptor)
init_ mlcActivationDescriptor =
  sendOwnedMessage mlcActivationDescriptor initSelector

-- | Create a MLCActivationDescriptor object
--
-- @activationType@ — A type of activation function.
--
-- Returns: A new neuron descriptor or nil if failure
--
-- ObjC selector: @+ descriptorWithType:@
descriptorWithType :: MLCActivationType -> IO (Id MLCActivationDescriptor)
descriptorWithType activationType =
  do
    cls' <- getRequiredClass "MLCActivationDescriptor"
    sendClassMessage cls' descriptorWithTypeSelector activationType

-- | Create a MLCActivationDescriptor object
--
-- @activationType@ — A type of activation function.
--
-- @a@ — Parameter "a".
--
-- Returns: A new neuron descriptor or nil if failure
--
-- ObjC selector: @+ descriptorWithType:a:@
descriptorWithType_a :: MLCActivationType -> CFloat -> IO (Id MLCActivationDescriptor)
descriptorWithType_a activationType a =
  do
    cls' <- getRequiredClass "MLCActivationDescriptor"
    sendClassMessage cls' descriptorWithType_aSelector activationType a

-- | Create a MLCActivationDescriptor object
--
-- @activationType@ — A type of activation function.
--
-- @a@ — Parameter "a".
--
-- @b@ — Parameter "b".
--
-- Returns: A new neuron descriptor or nil if failure
--
-- ObjC selector: @+ descriptorWithType:a:b:@
descriptorWithType_a_b :: MLCActivationType -> CFloat -> CFloat -> IO (Id MLCActivationDescriptor)
descriptorWithType_a_b activationType a b =
  do
    cls' <- getRequiredClass "MLCActivationDescriptor"
    sendClassMessage cls' descriptorWithType_a_bSelector activationType a b

-- | Create a MLCActivationDescriptor object
--
-- @activationType@ — A type of activation function.
--
-- @a@ — Parameter "a".
--
-- @b@ — Parameter "b".
--
-- @c@ — Parameter "c".
--
-- Returns: A new neuron descriptor or nil if failure
--
-- ObjC selector: @+ descriptorWithType:a:b:c:@
descriptorWithType_a_b_c :: MLCActivationType -> CFloat -> CFloat -> CFloat -> IO (Id MLCActivationDescriptor)
descriptorWithType_a_b_c activationType a b c =
  do
    cls' <- getRequiredClass "MLCActivationDescriptor"
    sendClassMessage cls' descriptorWithType_a_b_cSelector activationType a b c

-- | activationType
--
-- The type of activation function
--
-- ObjC selector: @- activationType@
activationType :: IsMLCActivationDescriptor mlcActivationDescriptor => mlcActivationDescriptor -> IO MLCActivationType
activationType mlcActivationDescriptor =
  sendMessage mlcActivationDescriptor activationTypeSelector

-- | a
--
-- Parameter to the activation function
--
-- ObjC selector: @- a@
a :: IsMLCActivationDescriptor mlcActivationDescriptor => mlcActivationDescriptor -> IO CFloat
a mlcActivationDescriptor =
  sendMessage mlcActivationDescriptor aSelector

-- | b
--
-- Parameter to the activation function
--
-- ObjC selector: @- b@
b :: IsMLCActivationDescriptor mlcActivationDescriptor => mlcActivationDescriptor -> IO CFloat
b mlcActivationDescriptor =
  sendMessage mlcActivationDescriptor bSelector

-- | c
--
-- Parameter to the activation function
--
-- ObjC selector: @- c@
c :: IsMLCActivationDescriptor mlcActivationDescriptor => mlcActivationDescriptor -> IO CFloat
c mlcActivationDescriptor =
  sendMessage mlcActivationDescriptor cSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MLCActivationDescriptor)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MLCActivationDescriptor)
initSelector = mkSelector "init"

-- | @Selector@ for @descriptorWithType:@
descriptorWithTypeSelector :: Selector '[MLCActivationType] (Id MLCActivationDescriptor)
descriptorWithTypeSelector = mkSelector "descriptorWithType:"

-- | @Selector@ for @descriptorWithType:a:@
descriptorWithType_aSelector :: Selector '[MLCActivationType, CFloat] (Id MLCActivationDescriptor)
descriptorWithType_aSelector = mkSelector "descriptorWithType:a:"

-- | @Selector@ for @descriptorWithType:a:b:@
descriptorWithType_a_bSelector :: Selector '[MLCActivationType, CFloat, CFloat] (Id MLCActivationDescriptor)
descriptorWithType_a_bSelector = mkSelector "descriptorWithType:a:b:"

-- | @Selector@ for @descriptorWithType:a:b:c:@
descriptorWithType_a_b_cSelector :: Selector '[MLCActivationType, CFloat, CFloat, CFloat] (Id MLCActivationDescriptor)
descriptorWithType_a_b_cSelector = mkSelector "descriptorWithType:a:b:c:"

-- | @Selector@ for @activationType@
activationTypeSelector :: Selector '[] MLCActivationType
activationTypeSelector = mkSelector "activationType"

-- | @Selector@ for @a@
aSelector :: Selector '[] CFloat
aSelector = mkSelector "a"

-- | @Selector@ for @b@
bSelector :: Selector '[] CFloat
bSelector = mkSelector "b"

-- | @Selector@ for @c@
cSelector :: Selector '[] CFloat
cSelector = mkSelector "c"

