{-# LANGUAGE PatternSynonyms #-}
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
  , newSelector
  , initSelector
  , descriptorWithTypeSelector
  , descriptorWithType_aSelector
  , descriptorWithType_a_bSelector
  , descriptorWithType_a_b_cSelector
  , activationTypeSelector
  , aSelector
  , bSelector
  , cSelector

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

import ObjC.MLCompute.Internal.Classes
import ObjC.MLCompute.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MLCActivationDescriptor)
new  =
  do
    cls' <- getRequiredClass "MLCActivationDescriptor"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMLCActivationDescriptor mlcActivationDescriptor => mlcActivationDescriptor -> IO (Id MLCActivationDescriptor)
init_ mlcActivationDescriptor  =
  sendMsg mlcActivationDescriptor (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
    sendClassMsg cls' (mkSelector "descriptorWithType:") (retPtr retVoid) [argCInt (coerce activationType)] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "descriptorWithType:a:") (retPtr retVoid) [argCInt (coerce activationType), argCFloat (fromIntegral a)] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "descriptorWithType:a:b:") (retPtr retVoid) [argCInt (coerce activationType), argCFloat (fromIntegral a), argCFloat (fromIntegral b)] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "descriptorWithType:a:b:c:") (retPtr retVoid) [argCInt (coerce activationType), argCFloat (fromIntegral a), argCFloat (fromIntegral b), argCFloat (fromIntegral c)] >>= retainedObject . castPtr

-- | activationType
--
-- The type of activation function
--
-- ObjC selector: @- activationType@
activationType :: IsMLCActivationDescriptor mlcActivationDescriptor => mlcActivationDescriptor -> IO MLCActivationType
activationType mlcActivationDescriptor  =
  fmap (coerce :: CInt -> MLCActivationType) $ sendMsg mlcActivationDescriptor (mkSelector "activationType") retCInt []

-- | a
--
-- Parameter to the activation function
--
-- ObjC selector: @- a@
a :: IsMLCActivationDescriptor mlcActivationDescriptor => mlcActivationDescriptor -> IO CFloat
a mlcActivationDescriptor  =
  sendMsg mlcActivationDescriptor (mkSelector "a") retCFloat []

-- | b
--
-- Parameter to the activation function
--
-- ObjC selector: @- b@
b :: IsMLCActivationDescriptor mlcActivationDescriptor => mlcActivationDescriptor -> IO CFloat
b mlcActivationDescriptor  =
  sendMsg mlcActivationDescriptor (mkSelector "b") retCFloat []

-- | c
--
-- Parameter to the activation function
--
-- ObjC selector: @- c@
c :: IsMLCActivationDescriptor mlcActivationDescriptor => mlcActivationDescriptor -> IO CFloat
c mlcActivationDescriptor  =
  sendMsg mlcActivationDescriptor (mkSelector "c") retCFloat []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @descriptorWithType:@
descriptorWithTypeSelector :: Selector
descriptorWithTypeSelector = mkSelector "descriptorWithType:"

-- | @Selector@ for @descriptorWithType:a:@
descriptorWithType_aSelector :: Selector
descriptorWithType_aSelector = mkSelector "descriptorWithType:a:"

-- | @Selector@ for @descriptorWithType:a:b:@
descriptorWithType_a_bSelector :: Selector
descriptorWithType_a_bSelector = mkSelector "descriptorWithType:a:b:"

-- | @Selector@ for @descriptorWithType:a:b:c:@
descriptorWithType_a_b_cSelector :: Selector
descriptorWithType_a_b_cSelector = mkSelector "descriptorWithType:a:b:c:"

-- | @Selector@ for @activationType@
activationTypeSelector :: Selector
activationTypeSelector = mkSelector "activationType"

-- | @Selector@ for @a@
aSelector :: Selector
aSelector = mkSelector "a"

-- | @Selector@ for @b@
bSelector :: Selector
bSelector = mkSelector "b"

-- | @Selector@ for @c@
cSelector :: Selector
cSelector = mkSelector "c"

