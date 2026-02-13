{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCMatMulDescriptor
--
-- The MLCMatMulDescriptor specifies a batched matrix multiplication descriptor
--
-- Generated bindings for @MLCMatMulDescriptor@.
module ObjC.MLCompute.MLCMatMulDescriptor
  ( MLCMatMulDescriptor
  , IsMLCMatMulDescriptor(..)
  , new
  , init_
  , descriptorWithAlpha_transposesX_transposesY
  , descriptor
  , alpha
  , transposesX
  , transposesY
  , alphaSelector
  , descriptorSelector
  , descriptorWithAlpha_transposesX_transposesYSelector
  , initSelector
  , newSelector
  , transposesXSelector
  , transposesYSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MLCompute.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MLCMatMulDescriptor)
new  =
  do
    cls' <- getRequiredClass "MLCMatMulDescriptor"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsMLCMatMulDescriptor mlcMatMulDescriptor => mlcMatMulDescriptor -> IO (Id MLCMatMulDescriptor)
init_ mlcMatMulDescriptor =
  sendOwnedMessage mlcMatMulDescriptor initSelector

-- | A matrix multiplication layer descriptor
--
-- @alpha@ — a scalar to scale the left hand side, C = alpha x X x Y
--
-- @transposesX@ — if true, transposes the last two dimensions of X
--
-- @transposesY@ — if true, transposes the last two dimensions of Y
--
-- Returns: A new matrix multiplication layer descriptor
--
-- ObjC selector: @+ descriptorWithAlpha:transposesX:transposesY:@
descriptorWithAlpha_transposesX_transposesY :: CFloat -> Bool -> Bool -> IO (Id MLCMatMulDescriptor)
descriptorWithAlpha_transposesX_transposesY alpha transposesX transposesY =
  do
    cls' <- getRequiredClass "MLCMatMulDescriptor"
    sendClassMessage cls' descriptorWithAlpha_transposesX_transposesYSelector alpha transposesX transposesY

-- | descriptor
--
-- A matrix multiplication layer descriptor
--
-- ObjC selector: @+ descriptor@
descriptor :: IO (Id MLCMatMulDescriptor)
descriptor  =
  do
    cls' <- getRequiredClass "MLCMatMulDescriptor"
    sendClassMessage cls' descriptorSelector

-- | a scalar to scale the result in C=alpha x X x Y. Default = 1.0
--
-- ObjC selector: @- alpha@
alpha :: IsMLCMatMulDescriptor mlcMatMulDescriptor => mlcMatMulDescriptor -> IO CFloat
alpha mlcMatMulDescriptor =
  sendMessage mlcMatMulDescriptor alphaSelector

-- | if true, transposes the last two dimensions of X. Default = False
--
-- ObjC selector: @- transposesX@
transposesX :: IsMLCMatMulDescriptor mlcMatMulDescriptor => mlcMatMulDescriptor -> IO Bool
transposesX mlcMatMulDescriptor =
  sendMessage mlcMatMulDescriptor transposesXSelector

-- | if true, transposes the last two dimensions of Y. Default = False
--
-- ObjC selector: @- transposesY@
transposesY :: IsMLCMatMulDescriptor mlcMatMulDescriptor => mlcMatMulDescriptor -> IO Bool
transposesY mlcMatMulDescriptor =
  sendMessage mlcMatMulDescriptor transposesYSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MLCMatMulDescriptor)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MLCMatMulDescriptor)
initSelector = mkSelector "init"

-- | @Selector@ for @descriptorWithAlpha:transposesX:transposesY:@
descriptorWithAlpha_transposesX_transposesYSelector :: Selector '[CFloat, Bool, Bool] (Id MLCMatMulDescriptor)
descriptorWithAlpha_transposesX_transposesYSelector = mkSelector "descriptorWithAlpha:transposesX:transposesY:"

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector '[] (Id MLCMatMulDescriptor)
descriptorSelector = mkSelector "descriptor"

-- | @Selector@ for @alpha@
alphaSelector :: Selector '[] CFloat
alphaSelector = mkSelector "alpha"

-- | @Selector@ for @transposesX@
transposesXSelector :: Selector '[] Bool
transposesXSelector = mkSelector "transposesX"

-- | @Selector@ for @transposesY@
transposesYSelector :: Selector '[] Bool
transposesYSelector = mkSelector "transposesY"

