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
  , newSelector
  , initSelector
  , descriptorWithAlpha_transposesX_transposesYSelector
  , descriptorSelector
  , alphaSelector
  , transposesXSelector
  , transposesYSelector


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
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MLCMatMulDescriptor)
new  =
  do
    cls' <- getRequiredClass "MLCMatMulDescriptor"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMLCMatMulDescriptor mlcMatMulDescriptor => mlcMatMulDescriptor -> IO (Id MLCMatMulDescriptor)
init_ mlcMatMulDescriptor  =
  sendMsg mlcMatMulDescriptor (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
    sendClassMsg cls' (mkSelector "descriptorWithAlpha:transposesX:transposesY:") (retPtr retVoid) [argCFloat (fromIntegral alpha), argCULong (if transposesX then 1 else 0), argCULong (if transposesY then 1 else 0)] >>= retainedObject . castPtr

-- | descriptor
--
-- A matrix multiplication layer descriptor
--
-- ObjC selector: @+ descriptor@
descriptor :: IO (Id MLCMatMulDescriptor)
descriptor  =
  do
    cls' <- getRequiredClass "MLCMatMulDescriptor"
    sendClassMsg cls' (mkSelector "descriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | a scalar to scale the result in C=alpha x X x Y. Default = 1.0
--
-- ObjC selector: @- alpha@
alpha :: IsMLCMatMulDescriptor mlcMatMulDescriptor => mlcMatMulDescriptor -> IO CFloat
alpha mlcMatMulDescriptor  =
  sendMsg mlcMatMulDescriptor (mkSelector "alpha") retCFloat []

-- | if true, transposes the last two dimensions of X. Default = False
--
-- ObjC selector: @- transposesX@
transposesX :: IsMLCMatMulDescriptor mlcMatMulDescriptor => mlcMatMulDescriptor -> IO Bool
transposesX mlcMatMulDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mlcMatMulDescriptor (mkSelector "transposesX") retCULong []

-- | if true, transposes the last two dimensions of Y. Default = False
--
-- ObjC selector: @- transposesY@
transposesY :: IsMLCMatMulDescriptor mlcMatMulDescriptor => mlcMatMulDescriptor -> IO Bool
transposesY mlcMatMulDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mlcMatMulDescriptor (mkSelector "transposesY") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @descriptorWithAlpha:transposesX:transposesY:@
descriptorWithAlpha_transposesX_transposesYSelector :: Selector
descriptorWithAlpha_transposesX_transposesYSelector = mkSelector "descriptorWithAlpha:transposesX:transposesY:"

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector
descriptorSelector = mkSelector "descriptor"

-- | @Selector@ for @alpha@
alphaSelector :: Selector
alphaSelector = mkSelector "alpha"

-- | @Selector@ for @transposesX@
transposesXSelector :: Selector
transposesXSelector = mkSelector "transposesX"

-- | @Selector@ for @transposesY@
transposesYSelector :: Selector
transposesYSelector = mkSelector "transposesY"

