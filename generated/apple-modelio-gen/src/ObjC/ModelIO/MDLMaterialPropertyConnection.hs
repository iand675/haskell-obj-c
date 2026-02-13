{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MDLMaterialPropertyConnection@.
module ObjC.ModelIO.MDLMaterialPropertyConnection
  ( MDLMaterialPropertyConnection
  , IsMDLMaterialPropertyConnection(..)
  , init_
  , initWithOutput_input
  , output
  , input
  , initSelector
  , initWithOutput_inputSelector
  , inputSelector
  , outputSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ModelIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMDLMaterialPropertyConnection mdlMaterialPropertyConnection => mdlMaterialPropertyConnection -> IO (Id MDLMaterialPropertyConnection)
init_ mdlMaterialPropertyConnection =
  sendOwnedMessage mdlMaterialPropertyConnection initSelector

-- | Connects the output to the input
--
-- ObjC selector: @- initWithOutput:input:@
initWithOutput_input :: (IsMDLMaterialPropertyConnection mdlMaterialPropertyConnection, IsMDLMaterialProperty output, IsMDLMaterialProperty input) => mdlMaterialPropertyConnection -> output -> input -> IO (Id MDLMaterialPropertyConnection)
initWithOutput_input mdlMaterialPropertyConnection output input =
  sendOwnedMessage mdlMaterialPropertyConnection initWithOutput_inputSelector (toMDLMaterialProperty output) (toMDLMaterialProperty input)

-- | @- output@
output :: IsMDLMaterialPropertyConnection mdlMaterialPropertyConnection => mdlMaterialPropertyConnection -> IO (Id MDLMaterialProperty)
output mdlMaterialPropertyConnection =
  sendMessage mdlMaterialPropertyConnection outputSelector

-- | @- input@
input :: IsMDLMaterialPropertyConnection mdlMaterialPropertyConnection => mdlMaterialPropertyConnection -> IO (Id MDLMaterialProperty)
input mdlMaterialPropertyConnection =
  sendMessage mdlMaterialPropertyConnection inputSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MDLMaterialPropertyConnection)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithOutput:input:@
initWithOutput_inputSelector :: Selector '[Id MDLMaterialProperty, Id MDLMaterialProperty] (Id MDLMaterialPropertyConnection)
initWithOutput_inputSelector = mkSelector "initWithOutput:input:"

-- | @Selector@ for @output@
outputSelector :: Selector '[] (Id MDLMaterialProperty)
outputSelector = mkSelector "output"

-- | @Selector@ for @input@
inputSelector :: Selector '[] (Id MDLMaterialProperty)
inputSelector = mkSelector "input"

