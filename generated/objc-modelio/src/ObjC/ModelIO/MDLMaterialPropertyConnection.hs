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
  , outputSelector
  , inputSelector


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

import ObjC.ModelIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMDLMaterialPropertyConnection mdlMaterialPropertyConnection => mdlMaterialPropertyConnection -> IO (Id MDLMaterialPropertyConnection)
init_ mdlMaterialPropertyConnection  =
  sendMsg mdlMaterialPropertyConnection (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Connects the output to the input
--
-- ObjC selector: @- initWithOutput:input:@
initWithOutput_input :: (IsMDLMaterialPropertyConnection mdlMaterialPropertyConnection, IsMDLMaterialProperty output, IsMDLMaterialProperty input) => mdlMaterialPropertyConnection -> output -> input -> IO (Id MDLMaterialPropertyConnection)
initWithOutput_input mdlMaterialPropertyConnection  output input =
withObjCPtr output $ \raw_output ->
  withObjCPtr input $ \raw_input ->
      sendMsg mdlMaterialPropertyConnection (mkSelector "initWithOutput:input:") (retPtr retVoid) [argPtr (castPtr raw_output :: Ptr ()), argPtr (castPtr raw_input :: Ptr ())] >>= ownedObject . castPtr

-- | @- output@
output :: IsMDLMaterialPropertyConnection mdlMaterialPropertyConnection => mdlMaterialPropertyConnection -> IO (Id MDLMaterialProperty)
output mdlMaterialPropertyConnection  =
  sendMsg mdlMaterialPropertyConnection (mkSelector "output") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- input@
input :: IsMDLMaterialPropertyConnection mdlMaterialPropertyConnection => mdlMaterialPropertyConnection -> IO (Id MDLMaterialProperty)
input mdlMaterialPropertyConnection  =
  sendMsg mdlMaterialPropertyConnection (mkSelector "input") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithOutput:input:@
initWithOutput_inputSelector :: Selector
initWithOutput_inputSelector = mkSelector "initWithOutput:input:"

-- | @Selector@ for @output@
outputSelector :: Selector
outputSelector = mkSelector "output"

-- | @Selector@ for @input@
inputSelector :: Selector
inputSelector = mkSelector "input"

