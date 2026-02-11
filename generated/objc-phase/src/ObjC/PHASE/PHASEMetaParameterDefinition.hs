{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASEMetaParameterDefinition
--
-- A base object for metaparameter definitions
--
-- Generated bindings for @PHASEMetaParameterDefinition@.
module ObjC.PHASE.PHASEMetaParameterDefinition
  ( PHASEMetaParameterDefinition
  , IsPHASEMetaParameterDefinition(..)
  , init_
  , new
  , value
  , initSelector
  , newSelector
  , valueSelector


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

import ObjC.PHASE.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHASEMetaParameterDefinition phaseMetaParameterDefinition => phaseMetaParameterDefinition -> IO (Id PHASEMetaParameterDefinition)
init_ phaseMetaParameterDefinition  =
  sendMsg phaseMetaParameterDefinition (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PHASEMetaParameterDefinition)
new  =
  do
    cls' <- getRequiredClass "PHASEMetaParameterDefinition"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | value
--
-- The value of the metaparameter.
--
-- ObjC selector: @- value@
value :: IsPHASEMetaParameterDefinition phaseMetaParameterDefinition => phaseMetaParameterDefinition -> IO RawId
value phaseMetaParameterDefinition  =
  fmap (RawId . castPtr) $ sendMsg phaseMetaParameterDefinition (mkSelector "value") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

