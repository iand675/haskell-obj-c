{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASEDefinition
--
-- The base class for a definition.
--
-- Contains an identifer that uniquely represents this definition.
--
-- Generated bindings for @PHASEDefinition@.
module ObjC.PHASE.PHASEDefinition
  ( PHASEDefinition
  , IsPHASEDefinition(..)
  , init_
  , new
  , identifier
  , initSelector
  , newSelector
  , identifierSelector


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
init_ :: IsPHASEDefinition phaseDefinition => phaseDefinition -> IO (Id PHASEDefinition)
init_ phaseDefinition  =
  sendMsg phaseDefinition (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PHASEDefinition)
new  =
  do
    cls' <- getRequiredClass "PHASEDefinition"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | identifier
--
-- The identifier that uniquely represents this definition.
--
-- ObjC selector: @- identifier@
identifier :: IsPHASEDefinition phaseDefinition => phaseDefinition -> IO (Id NSString)
identifier phaseDefinition  =
  sendMsg phaseDefinition (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

