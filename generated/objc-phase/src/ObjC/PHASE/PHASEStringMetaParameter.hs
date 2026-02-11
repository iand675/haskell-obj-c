{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASEStringMetaParameter
--
-- An object that represents an active string metaparameter in the system
--
-- Generated bindings for @PHASEStringMetaParameter@.
module ObjC.PHASE.PHASEStringMetaParameter
  ( PHASEStringMetaParameter
  , IsPHASEStringMetaParameter(..)
  , init_
  , new
  , initSelector
  , newSelector


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
init_ :: IsPHASEStringMetaParameter phaseStringMetaParameter => phaseStringMetaParameter -> IO (Id PHASEStringMetaParameter)
init_ phaseStringMetaParameter  =
  sendMsg phaseStringMetaParameter (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PHASEStringMetaParameter)
new  =
  do
    cls' <- getRequiredClass "PHASEStringMetaParameter"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

