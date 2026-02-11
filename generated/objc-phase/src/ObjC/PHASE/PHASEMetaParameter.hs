{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASEMetaParameter
--
-- A generic object that represents an active metaparameter in the system
--
-- Generated bindings for @PHASEMetaParameter@.
module ObjC.PHASE.PHASEMetaParameter
  ( PHASEMetaParameter
  , IsPHASEMetaParameter(..)
  , init_
  , new
  , identifier
  , value
  , setValue
  , initSelector
  , newSelector
  , identifierSelector
  , valueSelector
  , setValueSelector


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
init_ :: IsPHASEMetaParameter phaseMetaParameter => phaseMetaParameter -> IO (Id PHASEMetaParameter)
init_ phaseMetaParameter  =
  sendMsg phaseMetaParameter (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PHASEMetaParameter)
new  =
  do
    cls' <- getRequiredClass "PHASEMetaParameter"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | identifier
--
-- The identifier that uniquely represents this metaparameter.
--
-- ObjC selector: @- identifier@
identifier :: IsPHASEMetaParameter phaseMetaParameter => phaseMetaParameter -> IO (Id NSString)
identifier phaseMetaParameter  =
  sendMsg phaseMetaParameter (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | value
--
-- The value of this metaparameter
--
-- ObjC selector: @- value@
value :: IsPHASEMetaParameter phaseMetaParameter => phaseMetaParameter -> IO RawId
value phaseMetaParameter  =
  fmap (RawId . castPtr) $ sendMsg phaseMetaParameter (mkSelector "value") (retPtr retVoid) []

-- | value
--
-- The value of this metaparameter
--
-- ObjC selector: @- setValue:@
setValue :: IsPHASEMetaParameter phaseMetaParameter => phaseMetaParameter -> RawId -> IO ()
setValue phaseMetaParameter  value =
  sendMsg phaseMetaParameter (mkSelector "setValue:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

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

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector
setValueSelector = mkSelector "setValue:"

