{-# LANGUAGE DataKinds #-}
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
  , identifierSelector
  , initSelector
  , newSelector
  , setValueSelector
  , valueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PHASE.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHASEMetaParameter phaseMetaParameter => phaseMetaParameter -> IO (Id PHASEMetaParameter)
init_ phaseMetaParameter =
  sendOwnedMessage phaseMetaParameter initSelector

-- | @+ new@
new :: IO (Id PHASEMetaParameter)
new  =
  do
    cls' <- getRequiredClass "PHASEMetaParameter"
    sendOwnedClassMessage cls' newSelector

-- | identifier
--
-- The identifier that uniquely represents this metaparameter.
--
-- ObjC selector: @- identifier@
identifier :: IsPHASEMetaParameter phaseMetaParameter => phaseMetaParameter -> IO (Id NSString)
identifier phaseMetaParameter =
  sendMessage phaseMetaParameter identifierSelector

-- | value
--
-- The value of this metaparameter
--
-- ObjC selector: @- value@
value :: IsPHASEMetaParameter phaseMetaParameter => phaseMetaParameter -> IO RawId
value phaseMetaParameter =
  sendMessage phaseMetaParameter valueSelector

-- | value
--
-- The value of this metaparameter
--
-- ObjC selector: @- setValue:@
setValue :: IsPHASEMetaParameter phaseMetaParameter => phaseMetaParameter -> RawId -> IO ()
setValue phaseMetaParameter value =
  sendMessage phaseMetaParameter setValueSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASEMetaParameter)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHASEMetaParameter)
newSelector = mkSelector "new"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @value@
valueSelector :: Selector '[] RawId
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector '[RawId] ()
setValueSelector = mkSelector "setValue:"

