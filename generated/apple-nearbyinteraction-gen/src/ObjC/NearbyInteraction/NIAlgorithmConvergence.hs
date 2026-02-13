{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NIAlgorithmConvergence@.
module ObjC.NearbyInteraction.NIAlgorithmConvergence
  ( NIAlgorithmConvergence
  , IsNIAlgorithmConvergence(..)
  , init_
  , new
  , status
  , reasons
  , initSelector
  , newSelector
  , reasonsSelector
  , statusSelector

  -- * Enum types
  , NIAlgorithmConvergenceStatus(NIAlgorithmConvergenceStatus)
  , pattern NIAlgorithmConvergenceStatusUnknown
  , pattern NIAlgorithmConvergenceStatusNotConverged
  , pattern NIAlgorithmConvergenceStatusConverged

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NearbyInteraction.Internal.Classes
import ObjC.NearbyInteraction.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Unavailable
--
-- ObjC selector: @- init@
init_ :: IsNIAlgorithmConvergence niAlgorithmConvergence => niAlgorithmConvergence -> IO (Id NIAlgorithmConvergence)
init_ niAlgorithmConvergence =
  sendOwnedMessage niAlgorithmConvergence initSelector

-- | @+ new@
new :: IO (Id NIAlgorithmConvergence)
new  =
  do
    cls' <- getRequiredClass "NIAlgorithmConvergence"
    sendOwnedClassMessage cls' newSelector

-- | @- status@
status :: IsNIAlgorithmConvergence niAlgorithmConvergence => niAlgorithmConvergence -> IO NIAlgorithmConvergenceStatus
status niAlgorithmConvergence =
  sendMessage niAlgorithmConvergence statusSelector

-- | @- reasons@
reasons :: IsNIAlgorithmConvergence niAlgorithmConvergence => niAlgorithmConvergence -> IO (Id NSArray)
reasons niAlgorithmConvergence =
  sendMessage niAlgorithmConvergence reasonsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NIAlgorithmConvergence)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NIAlgorithmConvergence)
newSelector = mkSelector "new"

-- | @Selector@ for @status@
statusSelector :: Selector '[] NIAlgorithmConvergenceStatus
statusSelector = mkSelector "status"

-- | @Selector@ for @reasons@
reasonsSelector :: Selector '[] (Id NSArray)
reasonsSelector = mkSelector "reasons"

