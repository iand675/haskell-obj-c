{-# LANGUAGE PatternSynonyms #-}
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
  , statusSelector
  , reasonsSelector

  -- * Enum types
  , NIAlgorithmConvergenceStatus(NIAlgorithmConvergenceStatus)
  , pattern NIAlgorithmConvergenceStatusUnknown
  , pattern NIAlgorithmConvergenceStatusNotConverged
  , pattern NIAlgorithmConvergenceStatusConverged

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

import ObjC.NearbyInteraction.Internal.Classes
import ObjC.NearbyInteraction.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Unavailable
--
-- ObjC selector: @- init@
init_ :: IsNIAlgorithmConvergence niAlgorithmConvergence => niAlgorithmConvergence -> IO (Id NIAlgorithmConvergence)
init_ niAlgorithmConvergence  =
    sendMsg niAlgorithmConvergence (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id NIAlgorithmConvergence)
new  =
  do
    cls' <- getRequiredClass "NIAlgorithmConvergence"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- status@
status :: IsNIAlgorithmConvergence niAlgorithmConvergence => niAlgorithmConvergence -> IO NIAlgorithmConvergenceStatus
status niAlgorithmConvergence  =
    fmap (coerce :: CLong -> NIAlgorithmConvergenceStatus) $ sendMsg niAlgorithmConvergence (mkSelector "status") retCLong []

-- | @- reasons@
reasons :: IsNIAlgorithmConvergence niAlgorithmConvergence => niAlgorithmConvergence -> IO (Id NSArray)
reasons niAlgorithmConvergence  =
    sendMsg niAlgorithmConvergence (mkSelector "reasons") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @status@
statusSelector :: Selector
statusSelector = mkSelector "status"

-- | @Selector@ for @reasons@
reasonsSelector :: Selector
reasonsSelector = mkSelector "reasons"

