{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SRSupplementalCategory
--
-- A supplemental category to provide more context than just the app category
--
-- The app categories from @SRDeviceUsageCategoryKey@ are very general. Providing a supplemental category allows more context about the specific app while not revealing the exact app identity.
--
-- Generated bindings for @SRSupplementalCategory@.
module ObjC.SensorKit.SRSupplementalCategory
  ( SRSupplementalCategory
  , IsSRSupplementalCategory(..)
  , init_
  , new
  , identifier
  , identifierSelector
  , initSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SensorKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSRSupplementalCategory srSupplementalCategory => srSupplementalCategory -> IO (Id SRSupplementalCategory)
init_ srSupplementalCategory =
  sendOwnedMessage srSupplementalCategory initSelector

-- | @+ new@
new :: IO (Id SRSupplementalCategory)
new  =
  do
    cls' <- getRequiredClass "SRSupplementalCategory"
    sendOwnedClassMessage cls' newSelector

-- | identifier
--
-- An opaque identifier for the supplemental category
--
-- More information about what this category represents can be found in Apple's developer documentation
--
-- ObjC selector: @- identifier@
identifier :: IsSRSupplementalCategory srSupplementalCategory => srSupplementalCategory -> IO (Id NSString)
identifier srSupplementalCategory =
  sendMessage srSupplementalCategory identifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SRSupplementalCategory)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SRSupplementalCategory)
newSelector = mkSelector "new"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

