{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MESampleLocation
--
-- Provides information about the sample location with the media.
--
-- An instance of this class is returned by calls to the MESampleCursor method sampleLocation.
--
-- Generated bindings for @MESampleLocation@.
module ObjC.MediaExtension.MESampleLocation
  ( MESampleLocation
  , IsMESampleLocation(..)
  , new
  , init_
  , byteSource
  , byteSourceSelector
  , initSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MESampleLocation)
new  =
  do
    cls' <- getRequiredClass "MESampleLocation"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsMESampleLocation meSampleLocation => meSampleLocation -> IO (Id MESampleLocation)
init_ meSampleLocation =
  sendOwnedMessage meSampleLocation initSelector

-- | byteSource
--
-- The MEByteSource to be used to read the data for the sample.
--
-- ObjC selector: @- byteSource@
byteSource :: IsMESampleLocation meSampleLocation => meSampleLocation -> IO (Id MEByteSource)
byteSource meSampleLocation =
  sendMessage meSampleLocation byteSourceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MESampleLocation)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MESampleLocation)
initSelector = mkSelector "init"

-- | @Selector@ for @byteSource@
byteSourceSelector :: Selector '[] (Id MEByteSource)
byteSourceSelector = mkSelector "byteSource"

