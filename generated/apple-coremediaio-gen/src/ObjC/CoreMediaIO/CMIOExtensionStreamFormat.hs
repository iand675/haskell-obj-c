{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CMIOExtensionStreamFormat
--
-- A CMIOExtensionStreamFormat describes a stream format.
--
-- Generated bindings for @CMIOExtensionStreamFormat@.
module ObjC.CoreMediaIO.CMIOExtensionStreamFormat
  ( CMIOExtensionStreamFormat
  , IsCMIOExtensionStreamFormat(..)
  , init_
  , new
  , formatDescription
  , validFrameDurations
  , formatDescriptionSelector
  , initSelector
  , newSelector
  , validFrameDurationsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreMediaIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCMIOExtensionStreamFormat cmioExtensionStreamFormat => cmioExtensionStreamFormat -> IO (Id CMIOExtensionStreamFormat)
init_ cmioExtensionStreamFormat =
  sendOwnedMessage cmioExtensionStreamFormat initSelector

-- | @+ new@
new :: IO (Id CMIOExtensionStreamFormat)
new  =
  do
    cls' <- getRequiredClass "CMIOExtensionStreamFormat"
    sendOwnedClassMessage cls' newSelector

-- | formatDescription
--
-- The format description of the samples delivered by the stream.
--
-- ObjC selector: @- formatDescription@
formatDescription :: IsCMIOExtensionStreamFormat cmioExtensionStreamFormat => cmioExtensionStreamFormat -> IO RawId
formatDescription cmioExtensionStreamFormat =
  sendMessage cmioExtensionStreamFormat formatDescriptionSelector

-- | validFrameDurations
--
-- The valid frame durations as an array of CMTime as dictionaries. The CMTime in dictionary format are made with CMTimeCopyAsDictionary.
--
-- ObjC selector: @- validFrameDurations@
validFrameDurations :: IsCMIOExtensionStreamFormat cmioExtensionStreamFormat => cmioExtensionStreamFormat -> IO (Id NSArray)
validFrameDurations cmioExtensionStreamFormat =
  sendMessage cmioExtensionStreamFormat validFrameDurationsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CMIOExtensionStreamFormat)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CMIOExtensionStreamFormat)
newSelector = mkSelector "new"

-- | @Selector@ for @formatDescription@
formatDescriptionSelector :: Selector '[] RawId
formatDescriptionSelector = mkSelector "formatDescription"

-- | @Selector@ for @validFrameDurations@
validFrameDurationsSelector :: Selector '[] (Id NSArray)
validFrameDurationsSelector = mkSelector "validFrameDurations"

