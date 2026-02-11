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
  , initSelector
  , newSelector
  , formatDescriptionSelector
  , validFrameDurationsSelector


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

import ObjC.CoreMediaIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCMIOExtensionStreamFormat cmioExtensionStreamFormat => cmioExtensionStreamFormat -> IO (Id CMIOExtensionStreamFormat)
init_ cmioExtensionStreamFormat  =
    sendMsg cmioExtensionStreamFormat (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CMIOExtensionStreamFormat)
new  =
  do
    cls' <- getRequiredClass "CMIOExtensionStreamFormat"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | formatDescription
--
-- The format description of the samples delivered by the stream.
--
-- ObjC selector: @- formatDescription@
formatDescription :: IsCMIOExtensionStreamFormat cmioExtensionStreamFormat => cmioExtensionStreamFormat -> IO RawId
formatDescription cmioExtensionStreamFormat  =
    fmap (RawId . castPtr) $ sendMsg cmioExtensionStreamFormat (mkSelector "formatDescription") (retPtr retVoid) []

-- | validFrameDurations
--
-- The valid frame durations as an array of CMTime as dictionaries. The CMTime in dictionary format are made with CMTimeCopyAsDictionary.
--
-- ObjC selector: @- validFrameDurations@
validFrameDurations :: IsCMIOExtensionStreamFormat cmioExtensionStreamFormat => cmioExtensionStreamFormat -> IO (Id NSArray)
validFrameDurations cmioExtensionStreamFormat  =
    sendMsg cmioExtensionStreamFormat (mkSelector "validFrameDurations") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @formatDescription@
formatDescriptionSelector :: Selector
formatDescriptionSelector = mkSelector "formatDescription"

-- | @Selector@ for @validFrameDurations@
validFrameDurationsSelector :: Selector
validFrameDurationsSelector = mkSelector "validFrameDurations"

