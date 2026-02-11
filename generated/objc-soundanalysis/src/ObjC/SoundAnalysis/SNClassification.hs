{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The likelihood of a sound belonging to identified class
--
-- Generated bindings for @SNClassification@.
module ObjC.SoundAnalysis.SNClassification
  ( SNClassification
  , IsSNClassification(..)
  , init_
  , new
  , identifier
  , confidence
  , initSelector
  , newSelector
  , identifierSelector
  , confidenceSelector


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

import ObjC.SoundAnalysis.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSNClassification snClassification => snClassification -> IO (Id SNClassification)
init_ snClassification  =
  sendMsg snClassification (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SNClassification)
new  =
  do
    cls' <- getRequiredClass "SNClassification"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The identifier of a classification request. An example classification could be a string like 'laughter' or 'applause'. The string is defined in the model that was used for the classification. Usually these are technical labels that are not localized and not meant to be used directly to be presented to an end user in the UI.
--
-- ObjC selector: @- identifier@
identifier :: IsSNClassification snClassification => snClassification -> IO (Id NSString)
identifier snClassification  =
  sendMsg snClassification (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The level of confidence normalized to [0, 1], where 1 is most confident
--
-- ObjC selector: @- confidence@
confidence :: IsSNClassification snClassification => snClassification -> IO CDouble
confidence snClassification  =
  sendMsg snClassification (mkSelector "confidence") retCDouble []

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

-- | @Selector@ for @confidence@
confidenceSelector :: Selector
confidenceSelector = mkSelector "confidence"

