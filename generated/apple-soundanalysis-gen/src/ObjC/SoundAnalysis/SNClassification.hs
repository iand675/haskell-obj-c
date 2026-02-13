{-# LANGUAGE DataKinds #-}
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
  , confidenceSelector
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

import ObjC.SoundAnalysis.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSNClassification snClassification => snClassification -> IO (Id SNClassification)
init_ snClassification =
  sendOwnedMessage snClassification initSelector

-- | @+ new@
new :: IO (Id SNClassification)
new  =
  do
    cls' <- getRequiredClass "SNClassification"
    sendOwnedClassMessage cls' newSelector

-- | The identifier of a classification request. An example classification could be a string like 'laughter' or 'applause'. The string is defined in the model that was used for the classification. Usually these are technical labels that are not localized and not meant to be used directly to be presented to an end user in the UI.
--
-- ObjC selector: @- identifier@
identifier :: IsSNClassification snClassification => snClassification -> IO (Id NSString)
identifier snClassification =
  sendMessage snClassification identifierSelector

-- | The level of confidence normalized to [0, 1], where 1 is most confident
--
-- ObjC selector: @- confidence@
confidence :: IsSNClassification snClassification => snClassification -> IO CDouble
confidence snClassification =
  sendMessage snClassification confidenceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SNClassification)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SNClassification)
newSelector = mkSelector "new"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @confidence@
confidenceSelector :: Selector '[] CDouble
confidenceSelector = mkSelector "confidence"

