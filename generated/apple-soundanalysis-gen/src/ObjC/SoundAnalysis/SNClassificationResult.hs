{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A result containing the most likely classification candidates in the time range specified
--
-- Generated bindings for @SNClassificationResult@.
module ObjC.SoundAnalysis.SNClassificationResult
  ( SNClassificationResult
  , IsSNClassificationResult(..)
  , init_
  , new
  , classificationForIdentifier
  , classifications
  , classificationForIdentifierSelector
  , classificationsSelector
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
init_ :: IsSNClassificationResult snClassificationResult => snClassificationResult -> IO (Id SNClassificationResult)
init_ snClassificationResult =
  sendOwnedMessage snClassificationResult initSelector

-- | @+ new@
new :: IO (Id SNClassificationResult)
new  =
  do
    cls' <- getRequiredClass "SNClassificationResult"
    sendOwnedClassMessage cls' newSelector

-- | Retrieves the classification candidate with the specified identifier.
--
-- - Parameter identifier: An identifier on which to query for a particular classification candidate. The query will match to any classification candidate whose @identifier@ property (see @identifier@ property of @SNClassification@) contains a value equal to the provided argument.
--
-- - Returns: The classification candidate which has the specified identifier, if it exists. If no such candidate exists, @nil@ will be returned.
--
-- ObjC selector: @- classificationForIdentifier:@
classificationForIdentifier :: (IsSNClassificationResult snClassificationResult, IsNSString identifier) => snClassificationResult -> identifier -> IO (Id SNClassification)
classificationForIdentifier snClassificationResult identifier =
  sendMessage snClassificationResult classificationForIdentifierSelector (toNSString identifier)

-- | All classification candidates, sorted with highest confidence first.
--
-- ObjC selector: @- classifications@
classifications :: IsSNClassificationResult snClassificationResult => snClassificationResult -> IO (Id NSArray)
classifications snClassificationResult =
  sendMessage snClassificationResult classificationsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SNClassificationResult)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SNClassificationResult)
newSelector = mkSelector "new"

-- | @Selector@ for @classificationForIdentifier:@
classificationForIdentifierSelector :: Selector '[Id NSString] (Id SNClassification)
classificationForIdentifierSelector = mkSelector "classificationForIdentifier:"

-- | @Selector@ for @classifications@
classificationsSelector :: Selector '[] (Id NSArray)
classificationsSelector = mkSelector "classifications"

