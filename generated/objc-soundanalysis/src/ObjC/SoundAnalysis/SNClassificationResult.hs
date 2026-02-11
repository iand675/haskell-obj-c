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
  , initSelector
  , newSelector
  , classificationForIdentifierSelector
  , classificationsSelector


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
init_ :: IsSNClassificationResult snClassificationResult => snClassificationResult -> IO (Id SNClassificationResult)
init_ snClassificationResult  =
  sendMsg snClassificationResult (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SNClassificationResult)
new  =
  do
    cls' <- getRequiredClass "SNClassificationResult"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Retrieves the classification candidate with the specified identifier.
--
-- - Parameter identifier: An identifier on which to query for a particular classification candidate. The query will match to any classification candidate whose @identifier@ property (see @identifier@ property of @SNClassification@) contains a value equal to the provided argument.
--
-- - Returns: The classification candidate which has the specified identifier, if it exists. If no such candidate exists, @nil@ will be returned.
--
-- ObjC selector: @- classificationForIdentifier:@
classificationForIdentifier :: (IsSNClassificationResult snClassificationResult, IsNSString identifier) => snClassificationResult -> identifier -> IO (Id SNClassification)
classificationForIdentifier snClassificationResult  identifier =
withObjCPtr identifier $ \raw_identifier ->
    sendMsg snClassificationResult (mkSelector "classificationForIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= retainedObject . castPtr

-- | All classification candidates, sorted with highest confidence first.
--
-- ObjC selector: @- classifications@
classifications :: IsSNClassificationResult snClassificationResult => snClassificationResult -> IO (Id NSArray)
classifications snClassificationResult  =
  sendMsg snClassificationResult (mkSelector "classifications") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @classificationForIdentifier:@
classificationForIdentifierSelector :: Selector
classificationForIdentifierSelector = mkSelector "classificationForIdentifier:"

-- | @Selector@ for @classifications@
classificationsSelector :: Selector
classificationsSelector = mkSelector "classifications"

