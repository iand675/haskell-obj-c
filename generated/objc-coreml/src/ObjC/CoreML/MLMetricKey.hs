{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class to specify list of supported model update metrics.
--
-- Generated bindings for @MLMetricKey@.
module ObjC.CoreML.MLMetricKey
  ( MLMetricKey
  , IsMLMetricKey(..)
  , init_
  , new
  , lossValue
  , epochIndex
  , miniBatchIndex
  , initSelector
  , newSelector
  , lossValueSelector
  , epochIndexSelector
  , miniBatchIndexSelector


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

import ObjC.CoreML.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMLMetricKey mlMetricKey => mlMetricKey -> IO (Id MLMetricKey)
init_ mlMetricKey  =
  sendMsg mlMetricKey (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO RawId
new  =
  do
    cls' <- getRequiredClass "MLMetricKey"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "new") (retPtr retVoid) []

-- | @+ lossValue@
lossValue :: IO (Id MLMetricKey)
lossValue  =
  do
    cls' <- getRequiredClass "MLMetricKey"
    sendClassMsg cls' (mkSelector "lossValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ epochIndex@
epochIndex :: IO (Id MLMetricKey)
epochIndex  =
  do
    cls' <- getRequiredClass "MLMetricKey"
    sendClassMsg cls' (mkSelector "epochIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ miniBatchIndex@
miniBatchIndex :: IO (Id MLMetricKey)
miniBatchIndex  =
  do
    cls' <- getRequiredClass "MLMetricKey"
    sendClassMsg cls' (mkSelector "miniBatchIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @lossValue@
lossValueSelector :: Selector
lossValueSelector = mkSelector "lossValue"

-- | @Selector@ for @epochIndex@
epochIndexSelector :: Selector
epochIndexSelector = mkSelector "epochIndex"

-- | @Selector@ for @miniBatchIndex@
miniBatchIndexSelector :: Selector
miniBatchIndexSelector = mkSelector "miniBatchIndex"

