{-# LANGUAGE DataKinds #-}
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
  , epochIndexSelector
  , initSelector
  , lossValueSelector
  , miniBatchIndexSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreML.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMLMetricKey mlMetricKey => mlMetricKey -> IO (Id MLMetricKey)
init_ mlMetricKey =
  sendOwnedMessage mlMetricKey initSelector

-- | @+ new@
new :: IO RawId
new  =
  do
    cls' <- getRequiredClass "MLMetricKey"
    sendOwnedClassMessage cls' newSelector

-- | @+ lossValue@
lossValue :: IO (Id MLMetricKey)
lossValue  =
  do
    cls' <- getRequiredClass "MLMetricKey"
    sendClassMessage cls' lossValueSelector

-- | @+ epochIndex@
epochIndex :: IO (Id MLMetricKey)
epochIndex  =
  do
    cls' <- getRequiredClass "MLMetricKey"
    sendClassMessage cls' epochIndexSelector

-- | @+ miniBatchIndex@
miniBatchIndex :: IO (Id MLMetricKey)
miniBatchIndex  =
  do
    cls' <- getRequiredClass "MLMetricKey"
    sendClassMessage cls' miniBatchIndexSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MLMetricKey)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] RawId
newSelector = mkSelector "new"

-- | @Selector@ for @lossValue@
lossValueSelector :: Selector '[] (Id MLMetricKey)
lossValueSelector = mkSelector "lossValue"

-- | @Selector@ for @epochIndex@
epochIndexSelector :: Selector '[] (Id MLMetricKey)
epochIndexSelector = mkSelector "epochIndex"

-- | @Selector@ for @miniBatchIndex@
miniBatchIndexSelector :: Selector '[] (Id MLMetricKey)
miniBatchIndexSelector = mkSelector "miniBatchIndex"

