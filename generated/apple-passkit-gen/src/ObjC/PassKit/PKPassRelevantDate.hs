{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKPassRelevantDate@.
module ObjC.PassKit.PKPassRelevantDate
  ( PKPassRelevantDate
  , IsPKPassRelevantDate(..)
  , new
  , init_
  , interval
  , date
  , dateSelector
  , initSelector
  , intervalSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id PKPassRelevantDate)
new  =
  do
    cls' <- getRequiredClass "PKPassRelevantDate"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsPKPassRelevantDate pkPassRelevantDate => pkPassRelevantDate -> IO (Id PKPassRelevantDate)
init_ pkPassRelevantDate =
  sendOwnedMessage pkPassRelevantDate initSelector

-- | @- interval@
interval :: IsPKPassRelevantDate pkPassRelevantDate => pkPassRelevantDate -> IO (Id NSDateInterval)
interval pkPassRelevantDate =
  sendMessage pkPassRelevantDate intervalSelector

-- | @- date@
date :: IsPKPassRelevantDate pkPassRelevantDate => pkPassRelevantDate -> IO (Id NSDate)
date pkPassRelevantDate =
  sendMessage pkPassRelevantDate dateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PKPassRelevantDate)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PKPassRelevantDate)
initSelector = mkSelector "init"

-- | @Selector@ for @interval@
intervalSelector :: Selector '[] (Id NSDateInterval)
intervalSelector = mkSelector "interval"

-- | @Selector@ for @date@
dateSelector :: Selector '[] (Id NSDate)
dateSelector = mkSelector "date"

