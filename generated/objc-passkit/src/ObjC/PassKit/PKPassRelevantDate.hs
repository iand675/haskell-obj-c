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
  , newSelector
  , initSelector
  , intervalSelector
  , dateSelector


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

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id PKPassRelevantDate)
new  =
  do
    cls' <- getRequiredClass "PKPassRelevantDate"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsPKPassRelevantDate pkPassRelevantDate => pkPassRelevantDate -> IO (Id PKPassRelevantDate)
init_ pkPassRelevantDate  =
  sendMsg pkPassRelevantDate (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- interval@
interval :: IsPKPassRelevantDate pkPassRelevantDate => pkPassRelevantDate -> IO (Id NSDateInterval)
interval pkPassRelevantDate  =
  sendMsg pkPassRelevantDate (mkSelector "interval") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- date@
date :: IsPKPassRelevantDate pkPassRelevantDate => pkPassRelevantDate -> IO (Id NSDate)
date pkPassRelevantDate  =
  sendMsg pkPassRelevantDate (mkSelector "date") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @interval@
intervalSelector :: Selector
intervalSelector = mkSelector "interval"

-- | @Selector@ for @date@
dateSelector :: Selector
dateSelector = mkSelector "date"

