{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CNFetchResult@.
module ObjC.Contacts.CNFetchResult
  ( CNFetchResult
  , IsCNFetchResult(..)
  , init_
  , new
  , value
  , currentHistoryToken
  , initSelector
  , newSelector
  , valueSelector
  , currentHistoryTokenSelector


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

import ObjC.Contacts.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCNFetchResult cnFetchResult => cnFetchResult -> IO (Id CNFetchResult)
init_ cnFetchResult  =
  sendMsg cnFetchResult (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CNFetchResult)
new  =
  do
    cls' <- getRequiredClass "CNFetchResult"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- value@
value :: IsCNFetchResult cnFetchResult => cnFetchResult -> IO RawId
value cnFetchResult  =
  fmap (RawId . castPtr) $ sendMsg cnFetchResult (mkSelector "value") (retPtr retVoid) []

-- | @- currentHistoryToken@
currentHistoryToken :: IsCNFetchResult cnFetchResult => cnFetchResult -> IO (Id NSData)
currentHistoryToken cnFetchResult  =
  sendMsg cnFetchResult (mkSelector "currentHistoryToken") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @currentHistoryToken@
currentHistoryTokenSelector :: Selector
currentHistoryTokenSelector = mkSelector "currentHistoryToken"

