{-# LANGUAGE DataKinds #-}
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
  , currentHistoryTokenSelector
  , initSelector
  , newSelector
  , valueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Contacts.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCNFetchResult cnFetchResult => cnFetchResult -> IO (Id CNFetchResult)
init_ cnFetchResult =
  sendOwnedMessage cnFetchResult initSelector

-- | @+ new@
new :: IO (Id CNFetchResult)
new  =
  do
    cls' <- getRequiredClass "CNFetchResult"
    sendOwnedClassMessage cls' newSelector

-- | @- value@
value :: IsCNFetchResult cnFetchResult => cnFetchResult -> IO RawId
value cnFetchResult =
  sendMessage cnFetchResult valueSelector

-- | @- currentHistoryToken@
currentHistoryToken :: IsCNFetchResult cnFetchResult => cnFetchResult -> IO (Id NSData)
currentHistoryToken cnFetchResult =
  sendMessage cnFetchResult currentHistoryTokenSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CNFetchResult)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CNFetchResult)
newSelector = mkSelector "new"

-- | @Selector@ for @value@
valueSelector :: Selector '[] RawId
valueSelector = mkSelector "value"

-- | @Selector@ for @currentHistoryToken@
currentHistoryTokenSelector :: Selector '[] (Id NSData)
currentHistoryTokenSelector = mkSelector "currentHistoryToken"

