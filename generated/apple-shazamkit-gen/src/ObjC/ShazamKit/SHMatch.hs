{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that represents the catalog media items that match a query.
--
-- A single query signature may match more than one reference signature. In addition, one reference signature may map to many media items.
--
-- Generated bindings for @SHMatch@.
module ObjC.ShazamKit.SHMatch
  ( SHMatch
  , IsSHMatch(..)
  , new
  , init_
  , mediaItems
  , querySignature
  , initSelector
  , mediaItemsSelector
  , newSelector
  , querySignatureSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ShazamKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id SHMatch)
new  =
  do
    cls' <- getRequiredClass "SHMatch"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsSHMatch shMatch => shMatch -> IO (Id SHMatch)
init_ shMatch =
  sendOwnedMessage shMatch initSelector

-- | An array of the media items in the catalog that match the query signature, in order of the quality of the match.
--
-- ObjC selector: @- mediaItems@
mediaItems :: IsSHMatch shMatch => shMatch -> IO (Id NSArray)
mediaItems shMatch =
  sendMessage shMatch mediaItemsSelector

-- | The query signature for the match.
--
-- ObjC selector: @- querySignature@
querySignature :: IsSHMatch shMatch => shMatch -> IO (Id SHSignature)
querySignature shMatch =
  sendMessage shMatch querySignatureSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SHMatch)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SHMatch)
initSelector = mkSelector "init"

-- | @Selector@ for @mediaItems@
mediaItemsSelector :: Selector '[] (Id NSArray)
mediaItemsSelector = mkSelector "mediaItems"

-- | @Selector@ for @querySignature@
querySignatureSelector :: Selector '[] (Id SHSignature)
querySignatureSelector = mkSelector "querySignature"

