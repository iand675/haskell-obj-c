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
  , newSelector
  , initSelector
  , mediaItemsSelector
  , querySignatureSelector


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

import ObjC.ShazamKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id SHMatch)
new  =
  do
    cls' <- getRequiredClass "SHMatch"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsSHMatch shMatch => shMatch -> IO (Id SHMatch)
init_ shMatch  =
  sendMsg shMatch (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | An array of the media items in the catalog that match the query signature, in order of the quality of the match.
--
-- ObjC selector: @- mediaItems@
mediaItems :: IsSHMatch shMatch => shMatch -> IO (Id NSArray)
mediaItems shMatch  =
  sendMsg shMatch (mkSelector "mediaItems") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The query signature for the match.
--
-- ObjC selector: @- querySignature@
querySignature :: IsSHMatch shMatch => shMatch -> IO (Id SHSignature)
querySignature shMatch  =
  sendMsg shMatch (mkSelector "querySignature") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @mediaItems@
mediaItemsSelector :: Selector
mediaItemsSelector = mkSelector "mediaItems"

-- | @Selector@ for @querySignature@
querySignatureSelector :: Selector
querySignatureSelector = mkSelector "querySignature"

