{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKMapItemRequest@.
module ObjC.MapKit.MKMapItemRequest
  ( MKMapItemRequest
  , IsMKMapItemRequest(..)
  , init_
  , new
  , initWithMapItemIdentifier
  , getMapItemWithCompletionHandler
  , cancel
  , mapItemIdentifier
  , cancelled
  , loading
  , initSelector
  , newSelector
  , initWithMapItemIdentifierSelector
  , getMapItemWithCompletionHandlerSelector
  , cancelSelector
  , mapItemIdentifierSelector
  , cancelledSelector
  , loadingSelector


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

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMKMapItemRequest mkMapItemRequest => mkMapItemRequest -> IO (Id MKMapItemRequest)
init_ mkMapItemRequest  =
  sendMsg mkMapItemRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MKMapItemRequest)
new  =
  do
    cls' <- getRequiredClass "MKMapItemRequest"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithMapItemIdentifier:@
initWithMapItemIdentifier :: (IsMKMapItemRequest mkMapItemRequest, IsMKMapItemIdentifier identifier) => mkMapItemRequest -> identifier -> IO (Id MKMapItemRequest)
initWithMapItemIdentifier mkMapItemRequest  identifier =
withObjCPtr identifier $ \raw_identifier ->
    sendMsg mkMapItemRequest (mkSelector "initWithMapItemIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- getMapItemWithCompletionHandler:@
getMapItemWithCompletionHandler :: IsMKMapItemRequest mkMapItemRequest => mkMapItemRequest -> Ptr () -> IO ()
getMapItemWithCompletionHandler mkMapItemRequest  completionHandler =
  sendMsg mkMapItemRequest (mkSelector "getMapItemWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- cancel@
cancel :: IsMKMapItemRequest mkMapItemRequest => mkMapItemRequest -> IO ()
cancel mkMapItemRequest  =
  sendMsg mkMapItemRequest (mkSelector "cancel") retVoid []

-- | @- mapItemIdentifier@
mapItemIdentifier :: IsMKMapItemRequest mkMapItemRequest => mkMapItemRequest -> IO (Id MKMapItemIdentifier)
mapItemIdentifier mkMapItemRequest  =
  sendMsg mkMapItemRequest (mkSelector "mapItemIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- cancelled@
cancelled :: IsMKMapItemRequest mkMapItemRequest => mkMapItemRequest -> IO Bool
cancelled mkMapItemRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkMapItemRequest (mkSelector "cancelled") retCULong []

-- | @- loading@
loading :: IsMKMapItemRequest mkMapItemRequest => mkMapItemRequest -> IO Bool
loading mkMapItemRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkMapItemRequest (mkSelector "loading") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithMapItemIdentifier:@
initWithMapItemIdentifierSelector :: Selector
initWithMapItemIdentifierSelector = mkSelector "initWithMapItemIdentifier:"

-- | @Selector@ for @getMapItemWithCompletionHandler:@
getMapItemWithCompletionHandlerSelector :: Selector
getMapItemWithCompletionHandlerSelector = mkSelector "getMapItemWithCompletionHandler:"

-- | @Selector@ for @cancel@
cancelSelector :: Selector
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @mapItemIdentifier@
mapItemIdentifierSelector :: Selector
mapItemIdentifierSelector = mkSelector "mapItemIdentifier"

-- | @Selector@ for @cancelled@
cancelledSelector :: Selector
cancelledSelector = mkSelector "cancelled"

-- | @Selector@ for @loading@
loadingSelector :: Selector
loadingSelector = mkSelector "loading"

