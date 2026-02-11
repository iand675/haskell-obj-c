{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKDirections@.
module ObjC.MapKit.MKDirections
  ( MKDirections
  , IsMKDirections(..)
  , initWithRequest
  , calculateDirectionsWithCompletionHandler
  , calculateETAWithCompletionHandler
  , cancel
  , calculating
  , initWithRequestSelector
  , calculateDirectionsWithCompletionHandlerSelector
  , calculateETAWithCompletionHandlerSelector
  , cancelSelector
  , calculatingSelector


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

-- | @- initWithRequest:@
initWithRequest :: (IsMKDirections mkDirections, IsMKDirectionsRequest request) => mkDirections -> request -> IO (Id MKDirections)
initWithRequest mkDirections  request =
withObjCPtr request $ \raw_request ->
    sendMsg mkDirections (mkSelector "initWithRequest:") (retPtr retVoid) [argPtr (castPtr raw_request :: Ptr ())] >>= ownedObject . castPtr

-- | @- calculateDirectionsWithCompletionHandler:@
calculateDirectionsWithCompletionHandler :: IsMKDirections mkDirections => mkDirections -> Ptr () -> IO ()
calculateDirectionsWithCompletionHandler mkDirections  completionHandler =
  sendMsg mkDirections (mkSelector "calculateDirectionsWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- calculateETAWithCompletionHandler:@
calculateETAWithCompletionHandler :: IsMKDirections mkDirections => mkDirections -> Ptr () -> IO ()
calculateETAWithCompletionHandler mkDirections  completionHandler =
  sendMsg mkDirections (mkSelector "calculateETAWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- cancel@
cancel :: IsMKDirections mkDirections => mkDirections -> IO ()
cancel mkDirections  =
  sendMsg mkDirections (mkSelector "cancel") retVoid []

-- | @- calculating@
calculating :: IsMKDirections mkDirections => mkDirections -> IO Bool
calculating mkDirections  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkDirections (mkSelector "calculating") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRequest:@
initWithRequestSelector :: Selector
initWithRequestSelector = mkSelector "initWithRequest:"

-- | @Selector@ for @calculateDirectionsWithCompletionHandler:@
calculateDirectionsWithCompletionHandlerSelector :: Selector
calculateDirectionsWithCompletionHandlerSelector = mkSelector "calculateDirectionsWithCompletionHandler:"

-- | @Selector@ for @calculateETAWithCompletionHandler:@
calculateETAWithCompletionHandlerSelector :: Selector
calculateETAWithCompletionHandlerSelector = mkSelector "calculateETAWithCompletionHandler:"

-- | @Selector@ for @cancel@
cancelSelector :: Selector
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @calculating@
calculatingSelector :: Selector
calculatingSelector = mkSelector "calculating"

