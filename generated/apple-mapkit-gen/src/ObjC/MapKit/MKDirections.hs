{-# LANGUAGE DataKinds #-}
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
  , calculateDirectionsWithCompletionHandlerSelector
  , calculateETAWithCompletionHandlerSelector
  , calculatingSelector
  , cancelSelector
  , initWithRequestSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithRequest:@
initWithRequest :: (IsMKDirections mkDirections, IsMKDirectionsRequest request) => mkDirections -> request -> IO (Id MKDirections)
initWithRequest mkDirections request =
  sendOwnedMessage mkDirections initWithRequestSelector (toMKDirectionsRequest request)

-- | @- calculateDirectionsWithCompletionHandler:@
calculateDirectionsWithCompletionHandler :: IsMKDirections mkDirections => mkDirections -> Ptr () -> IO ()
calculateDirectionsWithCompletionHandler mkDirections completionHandler =
  sendMessage mkDirections calculateDirectionsWithCompletionHandlerSelector completionHandler

-- | @- calculateETAWithCompletionHandler:@
calculateETAWithCompletionHandler :: IsMKDirections mkDirections => mkDirections -> Ptr () -> IO ()
calculateETAWithCompletionHandler mkDirections completionHandler =
  sendMessage mkDirections calculateETAWithCompletionHandlerSelector completionHandler

-- | @- cancel@
cancel :: IsMKDirections mkDirections => mkDirections -> IO ()
cancel mkDirections =
  sendMessage mkDirections cancelSelector

-- | @- calculating@
calculating :: IsMKDirections mkDirections => mkDirections -> IO Bool
calculating mkDirections =
  sendMessage mkDirections calculatingSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRequest:@
initWithRequestSelector :: Selector '[Id MKDirectionsRequest] (Id MKDirections)
initWithRequestSelector = mkSelector "initWithRequest:"

-- | @Selector@ for @calculateDirectionsWithCompletionHandler:@
calculateDirectionsWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
calculateDirectionsWithCompletionHandlerSelector = mkSelector "calculateDirectionsWithCompletionHandler:"

-- | @Selector@ for @calculateETAWithCompletionHandler:@
calculateETAWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
calculateETAWithCompletionHandlerSelector = mkSelector "calculateETAWithCompletionHandler:"

-- | @Selector@ for @cancel@
cancelSelector :: Selector '[] ()
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @calculating@
calculatingSelector :: Selector '[] Bool
calculatingSelector = mkSelector "calculating"

