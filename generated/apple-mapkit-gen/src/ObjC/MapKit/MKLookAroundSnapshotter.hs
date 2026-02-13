{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKLookAroundSnapshotter@.
module ObjC.MapKit.MKLookAroundSnapshotter
  ( MKLookAroundSnapshotter
  , IsMKLookAroundSnapshotter(..)
  , new
  , init_
  , initWithScene_options
  , getSnapshotWithCompletionHandler
  , cancel
  , loading
  , cancelSelector
  , getSnapshotWithCompletionHandlerSelector
  , initSelector
  , initWithScene_optionsSelector
  , loadingSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MKLookAroundSnapshotter)
new  =
  do
    cls' <- getRequiredClass "MKLookAroundSnapshotter"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsMKLookAroundSnapshotter mkLookAroundSnapshotter => mkLookAroundSnapshotter -> IO (Id MKLookAroundSnapshotter)
init_ mkLookAroundSnapshotter =
  sendOwnedMessage mkLookAroundSnapshotter initSelector

-- | @- initWithScene:options:@
initWithScene_options :: (IsMKLookAroundSnapshotter mkLookAroundSnapshotter, IsMKLookAroundScene scene, IsMKLookAroundSnapshotOptions options) => mkLookAroundSnapshotter -> scene -> options -> IO (Id MKLookAroundSnapshotter)
initWithScene_options mkLookAroundSnapshotter scene options =
  sendOwnedMessage mkLookAroundSnapshotter initWithScene_optionsSelector (toMKLookAroundScene scene) (toMKLookAroundSnapshotOptions options)

-- | @- getSnapshotWithCompletionHandler:@
getSnapshotWithCompletionHandler :: IsMKLookAroundSnapshotter mkLookAroundSnapshotter => mkLookAroundSnapshotter -> Ptr () -> IO ()
getSnapshotWithCompletionHandler mkLookAroundSnapshotter completionHandler =
  sendMessage mkLookAroundSnapshotter getSnapshotWithCompletionHandlerSelector completionHandler

-- | @- cancel@
cancel :: IsMKLookAroundSnapshotter mkLookAroundSnapshotter => mkLookAroundSnapshotter -> IO ()
cancel mkLookAroundSnapshotter =
  sendMessage mkLookAroundSnapshotter cancelSelector

-- | @- loading@
loading :: IsMKLookAroundSnapshotter mkLookAroundSnapshotter => mkLookAroundSnapshotter -> IO Bool
loading mkLookAroundSnapshotter =
  sendMessage mkLookAroundSnapshotter loadingSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MKLookAroundSnapshotter)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MKLookAroundSnapshotter)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithScene:options:@
initWithScene_optionsSelector :: Selector '[Id MKLookAroundScene, Id MKLookAroundSnapshotOptions] (Id MKLookAroundSnapshotter)
initWithScene_optionsSelector = mkSelector "initWithScene:options:"

-- | @Selector@ for @getSnapshotWithCompletionHandler:@
getSnapshotWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
getSnapshotWithCompletionHandlerSelector = mkSelector "getSnapshotWithCompletionHandler:"

-- | @Selector@ for @cancel@
cancelSelector :: Selector '[] ()
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @loading@
loadingSelector :: Selector '[] Bool
loadingSelector = mkSelector "loading"

