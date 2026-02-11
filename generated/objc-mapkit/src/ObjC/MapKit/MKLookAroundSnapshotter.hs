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
  , newSelector
  , initSelector
  , initWithScene_optionsSelector
  , getSnapshotWithCompletionHandlerSelector
  , cancelSelector
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

-- | @+ new@
new :: IO (Id MKLookAroundSnapshotter)
new  =
  do
    cls' <- getRequiredClass "MKLookAroundSnapshotter"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMKLookAroundSnapshotter mkLookAroundSnapshotter => mkLookAroundSnapshotter -> IO (Id MKLookAroundSnapshotter)
init_ mkLookAroundSnapshotter  =
  sendMsg mkLookAroundSnapshotter (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithScene:options:@
initWithScene_options :: (IsMKLookAroundSnapshotter mkLookAroundSnapshotter, IsMKLookAroundScene scene, IsMKLookAroundSnapshotOptions options) => mkLookAroundSnapshotter -> scene -> options -> IO (Id MKLookAroundSnapshotter)
initWithScene_options mkLookAroundSnapshotter  scene options =
withObjCPtr scene $ \raw_scene ->
  withObjCPtr options $ \raw_options ->
      sendMsg mkLookAroundSnapshotter (mkSelector "initWithScene:options:") (retPtr retVoid) [argPtr (castPtr raw_scene :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= ownedObject . castPtr

-- | @- getSnapshotWithCompletionHandler:@
getSnapshotWithCompletionHandler :: IsMKLookAroundSnapshotter mkLookAroundSnapshotter => mkLookAroundSnapshotter -> Ptr () -> IO ()
getSnapshotWithCompletionHandler mkLookAroundSnapshotter  completionHandler =
  sendMsg mkLookAroundSnapshotter (mkSelector "getSnapshotWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- cancel@
cancel :: IsMKLookAroundSnapshotter mkLookAroundSnapshotter => mkLookAroundSnapshotter -> IO ()
cancel mkLookAroundSnapshotter  =
  sendMsg mkLookAroundSnapshotter (mkSelector "cancel") retVoid []

-- | @- loading@
loading :: IsMKLookAroundSnapshotter mkLookAroundSnapshotter => mkLookAroundSnapshotter -> IO Bool
loading mkLookAroundSnapshotter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkLookAroundSnapshotter (mkSelector "loading") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithScene:options:@
initWithScene_optionsSelector :: Selector
initWithScene_optionsSelector = mkSelector "initWithScene:options:"

-- | @Selector@ for @getSnapshotWithCompletionHandler:@
getSnapshotWithCompletionHandlerSelector :: Selector
getSnapshotWithCompletionHandlerSelector = mkSelector "getSnapshotWithCompletionHandler:"

-- | @Selector@ for @cancel@
cancelSelector :: Selector
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @loading@
loadingSelector :: Selector
loadingSelector = mkSelector "loading"

