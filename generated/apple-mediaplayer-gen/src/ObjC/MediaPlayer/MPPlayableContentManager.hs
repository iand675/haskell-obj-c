{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPPlayableContentManager is a class that manages the interactions between a media application and an external media player interface. The application provides the content manager with a data source, which allows the media player to browse the media content offered by the application, as well as a delegate, which allows the media player to relay non-media remote playback commands to the application.
--
-- Generated bindings for @MPPlayableContentManager@.
module ObjC.MediaPlayer.MPPlayableContentManager
  ( MPPlayableContentManager
  , IsMPPlayableContentManager(..)
  , sharedContentManager
  , reloadData
  , beginUpdates
  , endUpdates
  , dataSource
  , setDataSource
  , delegate
  , setDelegate
  , context
  , nowPlayingIdentifiers
  , setNowPlayingIdentifiers
  , sharedContentManagerSelector
  , reloadDataSelector
  , beginUpdatesSelector
  , endUpdatesSelector
  , dataSourceSelector
  , setDataSourceSelector
  , delegateSelector
  , setDelegateSelector
  , contextSelector
  , nowPlayingIdentifiersSelector
  , setNowPlayingIdentifiersSelector


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

import ObjC.MediaPlayer.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Returns the application's instance of the content manager.
--
-- ObjC selector: @+ sharedContentManager@
sharedContentManager :: IO (Id MPPlayableContentManager)
sharedContentManager  =
  do
    cls' <- getRequiredClass "MPPlayableContentManager"
    sendClassMsg cls' (mkSelector "sharedContentManager") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Tells the content manager that the data source has changed and that we need to reload data from the data source.
--
-- ObjC selector: @- reloadData@
reloadData :: IsMPPlayableContentManager mpPlayableContentManager => mpPlayableContentManager -> IO ()
reloadData mpPlayableContentManager  =
    sendMsg mpPlayableContentManager (mkSelector "reloadData") retVoid []

-- | Used to begin a synchronized update to multiple MPContentItems at once.
--
-- ObjC selector: @- beginUpdates@
beginUpdates :: IsMPPlayableContentManager mpPlayableContentManager => mpPlayableContentManager -> IO ()
beginUpdates mpPlayableContentManager  =
    sendMsg mpPlayableContentManager (mkSelector "beginUpdates") retVoid []

-- | Ends a synchronized update.
--
-- ObjC selector: @- endUpdates@
endUpdates :: IsMPPlayableContentManager mpPlayableContentManager => mpPlayableContentManager -> IO ()
endUpdates mpPlayableContentManager  =
    sendMsg mpPlayableContentManager (mkSelector "endUpdates") retVoid []

-- | @- dataSource@
dataSource :: IsMPPlayableContentManager mpPlayableContentManager => mpPlayableContentManager -> IO RawId
dataSource mpPlayableContentManager  =
    fmap (RawId . castPtr) $ sendMsg mpPlayableContentManager (mkSelector "dataSource") (retPtr retVoid) []

-- | @- setDataSource:@
setDataSource :: IsMPPlayableContentManager mpPlayableContentManager => mpPlayableContentManager -> RawId -> IO ()
setDataSource mpPlayableContentManager  value =
    sendMsg mpPlayableContentManager (mkSelector "setDataSource:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- delegate@
delegate :: IsMPPlayableContentManager mpPlayableContentManager => mpPlayableContentManager -> IO RawId
delegate mpPlayableContentManager  =
    fmap (RawId . castPtr) $ sendMsg mpPlayableContentManager (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsMPPlayableContentManager mpPlayableContentManager => mpPlayableContentManager -> RawId -> IO ()
setDelegate mpPlayableContentManager  value =
    sendMsg mpPlayableContentManager (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- context@
context :: IsMPPlayableContentManager mpPlayableContentManager => mpPlayableContentManager -> IO (Id MPPlayableContentManagerContext)
context mpPlayableContentManager  =
    sendMsg mpPlayableContentManager (mkSelector "context") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Tells the content manager which MPContentItems are currently playing based on their identifiers.
--
-- ObjC selector: @- nowPlayingIdentifiers@
nowPlayingIdentifiers :: IsMPPlayableContentManager mpPlayableContentManager => mpPlayableContentManager -> IO (Id NSArray)
nowPlayingIdentifiers mpPlayableContentManager  =
    sendMsg mpPlayableContentManager (mkSelector "nowPlayingIdentifiers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Tells the content manager which MPContentItems are currently playing based on their identifiers.
--
-- ObjC selector: @- setNowPlayingIdentifiers:@
setNowPlayingIdentifiers :: (IsMPPlayableContentManager mpPlayableContentManager, IsNSArray value) => mpPlayableContentManager -> value -> IO ()
setNowPlayingIdentifiers mpPlayableContentManager  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mpPlayableContentManager (mkSelector "setNowPlayingIdentifiers:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedContentManager@
sharedContentManagerSelector :: Selector
sharedContentManagerSelector = mkSelector "sharedContentManager"

-- | @Selector@ for @reloadData@
reloadDataSelector :: Selector
reloadDataSelector = mkSelector "reloadData"

-- | @Selector@ for @beginUpdates@
beginUpdatesSelector :: Selector
beginUpdatesSelector = mkSelector "beginUpdates"

-- | @Selector@ for @endUpdates@
endUpdatesSelector :: Selector
endUpdatesSelector = mkSelector "endUpdates"

-- | @Selector@ for @dataSource@
dataSourceSelector :: Selector
dataSourceSelector = mkSelector "dataSource"

-- | @Selector@ for @setDataSource:@
setDataSourceSelector :: Selector
setDataSourceSelector = mkSelector "setDataSource:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @context@
contextSelector :: Selector
contextSelector = mkSelector "context"

-- | @Selector@ for @nowPlayingIdentifiers@
nowPlayingIdentifiersSelector :: Selector
nowPlayingIdentifiersSelector = mkSelector "nowPlayingIdentifiers"

-- | @Selector@ for @setNowPlayingIdentifiers:@
setNowPlayingIdentifiersSelector :: Selector
setNowPlayingIdentifiersSelector = mkSelector "setNowPlayingIdentifiers:"

