{-# LANGUAGE DataKinds #-}
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
  , beginUpdatesSelector
  , contextSelector
  , dataSourceSelector
  , delegateSelector
  , endUpdatesSelector
  , nowPlayingIdentifiersSelector
  , reloadDataSelector
  , setDataSourceSelector
  , setDelegateSelector
  , setNowPlayingIdentifiersSelector
  , sharedContentManagerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' sharedContentManagerSelector

-- | Tells the content manager that the data source has changed and that we need to reload data from the data source.
--
-- ObjC selector: @- reloadData@
reloadData :: IsMPPlayableContentManager mpPlayableContentManager => mpPlayableContentManager -> IO ()
reloadData mpPlayableContentManager =
  sendMessage mpPlayableContentManager reloadDataSelector

-- | Used to begin a synchronized update to multiple MPContentItems at once.
--
-- ObjC selector: @- beginUpdates@
beginUpdates :: IsMPPlayableContentManager mpPlayableContentManager => mpPlayableContentManager -> IO ()
beginUpdates mpPlayableContentManager =
  sendMessage mpPlayableContentManager beginUpdatesSelector

-- | Ends a synchronized update.
--
-- ObjC selector: @- endUpdates@
endUpdates :: IsMPPlayableContentManager mpPlayableContentManager => mpPlayableContentManager -> IO ()
endUpdates mpPlayableContentManager =
  sendMessage mpPlayableContentManager endUpdatesSelector

-- | @- dataSource@
dataSource :: IsMPPlayableContentManager mpPlayableContentManager => mpPlayableContentManager -> IO RawId
dataSource mpPlayableContentManager =
  sendMessage mpPlayableContentManager dataSourceSelector

-- | @- setDataSource:@
setDataSource :: IsMPPlayableContentManager mpPlayableContentManager => mpPlayableContentManager -> RawId -> IO ()
setDataSource mpPlayableContentManager value =
  sendMessage mpPlayableContentManager setDataSourceSelector value

-- | @- delegate@
delegate :: IsMPPlayableContentManager mpPlayableContentManager => mpPlayableContentManager -> IO RawId
delegate mpPlayableContentManager =
  sendMessage mpPlayableContentManager delegateSelector

-- | @- setDelegate:@
setDelegate :: IsMPPlayableContentManager mpPlayableContentManager => mpPlayableContentManager -> RawId -> IO ()
setDelegate mpPlayableContentManager value =
  sendMessage mpPlayableContentManager setDelegateSelector value

-- | @- context@
context :: IsMPPlayableContentManager mpPlayableContentManager => mpPlayableContentManager -> IO (Id MPPlayableContentManagerContext)
context mpPlayableContentManager =
  sendMessage mpPlayableContentManager contextSelector

-- | Tells the content manager which MPContentItems are currently playing based on their identifiers.
--
-- ObjC selector: @- nowPlayingIdentifiers@
nowPlayingIdentifiers :: IsMPPlayableContentManager mpPlayableContentManager => mpPlayableContentManager -> IO (Id NSArray)
nowPlayingIdentifiers mpPlayableContentManager =
  sendMessage mpPlayableContentManager nowPlayingIdentifiersSelector

-- | Tells the content manager which MPContentItems are currently playing based on their identifiers.
--
-- ObjC selector: @- setNowPlayingIdentifiers:@
setNowPlayingIdentifiers :: (IsMPPlayableContentManager mpPlayableContentManager, IsNSArray value) => mpPlayableContentManager -> value -> IO ()
setNowPlayingIdentifiers mpPlayableContentManager value =
  sendMessage mpPlayableContentManager setNowPlayingIdentifiersSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedContentManager@
sharedContentManagerSelector :: Selector '[] (Id MPPlayableContentManager)
sharedContentManagerSelector = mkSelector "sharedContentManager"

-- | @Selector@ for @reloadData@
reloadDataSelector :: Selector '[] ()
reloadDataSelector = mkSelector "reloadData"

-- | @Selector@ for @beginUpdates@
beginUpdatesSelector :: Selector '[] ()
beginUpdatesSelector = mkSelector "beginUpdates"

-- | @Selector@ for @endUpdates@
endUpdatesSelector :: Selector '[] ()
endUpdatesSelector = mkSelector "endUpdates"

-- | @Selector@ for @dataSource@
dataSourceSelector :: Selector '[] RawId
dataSourceSelector = mkSelector "dataSource"

-- | @Selector@ for @setDataSource:@
setDataSourceSelector :: Selector '[RawId] ()
setDataSourceSelector = mkSelector "setDataSource:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @context@
contextSelector :: Selector '[] (Id MPPlayableContentManagerContext)
contextSelector = mkSelector "context"

-- | @Selector@ for @nowPlayingIdentifiers@
nowPlayingIdentifiersSelector :: Selector '[] (Id NSArray)
nowPlayingIdentifiersSelector = mkSelector "nowPlayingIdentifiers"

-- | @Selector@ for @setNowPlayingIdentifiers:@
setNowPlayingIdentifiersSelector :: Selector '[Id NSArray] ()
setNowPlayingIdentifiersSelector = mkSelector "setNowPlayingIdentifiers:"

