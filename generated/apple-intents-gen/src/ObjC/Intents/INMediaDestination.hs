{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INMediaDestination@.
module ObjC.Intents.INMediaDestination
  ( INMediaDestination
  , IsINMediaDestination(..)
  , init_
  , libraryDestination
  , playlistDestinationWithName
  , mediaDestinationType
  , playlistName
  , initSelector
  , libraryDestinationSelector
  , mediaDestinationTypeSelector
  , playlistDestinationWithNameSelector
  , playlistNameSelector

  -- * Enum types
  , INMediaDestinationType(INMediaDestinationType)
  , pattern INMediaDestinationTypeUnknown
  , pattern INMediaDestinationTypeLibrary
  , pattern INMediaDestinationTypePlaylist

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINMediaDestination inMediaDestination => inMediaDestination -> IO (Id INMediaDestination)
init_ inMediaDestination =
  sendOwnedMessage inMediaDestination initSelector

-- | @+ libraryDestination@
libraryDestination :: IO (Id INMediaDestination)
libraryDestination  =
  do
    cls' <- getRequiredClass "INMediaDestination"
    sendClassMessage cls' libraryDestinationSelector

-- | @+ playlistDestinationWithName:@
playlistDestinationWithName :: IsNSString playlistName => playlistName -> IO (Id INMediaDestination)
playlistDestinationWithName playlistName =
  do
    cls' <- getRequiredClass "INMediaDestination"
    sendClassMessage cls' playlistDestinationWithNameSelector (toNSString playlistName)

-- | @- mediaDestinationType@
mediaDestinationType :: IsINMediaDestination inMediaDestination => inMediaDestination -> IO INMediaDestinationType
mediaDestinationType inMediaDestination =
  sendMessage inMediaDestination mediaDestinationTypeSelector

-- | @- playlistName@
playlistName :: IsINMediaDestination inMediaDestination => inMediaDestination -> IO (Id NSString)
playlistName inMediaDestination =
  sendMessage inMediaDestination playlistNameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INMediaDestination)
initSelector = mkSelector "init"

-- | @Selector@ for @libraryDestination@
libraryDestinationSelector :: Selector '[] (Id INMediaDestination)
libraryDestinationSelector = mkSelector "libraryDestination"

-- | @Selector@ for @playlistDestinationWithName:@
playlistDestinationWithNameSelector :: Selector '[Id NSString] (Id INMediaDestination)
playlistDestinationWithNameSelector = mkSelector "playlistDestinationWithName:"

-- | @Selector@ for @mediaDestinationType@
mediaDestinationTypeSelector :: Selector '[] INMediaDestinationType
mediaDestinationTypeSelector = mkSelector "mediaDestinationType"

-- | @Selector@ for @playlistName@
playlistNameSelector :: Selector '[] (Id NSString)
playlistNameSelector = mkSelector "playlistName"

