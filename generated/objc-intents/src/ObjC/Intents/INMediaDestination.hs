{-# LANGUAGE PatternSynonyms #-}
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
  , playlistDestinationWithNameSelector
  , mediaDestinationTypeSelector
  , playlistNameSelector

  -- * Enum types
  , INMediaDestinationType(INMediaDestinationType)
  , pattern INMediaDestinationTypeUnknown
  , pattern INMediaDestinationTypeLibrary
  , pattern INMediaDestinationTypePlaylist

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

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINMediaDestination inMediaDestination => inMediaDestination -> IO (Id INMediaDestination)
init_ inMediaDestination  =
  sendMsg inMediaDestination (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ libraryDestination@
libraryDestination :: IO (Id INMediaDestination)
libraryDestination  =
  do
    cls' <- getRequiredClass "INMediaDestination"
    sendClassMsg cls' (mkSelector "libraryDestination") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ playlistDestinationWithName:@
playlistDestinationWithName :: IsNSString playlistName => playlistName -> IO (Id INMediaDestination)
playlistDestinationWithName playlistName =
  do
    cls' <- getRequiredClass "INMediaDestination"
    withObjCPtr playlistName $ \raw_playlistName ->
      sendClassMsg cls' (mkSelector "playlistDestinationWithName:") (retPtr retVoid) [argPtr (castPtr raw_playlistName :: Ptr ())] >>= retainedObject . castPtr

-- | @- mediaDestinationType@
mediaDestinationType :: IsINMediaDestination inMediaDestination => inMediaDestination -> IO INMediaDestinationType
mediaDestinationType inMediaDestination  =
  fmap (coerce :: CLong -> INMediaDestinationType) $ sendMsg inMediaDestination (mkSelector "mediaDestinationType") retCLong []

-- | @- playlistName@
playlistName :: IsINMediaDestination inMediaDestination => inMediaDestination -> IO (Id NSString)
playlistName inMediaDestination  =
  sendMsg inMediaDestination (mkSelector "playlistName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @libraryDestination@
libraryDestinationSelector :: Selector
libraryDestinationSelector = mkSelector "libraryDestination"

-- | @Selector@ for @playlistDestinationWithName:@
playlistDestinationWithNameSelector :: Selector
playlistDestinationWithNameSelector = mkSelector "playlistDestinationWithName:"

-- | @Selector@ for @mediaDestinationType@
mediaDestinationTypeSelector :: Selector
mediaDestinationTypeSelector = mkSelector "mediaDestinationType"

-- | @Selector@ for @playlistName@
playlistNameSelector :: Selector
playlistNameSelector = mkSelector "playlistName"

