{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRMediaPlaybackClusterTrackStruct@.
module ObjC.Matter.MTRMediaPlaybackClusterTrackStruct
  ( MTRMediaPlaybackClusterTrackStruct
  , IsMTRMediaPlaybackClusterTrackStruct(..)
  , id_
  , setId
  , trackAttributes
  , setTrackAttributes
  , idSelector
  , setIdSelector
  , setTrackAttributesSelector
  , trackAttributesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- id@
id_ :: IsMTRMediaPlaybackClusterTrackStruct mtrMediaPlaybackClusterTrackStruct => mtrMediaPlaybackClusterTrackStruct -> IO (Id NSString)
id_ mtrMediaPlaybackClusterTrackStruct =
  sendMessage mtrMediaPlaybackClusterTrackStruct idSelector

-- | @- setId:@
setId :: (IsMTRMediaPlaybackClusterTrackStruct mtrMediaPlaybackClusterTrackStruct, IsNSString value) => mtrMediaPlaybackClusterTrackStruct -> value -> IO ()
setId mtrMediaPlaybackClusterTrackStruct value =
  sendMessage mtrMediaPlaybackClusterTrackStruct setIdSelector (toNSString value)

-- | @- trackAttributes@
trackAttributes :: IsMTRMediaPlaybackClusterTrackStruct mtrMediaPlaybackClusterTrackStruct => mtrMediaPlaybackClusterTrackStruct -> IO (Id MTRMediaPlaybackClusterTrackAttributesStruct)
trackAttributes mtrMediaPlaybackClusterTrackStruct =
  sendMessage mtrMediaPlaybackClusterTrackStruct trackAttributesSelector

-- | @- setTrackAttributes:@
setTrackAttributes :: (IsMTRMediaPlaybackClusterTrackStruct mtrMediaPlaybackClusterTrackStruct, IsMTRMediaPlaybackClusterTrackAttributesStruct value) => mtrMediaPlaybackClusterTrackStruct -> value -> IO ()
setTrackAttributes mtrMediaPlaybackClusterTrackStruct value =
  sendMessage mtrMediaPlaybackClusterTrackStruct setTrackAttributesSelector (toMTRMediaPlaybackClusterTrackAttributesStruct value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @id@
idSelector :: Selector '[] (Id NSString)
idSelector = mkSelector "id"

-- | @Selector@ for @setId:@
setIdSelector :: Selector '[Id NSString] ()
setIdSelector = mkSelector "setId:"

-- | @Selector@ for @trackAttributes@
trackAttributesSelector :: Selector '[] (Id MTRMediaPlaybackClusterTrackAttributesStruct)
trackAttributesSelector = mkSelector "trackAttributes"

-- | @Selector@ for @setTrackAttributes:@
setTrackAttributesSelector :: Selector '[Id MTRMediaPlaybackClusterTrackAttributesStruct] ()
setTrackAttributesSelector = mkSelector "setTrackAttributes:"

