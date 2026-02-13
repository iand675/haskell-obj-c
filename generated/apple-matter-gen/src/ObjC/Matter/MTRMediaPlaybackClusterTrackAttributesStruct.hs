{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRMediaPlaybackClusterTrackAttributesStruct@.
module ObjC.Matter.MTRMediaPlaybackClusterTrackAttributesStruct
  ( MTRMediaPlaybackClusterTrackAttributesStruct
  , IsMTRMediaPlaybackClusterTrackAttributesStruct(..)
  , languageCode
  , setLanguageCode
  , displayName
  , setDisplayName
  , displayNameSelector
  , languageCodeSelector
  , setDisplayNameSelector
  , setLanguageCodeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- languageCode@
languageCode :: IsMTRMediaPlaybackClusterTrackAttributesStruct mtrMediaPlaybackClusterTrackAttributesStruct => mtrMediaPlaybackClusterTrackAttributesStruct -> IO (Id NSString)
languageCode mtrMediaPlaybackClusterTrackAttributesStruct =
  sendMessage mtrMediaPlaybackClusterTrackAttributesStruct languageCodeSelector

-- | @- setLanguageCode:@
setLanguageCode :: (IsMTRMediaPlaybackClusterTrackAttributesStruct mtrMediaPlaybackClusterTrackAttributesStruct, IsNSString value) => mtrMediaPlaybackClusterTrackAttributesStruct -> value -> IO ()
setLanguageCode mtrMediaPlaybackClusterTrackAttributesStruct value =
  sendMessage mtrMediaPlaybackClusterTrackAttributesStruct setLanguageCodeSelector (toNSString value)

-- | @- displayName@
displayName :: IsMTRMediaPlaybackClusterTrackAttributesStruct mtrMediaPlaybackClusterTrackAttributesStruct => mtrMediaPlaybackClusterTrackAttributesStruct -> IO (Id NSString)
displayName mtrMediaPlaybackClusterTrackAttributesStruct =
  sendMessage mtrMediaPlaybackClusterTrackAttributesStruct displayNameSelector

-- | @- setDisplayName:@
setDisplayName :: (IsMTRMediaPlaybackClusterTrackAttributesStruct mtrMediaPlaybackClusterTrackAttributesStruct, IsNSString value) => mtrMediaPlaybackClusterTrackAttributesStruct -> value -> IO ()
setDisplayName mtrMediaPlaybackClusterTrackAttributesStruct value =
  sendMessage mtrMediaPlaybackClusterTrackAttributesStruct setDisplayNameSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @languageCode@
languageCodeSelector :: Selector '[] (Id NSString)
languageCodeSelector = mkSelector "languageCode"

-- | @Selector@ for @setLanguageCode:@
setLanguageCodeSelector :: Selector '[Id NSString] ()
setLanguageCodeSelector = mkSelector "setLanguageCode:"

-- | @Selector@ for @displayName@
displayNameSelector :: Selector '[] (Id NSString)
displayNameSelector = mkSelector "displayName"

-- | @Selector@ for @setDisplayName:@
setDisplayNameSelector :: Selector '[Id NSString] ()
setDisplayNameSelector = mkSelector "setDisplayName:"

