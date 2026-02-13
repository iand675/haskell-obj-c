{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An overlay configuration that can be used to show an app clip's full app.
--
-- Generated bindings for @SKOverlayAppClipConfiguration@.
module ObjC.StoreKit.SKOverlayAppClipConfiguration
  ( SKOverlayAppClipConfiguration
  , IsSKOverlayAppClipConfiguration(..)
  , init_
  , new
  , initWithPosition
  , setAdditionalValue_forKey
  , additionalValueForKey
  , campaignToken
  , setCampaignToken
  , providerToken
  , setProviderToken
  , position
  , setPosition
  , additionalValueForKeySelector
  , campaignTokenSelector
  , initSelector
  , initWithPositionSelector
  , newSelector
  , positionSelector
  , providerTokenSelector
  , setAdditionalValue_forKeySelector
  , setCampaignTokenSelector
  , setPositionSelector
  , setProviderTokenSelector

  -- * Enum types
  , SKOverlayPosition(SKOverlayPosition)
  , pattern SKOverlayPositionBottom
  , pattern SKOverlayPositionBottomRaised

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.StoreKit.Internal.Classes
import ObjC.StoreKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSKOverlayAppClipConfiguration skOverlayAppClipConfiguration => skOverlayAppClipConfiguration -> IO (Id SKOverlayAppClipConfiguration)
init_ skOverlayAppClipConfiguration =
  sendOwnedMessage skOverlayAppClipConfiguration initSelector

-- | @+ new@
new :: IO (Id SKOverlayAppClipConfiguration)
new  =
  do
    cls' <- getRequiredClass "SKOverlayAppClipConfiguration"
    sendOwnedClassMessage cls' newSelector

-- | Creates a new app overlay configuration that will show an app clip's full app.
--
-- @position@ — the desired position of the overlay.
--
-- ObjC selector: @- initWithPosition:@
initWithPosition :: IsSKOverlayAppClipConfiguration skOverlayAppClipConfiguration => skOverlayAppClipConfiguration -> SKOverlayPosition -> IO (Id SKOverlayAppClipConfiguration)
initWithPosition skOverlayAppClipConfiguration position =
  sendOwnedMessage skOverlayAppClipConfiguration initWithPositionSelector position

-- | @- setAdditionalValue:forKey:@
setAdditionalValue_forKey :: (IsSKOverlayAppClipConfiguration skOverlayAppClipConfiguration, IsNSString key) => skOverlayAppClipConfiguration -> RawId -> key -> IO ()
setAdditionalValue_forKey skOverlayAppClipConfiguration value key =
  sendMessage skOverlayAppClipConfiguration setAdditionalValue_forKeySelector value (toNSString key)

-- | @- additionalValueForKey:@
additionalValueForKey :: (IsSKOverlayAppClipConfiguration skOverlayAppClipConfiguration, IsNSString key) => skOverlayAppClipConfiguration -> key -> IO RawId
additionalValueForKey skOverlayAppClipConfiguration key =
  sendMessage skOverlayAppClipConfiguration additionalValueForKeySelector (toNSString key)

-- | A token representing an App Analytics campaign.
--
-- ObjC selector: @- campaignToken@
campaignToken :: IsSKOverlayAppClipConfiguration skOverlayAppClipConfiguration => skOverlayAppClipConfiguration -> IO (Id NSString)
campaignToken skOverlayAppClipConfiguration =
  sendMessage skOverlayAppClipConfiguration campaignTokenSelector

-- | A token representing an App Analytics campaign.
--
-- ObjC selector: @- setCampaignToken:@
setCampaignToken :: (IsSKOverlayAppClipConfiguration skOverlayAppClipConfiguration, IsNSString value) => skOverlayAppClipConfiguration -> value -> IO ()
setCampaignToken skOverlayAppClipConfiguration value =
  sendMessage skOverlayAppClipConfiguration setCampaignTokenSelector (toNSString value)

-- | The provider token for the developer that created the app being presented.
--
-- ObjC selector: @- providerToken@
providerToken :: IsSKOverlayAppClipConfiguration skOverlayAppClipConfiguration => skOverlayAppClipConfiguration -> IO (Id NSString)
providerToken skOverlayAppClipConfiguration =
  sendMessage skOverlayAppClipConfiguration providerTokenSelector

-- | The provider token for the developer that created the app being presented.
--
-- ObjC selector: @- setProviderToken:@
setProviderToken :: (IsSKOverlayAppClipConfiguration skOverlayAppClipConfiguration, IsNSString value) => skOverlayAppClipConfiguration -> value -> IO ()
setProviderToken skOverlayAppClipConfiguration value =
  sendMessage skOverlayAppClipConfiguration setProviderTokenSelector (toNSString value)

-- | The position an overlay will show at on screen.
--
-- ObjC selector: @- position@
position :: IsSKOverlayAppClipConfiguration skOverlayAppClipConfiguration => skOverlayAppClipConfiguration -> IO SKOverlayPosition
position skOverlayAppClipConfiguration =
  sendMessage skOverlayAppClipConfiguration positionSelector

-- | The position an overlay will show at on screen.
--
-- ObjC selector: @- setPosition:@
setPosition :: IsSKOverlayAppClipConfiguration skOverlayAppClipConfiguration => skOverlayAppClipConfiguration -> SKOverlayPosition -> IO ()
setPosition skOverlayAppClipConfiguration value =
  sendMessage skOverlayAppClipConfiguration setPositionSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SKOverlayAppClipConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SKOverlayAppClipConfiguration)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithPosition:@
initWithPositionSelector :: Selector '[SKOverlayPosition] (Id SKOverlayAppClipConfiguration)
initWithPositionSelector = mkSelector "initWithPosition:"

-- | @Selector@ for @setAdditionalValue:forKey:@
setAdditionalValue_forKeySelector :: Selector '[RawId, Id NSString] ()
setAdditionalValue_forKeySelector = mkSelector "setAdditionalValue:forKey:"

-- | @Selector@ for @additionalValueForKey:@
additionalValueForKeySelector :: Selector '[Id NSString] RawId
additionalValueForKeySelector = mkSelector "additionalValueForKey:"

-- | @Selector@ for @campaignToken@
campaignTokenSelector :: Selector '[] (Id NSString)
campaignTokenSelector = mkSelector "campaignToken"

-- | @Selector@ for @setCampaignToken:@
setCampaignTokenSelector :: Selector '[Id NSString] ()
setCampaignTokenSelector = mkSelector "setCampaignToken:"

-- | @Selector@ for @providerToken@
providerTokenSelector :: Selector '[] (Id NSString)
providerTokenSelector = mkSelector "providerToken"

-- | @Selector@ for @setProviderToken:@
setProviderTokenSelector :: Selector '[Id NSString] ()
setProviderTokenSelector = mkSelector "setProviderToken:"

-- | @Selector@ for @position@
positionSelector :: Selector '[] SKOverlayPosition
positionSelector = mkSelector "position"

-- | @Selector@ for @setPosition:@
setPositionSelector :: Selector '[SKOverlayPosition] ()
setPositionSelector = mkSelector "setPosition:"

