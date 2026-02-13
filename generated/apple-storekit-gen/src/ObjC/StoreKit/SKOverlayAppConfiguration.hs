{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An overlay configuration that can be used to show any app from the App Store.
--
-- Generated bindings for @SKOverlayAppConfiguration@.
module ObjC.StoreKit.SKOverlayAppConfiguration
  ( SKOverlayAppConfiguration
  , IsSKOverlayAppConfiguration(..)
  , init_
  , new
  , initWithAppIdentifier_position
  , setAdditionalValue_forKey
  , additionalValueForKey
  , setAdImpression
  , appIdentifier
  , setAppIdentifier
  , campaignToken
  , setCampaignToken
  , providerToken
  , setProviderToken
  , position
  , setPosition
  , userDismissible
  , setUserDismissible
  , additionalValueForKeySelector
  , appIdentifierSelector
  , campaignTokenSelector
  , initSelector
  , initWithAppIdentifier_positionSelector
  , newSelector
  , positionSelector
  , providerTokenSelector
  , setAdImpressionSelector
  , setAdditionalValue_forKeySelector
  , setAppIdentifierSelector
  , setCampaignTokenSelector
  , setPositionSelector
  , setProviderTokenSelector
  , setUserDismissibleSelector
  , userDismissibleSelector

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
init_ :: IsSKOverlayAppConfiguration skOverlayAppConfiguration => skOverlayAppConfiguration -> IO (Id SKOverlayAppConfiguration)
init_ skOverlayAppConfiguration =
  sendOwnedMessage skOverlayAppConfiguration initSelector

-- | @+ new@
new :: IO (Id SKOverlayAppConfiguration)
new  =
  do
    cls' <- getRequiredClass "SKOverlayAppConfiguration"
    sendOwnedClassMessage cls' newSelector

-- | Creates a new app overlay configuration that will show an app from the App Store.
--
-- @appIdentifier@ — the app identifier of the app to show.
--
-- @position@ — the desired position of the overlay.
--
-- ObjC selector: @- initWithAppIdentifier:position:@
initWithAppIdentifier_position :: (IsSKOverlayAppConfiguration skOverlayAppConfiguration, IsNSString appIdentifier) => skOverlayAppConfiguration -> appIdentifier -> SKOverlayPosition -> IO (Id SKOverlayAppConfiguration)
initWithAppIdentifier_position skOverlayAppConfiguration appIdentifier position =
  sendOwnedMessage skOverlayAppConfiguration initWithAppIdentifier_positionSelector (toNSString appIdentifier) position

-- | @- setAdditionalValue:forKey:@
setAdditionalValue_forKey :: (IsSKOverlayAppConfiguration skOverlayAppConfiguration, IsNSString key) => skOverlayAppConfiguration -> RawId -> key -> IO ()
setAdditionalValue_forKey skOverlayAppConfiguration value key =
  sendMessage skOverlayAppConfiguration setAdditionalValue_forKeySelector value (toNSString key)

-- | @- additionalValueForKey:@
additionalValueForKey :: (IsSKOverlayAppConfiguration skOverlayAppConfiguration, IsNSString key) => skOverlayAppConfiguration -> key -> IO RawId
additionalValueForKey skOverlayAppConfiguration key =
  sendMessage skOverlayAppConfiguration additionalValueForKeySelector (toNSString key)

-- | @- setAdImpression:@
setAdImpression :: (IsSKOverlayAppConfiguration skOverlayAppConfiguration, IsSKAdImpression impression) => skOverlayAppConfiguration -> impression -> IO ()
setAdImpression skOverlayAppConfiguration impression =
  sendMessage skOverlayAppConfiguration setAdImpressionSelector (toSKAdImpression impression)

-- | The identifier of the app that will be shown.
--
-- ObjC selector: @- appIdentifier@
appIdentifier :: IsSKOverlayAppConfiguration skOverlayAppConfiguration => skOverlayAppConfiguration -> IO (Id NSString)
appIdentifier skOverlayAppConfiguration =
  sendMessage skOverlayAppConfiguration appIdentifierSelector

-- | The identifier of the app that will be shown.
--
-- ObjC selector: @- setAppIdentifier:@
setAppIdentifier :: (IsSKOverlayAppConfiguration skOverlayAppConfiguration, IsNSString value) => skOverlayAppConfiguration -> value -> IO ()
setAppIdentifier skOverlayAppConfiguration value =
  sendMessage skOverlayAppConfiguration setAppIdentifierSelector (toNSString value)

-- | A token representing an App Analytics campaign.
--
-- ObjC selector: @- campaignToken@
campaignToken :: IsSKOverlayAppConfiguration skOverlayAppConfiguration => skOverlayAppConfiguration -> IO (Id NSString)
campaignToken skOverlayAppConfiguration =
  sendMessage skOverlayAppConfiguration campaignTokenSelector

-- | A token representing an App Analytics campaign.
--
-- ObjC selector: @- setCampaignToken:@
setCampaignToken :: (IsSKOverlayAppConfiguration skOverlayAppConfiguration, IsNSString value) => skOverlayAppConfiguration -> value -> IO ()
setCampaignToken skOverlayAppConfiguration value =
  sendMessage skOverlayAppConfiguration setCampaignTokenSelector (toNSString value)

-- | The provider token for the developer that created the app being presented.
--
-- ObjC selector: @- providerToken@
providerToken :: IsSKOverlayAppConfiguration skOverlayAppConfiguration => skOverlayAppConfiguration -> IO (Id NSString)
providerToken skOverlayAppConfiguration =
  sendMessage skOverlayAppConfiguration providerTokenSelector

-- | The provider token for the developer that created the app being presented.
--
-- ObjC selector: @- setProviderToken:@
setProviderToken :: (IsSKOverlayAppConfiguration skOverlayAppConfiguration, IsNSString value) => skOverlayAppConfiguration -> value -> IO ()
setProviderToken skOverlayAppConfiguration value =
  sendMessage skOverlayAppConfiguration setProviderTokenSelector (toNSString value)

-- | The position an overlay will show at on screen.
--
-- ObjC selector: @- position@
position :: IsSKOverlayAppConfiguration skOverlayAppConfiguration => skOverlayAppConfiguration -> IO SKOverlayPosition
position skOverlayAppConfiguration =
  sendMessage skOverlayAppConfiguration positionSelector

-- | The position an overlay will show at on screen.
--
-- ObjC selector: @- setPosition:@
setPosition :: IsSKOverlayAppConfiguration skOverlayAppConfiguration => skOverlayAppConfiguration -> SKOverlayPosition -> IO ()
setPosition skOverlayAppConfiguration value =
  sendMessage skOverlayAppConfiguration setPositionSelector value

-- | Allows the user to interactively dismiss an overlay.
--
-- ObjC selector: @- userDismissible@
userDismissible :: IsSKOverlayAppConfiguration skOverlayAppConfiguration => skOverlayAppConfiguration -> IO Bool
userDismissible skOverlayAppConfiguration =
  sendMessage skOverlayAppConfiguration userDismissibleSelector

-- | Allows the user to interactively dismiss an overlay.
--
-- ObjC selector: @- setUserDismissible:@
setUserDismissible :: IsSKOverlayAppConfiguration skOverlayAppConfiguration => skOverlayAppConfiguration -> Bool -> IO ()
setUserDismissible skOverlayAppConfiguration value =
  sendMessage skOverlayAppConfiguration setUserDismissibleSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SKOverlayAppConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SKOverlayAppConfiguration)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithAppIdentifier:position:@
initWithAppIdentifier_positionSelector :: Selector '[Id NSString, SKOverlayPosition] (Id SKOverlayAppConfiguration)
initWithAppIdentifier_positionSelector = mkSelector "initWithAppIdentifier:position:"

-- | @Selector@ for @setAdditionalValue:forKey:@
setAdditionalValue_forKeySelector :: Selector '[RawId, Id NSString] ()
setAdditionalValue_forKeySelector = mkSelector "setAdditionalValue:forKey:"

-- | @Selector@ for @additionalValueForKey:@
additionalValueForKeySelector :: Selector '[Id NSString] RawId
additionalValueForKeySelector = mkSelector "additionalValueForKey:"

-- | @Selector@ for @setAdImpression:@
setAdImpressionSelector :: Selector '[Id SKAdImpression] ()
setAdImpressionSelector = mkSelector "setAdImpression:"

-- | @Selector@ for @appIdentifier@
appIdentifierSelector :: Selector '[] (Id NSString)
appIdentifierSelector = mkSelector "appIdentifier"

-- | @Selector@ for @setAppIdentifier:@
setAppIdentifierSelector :: Selector '[Id NSString] ()
setAppIdentifierSelector = mkSelector "setAppIdentifier:"

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

-- | @Selector@ for @userDismissible@
userDismissibleSelector :: Selector '[] Bool
userDismissibleSelector = mkSelector "userDismissible"

-- | @Selector@ for @setUserDismissible:@
setUserDismissibleSelector :: Selector '[Bool] ()
setUserDismissibleSelector = mkSelector "setUserDismissible:"

