{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , newSelector
  , initWithAppIdentifier_positionSelector
  , setAdditionalValue_forKeySelector
  , additionalValueForKeySelector
  , setAdImpressionSelector
  , appIdentifierSelector
  , setAppIdentifierSelector
  , campaignTokenSelector
  , setCampaignTokenSelector
  , providerTokenSelector
  , setProviderTokenSelector
  , positionSelector
  , setPositionSelector
  , userDismissibleSelector
  , setUserDismissibleSelector

  -- * Enum types
  , SKOverlayPosition(SKOverlayPosition)
  , pattern SKOverlayPositionBottom
  , pattern SKOverlayPositionBottomRaised

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

import ObjC.StoreKit.Internal.Classes
import ObjC.StoreKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSKOverlayAppConfiguration skOverlayAppConfiguration => skOverlayAppConfiguration -> IO (Id SKOverlayAppConfiguration)
init_ skOverlayAppConfiguration  =
  sendMsg skOverlayAppConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SKOverlayAppConfiguration)
new  =
  do
    cls' <- getRequiredClass "SKOverlayAppConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Creates a new app overlay configuration that will show an app from the App Store.
--
-- @appIdentifier@ — the app identifier of the app to show.
--
-- @position@ — the desired position of the overlay.
--
-- ObjC selector: @- initWithAppIdentifier:position:@
initWithAppIdentifier_position :: (IsSKOverlayAppConfiguration skOverlayAppConfiguration, IsNSString appIdentifier) => skOverlayAppConfiguration -> appIdentifier -> SKOverlayPosition -> IO (Id SKOverlayAppConfiguration)
initWithAppIdentifier_position skOverlayAppConfiguration  appIdentifier position =
withObjCPtr appIdentifier $ \raw_appIdentifier ->
    sendMsg skOverlayAppConfiguration (mkSelector "initWithAppIdentifier:position:") (retPtr retVoid) [argPtr (castPtr raw_appIdentifier :: Ptr ()), argCLong (coerce position)] >>= ownedObject . castPtr

-- | @- setAdditionalValue:forKey:@
setAdditionalValue_forKey :: (IsSKOverlayAppConfiguration skOverlayAppConfiguration, IsNSString key) => skOverlayAppConfiguration -> RawId -> key -> IO ()
setAdditionalValue_forKey skOverlayAppConfiguration  value key =
withObjCPtr key $ \raw_key ->
    sendMsg skOverlayAppConfiguration (mkSelector "setAdditionalValue:forKey:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | @- additionalValueForKey:@
additionalValueForKey :: (IsSKOverlayAppConfiguration skOverlayAppConfiguration, IsNSString key) => skOverlayAppConfiguration -> key -> IO RawId
additionalValueForKey skOverlayAppConfiguration  key =
withObjCPtr key $ \raw_key ->
    fmap (RawId . castPtr) $ sendMsg skOverlayAppConfiguration (mkSelector "additionalValueForKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())]

-- | @- setAdImpression:@
setAdImpression :: (IsSKOverlayAppConfiguration skOverlayAppConfiguration, IsSKAdImpression impression) => skOverlayAppConfiguration -> impression -> IO ()
setAdImpression skOverlayAppConfiguration  impression =
withObjCPtr impression $ \raw_impression ->
    sendMsg skOverlayAppConfiguration (mkSelector "setAdImpression:") retVoid [argPtr (castPtr raw_impression :: Ptr ())]

-- | The identifier of the app that will be shown.
--
-- ObjC selector: @- appIdentifier@
appIdentifier :: IsSKOverlayAppConfiguration skOverlayAppConfiguration => skOverlayAppConfiguration -> IO (Id NSString)
appIdentifier skOverlayAppConfiguration  =
  sendMsg skOverlayAppConfiguration (mkSelector "appIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The identifier of the app that will be shown.
--
-- ObjC selector: @- setAppIdentifier:@
setAppIdentifier :: (IsSKOverlayAppConfiguration skOverlayAppConfiguration, IsNSString value) => skOverlayAppConfiguration -> value -> IO ()
setAppIdentifier skOverlayAppConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg skOverlayAppConfiguration (mkSelector "setAppIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | A token representing an App Analytics campaign.
--
-- ObjC selector: @- campaignToken@
campaignToken :: IsSKOverlayAppConfiguration skOverlayAppConfiguration => skOverlayAppConfiguration -> IO (Id NSString)
campaignToken skOverlayAppConfiguration  =
  sendMsg skOverlayAppConfiguration (mkSelector "campaignToken") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A token representing an App Analytics campaign.
--
-- ObjC selector: @- setCampaignToken:@
setCampaignToken :: (IsSKOverlayAppConfiguration skOverlayAppConfiguration, IsNSString value) => skOverlayAppConfiguration -> value -> IO ()
setCampaignToken skOverlayAppConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg skOverlayAppConfiguration (mkSelector "setCampaignToken:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The provider token for the developer that created the app being presented.
--
-- ObjC selector: @- providerToken@
providerToken :: IsSKOverlayAppConfiguration skOverlayAppConfiguration => skOverlayAppConfiguration -> IO (Id NSString)
providerToken skOverlayAppConfiguration  =
  sendMsg skOverlayAppConfiguration (mkSelector "providerToken") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The provider token for the developer that created the app being presented.
--
-- ObjC selector: @- setProviderToken:@
setProviderToken :: (IsSKOverlayAppConfiguration skOverlayAppConfiguration, IsNSString value) => skOverlayAppConfiguration -> value -> IO ()
setProviderToken skOverlayAppConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg skOverlayAppConfiguration (mkSelector "setProviderToken:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The position an overlay will show at on screen.
--
-- ObjC selector: @- position@
position :: IsSKOverlayAppConfiguration skOverlayAppConfiguration => skOverlayAppConfiguration -> IO SKOverlayPosition
position skOverlayAppConfiguration  =
  fmap (coerce :: CLong -> SKOverlayPosition) $ sendMsg skOverlayAppConfiguration (mkSelector "position") retCLong []

-- | The position an overlay will show at on screen.
--
-- ObjC selector: @- setPosition:@
setPosition :: IsSKOverlayAppConfiguration skOverlayAppConfiguration => skOverlayAppConfiguration -> SKOverlayPosition -> IO ()
setPosition skOverlayAppConfiguration  value =
  sendMsg skOverlayAppConfiguration (mkSelector "setPosition:") retVoid [argCLong (coerce value)]

-- | Allows the user to interactively dismiss an overlay.
--
-- ObjC selector: @- userDismissible@
userDismissible :: IsSKOverlayAppConfiguration skOverlayAppConfiguration => skOverlayAppConfiguration -> IO Bool
userDismissible skOverlayAppConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skOverlayAppConfiguration (mkSelector "userDismissible") retCULong []

-- | Allows the user to interactively dismiss an overlay.
--
-- ObjC selector: @- setUserDismissible:@
setUserDismissible :: IsSKOverlayAppConfiguration skOverlayAppConfiguration => skOverlayAppConfiguration -> Bool -> IO ()
setUserDismissible skOverlayAppConfiguration  value =
  sendMsg skOverlayAppConfiguration (mkSelector "setUserDismissible:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithAppIdentifier:position:@
initWithAppIdentifier_positionSelector :: Selector
initWithAppIdentifier_positionSelector = mkSelector "initWithAppIdentifier:position:"

-- | @Selector@ for @setAdditionalValue:forKey:@
setAdditionalValue_forKeySelector :: Selector
setAdditionalValue_forKeySelector = mkSelector "setAdditionalValue:forKey:"

-- | @Selector@ for @additionalValueForKey:@
additionalValueForKeySelector :: Selector
additionalValueForKeySelector = mkSelector "additionalValueForKey:"

-- | @Selector@ for @setAdImpression:@
setAdImpressionSelector :: Selector
setAdImpressionSelector = mkSelector "setAdImpression:"

-- | @Selector@ for @appIdentifier@
appIdentifierSelector :: Selector
appIdentifierSelector = mkSelector "appIdentifier"

-- | @Selector@ for @setAppIdentifier:@
setAppIdentifierSelector :: Selector
setAppIdentifierSelector = mkSelector "setAppIdentifier:"

-- | @Selector@ for @campaignToken@
campaignTokenSelector :: Selector
campaignTokenSelector = mkSelector "campaignToken"

-- | @Selector@ for @setCampaignToken:@
setCampaignTokenSelector :: Selector
setCampaignTokenSelector = mkSelector "setCampaignToken:"

-- | @Selector@ for @providerToken@
providerTokenSelector :: Selector
providerTokenSelector = mkSelector "providerToken"

-- | @Selector@ for @setProviderToken:@
setProviderTokenSelector :: Selector
setProviderTokenSelector = mkSelector "setProviderToken:"

-- | @Selector@ for @position@
positionSelector :: Selector
positionSelector = mkSelector "position"

-- | @Selector@ for @setPosition:@
setPositionSelector :: Selector
setPositionSelector = mkSelector "setPosition:"

-- | @Selector@ for @userDismissible@
userDismissibleSelector :: Selector
userDismissibleSelector = mkSelector "userDismissible"

-- | @Selector@ for @setUserDismissible:@
setUserDismissibleSelector :: Selector
setUserDismissibleSelector = mkSelector "setUserDismissible:"

