{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , newSelector
  , initWithPositionSelector
  , setAdditionalValue_forKeySelector
  , additionalValueForKeySelector
  , campaignTokenSelector
  , setCampaignTokenSelector
  , providerTokenSelector
  , setProviderTokenSelector
  , positionSelector
  , setPositionSelector

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
init_ :: IsSKOverlayAppClipConfiguration skOverlayAppClipConfiguration => skOverlayAppClipConfiguration -> IO (Id SKOverlayAppClipConfiguration)
init_ skOverlayAppClipConfiguration  =
  sendMsg skOverlayAppClipConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SKOverlayAppClipConfiguration)
new  =
  do
    cls' <- getRequiredClass "SKOverlayAppClipConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Creates a new app overlay configuration that will show an app clip's full app.
--
-- @position@ — the desired position of the overlay.
--
-- ObjC selector: @- initWithPosition:@
initWithPosition :: IsSKOverlayAppClipConfiguration skOverlayAppClipConfiguration => skOverlayAppClipConfiguration -> SKOverlayPosition -> IO (Id SKOverlayAppClipConfiguration)
initWithPosition skOverlayAppClipConfiguration  position =
  sendMsg skOverlayAppClipConfiguration (mkSelector "initWithPosition:") (retPtr retVoid) [argCLong (coerce position)] >>= ownedObject . castPtr

-- | @- setAdditionalValue:forKey:@
setAdditionalValue_forKey :: (IsSKOverlayAppClipConfiguration skOverlayAppClipConfiguration, IsNSString key) => skOverlayAppClipConfiguration -> RawId -> key -> IO ()
setAdditionalValue_forKey skOverlayAppClipConfiguration  value key =
withObjCPtr key $ \raw_key ->
    sendMsg skOverlayAppClipConfiguration (mkSelector "setAdditionalValue:forKey:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | @- additionalValueForKey:@
additionalValueForKey :: (IsSKOverlayAppClipConfiguration skOverlayAppClipConfiguration, IsNSString key) => skOverlayAppClipConfiguration -> key -> IO RawId
additionalValueForKey skOverlayAppClipConfiguration  key =
withObjCPtr key $ \raw_key ->
    fmap (RawId . castPtr) $ sendMsg skOverlayAppClipConfiguration (mkSelector "additionalValueForKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())]

-- | A token representing an App Analytics campaign.
--
-- ObjC selector: @- campaignToken@
campaignToken :: IsSKOverlayAppClipConfiguration skOverlayAppClipConfiguration => skOverlayAppClipConfiguration -> IO (Id NSString)
campaignToken skOverlayAppClipConfiguration  =
  sendMsg skOverlayAppClipConfiguration (mkSelector "campaignToken") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A token representing an App Analytics campaign.
--
-- ObjC selector: @- setCampaignToken:@
setCampaignToken :: (IsSKOverlayAppClipConfiguration skOverlayAppClipConfiguration, IsNSString value) => skOverlayAppClipConfiguration -> value -> IO ()
setCampaignToken skOverlayAppClipConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg skOverlayAppClipConfiguration (mkSelector "setCampaignToken:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The provider token for the developer that created the app being presented.
--
-- ObjC selector: @- providerToken@
providerToken :: IsSKOverlayAppClipConfiguration skOverlayAppClipConfiguration => skOverlayAppClipConfiguration -> IO (Id NSString)
providerToken skOverlayAppClipConfiguration  =
  sendMsg skOverlayAppClipConfiguration (mkSelector "providerToken") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The provider token for the developer that created the app being presented.
--
-- ObjC selector: @- setProviderToken:@
setProviderToken :: (IsSKOverlayAppClipConfiguration skOverlayAppClipConfiguration, IsNSString value) => skOverlayAppClipConfiguration -> value -> IO ()
setProviderToken skOverlayAppClipConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg skOverlayAppClipConfiguration (mkSelector "setProviderToken:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The position an overlay will show at on screen.
--
-- ObjC selector: @- position@
position :: IsSKOverlayAppClipConfiguration skOverlayAppClipConfiguration => skOverlayAppClipConfiguration -> IO SKOverlayPosition
position skOverlayAppClipConfiguration  =
  fmap (coerce :: CLong -> SKOverlayPosition) $ sendMsg skOverlayAppClipConfiguration (mkSelector "position") retCLong []

-- | The position an overlay will show at on screen.
--
-- ObjC selector: @- setPosition:@
setPosition :: IsSKOverlayAppClipConfiguration skOverlayAppClipConfiguration => skOverlayAppClipConfiguration -> SKOverlayPosition -> IO ()
setPosition skOverlayAppClipConfiguration  value =
  sendMsg skOverlayAppClipConfiguration (mkSelector "setPosition:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithPosition:@
initWithPositionSelector :: Selector
initWithPositionSelector = mkSelector "initWithPosition:"

-- | @Selector@ for @setAdditionalValue:forKey:@
setAdditionalValue_forKeySelector :: Selector
setAdditionalValue_forKeySelector = mkSelector "setAdditionalValue:forKey:"

-- | @Selector@ for @additionalValueForKey:@
additionalValueForKeySelector :: Selector
additionalValueForKeySelector = mkSelector "additionalValueForKey:"

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

