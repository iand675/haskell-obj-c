{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRContentLauncherClusterBrandingInformation@.
module ObjC.Matter.MTRContentLauncherClusterBrandingInformation
  ( MTRContentLauncherClusterBrandingInformation
  , IsMTRContentLauncherClusterBrandingInformation(..)
  , providerName
  , setProviderName
  , background
  , setBackground
  , logo
  , setLogo
  , progressBar
  , setProgressBar
  , splash
  , setSplash
  , waterMark
  , setWaterMark
  , backgroundSelector
  , logoSelector
  , progressBarSelector
  , providerNameSelector
  , setBackgroundSelector
  , setLogoSelector
  , setProgressBarSelector
  , setProviderNameSelector
  , setSplashSelector
  , setWaterMarkSelector
  , splashSelector
  , waterMarkSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- providerName@
providerName :: IsMTRContentLauncherClusterBrandingInformation mtrContentLauncherClusterBrandingInformation => mtrContentLauncherClusterBrandingInformation -> IO (Id NSString)
providerName mtrContentLauncherClusterBrandingInformation =
  sendMessage mtrContentLauncherClusterBrandingInformation providerNameSelector

-- | @- setProviderName:@
setProviderName :: (IsMTRContentLauncherClusterBrandingInformation mtrContentLauncherClusterBrandingInformation, IsNSString value) => mtrContentLauncherClusterBrandingInformation -> value -> IO ()
setProviderName mtrContentLauncherClusterBrandingInformation value =
  sendMessage mtrContentLauncherClusterBrandingInformation setProviderNameSelector (toNSString value)

-- | @- background@
background :: IsMTRContentLauncherClusterBrandingInformation mtrContentLauncherClusterBrandingInformation => mtrContentLauncherClusterBrandingInformation -> IO (Id MTRContentLauncherClusterStyleInformationStruct)
background mtrContentLauncherClusterBrandingInformation =
  sendMessage mtrContentLauncherClusterBrandingInformation backgroundSelector

-- | @- setBackground:@
setBackground :: (IsMTRContentLauncherClusterBrandingInformation mtrContentLauncherClusterBrandingInformation, IsMTRContentLauncherClusterStyleInformationStruct value) => mtrContentLauncherClusterBrandingInformation -> value -> IO ()
setBackground mtrContentLauncherClusterBrandingInformation value =
  sendMessage mtrContentLauncherClusterBrandingInformation setBackgroundSelector (toMTRContentLauncherClusterStyleInformationStruct value)

-- | @- logo@
logo :: IsMTRContentLauncherClusterBrandingInformation mtrContentLauncherClusterBrandingInformation => mtrContentLauncherClusterBrandingInformation -> IO (Id MTRContentLauncherClusterStyleInformationStruct)
logo mtrContentLauncherClusterBrandingInformation =
  sendMessage mtrContentLauncherClusterBrandingInformation logoSelector

-- | @- setLogo:@
setLogo :: (IsMTRContentLauncherClusterBrandingInformation mtrContentLauncherClusterBrandingInformation, IsMTRContentLauncherClusterStyleInformationStruct value) => mtrContentLauncherClusterBrandingInformation -> value -> IO ()
setLogo mtrContentLauncherClusterBrandingInformation value =
  sendMessage mtrContentLauncherClusterBrandingInformation setLogoSelector (toMTRContentLauncherClusterStyleInformationStruct value)

-- | @- progressBar@
progressBar :: IsMTRContentLauncherClusterBrandingInformation mtrContentLauncherClusterBrandingInformation => mtrContentLauncherClusterBrandingInformation -> IO (Id MTRContentLauncherClusterStyleInformationStruct)
progressBar mtrContentLauncherClusterBrandingInformation =
  sendMessage mtrContentLauncherClusterBrandingInformation progressBarSelector

-- | @- setProgressBar:@
setProgressBar :: (IsMTRContentLauncherClusterBrandingInformation mtrContentLauncherClusterBrandingInformation, IsMTRContentLauncherClusterStyleInformationStruct value) => mtrContentLauncherClusterBrandingInformation -> value -> IO ()
setProgressBar mtrContentLauncherClusterBrandingInformation value =
  sendMessage mtrContentLauncherClusterBrandingInformation setProgressBarSelector (toMTRContentLauncherClusterStyleInformationStruct value)

-- | @- splash@
splash :: IsMTRContentLauncherClusterBrandingInformation mtrContentLauncherClusterBrandingInformation => mtrContentLauncherClusterBrandingInformation -> IO (Id MTRContentLauncherClusterStyleInformationStruct)
splash mtrContentLauncherClusterBrandingInformation =
  sendMessage mtrContentLauncherClusterBrandingInformation splashSelector

-- | @- setSplash:@
setSplash :: (IsMTRContentLauncherClusterBrandingInformation mtrContentLauncherClusterBrandingInformation, IsMTRContentLauncherClusterStyleInformationStruct value) => mtrContentLauncherClusterBrandingInformation -> value -> IO ()
setSplash mtrContentLauncherClusterBrandingInformation value =
  sendMessage mtrContentLauncherClusterBrandingInformation setSplashSelector (toMTRContentLauncherClusterStyleInformationStruct value)

-- | @- waterMark@
waterMark :: IsMTRContentLauncherClusterBrandingInformation mtrContentLauncherClusterBrandingInformation => mtrContentLauncherClusterBrandingInformation -> IO (Id MTRContentLauncherClusterStyleInformationStruct)
waterMark mtrContentLauncherClusterBrandingInformation =
  sendMessage mtrContentLauncherClusterBrandingInformation waterMarkSelector

-- | @- setWaterMark:@
setWaterMark :: (IsMTRContentLauncherClusterBrandingInformation mtrContentLauncherClusterBrandingInformation, IsMTRContentLauncherClusterStyleInformationStruct value) => mtrContentLauncherClusterBrandingInformation -> value -> IO ()
setWaterMark mtrContentLauncherClusterBrandingInformation value =
  sendMessage mtrContentLauncherClusterBrandingInformation setWaterMarkSelector (toMTRContentLauncherClusterStyleInformationStruct value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @providerName@
providerNameSelector :: Selector '[] (Id NSString)
providerNameSelector = mkSelector "providerName"

-- | @Selector@ for @setProviderName:@
setProviderNameSelector :: Selector '[Id NSString] ()
setProviderNameSelector = mkSelector "setProviderName:"

-- | @Selector@ for @background@
backgroundSelector :: Selector '[] (Id MTRContentLauncherClusterStyleInformationStruct)
backgroundSelector = mkSelector "background"

-- | @Selector@ for @setBackground:@
setBackgroundSelector :: Selector '[Id MTRContentLauncherClusterStyleInformationStruct] ()
setBackgroundSelector = mkSelector "setBackground:"

-- | @Selector@ for @logo@
logoSelector :: Selector '[] (Id MTRContentLauncherClusterStyleInformationStruct)
logoSelector = mkSelector "logo"

-- | @Selector@ for @setLogo:@
setLogoSelector :: Selector '[Id MTRContentLauncherClusterStyleInformationStruct] ()
setLogoSelector = mkSelector "setLogo:"

-- | @Selector@ for @progressBar@
progressBarSelector :: Selector '[] (Id MTRContentLauncherClusterStyleInformationStruct)
progressBarSelector = mkSelector "progressBar"

-- | @Selector@ for @setProgressBar:@
setProgressBarSelector :: Selector '[Id MTRContentLauncherClusterStyleInformationStruct] ()
setProgressBarSelector = mkSelector "setProgressBar:"

-- | @Selector@ for @splash@
splashSelector :: Selector '[] (Id MTRContentLauncherClusterStyleInformationStruct)
splashSelector = mkSelector "splash"

-- | @Selector@ for @setSplash:@
setSplashSelector :: Selector '[Id MTRContentLauncherClusterStyleInformationStruct] ()
setSplashSelector = mkSelector "setSplash:"

-- | @Selector@ for @waterMark@
waterMarkSelector :: Selector '[] (Id MTRContentLauncherClusterStyleInformationStruct)
waterMarkSelector = mkSelector "waterMark"

-- | @Selector@ for @setWaterMark:@
setWaterMarkSelector :: Selector '[Id MTRContentLauncherClusterStyleInformationStruct] ()
setWaterMarkSelector = mkSelector "setWaterMark:"

