{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRContentLauncherClusterBrandingInformationStruct@.
module ObjC.Matter.MTRContentLauncherClusterBrandingInformationStruct
  ( MTRContentLauncherClusterBrandingInformationStruct
  , IsMTRContentLauncherClusterBrandingInformationStruct(..)
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
providerName :: IsMTRContentLauncherClusterBrandingInformationStruct mtrContentLauncherClusterBrandingInformationStruct => mtrContentLauncherClusterBrandingInformationStruct -> IO (Id NSString)
providerName mtrContentLauncherClusterBrandingInformationStruct =
  sendMessage mtrContentLauncherClusterBrandingInformationStruct providerNameSelector

-- | @- setProviderName:@
setProviderName :: (IsMTRContentLauncherClusterBrandingInformationStruct mtrContentLauncherClusterBrandingInformationStruct, IsNSString value) => mtrContentLauncherClusterBrandingInformationStruct -> value -> IO ()
setProviderName mtrContentLauncherClusterBrandingInformationStruct value =
  sendMessage mtrContentLauncherClusterBrandingInformationStruct setProviderNameSelector (toNSString value)

-- | @- background@
background :: IsMTRContentLauncherClusterBrandingInformationStruct mtrContentLauncherClusterBrandingInformationStruct => mtrContentLauncherClusterBrandingInformationStruct -> IO (Id MTRContentLauncherClusterStyleInformationStruct)
background mtrContentLauncherClusterBrandingInformationStruct =
  sendMessage mtrContentLauncherClusterBrandingInformationStruct backgroundSelector

-- | @- setBackground:@
setBackground :: (IsMTRContentLauncherClusterBrandingInformationStruct mtrContentLauncherClusterBrandingInformationStruct, IsMTRContentLauncherClusterStyleInformationStruct value) => mtrContentLauncherClusterBrandingInformationStruct -> value -> IO ()
setBackground mtrContentLauncherClusterBrandingInformationStruct value =
  sendMessage mtrContentLauncherClusterBrandingInformationStruct setBackgroundSelector (toMTRContentLauncherClusterStyleInformationStruct value)

-- | @- logo@
logo :: IsMTRContentLauncherClusterBrandingInformationStruct mtrContentLauncherClusterBrandingInformationStruct => mtrContentLauncherClusterBrandingInformationStruct -> IO (Id MTRContentLauncherClusterStyleInformationStruct)
logo mtrContentLauncherClusterBrandingInformationStruct =
  sendMessage mtrContentLauncherClusterBrandingInformationStruct logoSelector

-- | @- setLogo:@
setLogo :: (IsMTRContentLauncherClusterBrandingInformationStruct mtrContentLauncherClusterBrandingInformationStruct, IsMTRContentLauncherClusterStyleInformationStruct value) => mtrContentLauncherClusterBrandingInformationStruct -> value -> IO ()
setLogo mtrContentLauncherClusterBrandingInformationStruct value =
  sendMessage mtrContentLauncherClusterBrandingInformationStruct setLogoSelector (toMTRContentLauncherClusterStyleInformationStruct value)

-- | @- progressBar@
progressBar :: IsMTRContentLauncherClusterBrandingInformationStruct mtrContentLauncherClusterBrandingInformationStruct => mtrContentLauncherClusterBrandingInformationStruct -> IO (Id MTRContentLauncherClusterStyleInformationStruct)
progressBar mtrContentLauncherClusterBrandingInformationStruct =
  sendMessage mtrContentLauncherClusterBrandingInformationStruct progressBarSelector

-- | @- setProgressBar:@
setProgressBar :: (IsMTRContentLauncherClusterBrandingInformationStruct mtrContentLauncherClusterBrandingInformationStruct, IsMTRContentLauncherClusterStyleInformationStruct value) => mtrContentLauncherClusterBrandingInformationStruct -> value -> IO ()
setProgressBar mtrContentLauncherClusterBrandingInformationStruct value =
  sendMessage mtrContentLauncherClusterBrandingInformationStruct setProgressBarSelector (toMTRContentLauncherClusterStyleInformationStruct value)

-- | @- splash@
splash :: IsMTRContentLauncherClusterBrandingInformationStruct mtrContentLauncherClusterBrandingInformationStruct => mtrContentLauncherClusterBrandingInformationStruct -> IO (Id MTRContentLauncherClusterStyleInformationStruct)
splash mtrContentLauncherClusterBrandingInformationStruct =
  sendMessage mtrContentLauncherClusterBrandingInformationStruct splashSelector

-- | @- setSplash:@
setSplash :: (IsMTRContentLauncherClusterBrandingInformationStruct mtrContentLauncherClusterBrandingInformationStruct, IsMTRContentLauncherClusterStyleInformationStruct value) => mtrContentLauncherClusterBrandingInformationStruct -> value -> IO ()
setSplash mtrContentLauncherClusterBrandingInformationStruct value =
  sendMessage mtrContentLauncherClusterBrandingInformationStruct setSplashSelector (toMTRContentLauncherClusterStyleInformationStruct value)

-- | @- waterMark@
waterMark :: IsMTRContentLauncherClusterBrandingInformationStruct mtrContentLauncherClusterBrandingInformationStruct => mtrContentLauncherClusterBrandingInformationStruct -> IO (Id MTRContentLauncherClusterStyleInformationStruct)
waterMark mtrContentLauncherClusterBrandingInformationStruct =
  sendMessage mtrContentLauncherClusterBrandingInformationStruct waterMarkSelector

-- | @- setWaterMark:@
setWaterMark :: (IsMTRContentLauncherClusterBrandingInformationStruct mtrContentLauncherClusterBrandingInformationStruct, IsMTRContentLauncherClusterStyleInformationStruct value) => mtrContentLauncherClusterBrandingInformationStruct -> value -> IO ()
setWaterMark mtrContentLauncherClusterBrandingInformationStruct value =
  sendMessage mtrContentLauncherClusterBrandingInformationStruct setWaterMarkSelector (toMTRContentLauncherClusterStyleInformationStruct value)

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

