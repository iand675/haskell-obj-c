{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSColorSpace@.
module ObjC.AppKit.NSColorSpace
  ( NSColorSpace
  , IsNSColorSpace(..)
  , initWithICCProfileData
  , initWithColorSyncProfile
  , initWithCGColorSpace
  , availableColorSpacesWithModel
  , iccProfileData
  , colorSyncProfile
  , cgColorSpace
  , numberOfColorComponents
  , colorSpaceModel
  , localizedName
  , sRGBColorSpace
  , genericGamma22GrayColorSpace
  , extendedSRGBColorSpace
  , extendedGenericGamma22GrayColorSpace
  , displayP3ColorSpace
  , adobeRGB1998ColorSpace
  , genericRGBColorSpace
  , genericGrayColorSpace
  , genericCMYKColorSpace
  , deviceRGBColorSpace
  , deviceGrayColorSpace
  , deviceCMYKColorSpace
  , adobeRGB1998ColorSpaceSelector
  , availableColorSpacesWithModelSelector
  , cgColorSpaceSelector
  , colorSpaceModelSelector
  , colorSyncProfileSelector
  , deviceCMYKColorSpaceSelector
  , deviceGrayColorSpaceSelector
  , deviceRGBColorSpaceSelector
  , displayP3ColorSpaceSelector
  , extendedGenericGamma22GrayColorSpaceSelector
  , extendedSRGBColorSpaceSelector
  , genericCMYKColorSpaceSelector
  , genericGamma22GrayColorSpaceSelector
  , genericGrayColorSpaceSelector
  , genericRGBColorSpaceSelector
  , iccProfileDataSelector
  , initWithCGColorSpaceSelector
  , initWithColorSyncProfileSelector
  , initWithICCProfileDataSelector
  , localizedNameSelector
  , numberOfColorComponentsSelector
  , sRGBColorSpaceSelector

  -- * Enum types
  , NSColorSpaceModel(NSColorSpaceModel)
  , pattern NSColorSpaceModelUnknown
  , pattern NSColorSpaceModelGray
  , pattern NSColorSpaceModelRGB
  , pattern NSColorSpaceModelCMYK
  , pattern NSColorSpaceModelLAB
  , pattern NSColorSpaceModelDeviceN
  , pattern NSColorSpaceModelIndexed
  , pattern NSColorSpaceModelPatterned

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithICCProfileData:@
initWithICCProfileData :: (IsNSColorSpace nsColorSpace, IsNSData iccData) => nsColorSpace -> iccData -> IO (Id NSColorSpace)
initWithICCProfileData nsColorSpace iccData =
  sendOwnedMessage nsColorSpace initWithICCProfileDataSelector (toNSData iccData)

-- | @- initWithColorSyncProfile:@
initWithColorSyncProfile :: IsNSColorSpace nsColorSpace => nsColorSpace -> Ptr () -> IO (Id NSColorSpace)
initWithColorSyncProfile nsColorSpace prof =
  sendOwnedMessage nsColorSpace initWithColorSyncProfileSelector prof

-- | @- initWithCGColorSpace:@
initWithCGColorSpace :: IsNSColorSpace nsColorSpace => nsColorSpace -> Ptr () -> IO (Id NSColorSpace)
initWithCGColorSpace nsColorSpace cgColorSpace =
  sendOwnedMessage nsColorSpace initWithCGColorSpaceSelector cgColorSpace

-- | @+ availableColorSpacesWithModel:@
availableColorSpacesWithModel :: NSColorSpaceModel -> IO (Id NSArray)
availableColorSpacesWithModel model =
  do
    cls' <- getRequiredClass "NSColorSpace"
    sendClassMessage cls' availableColorSpacesWithModelSelector model

-- | @- ICCProfileData@
iccProfileData :: IsNSColorSpace nsColorSpace => nsColorSpace -> IO (Id NSData)
iccProfileData nsColorSpace =
  sendMessage nsColorSpace iccProfileDataSelector

-- | @- colorSyncProfile@
colorSyncProfile :: IsNSColorSpace nsColorSpace => nsColorSpace -> IO (Ptr ())
colorSyncProfile nsColorSpace =
  sendMessage nsColorSpace colorSyncProfileSelector

-- | @- CGColorSpace@
cgColorSpace :: IsNSColorSpace nsColorSpace => nsColorSpace -> IO (Ptr ())
cgColorSpace nsColorSpace =
  sendMessage nsColorSpace cgColorSpaceSelector

-- | @- numberOfColorComponents@
numberOfColorComponents :: IsNSColorSpace nsColorSpace => nsColorSpace -> IO CLong
numberOfColorComponents nsColorSpace =
  sendMessage nsColorSpace numberOfColorComponentsSelector

-- | @- colorSpaceModel@
colorSpaceModel :: IsNSColorSpace nsColorSpace => nsColorSpace -> IO NSColorSpaceModel
colorSpaceModel nsColorSpace =
  sendMessage nsColorSpace colorSpaceModelSelector

-- | @- localizedName@
localizedName :: IsNSColorSpace nsColorSpace => nsColorSpace -> IO (Id NSString)
localizedName nsColorSpace =
  sendMessage nsColorSpace localizedNameSelector

-- | @+ sRGBColorSpace@
sRGBColorSpace :: IO (Id NSColorSpace)
sRGBColorSpace  =
  do
    cls' <- getRequiredClass "NSColorSpace"
    sendClassMessage cls' sRGBColorSpaceSelector

-- | @+ genericGamma22GrayColorSpace@
genericGamma22GrayColorSpace :: IO (Id NSColorSpace)
genericGamma22GrayColorSpace  =
  do
    cls' <- getRequiredClass "NSColorSpace"
    sendClassMessage cls' genericGamma22GrayColorSpaceSelector

-- | @+ extendedSRGBColorSpace@
extendedSRGBColorSpace :: IO (Id NSColorSpace)
extendedSRGBColorSpace  =
  do
    cls' <- getRequiredClass "NSColorSpace"
    sendClassMessage cls' extendedSRGBColorSpaceSelector

-- | @+ extendedGenericGamma22GrayColorSpace@
extendedGenericGamma22GrayColorSpace :: IO (Id NSColorSpace)
extendedGenericGamma22GrayColorSpace  =
  do
    cls' <- getRequiredClass "NSColorSpace"
    sendClassMessage cls' extendedGenericGamma22GrayColorSpaceSelector

-- | @+ displayP3ColorSpace@
displayP3ColorSpace :: IO (Id NSColorSpace)
displayP3ColorSpace  =
  do
    cls' <- getRequiredClass "NSColorSpace"
    sendClassMessage cls' displayP3ColorSpaceSelector

-- | @+ adobeRGB1998ColorSpace@
adobeRGB1998ColorSpace :: IO (Id NSColorSpace)
adobeRGB1998ColorSpace  =
  do
    cls' <- getRequiredClass "NSColorSpace"
    sendClassMessage cls' adobeRGB1998ColorSpaceSelector

-- | @+ genericRGBColorSpace@
genericRGBColorSpace :: IO (Id NSColorSpace)
genericRGBColorSpace  =
  do
    cls' <- getRequiredClass "NSColorSpace"
    sendClassMessage cls' genericRGBColorSpaceSelector

-- | @+ genericGrayColorSpace@
genericGrayColorSpace :: IO (Id NSColorSpace)
genericGrayColorSpace  =
  do
    cls' <- getRequiredClass "NSColorSpace"
    sendClassMessage cls' genericGrayColorSpaceSelector

-- | @+ genericCMYKColorSpace@
genericCMYKColorSpace :: IO (Id NSColorSpace)
genericCMYKColorSpace  =
  do
    cls' <- getRequiredClass "NSColorSpace"
    sendClassMessage cls' genericCMYKColorSpaceSelector

-- | @+ deviceRGBColorSpace@
deviceRGBColorSpace :: IO (Id NSColorSpace)
deviceRGBColorSpace  =
  do
    cls' <- getRequiredClass "NSColorSpace"
    sendClassMessage cls' deviceRGBColorSpaceSelector

-- | @+ deviceGrayColorSpace@
deviceGrayColorSpace :: IO (Id NSColorSpace)
deviceGrayColorSpace  =
  do
    cls' <- getRequiredClass "NSColorSpace"
    sendClassMessage cls' deviceGrayColorSpaceSelector

-- | @+ deviceCMYKColorSpace@
deviceCMYKColorSpace :: IO (Id NSColorSpace)
deviceCMYKColorSpace  =
  do
    cls' <- getRequiredClass "NSColorSpace"
    sendClassMessage cls' deviceCMYKColorSpaceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithICCProfileData:@
initWithICCProfileDataSelector :: Selector '[Id NSData] (Id NSColorSpace)
initWithICCProfileDataSelector = mkSelector "initWithICCProfileData:"

-- | @Selector@ for @initWithColorSyncProfile:@
initWithColorSyncProfileSelector :: Selector '[Ptr ()] (Id NSColorSpace)
initWithColorSyncProfileSelector = mkSelector "initWithColorSyncProfile:"

-- | @Selector@ for @initWithCGColorSpace:@
initWithCGColorSpaceSelector :: Selector '[Ptr ()] (Id NSColorSpace)
initWithCGColorSpaceSelector = mkSelector "initWithCGColorSpace:"

-- | @Selector@ for @availableColorSpacesWithModel:@
availableColorSpacesWithModelSelector :: Selector '[NSColorSpaceModel] (Id NSArray)
availableColorSpacesWithModelSelector = mkSelector "availableColorSpacesWithModel:"

-- | @Selector@ for @ICCProfileData@
iccProfileDataSelector :: Selector '[] (Id NSData)
iccProfileDataSelector = mkSelector "ICCProfileData"

-- | @Selector@ for @colorSyncProfile@
colorSyncProfileSelector :: Selector '[] (Ptr ())
colorSyncProfileSelector = mkSelector "colorSyncProfile"

-- | @Selector@ for @CGColorSpace@
cgColorSpaceSelector :: Selector '[] (Ptr ())
cgColorSpaceSelector = mkSelector "CGColorSpace"

-- | @Selector@ for @numberOfColorComponents@
numberOfColorComponentsSelector :: Selector '[] CLong
numberOfColorComponentsSelector = mkSelector "numberOfColorComponents"

-- | @Selector@ for @colorSpaceModel@
colorSpaceModelSelector :: Selector '[] NSColorSpaceModel
colorSpaceModelSelector = mkSelector "colorSpaceModel"

-- | @Selector@ for @localizedName@
localizedNameSelector :: Selector '[] (Id NSString)
localizedNameSelector = mkSelector "localizedName"

-- | @Selector@ for @sRGBColorSpace@
sRGBColorSpaceSelector :: Selector '[] (Id NSColorSpace)
sRGBColorSpaceSelector = mkSelector "sRGBColorSpace"

-- | @Selector@ for @genericGamma22GrayColorSpace@
genericGamma22GrayColorSpaceSelector :: Selector '[] (Id NSColorSpace)
genericGamma22GrayColorSpaceSelector = mkSelector "genericGamma22GrayColorSpace"

-- | @Selector@ for @extendedSRGBColorSpace@
extendedSRGBColorSpaceSelector :: Selector '[] (Id NSColorSpace)
extendedSRGBColorSpaceSelector = mkSelector "extendedSRGBColorSpace"

-- | @Selector@ for @extendedGenericGamma22GrayColorSpace@
extendedGenericGamma22GrayColorSpaceSelector :: Selector '[] (Id NSColorSpace)
extendedGenericGamma22GrayColorSpaceSelector = mkSelector "extendedGenericGamma22GrayColorSpace"

-- | @Selector@ for @displayP3ColorSpace@
displayP3ColorSpaceSelector :: Selector '[] (Id NSColorSpace)
displayP3ColorSpaceSelector = mkSelector "displayP3ColorSpace"

-- | @Selector@ for @adobeRGB1998ColorSpace@
adobeRGB1998ColorSpaceSelector :: Selector '[] (Id NSColorSpace)
adobeRGB1998ColorSpaceSelector = mkSelector "adobeRGB1998ColorSpace"

-- | @Selector@ for @genericRGBColorSpace@
genericRGBColorSpaceSelector :: Selector '[] (Id NSColorSpace)
genericRGBColorSpaceSelector = mkSelector "genericRGBColorSpace"

-- | @Selector@ for @genericGrayColorSpace@
genericGrayColorSpaceSelector :: Selector '[] (Id NSColorSpace)
genericGrayColorSpaceSelector = mkSelector "genericGrayColorSpace"

-- | @Selector@ for @genericCMYKColorSpace@
genericCMYKColorSpaceSelector :: Selector '[] (Id NSColorSpace)
genericCMYKColorSpaceSelector = mkSelector "genericCMYKColorSpace"

-- | @Selector@ for @deviceRGBColorSpace@
deviceRGBColorSpaceSelector :: Selector '[] (Id NSColorSpace)
deviceRGBColorSpaceSelector = mkSelector "deviceRGBColorSpace"

-- | @Selector@ for @deviceGrayColorSpace@
deviceGrayColorSpaceSelector :: Selector '[] (Id NSColorSpace)
deviceGrayColorSpaceSelector = mkSelector "deviceGrayColorSpace"

-- | @Selector@ for @deviceCMYKColorSpace@
deviceCMYKColorSpaceSelector :: Selector '[] (Id NSColorSpace)
deviceCMYKColorSpaceSelector = mkSelector "deviceCMYKColorSpace"

