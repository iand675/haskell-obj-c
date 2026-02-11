{-# LANGUAGE PatternSynonyms #-}
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
  , cgColorSpace
  , numberOfColorComponents
  , colorSpaceModel
  , localizedName
  , genericRGBColorSpace
  , genericGrayColorSpace
  , genericCMYKColorSpace
  , deviceRGBColorSpace
  , deviceGrayColorSpace
  , deviceCMYKColorSpace
  , initWithICCProfileDataSelector
  , initWithColorSyncProfileSelector
  , initWithCGColorSpaceSelector
  , availableColorSpacesWithModelSelector
  , iccProfileDataSelector
  , cgColorSpaceSelector
  , numberOfColorComponentsSelector
  , colorSpaceModelSelector
  , localizedNameSelector
  , genericRGBColorSpaceSelector
  , genericGrayColorSpaceSelector
  , genericCMYKColorSpaceSelector
  , deviceRGBColorSpaceSelector
  , deviceGrayColorSpaceSelector
  , deviceCMYKColorSpaceSelector

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

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithICCProfileData:@
initWithICCProfileData :: (IsNSColorSpace nsColorSpace, IsNSData iccData) => nsColorSpace -> iccData -> IO (Id NSColorSpace)
initWithICCProfileData nsColorSpace  iccData =
withObjCPtr iccData $ \raw_iccData ->
    sendMsg nsColorSpace (mkSelector "initWithICCProfileData:") (retPtr retVoid) [argPtr (castPtr raw_iccData :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithColorSyncProfile:@
initWithColorSyncProfile :: IsNSColorSpace nsColorSpace => nsColorSpace -> Ptr () -> IO (Id NSColorSpace)
initWithColorSyncProfile nsColorSpace  prof =
  sendMsg nsColorSpace (mkSelector "initWithColorSyncProfile:") (retPtr retVoid) [argPtr prof] >>= ownedObject . castPtr

-- | @- initWithCGColorSpace:@
initWithCGColorSpace :: IsNSColorSpace nsColorSpace => nsColorSpace -> Ptr () -> IO (Id NSColorSpace)
initWithCGColorSpace nsColorSpace  cgColorSpace =
  sendMsg nsColorSpace (mkSelector "initWithCGColorSpace:") (retPtr retVoid) [argPtr cgColorSpace] >>= ownedObject . castPtr

-- | @+ availableColorSpacesWithModel:@
availableColorSpacesWithModel :: NSColorSpaceModel -> IO (Id NSArray)
availableColorSpacesWithModel model =
  do
    cls' <- getRequiredClass "NSColorSpace"
    sendClassMsg cls' (mkSelector "availableColorSpacesWithModel:") (retPtr retVoid) [argCLong (coerce model)] >>= retainedObject . castPtr

-- | @- ICCProfileData@
iccProfileData :: IsNSColorSpace nsColorSpace => nsColorSpace -> IO (Id NSData)
iccProfileData nsColorSpace  =
  sendMsg nsColorSpace (mkSelector "ICCProfileData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- CGColorSpace@
cgColorSpace :: IsNSColorSpace nsColorSpace => nsColorSpace -> IO (Ptr ())
cgColorSpace nsColorSpace  =
  fmap castPtr $ sendMsg nsColorSpace (mkSelector "CGColorSpace") (retPtr retVoid) []

-- | @- numberOfColorComponents@
numberOfColorComponents :: IsNSColorSpace nsColorSpace => nsColorSpace -> IO CLong
numberOfColorComponents nsColorSpace  =
  sendMsg nsColorSpace (mkSelector "numberOfColorComponents") retCLong []

-- | @- colorSpaceModel@
colorSpaceModel :: IsNSColorSpace nsColorSpace => nsColorSpace -> IO NSColorSpaceModel
colorSpaceModel nsColorSpace  =
  fmap (coerce :: CLong -> NSColorSpaceModel) $ sendMsg nsColorSpace (mkSelector "colorSpaceModel") retCLong []

-- | @- localizedName@
localizedName :: IsNSColorSpace nsColorSpace => nsColorSpace -> IO (Id NSString)
localizedName nsColorSpace  =
  sendMsg nsColorSpace (mkSelector "localizedName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ genericRGBColorSpace@
genericRGBColorSpace :: IO (Id NSColorSpace)
genericRGBColorSpace  =
  do
    cls' <- getRequiredClass "NSColorSpace"
    sendClassMsg cls' (mkSelector "genericRGBColorSpace") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ genericGrayColorSpace@
genericGrayColorSpace :: IO (Id NSColorSpace)
genericGrayColorSpace  =
  do
    cls' <- getRequiredClass "NSColorSpace"
    sendClassMsg cls' (mkSelector "genericGrayColorSpace") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ genericCMYKColorSpace@
genericCMYKColorSpace :: IO (Id NSColorSpace)
genericCMYKColorSpace  =
  do
    cls' <- getRequiredClass "NSColorSpace"
    sendClassMsg cls' (mkSelector "genericCMYKColorSpace") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ deviceRGBColorSpace@
deviceRGBColorSpace :: IO (Id NSColorSpace)
deviceRGBColorSpace  =
  do
    cls' <- getRequiredClass "NSColorSpace"
    sendClassMsg cls' (mkSelector "deviceRGBColorSpace") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ deviceGrayColorSpace@
deviceGrayColorSpace :: IO (Id NSColorSpace)
deviceGrayColorSpace  =
  do
    cls' <- getRequiredClass "NSColorSpace"
    sendClassMsg cls' (mkSelector "deviceGrayColorSpace") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ deviceCMYKColorSpace@
deviceCMYKColorSpace :: IO (Id NSColorSpace)
deviceCMYKColorSpace  =
  do
    cls' <- getRequiredClass "NSColorSpace"
    sendClassMsg cls' (mkSelector "deviceCMYKColorSpace") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithICCProfileData:@
initWithICCProfileDataSelector :: Selector
initWithICCProfileDataSelector = mkSelector "initWithICCProfileData:"

-- | @Selector@ for @initWithColorSyncProfile:@
initWithColorSyncProfileSelector :: Selector
initWithColorSyncProfileSelector = mkSelector "initWithColorSyncProfile:"

-- | @Selector@ for @initWithCGColorSpace:@
initWithCGColorSpaceSelector :: Selector
initWithCGColorSpaceSelector = mkSelector "initWithCGColorSpace:"

-- | @Selector@ for @availableColorSpacesWithModel:@
availableColorSpacesWithModelSelector :: Selector
availableColorSpacesWithModelSelector = mkSelector "availableColorSpacesWithModel:"

-- | @Selector@ for @ICCProfileData@
iccProfileDataSelector :: Selector
iccProfileDataSelector = mkSelector "ICCProfileData"

-- | @Selector@ for @CGColorSpace@
cgColorSpaceSelector :: Selector
cgColorSpaceSelector = mkSelector "CGColorSpace"

-- | @Selector@ for @numberOfColorComponents@
numberOfColorComponentsSelector :: Selector
numberOfColorComponentsSelector = mkSelector "numberOfColorComponents"

-- | @Selector@ for @colorSpaceModel@
colorSpaceModelSelector :: Selector
colorSpaceModelSelector = mkSelector "colorSpaceModel"

-- | @Selector@ for @localizedName@
localizedNameSelector :: Selector
localizedNameSelector = mkSelector "localizedName"

-- | @Selector@ for @genericRGBColorSpace@
genericRGBColorSpaceSelector :: Selector
genericRGBColorSpaceSelector = mkSelector "genericRGBColorSpace"

-- | @Selector@ for @genericGrayColorSpace@
genericGrayColorSpaceSelector :: Selector
genericGrayColorSpaceSelector = mkSelector "genericGrayColorSpace"

-- | @Selector@ for @genericCMYKColorSpace@
genericCMYKColorSpaceSelector :: Selector
genericCMYKColorSpaceSelector = mkSelector "genericCMYKColorSpace"

-- | @Selector@ for @deviceRGBColorSpace@
deviceRGBColorSpaceSelector :: Selector
deviceRGBColorSpaceSelector = mkSelector "deviceRGBColorSpace"

-- | @Selector@ for @deviceGrayColorSpace@
deviceGrayColorSpaceSelector :: Selector
deviceGrayColorSpaceSelector = mkSelector "deviceGrayColorSpace"

-- | @Selector@ for @deviceCMYKColorSpace@
deviceCMYKColorSpaceSelector :: Selector
deviceCMYKColorSpaceSelector = mkSelector "deviceCMYKColorSpace"

