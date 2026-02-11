{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A configuration for @PHPickerViewController.@
--
-- Generated bindings for @PHPickerConfiguration@.
module ObjC.PhotosUI.PHPickerConfiguration
  ( PHPickerConfiguration
  , IsPHPickerConfiguration(..)
  , initWithPhotoLibrary
  , init_
  , preferredAssetRepresentationMode
  , setPreferredAssetRepresentationMode
  , selection
  , setSelection
  , selectionLimit
  , setSelectionLimit
  , mode
  , setMode
  , edgesWithoutContentMargins
  , setEdgesWithoutContentMargins
  , disabledCapabilities
  , setDisabledCapabilities
  , initWithPhotoLibrarySelector
  , initSelector
  , preferredAssetRepresentationModeSelector
  , setPreferredAssetRepresentationModeSelector
  , selectionSelector
  , setSelectionSelector
  , selectionLimitSelector
  , setSelectionLimitSelector
  , modeSelector
  , setModeSelector
  , edgesWithoutContentMarginsSelector
  , setEdgesWithoutContentMarginsSelector
  , disabledCapabilitiesSelector
  , setDisabledCapabilitiesSelector

  -- * Enum types
  , NSDirectionalRectEdge(NSDirectionalRectEdge)
  , pattern NSDirectionalRectEdgeNone
  , pattern NSDirectionalRectEdgeTop
  , pattern NSDirectionalRectEdgeLeading
  , pattern NSDirectionalRectEdgeBottom
  , pattern NSDirectionalRectEdgeTrailing
  , pattern NSDirectionalRectEdgeAll
  , PHPickerCapabilities(PHPickerCapabilities)
  , pattern PHPickerCapabilitiesNone
  , pattern PHPickerCapabilitiesSearch
  , pattern PHPickerCapabilitiesStagingArea
  , pattern PHPickerCapabilitiesCollectionNavigation
  , pattern PHPickerCapabilitiesSelectionActions
  , pattern PHPickerCapabilitiesSensitivityAnalysisIntervention
  , PHPickerConfigurationAssetRepresentationMode(PHPickerConfigurationAssetRepresentationMode)
  , pattern PHPickerConfigurationAssetRepresentationModeAutomatic
  , pattern PHPickerConfigurationAssetRepresentationModeCurrent
  , pattern PHPickerConfigurationAssetRepresentationModeCompatible
  , PHPickerConfigurationSelection(PHPickerConfigurationSelection)
  , pattern PHPickerConfigurationSelectionDefault
  , pattern PHPickerConfigurationSelectionOrdered
  , pattern PHPickerConfigurationSelectionContinuous
  , pattern PHPickerConfigurationSelectionContinuousAndOrdered
  , PHPickerMode(PHPickerMode)
  , pattern PHPickerModeDefault
  , pattern PHPickerModeCompact

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

import ObjC.PhotosUI.Internal.Classes
import ObjC.PhotosUI.Internal.Enums
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes
import ObjC.Photos.Internal.Classes

-- | Initializes a new configuration with the @photoLibrary@ the picker should use.
--
-- ObjC selector: @- initWithPhotoLibrary:@
initWithPhotoLibrary :: (IsPHPickerConfiguration phPickerConfiguration, IsPHPhotoLibrary photoLibrary) => phPickerConfiguration -> photoLibrary -> IO (Id PHPickerConfiguration)
initWithPhotoLibrary phPickerConfiguration  photoLibrary =
withObjCPtr photoLibrary $ \raw_photoLibrary ->
    sendMsg phPickerConfiguration (mkSelector "initWithPhotoLibrary:") (retPtr retVoid) [argPtr (castPtr raw_photoLibrary :: Ptr ())] >>= ownedObject . castPtr

-- | Initializes a new configuration with the system photo library. This configuration never returns asset identifiers.
--
-- ObjC selector: @- init@
init_ :: IsPHPickerConfiguration phPickerConfiguration => phPickerConfiguration -> IO (Id PHPickerConfiguration)
init_ phPickerConfiguration  =
  sendMsg phPickerConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The preferred representation mode of selected assets. Default is @PHPickerConfigurationAssetRepresentationModeAutomatic.@
--
-- Setting @preferredAssetRepresentationMode@ to @PHPickerConfigurationAssetRepresentationModeAutomatic@ means the best representation determined by the system will be used.
--
-- ObjC selector: @- preferredAssetRepresentationMode@
preferredAssetRepresentationMode :: IsPHPickerConfiguration phPickerConfiguration => phPickerConfiguration -> IO PHPickerConfigurationAssetRepresentationMode
preferredAssetRepresentationMode phPickerConfiguration  =
  fmap (coerce :: CLong -> PHPickerConfigurationAssetRepresentationMode) $ sendMsg phPickerConfiguration (mkSelector "preferredAssetRepresentationMode") retCLong []

-- | The preferred representation mode of selected assets. Default is @PHPickerConfigurationAssetRepresentationModeAutomatic.@
--
-- Setting @preferredAssetRepresentationMode@ to @PHPickerConfigurationAssetRepresentationModeAutomatic@ means the best representation determined by the system will be used.
--
-- ObjC selector: @- setPreferredAssetRepresentationMode:@
setPreferredAssetRepresentationMode :: IsPHPickerConfiguration phPickerConfiguration => phPickerConfiguration -> PHPickerConfigurationAssetRepresentationMode -> IO ()
setPreferredAssetRepresentationMode phPickerConfiguration  value =
  sendMsg phPickerConfiguration (mkSelector "setPreferredAssetRepresentationMode:") retVoid [argCLong (coerce value)]

-- | The selection behavior of the picker. Default is @PHPickerConfigurationSelectionDefault.@
--
-- ObjC selector: @- selection@
selection :: IsPHPickerConfiguration phPickerConfiguration => phPickerConfiguration -> IO PHPickerConfigurationSelection
selection phPickerConfiguration  =
  fmap (coerce :: CLong -> PHPickerConfigurationSelection) $ sendMsg phPickerConfiguration (mkSelector "selection") retCLong []

-- | The selection behavior of the picker. Default is @PHPickerConfigurationSelectionDefault.@
--
-- ObjC selector: @- setSelection:@
setSelection :: IsPHPickerConfiguration phPickerConfiguration => phPickerConfiguration -> PHPickerConfigurationSelection -> IO ()
setSelection phPickerConfiguration  value =
  sendMsg phPickerConfiguration (mkSelector "setSelection:") retVoid [argCLong (coerce value)]

-- | The maximum number of assets that can be selected. Default is 1.
--
-- Setting @selectionLimit@ to 0 means maximum supported by the system.
--
-- ObjC selector: @- selectionLimit@
selectionLimit :: IsPHPickerConfiguration phPickerConfiguration => phPickerConfiguration -> IO CLong
selectionLimit phPickerConfiguration  =
  sendMsg phPickerConfiguration (mkSelector "selectionLimit") retCLong []

-- | The maximum number of assets that can be selected. Default is 1.
--
-- Setting @selectionLimit@ to 0 means maximum supported by the system.
--
-- ObjC selector: @- setSelectionLimit:@
setSelectionLimit :: IsPHPickerConfiguration phPickerConfiguration => phPickerConfiguration -> CLong -> IO ()
setSelectionLimit phPickerConfiguration  value =
  sendMsg phPickerConfiguration (mkSelector "setSelectionLimit:") retVoid [argCLong (fromIntegral value)]

-- | The mode of the picker. Default is @PHPickerModeDefault.@
--
-- ObjC selector: @- mode@
mode :: IsPHPickerConfiguration phPickerConfiguration => phPickerConfiguration -> IO PHPickerMode
mode phPickerConfiguration  =
  fmap (coerce :: CLong -> PHPickerMode) $ sendMsg phPickerConfiguration (mkSelector "mode") retCLong []

-- | The mode of the picker. Default is @PHPickerModeDefault.@
--
-- ObjC selector: @- setMode:@
setMode :: IsPHPickerConfiguration phPickerConfiguration => phPickerConfiguration -> PHPickerMode -> IO ()
setMode phPickerConfiguration  value =
  sendMsg phPickerConfiguration (mkSelector "setMode:") retVoid [argCLong (coerce value)]

-- | Edges of the picker that have no margin between the content and the edge (e.g. without bars in between). Default is @NSDirectionalRectEdgeNone.@
--
-- ObjC selector: @- edgesWithoutContentMargins@
edgesWithoutContentMargins :: IsPHPickerConfiguration phPickerConfiguration => phPickerConfiguration -> IO NSDirectionalRectEdge
edgesWithoutContentMargins phPickerConfiguration  =
  fmap (coerce :: CULong -> NSDirectionalRectEdge) $ sendMsg phPickerConfiguration (mkSelector "edgesWithoutContentMargins") retCULong []

-- | Edges of the picker that have no margin between the content and the edge (e.g. without bars in between). Default is @NSDirectionalRectEdgeNone.@
--
-- ObjC selector: @- setEdgesWithoutContentMargins:@
setEdgesWithoutContentMargins :: IsPHPickerConfiguration phPickerConfiguration => phPickerConfiguration -> NSDirectionalRectEdge -> IO ()
setEdgesWithoutContentMargins phPickerConfiguration  value =
  sendMsg phPickerConfiguration (mkSelector "setEdgesWithoutContentMargins:") retVoid [argCULong (coerce value)]

-- | Capabilities of the picker that should be disabled. Default is @PHPickerCapabilitiesNone.@
--
-- ObjC selector: @- disabledCapabilities@
disabledCapabilities :: IsPHPickerConfiguration phPickerConfiguration => phPickerConfiguration -> IO PHPickerCapabilities
disabledCapabilities phPickerConfiguration  =
  fmap (coerce :: CULong -> PHPickerCapabilities) $ sendMsg phPickerConfiguration (mkSelector "disabledCapabilities") retCULong []

-- | Capabilities of the picker that should be disabled. Default is @PHPickerCapabilitiesNone.@
--
-- ObjC selector: @- setDisabledCapabilities:@
setDisabledCapabilities :: IsPHPickerConfiguration phPickerConfiguration => phPickerConfiguration -> PHPickerCapabilities -> IO ()
setDisabledCapabilities phPickerConfiguration  value =
  sendMsg phPickerConfiguration (mkSelector "setDisabledCapabilities:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPhotoLibrary:@
initWithPhotoLibrarySelector :: Selector
initWithPhotoLibrarySelector = mkSelector "initWithPhotoLibrary:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @preferredAssetRepresentationMode@
preferredAssetRepresentationModeSelector :: Selector
preferredAssetRepresentationModeSelector = mkSelector "preferredAssetRepresentationMode"

-- | @Selector@ for @setPreferredAssetRepresentationMode:@
setPreferredAssetRepresentationModeSelector :: Selector
setPreferredAssetRepresentationModeSelector = mkSelector "setPreferredAssetRepresentationMode:"

-- | @Selector@ for @selection@
selectionSelector :: Selector
selectionSelector = mkSelector "selection"

-- | @Selector@ for @setSelection:@
setSelectionSelector :: Selector
setSelectionSelector = mkSelector "setSelection:"

-- | @Selector@ for @selectionLimit@
selectionLimitSelector :: Selector
selectionLimitSelector = mkSelector "selectionLimit"

-- | @Selector@ for @setSelectionLimit:@
setSelectionLimitSelector :: Selector
setSelectionLimitSelector = mkSelector "setSelectionLimit:"

-- | @Selector@ for @mode@
modeSelector :: Selector
modeSelector = mkSelector "mode"

-- | @Selector@ for @setMode:@
setModeSelector :: Selector
setModeSelector = mkSelector "setMode:"

-- | @Selector@ for @edgesWithoutContentMargins@
edgesWithoutContentMarginsSelector :: Selector
edgesWithoutContentMarginsSelector = mkSelector "edgesWithoutContentMargins"

-- | @Selector@ for @setEdgesWithoutContentMargins:@
setEdgesWithoutContentMarginsSelector :: Selector
setEdgesWithoutContentMarginsSelector = mkSelector "setEdgesWithoutContentMargins:"

-- | @Selector@ for @disabledCapabilities@
disabledCapabilitiesSelector :: Selector
disabledCapabilitiesSelector = mkSelector "disabledCapabilities"

-- | @Selector@ for @setDisabledCapabilities:@
setDisabledCapabilitiesSelector :: Selector
setDisabledCapabilitiesSelector = mkSelector "setDisabledCapabilities:"

