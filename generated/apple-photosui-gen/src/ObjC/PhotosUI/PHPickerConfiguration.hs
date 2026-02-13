{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , filter_
  , setFilter
  , preselectedAssetIdentifiers
  , setPreselectedAssetIdentifiers
  , mode
  , setMode
  , edgesWithoutContentMargins
  , setEdgesWithoutContentMargins
  , disabledCapabilities
  , setDisabledCapabilities
  , disabledCapabilitiesSelector
  , edgesWithoutContentMarginsSelector
  , filterSelector
  , initSelector
  , initWithPhotoLibrarySelector
  , modeSelector
  , preferredAssetRepresentationModeSelector
  , preselectedAssetIdentifiersSelector
  , selectionLimitSelector
  , selectionSelector
  , setDisabledCapabilitiesSelector
  , setEdgesWithoutContentMarginsSelector
  , setFilterSelector
  , setModeSelector
  , setPreferredAssetRepresentationModeSelector
  , setPreselectedAssetIdentifiersSelector
  , setSelectionLimitSelector
  , setSelectionSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithPhotoLibrary phPickerConfiguration photoLibrary =
  sendOwnedMessage phPickerConfiguration initWithPhotoLibrarySelector (toPHPhotoLibrary photoLibrary)

-- | Initializes a new configuration with the system photo library. This configuration never returns asset identifiers.
--
-- ObjC selector: @- init@
init_ :: IsPHPickerConfiguration phPickerConfiguration => phPickerConfiguration -> IO (Id PHPickerConfiguration)
init_ phPickerConfiguration =
  sendOwnedMessage phPickerConfiguration initSelector

-- | The preferred representation mode of selected assets. Default is @PHPickerConfigurationAssetRepresentationModeAutomatic.@
--
-- Setting @preferredAssetRepresentationMode@ to @PHPickerConfigurationAssetRepresentationModeAutomatic@ means the best representation determined by the system will be used.
--
-- ObjC selector: @- preferredAssetRepresentationMode@
preferredAssetRepresentationMode :: IsPHPickerConfiguration phPickerConfiguration => phPickerConfiguration -> IO PHPickerConfigurationAssetRepresentationMode
preferredAssetRepresentationMode phPickerConfiguration =
  sendMessage phPickerConfiguration preferredAssetRepresentationModeSelector

-- | The preferred representation mode of selected assets. Default is @PHPickerConfigurationAssetRepresentationModeAutomatic.@
--
-- Setting @preferredAssetRepresentationMode@ to @PHPickerConfigurationAssetRepresentationModeAutomatic@ means the best representation determined by the system will be used.
--
-- ObjC selector: @- setPreferredAssetRepresentationMode:@
setPreferredAssetRepresentationMode :: IsPHPickerConfiguration phPickerConfiguration => phPickerConfiguration -> PHPickerConfigurationAssetRepresentationMode -> IO ()
setPreferredAssetRepresentationMode phPickerConfiguration value =
  sendMessage phPickerConfiguration setPreferredAssetRepresentationModeSelector value

-- | The selection behavior of the picker. Default is @PHPickerConfigurationSelectionDefault.@
--
-- ObjC selector: @- selection@
selection :: IsPHPickerConfiguration phPickerConfiguration => phPickerConfiguration -> IO PHPickerConfigurationSelection
selection phPickerConfiguration =
  sendMessage phPickerConfiguration selectionSelector

-- | The selection behavior of the picker. Default is @PHPickerConfigurationSelectionDefault.@
--
-- ObjC selector: @- setSelection:@
setSelection :: IsPHPickerConfiguration phPickerConfiguration => phPickerConfiguration -> PHPickerConfigurationSelection -> IO ()
setSelection phPickerConfiguration value =
  sendMessage phPickerConfiguration setSelectionSelector value

-- | The maximum number of assets that can be selected. Default is 1.
--
-- Setting @selectionLimit@ to 0 means maximum supported by the system.
--
-- ObjC selector: @- selectionLimit@
selectionLimit :: IsPHPickerConfiguration phPickerConfiguration => phPickerConfiguration -> IO CLong
selectionLimit phPickerConfiguration =
  sendMessage phPickerConfiguration selectionLimitSelector

-- | The maximum number of assets that can be selected. Default is 1.
--
-- Setting @selectionLimit@ to 0 means maximum supported by the system.
--
-- ObjC selector: @- setSelectionLimit:@
setSelectionLimit :: IsPHPickerConfiguration phPickerConfiguration => phPickerConfiguration -> CLong -> IO ()
setSelectionLimit phPickerConfiguration value =
  sendMessage phPickerConfiguration setSelectionLimitSelector value

-- | Types of assets that can be shown. Default is @nil.@
--
-- Setting @filter@ to @nil@ means all asset types can be shown.
--
-- ObjC selector: @- filter@
filter_ :: IsPHPickerConfiguration phPickerConfiguration => phPickerConfiguration -> IO (Id PHPickerFilter)
filter_ phPickerConfiguration =
  sendMessage phPickerConfiguration filterSelector

-- | Types of assets that can be shown. Default is @nil.@
--
-- Setting @filter@ to @nil@ means all asset types can be shown.
--
-- ObjC selector: @- setFilter:@
setFilter :: (IsPHPickerConfiguration phPickerConfiguration, IsPHPickerFilter value) => phPickerConfiguration -> value -> IO ()
setFilter phPickerConfiguration value =
  sendMessage phPickerConfiguration setFilterSelector (toPHPickerFilter value)

-- | Local identifiers of assets to be shown as selected when the picker is presented. Default is an empty array.
--
-- @preselectedAssetIdentifiers@ should be an empty array if @selectionLimit@ is 1 or @photoLibrary@ is not specified. Returned item providers for preselected assets are always empty.
--
-- ObjC selector: @- preselectedAssetIdentifiers@
preselectedAssetIdentifiers :: IsPHPickerConfiguration phPickerConfiguration => phPickerConfiguration -> IO (Id NSArray)
preselectedAssetIdentifiers phPickerConfiguration =
  sendMessage phPickerConfiguration preselectedAssetIdentifiersSelector

-- | Local identifiers of assets to be shown as selected when the picker is presented. Default is an empty array.
--
-- @preselectedAssetIdentifiers@ should be an empty array if @selectionLimit@ is 1 or @photoLibrary@ is not specified. Returned item providers for preselected assets are always empty.
--
-- ObjC selector: @- setPreselectedAssetIdentifiers:@
setPreselectedAssetIdentifiers :: (IsPHPickerConfiguration phPickerConfiguration, IsNSArray value) => phPickerConfiguration -> value -> IO ()
setPreselectedAssetIdentifiers phPickerConfiguration value =
  sendMessage phPickerConfiguration setPreselectedAssetIdentifiersSelector (toNSArray value)

-- | The mode of the picker. Default is @PHPickerModeDefault.@
--
-- ObjC selector: @- mode@
mode :: IsPHPickerConfiguration phPickerConfiguration => phPickerConfiguration -> IO PHPickerMode
mode phPickerConfiguration =
  sendMessage phPickerConfiguration modeSelector

-- | The mode of the picker. Default is @PHPickerModeDefault.@
--
-- ObjC selector: @- setMode:@
setMode :: IsPHPickerConfiguration phPickerConfiguration => phPickerConfiguration -> PHPickerMode -> IO ()
setMode phPickerConfiguration value =
  sendMessage phPickerConfiguration setModeSelector value

-- | Edges of the picker that have no margin between the content and the edge (e.g. without bars in between). Default is @NSDirectionalRectEdgeNone.@
--
-- ObjC selector: @- edgesWithoutContentMargins@
edgesWithoutContentMargins :: IsPHPickerConfiguration phPickerConfiguration => phPickerConfiguration -> IO NSDirectionalRectEdge
edgesWithoutContentMargins phPickerConfiguration =
  sendMessage phPickerConfiguration edgesWithoutContentMarginsSelector

-- | Edges of the picker that have no margin between the content and the edge (e.g. without bars in between). Default is @NSDirectionalRectEdgeNone.@
--
-- ObjC selector: @- setEdgesWithoutContentMargins:@
setEdgesWithoutContentMargins :: IsPHPickerConfiguration phPickerConfiguration => phPickerConfiguration -> NSDirectionalRectEdge -> IO ()
setEdgesWithoutContentMargins phPickerConfiguration value =
  sendMessage phPickerConfiguration setEdgesWithoutContentMarginsSelector value

-- | Capabilities of the picker that should be disabled. Default is @PHPickerCapabilitiesNone.@
--
-- ObjC selector: @- disabledCapabilities@
disabledCapabilities :: IsPHPickerConfiguration phPickerConfiguration => phPickerConfiguration -> IO PHPickerCapabilities
disabledCapabilities phPickerConfiguration =
  sendMessage phPickerConfiguration disabledCapabilitiesSelector

-- | Capabilities of the picker that should be disabled. Default is @PHPickerCapabilitiesNone.@
--
-- ObjC selector: @- setDisabledCapabilities:@
setDisabledCapabilities :: IsPHPickerConfiguration phPickerConfiguration => phPickerConfiguration -> PHPickerCapabilities -> IO ()
setDisabledCapabilities phPickerConfiguration value =
  sendMessage phPickerConfiguration setDisabledCapabilitiesSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPhotoLibrary:@
initWithPhotoLibrarySelector :: Selector '[Id PHPhotoLibrary] (Id PHPickerConfiguration)
initWithPhotoLibrarySelector = mkSelector "initWithPhotoLibrary:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHPickerConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @preferredAssetRepresentationMode@
preferredAssetRepresentationModeSelector :: Selector '[] PHPickerConfigurationAssetRepresentationMode
preferredAssetRepresentationModeSelector = mkSelector "preferredAssetRepresentationMode"

-- | @Selector@ for @setPreferredAssetRepresentationMode:@
setPreferredAssetRepresentationModeSelector :: Selector '[PHPickerConfigurationAssetRepresentationMode] ()
setPreferredAssetRepresentationModeSelector = mkSelector "setPreferredAssetRepresentationMode:"

-- | @Selector@ for @selection@
selectionSelector :: Selector '[] PHPickerConfigurationSelection
selectionSelector = mkSelector "selection"

-- | @Selector@ for @setSelection:@
setSelectionSelector :: Selector '[PHPickerConfigurationSelection] ()
setSelectionSelector = mkSelector "setSelection:"

-- | @Selector@ for @selectionLimit@
selectionLimitSelector :: Selector '[] CLong
selectionLimitSelector = mkSelector "selectionLimit"

-- | @Selector@ for @setSelectionLimit:@
setSelectionLimitSelector :: Selector '[CLong] ()
setSelectionLimitSelector = mkSelector "setSelectionLimit:"

-- | @Selector@ for @filter@
filterSelector :: Selector '[] (Id PHPickerFilter)
filterSelector = mkSelector "filter"

-- | @Selector@ for @setFilter:@
setFilterSelector :: Selector '[Id PHPickerFilter] ()
setFilterSelector = mkSelector "setFilter:"

-- | @Selector@ for @preselectedAssetIdentifiers@
preselectedAssetIdentifiersSelector :: Selector '[] (Id NSArray)
preselectedAssetIdentifiersSelector = mkSelector "preselectedAssetIdentifiers"

-- | @Selector@ for @setPreselectedAssetIdentifiers:@
setPreselectedAssetIdentifiersSelector :: Selector '[Id NSArray] ()
setPreselectedAssetIdentifiersSelector = mkSelector "setPreselectedAssetIdentifiers:"

-- | @Selector@ for @mode@
modeSelector :: Selector '[] PHPickerMode
modeSelector = mkSelector "mode"

-- | @Selector@ for @setMode:@
setModeSelector :: Selector '[PHPickerMode] ()
setModeSelector = mkSelector "setMode:"

-- | @Selector@ for @edgesWithoutContentMargins@
edgesWithoutContentMarginsSelector :: Selector '[] NSDirectionalRectEdge
edgesWithoutContentMarginsSelector = mkSelector "edgesWithoutContentMargins"

-- | @Selector@ for @setEdgesWithoutContentMargins:@
setEdgesWithoutContentMarginsSelector :: Selector '[NSDirectionalRectEdge] ()
setEdgesWithoutContentMarginsSelector = mkSelector "setEdgesWithoutContentMargins:"

-- | @Selector@ for @disabledCapabilities@
disabledCapabilitiesSelector :: Selector '[] PHPickerCapabilities
disabledCapabilitiesSelector = mkSelector "disabledCapabilities"

-- | @Selector@ for @setDisabledCapabilities:@
setDisabledCapabilitiesSelector :: Selector '[PHPickerCapabilities] ()
setDisabledCapabilitiesSelector = mkSelector "setDisabledCapabilities:"

