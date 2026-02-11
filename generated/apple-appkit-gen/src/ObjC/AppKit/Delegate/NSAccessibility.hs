{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSAccessibility@.
--
-- Usage:
--
-- @
-- delegate <- newNSAccessibility defaultNSAccessibilityOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSAccessibility
  ( NSAccessibilityOverrides(..)
  , defaultNSAccessibilityOverrides
  , newNSAccessibility
  ) where

import Foreign.Ptr (Ptr, FunPtr, castPtr, nullPtr)
import Foreign.C.Types
import Foreign.StablePtr (newStablePtr, deRefStablePtr)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.C.String (withCString)
import Foreign.LibFFI (retCULong, argPtr)

import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass, class_createInstance)
import ObjC.Runtime.ClassBuilder (objc_allocateClassPair, objc_registerClassPair)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.MsgSend (sendSuperMsg)
import ObjC.Runtime.StableIvar

-- | Overrides record for @\@protocol NSAccessibility@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSAccessibilityOverrides = NSAccessibilityOverrides
  { _accessibilityCellForColumn_row :: !(Maybe (Int -> Int -> IO RawId))
  , _accessibilityLineForIndex :: !(Maybe (Int -> IO Int))
  , _accessibilityPerformCancel :: !(Maybe (IO Bool))
  , _accessibilityPerformConfirm :: !(Maybe (IO Bool))
  , _accessibilityPerformDecrement :: !(Maybe (IO Bool))
  , _accessibilityPerformDelete :: !(Maybe (IO Bool))
  , _accessibilityPerformIncrement :: !(Maybe (IO Bool))
  , _accessibilityPerformPick :: !(Maybe (IO Bool))
  , _accessibilityPerformPress :: !(Maybe (IO Bool))
  , _accessibilityPerformRaise :: !(Maybe (IO Bool))
  , _accessibilityPerformShowAlternateUI :: !(Maybe (IO Bool))
  , _accessibilityPerformShowDefaultUI :: !(Maybe (IO Bool))
  , _accessibilityPerformShowMenu :: !(Maybe (IO Bool))
  , _isAccessibilitySelectorAllowed :: !(Maybe (Selector -> IO Bool))
  , _accessibilityElement :: !(Maybe (IO Bool))
  , _setAccessibilityElement :: !(Maybe (Bool -> IO ()))
  , _accessibilityFocused :: !(Maybe (IO Bool))
  , _setAccessibilityFocused :: !(Maybe (Bool -> IO ()))
  , _accessibilityTopLevelUIElement :: !(Maybe (IO RawId))
  , _setAccessibilityTopLevelUIElement :: !(Maybe (RawId -> IO ()))
  , _accessibilityURL :: !(Maybe (IO RawId))
  , _setAccessibilityURL :: !(Maybe (RawId -> IO ()))
  , _accessibilityValue :: !(Maybe (IO RawId))
  , _setAccessibilityValue :: !(Maybe (RawId -> IO ()))
  , _accessibilityValueDescription :: !(Maybe (IO RawId))
  , _setAccessibilityValueDescription :: !(Maybe (RawId -> IO ()))
  , _accessibilityVisibleChildren :: !(Maybe (IO RawId))
  , _setAccessibilityVisibleChildren :: !(Maybe (RawId -> IO ()))
  , _accessibilitySubrole :: !(Maybe (IO RawId))
  , _setAccessibilitySubrole :: !(Maybe (RawId -> IO ()))
  , _accessibilityTitle :: !(Maybe (IO RawId))
  , _setAccessibilityTitle :: !(Maybe (RawId -> IO ()))
  , _accessibilityTitleUIElement :: !(Maybe (IO RawId))
  , _setAccessibilityTitleUIElement :: !(Maybe (RawId -> IO ()))
  , _accessibilityNextContents :: !(Maybe (IO RawId))
  , _setAccessibilityNextContents :: !(Maybe (RawId -> IO ()))
  , _accessibilityOverflowButton :: !(Maybe (IO RawId))
  , _setAccessibilityOverflowButton :: !(Maybe (RawId -> IO ()))
  , _accessibilityParent :: !(Maybe (IO RawId))
  , _setAccessibilityParent :: !(Maybe (RawId -> IO ()))
  , _accessibilityPlaceholderValue :: !(Maybe (IO RawId))
  , _setAccessibilityPlaceholderValue :: !(Maybe (RawId -> IO ()))
  , _accessibilityPreviousContents :: !(Maybe (IO RawId))
  , _setAccessibilityPreviousContents :: !(Maybe (RawId -> IO ()))
  , _accessibilityRole :: !(Maybe (IO RawId))
  , _setAccessibilityRole :: !(Maybe (RawId -> IO ()))
  , _accessibilityRoleDescription :: !(Maybe (IO RawId))
  , _setAccessibilityRoleDescription :: !(Maybe (RawId -> IO ()))
  , _accessibilitySearchButton :: !(Maybe (IO RawId))
  , _setAccessibilitySearchButton :: !(Maybe (RawId -> IO ()))
  , _accessibilitySearchMenu :: !(Maybe (IO RawId))
  , _setAccessibilitySearchMenu :: !(Maybe (RawId -> IO ()))
  , _accessibilitySelected :: !(Maybe (IO Bool))
  , _setAccessibilitySelected :: !(Maybe (Bool -> IO ()))
  , _accessibilitySelectedChildren :: !(Maybe (IO RawId))
  , _setAccessibilitySelectedChildren :: !(Maybe (RawId -> IO ()))
  , _accessibilityServesAsTitleForUIElements :: !(Maybe (IO RawId))
  , _setAccessibilityServesAsTitleForUIElements :: !(Maybe (RawId -> IO ()))
  , _accessibilityShownMenu :: !(Maybe (IO RawId))
  , _setAccessibilityShownMenu :: !(Maybe (RawId -> IO ()))
  , _accessibilityMinValue :: !(Maybe (IO RawId))
  , _setAccessibilityMinValue :: !(Maybe (RawId -> IO ()))
  , _accessibilityMaxValue :: !(Maybe (IO RawId))
  , _setAccessibilityMaxValue :: !(Maybe (RawId -> IO ()))
  , _accessibilityLinkedUIElements :: !(Maybe (IO RawId))
  , _setAccessibilityLinkedUIElements :: !(Maybe (RawId -> IO ()))
  , _accessibilityWindow :: !(Maybe (IO RawId))
  , _setAccessibilityWindow :: !(Maybe (RawId -> IO ()))
  , _accessibilityIdentifier :: !(Maybe (IO RawId))
  , _setAccessibilityIdentifier :: !(Maybe (RawId -> IO ()))
  , _accessibilityHelp :: !(Maybe (IO RawId))
  , _setAccessibilityHelp :: !(Maybe (RawId -> IO ()))
  , _accessibilityFilename :: !(Maybe (IO RawId))
  , _setAccessibilityFilename :: !(Maybe (RawId -> IO ()))
  , _accessibilityExpanded :: !(Maybe (IO Bool))
  , _setAccessibilityExpanded :: !(Maybe (Bool -> IO ()))
  , _accessibilityEdited :: !(Maybe (IO Bool))
  , _setAccessibilityEdited :: !(Maybe (Bool -> IO ()))
  , _accessibilityEnabled :: !(Maybe (IO Bool))
  , _setAccessibilityEnabled :: !(Maybe (Bool -> IO ()))
  , _accessibilityChildren :: !(Maybe (IO RawId))
  , _setAccessibilityChildren :: !(Maybe (RawId -> IO ()))
  , _accessibilityChildrenInNavigationOrder :: !(Maybe (IO RawId))
  , _setAccessibilityChildrenInNavigationOrder :: !(Maybe (RawId -> IO ()))
  , _accessibilityClearButton :: !(Maybe (IO RawId))
  , _setAccessibilityClearButton :: !(Maybe (RawId -> IO ()))
  , _accessibilityCancelButton :: !(Maybe (IO RawId))
  , _setAccessibilityCancelButton :: !(Maybe (RawId -> IO ()))
  , _accessibilityProtectedContent :: !(Maybe (IO Bool))
  , _setAccessibilityProtectedContent :: !(Maybe (Bool -> IO ()))
  , _accessibilityContents :: !(Maybe (IO RawId))
  , _setAccessibilityContents :: !(Maybe (RawId -> IO ()))
  , _accessibilityLabel :: !(Maybe (IO RawId))
  , _setAccessibilityLabel :: !(Maybe (RawId -> IO ()))
  , _accessibilityAlternateUIVisible :: !(Maybe (IO Bool))
  , _setAccessibilityAlternateUIVisible :: !(Maybe (Bool -> IO ()))
  , _accessibilitySharedFocusElements :: !(Maybe (IO RawId))
  , _setAccessibilitySharedFocusElements :: !(Maybe (RawId -> IO ()))
  , _accessibilityRequired :: !(Maybe (IO Bool))
  , _setAccessibilityRequired :: !(Maybe (Bool -> IO ()))
  , _accessibilityCustomRotors :: !(Maybe (IO RawId))
  , _setAccessibilityCustomRotors :: !(Maybe (RawId -> IO ()))
  , _accessibilityUserInputLabels :: !(Maybe (IO RawId))
  , _setAccessibilityUserInputLabels :: !(Maybe (RawId -> IO ()))
  , _accessibilityAttributedUserInputLabels :: !(Maybe (IO RawId))
  , _setAccessibilityAttributedUserInputLabels :: !(Maybe (RawId -> IO ()))
  , _accessibilityApplicationFocusedUIElement :: !(Maybe (IO RawId))
  , _setAccessibilityApplicationFocusedUIElement :: !(Maybe (RawId -> IO ()))
  , _accessibilityMainWindow :: !(Maybe (IO RawId))
  , _setAccessibilityMainWindow :: !(Maybe (RawId -> IO ()))
  , _accessibilityHidden :: !(Maybe (IO Bool))
  , _setAccessibilityHidden :: !(Maybe (Bool -> IO ()))
  , _accessibilityFrontmost :: !(Maybe (IO Bool))
  , _setAccessibilityFrontmost :: !(Maybe (Bool -> IO ()))
  , _accessibilityFocusedWindow :: !(Maybe (IO RawId))
  , _setAccessibilityFocusedWindow :: !(Maybe (RawId -> IO ()))
  , _accessibilityWindows :: !(Maybe (IO RawId))
  , _setAccessibilityWindows :: !(Maybe (RawId -> IO ()))
  , _accessibilityExtrasMenuBar :: !(Maybe (IO RawId))
  , _setAccessibilityExtrasMenuBar :: !(Maybe (RawId -> IO ()))
  , _accessibilityMenuBar :: !(Maybe (IO RawId))
  , _setAccessibilityMenuBar :: !(Maybe (RawId -> IO ()))
  , _accessibilityColumnTitles :: !(Maybe (IO RawId))
  , _setAccessibilityColumnTitles :: !(Maybe (RawId -> IO ()))
  , _accessibilityOrderedByRow :: !(Maybe (IO Bool))
  , _setAccessibilityOrderedByRow :: !(Maybe (Bool -> IO ()))
  , _accessibilityHorizontalUnitDescription :: !(Maybe (IO RawId))
  , _setAccessibilityHorizontalUnitDescription :: !(Maybe (RawId -> IO ()))
  , _accessibilityVerticalUnitDescription :: !(Maybe (IO RawId))
  , _setAccessibilityVerticalUnitDescription :: !(Maybe (RawId -> IO ()))
  , _accessibilityHandles :: !(Maybe (IO RawId))
  , _setAccessibilityHandles :: !(Maybe (RawId -> IO ()))
  , _accessibilityWarningValue :: !(Maybe (IO RawId))
  , _setAccessibilityWarningValue :: !(Maybe (RawId -> IO ()))
  , _accessibilityCriticalValue :: !(Maybe (IO RawId))
  , _setAccessibilityCriticalValue :: !(Maybe (RawId -> IO ()))
  , _accessibilityDisclosed :: !(Maybe (IO Bool))
  , _setAccessibilityDisclosed :: !(Maybe (Bool -> IO ()))
  , _accessibilityDisclosedByRow :: !(Maybe (IO RawId))
  , _setAccessibilityDisclosedByRow :: !(Maybe (RawId -> IO ()))
  , _accessibilityDisclosedRows :: !(Maybe (IO RawId))
  , _setAccessibilityDisclosedRows :: !(Maybe (RawId -> IO ()))
  , _accessibilityDisclosureLevel :: !(Maybe (IO Int))
  , _setAccessibilityDisclosureLevel :: !(Maybe (Int -> IO ()))
  , _accessibilityMarkerUIElements :: !(Maybe (IO RawId))
  , _setAccessibilityMarkerUIElements :: !(Maybe (RawId -> IO ()))
  , _accessibilityMarkerValues :: !(Maybe (IO RawId))
  , _setAccessibilityMarkerValues :: !(Maybe (RawId -> IO ()))
  , _accessibilityMarkerGroupUIElement :: !(Maybe (IO RawId))
  , _setAccessibilityMarkerGroupUIElement :: !(Maybe (RawId -> IO ()))
  , _accessibilityUnitDescription :: !(Maybe (IO RawId))
  , _setAccessibilityUnitDescription :: !(Maybe (RawId -> IO ()))
  , _accessibilityMarkerTypeDescription :: !(Maybe (IO RawId))
  , _setAccessibilityMarkerTypeDescription :: !(Maybe (RawId -> IO ()))
  , _accessibilityHorizontalScrollBar :: !(Maybe (IO RawId))
  , _setAccessibilityHorizontalScrollBar :: !(Maybe (RawId -> IO ()))
  , _accessibilityVerticalScrollBar :: !(Maybe (IO RawId))
  , _setAccessibilityVerticalScrollBar :: !(Maybe (RawId -> IO ()))
  , _accessibilityAllowedValues :: !(Maybe (IO RawId))
  , _setAccessibilityAllowedValues :: !(Maybe (RawId -> IO ()))
  , _accessibilityLabelUIElements :: !(Maybe (IO RawId))
  , _setAccessibilityLabelUIElements :: !(Maybe (RawId -> IO ()))
  , _accessibilityLabelValue :: !(Maybe (IO Float))
  , _setAccessibilityLabelValue :: !(Maybe (Float -> IO ()))
  , _accessibilitySplitters :: !(Maybe (IO RawId))
  , _setAccessibilitySplitters :: !(Maybe (RawId -> IO ()))
  , _accessibilityDecrementButton :: !(Maybe (IO RawId))
  , _setAccessibilityDecrementButton :: !(Maybe (RawId -> IO ()))
  , _accessibilityIncrementButton :: !(Maybe (IO RawId))
  , _setAccessibilityIncrementButton :: !(Maybe (RawId -> IO ()))
  , _accessibilityTabs :: !(Maybe (IO RawId))
  , _setAccessibilityTabs :: !(Maybe (RawId -> IO ()))
  , _accessibilityHeader :: !(Maybe (IO RawId))
  , _setAccessibilityHeader :: !(Maybe (RawId -> IO ()))
  , _accessibilityColumnCount :: !(Maybe (IO Int))
  , _setAccessibilityColumnCount :: !(Maybe (Int -> IO ()))
  , _accessibilityRowCount :: !(Maybe (IO Int))
  , _setAccessibilityRowCount :: !(Maybe (Int -> IO ()))
  , _accessibilityIndex :: !(Maybe (IO Int))
  , _setAccessibilityIndex :: !(Maybe (Int -> IO ()))
  , _accessibilityColumns :: !(Maybe (IO RawId))
  , _setAccessibilityColumns :: !(Maybe (RawId -> IO ()))
  , _accessibilityRows :: !(Maybe (IO RawId))
  , _setAccessibilityRows :: !(Maybe (RawId -> IO ()))
  , _accessibilityVisibleRows :: !(Maybe (IO RawId))
  , _setAccessibilityVisibleRows :: !(Maybe (RawId -> IO ()))
  , _accessibilitySelectedRows :: !(Maybe (IO RawId))
  , _setAccessibilitySelectedRows :: !(Maybe (RawId -> IO ()))
  , _accessibilityVisibleColumns :: !(Maybe (IO RawId))
  , _setAccessibilityVisibleColumns :: !(Maybe (RawId -> IO ()))
  , _accessibilitySelectedColumns :: !(Maybe (IO RawId))
  , _setAccessibilitySelectedColumns :: !(Maybe (RawId -> IO ()))
  , _accessibilityRowHeaderUIElements :: !(Maybe (IO RawId))
  , _setAccessibilityRowHeaderUIElements :: !(Maybe (RawId -> IO ()))
  , _accessibilitySelectedCells :: !(Maybe (IO RawId))
  , _setAccessibilitySelectedCells :: !(Maybe (RawId -> IO ()))
  , _accessibilityVisibleCells :: !(Maybe (IO RawId))
  , _setAccessibilityVisibleCells :: !(Maybe (RawId -> IO ()))
  , _accessibilityColumnHeaderUIElements :: !(Maybe (IO RawId))
  , _setAccessibilityColumnHeaderUIElements :: !(Maybe (RawId -> IO ()))
  , _accessibilityInsertionPointLineNumber :: !(Maybe (IO Int))
  , _setAccessibilityInsertionPointLineNumber :: !(Maybe (Int -> IO ()))
  , _accessibilitySharedTextUIElements :: !(Maybe (IO RawId))
  , _setAccessibilitySharedTextUIElements :: !(Maybe (RawId -> IO ()))
  , _accessibilityNumberOfCharacters :: !(Maybe (IO Int))
  , _setAccessibilityNumberOfCharacters :: !(Maybe (Int -> IO ()))
  , _accessibilitySelectedText :: !(Maybe (IO RawId))
  , _setAccessibilitySelectedText :: !(Maybe (RawId -> IO ()))
  , _accessibilitySelectedTextRanges :: !(Maybe (IO RawId))
  , _setAccessibilitySelectedTextRanges :: !(Maybe (RawId -> IO ()))
  , _accessibilityToolbarButton :: !(Maybe (IO RawId))
  , _setAccessibilityToolbarButton :: !(Maybe (RawId -> IO ()))
  , _accessibilityModal :: !(Maybe (IO Bool))
  , _setAccessibilityModal :: !(Maybe (Bool -> IO ()))
  , _accessibilityProxy :: !(Maybe (IO RawId))
  , _setAccessibilityProxy :: !(Maybe (RawId -> IO ()))
  , _accessibilityMain :: !(Maybe (IO Bool))
  , _setAccessibilityMain :: !(Maybe (Bool -> IO ()))
  , _accessibilityFullScreenButton :: !(Maybe (IO RawId))
  , _setAccessibilityFullScreenButton :: !(Maybe (RawId -> IO ()))
  , _accessibilityGrowArea :: !(Maybe (IO RawId))
  , _setAccessibilityGrowArea :: !(Maybe (RawId -> IO ()))
  , _accessibilityDocument :: !(Maybe (IO RawId))
  , _setAccessibilityDocument :: !(Maybe (RawId -> IO ()))
  , _accessibilityDefaultButton :: !(Maybe (IO RawId))
  , _setAccessibilityDefaultButton :: !(Maybe (RawId -> IO ()))
  , _accessibilityCloseButton :: !(Maybe (IO RawId))
  , _setAccessibilityCloseButton :: !(Maybe (RawId -> IO ()))
  , _accessibilityZoomButton :: !(Maybe (IO RawId))
  , _setAccessibilityZoomButton :: !(Maybe (RawId -> IO ()))
  , _accessibilityMinimizeButton :: !(Maybe (IO RawId))
  , _setAccessibilityMinimizeButton :: !(Maybe (RawId -> IO ()))
  , _accessibilityMinimized :: !(Maybe (IO Bool))
  , _setAccessibilityMinimized :: !(Maybe (Bool -> IO ()))
  , _accessibilityCustomActions :: !(Maybe (IO RawId))
  , _setAccessibilityCustomActions :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSAccessibilityOverrides :: NSAccessibilityOverrides
defaultNSAccessibilityOverrides = NSAccessibilityOverrides
  { _accessibilityCellForColumn_row = Nothing
  , _accessibilityLineForIndex = Nothing
  , _accessibilityPerformCancel = Nothing
  , _accessibilityPerformConfirm = Nothing
  , _accessibilityPerformDecrement = Nothing
  , _accessibilityPerformDelete = Nothing
  , _accessibilityPerformIncrement = Nothing
  , _accessibilityPerformPick = Nothing
  , _accessibilityPerformPress = Nothing
  , _accessibilityPerformRaise = Nothing
  , _accessibilityPerformShowAlternateUI = Nothing
  , _accessibilityPerformShowDefaultUI = Nothing
  , _accessibilityPerformShowMenu = Nothing
  , _isAccessibilitySelectorAllowed = Nothing
  , _accessibilityElement = Nothing
  , _setAccessibilityElement = Nothing
  , _accessibilityFocused = Nothing
  , _setAccessibilityFocused = Nothing
  , _accessibilityTopLevelUIElement = Nothing
  , _setAccessibilityTopLevelUIElement = Nothing
  , _accessibilityURL = Nothing
  , _setAccessibilityURL = Nothing
  , _accessibilityValue = Nothing
  , _setAccessibilityValue = Nothing
  , _accessibilityValueDescription = Nothing
  , _setAccessibilityValueDescription = Nothing
  , _accessibilityVisibleChildren = Nothing
  , _setAccessibilityVisibleChildren = Nothing
  , _accessibilitySubrole = Nothing
  , _setAccessibilitySubrole = Nothing
  , _accessibilityTitle = Nothing
  , _setAccessibilityTitle = Nothing
  , _accessibilityTitleUIElement = Nothing
  , _setAccessibilityTitleUIElement = Nothing
  , _accessibilityNextContents = Nothing
  , _setAccessibilityNextContents = Nothing
  , _accessibilityOverflowButton = Nothing
  , _setAccessibilityOverflowButton = Nothing
  , _accessibilityParent = Nothing
  , _setAccessibilityParent = Nothing
  , _accessibilityPlaceholderValue = Nothing
  , _setAccessibilityPlaceholderValue = Nothing
  , _accessibilityPreviousContents = Nothing
  , _setAccessibilityPreviousContents = Nothing
  , _accessibilityRole = Nothing
  , _setAccessibilityRole = Nothing
  , _accessibilityRoleDescription = Nothing
  , _setAccessibilityRoleDescription = Nothing
  , _accessibilitySearchButton = Nothing
  , _setAccessibilitySearchButton = Nothing
  , _accessibilitySearchMenu = Nothing
  , _setAccessibilitySearchMenu = Nothing
  , _accessibilitySelected = Nothing
  , _setAccessibilitySelected = Nothing
  , _accessibilitySelectedChildren = Nothing
  , _setAccessibilitySelectedChildren = Nothing
  , _accessibilityServesAsTitleForUIElements = Nothing
  , _setAccessibilityServesAsTitleForUIElements = Nothing
  , _accessibilityShownMenu = Nothing
  , _setAccessibilityShownMenu = Nothing
  , _accessibilityMinValue = Nothing
  , _setAccessibilityMinValue = Nothing
  , _accessibilityMaxValue = Nothing
  , _setAccessibilityMaxValue = Nothing
  , _accessibilityLinkedUIElements = Nothing
  , _setAccessibilityLinkedUIElements = Nothing
  , _accessibilityWindow = Nothing
  , _setAccessibilityWindow = Nothing
  , _accessibilityIdentifier = Nothing
  , _setAccessibilityIdentifier = Nothing
  , _accessibilityHelp = Nothing
  , _setAccessibilityHelp = Nothing
  , _accessibilityFilename = Nothing
  , _setAccessibilityFilename = Nothing
  , _accessibilityExpanded = Nothing
  , _setAccessibilityExpanded = Nothing
  , _accessibilityEdited = Nothing
  , _setAccessibilityEdited = Nothing
  , _accessibilityEnabled = Nothing
  , _setAccessibilityEnabled = Nothing
  , _accessibilityChildren = Nothing
  , _setAccessibilityChildren = Nothing
  , _accessibilityChildrenInNavigationOrder = Nothing
  , _setAccessibilityChildrenInNavigationOrder = Nothing
  , _accessibilityClearButton = Nothing
  , _setAccessibilityClearButton = Nothing
  , _accessibilityCancelButton = Nothing
  , _setAccessibilityCancelButton = Nothing
  , _accessibilityProtectedContent = Nothing
  , _setAccessibilityProtectedContent = Nothing
  , _accessibilityContents = Nothing
  , _setAccessibilityContents = Nothing
  , _accessibilityLabel = Nothing
  , _setAccessibilityLabel = Nothing
  , _accessibilityAlternateUIVisible = Nothing
  , _setAccessibilityAlternateUIVisible = Nothing
  , _accessibilitySharedFocusElements = Nothing
  , _setAccessibilitySharedFocusElements = Nothing
  , _accessibilityRequired = Nothing
  , _setAccessibilityRequired = Nothing
  , _accessibilityCustomRotors = Nothing
  , _setAccessibilityCustomRotors = Nothing
  , _accessibilityUserInputLabels = Nothing
  , _setAccessibilityUserInputLabels = Nothing
  , _accessibilityAttributedUserInputLabels = Nothing
  , _setAccessibilityAttributedUserInputLabels = Nothing
  , _accessibilityApplicationFocusedUIElement = Nothing
  , _setAccessibilityApplicationFocusedUIElement = Nothing
  , _accessibilityMainWindow = Nothing
  , _setAccessibilityMainWindow = Nothing
  , _accessibilityHidden = Nothing
  , _setAccessibilityHidden = Nothing
  , _accessibilityFrontmost = Nothing
  , _setAccessibilityFrontmost = Nothing
  , _accessibilityFocusedWindow = Nothing
  , _setAccessibilityFocusedWindow = Nothing
  , _accessibilityWindows = Nothing
  , _setAccessibilityWindows = Nothing
  , _accessibilityExtrasMenuBar = Nothing
  , _setAccessibilityExtrasMenuBar = Nothing
  , _accessibilityMenuBar = Nothing
  , _setAccessibilityMenuBar = Nothing
  , _accessibilityColumnTitles = Nothing
  , _setAccessibilityColumnTitles = Nothing
  , _accessibilityOrderedByRow = Nothing
  , _setAccessibilityOrderedByRow = Nothing
  , _accessibilityHorizontalUnitDescription = Nothing
  , _setAccessibilityHorizontalUnitDescription = Nothing
  , _accessibilityVerticalUnitDescription = Nothing
  , _setAccessibilityVerticalUnitDescription = Nothing
  , _accessibilityHandles = Nothing
  , _setAccessibilityHandles = Nothing
  , _accessibilityWarningValue = Nothing
  , _setAccessibilityWarningValue = Nothing
  , _accessibilityCriticalValue = Nothing
  , _setAccessibilityCriticalValue = Nothing
  , _accessibilityDisclosed = Nothing
  , _setAccessibilityDisclosed = Nothing
  , _accessibilityDisclosedByRow = Nothing
  , _setAccessibilityDisclosedByRow = Nothing
  , _accessibilityDisclosedRows = Nothing
  , _setAccessibilityDisclosedRows = Nothing
  , _accessibilityDisclosureLevel = Nothing
  , _setAccessibilityDisclosureLevel = Nothing
  , _accessibilityMarkerUIElements = Nothing
  , _setAccessibilityMarkerUIElements = Nothing
  , _accessibilityMarkerValues = Nothing
  , _setAccessibilityMarkerValues = Nothing
  , _accessibilityMarkerGroupUIElement = Nothing
  , _setAccessibilityMarkerGroupUIElement = Nothing
  , _accessibilityUnitDescription = Nothing
  , _setAccessibilityUnitDescription = Nothing
  , _accessibilityMarkerTypeDescription = Nothing
  , _setAccessibilityMarkerTypeDescription = Nothing
  , _accessibilityHorizontalScrollBar = Nothing
  , _setAccessibilityHorizontalScrollBar = Nothing
  , _accessibilityVerticalScrollBar = Nothing
  , _setAccessibilityVerticalScrollBar = Nothing
  , _accessibilityAllowedValues = Nothing
  , _setAccessibilityAllowedValues = Nothing
  , _accessibilityLabelUIElements = Nothing
  , _setAccessibilityLabelUIElements = Nothing
  , _accessibilityLabelValue = Nothing
  , _setAccessibilityLabelValue = Nothing
  , _accessibilitySplitters = Nothing
  , _setAccessibilitySplitters = Nothing
  , _accessibilityDecrementButton = Nothing
  , _setAccessibilityDecrementButton = Nothing
  , _accessibilityIncrementButton = Nothing
  , _setAccessibilityIncrementButton = Nothing
  , _accessibilityTabs = Nothing
  , _setAccessibilityTabs = Nothing
  , _accessibilityHeader = Nothing
  , _setAccessibilityHeader = Nothing
  , _accessibilityColumnCount = Nothing
  , _setAccessibilityColumnCount = Nothing
  , _accessibilityRowCount = Nothing
  , _setAccessibilityRowCount = Nothing
  , _accessibilityIndex = Nothing
  , _setAccessibilityIndex = Nothing
  , _accessibilityColumns = Nothing
  , _setAccessibilityColumns = Nothing
  , _accessibilityRows = Nothing
  , _setAccessibilityRows = Nothing
  , _accessibilityVisibleRows = Nothing
  , _setAccessibilityVisibleRows = Nothing
  , _accessibilitySelectedRows = Nothing
  , _setAccessibilitySelectedRows = Nothing
  , _accessibilityVisibleColumns = Nothing
  , _setAccessibilityVisibleColumns = Nothing
  , _accessibilitySelectedColumns = Nothing
  , _setAccessibilitySelectedColumns = Nothing
  , _accessibilityRowHeaderUIElements = Nothing
  , _setAccessibilityRowHeaderUIElements = Nothing
  , _accessibilitySelectedCells = Nothing
  , _setAccessibilitySelectedCells = Nothing
  , _accessibilityVisibleCells = Nothing
  , _setAccessibilityVisibleCells = Nothing
  , _accessibilityColumnHeaderUIElements = Nothing
  , _setAccessibilityColumnHeaderUIElements = Nothing
  , _accessibilityInsertionPointLineNumber = Nothing
  , _setAccessibilityInsertionPointLineNumber = Nothing
  , _accessibilitySharedTextUIElements = Nothing
  , _setAccessibilitySharedTextUIElements = Nothing
  , _accessibilityNumberOfCharacters = Nothing
  , _setAccessibilityNumberOfCharacters = Nothing
  , _accessibilitySelectedText = Nothing
  , _setAccessibilitySelectedText = Nothing
  , _accessibilitySelectedTextRanges = Nothing
  , _setAccessibilitySelectedTextRanges = Nothing
  , _accessibilityToolbarButton = Nothing
  , _setAccessibilityToolbarButton = Nothing
  , _accessibilityModal = Nothing
  , _setAccessibilityModal = Nothing
  , _accessibilityProxy = Nothing
  , _setAccessibilityProxy = Nothing
  , _accessibilityMain = Nothing
  , _setAccessibilityMain = Nothing
  , _accessibilityFullScreenButton = Nothing
  , _setAccessibilityFullScreenButton = Nothing
  , _accessibilityGrowArea = Nothing
  , _setAccessibilityGrowArea = Nothing
  , _accessibilityDocument = Nothing
  , _setAccessibilityDocument = Nothing
  , _accessibilityDefaultButton = Nothing
  , _setAccessibilityDefaultButton = Nothing
  , _accessibilityCloseButton = Nothing
  , _setAccessibilityCloseButton = Nothing
  , _accessibilityZoomButton = Nothing
  , _setAccessibilityZoomButton = Nothing
  , _accessibilityMinimizeButton = Nothing
  , _setAccessibilityMinimizeButton = Nothing
  , _accessibilityMinimized = Nothing
  , _setAccessibilityMinimized = Nothing
  , _accessibilityCustomActions = Nothing
  , _setAccessibilityCustomActions = Nothing
  }

foreign import ccall "wrapper"
  wrap_f_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CFloat -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CFloat -> IO ()))

foreign import ccall "wrapper"
  wrap_f
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CFloat)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CFloat))

foreign import ccall "wrapper"
  wrap_q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CLong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CLong -> IO ()))

foreign import ccall "wrapper"
  wrap_q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CLong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CLong))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_B_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_sel_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

foreign import ccall "wrapper"
  wrap_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))

foreign import ccall "wrapper"
  wrap_q_q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CLong -> IO CLong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CLong -> IO CLong))

foreign import ccall "wrapper"
  wrap_q_q_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CLong -> CLong -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CLong -> CLong -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsAccessibilityDelegateClass #-}
nsAccessibilityDelegateClass :: Class
nsAccessibilityDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSAccessibility" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_accessibilityCellForColumn_row = unSelector (mkSelector "accessibilityCellForColumn:row:")
      sel_accessibilityLineForIndex = unSelector (mkSelector "accessibilityLineForIndex:")
      sel_accessibilityPerformCancel = unSelector (mkSelector "accessibilityPerformCancel")
      sel_accessibilityPerformConfirm = unSelector (mkSelector "accessibilityPerformConfirm")
      sel_accessibilityPerformDecrement = unSelector (mkSelector "accessibilityPerformDecrement")
      sel_accessibilityPerformDelete = unSelector (mkSelector "accessibilityPerformDelete")
      sel_accessibilityPerformIncrement = unSelector (mkSelector "accessibilityPerformIncrement")
      sel_accessibilityPerformPick = unSelector (mkSelector "accessibilityPerformPick")
      sel_accessibilityPerformPress = unSelector (mkSelector "accessibilityPerformPress")
      sel_accessibilityPerformRaise = unSelector (mkSelector "accessibilityPerformRaise")
      sel_accessibilityPerformShowAlternateUI = unSelector (mkSelector "accessibilityPerformShowAlternateUI")
      sel_accessibilityPerformShowDefaultUI = unSelector (mkSelector "accessibilityPerformShowDefaultUI")
      sel_accessibilityPerformShowMenu = unSelector (mkSelector "accessibilityPerformShowMenu")
      sel_isAccessibilitySelectorAllowed = unSelector (mkSelector "isAccessibilitySelectorAllowed:")
      sel_accessibilityElement = unSelector (mkSelector "accessibilityElement")
      sel_setAccessibilityElement = unSelector (mkSelector "setAccessibilityElement:")
      sel_accessibilityFocused = unSelector (mkSelector "accessibilityFocused")
      sel_setAccessibilityFocused = unSelector (mkSelector "setAccessibilityFocused:")
      sel_accessibilityTopLevelUIElement = unSelector (mkSelector "accessibilityTopLevelUIElement")
      sel_setAccessibilityTopLevelUIElement = unSelector (mkSelector "setAccessibilityTopLevelUIElement:")
      sel_accessibilityURL = unSelector (mkSelector "accessibilityURL")
      sel_setAccessibilityURL = unSelector (mkSelector "setAccessibilityURL:")
      sel_accessibilityValue = unSelector (mkSelector "accessibilityValue")
      sel_setAccessibilityValue = unSelector (mkSelector "setAccessibilityValue:")
      sel_accessibilityValueDescription = unSelector (mkSelector "accessibilityValueDescription")
      sel_setAccessibilityValueDescription = unSelector (mkSelector "setAccessibilityValueDescription:")
      sel_accessibilityVisibleChildren = unSelector (mkSelector "accessibilityVisibleChildren")
      sel_setAccessibilityVisibleChildren = unSelector (mkSelector "setAccessibilityVisibleChildren:")
      sel_accessibilitySubrole = unSelector (mkSelector "accessibilitySubrole")
      sel_setAccessibilitySubrole = unSelector (mkSelector "setAccessibilitySubrole:")
      sel_accessibilityTitle = unSelector (mkSelector "accessibilityTitle")
      sel_setAccessibilityTitle = unSelector (mkSelector "setAccessibilityTitle:")
      sel_accessibilityTitleUIElement = unSelector (mkSelector "accessibilityTitleUIElement")
      sel_setAccessibilityTitleUIElement = unSelector (mkSelector "setAccessibilityTitleUIElement:")
      sel_accessibilityNextContents = unSelector (mkSelector "accessibilityNextContents")
      sel_setAccessibilityNextContents = unSelector (mkSelector "setAccessibilityNextContents:")
      sel_accessibilityOverflowButton = unSelector (mkSelector "accessibilityOverflowButton")
      sel_setAccessibilityOverflowButton = unSelector (mkSelector "setAccessibilityOverflowButton:")
      sel_accessibilityParent = unSelector (mkSelector "accessibilityParent")
      sel_setAccessibilityParent = unSelector (mkSelector "setAccessibilityParent:")
      sel_accessibilityPlaceholderValue = unSelector (mkSelector "accessibilityPlaceholderValue")
      sel_setAccessibilityPlaceholderValue = unSelector (mkSelector "setAccessibilityPlaceholderValue:")
      sel_accessibilityPreviousContents = unSelector (mkSelector "accessibilityPreviousContents")
      sel_setAccessibilityPreviousContents = unSelector (mkSelector "setAccessibilityPreviousContents:")
      sel_accessibilityRole = unSelector (mkSelector "accessibilityRole")
      sel_setAccessibilityRole = unSelector (mkSelector "setAccessibilityRole:")
      sel_accessibilityRoleDescription = unSelector (mkSelector "accessibilityRoleDescription")
      sel_setAccessibilityRoleDescription = unSelector (mkSelector "setAccessibilityRoleDescription:")
      sel_accessibilitySearchButton = unSelector (mkSelector "accessibilitySearchButton")
      sel_setAccessibilitySearchButton = unSelector (mkSelector "setAccessibilitySearchButton:")
      sel_accessibilitySearchMenu = unSelector (mkSelector "accessibilitySearchMenu")
      sel_setAccessibilitySearchMenu = unSelector (mkSelector "setAccessibilitySearchMenu:")
      sel_accessibilitySelected = unSelector (mkSelector "accessibilitySelected")
      sel_setAccessibilitySelected = unSelector (mkSelector "setAccessibilitySelected:")
      sel_accessibilitySelectedChildren = unSelector (mkSelector "accessibilitySelectedChildren")
      sel_setAccessibilitySelectedChildren = unSelector (mkSelector "setAccessibilitySelectedChildren:")
      sel_accessibilityServesAsTitleForUIElements = unSelector (mkSelector "accessibilityServesAsTitleForUIElements")
      sel_setAccessibilityServesAsTitleForUIElements = unSelector (mkSelector "setAccessibilityServesAsTitleForUIElements:")
      sel_accessibilityShownMenu = unSelector (mkSelector "accessibilityShownMenu")
      sel_setAccessibilityShownMenu = unSelector (mkSelector "setAccessibilityShownMenu:")
      sel_accessibilityMinValue = unSelector (mkSelector "accessibilityMinValue")
      sel_setAccessibilityMinValue = unSelector (mkSelector "setAccessibilityMinValue:")
      sel_accessibilityMaxValue = unSelector (mkSelector "accessibilityMaxValue")
      sel_setAccessibilityMaxValue = unSelector (mkSelector "setAccessibilityMaxValue:")
      sel_accessibilityLinkedUIElements = unSelector (mkSelector "accessibilityLinkedUIElements")
      sel_setAccessibilityLinkedUIElements = unSelector (mkSelector "setAccessibilityLinkedUIElements:")
      sel_accessibilityWindow = unSelector (mkSelector "accessibilityWindow")
      sel_setAccessibilityWindow = unSelector (mkSelector "setAccessibilityWindow:")
      sel_accessibilityIdentifier = unSelector (mkSelector "accessibilityIdentifier")
      sel_setAccessibilityIdentifier = unSelector (mkSelector "setAccessibilityIdentifier:")
      sel_accessibilityHelp = unSelector (mkSelector "accessibilityHelp")
      sel_setAccessibilityHelp = unSelector (mkSelector "setAccessibilityHelp:")
      sel_accessibilityFilename = unSelector (mkSelector "accessibilityFilename")
      sel_setAccessibilityFilename = unSelector (mkSelector "setAccessibilityFilename:")
      sel_accessibilityExpanded = unSelector (mkSelector "accessibilityExpanded")
      sel_setAccessibilityExpanded = unSelector (mkSelector "setAccessibilityExpanded:")
      sel_accessibilityEdited = unSelector (mkSelector "accessibilityEdited")
      sel_setAccessibilityEdited = unSelector (mkSelector "setAccessibilityEdited:")
      sel_accessibilityEnabled = unSelector (mkSelector "accessibilityEnabled")
      sel_setAccessibilityEnabled = unSelector (mkSelector "setAccessibilityEnabled:")
      sel_accessibilityChildren = unSelector (mkSelector "accessibilityChildren")
      sel_setAccessibilityChildren = unSelector (mkSelector "setAccessibilityChildren:")
      sel_accessibilityChildrenInNavigationOrder = unSelector (mkSelector "accessibilityChildrenInNavigationOrder")
      sel_setAccessibilityChildrenInNavigationOrder = unSelector (mkSelector "setAccessibilityChildrenInNavigationOrder:")
      sel_accessibilityClearButton = unSelector (mkSelector "accessibilityClearButton")
      sel_setAccessibilityClearButton = unSelector (mkSelector "setAccessibilityClearButton:")
      sel_accessibilityCancelButton = unSelector (mkSelector "accessibilityCancelButton")
      sel_setAccessibilityCancelButton = unSelector (mkSelector "setAccessibilityCancelButton:")
      sel_accessibilityProtectedContent = unSelector (mkSelector "accessibilityProtectedContent")
      sel_setAccessibilityProtectedContent = unSelector (mkSelector "setAccessibilityProtectedContent:")
      sel_accessibilityContents = unSelector (mkSelector "accessibilityContents")
      sel_setAccessibilityContents = unSelector (mkSelector "setAccessibilityContents:")
      sel_accessibilityLabel = unSelector (mkSelector "accessibilityLabel")
      sel_setAccessibilityLabel = unSelector (mkSelector "setAccessibilityLabel:")
      sel_accessibilityAlternateUIVisible = unSelector (mkSelector "accessibilityAlternateUIVisible")
      sel_setAccessibilityAlternateUIVisible = unSelector (mkSelector "setAccessibilityAlternateUIVisible:")
      sel_accessibilitySharedFocusElements = unSelector (mkSelector "accessibilitySharedFocusElements")
      sel_setAccessibilitySharedFocusElements = unSelector (mkSelector "setAccessibilitySharedFocusElements:")
      sel_accessibilityRequired = unSelector (mkSelector "accessibilityRequired")
      sel_setAccessibilityRequired = unSelector (mkSelector "setAccessibilityRequired:")
      sel_accessibilityCustomRotors = unSelector (mkSelector "accessibilityCustomRotors")
      sel_setAccessibilityCustomRotors = unSelector (mkSelector "setAccessibilityCustomRotors:")
      sel_accessibilityUserInputLabels = unSelector (mkSelector "accessibilityUserInputLabels")
      sel_setAccessibilityUserInputLabels = unSelector (mkSelector "setAccessibilityUserInputLabels:")
      sel_accessibilityAttributedUserInputLabels = unSelector (mkSelector "accessibilityAttributedUserInputLabels")
      sel_setAccessibilityAttributedUserInputLabels = unSelector (mkSelector "setAccessibilityAttributedUserInputLabels:")
      sel_accessibilityApplicationFocusedUIElement = unSelector (mkSelector "accessibilityApplicationFocusedUIElement")
      sel_setAccessibilityApplicationFocusedUIElement = unSelector (mkSelector "setAccessibilityApplicationFocusedUIElement:")
      sel_accessibilityMainWindow = unSelector (mkSelector "accessibilityMainWindow")
      sel_setAccessibilityMainWindow = unSelector (mkSelector "setAccessibilityMainWindow:")
      sel_accessibilityHidden = unSelector (mkSelector "accessibilityHidden")
      sel_setAccessibilityHidden = unSelector (mkSelector "setAccessibilityHidden:")
      sel_accessibilityFrontmost = unSelector (mkSelector "accessibilityFrontmost")
      sel_setAccessibilityFrontmost = unSelector (mkSelector "setAccessibilityFrontmost:")
      sel_accessibilityFocusedWindow = unSelector (mkSelector "accessibilityFocusedWindow")
      sel_setAccessibilityFocusedWindow = unSelector (mkSelector "setAccessibilityFocusedWindow:")
      sel_accessibilityWindows = unSelector (mkSelector "accessibilityWindows")
      sel_setAccessibilityWindows = unSelector (mkSelector "setAccessibilityWindows:")
      sel_accessibilityExtrasMenuBar = unSelector (mkSelector "accessibilityExtrasMenuBar")
      sel_setAccessibilityExtrasMenuBar = unSelector (mkSelector "setAccessibilityExtrasMenuBar:")
      sel_accessibilityMenuBar = unSelector (mkSelector "accessibilityMenuBar")
      sel_setAccessibilityMenuBar = unSelector (mkSelector "setAccessibilityMenuBar:")
      sel_accessibilityColumnTitles = unSelector (mkSelector "accessibilityColumnTitles")
      sel_setAccessibilityColumnTitles = unSelector (mkSelector "setAccessibilityColumnTitles:")
      sel_accessibilityOrderedByRow = unSelector (mkSelector "accessibilityOrderedByRow")
      sel_setAccessibilityOrderedByRow = unSelector (mkSelector "setAccessibilityOrderedByRow:")
      sel_accessibilityHorizontalUnitDescription = unSelector (mkSelector "accessibilityHorizontalUnitDescription")
      sel_setAccessibilityHorizontalUnitDescription = unSelector (mkSelector "setAccessibilityHorizontalUnitDescription:")
      sel_accessibilityVerticalUnitDescription = unSelector (mkSelector "accessibilityVerticalUnitDescription")
      sel_setAccessibilityVerticalUnitDescription = unSelector (mkSelector "setAccessibilityVerticalUnitDescription:")
      sel_accessibilityHandles = unSelector (mkSelector "accessibilityHandles")
      sel_setAccessibilityHandles = unSelector (mkSelector "setAccessibilityHandles:")
      sel_accessibilityWarningValue = unSelector (mkSelector "accessibilityWarningValue")
      sel_setAccessibilityWarningValue = unSelector (mkSelector "setAccessibilityWarningValue:")
      sel_accessibilityCriticalValue = unSelector (mkSelector "accessibilityCriticalValue")
      sel_setAccessibilityCriticalValue = unSelector (mkSelector "setAccessibilityCriticalValue:")
      sel_accessibilityDisclosed = unSelector (mkSelector "accessibilityDisclosed")
      sel_setAccessibilityDisclosed = unSelector (mkSelector "setAccessibilityDisclosed:")
      sel_accessibilityDisclosedByRow = unSelector (mkSelector "accessibilityDisclosedByRow")
      sel_setAccessibilityDisclosedByRow = unSelector (mkSelector "setAccessibilityDisclosedByRow:")
      sel_accessibilityDisclosedRows = unSelector (mkSelector "accessibilityDisclosedRows")
      sel_setAccessibilityDisclosedRows = unSelector (mkSelector "setAccessibilityDisclosedRows:")
      sel_accessibilityDisclosureLevel = unSelector (mkSelector "accessibilityDisclosureLevel")
      sel_setAccessibilityDisclosureLevel = unSelector (mkSelector "setAccessibilityDisclosureLevel:")
      sel_accessibilityMarkerUIElements = unSelector (mkSelector "accessibilityMarkerUIElements")
      sel_setAccessibilityMarkerUIElements = unSelector (mkSelector "setAccessibilityMarkerUIElements:")
      sel_accessibilityMarkerValues = unSelector (mkSelector "accessibilityMarkerValues")
      sel_setAccessibilityMarkerValues = unSelector (mkSelector "setAccessibilityMarkerValues:")
      sel_accessibilityMarkerGroupUIElement = unSelector (mkSelector "accessibilityMarkerGroupUIElement")
      sel_setAccessibilityMarkerGroupUIElement = unSelector (mkSelector "setAccessibilityMarkerGroupUIElement:")
      sel_accessibilityUnitDescription = unSelector (mkSelector "accessibilityUnitDescription")
      sel_setAccessibilityUnitDescription = unSelector (mkSelector "setAccessibilityUnitDescription:")
      sel_accessibilityMarkerTypeDescription = unSelector (mkSelector "accessibilityMarkerTypeDescription")
      sel_setAccessibilityMarkerTypeDescription = unSelector (mkSelector "setAccessibilityMarkerTypeDescription:")
      sel_accessibilityHorizontalScrollBar = unSelector (mkSelector "accessibilityHorizontalScrollBar")
      sel_setAccessibilityHorizontalScrollBar = unSelector (mkSelector "setAccessibilityHorizontalScrollBar:")
      sel_accessibilityVerticalScrollBar = unSelector (mkSelector "accessibilityVerticalScrollBar")
      sel_setAccessibilityVerticalScrollBar = unSelector (mkSelector "setAccessibilityVerticalScrollBar:")
      sel_accessibilityAllowedValues = unSelector (mkSelector "accessibilityAllowedValues")
      sel_setAccessibilityAllowedValues = unSelector (mkSelector "setAccessibilityAllowedValues:")
      sel_accessibilityLabelUIElements = unSelector (mkSelector "accessibilityLabelUIElements")
      sel_setAccessibilityLabelUIElements = unSelector (mkSelector "setAccessibilityLabelUIElements:")
      sel_accessibilityLabelValue = unSelector (mkSelector "accessibilityLabelValue")
      sel_setAccessibilityLabelValue = unSelector (mkSelector "setAccessibilityLabelValue:")
      sel_accessibilitySplitters = unSelector (mkSelector "accessibilitySplitters")
      sel_setAccessibilitySplitters = unSelector (mkSelector "setAccessibilitySplitters:")
      sel_accessibilityDecrementButton = unSelector (mkSelector "accessibilityDecrementButton")
      sel_setAccessibilityDecrementButton = unSelector (mkSelector "setAccessibilityDecrementButton:")
      sel_accessibilityIncrementButton = unSelector (mkSelector "accessibilityIncrementButton")
      sel_setAccessibilityIncrementButton = unSelector (mkSelector "setAccessibilityIncrementButton:")
      sel_accessibilityTabs = unSelector (mkSelector "accessibilityTabs")
      sel_setAccessibilityTabs = unSelector (mkSelector "setAccessibilityTabs:")
      sel_accessibilityHeader = unSelector (mkSelector "accessibilityHeader")
      sel_setAccessibilityHeader = unSelector (mkSelector "setAccessibilityHeader:")
      sel_accessibilityColumnCount = unSelector (mkSelector "accessibilityColumnCount")
      sel_setAccessibilityColumnCount = unSelector (mkSelector "setAccessibilityColumnCount:")
      sel_accessibilityRowCount = unSelector (mkSelector "accessibilityRowCount")
      sel_setAccessibilityRowCount = unSelector (mkSelector "setAccessibilityRowCount:")
      sel_accessibilityIndex = unSelector (mkSelector "accessibilityIndex")
      sel_setAccessibilityIndex = unSelector (mkSelector "setAccessibilityIndex:")
      sel_accessibilityColumns = unSelector (mkSelector "accessibilityColumns")
      sel_setAccessibilityColumns = unSelector (mkSelector "setAccessibilityColumns:")
      sel_accessibilityRows = unSelector (mkSelector "accessibilityRows")
      sel_setAccessibilityRows = unSelector (mkSelector "setAccessibilityRows:")
      sel_accessibilityVisibleRows = unSelector (mkSelector "accessibilityVisibleRows")
      sel_setAccessibilityVisibleRows = unSelector (mkSelector "setAccessibilityVisibleRows:")
      sel_accessibilitySelectedRows = unSelector (mkSelector "accessibilitySelectedRows")
      sel_setAccessibilitySelectedRows = unSelector (mkSelector "setAccessibilitySelectedRows:")
      sel_accessibilityVisibleColumns = unSelector (mkSelector "accessibilityVisibleColumns")
      sel_setAccessibilityVisibleColumns = unSelector (mkSelector "setAccessibilityVisibleColumns:")
      sel_accessibilitySelectedColumns = unSelector (mkSelector "accessibilitySelectedColumns")
      sel_setAccessibilitySelectedColumns = unSelector (mkSelector "setAccessibilitySelectedColumns:")
      sel_accessibilityRowHeaderUIElements = unSelector (mkSelector "accessibilityRowHeaderUIElements")
      sel_setAccessibilityRowHeaderUIElements = unSelector (mkSelector "setAccessibilityRowHeaderUIElements:")
      sel_accessibilitySelectedCells = unSelector (mkSelector "accessibilitySelectedCells")
      sel_setAccessibilitySelectedCells = unSelector (mkSelector "setAccessibilitySelectedCells:")
      sel_accessibilityVisibleCells = unSelector (mkSelector "accessibilityVisibleCells")
      sel_setAccessibilityVisibleCells = unSelector (mkSelector "setAccessibilityVisibleCells:")
      sel_accessibilityColumnHeaderUIElements = unSelector (mkSelector "accessibilityColumnHeaderUIElements")
      sel_setAccessibilityColumnHeaderUIElements = unSelector (mkSelector "setAccessibilityColumnHeaderUIElements:")
      sel_accessibilityInsertionPointLineNumber = unSelector (mkSelector "accessibilityInsertionPointLineNumber")
      sel_setAccessibilityInsertionPointLineNumber = unSelector (mkSelector "setAccessibilityInsertionPointLineNumber:")
      sel_accessibilitySharedTextUIElements = unSelector (mkSelector "accessibilitySharedTextUIElements")
      sel_setAccessibilitySharedTextUIElements = unSelector (mkSelector "setAccessibilitySharedTextUIElements:")
      sel_accessibilityNumberOfCharacters = unSelector (mkSelector "accessibilityNumberOfCharacters")
      sel_setAccessibilityNumberOfCharacters = unSelector (mkSelector "setAccessibilityNumberOfCharacters:")
      sel_accessibilitySelectedText = unSelector (mkSelector "accessibilitySelectedText")
      sel_setAccessibilitySelectedText = unSelector (mkSelector "setAccessibilitySelectedText:")
      sel_accessibilitySelectedTextRanges = unSelector (mkSelector "accessibilitySelectedTextRanges")
      sel_setAccessibilitySelectedTextRanges = unSelector (mkSelector "setAccessibilitySelectedTextRanges:")
      sel_accessibilityToolbarButton = unSelector (mkSelector "accessibilityToolbarButton")
      sel_setAccessibilityToolbarButton = unSelector (mkSelector "setAccessibilityToolbarButton:")
      sel_accessibilityModal = unSelector (mkSelector "accessibilityModal")
      sel_setAccessibilityModal = unSelector (mkSelector "setAccessibilityModal:")
      sel_accessibilityProxy = unSelector (mkSelector "accessibilityProxy")
      sel_setAccessibilityProxy = unSelector (mkSelector "setAccessibilityProxy:")
      sel_accessibilityMain = unSelector (mkSelector "accessibilityMain")
      sel_setAccessibilityMain = unSelector (mkSelector "setAccessibilityMain:")
      sel_accessibilityFullScreenButton = unSelector (mkSelector "accessibilityFullScreenButton")
      sel_setAccessibilityFullScreenButton = unSelector (mkSelector "setAccessibilityFullScreenButton:")
      sel_accessibilityGrowArea = unSelector (mkSelector "accessibilityGrowArea")
      sel_setAccessibilityGrowArea = unSelector (mkSelector "setAccessibilityGrowArea:")
      sel_accessibilityDocument = unSelector (mkSelector "accessibilityDocument")
      sel_setAccessibilityDocument = unSelector (mkSelector "setAccessibilityDocument:")
      sel_accessibilityDefaultButton = unSelector (mkSelector "accessibilityDefaultButton")
      sel_setAccessibilityDefaultButton = unSelector (mkSelector "setAccessibilityDefaultButton:")
      sel_accessibilityCloseButton = unSelector (mkSelector "accessibilityCloseButton")
      sel_setAccessibilityCloseButton = unSelector (mkSelector "setAccessibilityCloseButton:")
      sel_accessibilityZoomButton = unSelector (mkSelector "accessibilityZoomButton")
      sel_setAccessibilityZoomButton = unSelector (mkSelector "setAccessibilityZoomButton:")
      sel_accessibilityMinimizeButton = unSelector (mkSelector "accessibilityMinimizeButton")
      sel_setAccessibilityMinimizeButton = unSelector (mkSelector "setAccessibilityMinimizeButton:")
      sel_accessibilityMinimized = unSelector (mkSelector "accessibilityMinimized")
      sel_setAccessibilityMinimized = unSelector (mkSelector "setAccessibilityMinimized:")
      sel_accessibilityCustomActions = unSelector (mkSelector "accessibilityCustomActions")
      sel_setAccessibilityCustomActions = unSelector (mkSelector "setAccessibilityCustomActions:")
  -- accessibilityCellForColumn:row:
  stub_0 <- wrap_q_q_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityCellForColumn_row rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (fromIntegral arg0) (fromIntegral arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityCellForColumn:row:" "@@:qq" stub_0

  -- accessibilityLineForIndex:
  stub_1 <- wrap_q_q $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityLineForIndex rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (fromIntegral arg0)
        pure (fromIntegral r)
  addObjCMethod cls "accessibilityLineForIndex:" "q@:q" stub_1

  -- accessibilityPerformCancel
  stub_2 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityPerformCancel rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "accessibilityPerformCancel" "B@:" stub_2

  -- accessibilityPerformConfirm
  stub_3 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityPerformConfirm rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "accessibilityPerformConfirm" "B@:" stub_3

  -- accessibilityPerformDecrement
  stub_4 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityPerformDecrement rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "accessibilityPerformDecrement" "B@:" stub_4

  -- accessibilityPerformDelete
  stub_5 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityPerformDelete rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "accessibilityPerformDelete" "B@:" stub_5

  -- accessibilityPerformIncrement
  stub_6 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityPerformIncrement rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "accessibilityPerformIncrement" "B@:" stub_6

  -- accessibilityPerformPick
  stub_7 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityPerformPick rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "accessibilityPerformPick" "B@:" stub_7

  -- accessibilityPerformPress
  stub_8 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityPerformPress rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "accessibilityPerformPress" "B@:" stub_8

  -- accessibilityPerformRaise
  stub_9 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityPerformRaise rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "accessibilityPerformRaise" "B@:" stub_9

  -- accessibilityPerformShowAlternateUI
  stub_10 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityPerformShowAlternateUI rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "accessibilityPerformShowAlternateUI" "B@:" stub_10

  -- accessibilityPerformShowDefaultUI
  stub_11 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityPerformShowDefaultUI rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "accessibilityPerformShowDefaultUI" "B@:" stub_11

  -- accessibilityPerformShowMenu
  stub_12 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityPerformShowMenu rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "accessibilityPerformShowMenu" "B@:" stub_12

  -- isAccessibilitySelectorAllowed:
  stub_13 <- wrap_sel_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _isAccessibilitySelectorAllowed rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (Selector arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "isAccessibilitySelectorAllowed:" "B@::" stub_13

  -- accessibilityElement
  stub_14 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityElement rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "accessibilityElement" "B@:" stub_14

  -- setAccessibilityElement:
  stub_15 <- wrap_B_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityElement rec_ of
      Nothing -> pure ()
      Just f -> f (arg0 /= 0)
  addObjCMethod cls "setAccessibilityElement:" "v@:B" stub_15

  -- accessibilityFocused
  stub_16 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityFocused rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "accessibilityFocused" "B@:" stub_16

  -- setAccessibilityFocused:
  stub_17 <- wrap_B_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityFocused rec_ of
      Nothing -> pure ()
      Just f -> f (arg0 /= 0)
  addObjCMethod cls "setAccessibilityFocused:" "v@:B" stub_17

  -- accessibilityTopLevelUIElement
  stub_18 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityTopLevelUIElement rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityTopLevelUIElement" "@@:" stub_18

  -- setAccessibilityTopLevelUIElement:
  stub_19 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityTopLevelUIElement rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityTopLevelUIElement:" "v@:@" stub_19

  -- accessibilityURL
  stub_20 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityURL rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityURL" "@@:" stub_20

  -- setAccessibilityURL:
  stub_21 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityURL rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityURL:" "v@:@" stub_21

  -- accessibilityValue
  stub_22 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityValue rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityValue" "@@:" stub_22

  -- setAccessibilityValue:
  stub_23 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityValue rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityValue:" "v@:@" stub_23

  -- accessibilityValueDescription
  stub_24 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityValueDescription rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityValueDescription" "@@:" stub_24

  -- setAccessibilityValueDescription:
  stub_25 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityValueDescription rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityValueDescription:" "v@:@" stub_25

  -- accessibilityVisibleChildren
  stub_26 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityVisibleChildren rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityVisibleChildren" "@@:" stub_26

  -- setAccessibilityVisibleChildren:
  stub_27 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityVisibleChildren rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityVisibleChildren:" "v@:@" stub_27

  -- accessibilitySubrole
  stub_28 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilitySubrole rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilitySubrole" "@@:" stub_28

  -- setAccessibilitySubrole:
  stub_29 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilitySubrole rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilitySubrole:" "v@:@" stub_29

  -- accessibilityTitle
  stub_30 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityTitle rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityTitle" "@@:" stub_30

  -- setAccessibilityTitle:
  stub_31 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityTitle rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityTitle:" "v@:@" stub_31

  -- accessibilityTitleUIElement
  stub_32 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityTitleUIElement rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityTitleUIElement" "@@:" stub_32

  -- setAccessibilityTitleUIElement:
  stub_33 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityTitleUIElement rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityTitleUIElement:" "v@:@" stub_33

  -- accessibilityNextContents
  stub_34 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityNextContents rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityNextContents" "@@:" stub_34

  -- setAccessibilityNextContents:
  stub_35 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityNextContents rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityNextContents:" "v@:@" stub_35

  -- accessibilityOverflowButton
  stub_36 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityOverflowButton rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityOverflowButton" "@@:" stub_36

  -- setAccessibilityOverflowButton:
  stub_37 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityOverflowButton rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityOverflowButton:" "v@:@" stub_37

  -- accessibilityParent
  stub_38 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityParent rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityParent" "@@:" stub_38

  -- setAccessibilityParent:
  stub_39 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityParent rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityParent:" "v@:@" stub_39

  -- accessibilityPlaceholderValue
  stub_40 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityPlaceholderValue rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityPlaceholderValue" "@@:" stub_40

  -- setAccessibilityPlaceholderValue:
  stub_41 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityPlaceholderValue rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityPlaceholderValue:" "v@:@" stub_41

  -- accessibilityPreviousContents
  stub_42 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityPreviousContents rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityPreviousContents" "@@:" stub_42

  -- setAccessibilityPreviousContents:
  stub_43 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityPreviousContents rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityPreviousContents:" "v@:@" stub_43

  -- accessibilityRole
  stub_44 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityRole rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityRole" "@@:" stub_44

  -- setAccessibilityRole:
  stub_45 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityRole rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityRole:" "v@:@" stub_45

  -- accessibilityRoleDescription
  stub_46 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityRoleDescription rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityRoleDescription" "@@:" stub_46

  -- setAccessibilityRoleDescription:
  stub_47 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityRoleDescription rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityRoleDescription:" "v@:@" stub_47

  -- accessibilitySearchButton
  stub_48 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilitySearchButton rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilitySearchButton" "@@:" stub_48

  -- setAccessibilitySearchButton:
  stub_49 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilitySearchButton rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilitySearchButton:" "v@:@" stub_49

  -- accessibilitySearchMenu
  stub_50 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilitySearchMenu rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilitySearchMenu" "@@:" stub_50

  -- setAccessibilitySearchMenu:
  stub_51 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilitySearchMenu rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilitySearchMenu:" "v@:@" stub_51

  -- accessibilitySelected
  stub_52 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilitySelected rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "accessibilitySelected" "B@:" stub_52

  -- setAccessibilitySelected:
  stub_53 <- wrap_B_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilitySelected rec_ of
      Nothing -> pure ()
      Just f -> f (arg0 /= 0)
  addObjCMethod cls "setAccessibilitySelected:" "v@:B" stub_53

  -- accessibilitySelectedChildren
  stub_54 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilitySelectedChildren rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilitySelectedChildren" "@@:" stub_54

  -- setAccessibilitySelectedChildren:
  stub_55 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilitySelectedChildren rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilitySelectedChildren:" "v@:@" stub_55

  -- accessibilityServesAsTitleForUIElements
  stub_56 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityServesAsTitleForUIElements rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityServesAsTitleForUIElements" "@@:" stub_56

  -- setAccessibilityServesAsTitleForUIElements:
  stub_57 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityServesAsTitleForUIElements rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityServesAsTitleForUIElements:" "v@:@" stub_57

  -- accessibilityShownMenu
  stub_58 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityShownMenu rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityShownMenu" "@@:" stub_58

  -- setAccessibilityShownMenu:
  stub_59 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityShownMenu rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityShownMenu:" "v@:@" stub_59

  -- accessibilityMinValue
  stub_60 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityMinValue rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityMinValue" "@@:" stub_60

  -- setAccessibilityMinValue:
  stub_61 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityMinValue rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityMinValue:" "v@:@" stub_61

  -- accessibilityMaxValue
  stub_62 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityMaxValue rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityMaxValue" "@@:" stub_62

  -- setAccessibilityMaxValue:
  stub_63 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityMaxValue rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityMaxValue:" "v@:@" stub_63

  -- accessibilityLinkedUIElements
  stub_64 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityLinkedUIElements rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityLinkedUIElements" "@@:" stub_64

  -- setAccessibilityLinkedUIElements:
  stub_65 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityLinkedUIElements rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityLinkedUIElements:" "v@:@" stub_65

  -- accessibilityWindow
  stub_66 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityWindow rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityWindow" "@@:" stub_66

  -- setAccessibilityWindow:
  stub_67 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityWindow rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityWindow:" "v@:@" stub_67

  -- accessibilityIdentifier
  stub_68 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityIdentifier rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityIdentifier" "@@:" stub_68

  -- setAccessibilityIdentifier:
  stub_69 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityIdentifier rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityIdentifier:" "v@:@" stub_69

  -- accessibilityHelp
  stub_70 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityHelp rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityHelp" "@@:" stub_70

  -- setAccessibilityHelp:
  stub_71 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityHelp rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityHelp:" "v@:@" stub_71

  -- accessibilityFilename
  stub_72 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityFilename rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityFilename" "@@:" stub_72

  -- setAccessibilityFilename:
  stub_73 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityFilename rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityFilename:" "v@:@" stub_73

  -- accessibilityExpanded
  stub_74 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityExpanded rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "accessibilityExpanded" "B@:" stub_74

  -- setAccessibilityExpanded:
  stub_75 <- wrap_B_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityExpanded rec_ of
      Nothing -> pure ()
      Just f -> f (arg0 /= 0)
  addObjCMethod cls "setAccessibilityExpanded:" "v@:B" stub_75

  -- accessibilityEdited
  stub_76 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityEdited rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "accessibilityEdited" "B@:" stub_76

  -- setAccessibilityEdited:
  stub_77 <- wrap_B_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityEdited rec_ of
      Nothing -> pure ()
      Just f -> f (arg0 /= 0)
  addObjCMethod cls "setAccessibilityEdited:" "v@:B" stub_77

  -- accessibilityEnabled
  stub_78 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityEnabled rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "accessibilityEnabled" "B@:" stub_78

  -- setAccessibilityEnabled:
  stub_79 <- wrap_B_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityEnabled rec_ of
      Nothing -> pure ()
      Just f -> f (arg0 /= 0)
  addObjCMethod cls "setAccessibilityEnabled:" "v@:B" stub_79

  -- accessibilityChildren
  stub_80 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityChildren rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityChildren" "@@:" stub_80

  -- setAccessibilityChildren:
  stub_81 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityChildren rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityChildren:" "v@:@" stub_81

  -- accessibilityChildrenInNavigationOrder
  stub_82 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityChildrenInNavigationOrder rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityChildrenInNavigationOrder" "@@:" stub_82

  -- setAccessibilityChildrenInNavigationOrder:
  stub_83 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityChildrenInNavigationOrder rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityChildrenInNavigationOrder:" "v@:@" stub_83

  -- accessibilityClearButton
  stub_84 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityClearButton rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityClearButton" "@@:" stub_84

  -- setAccessibilityClearButton:
  stub_85 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityClearButton rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityClearButton:" "v@:@" stub_85

  -- accessibilityCancelButton
  stub_86 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityCancelButton rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityCancelButton" "@@:" stub_86

  -- setAccessibilityCancelButton:
  stub_87 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityCancelButton rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityCancelButton:" "v@:@" stub_87

  -- accessibilityProtectedContent
  stub_88 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityProtectedContent rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "accessibilityProtectedContent" "B@:" stub_88

  -- setAccessibilityProtectedContent:
  stub_89 <- wrap_B_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityProtectedContent rec_ of
      Nothing -> pure ()
      Just f -> f (arg0 /= 0)
  addObjCMethod cls "setAccessibilityProtectedContent:" "v@:B" stub_89

  -- accessibilityContents
  stub_90 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityContents rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityContents" "@@:" stub_90

  -- setAccessibilityContents:
  stub_91 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityContents rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityContents:" "v@:@" stub_91

  -- accessibilityLabel
  stub_92 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityLabel rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityLabel" "@@:" stub_92

  -- setAccessibilityLabel:
  stub_93 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityLabel rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityLabel:" "v@:@" stub_93

  -- accessibilityAlternateUIVisible
  stub_94 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityAlternateUIVisible rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "accessibilityAlternateUIVisible" "B@:" stub_94

  -- setAccessibilityAlternateUIVisible:
  stub_95 <- wrap_B_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityAlternateUIVisible rec_ of
      Nothing -> pure ()
      Just f -> f (arg0 /= 0)
  addObjCMethod cls "setAccessibilityAlternateUIVisible:" "v@:B" stub_95

  -- accessibilitySharedFocusElements
  stub_96 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilitySharedFocusElements rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilitySharedFocusElements" "@@:" stub_96

  -- setAccessibilitySharedFocusElements:
  stub_97 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilitySharedFocusElements rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilitySharedFocusElements:" "v@:@" stub_97

  -- accessibilityRequired
  stub_98 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityRequired rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "accessibilityRequired" "B@:" stub_98

  -- setAccessibilityRequired:
  stub_99 <- wrap_B_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityRequired rec_ of
      Nothing -> pure ()
      Just f -> f (arg0 /= 0)
  addObjCMethod cls "setAccessibilityRequired:" "v@:B" stub_99

  -- accessibilityCustomRotors
  stub_100 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityCustomRotors rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityCustomRotors" "@@:" stub_100

  -- setAccessibilityCustomRotors:
  stub_101 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityCustomRotors rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityCustomRotors:" "v@:@" stub_101

  -- accessibilityUserInputLabels
  stub_102 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityUserInputLabels rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityUserInputLabels" "@@:" stub_102

  -- setAccessibilityUserInputLabels:
  stub_103 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityUserInputLabels rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityUserInputLabels:" "v@:@" stub_103

  -- accessibilityAttributedUserInputLabels
  stub_104 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityAttributedUserInputLabels rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityAttributedUserInputLabels" "@@:" stub_104

  -- setAccessibilityAttributedUserInputLabels:
  stub_105 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityAttributedUserInputLabels rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityAttributedUserInputLabels:" "v@:@" stub_105

  -- accessibilityApplicationFocusedUIElement
  stub_106 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityApplicationFocusedUIElement rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityApplicationFocusedUIElement" "@@:" stub_106

  -- setAccessibilityApplicationFocusedUIElement:
  stub_107 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityApplicationFocusedUIElement rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityApplicationFocusedUIElement:" "v@:@" stub_107

  -- accessibilityMainWindow
  stub_108 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityMainWindow rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityMainWindow" "@@:" stub_108

  -- setAccessibilityMainWindow:
  stub_109 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityMainWindow rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityMainWindow:" "v@:@" stub_109

  -- accessibilityHidden
  stub_110 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityHidden rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "accessibilityHidden" "B@:" stub_110

  -- setAccessibilityHidden:
  stub_111 <- wrap_B_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityHidden rec_ of
      Nothing -> pure ()
      Just f -> f (arg0 /= 0)
  addObjCMethod cls "setAccessibilityHidden:" "v@:B" stub_111

  -- accessibilityFrontmost
  stub_112 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityFrontmost rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "accessibilityFrontmost" "B@:" stub_112

  -- setAccessibilityFrontmost:
  stub_113 <- wrap_B_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityFrontmost rec_ of
      Nothing -> pure ()
      Just f -> f (arg0 /= 0)
  addObjCMethod cls "setAccessibilityFrontmost:" "v@:B" stub_113

  -- accessibilityFocusedWindow
  stub_114 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityFocusedWindow rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityFocusedWindow" "@@:" stub_114

  -- setAccessibilityFocusedWindow:
  stub_115 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityFocusedWindow rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityFocusedWindow:" "v@:@" stub_115

  -- accessibilityWindows
  stub_116 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityWindows rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityWindows" "@@:" stub_116

  -- setAccessibilityWindows:
  stub_117 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityWindows rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityWindows:" "v@:@" stub_117

  -- accessibilityExtrasMenuBar
  stub_118 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityExtrasMenuBar rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityExtrasMenuBar" "@@:" stub_118

  -- setAccessibilityExtrasMenuBar:
  stub_119 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityExtrasMenuBar rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityExtrasMenuBar:" "v@:@" stub_119

  -- accessibilityMenuBar
  stub_120 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityMenuBar rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityMenuBar" "@@:" stub_120

  -- setAccessibilityMenuBar:
  stub_121 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityMenuBar rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityMenuBar:" "v@:@" stub_121

  -- accessibilityColumnTitles
  stub_122 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityColumnTitles rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityColumnTitles" "@@:" stub_122

  -- setAccessibilityColumnTitles:
  stub_123 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityColumnTitles rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityColumnTitles:" "v@:@" stub_123

  -- accessibilityOrderedByRow
  stub_124 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityOrderedByRow rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "accessibilityOrderedByRow" "B@:" stub_124

  -- setAccessibilityOrderedByRow:
  stub_125 <- wrap_B_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityOrderedByRow rec_ of
      Nothing -> pure ()
      Just f -> f (arg0 /= 0)
  addObjCMethod cls "setAccessibilityOrderedByRow:" "v@:B" stub_125

  -- accessibilityHorizontalUnitDescription
  stub_126 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityHorizontalUnitDescription rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityHorizontalUnitDescription" "@@:" stub_126

  -- setAccessibilityHorizontalUnitDescription:
  stub_127 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityHorizontalUnitDescription rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityHorizontalUnitDescription:" "v@:@" stub_127

  -- accessibilityVerticalUnitDescription
  stub_128 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityVerticalUnitDescription rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityVerticalUnitDescription" "@@:" stub_128

  -- setAccessibilityVerticalUnitDescription:
  stub_129 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityVerticalUnitDescription rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityVerticalUnitDescription:" "v@:@" stub_129

  -- accessibilityHandles
  stub_130 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityHandles rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityHandles" "@@:" stub_130

  -- setAccessibilityHandles:
  stub_131 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityHandles rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityHandles:" "v@:@" stub_131

  -- accessibilityWarningValue
  stub_132 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityWarningValue rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityWarningValue" "@@:" stub_132

  -- setAccessibilityWarningValue:
  stub_133 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityWarningValue rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityWarningValue:" "v@:@" stub_133

  -- accessibilityCriticalValue
  stub_134 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityCriticalValue rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityCriticalValue" "@@:" stub_134

  -- setAccessibilityCriticalValue:
  stub_135 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityCriticalValue rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityCriticalValue:" "v@:@" stub_135

  -- accessibilityDisclosed
  stub_136 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityDisclosed rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "accessibilityDisclosed" "B@:" stub_136

  -- setAccessibilityDisclosed:
  stub_137 <- wrap_B_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityDisclosed rec_ of
      Nothing -> pure ()
      Just f -> f (arg0 /= 0)
  addObjCMethod cls "setAccessibilityDisclosed:" "v@:B" stub_137

  -- accessibilityDisclosedByRow
  stub_138 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityDisclosedByRow rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityDisclosedByRow" "@@:" stub_138

  -- setAccessibilityDisclosedByRow:
  stub_139 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityDisclosedByRow rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityDisclosedByRow:" "v@:@" stub_139

  -- accessibilityDisclosedRows
  stub_140 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityDisclosedRows rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityDisclosedRows" "@@:" stub_140

  -- setAccessibilityDisclosedRows:
  stub_141 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityDisclosedRows rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityDisclosedRows:" "v@:@" stub_141

  -- accessibilityDisclosureLevel
  stub_142 <- wrap_q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityDisclosureLevel rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "accessibilityDisclosureLevel" "q@:" stub_142

  -- setAccessibilityDisclosureLevel:
  stub_143 <- wrap_q_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityDisclosureLevel rec_ of
      Nothing -> pure ()
      Just f -> f (fromIntegral arg0)
  addObjCMethod cls "setAccessibilityDisclosureLevel:" "v@:q" stub_143

  -- accessibilityMarkerUIElements
  stub_144 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityMarkerUIElements rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityMarkerUIElements" "@@:" stub_144

  -- setAccessibilityMarkerUIElements:
  stub_145 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityMarkerUIElements rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityMarkerUIElements:" "v@:@" stub_145

  -- accessibilityMarkerValues
  stub_146 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityMarkerValues rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityMarkerValues" "@@:" stub_146

  -- setAccessibilityMarkerValues:
  stub_147 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityMarkerValues rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityMarkerValues:" "v@:@" stub_147

  -- accessibilityMarkerGroupUIElement
  stub_148 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityMarkerGroupUIElement rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityMarkerGroupUIElement" "@@:" stub_148

  -- setAccessibilityMarkerGroupUIElement:
  stub_149 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityMarkerGroupUIElement rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityMarkerGroupUIElement:" "v@:@" stub_149

  -- accessibilityUnitDescription
  stub_150 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityUnitDescription rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityUnitDescription" "@@:" stub_150

  -- setAccessibilityUnitDescription:
  stub_151 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityUnitDescription rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityUnitDescription:" "v@:@" stub_151

  -- accessibilityMarkerTypeDescription
  stub_152 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityMarkerTypeDescription rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityMarkerTypeDescription" "@@:" stub_152

  -- setAccessibilityMarkerTypeDescription:
  stub_153 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityMarkerTypeDescription rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityMarkerTypeDescription:" "v@:@" stub_153

  -- accessibilityHorizontalScrollBar
  stub_154 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityHorizontalScrollBar rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityHorizontalScrollBar" "@@:" stub_154

  -- setAccessibilityHorizontalScrollBar:
  stub_155 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityHorizontalScrollBar rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityHorizontalScrollBar:" "v@:@" stub_155

  -- accessibilityVerticalScrollBar
  stub_156 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityVerticalScrollBar rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityVerticalScrollBar" "@@:" stub_156

  -- setAccessibilityVerticalScrollBar:
  stub_157 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityVerticalScrollBar rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityVerticalScrollBar:" "v@:@" stub_157

  -- accessibilityAllowedValues
  stub_158 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityAllowedValues rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityAllowedValues" "@@:" stub_158

  -- setAccessibilityAllowedValues:
  stub_159 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityAllowedValues rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityAllowedValues:" "v@:@" stub_159

  -- accessibilityLabelUIElements
  stub_160 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityLabelUIElements rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityLabelUIElements" "@@:" stub_160

  -- setAccessibilityLabelUIElements:
  stub_161 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityLabelUIElements rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityLabelUIElements:" "v@:@" stub_161

  -- accessibilityLabelValue
  stub_162 <- wrap_f $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityLabelValue rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "accessibilityLabelValue" "f@:" stub_162

  -- setAccessibilityLabelValue:
  stub_163 <- wrap_f_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityLabelValue rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0)
  addObjCMethod cls "setAccessibilityLabelValue:" "v@:f" stub_163

  -- accessibilitySplitters
  stub_164 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilitySplitters rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilitySplitters" "@@:" stub_164

  -- setAccessibilitySplitters:
  stub_165 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilitySplitters rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilitySplitters:" "v@:@" stub_165

  -- accessibilityDecrementButton
  stub_166 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityDecrementButton rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityDecrementButton" "@@:" stub_166

  -- setAccessibilityDecrementButton:
  stub_167 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityDecrementButton rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityDecrementButton:" "v@:@" stub_167

  -- accessibilityIncrementButton
  stub_168 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityIncrementButton rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityIncrementButton" "@@:" stub_168

  -- setAccessibilityIncrementButton:
  stub_169 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityIncrementButton rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityIncrementButton:" "v@:@" stub_169

  -- accessibilityTabs
  stub_170 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityTabs rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityTabs" "@@:" stub_170

  -- setAccessibilityTabs:
  stub_171 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityTabs rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityTabs:" "v@:@" stub_171

  -- accessibilityHeader
  stub_172 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityHeader rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityHeader" "@@:" stub_172

  -- setAccessibilityHeader:
  stub_173 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityHeader rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityHeader:" "v@:@" stub_173

  -- accessibilityColumnCount
  stub_174 <- wrap_q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityColumnCount rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "accessibilityColumnCount" "q@:" stub_174

  -- setAccessibilityColumnCount:
  stub_175 <- wrap_q_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityColumnCount rec_ of
      Nothing -> pure ()
      Just f -> f (fromIntegral arg0)
  addObjCMethod cls "setAccessibilityColumnCount:" "v@:q" stub_175

  -- accessibilityRowCount
  stub_176 <- wrap_q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityRowCount rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "accessibilityRowCount" "q@:" stub_176

  -- setAccessibilityRowCount:
  stub_177 <- wrap_q_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityRowCount rec_ of
      Nothing -> pure ()
      Just f -> f (fromIntegral arg0)
  addObjCMethod cls "setAccessibilityRowCount:" "v@:q" stub_177

  -- accessibilityIndex
  stub_178 <- wrap_q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityIndex rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "accessibilityIndex" "q@:" stub_178

  -- setAccessibilityIndex:
  stub_179 <- wrap_q_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityIndex rec_ of
      Nothing -> pure ()
      Just f -> f (fromIntegral arg0)
  addObjCMethod cls "setAccessibilityIndex:" "v@:q" stub_179

  -- accessibilityColumns
  stub_180 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityColumns rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityColumns" "@@:" stub_180

  -- setAccessibilityColumns:
  stub_181 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityColumns rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityColumns:" "v@:@" stub_181

  -- accessibilityRows
  stub_182 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityRows rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityRows" "@@:" stub_182

  -- setAccessibilityRows:
  stub_183 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityRows rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityRows:" "v@:@" stub_183

  -- accessibilityVisibleRows
  stub_184 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityVisibleRows rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityVisibleRows" "@@:" stub_184

  -- setAccessibilityVisibleRows:
  stub_185 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityVisibleRows rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityVisibleRows:" "v@:@" stub_185

  -- accessibilitySelectedRows
  stub_186 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilitySelectedRows rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilitySelectedRows" "@@:" stub_186

  -- setAccessibilitySelectedRows:
  stub_187 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilitySelectedRows rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilitySelectedRows:" "v@:@" stub_187

  -- accessibilityVisibleColumns
  stub_188 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityVisibleColumns rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityVisibleColumns" "@@:" stub_188

  -- setAccessibilityVisibleColumns:
  stub_189 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityVisibleColumns rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityVisibleColumns:" "v@:@" stub_189

  -- accessibilitySelectedColumns
  stub_190 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilitySelectedColumns rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilitySelectedColumns" "@@:" stub_190

  -- setAccessibilitySelectedColumns:
  stub_191 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilitySelectedColumns rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilitySelectedColumns:" "v@:@" stub_191

  -- accessibilityRowHeaderUIElements
  stub_192 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityRowHeaderUIElements rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityRowHeaderUIElements" "@@:" stub_192

  -- setAccessibilityRowHeaderUIElements:
  stub_193 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityRowHeaderUIElements rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityRowHeaderUIElements:" "v@:@" stub_193

  -- accessibilitySelectedCells
  stub_194 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilitySelectedCells rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilitySelectedCells" "@@:" stub_194

  -- setAccessibilitySelectedCells:
  stub_195 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilitySelectedCells rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilitySelectedCells:" "v@:@" stub_195

  -- accessibilityVisibleCells
  stub_196 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityVisibleCells rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityVisibleCells" "@@:" stub_196

  -- setAccessibilityVisibleCells:
  stub_197 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityVisibleCells rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityVisibleCells:" "v@:@" stub_197

  -- accessibilityColumnHeaderUIElements
  stub_198 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityColumnHeaderUIElements rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityColumnHeaderUIElements" "@@:" stub_198

  -- setAccessibilityColumnHeaderUIElements:
  stub_199 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityColumnHeaderUIElements rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityColumnHeaderUIElements:" "v@:@" stub_199

  -- accessibilityInsertionPointLineNumber
  stub_200 <- wrap_q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityInsertionPointLineNumber rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "accessibilityInsertionPointLineNumber" "q@:" stub_200

  -- setAccessibilityInsertionPointLineNumber:
  stub_201 <- wrap_q_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityInsertionPointLineNumber rec_ of
      Nothing -> pure ()
      Just f -> f (fromIntegral arg0)
  addObjCMethod cls "setAccessibilityInsertionPointLineNumber:" "v@:q" stub_201

  -- accessibilitySharedTextUIElements
  stub_202 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilitySharedTextUIElements rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilitySharedTextUIElements" "@@:" stub_202

  -- setAccessibilitySharedTextUIElements:
  stub_203 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilitySharedTextUIElements rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilitySharedTextUIElements:" "v@:@" stub_203

  -- accessibilityNumberOfCharacters
  stub_204 <- wrap_q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityNumberOfCharacters rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "accessibilityNumberOfCharacters" "q@:" stub_204

  -- setAccessibilityNumberOfCharacters:
  stub_205 <- wrap_q_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityNumberOfCharacters rec_ of
      Nothing -> pure ()
      Just f -> f (fromIntegral arg0)
  addObjCMethod cls "setAccessibilityNumberOfCharacters:" "v@:q" stub_205

  -- accessibilitySelectedText
  stub_206 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilitySelectedText rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilitySelectedText" "@@:" stub_206

  -- setAccessibilitySelectedText:
  stub_207 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilitySelectedText rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilitySelectedText:" "v@:@" stub_207

  -- accessibilitySelectedTextRanges
  stub_208 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilitySelectedTextRanges rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilitySelectedTextRanges" "@@:" stub_208

  -- setAccessibilitySelectedTextRanges:
  stub_209 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilitySelectedTextRanges rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilitySelectedTextRanges:" "v@:@" stub_209

  -- accessibilityToolbarButton
  stub_210 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityToolbarButton rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityToolbarButton" "@@:" stub_210

  -- setAccessibilityToolbarButton:
  stub_211 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityToolbarButton rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityToolbarButton:" "v@:@" stub_211

  -- accessibilityModal
  stub_212 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityModal rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "accessibilityModal" "B@:" stub_212

  -- setAccessibilityModal:
  stub_213 <- wrap_B_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityModal rec_ of
      Nothing -> pure ()
      Just f -> f (arg0 /= 0)
  addObjCMethod cls "setAccessibilityModal:" "v@:B" stub_213

  -- accessibilityProxy
  stub_214 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityProxy rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityProxy" "@@:" stub_214

  -- setAccessibilityProxy:
  stub_215 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityProxy rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityProxy:" "v@:@" stub_215

  -- accessibilityMain
  stub_216 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityMain rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "accessibilityMain" "B@:" stub_216

  -- setAccessibilityMain:
  stub_217 <- wrap_B_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityMain rec_ of
      Nothing -> pure ()
      Just f -> f (arg0 /= 0)
  addObjCMethod cls "setAccessibilityMain:" "v@:B" stub_217

  -- accessibilityFullScreenButton
  stub_218 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityFullScreenButton rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityFullScreenButton" "@@:" stub_218

  -- setAccessibilityFullScreenButton:
  stub_219 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityFullScreenButton rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityFullScreenButton:" "v@:@" stub_219

  -- accessibilityGrowArea
  stub_220 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityGrowArea rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityGrowArea" "@@:" stub_220

  -- setAccessibilityGrowArea:
  stub_221 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityGrowArea rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityGrowArea:" "v@:@" stub_221

  -- accessibilityDocument
  stub_222 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityDocument rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityDocument" "@@:" stub_222

  -- setAccessibilityDocument:
  stub_223 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityDocument rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityDocument:" "v@:@" stub_223

  -- accessibilityDefaultButton
  stub_224 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityDefaultButton rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityDefaultButton" "@@:" stub_224

  -- setAccessibilityDefaultButton:
  stub_225 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityDefaultButton rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityDefaultButton:" "v@:@" stub_225

  -- accessibilityCloseButton
  stub_226 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityCloseButton rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityCloseButton" "@@:" stub_226

  -- setAccessibilityCloseButton:
  stub_227 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityCloseButton rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityCloseButton:" "v@:@" stub_227

  -- accessibilityZoomButton
  stub_228 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityZoomButton rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityZoomButton" "@@:" stub_228

  -- setAccessibilityZoomButton:
  stub_229 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityZoomButton rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityZoomButton:" "v@:@" stub_229

  -- accessibilityMinimizeButton
  stub_230 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityMinimizeButton rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityMinimizeButton" "@@:" stub_230

  -- setAccessibilityMinimizeButton:
  stub_231 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityMinimizeButton rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityMinimizeButton:" "v@:@" stub_231

  -- accessibilityMinimized
  stub_232 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityMinimized rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "accessibilityMinimized" "B@:" stub_232

  -- setAccessibilityMinimized:
  stub_233 <- wrap_B_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityMinimized rec_ of
      Nothing -> pure ()
      Just f -> f (arg0 /= 0)
  addObjCMethod cls "setAccessibilityMinimized:" "v@:B" stub_233

  -- accessibilityCustomActions
  stub_234 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _accessibilityCustomActions rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityCustomActions" "@@:" stub_234

  -- setAccessibilityCustomActions:
  stub_235 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    case _setAccessibilityCustomActions rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityCustomActions:" "v@:@" stub_235

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityOverrides
    if queriedSel == sel_accessibilityCellForColumn_row then pure (maybe 0 (const 1) (_accessibilityCellForColumn_row rec_))
    else if queriedSel == sel_accessibilityLineForIndex then pure (maybe 0 (const 1) (_accessibilityLineForIndex rec_))
    else if queriedSel == sel_accessibilityPerformCancel then pure (maybe 0 (const 1) (_accessibilityPerformCancel rec_))
    else if queriedSel == sel_accessibilityPerformConfirm then pure (maybe 0 (const 1) (_accessibilityPerformConfirm rec_))
    else if queriedSel == sel_accessibilityPerformDecrement then pure (maybe 0 (const 1) (_accessibilityPerformDecrement rec_))
    else if queriedSel == sel_accessibilityPerformDelete then pure (maybe 0 (const 1) (_accessibilityPerformDelete rec_))
    else if queriedSel == sel_accessibilityPerformIncrement then pure (maybe 0 (const 1) (_accessibilityPerformIncrement rec_))
    else if queriedSel == sel_accessibilityPerformPick then pure (maybe 0 (const 1) (_accessibilityPerformPick rec_))
    else if queriedSel == sel_accessibilityPerformPress then pure (maybe 0 (const 1) (_accessibilityPerformPress rec_))
    else if queriedSel == sel_accessibilityPerformRaise then pure (maybe 0 (const 1) (_accessibilityPerformRaise rec_))
    else if queriedSel == sel_accessibilityPerformShowAlternateUI then pure (maybe 0 (const 1) (_accessibilityPerformShowAlternateUI rec_))
    else if queriedSel == sel_accessibilityPerformShowDefaultUI then pure (maybe 0 (const 1) (_accessibilityPerformShowDefaultUI rec_))
    else if queriedSel == sel_accessibilityPerformShowMenu then pure (maybe 0 (const 1) (_accessibilityPerformShowMenu rec_))
    else if queriedSel == sel_isAccessibilitySelectorAllowed then pure (maybe 0 (const 1) (_isAccessibilitySelectorAllowed rec_))
    else if queriedSel == sel_accessibilityElement then pure (maybe 0 (const 1) (_accessibilityElement rec_))
    else if queriedSel == sel_setAccessibilityElement then pure (maybe 0 (const 1) (_setAccessibilityElement rec_))
    else if queriedSel == sel_accessibilityFocused then pure (maybe 0 (const 1) (_accessibilityFocused rec_))
    else if queriedSel == sel_setAccessibilityFocused then pure (maybe 0 (const 1) (_setAccessibilityFocused rec_))
    else if queriedSel == sel_accessibilityTopLevelUIElement then pure (maybe 0 (const 1) (_accessibilityTopLevelUIElement rec_))
    else if queriedSel == sel_setAccessibilityTopLevelUIElement then pure (maybe 0 (const 1) (_setAccessibilityTopLevelUIElement rec_))
    else if queriedSel == sel_accessibilityURL then pure (maybe 0 (const 1) (_accessibilityURL rec_))
    else if queriedSel == sel_setAccessibilityURL then pure (maybe 0 (const 1) (_setAccessibilityURL rec_))
    else if queriedSel == sel_accessibilityValue then pure (maybe 0 (const 1) (_accessibilityValue rec_))
    else if queriedSel == sel_setAccessibilityValue then pure (maybe 0 (const 1) (_setAccessibilityValue rec_))
    else if queriedSel == sel_accessibilityValueDescription then pure (maybe 0 (const 1) (_accessibilityValueDescription rec_))
    else if queriedSel == sel_setAccessibilityValueDescription then pure (maybe 0 (const 1) (_setAccessibilityValueDescription rec_))
    else if queriedSel == sel_accessibilityVisibleChildren then pure (maybe 0 (const 1) (_accessibilityVisibleChildren rec_))
    else if queriedSel == sel_setAccessibilityVisibleChildren then pure (maybe 0 (const 1) (_setAccessibilityVisibleChildren rec_))
    else if queriedSel == sel_accessibilitySubrole then pure (maybe 0 (const 1) (_accessibilitySubrole rec_))
    else if queriedSel == sel_setAccessibilitySubrole then pure (maybe 0 (const 1) (_setAccessibilitySubrole rec_))
    else if queriedSel == sel_accessibilityTitle then pure (maybe 0 (const 1) (_accessibilityTitle rec_))
    else if queriedSel == sel_setAccessibilityTitle then pure (maybe 0 (const 1) (_setAccessibilityTitle rec_))
    else if queriedSel == sel_accessibilityTitleUIElement then pure (maybe 0 (const 1) (_accessibilityTitleUIElement rec_))
    else if queriedSel == sel_setAccessibilityTitleUIElement then pure (maybe 0 (const 1) (_setAccessibilityTitleUIElement rec_))
    else if queriedSel == sel_accessibilityNextContents then pure (maybe 0 (const 1) (_accessibilityNextContents rec_))
    else if queriedSel == sel_setAccessibilityNextContents then pure (maybe 0 (const 1) (_setAccessibilityNextContents rec_))
    else if queriedSel == sel_accessibilityOverflowButton then pure (maybe 0 (const 1) (_accessibilityOverflowButton rec_))
    else if queriedSel == sel_setAccessibilityOverflowButton then pure (maybe 0 (const 1) (_setAccessibilityOverflowButton rec_))
    else if queriedSel == sel_accessibilityParent then pure (maybe 0 (const 1) (_accessibilityParent rec_))
    else if queriedSel == sel_setAccessibilityParent then pure (maybe 0 (const 1) (_setAccessibilityParent rec_))
    else if queriedSel == sel_accessibilityPlaceholderValue then pure (maybe 0 (const 1) (_accessibilityPlaceholderValue rec_))
    else if queriedSel == sel_setAccessibilityPlaceholderValue then pure (maybe 0 (const 1) (_setAccessibilityPlaceholderValue rec_))
    else if queriedSel == sel_accessibilityPreviousContents then pure (maybe 0 (const 1) (_accessibilityPreviousContents rec_))
    else if queriedSel == sel_setAccessibilityPreviousContents then pure (maybe 0 (const 1) (_setAccessibilityPreviousContents rec_))
    else if queriedSel == sel_accessibilityRole then pure (maybe 0 (const 1) (_accessibilityRole rec_))
    else if queriedSel == sel_setAccessibilityRole then pure (maybe 0 (const 1) (_setAccessibilityRole rec_))
    else if queriedSel == sel_accessibilityRoleDescription then pure (maybe 0 (const 1) (_accessibilityRoleDescription rec_))
    else if queriedSel == sel_setAccessibilityRoleDescription then pure (maybe 0 (const 1) (_setAccessibilityRoleDescription rec_))
    else if queriedSel == sel_accessibilitySearchButton then pure (maybe 0 (const 1) (_accessibilitySearchButton rec_))
    else if queriedSel == sel_setAccessibilitySearchButton then pure (maybe 0 (const 1) (_setAccessibilitySearchButton rec_))
    else if queriedSel == sel_accessibilitySearchMenu then pure (maybe 0 (const 1) (_accessibilitySearchMenu rec_))
    else if queriedSel == sel_setAccessibilitySearchMenu then pure (maybe 0 (const 1) (_setAccessibilitySearchMenu rec_))
    else if queriedSel == sel_accessibilitySelected then pure (maybe 0 (const 1) (_accessibilitySelected rec_))
    else if queriedSel == sel_setAccessibilitySelected then pure (maybe 0 (const 1) (_setAccessibilitySelected rec_))
    else if queriedSel == sel_accessibilitySelectedChildren then pure (maybe 0 (const 1) (_accessibilitySelectedChildren rec_))
    else if queriedSel == sel_setAccessibilitySelectedChildren then pure (maybe 0 (const 1) (_setAccessibilitySelectedChildren rec_))
    else if queriedSel == sel_accessibilityServesAsTitleForUIElements then pure (maybe 0 (const 1) (_accessibilityServesAsTitleForUIElements rec_))
    else if queriedSel == sel_setAccessibilityServesAsTitleForUIElements then pure (maybe 0 (const 1) (_setAccessibilityServesAsTitleForUIElements rec_))
    else if queriedSel == sel_accessibilityShownMenu then pure (maybe 0 (const 1) (_accessibilityShownMenu rec_))
    else if queriedSel == sel_setAccessibilityShownMenu then pure (maybe 0 (const 1) (_setAccessibilityShownMenu rec_))
    else if queriedSel == sel_accessibilityMinValue then pure (maybe 0 (const 1) (_accessibilityMinValue rec_))
    else if queriedSel == sel_setAccessibilityMinValue then pure (maybe 0 (const 1) (_setAccessibilityMinValue rec_))
    else if queriedSel == sel_accessibilityMaxValue then pure (maybe 0 (const 1) (_accessibilityMaxValue rec_))
    else if queriedSel == sel_setAccessibilityMaxValue then pure (maybe 0 (const 1) (_setAccessibilityMaxValue rec_))
    else if queriedSel == sel_accessibilityLinkedUIElements then pure (maybe 0 (const 1) (_accessibilityLinkedUIElements rec_))
    else if queriedSel == sel_setAccessibilityLinkedUIElements then pure (maybe 0 (const 1) (_setAccessibilityLinkedUIElements rec_))
    else if queriedSel == sel_accessibilityWindow then pure (maybe 0 (const 1) (_accessibilityWindow rec_))
    else if queriedSel == sel_setAccessibilityWindow then pure (maybe 0 (const 1) (_setAccessibilityWindow rec_))
    else if queriedSel == sel_accessibilityIdentifier then pure (maybe 0 (const 1) (_accessibilityIdentifier rec_))
    else if queriedSel == sel_setAccessibilityIdentifier then pure (maybe 0 (const 1) (_setAccessibilityIdentifier rec_))
    else if queriedSel == sel_accessibilityHelp then pure (maybe 0 (const 1) (_accessibilityHelp rec_))
    else if queriedSel == sel_setAccessibilityHelp then pure (maybe 0 (const 1) (_setAccessibilityHelp rec_))
    else if queriedSel == sel_accessibilityFilename then pure (maybe 0 (const 1) (_accessibilityFilename rec_))
    else if queriedSel == sel_setAccessibilityFilename then pure (maybe 0 (const 1) (_setAccessibilityFilename rec_))
    else if queriedSel == sel_accessibilityExpanded then pure (maybe 0 (const 1) (_accessibilityExpanded rec_))
    else if queriedSel == sel_setAccessibilityExpanded then pure (maybe 0 (const 1) (_setAccessibilityExpanded rec_))
    else if queriedSel == sel_accessibilityEdited then pure (maybe 0 (const 1) (_accessibilityEdited rec_))
    else if queriedSel == sel_setAccessibilityEdited then pure (maybe 0 (const 1) (_setAccessibilityEdited rec_))
    else if queriedSel == sel_accessibilityEnabled then pure (maybe 0 (const 1) (_accessibilityEnabled rec_))
    else if queriedSel == sel_setAccessibilityEnabled then pure (maybe 0 (const 1) (_setAccessibilityEnabled rec_))
    else if queriedSel == sel_accessibilityChildren then pure (maybe 0 (const 1) (_accessibilityChildren rec_))
    else if queriedSel == sel_setAccessibilityChildren then pure (maybe 0 (const 1) (_setAccessibilityChildren rec_))
    else if queriedSel == sel_accessibilityChildrenInNavigationOrder then pure (maybe 0 (const 1) (_accessibilityChildrenInNavigationOrder rec_))
    else if queriedSel == sel_setAccessibilityChildrenInNavigationOrder then pure (maybe 0 (const 1) (_setAccessibilityChildrenInNavigationOrder rec_))
    else if queriedSel == sel_accessibilityClearButton then pure (maybe 0 (const 1) (_accessibilityClearButton rec_))
    else if queriedSel == sel_setAccessibilityClearButton then pure (maybe 0 (const 1) (_setAccessibilityClearButton rec_))
    else if queriedSel == sel_accessibilityCancelButton then pure (maybe 0 (const 1) (_accessibilityCancelButton rec_))
    else if queriedSel == sel_setAccessibilityCancelButton then pure (maybe 0 (const 1) (_setAccessibilityCancelButton rec_))
    else if queriedSel == sel_accessibilityProtectedContent then pure (maybe 0 (const 1) (_accessibilityProtectedContent rec_))
    else if queriedSel == sel_setAccessibilityProtectedContent then pure (maybe 0 (const 1) (_setAccessibilityProtectedContent rec_))
    else if queriedSel == sel_accessibilityContents then pure (maybe 0 (const 1) (_accessibilityContents rec_))
    else if queriedSel == sel_setAccessibilityContents then pure (maybe 0 (const 1) (_setAccessibilityContents rec_))
    else if queriedSel == sel_accessibilityLabel then pure (maybe 0 (const 1) (_accessibilityLabel rec_))
    else if queriedSel == sel_setAccessibilityLabel then pure (maybe 0 (const 1) (_setAccessibilityLabel rec_))
    else if queriedSel == sel_accessibilityAlternateUIVisible then pure (maybe 0 (const 1) (_accessibilityAlternateUIVisible rec_))
    else if queriedSel == sel_setAccessibilityAlternateUIVisible then pure (maybe 0 (const 1) (_setAccessibilityAlternateUIVisible rec_))
    else if queriedSel == sel_accessibilitySharedFocusElements then pure (maybe 0 (const 1) (_accessibilitySharedFocusElements rec_))
    else if queriedSel == sel_setAccessibilitySharedFocusElements then pure (maybe 0 (const 1) (_setAccessibilitySharedFocusElements rec_))
    else if queriedSel == sel_accessibilityRequired then pure (maybe 0 (const 1) (_accessibilityRequired rec_))
    else if queriedSel == sel_setAccessibilityRequired then pure (maybe 0 (const 1) (_setAccessibilityRequired rec_))
    else if queriedSel == sel_accessibilityCustomRotors then pure (maybe 0 (const 1) (_accessibilityCustomRotors rec_))
    else if queriedSel == sel_setAccessibilityCustomRotors then pure (maybe 0 (const 1) (_setAccessibilityCustomRotors rec_))
    else if queriedSel == sel_accessibilityUserInputLabels then pure (maybe 0 (const 1) (_accessibilityUserInputLabels rec_))
    else if queriedSel == sel_setAccessibilityUserInputLabels then pure (maybe 0 (const 1) (_setAccessibilityUserInputLabels rec_))
    else if queriedSel == sel_accessibilityAttributedUserInputLabels then pure (maybe 0 (const 1) (_accessibilityAttributedUserInputLabels rec_))
    else if queriedSel == sel_setAccessibilityAttributedUserInputLabels then pure (maybe 0 (const 1) (_setAccessibilityAttributedUserInputLabels rec_))
    else if queriedSel == sel_accessibilityApplicationFocusedUIElement then pure (maybe 0 (const 1) (_accessibilityApplicationFocusedUIElement rec_))
    else if queriedSel == sel_setAccessibilityApplicationFocusedUIElement then pure (maybe 0 (const 1) (_setAccessibilityApplicationFocusedUIElement rec_))
    else if queriedSel == sel_accessibilityMainWindow then pure (maybe 0 (const 1) (_accessibilityMainWindow rec_))
    else if queriedSel == sel_setAccessibilityMainWindow then pure (maybe 0 (const 1) (_setAccessibilityMainWindow rec_))
    else if queriedSel == sel_accessibilityHidden then pure (maybe 0 (const 1) (_accessibilityHidden rec_))
    else if queriedSel == sel_setAccessibilityHidden then pure (maybe 0 (const 1) (_setAccessibilityHidden rec_))
    else if queriedSel == sel_accessibilityFrontmost then pure (maybe 0 (const 1) (_accessibilityFrontmost rec_))
    else if queriedSel == sel_setAccessibilityFrontmost then pure (maybe 0 (const 1) (_setAccessibilityFrontmost rec_))
    else if queriedSel == sel_accessibilityFocusedWindow then pure (maybe 0 (const 1) (_accessibilityFocusedWindow rec_))
    else if queriedSel == sel_setAccessibilityFocusedWindow then pure (maybe 0 (const 1) (_setAccessibilityFocusedWindow rec_))
    else if queriedSel == sel_accessibilityWindows then pure (maybe 0 (const 1) (_accessibilityWindows rec_))
    else if queriedSel == sel_setAccessibilityWindows then pure (maybe 0 (const 1) (_setAccessibilityWindows rec_))
    else if queriedSel == sel_accessibilityExtrasMenuBar then pure (maybe 0 (const 1) (_accessibilityExtrasMenuBar rec_))
    else if queriedSel == sel_setAccessibilityExtrasMenuBar then pure (maybe 0 (const 1) (_setAccessibilityExtrasMenuBar rec_))
    else if queriedSel == sel_accessibilityMenuBar then pure (maybe 0 (const 1) (_accessibilityMenuBar rec_))
    else if queriedSel == sel_setAccessibilityMenuBar then pure (maybe 0 (const 1) (_setAccessibilityMenuBar rec_))
    else if queriedSel == sel_accessibilityColumnTitles then pure (maybe 0 (const 1) (_accessibilityColumnTitles rec_))
    else if queriedSel == sel_setAccessibilityColumnTitles then pure (maybe 0 (const 1) (_setAccessibilityColumnTitles rec_))
    else if queriedSel == sel_accessibilityOrderedByRow then pure (maybe 0 (const 1) (_accessibilityOrderedByRow rec_))
    else if queriedSel == sel_setAccessibilityOrderedByRow then pure (maybe 0 (const 1) (_setAccessibilityOrderedByRow rec_))
    else if queriedSel == sel_accessibilityHorizontalUnitDescription then pure (maybe 0 (const 1) (_accessibilityHorizontalUnitDescription rec_))
    else if queriedSel == sel_setAccessibilityHorizontalUnitDescription then pure (maybe 0 (const 1) (_setAccessibilityHorizontalUnitDescription rec_))
    else if queriedSel == sel_accessibilityVerticalUnitDescription then pure (maybe 0 (const 1) (_accessibilityVerticalUnitDescription rec_))
    else if queriedSel == sel_setAccessibilityVerticalUnitDescription then pure (maybe 0 (const 1) (_setAccessibilityVerticalUnitDescription rec_))
    else if queriedSel == sel_accessibilityHandles then pure (maybe 0 (const 1) (_accessibilityHandles rec_))
    else if queriedSel == sel_setAccessibilityHandles then pure (maybe 0 (const 1) (_setAccessibilityHandles rec_))
    else if queriedSel == sel_accessibilityWarningValue then pure (maybe 0 (const 1) (_accessibilityWarningValue rec_))
    else if queriedSel == sel_setAccessibilityWarningValue then pure (maybe 0 (const 1) (_setAccessibilityWarningValue rec_))
    else if queriedSel == sel_accessibilityCriticalValue then pure (maybe 0 (const 1) (_accessibilityCriticalValue rec_))
    else if queriedSel == sel_setAccessibilityCriticalValue then pure (maybe 0 (const 1) (_setAccessibilityCriticalValue rec_))
    else if queriedSel == sel_accessibilityDisclosed then pure (maybe 0 (const 1) (_accessibilityDisclosed rec_))
    else if queriedSel == sel_setAccessibilityDisclosed then pure (maybe 0 (const 1) (_setAccessibilityDisclosed rec_))
    else if queriedSel == sel_accessibilityDisclosedByRow then pure (maybe 0 (const 1) (_accessibilityDisclosedByRow rec_))
    else if queriedSel == sel_setAccessibilityDisclosedByRow then pure (maybe 0 (const 1) (_setAccessibilityDisclosedByRow rec_))
    else if queriedSel == sel_accessibilityDisclosedRows then pure (maybe 0 (const 1) (_accessibilityDisclosedRows rec_))
    else if queriedSel == sel_setAccessibilityDisclosedRows then pure (maybe 0 (const 1) (_setAccessibilityDisclosedRows rec_))
    else if queriedSel == sel_accessibilityDisclosureLevel then pure (maybe 0 (const 1) (_accessibilityDisclosureLevel rec_))
    else if queriedSel == sel_setAccessibilityDisclosureLevel then pure (maybe 0 (const 1) (_setAccessibilityDisclosureLevel rec_))
    else if queriedSel == sel_accessibilityMarkerUIElements then pure (maybe 0 (const 1) (_accessibilityMarkerUIElements rec_))
    else if queriedSel == sel_setAccessibilityMarkerUIElements then pure (maybe 0 (const 1) (_setAccessibilityMarkerUIElements rec_))
    else if queriedSel == sel_accessibilityMarkerValues then pure (maybe 0 (const 1) (_accessibilityMarkerValues rec_))
    else if queriedSel == sel_setAccessibilityMarkerValues then pure (maybe 0 (const 1) (_setAccessibilityMarkerValues rec_))
    else if queriedSel == sel_accessibilityMarkerGroupUIElement then pure (maybe 0 (const 1) (_accessibilityMarkerGroupUIElement rec_))
    else if queriedSel == sel_setAccessibilityMarkerGroupUIElement then pure (maybe 0 (const 1) (_setAccessibilityMarkerGroupUIElement rec_))
    else if queriedSel == sel_accessibilityUnitDescription then pure (maybe 0 (const 1) (_accessibilityUnitDescription rec_))
    else if queriedSel == sel_setAccessibilityUnitDescription then pure (maybe 0 (const 1) (_setAccessibilityUnitDescription rec_))
    else if queriedSel == sel_accessibilityMarkerTypeDescription then pure (maybe 0 (const 1) (_accessibilityMarkerTypeDescription rec_))
    else if queriedSel == sel_setAccessibilityMarkerTypeDescription then pure (maybe 0 (const 1) (_setAccessibilityMarkerTypeDescription rec_))
    else if queriedSel == sel_accessibilityHorizontalScrollBar then pure (maybe 0 (const 1) (_accessibilityHorizontalScrollBar rec_))
    else if queriedSel == sel_setAccessibilityHorizontalScrollBar then pure (maybe 0 (const 1) (_setAccessibilityHorizontalScrollBar rec_))
    else if queriedSel == sel_accessibilityVerticalScrollBar then pure (maybe 0 (const 1) (_accessibilityVerticalScrollBar rec_))
    else if queriedSel == sel_setAccessibilityVerticalScrollBar then pure (maybe 0 (const 1) (_setAccessibilityVerticalScrollBar rec_))
    else if queriedSel == sel_accessibilityAllowedValues then pure (maybe 0 (const 1) (_accessibilityAllowedValues rec_))
    else if queriedSel == sel_setAccessibilityAllowedValues then pure (maybe 0 (const 1) (_setAccessibilityAllowedValues rec_))
    else if queriedSel == sel_accessibilityLabelUIElements then pure (maybe 0 (const 1) (_accessibilityLabelUIElements rec_))
    else if queriedSel == sel_setAccessibilityLabelUIElements then pure (maybe 0 (const 1) (_setAccessibilityLabelUIElements rec_))
    else if queriedSel == sel_accessibilityLabelValue then pure (maybe 0 (const 1) (_accessibilityLabelValue rec_))
    else if queriedSel == sel_setAccessibilityLabelValue then pure (maybe 0 (const 1) (_setAccessibilityLabelValue rec_))
    else if queriedSel == sel_accessibilitySplitters then pure (maybe 0 (const 1) (_accessibilitySplitters rec_))
    else if queriedSel == sel_setAccessibilitySplitters then pure (maybe 0 (const 1) (_setAccessibilitySplitters rec_))
    else if queriedSel == sel_accessibilityDecrementButton then pure (maybe 0 (const 1) (_accessibilityDecrementButton rec_))
    else if queriedSel == sel_setAccessibilityDecrementButton then pure (maybe 0 (const 1) (_setAccessibilityDecrementButton rec_))
    else if queriedSel == sel_accessibilityIncrementButton then pure (maybe 0 (const 1) (_accessibilityIncrementButton rec_))
    else if queriedSel == sel_setAccessibilityIncrementButton then pure (maybe 0 (const 1) (_setAccessibilityIncrementButton rec_))
    else if queriedSel == sel_accessibilityTabs then pure (maybe 0 (const 1) (_accessibilityTabs rec_))
    else if queriedSel == sel_setAccessibilityTabs then pure (maybe 0 (const 1) (_setAccessibilityTabs rec_))
    else if queriedSel == sel_accessibilityHeader then pure (maybe 0 (const 1) (_accessibilityHeader rec_))
    else if queriedSel == sel_setAccessibilityHeader then pure (maybe 0 (const 1) (_setAccessibilityHeader rec_))
    else if queriedSel == sel_accessibilityColumnCount then pure (maybe 0 (const 1) (_accessibilityColumnCount rec_))
    else if queriedSel == sel_setAccessibilityColumnCount then pure (maybe 0 (const 1) (_setAccessibilityColumnCount rec_))
    else if queriedSel == sel_accessibilityRowCount then pure (maybe 0 (const 1) (_accessibilityRowCount rec_))
    else if queriedSel == sel_setAccessibilityRowCount then pure (maybe 0 (const 1) (_setAccessibilityRowCount rec_))
    else if queriedSel == sel_accessibilityIndex then pure (maybe 0 (const 1) (_accessibilityIndex rec_))
    else if queriedSel == sel_setAccessibilityIndex then pure (maybe 0 (const 1) (_setAccessibilityIndex rec_))
    else if queriedSel == sel_accessibilityColumns then pure (maybe 0 (const 1) (_accessibilityColumns rec_))
    else if queriedSel == sel_setAccessibilityColumns then pure (maybe 0 (const 1) (_setAccessibilityColumns rec_))
    else if queriedSel == sel_accessibilityRows then pure (maybe 0 (const 1) (_accessibilityRows rec_))
    else if queriedSel == sel_setAccessibilityRows then pure (maybe 0 (const 1) (_setAccessibilityRows rec_))
    else if queriedSel == sel_accessibilityVisibleRows then pure (maybe 0 (const 1) (_accessibilityVisibleRows rec_))
    else if queriedSel == sel_setAccessibilityVisibleRows then pure (maybe 0 (const 1) (_setAccessibilityVisibleRows rec_))
    else if queriedSel == sel_accessibilitySelectedRows then pure (maybe 0 (const 1) (_accessibilitySelectedRows rec_))
    else if queriedSel == sel_setAccessibilitySelectedRows then pure (maybe 0 (const 1) (_setAccessibilitySelectedRows rec_))
    else if queriedSel == sel_accessibilityVisibleColumns then pure (maybe 0 (const 1) (_accessibilityVisibleColumns rec_))
    else if queriedSel == sel_setAccessibilityVisibleColumns then pure (maybe 0 (const 1) (_setAccessibilityVisibleColumns rec_))
    else if queriedSel == sel_accessibilitySelectedColumns then pure (maybe 0 (const 1) (_accessibilitySelectedColumns rec_))
    else if queriedSel == sel_setAccessibilitySelectedColumns then pure (maybe 0 (const 1) (_setAccessibilitySelectedColumns rec_))
    else if queriedSel == sel_accessibilityRowHeaderUIElements then pure (maybe 0 (const 1) (_accessibilityRowHeaderUIElements rec_))
    else if queriedSel == sel_setAccessibilityRowHeaderUIElements then pure (maybe 0 (const 1) (_setAccessibilityRowHeaderUIElements rec_))
    else if queriedSel == sel_accessibilitySelectedCells then pure (maybe 0 (const 1) (_accessibilitySelectedCells rec_))
    else if queriedSel == sel_setAccessibilitySelectedCells then pure (maybe 0 (const 1) (_setAccessibilitySelectedCells rec_))
    else if queriedSel == sel_accessibilityVisibleCells then pure (maybe 0 (const 1) (_accessibilityVisibleCells rec_))
    else if queriedSel == sel_setAccessibilityVisibleCells then pure (maybe 0 (const 1) (_setAccessibilityVisibleCells rec_))
    else if queriedSel == sel_accessibilityColumnHeaderUIElements then pure (maybe 0 (const 1) (_accessibilityColumnHeaderUIElements rec_))
    else if queriedSel == sel_setAccessibilityColumnHeaderUIElements then pure (maybe 0 (const 1) (_setAccessibilityColumnHeaderUIElements rec_))
    else if queriedSel == sel_accessibilityInsertionPointLineNumber then pure (maybe 0 (const 1) (_accessibilityInsertionPointLineNumber rec_))
    else if queriedSel == sel_setAccessibilityInsertionPointLineNumber then pure (maybe 0 (const 1) (_setAccessibilityInsertionPointLineNumber rec_))
    else if queriedSel == sel_accessibilitySharedTextUIElements then pure (maybe 0 (const 1) (_accessibilitySharedTextUIElements rec_))
    else if queriedSel == sel_setAccessibilitySharedTextUIElements then pure (maybe 0 (const 1) (_setAccessibilitySharedTextUIElements rec_))
    else if queriedSel == sel_accessibilityNumberOfCharacters then pure (maybe 0 (const 1) (_accessibilityNumberOfCharacters rec_))
    else if queriedSel == sel_setAccessibilityNumberOfCharacters then pure (maybe 0 (const 1) (_setAccessibilityNumberOfCharacters rec_))
    else if queriedSel == sel_accessibilitySelectedText then pure (maybe 0 (const 1) (_accessibilitySelectedText rec_))
    else if queriedSel == sel_setAccessibilitySelectedText then pure (maybe 0 (const 1) (_setAccessibilitySelectedText rec_))
    else if queriedSel == sel_accessibilitySelectedTextRanges then pure (maybe 0 (const 1) (_accessibilitySelectedTextRanges rec_))
    else if queriedSel == sel_setAccessibilitySelectedTextRanges then pure (maybe 0 (const 1) (_setAccessibilitySelectedTextRanges rec_))
    else if queriedSel == sel_accessibilityToolbarButton then pure (maybe 0 (const 1) (_accessibilityToolbarButton rec_))
    else if queriedSel == sel_setAccessibilityToolbarButton then pure (maybe 0 (const 1) (_setAccessibilityToolbarButton rec_))
    else if queriedSel == sel_accessibilityModal then pure (maybe 0 (const 1) (_accessibilityModal rec_))
    else if queriedSel == sel_setAccessibilityModal then pure (maybe 0 (const 1) (_setAccessibilityModal rec_))
    else if queriedSel == sel_accessibilityProxy then pure (maybe 0 (const 1) (_accessibilityProxy rec_))
    else if queriedSel == sel_setAccessibilityProxy then pure (maybe 0 (const 1) (_setAccessibilityProxy rec_))
    else if queriedSel == sel_accessibilityMain then pure (maybe 0 (const 1) (_accessibilityMain rec_))
    else if queriedSel == sel_setAccessibilityMain then pure (maybe 0 (const 1) (_setAccessibilityMain rec_))
    else if queriedSel == sel_accessibilityFullScreenButton then pure (maybe 0 (const 1) (_accessibilityFullScreenButton rec_))
    else if queriedSel == sel_setAccessibilityFullScreenButton then pure (maybe 0 (const 1) (_setAccessibilityFullScreenButton rec_))
    else if queriedSel == sel_accessibilityGrowArea then pure (maybe 0 (const 1) (_accessibilityGrowArea rec_))
    else if queriedSel == sel_setAccessibilityGrowArea then pure (maybe 0 (const 1) (_setAccessibilityGrowArea rec_))
    else if queriedSel == sel_accessibilityDocument then pure (maybe 0 (const 1) (_accessibilityDocument rec_))
    else if queriedSel == sel_setAccessibilityDocument then pure (maybe 0 (const 1) (_setAccessibilityDocument rec_))
    else if queriedSel == sel_accessibilityDefaultButton then pure (maybe 0 (const 1) (_accessibilityDefaultButton rec_))
    else if queriedSel == sel_setAccessibilityDefaultButton then pure (maybe 0 (const 1) (_setAccessibilityDefaultButton rec_))
    else if queriedSel == sel_accessibilityCloseButton then pure (maybe 0 (const 1) (_accessibilityCloseButton rec_))
    else if queriedSel == sel_setAccessibilityCloseButton then pure (maybe 0 (const 1) (_setAccessibilityCloseButton rec_))
    else if queriedSel == sel_accessibilityZoomButton then pure (maybe 0 (const 1) (_accessibilityZoomButton rec_))
    else if queriedSel == sel_setAccessibilityZoomButton then pure (maybe 0 (const 1) (_setAccessibilityZoomButton rec_))
    else if queriedSel == sel_accessibilityMinimizeButton then pure (maybe 0 (const 1) (_accessibilityMinimizeButton rec_))
    else if queriedSel == sel_setAccessibilityMinimizeButton then pure (maybe 0 (const 1) (_setAccessibilityMinimizeButton rec_))
    else if queriedSel == sel_accessibilityMinimized then pure (maybe 0 (const 1) (_accessibilityMinimized rec_))
    else if queriedSel == sel_setAccessibilityMinimized then pure (maybe 0 (const 1) (_setAccessibilityMinimized rec_))
    else if queriedSel == sel_accessibilityCustomActions then pure (maybe 0 (const 1) (_accessibilityCustomActions rec_))
    else if queriedSel == sel_setAccessibilityCustomActions then pure (maybe 0 (const 1) (_setAccessibilityCustomActions rec_))
    else do
      let super_ = ObjCSuper (RawId self) superCls
      sendSuperMsg super_ (mkSelector "respondsToSelector:") retCULong
        [argPtr (castPtr queriedSel :: Ptr ())]
  addObjCMethod cls "respondsToSelector:" "B@::" rtsStub

  addStablePtrDeallocHandler cls
  objc_registerClassPair cls
  pure cls

-- | Create a new delegate implementing this protocol.
--
-- The returned 'RawId' can be used as a delegate or data source.
newNSAccessibility :: NSAccessibilityOverrides -> IO RawId
newNSAccessibility overrides = do
  inst <- class_createInstance nsAccessibilityDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
