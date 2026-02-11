{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.AppKit.Internal.Classes (
    module ObjC.AppKit.Internal.Classes,
    module ObjC.CoreData.Internal.Classes,
    module ObjC.CoreImage.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
    module ObjC.QuartzCore.Internal.Classes,
    module ObjC.SpriteKit.Internal.Classes,
    module ObjC.Symbols.Internal.Classes,
    module ObjC.UniformTypeIdentifiers.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.CoreData.Internal.Classes
import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.QuartzCore.Internal.Classes
import ObjC.SpriteKit.Internal.Classes
import ObjC.Symbols.Internal.Classes
import ObjC.UniformTypeIdentifiers.Internal.Classes

-- ---------- NSAccessibilityCustomAction ----------

-- | Phantom type for @NSAccessibilityCustomAction@.
data NSAccessibilityCustomAction

instance IsObjCObject (Id NSAccessibilityCustomAction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSAccessibilityCustomAction"

class IsNSObject a => IsNSAccessibilityCustomAction a where
  toNSAccessibilityCustomAction :: a -> Id NSAccessibilityCustomAction

instance IsNSAccessibilityCustomAction (Id NSAccessibilityCustomAction) where
  toNSAccessibilityCustomAction = unsafeCastId

instance IsNSObject (Id NSAccessibilityCustomAction) where
  toNSObject = unsafeCastId

-- ---------- NSAccessibilityCustomRotor ----------

-- | NSAccessibilityCustomRotors allow assistive technologies, like VoiceOver, to search applications for content related to the given label.
-- 
-- Phantom type for @NSAccessibilityCustomRotor@.
data NSAccessibilityCustomRotor

instance IsObjCObject (Id NSAccessibilityCustomRotor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSAccessibilityCustomRotor"

class IsNSObject a => IsNSAccessibilityCustomRotor a where
  toNSAccessibilityCustomRotor :: a -> Id NSAccessibilityCustomRotor

instance IsNSAccessibilityCustomRotor (Id NSAccessibilityCustomRotor) where
  toNSAccessibilityCustomRotor = unsafeCastId

instance IsNSObject (Id NSAccessibilityCustomRotor) where
  toNSObject = unsafeCastId

-- ---------- NSAccessibilityCustomRotorItemResult ----------

-- | NSAccessibilityCustomRotorItemResults are the objects returned to assistive technologies that match a search parameter criteria.
-- 
-- Phantom type for @NSAccessibilityCustomRotorItemResult@.
data NSAccessibilityCustomRotorItemResult

instance IsObjCObject (Id NSAccessibilityCustomRotorItemResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSAccessibilityCustomRotorItemResult"

class IsNSObject a => IsNSAccessibilityCustomRotorItemResult a where
  toNSAccessibilityCustomRotorItemResult :: a -> Id NSAccessibilityCustomRotorItemResult

instance IsNSAccessibilityCustomRotorItemResult (Id NSAccessibilityCustomRotorItemResult) where
  toNSAccessibilityCustomRotorItemResult = unsafeCastId

instance IsNSObject (Id NSAccessibilityCustomRotorItemResult) where
  toNSObject = unsafeCastId

-- ---------- NSAccessibilityCustomRotorSearchParameters ----------

-- | NSAccessibilityCustomRotorSearchParameters is a container for  search parameters. It should be examined to determine the next matching NSAccessibilityCustomRotorItemResult.
-- 
-- Phantom type for @NSAccessibilityCustomRotorSearchParameters@.
data NSAccessibilityCustomRotorSearchParameters

instance IsObjCObject (Id NSAccessibilityCustomRotorSearchParameters) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSAccessibilityCustomRotorSearchParameters"

class IsNSObject a => IsNSAccessibilityCustomRotorSearchParameters a where
  toNSAccessibilityCustomRotorSearchParameters :: a -> Id NSAccessibilityCustomRotorSearchParameters

instance IsNSAccessibilityCustomRotorSearchParameters (Id NSAccessibilityCustomRotorSearchParameters) where
  toNSAccessibilityCustomRotorSearchParameters = unsafeCastId

instance IsNSObject (Id NSAccessibilityCustomRotorSearchParameters) where
  toNSObject = unsafeCastId

-- ---------- NSAccessibilityElement ----------

-- | Phantom type for @NSAccessibilityElement@.
data NSAccessibilityElement

instance IsObjCObject (Id NSAccessibilityElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSAccessibilityElement"

class IsNSObject a => IsNSAccessibilityElement a where
  toNSAccessibilityElement :: a -> Id NSAccessibilityElement

instance IsNSAccessibilityElement (Id NSAccessibilityElement) where
  toNSAccessibilityElement = unsafeCastId

instance IsNSObject (Id NSAccessibilityElement) where
  toNSObject = unsafeCastId

-- ---------- NSAdaptiveImageGlyph ----------

-- | Phantom type for @NSAdaptiveImageGlyph@.
data NSAdaptiveImageGlyph

instance IsObjCObject (Id NSAdaptiveImageGlyph) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSAdaptiveImageGlyph"

class IsNSObject a => IsNSAdaptiveImageGlyph a where
  toNSAdaptiveImageGlyph :: a -> Id NSAdaptiveImageGlyph

instance IsNSAdaptiveImageGlyph (Id NSAdaptiveImageGlyph) where
  toNSAdaptiveImageGlyph = unsafeCastId

instance IsNSObject (Id NSAdaptiveImageGlyph) where
  toNSObject = unsafeCastId

-- ---------- NSAlert ----------

-- | A modal dialog or sheet attached to a document window. The @NSAlert@ class is not designed for subclassing.
-- 
-- Phantom type for @NSAlert@.
data NSAlert

instance IsObjCObject (Id NSAlert) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSAlert"

class IsNSObject a => IsNSAlert a where
  toNSAlert :: a -> Id NSAlert

instance IsNSAlert (Id NSAlert) where
  toNSAlert = unsafeCastId

instance IsNSObject (Id NSAlert) where
  toNSObject = unsafeCastId

-- ---------- NSAlignmentFeedbackFilter ----------

-- | Phantom type for @NSAlignmentFeedbackFilter@.
data NSAlignmentFeedbackFilter

instance IsObjCObject (Id NSAlignmentFeedbackFilter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSAlignmentFeedbackFilter"

class IsNSObject a => IsNSAlignmentFeedbackFilter a where
  toNSAlignmentFeedbackFilter :: a -> Id NSAlignmentFeedbackFilter

instance IsNSAlignmentFeedbackFilter (Id NSAlignmentFeedbackFilter) where
  toNSAlignmentFeedbackFilter = unsafeCastId

instance IsNSObject (Id NSAlignmentFeedbackFilter) where
  toNSObject = unsafeCastId

-- ---------- NSAnimation ----------

-- | Phantom type for @NSAnimation@.
data NSAnimation

instance IsObjCObject (Id NSAnimation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSAnimation"

class IsNSObject a => IsNSAnimation a where
  toNSAnimation :: a -> Id NSAnimation

instance IsNSAnimation (Id NSAnimation) where
  toNSAnimation = unsafeCastId

instance IsNSObject (Id NSAnimation) where
  toNSObject = unsafeCastId

-- ---------- NSAnimationContext ----------

-- | Phantom type for @NSAnimationContext@.
data NSAnimationContext

instance IsObjCObject (Id NSAnimationContext) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSAnimationContext"

class IsNSObject a => IsNSAnimationContext a where
  toNSAnimationContext :: a -> Id NSAnimationContext

instance IsNSAnimationContext (Id NSAnimationContext) where
  toNSAnimationContext = unsafeCastId

instance IsNSObject (Id NSAnimationContext) where
  toNSObject = unsafeCastId

-- ---------- NSAppearance ----------

-- | Phantom type for @NSAppearance@.
data NSAppearance

instance IsObjCObject (Id NSAppearance) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSAppearance"

class IsNSObject a => IsNSAppearance a where
  toNSAppearance :: a -> Id NSAppearance

instance IsNSAppearance (Id NSAppearance) where
  toNSAppearance = unsafeCastId

instance IsNSObject (Id NSAppearance) where
  toNSObject = unsafeCastId

-- ---------- NSBezierPath ----------

-- | Phantom type for @NSBezierPath@.
data NSBezierPath

instance IsObjCObject (Id NSBezierPath) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSBezierPath"

class IsNSObject a => IsNSBezierPath a where
  toNSBezierPath :: a -> Id NSBezierPath

instance IsNSBezierPath (Id NSBezierPath) where
  toNSBezierPath = unsafeCastId

instance IsNSObject (Id NSBezierPath) where
  toNSObject = unsafeCastId

-- ---------- NSBindingSelectionMarker ----------

-- | Phantom type for @NSBindingSelectionMarker@.
data NSBindingSelectionMarker

instance IsObjCObject (Id NSBindingSelectionMarker) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSBindingSelectionMarker"

class IsNSObject a => IsNSBindingSelectionMarker a where
  toNSBindingSelectionMarker :: a -> Id NSBindingSelectionMarker

instance IsNSBindingSelectionMarker (Id NSBindingSelectionMarker) where
  toNSBindingSelectionMarker = unsafeCastId

instance IsNSObject (Id NSBindingSelectionMarker) where
  toNSObject = unsafeCastId

-- ---------- NSCell ----------

-- | Phantom type for @NSCell@.
data NSCell

instance IsObjCObject (Id NSCell) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSCell"

class IsNSObject a => IsNSCell a where
  toNSCell :: a -> Id NSCell

instance IsNSCell (Id NSCell) where
  toNSCell = unsafeCastId

instance IsNSObject (Id NSCell) where
  toNSObject = unsafeCastId

-- ---------- NSCollectionLayoutAnchor ----------

-- | Phantom type for @NSCollectionLayoutAnchor@.
data NSCollectionLayoutAnchor

instance IsObjCObject (Id NSCollectionLayoutAnchor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSCollectionLayoutAnchor"

class IsNSObject a => IsNSCollectionLayoutAnchor a where
  toNSCollectionLayoutAnchor :: a -> Id NSCollectionLayoutAnchor

instance IsNSCollectionLayoutAnchor (Id NSCollectionLayoutAnchor) where
  toNSCollectionLayoutAnchor = unsafeCastId

instance IsNSObject (Id NSCollectionLayoutAnchor) where
  toNSObject = unsafeCastId

-- ---------- NSCollectionLayoutDimension ----------

-- | Phantom type for @NSCollectionLayoutDimension@.
data NSCollectionLayoutDimension

instance IsObjCObject (Id NSCollectionLayoutDimension) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSCollectionLayoutDimension"

class IsNSObject a => IsNSCollectionLayoutDimension a where
  toNSCollectionLayoutDimension :: a -> Id NSCollectionLayoutDimension

instance IsNSCollectionLayoutDimension (Id NSCollectionLayoutDimension) where
  toNSCollectionLayoutDimension = unsafeCastId

instance IsNSObject (Id NSCollectionLayoutDimension) where
  toNSObject = unsafeCastId

-- ---------- NSCollectionLayoutEdgeSpacing ----------

-- | Phantom type for @NSCollectionLayoutEdgeSpacing@.
data NSCollectionLayoutEdgeSpacing

instance IsObjCObject (Id NSCollectionLayoutEdgeSpacing) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSCollectionLayoutEdgeSpacing"

class IsNSObject a => IsNSCollectionLayoutEdgeSpacing a where
  toNSCollectionLayoutEdgeSpacing :: a -> Id NSCollectionLayoutEdgeSpacing

instance IsNSCollectionLayoutEdgeSpacing (Id NSCollectionLayoutEdgeSpacing) where
  toNSCollectionLayoutEdgeSpacing = unsafeCastId

instance IsNSObject (Id NSCollectionLayoutEdgeSpacing) where
  toNSObject = unsafeCastId

-- ---------- NSCollectionLayoutGroupCustomItem ----------

-- | Phantom type for @NSCollectionLayoutGroupCustomItem@.
data NSCollectionLayoutGroupCustomItem

instance IsObjCObject (Id NSCollectionLayoutGroupCustomItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSCollectionLayoutGroupCustomItem"

class IsNSObject a => IsNSCollectionLayoutGroupCustomItem a where
  toNSCollectionLayoutGroupCustomItem :: a -> Id NSCollectionLayoutGroupCustomItem

instance IsNSCollectionLayoutGroupCustomItem (Id NSCollectionLayoutGroupCustomItem) where
  toNSCollectionLayoutGroupCustomItem = unsafeCastId

instance IsNSObject (Id NSCollectionLayoutGroupCustomItem) where
  toNSObject = unsafeCastId

-- ---------- NSCollectionLayoutItem ----------

-- | Phantom type for @NSCollectionLayoutItem@.
data NSCollectionLayoutItem

instance IsObjCObject (Id NSCollectionLayoutItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSCollectionLayoutItem"

class IsNSObject a => IsNSCollectionLayoutItem a where
  toNSCollectionLayoutItem :: a -> Id NSCollectionLayoutItem

instance IsNSCollectionLayoutItem (Id NSCollectionLayoutItem) where
  toNSCollectionLayoutItem = unsafeCastId

instance IsNSObject (Id NSCollectionLayoutItem) where
  toNSObject = unsafeCastId

-- ---------- NSCollectionLayoutSection ----------

-- | Phantom type for @NSCollectionLayoutSection@.
data NSCollectionLayoutSection

instance IsObjCObject (Id NSCollectionLayoutSection) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSCollectionLayoutSection"

class IsNSObject a => IsNSCollectionLayoutSection a where
  toNSCollectionLayoutSection :: a -> Id NSCollectionLayoutSection

instance IsNSCollectionLayoutSection (Id NSCollectionLayoutSection) where
  toNSCollectionLayoutSection = unsafeCastId

instance IsNSObject (Id NSCollectionLayoutSection) where
  toNSObject = unsafeCastId

-- ---------- NSCollectionLayoutSize ----------

-- | Phantom type for @NSCollectionLayoutSize@.
data NSCollectionLayoutSize

instance IsObjCObject (Id NSCollectionLayoutSize) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSCollectionLayoutSize"

class IsNSObject a => IsNSCollectionLayoutSize a where
  toNSCollectionLayoutSize :: a -> Id NSCollectionLayoutSize

instance IsNSCollectionLayoutSize (Id NSCollectionLayoutSize) where
  toNSCollectionLayoutSize = unsafeCastId

instance IsNSObject (Id NSCollectionLayoutSize) where
  toNSObject = unsafeCastId

-- ---------- NSCollectionLayoutSpacing ----------

-- | Phantom type for @NSCollectionLayoutSpacing@.
data NSCollectionLayoutSpacing

instance IsObjCObject (Id NSCollectionLayoutSpacing) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSCollectionLayoutSpacing"

class IsNSObject a => IsNSCollectionLayoutSpacing a where
  toNSCollectionLayoutSpacing :: a -> Id NSCollectionLayoutSpacing

instance IsNSCollectionLayoutSpacing (Id NSCollectionLayoutSpacing) where
  toNSCollectionLayoutSpacing = unsafeCastId

instance IsNSObject (Id NSCollectionLayoutSpacing) where
  toNSObject = unsafeCastId

-- ---------- NSCollectionViewCompositionalLayoutConfiguration ----------

-- | Phantom type for @NSCollectionViewCompositionalLayoutConfiguration@.
data NSCollectionViewCompositionalLayoutConfiguration

instance IsObjCObject (Id NSCollectionViewCompositionalLayoutConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSCollectionViewCompositionalLayoutConfiguration"

class IsNSObject a => IsNSCollectionViewCompositionalLayoutConfiguration a where
  toNSCollectionViewCompositionalLayoutConfiguration :: a -> Id NSCollectionViewCompositionalLayoutConfiguration

instance IsNSCollectionViewCompositionalLayoutConfiguration (Id NSCollectionViewCompositionalLayoutConfiguration) where
  toNSCollectionViewCompositionalLayoutConfiguration = unsafeCastId

instance IsNSObject (Id NSCollectionViewCompositionalLayoutConfiguration) where
  toNSObject = unsafeCastId

-- ---------- NSCollectionViewDiffableDataSource ----------

-- | Phantom type for @NSCollectionViewDiffableDataSource@.
data NSCollectionViewDiffableDataSource

instance IsObjCObject (Id NSCollectionViewDiffableDataSource) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSCollectionViewDiffableDataSource"

class IsNSObject a => IsNSCollectionViewDiffableDataSource a where
  toNSCollectionViewDiffableDataSource :: a -> Id NSCollectionViewDiffableDataSource

instance IsNSCollectionViewDiffableDataSource (Id NSCollectionViewDiffableDataSource) where
  toNSCollectionViewDiffableDataSource = unsafeCastId

instance IsNSObject (Id NSCollectionViewDiffableDataSource) where
  toNSObject = unsafeCastId

-- ---------- NSCollectionViewLayout ----------

-- | Phantom type for @NSCollectionViewLayout@.
data NSCollectionViewLayout

instance IsObjCObject (Id NSCollectionViewLayout) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSCollectionViewLayout"

class IsNSObject a => IsNSCollectionViewLayout a where
  toNSCollectionViewLayout :: a -> Id NSCollectionViewLayout

instance IsNSCollectionViewLayout (Id NSCollectionViewLayout) where
  toNSCollectionViewLayout = unsafeCastId

instance IsNSObject (Id NSCollectionViewLayout) where
  toNSObject = unsafeCastId

-- ---------- NSCollectionViewLayoutAttributes ----------

-- | Phantom type for @NSCollectionViewLayoutAttributes@.
data NSCollectionViewLayoutAttributes

instance IsObjCObject (Id NSCollectionViewLayoutAttributes) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSCollectionViewLayoutAttributes"

class IsNSObject a => IsNSCollectionViewLayoutAttributes a where
  toNSCollectionViewLayoutAttributes :: a -> Id NSCollectionViewLayoutAttributes

instance IsNSCollectionViewLayoutAttributes (Id NSCollectionViewLayoutAttributes) where
  toNSCollectionViewLayoutAttributes = unsafeCastId

instance IsNSObject (Id NSCollectionViewLayoutAttributes) where
  toNSObject = unsafeCastId

-- ---------- NSCollectionViewLayoutInvalidationContext ----------

-- | Phantom type for @NSCollectionViewLayoutInvalidationContext@.
data NSCollectionViewLayoutInvalidationContext

instance IsObjCObject (Id NSCollectionViewLayoutInvalidationContext) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSCollectionViewLayoutInvalidationContext"

class IsNSObject a => IsNSCollectionViewLayoutInvalidationContext a where
  toNSCollectionViewLayoutInvalidationContext :: a -> Id NSCollectionViewLayoutInvalidationContext

instance IsNSCollectionViewLayoutInvalidationContext (Id NSCollectionViewLayoutInvalidationContext) where
  toNSCollectionViewLayoutInvalidationContext = unsafeCastId

instance IsNSObject (Id NSCollectionViewLayoutInvalidationContext) where
  toNSObject = unsafeCastId

-- ---------- NSCollectionViewUpdateItem ----------

-- | Phantom type for @NSCollectionViewUpdateItem@.
data NSCollectionViewUpdateItem

instance IsObjCObject (Id NSCollectionViewUpdateItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSCollectionViewUpdateItem"

class IsNSObject a => IsNSCollectionViewUpdateItem a where
  toNSCollectionViewUpdateItem :: a -> Id NSCollectionViewUpdateItem

instance IsNSCollectionViewUpdateItem (Id NSCollectionViewUpdateItem) where
  toNSCollectionViewUpdateItem = unsafeCastId

instance IsNSObject (Id NSCollectionViewUpdateItem) where
  toNSObject = unsafeCastId

-- ---------- NSColor ----------

-- | Phantom type for @NSColor@.
data NSColor

instance IsObjCObject (Id NSColor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSColor"

class IsNSObject a => IsNSColor a where
  toNSColor :: a -> Id NSColor

instance IsNSColor (Id NSColor) where
  toNSColor = unsafeCastId

instance IsNSObject (Id NSColor) where
  toNSObject = unsafeCastId

-- ---------- NSColorList ----------

-- | Phantom type for @NSColorList@.
data NSColorList

instance IsObjCObject (Id NSColorList) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSColorList"

class IsNSObject a => IsNSColorList a where
  toNSColorList :: a -> Id NSColorList

instance IsNSColorList (Id NSColorList) where
  toNSColorList = unsafeCastId

instance IsNSObject (Id NSColorList) where
  toNSObject = unsafeCastId

-- ---------- NSColorPicker ----------

-- | Phantom type for @NSColorPicker@.
data NSColorPicker

instance IsObjCObject (Id NSColorPicker) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSColorPicker"

class IsNSObject a => IsNSColorPicker a where
  toNSColorPicker :: a -> Id NSColorPicker

instance IsNSColorPicker (Id NSColorPicker) where
  toNSColorPicker = unsafeCastId

instance IsNSObject (Id NSColorPicker) where
  toNSObject = unsafeCastId

-- ---------- NSColorSampler ----------

-- | Manages a color sampling interface to allow the user to select a color from their screen.
-- 
-- Phantom type for @NSColorSampler@.
data NSColorSampler

instance IsObjCObject (Id NSColorSampler) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSColorSampler"

class IsNSObject a => IsNSColorSampler a where
  toNSColorSampler :: a -> Id NSColorSampler

instance IsNSColorSampler (Id NSColorSampler) where
  toNSColorSampler = unsafeCastId

instance IsNSObject (Id NSColorSampler) where
  toNSObject = unsafeCastId

-- ---------- NSColorSpace ----------

-- | Phantom type for @NSColorSpace@.
data NSColorSpace

instance IsObjCObject (Id NSColorSpace) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSColorSpace"

class IsNSObject a => IsNSColorSpace a where
  toNSColorSpace :: a -> Id NSColorSpace

instance IsNSColorSpace (Id NSColorSpace) where
  toNSColorSpace = unsafeCastId

instance IsNSObject (Id NSColorSpace) where
  toNSObject = unsafeCastId

-- ---------- NSController ----------

-- | Phantom type for @NSController@.
data NSController

instance IsObjCObject (Id NSController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSController"

class IsNSObject a => IsNSController a where
  toNSController :: a -> Id NSController

instance IsNSController (Id NSController) where
  toNSController = unsafeCastId

instance IsNSObject (Id NSController) where
  toNSObject = unsafeCastId

-- ---------- NSCursor ----------

-- | Phantom type for @NSCursor@.
data NSCursor

instance IsObjCObject (Id NSCursor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSCursor"

class IsNSObject a => IsNSCursor a where
  toNSCursor :: a -> Id NSCursor

instance IsNSCursor (Id NSCursor) where
  toNSCursor = unsafeCastId

instance IsNSObject (Id NSCursor) where
  toNSObject = unsafeCastId

-- ---------- NSDataAsset ----------

-- | Phantom type for @NSDataAsset@.
data NSDataAsset

instance IsObjCObject (Id NSDataAsset) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSDataAsset"

class IsNSObject a => IsNSDataAsset a where
  toNSDataAsset :: a -> Id NSDataAsset

instance IsNSDataAsset (Id NSDataAsset) where
  toNSDataAsset = unsafeCastId

instance IsNSObject (Id NSDataAsset) where
  toNSObject = unsafeCastId

-- ---------- NSDictionaryControllerKeyValuePair ----------

-- | Phantom type for @NSDictionaryControllerKeyValuePair@.
data NSDictionaryControllerKeyValuePair

instance IsObjCObject (Id NSDictionaryControllerKeyValuePair) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSDictionaryControllerKeyValuePair"

class IsNSObject a => IsNSDictionaryControllerKeyValuePair a where
  toNSDictionaryControllerKeyValuePair :: a -> Id NSDictionaryControllerKeyValuePair

instance IsNSDictionaryControllerKeyValuePair (Id NSDictionaryControllerKeyValuePair) where
  toNSDictionaryControllerKeyValuePair = unsafeCastId

instance IsNSObject (Id NSDictionaryControllerKeyValuePair) where
  toNSObject = unsafeCastId

-- ---------- NSDiffableDataSourceSnapshot ----------

-- | Phantom type for @NSDiffableDataSourceSnapshot@.
data NSDiffableDataSourceSnapshot

instance IsObjCObject (Id NSDiffableDataSourceSnapshot) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSDiffableDataSourceSnapshot"

class IsNSObject a => IsNSDiffableDataSourceSnapshot a where
  toNSDiffableDataSourceSnapshot :: a -> Id NSDiffableDataSourceSnapshot

instance IsNSDiffableDataSourceSnapshot (Id NSDiffableDataSourceSnapshot) where
  toNSDiffableDataSourceSnapshot = unsafeCastId

instance IsNSObject (Id NSDiffableDataSourceSnapshot) where
  toNSObject = unsafeCastId

-- ---------- NSDockTile ----------

-- | Phantom type for @NSDockTile@.
data NSDockTile

instance IsObjCObject (Id NSDockTile) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSDockTile"

class IsNSObject a => IsNSDockTile a where
  toNSDockTile :: a -> Id NSDockTile

instance IsNSDockTile (Id NSDockTile) where
  toNSDockTile = unsafeCastId

instance IsNSObject (Id NSDockTile) where
  toNSObject = unsafeCastId

-- ---------- NSDocument ----------

-- | Phantom type for @NSDocument@.
data NSDocument

instance IsObjCObject (Id NSDocument) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSDocument"

class IsNSObject a => IsNSDocument a where
  toNSDocument :: a -> Id NSDocument

instance IsNSDocument (Id NSDocument) where
  toNSDocument = unsafeCastId

instance IsNSObject (Id NSDocument) where
  toNSObject = unsafeCastId

-- ---------- NSDocumentController ----------

-- | Phantom type for @NSDocumentController@.
data NSDocumentController

instance IsObjCObject (Id NSDocumentController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSDocumentController"

class IsNSObject a => IsNSDocumentController a where
  toNSDocumentController :: a -> Id NSDocumentController

instance IsNSDocumentController (Id NSDocumentController) where
  toNSDocumentController = unsafeCastId

instance IsNSObject (Id NSDocumentController) where
  toNSObject = unsafeCastId

-- ---------- NSDraggingImageComponent ----------

-- | Phantom type for @NSDraggingImageComponent@.
data NSDraggingImageComponent

instance IsObjCObject (Id NSDraggingImageComponent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSDraggingImageComponent"

class IsNSObject a => IsNSDraggingImageComponent a where
  toNSDraggingImageComponent :: a -> Id NSDraggingImageComponent

instance IsNSDraggingImageComponent (Id NSDraggingImageComponent) where
  toNSDraggingImageComponent = unsafeCastId

instance IsNSObject (Id NSDraggingImageComponent) where
  toNSObject = unsafeCastId

-- ---------- NSDraggingItem ----------

-- | Phantom type for @NSDraggingItem@.
data NSDraggingItem

instance IsObjCObject (Id NSDraggingItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSDraggingItem"

class IsNSObject a => IsNSDraggingItem a where
  toNSDraggingItem :: a -> Id NSDraggingItem

instance IsNSDraggingItem (Id NSDraggingItem) where
  toNSDraggingItem = unsafeCastId

instance IsNSObject (Id NSDraggingItem) where
  toNSObject = unsafeCastId

-- ---------- NSDraggingSession ----------

-- | Phantom type for @NSDraggingSession@.
data NSDraggingSession

instance IsObjCObject (Id NSDraggingSession) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSDraggingSession"

class IsNSObject a => IsNSDraggingSession a where
  toNSDraggingSession :: a -> Id NSDraggingSession

instance IsNSDraggingSession (Id NSDraggingSession) where
  toNSDraggingSession = unsafeCastId

instance IsNSObject (Id NSDraggingSession) where
  toNSObject = unsafeCastId

-- ---------- NSEvent ----------

-- | Phantom type for @NSEvent@.
data NSEvent

instance IsObjCObject (Id NSEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSEvent"

class IsNSObject a => IsNSEvent a where
  toNSEvent :: a -> Id NSEvent

instance IsNSEvent (Id NSEvent) where
  toNSEvent = unsafeCastId

instance IsNSObject (Id NSEvent) where
  toNSObject = unsafeCastId

-- ---------- NSFilePromiseProvider ----------

-- | Phantom type for @NSFilePromiseProvider@.
data NSFilePromiseProvider

instance IsObjCObject (Id NSFilePromiseProvider) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSFilePromiseProvider"

class IsNSObject a => IsNSFilePromiseProvider a where
  toNSFilePromiseProvider :: a -> Id NSFilePromiseProvider

instance IsNSFilePromiseProvider (Id NSFilePromiseProvider) where
  toNSFilePromiseProvider = unsafeCastId

instance IsNSObject (Id NSFilePromiseProvider) where
  toNSObject = unsafeCastId

-- ---------- NSFilePromiseReceiver ----------

-- | Phantom type for @NSFilePromiseReceiver@.
data NSFilePromiseReceiver

instance IsObjCObject (Id NSFilePromiseReceiver) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSFilePromiseReceiver"

class IsNSObject a => IsNSFilePromiseReceiver a where
  toNSFilePromiseReceiver :: a -> Id NSFilePromiseReceiver

instance IsNSFilePromiseReceiver (Id NSFilePromiseReceiver) where
  toNSFilePromiseReceiver = unsafeCastId

instance IsNSObject (Id NSFilePromiseReceiver) where
  toNSObject = unsafeCastId

-- ---------- NSFont ----------

-- | Phantom type for @NSFont@.
data NSFont

instance IsObjCObject (Id NSFont) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSFont"

class IsNSObject a => IsNSFont a where
  toNSFont :: a -> Id NSFont

instance IsNSFont (Id NSFont) where
  toNSFont = unsafeCastId

instance IsNSObject (Id NSFont) where
  toNSObject = unsafeCastId

-- ---------- NSFontAssetRequest ----------

-- | Phantom type for @NSFontAssetRequest@.
data NSFontAssetRequest

instance IsObjCObject (Id NSFontAssetRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSFontAssetRequest"

class IsNSObject a => IsNSFontAssetRequest a where
  toNSFontAssetRequest :: a -> Id NSFontAssetRequest

instance IsNSFontAssetRequest (Id NSFontAssetRequest) where
  toNSFontAssetRequest = unsafeCastId

instance IsNSObject (Id NSFontAssetRequest) where
  toNSObject = unsafeCastId

-- ---------- NSFontCollection ----------

-- | Phantom type for @NSFontCollection@.
data NSFontCollection

instance IsObjCObject (Id NSFontCollection) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSFontCollection"

class IsNSObject a => IsNSFontCollection a where
  toNSFontCollection :: a -> Id NSFontCollection

instance IsNSFontCollection (Id NSFontCollection) where
  toNSFontCollection = unsafeCastId

instance IsNSObject (Id NSFontCollection) where
  toNSObject = unsafeCastId

-- ---------- NSFontDescriptor ----------

-- | Phantom type for @NSFontDescriptor@.
data NSFontDescriptor

instance IsObjCObject (Id NSFontDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSFontDescriptor"

class IsNSObject a => IsNSFontDescriptor a where
  toNSFontDescriptor :: a -> Id NSFontDescriptor

instance IsNSFontDescriptor (Id NSFontDescriptor) where
  toNSFontDescriptor = unsafeCastId

instance IsNSObject (Id NSFontDescriptor) where
  toNSObject = unsafeCastId

-- ---------- NSFontManager ----------

-- | Phantom type for @NSFontManager@.
data NSFontManager

instance IsObjCObject (Id NSFontManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSFontManager"

class IsNSObject a => IsNSFontManager a where
  toNSFontManager :: a -> Id NSFontManager

instance IsNSFontManager (Id NSFontManager) where
  toNSFontManager = unsafeCastId

instance IsNSObject (Id NSFontManager) where
  toNSObject = unsafeCastId

-- ---------- NSGestureRecognizer ----------

-- | Phantom type for @NSGestureRecognizer@.
data NSGestureRecognizer

instance IsObjCObject (Id NSGestureRecognizer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSGestureRecognizer"

class IsNSObject a => IsNSGestureRecognizer a where
  toNSGestureRecognizer :: a -> Id NSGestureRecognizer

instance IsNSGestureRecognizer (Id NSGestureRecognizer) where
  toNSGestureRecognizer = unsafeCastId

instance IsNSObject (Id NSGestureRecognizer) where
  toNSObject = unsafeCastId

-- ---------- NSGlyphGenerator ----------

-- | Phantom type for @NSGlyphGenerator@.
data NSGlyphGenerator

instance IsObjCObject (Id NSGlyphGenerator) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSGlyphGenerator"

class IsNSObject a => IsNSGlyphGenerator a where
  toNSGlyphGenerator :: a -> Id NSGlyphGenerator

instance IsNSGlyphGenerator (Id NSGlyphGenerator) where
  toNSGlyphGenerator = unsafeCastId

instance IsNSObject (Id NSGlyphGenerator) where
  toNSObject = unsafeCastId

-- ---------- NSGlyphInfo ----------

-- | Phantom type for @NSGlyphInfo@.
data NSGlyphInfo

instance IsObjCObject (Id NSGlyphInfo) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSGlyphInfo"

class IsNSObject a => IsNSGlyphInfo a where
  toNSGlyphInfo :: a -> Id NSGlyphInfo

instance IsNSGlyphInfo (Id NSGlyphInfo) where
  toNSGlyphInfo = unsafeCastId

instance IsNSObject (Id NSGlyphInfo) where
  toNSObject = unsafeCastId

-- ---------- NSGradient ----------

-- | Phantom type for @NSGradient@.
data NSGradient

instance IsObjCObject (Id NSGradient) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSGradient"

class IsNSObject a => IsNSGradient a where
  toNSGradient :: a -> Id NSGradient

instance IsNSGradient (Id NSGradient) where
  toNSGradient = unsafeCastId

instance IsNSObject (Id NSGradient) where
  toNSObject = unsafeCastId

-- ---------- NSGraphicsContext ----------

-- | Phantom type for @NSGraphicsContext@.
data NSGraphicsContext

instance IsObjCObject (Id NSGraphicsContext) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSGraphicsContext"

class IsNSObject a => IsNSGraphicsContext a where
  toNSGraphicsContext :: a -> Id NSGraphicsContext

instance IsNSGraphicsContext (Id NSGraphicsContext) where
  toNSGraphicsContext = unsafeCastId

instance IsNSObject (Id NSGraphicsContext) where
  toNSObject = unsafeCastId

-- ---------- NSGridCell ----------

-- | Phantom type for @NSGridCell@.
data NSGridCell

instance IsObjCObject (Id NSGridCell) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSGridCell"

class IsNSObject a => IsNSGridCell a where
  toNSGridCell :: a -> Id NSGridCell

instance IsNSGridCell (Id NSGridCell) where
  toNSGridCell = unsafeCastId

instance IsNSObject (Id NSGridCell) where
  toNSObject = unsafeCastId

-- ---------- NSGridColumn ----------

-- | Phantom type for @NSGridColumn@.
data NSGridColumn

instance IsObjCObject (Id NSGridColumn) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSGridColumn"

class IsNSObject a => IsNSGridColumn a where
  toNSGridColumn :: a -> Id NSGridColumn

instance IsNSGridColumn (Id NSGridColumn) where
  toNSGridColumn = unsafeCastId

instance IsNSObject (Id NSGridColumn) where
  toNSObject = unsafeCastId

-- ---------- NSGridRow ----------

-- | Phantom type for @NSGridRow@.
data NSGridRow

instance IsObjCObject (Id NSGridRow) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSGridRow"

class IsNSObject a => IsNSGridRow a where
  toNSGridRow :: a -> Id NSGridRow

instance IsNSGridRow (Id NSGridRow) where
  toNSGridRow = unsafeCastId

instance IsNSObject (Id NSGridRow) where
  toNSObject = unsafeCastId

-- ---------- NSHapticFeedbackManager ----------

-- | Phantom type for @NSHapticFeedbackManager@.
data NSHapticFeedbackManager

instance IsObjCObject (Id NSHapticFeedbackManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSHapticFeedbackManager"

class IsNSObject a => IsNSHapticFeedbackManager a where
  toNSHapticFeedbackManager :: a -> Id NSHapticFeedbackManager

instance IsNSHapticFeedbackManager (Id NSHapticFeedbackManager) where
  toNSHapticFeedbackManager = unsafeCastId

instance IsNSObject (Id NSHapticFeedbackManager) where
  toNSObject = unsafeCastId

-- ---------- NSHelpManager ----------

-- | Phantom type for @NSHelpManager@.
data NSHelpManager

instance IsObjCObject (Id NSHelpManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSHelpManager"

class IsNSObject a => IsNSHelpManager a where
  toNSHelpManager :: a -> Id NSHelpManager

instance IsNSHelpManager (Id NSHelpManager) where
  toNSHelpManager = unsafeCastId

instance IsNSObject (Id NSHelpManager) where
  toNSObject = unsafeCastId

-- ---------- NSImage ----------

-- | Phantom type for @NSImage@.
data NSImage

instance IsObjCObject (Id NSImage) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSImage"

class IsNSObject a => IsNSImage a where
  toNSImage :: a -> Id NSImage

instance IsNSImage (Id NSImage) where
  toNSImage = unsafeCastId

instance IsNSObject (Id NSImage) where
  toNSObject = unsafeCastId

-- ---------- NSImageRep ----------

-- | Phantom type for @NSImageRep@.
data NSImageRep

instance IsObjCObject (Id NSImageRep) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSImageRep"

class IsNSObject a => IsNSImageRep a where
  toNSImageRep :: a -> Id NSImageRep

instance IsNSImageRep (Id NSImageRep) where
  toNSImageRep = unsafeCastId

instance IsNSObject (Id NSImageRep) where
  toNSObject = unsafeCastId

-- ---------- NSImageSymbolConfiguration ----------

-- | Phantom type for @NSImageSymbolConfiguration@.
data NSImageSymbolConfiguration

instance IsObjCObject (Id NSImageSymbolConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSImageSymbolConfiguration"

class IsNSObject a => IsNSImageSymbolConfiguration a where
  toNSImageSymbolConfiguration :: a -> Id NSImageSymbolConfiguration

instance IsNSImageSymbolConfiguration (Id NSImageSymbolConfiguration) where
  toNSImageSymbolConfiguration = unsafeCastId

instance IsNSObject (Id NSImageSymbolConfiguration) where
  toNSObject = unsafeCastId

-- ---------- NSInputManager ----------

-- | Phantom type for @NSInputManager@.
data NSInputManager

instance IsObjCObject (Id NSInputManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSInputManager"

class IsNSObject a => IsNSInputManager a where
  toNSInputManager :: a -> Id NSInputManager

instance IsNSInputManager (Id NSInputManager) where
  toNSInputManager = unsafeCastId

instance IsNSObject (Id NSInputManager) where
  toNSObject = unsafeCastId

-- ---------- NSInputServer ----------

-- | Phantom type for @NSInputServer@.
data NSInputServer

instance IsObjCObject (Id NSInputServer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSInputServer"

class IsNSObject a => IsNSInputServer a where
  toNSInputServer :: a -> Id NSInputServer

instance IsNSInputServer (Id NSInputServer) where
  toNSInputServer = unsafeCastId

instance IsNSObject (Id NSInputServer) where
  toNSObject = unsafeCastId

-- ---------- NSItemBadge ----------

-- | @NSItemBadge@ represents a badge that can be attached to an @NSToolbarItem@.
--
-- This badge provides a way to display small visual indicators, such as counts and text labels, within a toolbar item. Badges can be used to highlight important information, such as unread notifications or status indicators.
-- 
-- Phantom type for @NSItemBadge@.
data NSItemBadge

instance IsObjCObject (Id NSItemBadge) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSItemBadge"

class IsNSObject a => IsNSItemBadge a where
  toNSItemBadge :: a -> Id NSItemBadge

instance IsNSItemBadge (Id NSItemBadge) where
  toNSItemBadge = unsafeCastId

instance IsNSObject (Id NSItemBadge) where
  toNSObject = unsafeCastId

-- ---------- NSLayoutAnchor ----------

-- | Phantom type for @NSLayoutAnchor@.
data NSLayoutAnchor

instance IsObjCObject (Id NSLayoutAnchor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSLayoutAnchor"

class IsNSObject a => IsNSLayoutAnchor a where
  toNSLayoutAnchor :: a -> Id NSLayoutAnchor

instance IsNSLayoutAnchor (Id NSLayoutAnchor) where
  toNSLayoutAnchor = unsafeCastId

instance IsNSObject (Id NSLayoutAnchor) where
  toNSObject = unsafeCastId

-- ---------- NSLayoutConstraint ----------

-- | Phantom type for @NSLayoutConstraint@.
data NSLayoutConstraint

instance IsObjCObject (Id NSLayoutConstraint) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSLayoutConstraint"

class IsNSObject a => IsNSLayoutConstraint a where
  toNSLayoutConstraint :: a -> Id NSLayoutConstraint

instance IsNSLayoutConstraint (Id NSLayoutConstraint) where
  toNSLayoutConstraint = unsafeCastId

instance IsNSObject (Id NSLayoutConstraint) where
  toNSObject = unsafeCastId

-- ---------- NSLayoutGuide ----------

-- | Phantom type for @NSLayoutGuide@.
data NSLayoutGuide

instance IsObjCObject (Id NSLayoutGuide) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSLayoutGuide"

class IsNSObject a => IsNSLayoutGuide a where
  toNSLayoutGuide :: a -> Id NSLayoutGuide

instance IsNSLayoutGuide (Id NSLayoutGuide) where
  toNSLayoutGuide = unsafeCastId

instance IsNSObject (Id NSLayoutGuide) where
  toNSObject = unsafeCastId

-- ---------- NSLayoutManager ----------

-- | Phantom type for @NSLayoutManager@.
data NSLayoutManager

instance IsObjCObject (Id NSLayoutManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSLayoutManager"

class IsNSObject a => IsNSLayoutManager a where
  toNSLayoutManager :: a -> Id NSLayoutManager

instance IsNSLayoutManager (Id NSLayoutManager) where
  toNSLayoutManager = unsafeCastId

instance IsNSObject (Id NSLayoutManager) where
  toNSObject = unsafeCastId

-- ---------- NSManagedObjectContext ----------

-- | Phantom type for @NSManagedObjectContext@.
data NSManagedObjectContext

instance IsObjCObject (Id NSManagedObjectContext) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSManagedObjectContext"

class IsNSObject a => IsNSManagedObjectContext a where
  toNSManagedObjectContext :: a -> Id NSManagedObjectContext

instance IsNSManagedObjectContext (Id NSManagedObjectContext) where
  toNSManagedObjectContext = unsafeCastId

instance IsNSObject (Id NSManagedObjectContext) where
  toNSObject = unsafeCastId

-- ---------- NSMediaLibraryBrowserController ----------

-- | This class configures and displays a media browser panel.
--
-- This class provides no direct access to the panel, and its meaningful contents aren't in the calling process.
-- 
-- Phantom type for @NSMediaLibraryBrowserController@.
data NSMediaLibraryBrowserController

instance IsObjCObject (Id NSMediaLibraryBrowserController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSMediaLibraryBrowserController"

class IsNSObject a => IsNSMediaLibraryBrowserController a where
  toNSMediaLibraryBrowserController :: a -> Id NSMediaLibraryBrowserController

instance IsNSMediaLibraryBrowserController (Id NSMediaLibraryBrowserController) where
  toNSMediaLibraryBrowserController = unsafeCastId

instance IsNSObject (Id NSMediaLibraryBrowserController) where
  toNSObject = unsafeCastId

-- ---------- NSMenu ----------

-- | Phantom type for @NSMenu@.
data NSMenu

instance IsObjCObject (Id NSMenu) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSMenu"

class IsNSObject a => IsNSMenu a where
  toNSMenu :: a -> Id NSMenu

instance IsNSMenu (Id NSMenu) where
  toNSMenu = unsafeCastId

instance IsNSObject (Id NSMenu) where
  toNSObject = unsafeCastId

-- ---------- NSMenuItem ----------

-- | Phantom type for @NSMenuItem@.
data NSMenuItem

instance IsObjCObject (Id NSMenuItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSMenuItem"

class IsNSObject a => IsNSMenuItem a where
  toNSMenuItem :: a -> Id NSMenuItem

instance IsNSMenuItem (Id NSMenuItem) where
  toNSMenuItem = unsafeCastId

instance IsNSObject (Id NSMenuItem) where
  toNSObject = unsafeCastId

-- ---------- NSMenuItemBadge ----------

-- | A badge used to provide additional quantitative information specific to the menu item, such as the number of available updates.
-- 
-- Phantom type for @NSMenuItemBadge@.
data NSMenuItemBadge

instance IsObjCObject (Id NSMenuItemBadge) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSMenuItemBadge"

class IsNSObject a => IsNSMenuItemBadge a where
  toNSMenuItemBadge :: a -> Id NSMenuItemBadge

instance IsNSMenuItemBadge (Id NSMenuItemBadge) where
  toNSMenuItemBadge = unsafeCastId

instance IsNSObject (Id NSMenuItemBadge) where
  toNSObject = unsafeCastId

-- ---------- NSMovie ----------

-- | Phantom type for @NSMovie@.
data NSMovie

instance IsObjCObject (Id NSMovie) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSMovie"

class IsNSObject a => IsNSMovie a where
  toNSMovie :: a -> Id NSMovie

instance IsNSMovie (Id NSMovie) where
  toNSMovie = unsafeCastId

instance IsNSObject (Id NSMovie) where
  toNSObject = unsafeCastId

-- ---------- NSNib ----------

-- | Phantom type for @NSNib@.
data NSNib

instance IsObjCObject (Id NSNib) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSNib"

class IsNSObject a => IsNSNib a where
  toNSNib :: a -> Id NSNib

instance IsNSNib (Id NSNib) where
  toNSNib = unsafeCastId

instance IsNSObject (Id NSNib) where
  toNSObject = unsafeCastId

-- ---------- NSOpenGLContext ----------

-- | Phantom type for @NSOpenGLContext@.
data NSOpenGLContext

instance IsObjCObject (Id NSOpenGLContext) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSOpenGLContext"

class IsNSObject a => IsNSOpenGLContext a where
  toNSOpenGLContext :: a -> Id NSOpenGLContext

instance IsNSOpenGLContext (Id NSOpenGLContext) where
  toNSOpenGLContext = unsafeCastId

instance IsNSObject (Id NSOpenGLContext) where
  toNSObject = unsafeCastId

-- ---------- NSOpenGLPixelBuffer ----------

-- | ******************** NSOpenGLPixelBuffer*******************
-- 
-- Phantom type for @NSOpenGLPixelBuffer@.
data NSOpenGLPixelBuffer

instance IsObjCObject (Id NSOpenGLPixelBuffer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSOpenGLPixelBuffer"

class IsNSObject a => IsNSOpenGLPixelBuffer a where
  toNSOpenGLPixelBuffer :: a -> Id NSOpenGLPixelBuffer

instance IsNSOpenGLPixelBuffer (Id NSOpenGLPixelBuffer) where
  toNSOpenGLPixelBuffer = unsafeCastId

instance IsNSObject (Id NSOpenGLPixelBuffer) where
  toNSObject = unsafeCastId

-- ---------- NSOpenGLPixelFormat ----------

-- | Phantom type for @NSOpenGLPixelFormat@.
data NSOpenGLPixelFormat

instance IsObjCObject (Id NSOpenGLPixelFormat) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSOpenGLPixelFormat"

class IsNSObject a => IsNSOpenGLPixelFormat a where
  toNSOpenGLPixelFormat :: a -> Id NSOpenGLPixelFormat

instance IsNSOpenGLPixelFormat (Id NSOpenGLPixelFormat) where
  toNSOpenGLPixelFormat = unsafeCastId

instance IsNSObject (Id NSOpenGLPixelFormat) where
  toNSObject = unsafeCastId

-- ---------- NSPDFInfo ----------

-- | Phantom type for @NSPDFInfo@.
data NSPDFInfo

instance IsObjCObject (Id NSPDFInfo) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPDFInfo"

class IsNSObject a => IsNSPDFInfo a where
  toNSPDFInfo :: a -> Id NSPDFInfo

instance IsNSPDFInfo (Id NSPDFInfo) where
  toNSPDFInfo = unsafeCastId

instance IsNSObject (Id NSPDFInfo) where
  toNSObject = unsafeCastId

-- ---------- NSPDFPanel ----------

-- | Phantom type for @NSPDFPanel@.
data NSPDFPanel

instance IsObjCObject (Id NSPDFPanel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPDFPanel"

class IsNSObject a => IsNSPDFPanel a where
  toNSPDFPanel :: a -> Id NSPDFPanel

instance IsNSPDFPanel (Id NSPDFPanel) where
  toNSPDFPanel = unsafeCastId

instance IsNSObject (Id NSPDFPanel) where
  toNSObject = unsafeCastId

-- ---------- NSPageLayout ----------

-- | Phantom type for @NSPageLayout@.
data NSPageLayout

instance IsObjCObject (Id NSPageLayout) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPageLayout"

class IsNSObject a => IsNSPageLayout a where
  toNSPageLayout :: a -> Id NSPageLayout

instance IsNSPageLayout (Id NSPageLayout) where
  toNSPageLayout = unsafeCastId

instance IsNSObject (Id NSPageLayout) where
  toNSObject = unsafeCastId

-- ---------- NSParagraphStyle ----------

-- | Phantom type for @NSParagraphStyle@.
data NSParagraphStyle

instance IsObjCObject (Id NSParagraphStyle) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSParagraphStyle"

class IsNSObject a => IsNSParagraphStyle a where
  toNSParagraphStyle :: a -> Id NSParagraphStyle

instance IsNSParagraphStyle (Id NSParagraphStyle) where
  toNSParagraphStyle = unsafeCastId

instance IsNSObject (Id NSParagraphStyle) where
  toNSObject = unsafeCastId

-- ---------- NSPasteboard ----------

-- | Phantom type for @NSPasteboard@.
data NSPasteboard

instance IsObjCObject (Id NSPasteboard) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPasteboard"

class IsNSObject a => IsNSPasteboard a where
  toNSPasteboard :: a -> Id NSPasteboard

instance IsNSPasteboard (Id NSPasteboard) where
  toNSPasteboard = unsafeCastId

instance IsNSObject (Id NSPasteboard) where
  toNSObject = unsafeCastId

-- ---------- NSPasteboardItem ----------

-- | Phantom type for @NSPasteboardItem@.
data NSPasteboardItem

instance IsObjCObject (Id NSPasteboardItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPasteboardItem"

class IsNSObject a => IsNSPasteboardItem a where
  toNSPasteboardItem :: a -> Id NSPasteboardItem

instance IsNSPasteboardItem (Id NSPasteboardItem) where
  toNSPasteboardItem = unsafeCastId

instance IsNSObject (Id NSPasteboardItem) where
  toNSObject = unsafeCastId

-- ---------- NSPathControlItem ----------

-- | Phantom type for @NSPathControlItem@.
data NSPathControlItem

instance IsObjCObject (Id NSPathControlItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPathControlItem"

class IsNSObject a => IsNSPathControlItem a where
  toNSPathControlItem :: a -> Id NSPathControlItem

instance IsNSPathControlItem (Id NSPathControlItem) where
  toNSPathControlItem = unsafeCastId

instance IsNSObject (Id NSPathControlItem) where
  toNSObject = unsafeCastId

-- ---------- NSPredicateEditorRowTemplate ----------

-- | Phantom type for @NSPredicateEditorRowTemplate@.
data NSPredicateEditorRowTemplate

instance IsObjCObject (Id NSPredicateEditorRowTemplate) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPredicateEditorRowTemplate"

class IsNSObject a => IsNSPredicateEditorRowTemplate a where
  toNSPredicateEditorRowTemplate :: a -> Id NSPredicateEditorRowTemplate

instance IsNSPredicateEditorRowTemplate (Id NSPredicateEditorRowTemplate) where
  toNSPredicateEditorRowTemplate = unsafeCastId

instance IsNSObject (Id NSPredicateEditorRowTemplate) where
  toNSObject = unsafeCastId

-- ---------- NSPressureConfiguration ----------

-- | Phantom type for @NSPressureConfiguration@.
data NSPressureConfiguration

instance IsObjCObject (Id NSPressureConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPressureConfiguration"

class IsNSObject a => IsNSPressureConfiguration a where
  toNSPressureConfiguration :: a -> Id NSPressureConfiguration

instance IsNSPressureConfiguration (Id NSPressureConfiguration) where
  toNSPressureConfiguration = unsafeCastId

instance IsNSObject (Id NSPressureConfiguration) where
  toNSObject = unsafeCastId

-- ---------- NSPreviewRepresentingActivityItem ----------

-- | Phantom type for @NSPreviewRepresentingActivityItem@.
data NSPreviewRepresentingActivityItem

instance IsObjCObject (Id NSPreviewRepresentingActivityItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPreviewRepresentingActivityItem"

class IsNSObject a => IsNSPreviewRepresentingActivityItem a where
  toNSPreviewRepresentingActivityItem :: a -> Id NSPreviewRepresentingActivityItem

instance IsNSPreviewRepresentingActivityItem (Id NSPreviewRepresentingActivityItem) where
  toNSPreviewRepresentingActivityItem = unsafeCastId

instance IsNSObject (Id NSPreviewRepresentingActivityItem) where
  toNSObject = unsafeCastId

-- ---------- NSPrintInfo ----------

-- | Phantom type for @NSPrintInfo@.
data NSPrintInfo

instance IsObjCObject (Id NSPrintInfo) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPrintInfo"

class IsNSObject a => IsNSPrintInfo a where
  toNSPrintInfo :: a -> Id NSPrintInfo

instance IsNSPrintInfo (Id NSPrintInfo) where
  toNSPrintInfo = unsafeCastId

instance IsNSObject (Id NSPrintInfo) where
  toNSObject = unsafeCastId

-- ---------- NSPrintOperation ----------

-- | Phantom type for @NSPrintOperation@.
data NSPrintOperation

instance IsObjCObject (Id NSPrintOperation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPrintOperation"

class IsNSObject a => IsNSPrintOperation a where
  toNSPrintOperation :: a -> Id NSPrintOperation

instance IsNSPrintOperation (Id NSPrintOperation) where
  toNSPrintOperation = unsafeCastId

instance IsNSObject (Id NSPrintOperation) where
  toNSObject = unsafeCastId

-- ---------- NSPrintPanel ----------

-- | Phantom type for @NSPrintPanel@.
data NSPrintPanel

instance IsObjCObject (Id NSPrintPanel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPrintPanel"

class IsNSObject a => IsNSPrintPanel a where
  toNSPrintPanel :: a -> Id NSPrintPanel

instance IsNSPrintPanel (Id NSPrintPanel) where
  toNSPrintPanel = unsafeCastId

instance IsNSObject (Id NSPrintPanel) where
  toNSObject = unsafeCastId

-- ---------- NSPrinter ----------

-- | Phantom type for @NSPrinter@.
data NSPrinter

instance IsObjCObject (Id NSPrinter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPrinter"

class IsNSObject a => IsNSPrinter a where
  toNSPrinter :: a -> Id NSPrinter

instance IsNSPrinter (Id NSPrinter) where
  toNSPrinter = unsafeCastId

instance IsNSObject (Id NSPrinter) where
  toNSObject = unsafeCastId

-- ---------- NSResponder ----------

-- | Phantom type for @NSResponder@.
data NSResponder

instance IsObjCObject (Id NSResponder) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSResponder"

class IsNSObject a => IsNSResponder a where
  toNSResponder :: a -> Id NSResponder

instance IsNSResponder (Id NSResponder) where
  toNSResponder = unsafeCastId

instance IsNSObject (Id NSResponder) where
  toNSObject = unsafeCastId

-- ---------- NSRulerMarker ----------

-- | Phantom type for @NSRulerMarker@.
data NSRulerMarker

instance IsObjCObject (Id NSRulerMarker) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSRulerMarker"

class IsNSObject a => IsNSRulerMarker a where
  toNSRulerMarker :: a -> Id NSRulerMarker

instance IsNSRulerMarker (Id NSRulerMarker) where
  toNSRulerMarker = unsafeCastId

instance IsNSObject (Id NSRulerMarker) where
  toNSObject = unsafeCastId

-- ---------- NSRunningApplication ----------

-- | @NSRunningApplication@ is a class to manipulate and provide information for a single instance of an application.  Only user applications are tracked; this does not provide information about every process on the system.
--
-- Some properties of an application are fixed, such as the bundle identifier.  Other properties may vary over time, such as whether the app is hidden.  Properties that vary can be observed with KVO, in which case the description comment for the method will mention it.
--
-- Properties that vary over time are inherently race-prone.  For example, a hidden app may unhide itself at any time.  To ameliorate this, properties persist until the next turn of the main run loop in a common mode.  For example, if you repeatedly poll an unhidden app for its hidden property without allowing the run loop to run, it will continue to return @NO@, even if the app hides, until the next turn of the run loop.
--
-- @NSRunningApplication@ is thread safe, in that its properties are returned atomically.  However, it is still subject to the main run loop policy described above.  If you access an instance of @NSRunningApplication@ from a background thread, be aware that its time-varying properties may change from under you as the main run loop runs (or not).
--
-- An @NSRunningApplication@ instance remains valid after the application exits.  However, most properties lose their significance, and some properties may not be available on a terminated application.
--
-- To access the list of all running applications, use the @-runningApplications@ method on @NSWorkspace@.
-- 
-- Phantom type for @NSRunningApplication@.
data NSRunningApplication

instance IsObjCObject (Id NSRunningApplication) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSRunningApplication"

class IsNSObject a => IsNSRunningApplication a where
  toNSRunningApplication :: a -> Id NSRunningApplication

instance IsNSRunningApplication (Id NSRunningApplication) where
  toNSRunningApplication = unsafeCastId

instance IsNSObject (Id NSRunningApplication) where
  toNSObject = unsafeCastId

-- ---------- NSScreen ----------

-- | Phantom type for @NSScreen@.
data NSScreen

instance IsObjCObject (Id NSScreen) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSScreen"

class IsNSObject a => IsNSScreen a where
  toNSScreen :: a -> Id NSScreen

instance IsNSScreen (Id NSScreen) where
  toNSScreen = unsafeCastId

instance IsNSObject (Id NSScreen) where
  toNSObject = unsafeCastId

-- ---------- NSScrollEdgeEffectStyle ----------

-- | Styles for a scroll view’s edge effect.
-- 
-- Phantom type for @NSScrollEdgeEffectStyle@.
data NSScrollEdgeEffectStyle

instance IsObjCObject (Id NSScrollEdgeEffectStyle) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSScrollEdgeEffectStyle"

class IsNSObject a => IsNSScrollEdgeEffectStyle a where
  toNSScrollEdgeEffectStyle :: a -> Id NSScrollEdgeEffectStyle

instance IsNSScrollEdgeEffectStyle (Id NSScrollEdgeEffectStyle) where
  toNSScrollEdgeEffectStyle = unsafeCastId

instance IsNSObject (Id NSScrollEdgeEffectStyle) where
  toNSObject = unsafeCastId

-- ---------- NSScrubberLayout ----------

-- | NSScrubberLayout
--
-- @NSScrubberLayout@ is an abstract class that describes the layout of items within a @NSScrubber@ control.
-- 
-- Phantom type for @NSScrubberLayout@.
data NSScrubberLayout

instance IsObjCObject (Id NSScrubberLayout) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSScrubberLayout"

class IsNSObject a => IsNSScrubberLayout a where
  toNSScrubberLayout :: a -> Id NSScrubberLayout

instance IsNSScrubberLayout (Id NSScrubberLayout) where
  toNSScrubberLayout = unsafeCastId

instance IsNSObject (Id NSScrubberLayout) where
  toNSObject = unsafeCastId

-- ---------- NSScrubberLayoutAttributes ----------

-- | NSScrubberLayoutAttributes
--
-- @NSScrubberLayoutAttributes@ describes the layout of a single @NSScrubber@ item.
--
-- @NSScrubberLayout@ objects transact in terms of @NSScrubberLayoutAttributes.@ @NSScrubberLayoutAttributes@ can be subclassed if a layout object wants to include more layout information than the base implementation provides. Subclasses of @NSScrubberLayoutAttributes@ must implement @isEqual:,@ @hash,@ and the @NSCopying@ protocol.
-- 
-- Phantom type for @NSScrubberLayoutAttributes@.
data NSScrubberLayoutAttributes

instance IsObjCObject (Id NSScrubberLayoutAttributes) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSScrubberLayoutAttributes"

class IsNSObject a => IsNSScrubberLayoutAttributes a where
  toNSScrubberLayoutAttributes :: a -> Id NSScrubberLayoutAttributes

instance IsNSScrubberLayoutAttributes (Id NSScrubberLayoutAttributes) where
  toNSScrubberLayoutAttributes = unsafeCastId

instance IsNSObject (Id NSScrubberLayoutAttributes) where
  toNSObject = unsafeCastId

-- ---------- NSScrubberSelectionStyle ----------

-- | NSScrubberSelectionStyle
--
-- @NSScrubberSelectionStyle@ is an abstract class that provides decorative accessory views for selected and highlighted items within a NSScrubber control. Class properties provide convenient access to built-in styles. For a completely custom style, subclassers can override @-makeSelectionView@ to create and configure arbitrary @NSScrubberSelectionView@ subclasses.
-- 
-- Phantom type for @NSScrubberSelectionStyle@.
data NSScrubberSelectionStyle

instance IsObjCObject (Id NSScrubberSelectionStyle) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSScrubberSelectionStyle"

class IsNSObject a => IsNSScrubberSelectionStyle a where
  toNSScrubberSelectionStyle :: a -> Id NSScrubberSelectionStyle

instance IsNSScrubberSelectionStyle (Id NSScrubberSelectionStyle) where
  toNSScrubberSelectionStyle = unsafeCastId

instance IsNSObject (Id NSScrubberSelectionStyle) where
  toNSObject = unsafeCastId

-- ---------- NSShadow ----------

-- | Phantom type for @NSShadow@.
data NSShadow

instance IsObjCObject (Id NSShadow) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSShadow"

class IsNSObject a => IsNSShadow a where
  toNSShadow :: a -> Id NSShadow

instance IsNSShadow (Id NSShadow) where
  toNSShadow = unsafeCastId

instance IsNSObject (Id NSShadow) where
  toNSObject = unsafeCastId

-- ---------- NSSharingCollaborationModeRestriction ----------

-- | Specifies whether a specific type of sharing should be disabled in the share picker, and if so, whether a reason should be provided for the disablement If a reason is provided, the corresponding mode will show up as an option, but an alert explaining why it is disabled will show if it is chosen, and the mode will switch back to the supported one Optionally, an extra alert button can be provided for a "recovery suggestion". This can give a user a way to fix whatever is causing this type of sharing to be disabled If no reason is provided, the corresponding mode will not show up as an option
-- 
-- Phantom type for @NSSharingCollaborationModeRestriction@.
data NSSharingCollaborationModeRestriction

instance IsObjCObject (Id NSSharingCollaborationModeRestriction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSharingCollaborationModeRestriction"

class IsNSObject a => IsNSSharingCollaborationModeRestriction a where
  toNSSharingCollaborationModeRestriction :: a -> Id NSSharingCollaborationModeRestriction

instance IsNSSharingCollaborationModeRestriction (Id NSSharingCollaborationModeRestriction) where
  toNSSharingCollaborationModeRestriction = unsafeCastId

instance IsNSObject (Id NSSharingCollaborationModeRestriction) where
  toNSObject = unsafeCastId

-- ---------- NSSharingService ----------

-- | NSSharingService can be used to share items to different kinds of local and remote services. Items are objects which respond to the NSPasteboardWriting protocol, like NSURL, NSImage or NSString. If an NSURL is a file URL (point to a video for example), then the content of the file will be shared. If the URL is remote, then the URL itself will be shared.
-- 
-- Phantom type for @NSSharingService@.
data NSSharingService

instance IsObjCObject (Id NSSharingService) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSharingService"

class IsNSObject a => IsNSSharingService a where
  toNSSharingService :: a -> Id NSSharingService

instance IsNSSharingService (Id NSSharingService) where
  toNSSharingService = unsafeCastId

instance IsNSObject (Id NSSharingService) where
  toNSObject = unsafeCastId

-- ---------- NSSharingServicePicker ----------

-- | Phantom type for @NSSharingServicePicker@.
data NSSharingServicePicker

instance IsObjCObject (Id NSSharingServicePicker) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSharingServicePicker"

class IsNSObject a => IsNSSharingServicePicker a where
  toNSSharingServicePicker :: a -> Id NSSharingServicePicker

instance IsNSSharingServicePicker (Id NSSharingServicePicker) where
  toNSSharingServicePicker = unsafeCastId

instance IsNSObject (Id NSSharingServicePicker) where
  toNSObject = unsafeCastId

-- ---------- NSSliderAccessory ----------

-- | Phantom type for @NSSliderAccessory@.
data NSSliderAccessory

instance IsObjCObject (Id NSSliderAccessory) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSliderAccessory"

class IsNSObject a => IsNSSliderAccessory a where
  toNSSliderAccessory :: a -> Id NSSliderAccessory

instance IsNSSliderAccessory (Id NSSliderAccessory) where
  toNSSliderAccessory = unsafeCastId

instance IsNSObject (Id NSSliderAccessory) where
  toNSObject = unsafeCastId

-- ---------- NSSliderAccessoryBehavior ----------

-- | Phantom type for @NSSliderAccessoryBehavior@.
data NSSliderAccessoryBehavior

instance IsObjCObject (Id NSSliderAccessoryBehavior) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSliderAccessoryBehavior"

class IsNSObject a => IsNSSliderAccessoryBehavior a where
  toNSSliderAccessoryBehavior :: a -> Id NSSliderAccessoryBehavior

instance IsNSSliderAccessoryBehavior (Id NSSliderAccessoryBehavior) where
  toNSSliderAccessoryBehavior = unsafeCastId

instance IsNSObject (Id NSSliderAccessoryBehavior) where
  toNSObject = unsafeCastId

-- ---------- NSSound ----------

-- | Phantom type for @NSSound@.
data NSSound

instance IsObjCObject (Id NSSound) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSound"

class IsNSObject a => IsNSSound a where
  toNSSound :: a -> Id NSSound

instance IsNSSound (Id NSSound) where
  toNSSound = unsafeCastId

instance IsNSObject (Id NSSound) where
  toNSObject = unsafeCastId

-- ---------- NSSpeechRecognizer ----------

-- | Phantom type for @NSSpeechRecognizer@.
data NSSpeechRecognizer

instance IsObjCObject (Id NSSpeechRecognizer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSpeechRecognizer"

class IsNSObject a => IsNSSpeechRecognizer a where
  toNSSpeechRecognizer :: a -> Id NSSpeechRecognizer

instance IsNSSpeechRecognizer (Id NSSpeechRecognizer) where
  toNSSpeechRecognizer = unsafeCastId

instance IsNSObject (Id NSSpeechRecognizer) where
  toNSObject = unsafeCastId

-- ---------- NSSpeechSynthesizer ----------

-- | Phantom type for @NSSpeechSynthesizer@.
data NSSpeechSynthesizer

instance IsObjCObject (Id NSSpeechSynthesizer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSpeechSynthesizer"

class IsNSObject a => IsNSSpeechSynthesizer a where
  toNSSpeechSynthesizer :: a -> Id NSSpeechSynthesizer

instance IsNSSpeechSynthesizer (Id NSSpeechSynthesizer) where
  toNSSpeechSynthesizer = unsafeCastId

instance IsNSObject (Id NSSpeechSynthesizer) where
  toNSObject = unsafeCastId

-- ---------- NSSpellChecker ----------

-- | Phantom type for @NSSpellChecker@.
data NSSpellChecker

instance IsObjCObject (Id NSSpellChecker) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSpellChecker"

class IsNSObject a => IsNSSpellChecker a where
  toNSSpellChecker :: a -> Id NSSpellChecker

instance IsNSSpellChecker (Id NSSpellChecker) where
  toNSSpellChecker = unsafeCastId

instance IsNSObject (Id NSSpellChecker) where
  toNSObject = unsafeCastId

-- ---------- NSSplitViewItem ----------

-- | NSSplitViewItem implements the items used in an NSSplitViewController. The item describes a child ViewController's state in a SplitViewController, e.g. its collapsibility, holding priority and other metrics, and collapsed state.
-- 
-- Phantom type for @NSSplitViewItem@.
data NSSplitViewItem

instance IsObjCObject (Id NSSplitViewItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSplitViewItem"

class IsNSObject a => IsNSSplitViewItem a where
  toNSSplitViewItem :: a -> Id NSSplitViewItem

instance IsNSSplitViewItem (Id NSSplitViewItem) where
  toNSSplitViewItem = unsafeCastId

instance IsNSObject (Id NSSplitViewItem) where
  toNSObject = unsafeCastId

-- ---------- NSStatusBar ----------

-- | Phantom type for @NSStatusBar@.
data NSStatusBar

instance IsObjCObject (Id NSStatusBar) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSStatusBar"

class IsNSObject a => IsNSStatusBar a where
  toNSStatusBar :: a -> Id NSStatusBar

instance IsNSStatusBar (Id NSStatusBar) where
  toNSStatusBar = unsafeCastId

instance IsNSObject (Id NSStatusBar) where
  toNSObject = unsafeCastId

-- ---------- NSStatusItem ----------

-- | Phantom type for @NSStatusItem@.
data NSStatusItem

instance IsObjCObject (Id NSStatusItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSStatusItem"

class IsNSObject a => IsNSStatusItem a where
  toNSStatusItem :: a -> Id NSStatusItem

instance IsNSStatusItem (Id NSStatusItem) where
  toNSStatusItem = unsafeCastId

instance IsNSObject (Id NSStatusItem) where
  toNSObject = unsafeCastId

-- ---------- NSStoryboard ----------

-- | Phantom type for @NSStoryboard@.
data NSStoryboard

instance IsObjCObject (Id NSStoryboard) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSStoryboard"

class IsNSObject a => IsNSStoryboard a where
  toNSStoryboard :: a -> Id NSStoryboard

instance IsNSStoryboard (Id NSStoryboard) where
  toNSStoryboard = unsafeCastId

instance IsNSObject (Id NSStoryboard) where
  toNSObject = unsafeCastId

-- ---------- NSStoryboardSegue ----------

-- | Phantom type for @NSStoryboardSegue@.
data NSStoryboardSegue

instance IsObjCObject (Id NSStoryboardSegue) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSStoryboardSegue"

class IsNSObject a => IsNSStoryboardSegue a where
  toNSStoryboardSegue :: a -> Id NSStoryboardSegue

instance IsNSStoryboardSegue (Id NSStoryboardSegue) where
  toNSStoryboardSegue = unsafeCastId

instance IsNSObject (Id NSStoryboardSegue) where
  toNSObject = unsafeCastId

-- ---------- NSStringDrawingContext ----------

-- | Phantom type for @NSStringDrawingContext@.
data NSStringDrawingContext

instance IsObjCObject (Id NSStringDrawingContext) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSStringDrawingContext"

class IsNSObject a => IsNSStringDrawingContext a where
  toNSStringDrawingContext :: a -> Id NSStringDrawingContext

instance IsNSStringDrawingContext (Id NSStringDrawingContext) where
  toNSStringDrawingContext = unsafeCastId

instance IsNSObject (Id NSStringDrawingContext) where
  toNSObject = unsafeCastId

-- ---------- NSSymbolContentTransition ----------

-- | An abstract base class for transitions that can be applied to both NSImageViews and UIImageViews that have symbol-based images.
--
-- Don't use this class directly, instead use any of the concrete subclasses.
-- 
-- Phantom type for @NSSymbolContentTransition@.
data NSSymbolContentTransition

instance IsObjCObject (Id NSSymbolContentTransition) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSymbolContentTransition"

class IsNSObject a => IsNSSymbolContentTransition a where
  toNSSymbolContentTransition :: a -> Id NSSymbolContentTransition

instance IsNSSymbolContentTransition (Id NSSymbolContentTransition) where
  toNSSymbolContentTransition = unsafeCastId

instance IsNSObject (Id NSSymbolContentTransition) where
  toNSObject = unsafeCastId

-- ---------- NSSymbolEffect ----------

-- | An abstract base class for effects that can be applied to both NSImageViews and UIImageViews that have symbol-based images.
--
-- Don't use this class directly, instead use any of the concrete subclasses.
-- 
-- Phantom type for @NSSymbolEffect@.
data NSSymbolEffect

instance IsObjCObject (Id NSSymbolEffect) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSymbolEffect"

class IsNSObject a => IsNSSymbolEffect a where
  toNSSymbolEffect :: a -> Id NSSymbolEffect

instance IsNSSymbolEffect (Id NSSymbolEffect) where
  toNSSymbolEffect = unsafeCastId

instance IsNSObject (Id NSSymbolEffect) where
  toNSObject = unsafeCastId

-- ---------- NSSymbolEffectOptions ----------

-- | Options configuring how symbol effects apply to symbol views.
-- 
-- Phantom type for @NSSymbolEffectOptions@.
data NSSymbolEffectOptions

instance IsObjCObject (Id NSSymbolEffectOptions) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSymbolEffectOptions"

class IsNSObject a => IsNSSymbolEffectOptions a where
  toNSSymbolEffectOptions :: a -> Id NSSymbolEffectOptions

instance IsNSSymbolEffectOptions (Id NSSymbolEffectOptions) where
  toNSSymbolEffectOptions = unsafeCastId

instance IsNSObject (Id NSSymbolEffectOptions) where
  toNSObject = unsafeCastId

-- ---------- NSTabViewItem ----------

-- | Phantom type for @NSTabViewItem@.
data NSTabViewItem

instance IsObjCObject (Id NSTabViewItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTabViewItem"

class IsNSObject a => IsNSTabViewItem a where
  toNSTabViewItem :: a -> Id NSTabViewItem

instance IsNSTabViewItem (Id NSTabViewItem) where
  toNSTabViewItem = unsafeCastId

instance IsNSObject (Id NSTabViewItem) where
  toNSObject = unsafeCastId

-- ---------- NSTableColumn ----------

-- | Phantom type for @NSTableColumn@.
data NSTableColumn

instance IsObjCObject (Id NSTableColumn) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTableColumn"

class IsNSObject a => IsNSTableColumn a where
  toNSTableColumn :: a -> Id NSTableColumn

instance IsNSTableColumn (Id NSTableColumn) where
  toNSTableColumn = unsafeCastId

instance IsNSObject (Id NSTableColumn) where
  toNSObject = unsafeCastId

-- ---------- NSTableViewDiffableDataSource ----------

-- | Phantom type for @NSTableViewDiffableDataSource@.
data NSTableViewDiffableDataSource

instance IsObjCObject (Id NSTableViewDiffableDataSource) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTableViewDiffableDataSource"

class IsNSObject a => IsNSTableViewDiffableDataSource a where
  toNSTableViewDiffableDataSource :: a -> Id NSTableViewDiffableDataSource

instance IsNSTableViewDiffableDataSource (Id NSTableViewDiffableDataSource) where
  toNSTableViewDiffableDataSource = unsafeCastId

instance IsNSObject (Id NSTableViewDiffableDataSource) where
  toNSObject = unsafeCastId

-- ---------- NSTableViewRowAction ----------

-- | Phantom type for @NSTableViewRowAction@.
data NSTableViewRowAction

instance IsObjCObject (Id NSTableViewRowAction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTableViewRowAction"

class IsNSObject a => IsNSTableViewRowAction a where
  toNSTableViewRowAction :: a -> Id NSTableViewRowAction

instance IsNSTableViewRowAction (Id NSTableViewRowAction) where
  toNSTableViewRowAction = unsafeCastId

instance IsNSObject (Id NSTableViewRowAction) where
  toNSObject = unsafeCastId

-- ---------- NSTextAlternatives ----------

-- | Phantom type for @NSTextAlternatives@.
data NSTextAlternatives

instance IsObjCObject (Id NSTextAlternatives) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTextAlternatives"

class IsNSObject a => IsNSTextAlternatives a where
  toNSTextAlternatives :: a -> Id NSTextAlternatives

instance IsNSTextAlternatives (Id NSTextAlternatives) where
  toNSTextAlternatives = unsafeCastId

instance IsNSObject (Id NSTextAlternatives) where
  toNSObject = unsafeCastId

-- ---------- NSTextAttachment ----------

-- | Phantom type for @NSTextAttachment@.
data NSTextAttachment

instance IsObjCObject (Id NSTextAttachment) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTextAttachment"

class IsNSObject a => IsNSTextAttachment a where
  toNSTextAttachment :: a -> Id NSTextAttachment

instance IsNSTextAttachment (Id NSTextAttachment) where
  toNSTextAttachment = unsafeCastId

instance IsNSObject (Id NSTextAttachment) where
  toNSObject = unsafeCastId

-- ---------- NSTextAttachmentViewProvider ----------

-- | Phantom type for @NSTextAttachmentViewProvider@.
data NSTextAttachmentViewProvider

instance IsObjCObject (Id NSTextAttachmentViewProvider) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTextAttachmentViewProvider"

class IsNSObject a => IsNSTextAttachmentViewProvider a where
  toNSTextAttachmentViewProvider :: a -> Id NSTextAttachmentViewProvider

instance IsNSTextAttachmentViewProvider (Id NSTextAttachmentViewProvider) where
  toNSTextAttachmentViewProvider = unsafeCastId

instance IsNSObject (Id NSTextAttachmentViewProvider) where
  toNSObject = unsafeCastId

-- ---------- NSTextBlock ----------

-- | Phantom type for @NSTextBlock@.
data NSTextBlock

instance IsObjCObject (Id NSTextBlock) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTextBlock"

class IsNSObject a => IsNSTextBlock a where
  toNSTextBlock :: a -> Id NSTextBlock

instance IsNSTextBlock (Id NSTextBlock) where
  toNSTextBlock = unsafeCastId

instance IsNSObject (Id NSTextBlock) where
  toNSObject = unsafeCastId

-- ---------- NSTextCheckingController ----------

-- | Phantom type for @NSTextCheckingController@.
data NSTextCheckingController

instance IsObjCObject (Id NSTextCheckingController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTextCheckingController"

class IsNSObject a => IsNSTextCheckingController a where
  toNSTextCheckingController :: a -> Id NSTextCheckingController

instance IsNSTextCheckingController (Id NSTextCheckingController) where
  toNSTextCheckingController = unsafeCastId

instance IsNSObject (Id NSTextCheckingController) where
  toNSObject = unsafeCastId

-- ---------- NSTextContainer ----------

-- | Phantom type for @NSTextContainer@.
data NSTextContainer

instance IsObjCObject (Id NSTextContainer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTextContainer"

class IsNSObject a => IsNSTextContainer a where
  toNSTextContainer :: a -> Id NSTextContainer

instance IsNSTextContainer (Id NSTextContainer) where
  toNSTextContainer = unsafeCastId

instance IsNSObject (Id NSTextContainer) where
  toNSObject = unsafeCastId

-- ---------- NSTextContentManager ----------

-- | Phantom type for @NSTextContentManager@.
data NSTextContentManager

instance IsObjCObject (Id NSTextContentManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTextContentManager"

class IsNSObject a => IsNSTextContentManager a where
  toNSTextContentManager :: a -> Id NSTextContentManager

instance IsNSTextContentManager (Id NSTextContentManager) where
  toNSTextContentManager = unsafeCastId

instance IsNSObject (Id NSTextContentManager) where
  toNSObject = unsafeCastId

-- ---------- NSTextElement ----------

-- | Phantom type for @NSTextElement@.
data NSTextElement

instance IsObjCObject (Id NSTextElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTextElement"

class IsNSObject a => IsNSTextElement a where
  toNSTextElement :: a -> Id NSTextElement

instance IsNSTextElement (Id NSTextElement) where
  toNSTextElement = unsafeCastId

instance IsNSObject (Id NSTextElement) where
  toNSObject = unsafeCastId

-- ---------- NSTextFinder ----------

-- | Phantom type for @NSTextFinder@.
data NSTextFinder

instance IsObjCObject (Id NSTextFinder) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTextFinder"

class IsNSObject a => IsNSTextFinder a where
  toNSTextFinder :: a -> Id NSTextFinder

instance IsNSTextFinder (Id NSTextFinder) where
  toNSTextFinder = unsafeCastId

instance IsNSObject (Id NSTextFinder) where
  toNSObject = unsafeCastId

-- ---------- NSTextInputContext ----------

-- | Phantom type for @NSTextInputContext@.
data NSTextInputContext

instance IsObjCObject (Id NSTextInputContext) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTextInputContext"

class IsNSObject a => IsNSTextInputContext a where
  toNSTextInputContext :: a -> Id NSTextInputContext

instance IsNSTextInputContext (Id NSTextInputContext) where
  toNSTextInputContext = unsafeCastId

instance IsNSObject (Id NSTextInputContext) where
  toNSObject = unsafeCastId

-- ---------- NSTextLayoutFragment ----------

-- | Phantom type for @NSTextLayoutFragment@.
data NSTextLayoutFragment

instance IsObjCObject (Id NSTextLayoutFragment) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTextLayoutFragment"

class IsNSObject a => IsNSTextLayoutFragment a where
  toNSTextLayoutFragment :: a -> Id NSTextLayoutFragment

instance IsNSTextLayoutFragment (Id NSTextLayoutFragment) where
  toNSTextLayoutFragment = unsafeCastId

instance IsNSObject (Id NSTextLayoutFragment) where
  toNSObject = unsafeCastId

-- ---------- NSTextLayoutManager ----------

-- | Phantom type for @NSTextLayoutManager@.
data NSTextLayoutManager

instance IsObjCObject (Id NSTextLayoutManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTextLayoutManager"

class IsNSObject a => IsNSTextLayoutManager a where
  toNSTextLayoutManager :: a -> Id NSTextLayoutManager

instance IsNSTextLayoutManager (Id NSTextLayoutManager) where
  toNSTextLayoutManager = unsafeCastId

instance IsNSObject (Id NSTextLayoutManager) where
  toNSObject = unsafeCastId

-- ---------- NSTextLineFragment ----------

-- | Phantom type for @NSTextLineFragment@.
data NSTextLineFragment

instance IsObjCObject (Id NSTextLineFragment) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTextLineFragment"

class IsNSObject a => IsNSTextLineFragment a where
  toNSTextLineFragment :: a -> Id NSTextLineFragment

instance IsNSTextLineFragment (Id NSTextLineFragment) where
  toNSTextLineFragment = unsafeCastId

instance IsNSObject (Id NSTextLineFragment) where
  toNSObject = unsafeCastId

-- ---------- NSTextList ----------

-- | Phantom type for @NSTextList@.
data NSTextList

instance IsObjCObject (Id NSTextList) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTextList"

class IsNSObject a => IsNSTextList a where
  toNSTextList :: a -> Id NSTextList

instance IsNSTextList (Id NSTextList) where
  toNSTextList = unsafeCastId

instance IsNSObject (Id NSTextList) where
  toNSObject = unsafeCastId

-- ---------- NSTextPreview ----------

-- | A snapshot of the text in your view, which the system uses to create user-visible effects.
--
-- An @NSTextPreview@ object provides a static image of your view’s text content that the system can use to create animations. You provide preview objects in response to system requests, such as ones from Writing Tools. In addition to creating an image of your view’s text, you also specify the location of that text in your view’s frame rectangle. When creating animations, the system places the image on top of your view’s content and animates changes to the image instead of to your view.
--
-- Create an @NSTextPreview@ object in response to specific system requests. Create an image with a transparent background and render your view’s text into the image using the current text attributes. Construct your @NSTextPreview@ object with both the image and the frame rectangle that represents the location of the rendered text in your view’s coordinate system. To highlight specific portions of text, instead of all the text in the image, provide a set of candidate rectangles with the locations of the text you want to highlight.
-- 
-- Phantom type for @NSTextPreview@.
data NSTextPreview

instance IsObjCObject (Id NSTextPreview) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTextPreview"

class IsNSObject a => IsNSTextPreview a where
  toNSTextPreview :: a -> Id NSTextPreview

instance IsNSTextPreview (Id NSTextPreview) where
  toNSTextPreview = unsafeCastId

instance IsNSObject (Id NSTextPreview) where
  toNSObject = unsafeCastId

-- ---------- NSTextRange ----------

-- | A class that represents a contiguous range between two locations inside document contents.
--
-- An @NSTextRange@ consists of the starting and terminating locations. There the two basic properties: ``location`` and ``endLocation``, respectively. The terminating ``location``, ``endLocation``, is directly following the last location in the range. For example, a location contains a range if `(range.location <= location) && (location < range.endLocation)@ is @true`.
-- 
-- Phantom type for @NSTextRange@.
data NSTextRange

instance IsObjCObject (Id NSTextRange) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTextRange"

class IsNSObject a => IsNSTextRange a where
  toNSTextRange :: a -> Id NSTextRange

instance IsNSTextRange (Id NSTextRange) where
  toNSTextRange = unsafeCastId

instance IsNSObject (Id NSTextRange) where
  toNSObject = unsafeCastId

-- ---------- NSTextSelection ----------

-- | Phantom type for @NSTextSelection@.
data NSTextSelection

instance IsObjCObject (Id NSTextSelection) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTextSelection"

class IsNSObject a => IsNSTextSelection a where
  toNSTextSelection :: a -> Id NSTextSelection

instance IsNSTextSelection (Id NSTextSelection) where
  toNSTextSelection = unsafeCastId

instance IsNSObject (Id NSTextSelection) where
  toNSObject = unsafeCastId

-- ---------- NSTextSelectionNavigation ----------

-- | Phantom type for @NSTextSelectionNavigation@.
data NSTextSelectionNavigation

instance IsObjCObject (Id NSTextSelectionNavigation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTextSelectionNavigation"

class IsNSObject a => IsNSTextSelectionNavigation a where
  toNSTextSelectionNavigation :: a -> Id NSTextSelectionNavigation

instance IsNSTextSelectionNavigation (Id NSTextSelectionNavigation) where
  toNSTextSelectionNavigation = unsafeCastId

instance IsNSObject (Id NSTextSelectionNavigation) where
  toNSObject = unsafeCastId

-- ---------- NSTextTab ----------

-- | Phantom type for @NSTextTab@.
data NSTextTab

instance IsObjCObject (Id NSTextTab) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTextTab"

class IsNSObject a => IsNSTextTab a where
  toNSTextTab :: a -> Id NSTextTab

instance IsNSTextTab (Id NSTextTab) where
  toNSTextTab = unsafeCastId

instance IsNSObject (Id NSTextTab) where
  toNSObject = unsafeCastId

-- ---------- NSTextViewportLayoutController ----------

-- | Phantom type for @NSTextViewportLayoutController@.
data NSTextViewportLayoutController

instance IsObjCObject (Id NSTextViewportLayoutController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTextViewportLayoutController"

class IsNSObject a => IsNSTextViewportLayoutController a where
  toNSTextViewportLayoutController :: a -> Id NSTextViewportLayoutController

instance IsNSTextViewportLayoutController (Id NSTextViewportLayoutController) where
  toNSTextViewportLayoutController = unsafeCastId

instance IsNSObject (Id NSTextViewportLayoutController) where
  toNSObject = unsafeCastId

-- ---------- NSTintConfiguration ----------

-- | Phantom type for @NSTintConfiguration@.
data NSTintConfiguration

instance IsObjCObject (Id NSTintConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTintConfiguration"

class IsNSObject a => IsNSTintConfiguration a where
  toNSTintConfiguration :: a -> Id NSTintConfiguration

instance IsNSTintConfiguration (Id NSTintConfiguration) where
  toNSTintConfiguration = unsafeCastId

instance IsNSObject (Id NSTintConfiguration) where
  toNSObject = unsafeCastId

-- ---------- NSToolbar ----------

-- | Phantom type for @NSToolbar@.
data NSToolbar

instance IsObjCObject (Id NSToolbar) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSToolbar"

class IsNSObject a => IsNSToolbar a where
  toNSToolbar :: a -> Id NSToolbar

instance IsNSToolbar (Id NSToolbar) where
  toNSToolbar = unsafeCastId

instance IsNSObject (Id NSToolbar) where
  toNSObject = unsafeCastId

-- ---------- NSToolbarItem ----------

-- | Phantom type for @NSToolbarItem@.
data NSToolbarItem

instance IsObjCObject (Id NSToolbarItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSToolbarItem"

class IsNSObject a => IsNSToolbarItem a where
  toNSToolbarItem :: a -> Id NSToolbarItem

instance IsNSToolbarItem (Id NSToolbarItem) where
  toNSToolbarItem = unsafeCastId

instance IsNSObject (Id NSToolbarItem) where
  toNSObject = unsafeCastId

-- ---------- NSTouch ----------

-- | Phantom type for @NSTouch@.
data NSTouch

instance IsObjCObject (Id NSTouch) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTouch"

class IsNSObject a => IsNSTouch a where
  toNSTouch :: a -> Id NSTouch

instance IsNSTouch (Id NSTouch) where
  toNSTouch = unsafeCastId

instance IsNSObject (Id NSTouch) where
  toNSObject = unsafeCastId

-- ---------- NSTouchBar ----------

-- | Phantom type for @NSTouchBar@.
data NSTouchBar

instance IsObjCObject (Id NSTouchBar) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTouchBar"

class IsNSObject a => IsNSTouchBar a where
  toNSTouchBar :: a -> Id NSTouchBar

instance IsNSTouchBar (Id NSTouchBar) where
  toNSTouchBar = unsafeCastId

instance IsNSObject (Id NSTouchBar) where
  toNSObject = unsafeCastId

-- ---------- NSTouchBarItem ----------

-- | Phantom type for @NSTouchBarItem@.
data NSTouchBarItem

instance IsObjCObject (Id NSTouchBarItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTouchBarItem"

class IsNSObject a => IsNSTouchBarItem a where
  toNSTouchBarItem :: a -> Id NSTouchBarItem

instance IsNSTouchBarItem (Id NSTouchBarItem) where
  toNSTouchBarItem = unsafeCastId

instance IsNSObject (Id NSTouchBarItem) where
  toNSObject = unsafeCastId

-- ---------- NSTrackingArea ----------

-- | Phantom type for @NSTrackingArea@.
data NSTrackingArea

instance IsObjCObject (Id NSTrackingArea) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTrackingArea"

class IsNSObject a => IsNSTrackingArea a where
  toNSTrackingArea :: a -> Id NSTrackingArea

instance IsNSTrackingArea (Id NSTrackingArea) where
  toNSTrackingArea = unsafeCastId

instance IsNSObject (Id NSTrackingArea) where
  toNSObject = unsafeCastId

-- ---------- NSTreeNode ----------

-- | Phantom type for @NSTreeNode@.
data NSTreeNode

instance IsObjCObject (Id NSTreeNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTreeNode"

class IsNSObject a => IsNSTreeNode a where
  toNSTreeNode :: a -> Id NSTreeNode

instance IsNSTreeNode (Id NSTreeNode) where
  toNSTreeNode = unsafeCastId

instance IsNSObject (Id NSTreeNode) where
  toNSObject = unsafeCastId

-- ---------- NSTypesetter ----------

-- | Phantom type for @NSTypesetter@.
data NSTypesetter

instance IsObjCObject (Id NSTypesetter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTypesetter"

class IsNSObject a => IsNSTypesetter a where
  toNSTypesetter :: a -> Id NSTypesetter

instance IsNSTypesetter (Id NSTypesetter) where
  toNSTypesetter = unsafeCastId

instance IsNSObject (Id NSTypesetter) where
  toNSObject = unsafeCastId

-- ---------- NSUserInterfaceCompressionOptions ----------

-- | Phantom type for @NSUserInterfaceCompressionOptions@.
data NSUserInterfaceCompressionOptions

instance IsObjCObject (Id NSUserInterfaceCompressionOptions) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSUserInterfaceCompressionOptions"

class IsNSObject a => IsNSUserInterfaceCompressionOptions a where
  toNSUserInterfaceCompressionOptions :: a -> Id NSUserInterfaceCompressionOptions

instance IsNSUserInterfaceCompressionOptions (Id NSUserInterfaceCompressionOptions) where
  toNSUserInterfaceCompressionOptions = unsafeCastId

instance IsNSObject (Id NSUserInterfaceCompressionOptions) where
  toNSObject = unsafeCastId

-- ---------- NSViewLayoutRegion ----------

-- | Phantom type for @NSViewLayoutRegion@.
data NSViewLayoutRegion

instance IsObjCObject (Id NSViewLayoutRegion) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSViewLayoutRegion"

class IsNSObject a => IsNSViewLayoutRegion a where
  toNSViewLayoutRegion :: a -> Id NSViewLayoutRegion

instance IsNSViewLayoutRegion (Id NSViewLayoutRegion) where
  toNSViewLayoutRegion = unsafeCastId

instance IsNSObject (Id NSViewLayoutRegion) where
  toNSObject = unsafeCastId

-- ---------- NSWindowTab ----------

-- | Phantom type for @NSWindowTab@.
data NSWindowTab

instance IsObjCObject (Id NSWindowTab) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSWindowTab"

class IsNSObject a => IsNSWindowTab a where
  toNSWindowTab :: a -> Id NSWindowTab

instance IsNSWindowTab (Id NSWindowTab) where
  toNSWindowTab = unsafeCastId

instance IsNSObject (Id NSWindowTab) where
  toNSObject = unsafeCastId

-- ---------- NSWindowTabGroup ----------

-- | Phantom type for @NSWindowTabGroup@.
data NSWindowTabGroup

instance IsObjCObject (Id NSWindowTabGroup) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSWindowTabGroup"

class IsNSObject a => IsNSWindowTabGroup a where
  toNSWindowTabGroup :: a -> Id NSWindowTabGroup

instance IsNSWindowTabGroup (Id NSWindowTabGroup) where
  toNSWindowTabGroup = unsafeCastId

instance IsNSObject (Id NSWindowTabGroup) where
  toNSObject = unsafeCastId

-- ---------- NSWorkspace ----------

-- | Phantom type for @NSWorkspace@.
data NSWorkspace

instance IsObjCObject (Id NSWorkspace) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSWorkspace"

class IsNSObject a => IsNSWorkspace a where
  toNSWorkspace :: a -> Id NSWorkspace

instance IsNSWorkspace (Id NSWorkspace) where
  toNSWorkspace = unsafeCastId

instance IsNSObject (Id NSWorkspace) where
  toNSObject = unsafeCastId

-- ---------- NSWorkspaceAuthorization ----------

-- | Phantom type for @NSWorkspaceAuthorization@.
data NSWorkspaceAuthorization

instance IsObjCObject (Id NSWorkspaceAuthorization) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSWorkspaceAuthorization"

class IsNSObject a => IsNSWorkspaceAuthorization a where
  toNSWorkspaceAuthorization :: a -> Id NSWorkspaceAuthorization

instance IsNSWorkspaceAuthorization (Id NSWorkspaceAuthorization) where
  toNSWorkspaceAuthorization = unsafeCastId

instance IsNSObject (Id NSWorkspaceAuthorization) where
  toNSObject = unsafeCastId

-- ---------- NSWorkspaceOpenConfiguration ----------

-- | Phantom type for @NSWorkspaceOpenConfiguration@.
data NSWorkspaceOpenConfiguration

instance IsObjCObject (Id NSWorkspaceOpenConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSWorkspaceOpenConfiguration"

class IsNSObject a => IsNSWorkspaceOpenConfiguration a where
  toNSWorkspaceOpenConfiguration :: a -> Id NSWorkspaceOpenConfiguration

instance IsNSWorkspaceOpenConfiguration (Id NSWorkspaceOpenConfiguration) where
  toNSWorkspaceOpenConfiguration = unsafeCastId

instance IsNSObject (Id NSWorkspaceOpenConfiguration) where
  toNSObject = unsafeCastId

-- ---------- NSWritingToolsCoordinator ----------

-- | An object that manages interactions between Writing Tools and your custom text view.
--
-- Add a @NSWritingToolsCoordinator@ object to a custom view when you want to add Writing Tools support to that view. The coordinator manages interactions between your view and the Writing Tools UI and back-end capabilities. When creating a coordinator, you supply a delegate object to respond to requests from the system and provide needed information. Your delegate delivers your view’s text to Writing Tools, incorporates suggested changes back into your text storage, and supports the animations that Writing Tools creates to show the state of an operation.
--
-- Create the @NSWritingToolsCoordinator@ object when setting up your UI, and initialize it with a custom object that adopts the ``NSWritingToolsCoordinator/Delegate`` protocol. Add the coordinator to the ``NSView/writingToolsCoordinator`` property of your view. When a coordinator is present on a view, the system adds UI elements to initiate Writing Tools operations.
--
-- When defining the delegate, choose an object from your app that has access to your view and its text storage. You can adopt the ``NSWritingToolsCoordinator/Delegate`` protocol in the view itself, or in another type that your view uses to manage content. During the interactions with Writing Tools, the delegate gets and sets the contents of the view’s text storage and supports Writing Tools behaviors.
--
-- > Note: You don’t need to create an @NSWritingToolsCoordinator@  object if you display text using a <doc://com.apple.documentation/documentation/uikit/uitextview>, ``NSTextField``, ``NSTextView``, <doc://com.apple.documentation/documentation/swiftui/textfield>, or <doc://com.apple.documentation/documentation/swiftui/texteditor> view. Those views already include the required support to handle Writing Tools interactions.
-- 
-- Phantom type for @NSWritingToolsCoordinator@.
data NSWritingToolsCoordinator

instance IsObjCObject (Id NSWritingToolsCoordinator) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSWritingToolsCoordinator"

class IsNSObject a => IsNSWritingToolsCoordinator a where
  toNSWritingToolsCoordinator :: a -> Id NSWritingToolsCoordinator

instance IsNSWritingToolsCoordinator (Id NSWritingToolsCoordinator) where
  toNSWritingToolsCoordinator = unsafeCastId

instance IsNSObject (Id NSWritingToolsCoordinator) where
  toNSObject = unsafeCastId

-- ---------- NSWritingToolsCoordinatorAnimationParameters ----------

-- | An object you use to configure additional tasks or animations to run alongside the Writing Tools animations.
--
-- When Writing Tools replaces text in one of your context objects, it provides an @NSWritingToolsCoordinator.AnimationParameters@ object for you to use to configure any additional animations. During a Writing Tools session, you hide the text under evaluation and provide a targeted preview of your content. Writing Tools animations changes to that preview, but you might need to provide additional animations for other parts of your view’s content. For example, you might need to animate any layout changes caused by the insertion or removal of text in other parts of your view. Use this object to configure those animations.
--
-- You don’t create an @NSWritingToolsCoordinator.AnimationParameters@ object directly. Instead, the system creates one and passes it to the``NSWritingToolsCoordinator/writingToolsCoordinator(_:replaceRange:inContext:proposedText:reason:animationParameters:completion:)``method of your ``NSWritingToolsCoordinator/Delegate`` object. Use thatobject to specify the blocks to run during and after the system animations.
-- 
-- Phantom type for @NSWritingToolsCoordinatorAnimationParameters@.
data NSWritingToolsCoordinatorAnimationParameters

instance IsObjCObject (Id NSWritingToolsCoordinatorAnimationParameters) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSWritingToolsCoordinatorAnimationParameters"

class IsNSObject a => IsNSWritingToolsCoordinatorAnimationParameters a where
  toNSWritingToolsCoordinatorAnimationParameters :: a -> Id NSWritingToolsCoordinatorAnimationParameters

instance IsNSWritingToolsCoordinatorAnimationParameters (Id NSWritingToolsCoordinatorAnimationParameters) where
  toNSWritingToolsCoordinatorAnimationParameters = unsafeCastId

instance IsNSObject (Id NSWritingToolsCoordinatorAnimationParameters) where
  toNSObject = unsafeCastId

-- ---------- NSWritingToolsCoordinatorContext ----------

-- | A data object that you use to share your custom view’s text with Writing Tools.
--
-- At the start of every Writing Tools operation, you create one or more @NSWritingToolsCoordinator.Context@ objects with a copy of the text you want Writing Tools to evaluate. Each Writing Tools operation starts with a call to the ``NSWritingToolsCoordinator/Delegate/writingToolsCoordinator(_:requestsContextsFor:completion:)`` method of your ``NSWritingToolsCoordinator/Delegate`` object. Use the parameters of that method to determine how much of your view’s text to provide. For some operations, Writing Tools asks for all of your view’s text, but in others it asks for only a portion of the text. When Writing Tools finishes its evaluation, it reports changes back to your delegate relative to the context objects you provided.
--
-- When Writing Tools asks for your view’s text, create one or more @NSWritingToolsCoordinator.Context@ objects with the requested content. If your view contains only one text storage object, create only one context object for the request. However, if you use multiple text storage objects to manage different parts of your view’s content, you might need to create multiple context objects. The actual number depends on how much of your text Writing Tools asks for. For example, when Writing Tools asks for all of your view’s content, you return one context object for each text storage object in your view. However, if Writing Tools asks for the current selection, and one text storage object contains all of the selected text, you create only one context object for the content.
--
-- Writing Tools uses your context objects as the starting point for its evaluations, and as a reference point for any changes. Because Writing Tools doesn’t know anything about your view or its content, it makes suggestions only relative to your context objects. It’s your responsibility to take those suggestions and incorporate them back into your view’s text storage. In some cases, you might need to store additional information to update your storage correctly. For example, you might need to store, and update as needed, the offset from the start of your document to the start of the text in your context object.
--
-- When Writing Tools asks for the currently selected text in your view, include some of the surrounding text in your context object as well. Supply a string that includes the selection and any text up to the nearest paragraph boundary. When creating your context object, specify a range value that represents the portion of that string that corresponds to the text selection. Providing some additional text in your context object can help Writing Tools improve its evaluation of your content. Writing Tools uses the ``resolvedRange`` property of your context object to indicate what text it considered.
--
-- If your context object includes text that you don’t want Writing Tools to evaluate, add the @excludeFromWritingTools@ attribute to the corresponding characters of your <doc://com.apple.documentation/documentation/foundation/nsattributedstring> object. You might add this attribute if the text string includes a code listing or readonly content that you don’t want Writing Tools to change.
-- 
-- Phantom type for @NSWritingToolsCoordinatorContext@.
data NSWritingToolsCoordinatorContext

instance IsObjCObject (Id NSWritingToolsCoordinatorContext) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSWritingToolsCoordinatorContext"

class IsNSObject a => IsNSWritingToolsCoordinatorContext a where
  toNSWritingToolsCoordinatorContext :: a -> Id NSWritingToolsCoordinatorContext

instance IsNSWritingToolsCoordinatorContext (Id NSWritingToolsCoordinatorContext) where
  toNSWritingToolsCoordinatorContext = unsafeCastId

instance IsNSObject (Id NSWritingToolsCoordinatorContext) where
  toNSObject = unsafeCastId

-- ---------- NSViewAnimation ----------

-- | Phantom type for @NSViewAnimation@.
data NSViewAnimation

instance IsObjCObject (Id NSViewAnimation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSViewAnimation"

class IsNSAnimation a => IsNSViewAnimation a where
  toNSViewAnimation :: a -> Id NSViewAnimation

instance IsNSViewAnimation (Id NSViewAnimation) where
  toNSViewAnimation = unsafeCastId

instance IsNSAnimation (Id NSViewAnimation) where
  toNSAnimation = unsafeCastId

instance IsNSObject (Id NSViewAnimation) where
  toNSObject = unsafeCastId

-- ---------- NSActionCell ----------

-- | Phantom type for @NSActionCell@.
data NSActionCell

instance IsObjCObject (Id NSActionCell) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSActionCell"

class IsNSCell a => IsNSActionCell a where
  toNSActionCell :: a -> Id NSActionCell

instance IsNSActionCell (Id NSActionCell) where
  toNSActionCell = unsafeCastId

instance IsNSCell (Id NSActionCell) where
  toNSCell = unsafeCastId

instance IsNSObject (Id NSActionCell) where
  toNSObject = unsafeCastId

-- ---------- NSBrowserCell ----------

-- | Phantom type for @NSBrowserCell@.
data NSBrowserCell

instance IsObjCObject (Id NSBrowserCell) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSBrowserCell"

class IsNSCell a => IsNSBrowserCell a where
  toNSBrowserCell :: a -> Id NSBrowserCell

instance IsNSBrowserCell (Id NSBrowserCell) where
  toNSBrowserCell = unsafeCastId

instance IsNSCell (Id NSBrowserCell) where
  toNSCell = unsafeCastId

instance IsNSObject (Id NSBrowserCell) where
  toNSObject = unsafeCastId

-- ---------- NSImageCell ----------

-- | Phantom type for @NSImageCell@.
data NSImageCell

instance IsObjCObject (Id NSImageCell) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSImageCell"

class IsNSCell a => IsNSImageCell a where
  toNSImageCell :: a -> Id NSImageCell

instance IsNSImageCell (Id NSImageCell) where
  toNSImageCell = unsafeCastId

instance IsNSCell (Id NSImageCell) where
  toNSCell = unsafeCastId

instance IsNSObject (Id NSImageCell) where
  toNSObject = unsafeCastId

-- ---------- NSTextAttachmentCell ----------

-- | Phantom type for @NSTextAttachmentCell@.
data NSTextAttachmentCell

instance IsObjCObject (Id NSTextAttachmentCell) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTextAttachmentCell"

class IsNSCell a => IsNSTextAttachmentCell a where
  toNSTextAttachmentCell :: a -> Id NSTextAttachmentCell

instance IsNSTextAttachmentCell (Id NSTextAttachmentCell) where
  toNSTextAttachmentCell = unsafeCastId

instance IsNSCell (Id NSTextAttachmentCell) where
  toNSCell = unsafeCastId

instance IsNSObject (Id NSTextAttachmentCell) where
  toNSObject = unsafeCastId

-- ---------- NSCollectionLayoutDecorationItem ----------

-- | Phantom type for @NSCollectionLayoutDecorationItem@.
data NSCollectionLayoutDecorationItem

instance IsObjCObject (Id NSCollectionLayoutDecorationItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSCollectionLayoutDecorationItem"

class IsNSCollectionLayoutItem a => IsNSCollectionLayoutDecorationItem a where
  toNSCollectionLayoutDecorationItem :: a -> Id NSCollectionLayoutDecorationItem

instance IsNSCollectionLayoutDecorationItem (Id NSCollectionLayoutDecorationItem) where
  toNSCollectionLayoutDecorationItem = unsafeCastId

instance IsNSCollectionLayoutItem (Id NSCollectionLayoutDecorationItem) where
  toNSCollectionLayoutItem = unsafeCastId

instance IsNSObject (Id NSCollectionLayoutDecorationItem) where
  toNSObject = unsafeCastId

-- ---------- NSCollectionLayoutGroup ----------

-- | Phantom type for @NSCollectionLayoutGroup@.
data NSCollectionLayoutGroup

instance IsObjCObject (Id NSCollectionLayoutGroup) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSCollectionLayoutGroup"

class IsNSCollectionLayoutItem a => IsNSCollectionLayoutGroup a where
  toNSCollectionLayoutGroup :: a -> Id NSCollectionLayoutGroup

instance IsNSCollectionLayoutGroup (Id NSCollectionLayoutGroup) where
  toNSCollectionLayoutGroup = unsafeCastId

instance IsNSCollectionLayoutItem (Id NSCollectionLayoutGroup) where
  toNSCollectionLayoutItem = unsafeCastId

instance IsNSObject (Id NSCollectionLayoutGroup) where
  toNSObject = unsafeCastId

-- ---------- NSCollectionLayoutSupplementaryItem ----------

-- | Phantom type for @NSCollectionLayoutSupplementaryItem@.
data NSCollectionLayoutSupplementaryItem

instance IsObjCObject (Id NSCollectionLayoutSupplementaryItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSCollectionLayoutSupplementaryItem"

class IsNSCollectionLayoutItem a => IsNSCollectionLayoutSupplementaryItem a where
  toNSCollectionLayoutSupplementaryItem :: a -> Id NSCollectionLayoutSupplementaryItem

instance IsNSCollectionLayoutSupplementaryItem (Id NSCollectionLayoutSupplementaryItem) where
  toNSCollectionLayoutSupplementaryItem = unsafeCastId

instance IsNSCollectionLayoutItem (Id NSCollectionLayoutSupplementaryItem) where
  toNSCollectionLayoutItem = unsafeCastId

instance IsNSObject (Id NSCollectionLayoutSupplementaryItem) where
  toNSObject = unsafeCastId

-- ---------- NSCollectionViewCompositionalLayout ----------

-- | Phantom type for @NSCollectionViewCompositionalLayout@.
data NSCollectionViewCompositionalLayout

instance IsObjCObject (Id NSCollectionViewCompositionalLayout) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSCollectionViewCompositionalLayout"

class IsNSCollectionViewLayout a => IsNSCollectionViewCompositionalLayout a where
  toNSCollectionViewCompositionalLayout :: a -> Id NSCollectionViewCompositionalLayout

instance IsNSCollectionViewCompositionalLayout (Id NSCollectionViewCompositionalLayout) where
  toNSCollectionViewCompositionalLayout = unsafeCastId

instance IsNSCollectionViewLayout (Id NSCollectionViewCompositionalLayout) where
  toNSCollectionViewLayout = unsafeCastId

instance IsNSObject (Id NSCollectionViewCompositionalLayout) where
  toNSObject = unsafeCastId

-- ---------- NSCollectionViewFlowLayout ----------

-- | Phantom type for @NSCollectionViewFlowLayout@.
data NSCollectionViewFlowLayout

instance IsObjCObject (Id NSCollectionViewFlowLayout) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSCollectionViewFlowLayout"

class IsNSCollectionViewLayout a => IsNSCollectionViewFlowLayout a where
  toNSCollectionViewFlowLayout :: a -> Id NSCollectionViewFlowLayout

instance IsNSCollectionViewFlowLayout (Id NSCollectionViewFlowLayout) where
  toNSCollectionViewFlowLayout = unsafeCastId

instance IsNSCollectionViewLayout (Id NSCollectionViewFlowLayout) where
  toNSCollectionViewLayout = unsafeCastId

instance IsNSObject (Id NSCollectionViewFlowLayout) where
  toNSObject = unsafeCastId

-- ---------- NSCollectionViewGridLayout ----------

-- | Phantom type for @NSCollectionViewGridLayout@.
data NSCollectionViewGridLayout

instance IsObjCObject (Id NSCollectionViewGridLayout) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSCollectionViewGridLayout"

class IsNSCollectionViewLayout a => IsNSCollectionViewGridLayout a where
  toNSCollectionViewGridLayout :: a -> Id NSCollectionViewGridLayout

instance IsNSCollectionViewGridLayout (Id NSCollectionViewGridLayout) where
  toNSCollectionViewGridLayout = unsafeCastId

instance IsNSCollectionViewLayout (Id NSCollectionViewGridLayout) where
  toNSCollectionViewLayout = unsafeCastId

instance IsNSObject (Id NSCollectionViewGridLayout) where
  toNSObject = unsafeCastId

-- ---------- NSCollectionViewTransitionLayout ----------

-- | Phantom type for @NSCollectionViewTransitionLayout@.
data NSCollectionViewTransitionLayout

instance IsObjCObject (Id NSCollectionViewTransitionLayout) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSCollectionViewTransitionLayout"

class IsNSCollectionViewLayout a => IsNSCollectionViewTransitionLayout a where
  toNSCollectionViewTransitionLayout :: a -> Id NSCollectionViewTransitionLayout

instance IsNSCollectionViewTransitionLayout (Id NSCollectionViewTransitionLayout) where
  toNSCollectionViewTransitionLayout = unsafeCastId

instance IsNSCollectionViewLayout (Id NSCollectionViewTransitionLayout) where
  toNSCollectionViewLayout = unsafeCastId

instance IsNSObject (Id NSCollectionViewTransitionLayout) where
  toNSObject = unsafeCastId

-- ---------- NSCollectionViewFlowLayoutInvalidationContext ----------

-- | Phantom type for @NSCollectionViewFlowLayoutInvalidationContext@.
data NSCollectionViewFlowLayoutInvalidationContext

instance IsObjCObject (Id NSCollectionViewFlowLayoutInvalidationContext) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSCollectionViewFlowLayoutInvalidationContext"

class IsNSCollectionViewLayoutInvalidationContext a => IsNSCollectionViewFlowLayoutInvalidationContext a where
  toNSCollectionViewFlowLayoutInvalidationContext :: a -> Id NSCollectionViewFlowLayoutInvalidationContext

instance IsNSCollectionViewFlowLayoutInvalidationContext (Id NSCollectionViewFlowLayoutInvalidationContext) where
  toNSCollectionViewFlowLayoutInvalidationContext = unsafeCastId

instance IsNSCollectionViewLayoutInvalidationContext (Id NSCollectionViewFlowLayoutInvalidationContext) where
  toNSCollectionViewLayoutInvalidationContext = unsafeCastId

instance IsNSObject (Id NSCollectionViewFlowLayoutInvalidationContext) where
  toNSObject = unsafeCastId

-- ---------- NSObjectController ----------

-- | Phantom type for @NSObjectController@.
data NSObjectController

instance IsObjCObject (Id NSObjectController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSObjectController"

class IsNSController a => IsNSObjectController a where
  toNSObjectController :: a -> Id NSObjectController

instance IsNSObjectController (Id NSObjectController) where
  toNSObjectController = unsafeCastId

instance IsNSController (Id NSObjectController) where
  toNSController = unsafeCastId

instance IsNSObject (Id NSObjectController) where
  toNSObject = unsafeCastId

-- ---------- NSUserDefaultsController ----------

-- | Phantom type for @NSUserDefaultsController@.
data NSUserDefaultsController

instance IsObjCObject (Id NSUserDefaultsController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSUserDefaultsController"

class IsNSController a => IsNSUserDefaultsController a where
  toNSUserDefaultsController :: a -> Id NSUserDefaultsController

instance IsNSUserDefaultsController (Id NSUserDefaultsController) where
  toNSUserDefaultsController = unsafeCastId

instance IsNSController (Id NSUserDefaultsController) where
  toNSController = unsafeCastId

instance IsNSObject (Id NSUserDefaultsController) where
  toNSObject = unsafeCastId

-- ---------- NSPersistentDocument ----------

-- | Phantom type for @NSPersistentDocument@.
data NSPersistentDocument

instance IsObjCObject (Id NSPersistentDocument) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPersistentDocument"

class IsNSDocument a => IsNSPersistentDocument a where
  toNSPersistentDocument :: a -> Id NSPersistentDocument

instance IsNSPersistentDocument (Id NSPersistentDocument) where
  toNSPersistentDocument = unsafeCastId

instance IsNSDocument (Id NSPersistentDocument) where
  toNSDocument = unsafeCastId

instance IsNSObject (Id NSPersistentDocument) where
  toNSObject = unsafeCastId

-- ---------- NSMutableFontCollection ----------

-- | Phantom type for @NSMutableFontCollection@.
data NSMutableFontCollection

instance IsObjCObject (Id NSMutableFontCollection) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSMutableFontCollection"

class IsNSFontCollection a => IsNSMutableFontCollection a where
  toNSMutableFontCollection :: a -> Id NSMutableFontCollection

instance IsNSMutableFontCollection (Id NSMutableFontCollection) where
  toNSMutableFontCollection = unsafeCastId

instance IsNSFontCollection (Id NSMutableFontCollection) where
  toNSFontCollection = unsafeCastId

instance IsNSObject (Id NSMutableFontCollection) where
  toNSObject = unsafeCastId

-- ---------- NSClickGestureRecognizer ----------

-- | Phantom type for @NSClickGestureRecognizer@.
data NSClickGestureRecognizer

instance IsObjCObject (Id NSClickGestureRecognizer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSClickGestureRecognizer"

class IsNSGestureRecognizer a => IsNSClickGestureRecognizer a where
  toNSClickGestureRecognizer :: a -> Id NSClickGestureRecognizer

instance IsNSClickGestureRecognizer (Id NSClickGestureRecognizer) where
  toNSClickGestureRecognizer = unsafeCastId

instance IsNSGestureRecognizer (Id NSClickGestureRecognizer) where
  toNSGestureRecognizer = unsafeCastId

instance IsNSObject (Id NSClickGestureRecognizer) where
  toNSObject = unsafeCastId

-- ---------- NSMagnificationGestureRecognizer ----------

-- | Phantom type for @NSMagnificationGestureRecognizer@.
data NSMagnificationGestureRecognizer

instance IsObjCObject (Id NSMagnificationGestureRecognizer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSMagnificationGestureRecognizer"

class IsNSGestureRecognizer a => IsNSMagnificationGestureRecognizer a where
  toNSMagnificationGestureRecognizer :: a -> Id NSMagnificationGestureRecognizer

instance IsNSMagnificationGestureRecognizer (Id NSMagnificationGestureRecognizer) where
  toNSMagnificationGestureRecognizer = unsafeCastId

instance IsNSGestureRecognizer (Id NSMagnificationGestureRecognizer) where
  toNSGestureRecognizer = unsafeCastId

instance IsNSObject (Id NSMagnificationGestureRecognizer) where
  toNSObject = unsafeCastId

-- ---------- NSPanGestureRecognizer ----------

-- | Phantom type for @NSPanGestureRecognizer@.
data NSPanGestureRecognizer

instance IsObjCObject (Id NSPanGestureRecognizer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPanGestureRecognizer"

class IsNSGestureRecognizer a => IsNSPanGestureRecognizer a where
  toNSPanGestureRecognizer :: a -> Id NSPanGestureRecognizer

instance IsNSPanGestureRecognizer (Id NSPanGestureRecognizer) where
  toNSPanGestureRecognizer = unsafeCastId

instance IsNSGestureRecognizer (Id NSPanGestureRecognizer) where
  toNSGestureRecognizer = unsafeCastId

instance IsNSObject (Id NSPanGestureRecognizer) where
  toNSObject = unsafeCastId

-- ---------- NSPressGestureRecognizer ----------

-- | Phantom type for @NSPressGestureRecognizer@.
data NSPressGestureRecognizer

instance IsObjCObject (Id NSPressGestureRecognizer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPressGestureRecognizer"

class IsNSGestureRecognizer a => IsNSPressGestureRecognizer a where
  toNSPressGestureRecognizer :: a -> Id NSPressGestureRecognizer

instance IsNSPressGestureRecognizer (Id NSPressGestureRecognizer) where
  toNSPressGestureRecognizer = unsafeCastId

instance IsNSGestureRecognizer (Id NSPressGestureRecognizer) where
  toNSGestureRecognizer = unsafeCastId

instance IsNSObject (Id NSPressGestureRecognizer) where
  toNSObject = unsafeCastId

-- ---------- NSRotationGestureRecognizer ----------

-- | Phantom type for @NSRotationGestureRecognizer@.
data NSRotationGestureRecognizer

instance IsObjCObject (Id NSRotationGestureRecognizer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSRotationGestureRecognizer"

class IsNSGestureRecognizer a => IsNSRotationGestureRecognizer a where
  toNSRotationGestureRecognizer :: a -> Id NSRotationGestureRecognizer

instance IsNSRotationGestureRecognizer (Id NSRotationGestureRecognizer) where
  toNSRotationGestureRecognizer = unsafeCastId

instance IsNSGestureRecognizer (Id NSRotationGestureRecognizer) where
  toNSGestureRecognizer = unsafeCastId

instance IsNSObject (Id NSRotationGestureRecognizer) where
  toNSObject = unsafeCastId

-- ---------- NSBitmapImageRep ----------

-- | Phantom type for @NSBitmapImageRep@.
data NSBitmapImageRep

instance IsObjCObject (Id NSBitmapImageRep) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSBitmapImageRep"

class IsNSImageRep a => IsNSBitmapImageRep a where
  toNSBitmapImageRep :: a -> Id NSBitmapImageRep

instance IsNSBitmapImageRep (Id NSBitmapImageRep) where
  toNSBitmapImageRep = unsafeCastId

instance IsNSImageRep (Id NSBitmapImageRep) where
  toNSImageRep = unsafeCastId

instance IsNSObject (Id NSBitmapImageRep) where
  toNSObject = unsafeCastId

-- ---------- NSCIImageRep ----------

-- | Phantom type for @NSCIImageRep@.
data NSCIImageRep

instance IsObjCObject (Id NSCIImageRep) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSCIImageRep"

class IsNSImageRep a => IsNSCIImageRep a where
  toNSCIImageRep :: a -> Id NSCIImageRep

instance IsNSCIImageRep (Id NSCIImageRep) where
  toNSCIImageRep = unsafeCastId

instance IsNSImageRep (Id NSCIImageRep) where
  toNSImageRep = unsafeCastId

instance IsNSObject (Id NSCIImageRep) where
  toNSObject = unsafeCastId

-- ---------- NSCachedImageRep ----------

-- | Phantom type for @NSCachedImageRep@.
data NSCachedImageRep

instance IsObjCObject (Id NSCachedImageRep) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSCachedImageRep"

class IsNSImageRep a => IsNSCachedImageRep a where
  toNSCachedImageRep :: a -> Id NSCachedImageRep

instance IsNSCachedImageRep (Id NSCachedImageRep) where
  toNSCachedImageRep = unsafeCastId

instance IsNSImageRep (Id NSCachedImageRep) where
  toNSImageRep = unsafeCastId

instance IsNSObject (Id NSCachedImageRep) where
  toNSObject = unsafeCastId

-- ---------- NSCustomImageRep ----------

-- | Phantom type for @NSCustomImageRep@.
data NSCustomImageRep

instance IsObjCObject (Id NSCustomImageRep) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSCustomImageRep"

class IsNSImageRep a => IsNSCustomImageRep a where
  toNSCustomImageRep :: a -> Id NSCustomImageRep

instance IsNSCustomImageRep (Id NSCustomImageRep) where
  toNSCustomImageRep = unsafeCastId

instance IsNSImageRep (Id NSCustomImageRep) where
  toNSImageRep = unsafeCastId

instance IsNSObject (Id NSCustomImageRep) where
  toNSObject = unsafeCastId

-- ---------- NSEPSImageRep ----------

-- | An object that can render an image from encapsulated PostScript (EPS) code.
-- 
-- Phantom type for @NSEPSImageRep@.
data NSEPSImageRep

instance IsObjCObject (Id NSEPSImageRep) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSEPSImageRep"

class IsNSImageRep a => IsNSEPSImageRep a where
  toNSEPSImageRep :: a -> Id NSEPSImageRep

instance IsNSEPSImageRep (Id NSEPSImageRep) where
  toNSEPSImageRep = unsafeCastId

instance IsNSImageRep (Id NSEPSImageRep) where
  toNSImageRep = unsafeCastId

instance IsNSObject (Id NSEPSImageRep) where
  toNSObject = unsafeCastId

-- ---------- NSPDFImageRep ----------

-- | Phantom type for @NSPDFImageRep@.
data NSPDFImageRep

instance IsObjCObject (Id NSPDFImageRep) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPDFImageRep"

class IsNSImageRep a => IsNSPDFImageRep a where
  toNSPDFImageRep :: a -> Id NSPDFImageRep

instance IsNSPDFImageRep (Id NSPDFImageRep) where
  toNSPDFImageRep = unsafeCastId

instance IsNSImageRep (Id NSPDFImageRep) where
  toNSImageRep = unsafeCastId

instance IsNSObject (Id NSPDFImageRep) where
  toNSObject = unsafeCastId

-- ---------- NSPICTImageRep ----------

-- | Phantom type for @NSPICTImageRep@.
data NSPICTImageRep

instance IsObjCObject (Id NSPICTImageRep) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPICTImageRep"

class IsNSImageRep a => IsNSPICTImageRep a where
  toNSPICTImageRep :: a -> Id NSPICTImageRep

instance IsNSPICTImageRep (Id NSPICTImageRep) where
  toNSPICTImageRep = unsafeCastId

instance IsNSImageRep (Id NSPICTImageRep) where
  toNSImageRep = unsafeCastId

instance IsNSObject (Id NSPICTImageRep) where
  toNSObject = unsafeCastId

-- ---------- NSLayoutDimension ----------

-- | Phantom type for @NSLayoutDimension@.
data NSLayoutDimension

instance IsObjCObject (Id NSLayoutDimension) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSLayoutDimension"

class IsNSLayoutAnchor a => IsNSLayoutDimension a where
  toNSLayoutDimension :: a -> Id NSLayoutDimension

instance IsNSLayoutDimension (Id NSLayoutDimension) where
  toNSLayoutDimension = unsafeCastId

instance IsNSLayoutAnchor (Id NSLayoutDimension) where
  toNSLayoutAnchor = unsafeCastId

instance IsNSObject (Id NSLayoutDimension) where
  toNSObject = unsafeCastId

-- ---------- NSLayoutXAxisAnchor ----------

-- | Phantom type for @NSLayoutXAxisAnchor@.
data NSLayoutXAxisAnchor

instance IsObjCObject (Id NSLayoutXAxisAnchor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSLayoutXAxisAnchor"

class IsNSLayoutAnchor a => IsNSLayoutXAxisAnchor a where
  toNSLayoutXAxisAnchor :: a -> Id NSLayoutXAxisAnchor

instance IsNSLayoutXAxisAnchor (Id NSLayoutXAxisAnchor) where
  toNSLayoutXAxisAnchor = unsafeCastId

instance IsNSLayoutAnchor (Id NSLayoutXAxisAnchor) where
  toNSLayoutAnchor = unsafeCastId

instance IsNSObject (Id NSLayoutXAxisAnchor) where
  toNSObject = unsafeCastId

-- ---------- NSLayoutYAxisAnchor ----------

-- | Phantom type for @NSLayoutYAxisAnchor@.
data NSLayoutYAxisAnchor

instance IsObjCObject (Id NSLayoutYAxisAnchor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSLayoutYAxisAnchor"

class IsNSLayoutAnchor a => IsNSLayoutYAxisAnchor a where
  toNSLayoutYAxisAnchor :: a -> Id NSLayoutYAxisAnchor

instance IsNSLayoutYAxisAnchor (Id NSLayoutYAxisAnchor) where
  toNSLayoutYAxisAnchor = unsafeCastId

instance IsNSLayoutAnchor (Id NSLayoutYAxisAnchor) where
  toNSLayoutAnchor = unsafeCastId

instance IsNSObject (Id NSLayoutYAxisAnchor) where
  toNSObject = unsafeCastId

-- ---------- NSMutableParagraphStyle ----------

-- | Phantom type for @NSMutableParagraphStyle@.
data NSMutableParagraphStyle

instance IsObjCObject (Id NSMutableParagraphStyle) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSMutableParagraphStyle"

class IsNSParagraphStyle a => IsNSMutableParagraphStyle a where
  toNSMutableParagraphStyle :: a -> Id NSMutableParagraphStyle

instance IsNSMutableParagraphStyle (Id NSMutableParagraphStyle) where
  toNSMutableParagraphStyle = unsafeCastId

instance IsNSObject (Id NSMutableParagraphStyle) where
  toNSObject = unsafeCastId

instance IsNSParagraphStyle (Id NSMutableParagraphStyle) where
  toNSParagraphStyle = unsafeCastId

-- ---------- NSAttributeDescription ----------

-- | Phantom type for @NSAttributeDescription@.
data NSAttributeDescription

instance IsObjCObject (Id NSAttributeDescription) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSAttributeDescription"

class IsNSPropertyDescription a => IsNSAttributeDescription a where
  toNSAttributeDescription :: a -> Id NSAttributeDescription

instance IsNSAttributeDescription (Id NSAttributeDescription) where
  toNSAttributeDescription = unsafeCastId

instance IsNSObject (Id NSAttributeDescription) where
  toNSObject = unsafeCastId

instance IsNSPropertyDescription (Id NSAttributeDescription) where
  toNSPropertyDescription = unsafeCastId

-- ---------- NSApplication ----------

-- | Phantom type for @NSApplication@.
data NSApplication

instance IsObjCObject (Id NSApplication) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSApplication"

class IsNSResponder a => IsNSApplication a where
  toNSApplication :: a -> Id NSApplication

instance IsNSApplication (Id NSApplication) where
  toNSApplication = unsafeCastId

instance IsNSObject (Id NSApplication) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSApplication) where
  toNSResponder = unsafeCastId

-- ---------- NSDrawer ----------

-- | Phantom type for @NSDrawer@.
data NSDrawer

instance IsObjCObject (Id NSDrawer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSDrawer"

class IsNSResponder a => IsNSDrawer a where
  toNSDrawer :: a -> Id NSDrawer

instance IsNSDrawer (Id NSDrawer) where
  toNSDrawer = unsafeCastId

instance IsNSObject (Id NSDrawer) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSDrawer) where
  toNSResponder = unsafeCastId

-- ---------- NSPopover ----------

-- | Phantom type for @NSPopover@.
data NSPopover

instance IsObjCObject (Id NSPopover) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPopover"

class IsNSResponder a => IsNSPopover a where
  toNSPopover :: a -> Id NSPopover

instance IsNSPopover (Id NSPopover) where
  toNSPopover = unsafeCastId

instance IsNSObject (Id NSPopover) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSPopover) where
  toNSResponder = unsafeCastId

-- ---------- NSView ----------

-- | Phantom type for @NSView@.
data NSView

instance IsObjCObject (Id NSView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSView"

class IsNSResponder a => IsNSView a where
  toNSView :: a -> Id NSView

instance IsNSView (Id NSView) where
  toNSView = unsafeCastId

instance IsNSObject (Id NSView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSView) where
  toNSResponder = unsafeCastId

-- ---------- NSViewController ----------

-- | Phantom type for @NSViewController@.
data NSViewController

instance IsObjCObject (Id NSViewController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSViewController"

class IsNSResponder a => IsNSViewController a where
  toNSViewController :: a -> Id NSViewController

instance IsNSViewController (Id NSViewController) where
  toNSViewController = unsafeCastId

instance IsNSObject (Id NSViewController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSViewController) where
  toNSResponder = unsafeCastId

-- ---------- NSWindow ----------

-- | Phantom type for @NSWindow@.
data NSWindow

instance IsObjCObject (Id NSWindow) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSWindow"

class IsNSResponder a => IsNSWindow a where
  toNSWindow :: a -> Id NSWindow

instance IsNSWindow (Id NSWindow) where
  toNSWindow = unsafeCastId

instance IsNSObject (Id NSWindow) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSWindow) where
  toNSResponder = unsafeCastId

-- ---------- NSWindowController ----------

-- | Phantom type for @NSWindowController@.
data NSWindowController

instance IsObjCObject (Id NSWindowController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSWindowController"

class IsNSResponder a => IsNSWindowController a where
  toNSWindowController :: a -> Id NSWindowController

instance IsNSWindowController (Id NSWindowController) where
  toNSWindowController = unsafeCastId

instance IsNSObject (Id NSWindowController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSWindowController) where
  toNSResponder = unsafeCastId

-- ---------- NSScrubberFlowLayout ----------

-- | NSScrubberFlowLayout
--
-- @NSScrubberFlowLayout@ is a concrete layout object that arranges items end-to-end in a linear strip. It supports a fixed inter-item spacing and both fixed- and variable-sized items.
--
-- If the associated scrubber's @delegate@ conforms to @NSScrubberFlowLayoutDelegate,@ and it implements the @scrubber:layout:sizeForItemAtIndex:@ method, @NSScrubberFlowLayout@ will obtain the item size from the delegate. If the delegate does not implement that method, or if the method returns @NSZeroSize,@ it will fall back to using the layout's @itemSize@ property. By default, NSScrubberFlowLayout does not invalidate its layout on selection change, highlight change, or visible rectangle change.
-- 
-- Phantom type for @NSScrubberFlowLayout@.
data NSScrubberFlowLayout

instance IsObjCObject (Id NSScrubberFlowLayout) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSScrubberFlowLayout"

class IsNSScrubberLayout a => IsNSScrubberFlowLayout a where
  toNSScrubberFlowLayout :: a -> Id NSScrubberFlowLayout

instance IsNSScrubberFlowLayout (Id NSScrubberFlowLayout) where
  toNSScrubberFlowLayout = unsafeCastId

instance IsNSObject (Id NSScrubberFlowLayout) where
  toNSObject = unsafeCastId

instance IsNSScrubberLayout (Id NSScrubberFlowLayout) where
  toNSScrubberLayout = unsafeCastId

-- ---------- NSScrubberProportionalLayout ----------

-- | NSScrubberProportionalLayout
--
-- @NSScrubberProportionalLayout@ is a concrete layout object that sizes each item to some fraction of the scrubber's visible size.
-- 
-- Phantom type for @NSScrubberProportionalLayout@.
data NSScrubberProportionalLayout

instance IsObjCObject (Id NSScrubberProportionalLayout) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSScrubberProportionalLayout"

class IsNSScrubberLayout a => IsNSScrubberProportionalLayout a where
  toNSScrubberProportionalLayout :: a -> Id NSScrubberProportionalLayout

instance IsNSScrubberProportionalLayout (Id NSScrubberProportionalLayout) where
  toNSScrubberProportionalLayout = unsafeCastId

instance IsNSObject (Id NSScrubberProportionalLayout) where
  toNSObject = unsafeCastId

instance IsNSScrubberLayout (Id NSScrubberProportionalLayout) where
  toNSScrubberLayout = unsafeCastId

-- ---------- NSSymbolAutomaticContentTransition ----------

-- | The default symbol transition, resolves to a particular transition in a context-sensitive manner.
-- 
-- Phantom type for @NSSymbolAutomaticContentTransition@.
data NSSymbolAutomaticContentTransition

instance IsObjCObject (Id NSSymbolAutomaticContentTransition) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSymbolAutomaticContentTransition"

class IsNSSymbolContentTransition a => IsNSSymbolAutomaticContentTransition a where
  toNSSymbolAutomaticContentTransition :: a -> Id NSSymbolAutomaticContentTransition

instance IsNSSymbolAutomaticContentTransition (Id NSSymbolAutomaticContentTransition) where
  toNSSymbolAutomaticContentTransition = unsafeCastId

instance IsNSObject (Id NSSymbolAutomaticContentTransition) where
  toNSObject = unsafeCastId

instance IsNSSymbolContentTransition (Id NSSymbolAutomaticContentTransition) where
  toNSSymbolContentTransition = unsafeCastId

-- ---------- NSSymbolMagicReplaceContentTransition ----------

-- | A symbol effect applies the MagicReplace animation to symbol images.
--
-- The MagicReplace effect animates common elements across symbol images.
-- 
-- Phantom type for @NSSymbolMagicReplaceContentTransition@.
data NSSymbolMagicReplaceContentTransition

instance IsObjCObject (Id NSSymbolMagicReplaceContentTransition) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSymbolMagicReplaceContentTransition"

class IsNSSymbolContentTransition a => IsNSSymbolMagicReplaceContentTransition a where
  toNSSymbolMagicReplaceContentTransition :: a -> Id NSSymbolMagicReplaceContentTransition

instance IsNSSymbolMagicReplaceContentTransition (Id NSSymbolMagicReplaceContentTransition) where
  toNSSymbolMagicReplaceContentTransition = unsafeCastId

instance IsNSObject (Id NSSymbolMagicReplaceContentTransition) where
  toNSObject = unsafeCastId

instance IsNSSymbolContentTransition (Id NSSymbolMagicReplaceContentTransition) where
  toNSSymbolContentTransition = unsafeCastId

-- ---------- NSSymbolReplaceContentTransition ----------

-- | A symbol effect that animates the replacement of one symbol image with another.
-- 
-- Phantom type for @NSSymbolReplaceContentTransition@.
data NSSymbolReplaceContentTransition

instance IsObjCObject (Id NSSymbolReplaceContentTransition) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSymbolReplaceContentTransition"

class IsNSSymbolContentTransition a => IsNSSymbolReplaceContentTransition a where
  toNSSymbolReplaceContentTransition :: a -> Id NSSymbolReplaceContentTransition

instance IsNSSymbolReplaceContentTransition (Id NSSymbolReplaceContentTransition) where
  toNSSymbolReplaceContentTransition = unsafeCastId

instance IsNSObject (Id NSSymbolReplaceContentTransition) where
  toNSObject = unsafeCastId

instance IsNSSymbolContentTransition (Id NSSymbolReplaceContentTransition) where
  toNSSymbolContentTransition = unsafeCastId

-- ---------- NSSymbolAppearEffect ----------

-- | A symbol effect that applies the Appear animation to symbol images.
--
-- The Appear animation makes the symbol visible either as a whole, or one motion group at a time.
-- 
-- Phantom type for @NSSymbolAppearEffect@.
data NSSymbolAppearEffect

instance IsObjCObject (Id NSSymbolAppearEffect) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSymbolAppearEffect"

class IsNSSymbolEffect a => IsNSSymbolAppearEffect a where
  toNSSymbolAppearEffect :: a -> Id NSSymbolAppearEffect

instance IsNSSymbolAppearEffect (Id NSSymbolAppearEffect) where
  toNSSymbolAppearEffect = unsafeCastId

instance IsNSObject (Id NSSymbolAppearEffect) where
  toNSObject = unsafeCastId

instance IsNSSymbolEffect (Id NSSymbolAppearEffect) where
  toNSSymbolEffect = unsafeCastId

-- ---------- NSSymbolBounceEffect ----------

-- | A symbol effect that applies the Bounce animation to symbol images.
--
-- The Bounce animation applies a transitory scaling effect to the symbol.
-- 
-- Phantom type for @NSSymbolBounceEffect@.
data NSSymbolBounceEffect

instance IsObjCObject (Id NSSymbolBounceEffect) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSymbolBounceEffect"

class IsNSSymbolEffect a => IsNSSymbolBounceEffect a where
  toNSSymbolBounceEffect :: a -> Id NSSymbolBounceEffect

instance IsNSSymbolBounceEffect (Id NSSymbolBounceEffect) where
  toNSSymbolBounceEffect = unsafeCastId

instance IsNSObject (Id NSSymbolBounceEffect) where
  toNSObject = unsafeCastId

instance IsNSSymbolEffect (Id NSSymbolBounceEffect) where
  toNSSymbolEffect = unsafeCastId

-- ---------- NSSymbolBreatheEffect ----------

-- | A symbol effect that applies the Breathe animation to symbol images.
--
-- The Breathe animation smoothly scales a symbol up and down.
-- 
-- Phantom type for @NSSymbolBreatheEffect@.
data NSSymbolBreatheEffect

instance IsObjCObject (Id NSSymbolBreatheEffect) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSymbolBreatheEffect"

class IsNSSymbolEffect a => IsNSSymbolBreatheEffect a where
  toNSSymbolBreatheEffect :: a -> Id NSSymbolBreatheEffect

instance IsNSSymbolBreatheEffect (Id NSSymbolBreatheEffect) where
  toNSSymbolBreatheEffect = unsafeCastId

instance IsNSObject (Id NSSymbolBreatheEffect) where
  toNSObject = unsafeCastId

instance IsNSSymbolEffect (Id NSSymbolBreatheEffect) where
  toNSSymbolEffect = unsafeCastId

-- ---------- NSSymbolDisappearEffect ----------

-- | A symbol effect that applies the Disappear animation to symbol images.
--
-- The Disappear animation makes the symbol visible either as a whole, or one motion group at a time.
-- 
-- Phantom type for @NSSymbolDisappearEffect@.
data NSSymbolDisappearEffect

instance IsObjCObject (Id NSSymbolDisappearEffect) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSymbolDisappearEffect"

class IsNSSymbolEffect a => IsNSSymbolDisappearEffect a where
  toNSSymbolDisappearEffect :: a -> Id NSSymbolDisappearEffect

instance IsNSSymbolDisappearEffect (Id NSSymbolDisappearEffect) where
  toNSSymbolDisappearEffect = unsafeCastId

instance IsNSObject (Id NSSymbolDisappearEffect) where
  toNSObject = unsafeCastId

instance IsNSSymbolEffect (Id NSSymbolDisappearEffect) where
  toNSSymbolEffect = unsafeCastId

-- ---------- NSSymbolDrawOffEffect ----------

-- | A symbol effect that applies the DrawOff animation to symbol images.
--
-- The DrawOff animation makes the symbol hidden either as a whole, or one motion group at a time, animating parts of the symbol with draw data.
-- 
-- Phantom type for @NSSymbolDrawOffEffect@.
data NSSymbolDrawOffEffect

instance IsObjCObject (Id NSSymbolDrawOffEffect) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSymbolDrawOffEffect"

class IsNSSymbolEffect a => IsNSSymbolDrawOffEffect a where
  toNSSymbolDrawOffEffect :: a -> Id NSSymbolDrawOffEffect

instance IsNSSymbolDrawOffEffect (Id NSSymbolDrawOffEffect) where
  toNSSymbolDrawOffEffect = unsafeCastId

instance IsNSObject (Id NSSymbolDrawOffEffect) where
  toNSObject = unsafeCastId

instance IsNSSymbolEffect (Id NSSymbolDrawOffEffect) where
  toNSSymbolEffect = unsafeCastId

-- ---------- NSSymbolDrawOnEffect ----------

-- | A symbol effect that applies the DrawOn animation to symbol images.
--
-- The DrawOn animation makes the symbol visible either as a whole, or one motion group at a time, animating parts of the symbol with draw data.
-- 
-- Phantom type for @NSSymbolDrawOnEffect@.
data NSSymbolDrawOnEffect

instance IsObjCObject (Id NSSymbolDrawOnEffect) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSymbolDrawOnEffect"

class IsNSSymbolEffect a => IsNSSymbolDrawOnEffect a where
  toNSSymbolDrawOnEffect :: a -> Id NSSymbolDrawOnEffect

instance IsNSSymbolDrawOnEffect (Id NSSymbolDrawOnEffect) where
  toNSSymbolDrawOnEffect = unsafeCastId

instance IsNSObject (Id NSSymbolDrawOnEffect) where
  toNSObject = unsafeCastId

instance IsNSSymbolEffect (Id NSSymbolDrawOnEffect) where
  toNSSymbolEffect = unsafeCastId

-- ---------- NSSymbolPulseEffect ----------

-- | A symbol effect that applies the Pulse animation to symbol images.
--
-- The Pulse animation fades the opacity of either all layers in the symbol, or of a subset of the layers in the symbol.
-- 
-- Phantom type for @NSSymbolPulseEffect@.
data NSSymbolPulseEffect

instance IsObjCObject (Id NSSymbolPulseEffect) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSymbolPulseEffect"

class IsNSSymbolEffect a => IsNSSymbolPulseEffect a where
  toNSSymbolPulseEffect :: a -> Id NSSymbolPulseEffect

instance IsNSSymbolPulseEffect (Id NSSymbolPulseEffect) where
  toNSSymbolPulseEffect = unsafeCastId

instance IsNSObject (Id NSSymbolPulseEffect) where
  toNSObject = unsafeCastId

instance IsNSSymbolEffect (Id NSSymbolPulseEffect) where
  toNSSymbolEffect = unsafeCastId

-- ---------- NSSymbolRotateEffect ----------

-- | A symbol effect that applies the Rotate animation to symbol images.
--
-- The Rotate animation rotates parts of a symbol around a symbol-provided anchor point.
-- 
-- Phantom type for @NSSymbolRotateEffect@.
data NSSymbolRotateEffect

instance IsObjCObject (Id NSSymbolRotateEffect) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSymbolRotateEffect"

class IsNSSymbolEffect a => IsNSSymbolRotateEffect a where
  toNSSymbolRotateEffect :: a -> Id NSSymbolRotateEffect

instance IsNSSymbolRotateEffect (Id NSSymbolRotateEffect) where
  toNSSymbolRotateEffect = unsafeCastId

instance IsNSObject (Id NSSymbolRotateEffect) where
  toNSObject = unsafeCastId

instance IsNSSymbolEffect (Id NSSymbolRotateEffect) where
  toNSSymbolEffect = unsafeCastId

-- ---------- NSSymbolScaleEffect ----------

-- | A symbol effect that scales symbol images.
-- 
-- Phantom type for @NSSymbolScaleEffect@.
data NSSymbolScaleEffect

instance IsObjCObject (Id NSSymbolScaleEffect) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSymbolScaleEffect"

class IsNSSymbolEffect a => IsNSSymbolScaleEffect a where
  toNSSymbolScaleEffect :: a -> Id NSSymbolScaleEffect

instance IsNSSymbolScaleEffect (Id NSSymbolScaleEffect) where
  toNSSymbolScaleEffect = unsafeCastId

instance IsNSObject (Id NSSymbolScaleEffect) where
  toNSObject = unsafeCastId

instance IsNSSymbolEffect (Id NSSymbolScaleEffect) where
  toNSSymbolEffect = unsafeCastId

-- ---------- NSSymbolVariableColorEffect ----------

-- | A symbol effect that applies the Variable Color animation to symbol images.
--
-- The Variable Color animation replaces the opacity of variable layers in the symbol by a possibly repeating pattern that moves up and possibly back down the variable layers. It has no effect for non-variable color symbol images.
-- 
-- Phantom type for @NSSymbolVariableColorEffect@.
data NSSymbolVariableColorEffect

instance IsObjCObject (Id NSSymbolVariableColorEffect) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSymbolVariableColorEffect"

class IsNSSymbolEffect a => IsNSSymbolVariableColorEffect a where
  toNSSymbolVariableColorEffect :: a -> Id NSSymbolVariableColorEffect

instance IsNSSymbolVariableColorEffect (Id NSSymbolVariableColorEffect) where
  toNSSymbolVariableColorEffect = unsafeCastId

instance IsNSObject (Id NSSymbolVariableColorEffect) where
  toNSObject = unsafeCastId

instance IsNSSymbolEffect (Id NSSymbolVariableColorEffect) where
  toNSSymbolEffect = unsafeCastId

-- ---------- NSSymbolWiggleEffect ----------

-- | A symbol effect that applies the Wiggle animation to symbol images.
--
-- The Wiggle animation applies a transitory translation or rotation effect to the symbol.
-- 
-- Phantom type for @NSSymbolWiggleEffect@.
data NSSymbolWiggleEffect

instance IsObjCObject (Id NSSymbolWiggleEffect) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSymbolWiggleEffect"

class IsNSSymbolEffect a => IsNSSymbolWiggleEffect a where
  toNSSymbolWiggleEffect :: a -> Id NSSymbolWiggleEffect

instance IsNSSymbolWiggleEffect (Id NSSymbolWiggleEffect) where
  toNSSymbolWiggleEffect = unsafeCastId

instance IsNSObject (Id NSSymbolWiggleEffect) where
  toNSObject = unsafeCastId

instance IsNSSymbolEffect (Id NSSymbolWiggleEffect) where
  toNSSymbolEffect = unsafeCastId

-- ---------- NSTextTable ----------

-- | Phantom type for @NSTextTable@.
data NSTextTable

instance IsObjCObject (Id NSTextTable) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTextTable"

class IsNSTextBlock a => IsNSTextTable a where
  toNSTextTable :: a -> Id NSTextTable

instance IsNSTextTable (Id NSTextTable) where
  toNSTextTable = unsafeCastId

instance IsNSObject (Id NSTextTable) where
  toNSObject = unsafeCastId

instance IsNSTextBlock (Id NSTextTable) where
  toNSTextBlock = unsafeCastId

-- ---------- NSTextTableBlock ----------

-- | Phantom type for @NSTextTableBlock@.
data NSTextTableBlock

instance IsObjCObject (Id NSTextTableBlock) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTextTableBlock"

class IsNSTextBlock a => IsNSTextTableBlock a where
  toNSTextTableBlock :: a -> Id NSTextTableBlock

instance IsNSTextTableBlock (Id NSTextTableBlock) where
  toNSTextTableBlock = unsafeCastId

instance IsNSObject (Id NSTextTableBlock) where
  toNSObject = unsafeCastId

instance IsNSTextBlock (Id NSTextTableBlock) where
  toNSTextBlock = unsafeCastId

-- ---------- NSTextContentStorage ----------

-- | Phantom type for @NSTextContentStorage@.
data NSTextContentStorage

instance IsObjCObject (Id NSTextContentStorage) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTextContentStorage"

class IsNSTextContentManager a => IsNSTextContentStorage a where
  toNSTextContentStorage :: a -> Id NSTextContentStorage

instance IsNSTextContentStorage (Id NSTextContentStorage) where
  toNSTextContentStorage = unsafeCastId

instance IsNSObject (Id NSTextContentStorage) where
  toNSObject = unsafeCastId

instance IsNSTextContentManager (Id NSTextContentStorage) where
  toNSTextContentManager = unsafeCastId

-- ---------- NSTextParagraph ----------

-- | Phantom type for @NSTextParagraph@.
data NSTextParagraph

instance IsObjCObject (Id NSTextParagraph) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTextParagraph"

class IsNSTextElement a => IsNSTextParagraph a where
  toNSTextParagraph :: a -> Id NSTextParagraph

instance IsNSTextParagraph (Id NSTextParagraph) where
  toNSTextParagraph = unsafeCastId

instance IsNSObject (Id NSTextParagraph) where
  toNSObject = unsafeCastId

instance IsNSTextElement (Id NSTextParagraph) where
  toNSTextElement = unsafeCastId

-- ---------- NSMenuToolbarItem ----------

-- | Phantom type for @NSMenuToolbarItem@.
data NSMenuToolbarItem

instance IsObjCObject (Id NSMenuToolbarItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSMenuToolbarItem"

class IsNSToolbarItem a => IsNSMenuToolbarItem a where
  toNSMenuToolbarItem :: a -> Id NSMenuToolbarItem

instance IsNSMenuToolbarItem (Id NSMenuToolbarItem) where
  toNSMenuToolbarItem = unsafeCastId

instance IsNSObject (Id NSMenuToolbarItem) where
  toNSObject = unsafeCastId

instance IsNSToolbarItem (Id NSMenuToolbarItem) where
  toNSToolbarItem = unsafeCastId

-- ---------- NSSearchToolbarItem ----------

-- | @NSSearchToolbarItem@ provides the standard UI behavior for integrating a search field into the toolbar.
-- 
-- Phantom type for @NSSearchToolbarItem@.
data NSSearchToolbarItem

instance IsObjCObject (Id NSSearchToolbarItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSearchToolbarItem"

class IsNSToolbarItem a => IsNSSearchToolbarItem a where
  toNSSearchToolbarItem :: a -> Id NSSearchToolbarItem

instance IsNSSearchToolbarItem (Id NSSearchToolbarItem) where
  toNSSearchToolbarItem = unsafeCastId

instance IsNSObject (Id NSSearchToolbarItem) where
  toNSObject = unsafeCastId

instance IsNSToolbarItem (Id NSSearchToolbarItem) where
  toNSToolbarItem = unsafeCastId

-- ---------- NSSharingServicePickerToolbarItem ----------

-- | Phantom type for @NSSharingServicePickerToolbarItem@.
data NSSharingServicePickerToolbarItem

instance IsObjCObject (Id NSSharingServicePickerToolbarItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSharingServicePickerToolbarItem"

class IsNSToolbarItem a => IsNSSharingServicePickerToolbarItem a where
  toNSSharingServicePickerToolbarItem :: a -> Id NSSharingServicePickerToolbarItem

instance IsNSSharingServicePickerToolbarItem (Id NSSharingServicePickerToolbarItem) where
  toNSSharingServicePickerToolbarItem = unsafeCastId

instance IsNSObject (Id NSSharingServicePickerToolbarItem) where
  toNSObject = unsafeCastId

instance IsNSToolbarItem (Id NSSharingServicePickerToolbarItem) where
  toNSToolbarItem = unsafeCastId

-- ---------- NSToolbarItemGroup ----------

-- | Phantom type for @NSToolbarItemGroup@.
data NSToolbarItemGroup

instance IsObjCObject (Id NSToolbarItemGroup) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSToolbarItemGroup"

class IsNSToolbarItem a => IsNSToolbarItemGroup a where
  toNSToolbarItemGroup :: a -> Id NSToolbarItemGroup

instance IsNSToolbarItemGroup (Id NSToolbarItemGroup) where
  toNSToolbarItemGroup = unsafeCastId

instance IsNSObject (Id NSToolbarItemGroup) where
  toNSObject = unsafeCastId

instance IsNSToolbarItem (Id NSToolbarItemGroup) where
  toNSToolbarItem = unsafeCastId

-- ---------- NSTrackingSeparatorToolbarItem ----------

-- | Phantom type for @NSTrackingSeparatorToolbarItem@.
data NSTrackingSeparatorToolbarItem

instance IsObjCObject (Id NSTrackingSeparatorToolbarItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTrackingSeparatorToolbarItem"

class IsNSToolbarItem a => IsNSTrackingSeparatorToolbarItem a where
  toNSTrackingSeparatorToolbarItem :: a -> Id NSTrackingSeparatorToolbarItem

instance IsNSTrackingSeparatorToolbarItem (Id NSTrackingSeparatorToolbarItem) where
  toNSTrackingSeparatorToolbarItem = unsafeCastId

instance IsNSObject (Id NSTrackingSeparatorToolbarItem) where
  toNSObject = unsafeCastId

instance IsNSToolbarItem (Id NSTrackingSeparatorToolbarItem) where
  toNSToolbarItem = unsafeCastId

-- ---------- NSButtonTouchBarItem ----------

-- | Phantom type for @NSButtonTouchBarItem@.
data NSButtonTouchBarItem

instance IsObjCObject (Id NSButtonTouchBarItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSButtonTouchBarItem"

class IsNSTouchBarItem a => IsNSButtonTouchBarItem a where
  toNSButtonTouchBarItem :: a -> Id NSButtonTouchBarItem

instance IsNSButtonTouchBarItem (Id NSButtonTouchBarItem) where
  toNSButtonTouchBarItem = unsafeCastId

instance IsNSObject (Id NSButtonTouchBarItem) where
  toNSObject = unsafeCastId

instance IsNSTouchBarItem (Id NSButtonTouchBarItem) where
  toNSTouchBarItem = unsafeCastId

-- ---------- NSCandidateListTouchBarItem ----------

-- | Phantom type for @NSCandidateListTouchBarItem@.
data NSCandidateListTouchBarItem

instance IsObjCObject (Id NSCandidateListTouchBarItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSCandidateListTouchBarItem"

class IsNSTouchBarItem a => IsNSCandidateListTouchBarItem a where
  toNSCandidateListTouchBarItem :: a -> Id NSCandidateListTouchBarItem

instance IsNSCandidateListTouchBarItem (Id NSCandidateListTouchBarItem) where
  toNSCandidateListTouchBarItem = unsafeCastId

instance IsNSObject (Id NSCandidateListTouchBarItem) where
  toNSObject = unsafeCastId

instance IsNSTouchBarItem (Id NSCandidateListTouchBarItem) where
  toNSTouchBarItem = unsafeCastId

-- ---------- NSColorPickerTouchBarItem ----------

-- | Phantom type for @NSColorPickerTouchBarItem@.
data NSColorPickerTouchBarItem

instance IsObjCObject (Id NSColorPickerTouchBarItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSColorPickerTouchBarItem"

class IsNSTouchBarItem a => IsNSColorPickerTouchBarItem a where
  toNSColorPickerTouchBarItem :: a -> Id NSColorPickerTouchBarItem

instance IsNSColorPickerTouchBarItem (Id NSColorPickerTouchBarItem) where
  toNSColorPickerTouchBarItem = unsafeCastId

instance IsNSObject (Id NSColorPickerTouchBarItem) where
  toNSObject = unsafeCastId

instance IsNSTouchBarItem (Id NSColorPickerTouchBarItem) where
  toNSTouchBarItem = unsafeCastId

-- ---------- NSCustomTouchBarItem ----------

-- | Phantom type for @NSCustomTouchBarItem@.
data NSCustomTouchBarItem

instance IsObjCObject (Id NSCustomTouchBarItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSCustomTouchBarItem"

class IsNSTouchBarItem a => IsNSCustomTouchBarItem a where
  toNSCustomTouchBarItem :: a -> Id NSCustomTouchBarItem

instance IsNSCustomTouchBarItem (Id NSCustomTouchBarItem) where
  toNSCustomTouchBarItem = unsafeCastId

instance IsNSObject (Id NSCustomTouchBarItem) where
  toNSObject = unsafeCastId

instance IsNSTouchBarItem (Id NSCustomTouchBarItem) where
  toNSTouchBarItem = unsafeCastId

-- ---------- NSGroupTouchBarItem ----------

-- | Phantom type for @NSGroupTouchBarItem@.
data NSGroupTouchBarItem

instance IsObjCObject (Id NSGroupTouchBarItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSGroupTouchBarItem"

class IsNSTouchBarItem a => IsNSGroupTouchBarItem a where
  toNSGroupTouchBarItem :: a -> Id NSGroupTouchBarItem

instance IsNSGroupTouchBarItem (Id NSGroupTouchBarItem) where
  toNSGroupTouchBarItem = unsafeCastId

instance IsNSObject (Id NSGroupTouchBarItem) where
  toNSObject = unsafeCastId

instance IsNSTouchBarItem (Id NSGroupTouchBarItem) where
  toNSTouchBarItem = unsafeCastId

-- ---------- NSPickerTouchBarItem ----------

-- | Phantom type for @NSPickerTouchBarItem@.
data NSPickerTouchBarItem

instance IsObjCObject (Id NSPickerTouchBarItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPickerTouchBarItem"

class IsNSTouchBarItem a => IsNSPickerTouchBarItem a where
  toNSPickerTouchBarItem :: a -> Id NSPickerTouchBarItem

instance IsNSPickerTouchBarItem (Id NSPickerTouchBarItem) where
  toNSPickerTouchBarItem = unsafeCastId

instance IsNSObject (Id NSPickerTouchBarItem) where
  toNSObject = unsafeCastId

instance IsNSTouchBarItem (Id NSPickerTouchBarItem) where
  toNSTouchBarItem = unsafeCastId

-- ---------- NSPopoverTouchBarItem ----------

-- | Phantom type for @NSPopoverTouchBarItem@.
data NSPopoverTouchBarItem

instance IsObjCObject (Id NSPopoverTouchBarItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPopoverTouchBarItem"

class IsNSTouchBarItem a => IsNSPopoverTouchBarItem a where
  toNSPopoverTouchBarItem :: a -> Id NSPopoverTouchBarItem

instance IsNSPopoverTouchBarItem (Id NSPopoverTouchBarItem) where
  toNSPopoverTouchBarItem = unsafeCastId

instance IsNSObject (Id NSPopoverTouchBarItem) where
  toNSObject = unsafeCastId

instance IsNSTouchBarItem (Id NSPopoverTouchBarItem) where
  toNSTouchBarItem = unsafeCastId

-- ---------- NSSharingServicePickerTouchBarItem ----------

-- | Phantom type for @NSSharingServicePickerTouchBarItem@.
data NSSharingServicePickerTouchBarItem

instance IsObjCObject (Id NSSharingServicePickerTouchBarItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSharingServicePickerTouchBarItem"

class IsNSTouchBarItem a => IsNSSharingServicePickerTouchBarItem a where
  toNSSharingServicePickerTouchBarItem :: a -> Id NSSharingServicePickerTouchBarItem

instance IsNSSharingServicePickerTouchBarItem (Id NSSharingServicePickerTouchBarItem) where
  toNSSharingServicePickerTouchBarItem = unsafeCastId

instance IsNSObject (Id NSSharingServicePickerTouchBarItem) where
  toNSObject = unsafeCastId

instance IsNSTouchBarItem (Id NSSharingServicePickerTouchBarItem) where
  toNSTouchBarItem = unsafeCastId

-- ---------- NSSliderTouchBarItem ----------

-- | Phantom type for @NSSliderTouchBarItem@.
data NSSliderTouchBarItem

instance IsObjCObject (Id NSSliderTouchBarItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSliderTouchBarItem"

class IsNSTouchBarItem a => IsNSSliderTouchBarItem a where
  toNSSliderTouchBarItem :: a -> Id NSSliderTouchBarItem

instance IsNSSliderTouchBarItem (Id NSSliderTouchBarItem) where
  toNSSliderTouchBarItem = unsafeCastId

instance IsNSObject (Id NSSliderTouchBarItem) where
  toNSObject = unsafeCastId

instance IsNSTouchBarItem (Id NSSliderTouchBarItem) where
  toNSTouchBarItem = unsafeCastId

-- ---------- NSStepperTouchBarItem ----------

-- | Phantom type for @NSStepperTouchBarItem@.
data NSStepperTouchBarItem

instance IsObjCObject (Id NSStepperTouchBarItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSStepperTouchBarItem"

class IsNSTouchBarItem a => IsNSStepperTouchBarItem a where
  toNSStepperTouchBarItem :: a -> Id NSStepperTouchBarItem

instance IsNSStepperTouchBarItem (Id NSStepperTouchBarItem) where
  toNSStepperTouchBarItem = unsafeCastId

instance IsNSObject (Id NSStepperTouchBarItem) where
  toNSObject = unsafeCastId

instance IsNSTouchBarItem (Id NSStepperTouchBarItem) where
  toNSTouchBarItem = unsafeCastId

-- ---------- NSATSTypesetter ----------

-- | Phantom type for @NSATSTypesetter@.
data NSATSTypesetter

instance IsObjCObject (Id NSATSTypesetter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSATSTypesetter"

class IsNSTypesetter a => IsNSATSTypesetter a where
  toNSATSTypesetter :: a -> Id NSATSTypesetter

instance IsNSATSTypesetter (Id NSATSTypesetter) where
  toNSATSTypesetter = unsafeCastId

instance IsNSObject (Id NSATSTypesetter) where
  toNSObject = unsafeCastId

instance IsNSTypesetter (Id NSATSTypesetter) where
  toNSTypesetter = unsafeCastId

-- ---------- NSOpenGLLayer ----------

-- | Phantom type for @NSOpenGLLayer@.
data NSOpenGLLayer

instance IsObjCObject (Id NSOpenGLLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSOpenGLLayer"

class IsCAOpenGLLayer a => IsNSOpenGLLayer a where
  toNSOpenGLLayer :: a -> Id NSOpenGLLayer

instance IsNSOpenGLLayer (Id NSOpenGLLayer) where
  toNSOpenGLLayer = unsafeCastId

instance IsCALayer (Id NSOpenGLLayer) where
  toCALayer = unsafeCastId

instance IsCAOpenGLLayer (Id NSOpenGLLayer) where
  toCAOpenGLLayer = unsafeCastId

instance IsNSObject (Id NSOpenGLLayer) where
  toNSObject = unsafeCastId

-- ---------- NSTextStorage ----------

-- | Phantom type for @NSTextStorage@.
data NSTextStorage

instance IsObjCObject (Id NSTextStorage) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTextStorage"

class IsNSMutableAttributedString a => IsNSTextStorage a where
  toNSTextStorage :: a -> Id NSTextStorage

instance IsNSTextStorage (Id NSTextStorage) where
  toNSTextStorage = unsafeCastId

instance IsNSAttributedString (Id NSTextStorage) where
  toNSAttributedString = unsafeCastId

instance IsNSMutableAttributedString (Id NSTextStorage) where
  toNSMutableAttributedString = unsafeCastId

instance IsNSObject (Id NSTextStorage) where
  toNSObject = unsafeCastId

-- ---------- NSButtonCell ----------

-- | Phantom type for @NSButtonCell@.
data NSButtonCell

instance IsObjCObject (Id NSButtonCell) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSButtonCell"

class IsNSActionCell a => IsNSButtonCell a where
  toNSButtonCell :: a -> Id NSButtonCell

instance IsNSButtonCell (Id NSButtonCell) where
  toNSButtonCell = unsafeCastId

instance IsNSActionCell (Id NSButtonCell) where
  toNSActionCell = unsafeCastId

instance IsNSCell (Id NSButtonCell) where
  toNSCell = unsafeCastId

instance IsNSObject (Id NSButtonCell) where
  toNSObject = unsafeCastId

-- ---------- NSDatePickerCell ----------

-- | Phantom type for @NSDatePickerCell@.
data NSDatePickerCell

instance IsObjCObject (Id NSDatePickerCell) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSDatePickerCell"

class IsNSActionCell a => IsNSDatePickerCell a where
  toNSDatePickerCell :: a -> Id NSDatePickerCell

instance IsNSDatePickerCell (Id NSDatePickerCell) where
  toNSDatePickerCell = unsafeCastId

instance IsNSActionCell (Id NSDatePickerCell) where
  toNSActionCell = unsafeCastId

instance IsNSCell (Id NSDatePickerCell) where
  toNSCell = unsafeCastId

instance IsNSObject (Id NSDatePickerCell) where
  toNSObject = unsafeCastId

-- ---------- NSFormCell ----------

-- | Phantom type for @NSFormCell@.
data NSFormCell

instance IsObjCObject (Id NSFormCell) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSFormCell"

class IsNSActionCell a => IsNSFormCell a where
  toNSFormCell :: a -> Id NSFormCell

instance IsNSFormCell (Id NSFormCell) where
  toNSFormCell = unsafeCastId

instance IsNSActionCell (Id NSFormCell) where
  toNSActionCell = unsafeCastId

instance IsNSCell (Id NSFormCell) where
  toNSCell = unsafeCastId

instance IsNSObject (Id NSFormCell) where
  toNSObject = unsafeCastId

-- ---------- NSLevelIndicatorCell ----------

-- | Phantom type for @NSLevelIndicatorCell@.
data NSLevelIndicatorCell

instance IsObjCObject (Id NSLevelIndicatorCell) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSLevelIndicatorCell"

class IsNSActionCell a => IsNSLevelIndicatorCell a where
  toNSLevelIndicatorCell :: a -> Id NSLevelIndicatorCell

instance IsNSLevelIndicatorCell (Id NSLevelIndicatorCell) where
  toNSLevelIndicatorCell = unsafeCastId

instance IsNSActionCell (Id NSLevelIndicatorCell) where
  toNSActionCell = unsafeCastId

instance IsNSCell (Id NSLevelIndicatorCell) where
  toNSCell = unsafeCastId

instance IsNSObject (Id NSLevelIndicatorCell) where
  toNSObject = unsafeCastId

-- ---------- NSPathCell ----------

-- | Phantom type for @NSPathCell@.
data NSPathCell

instance IsObjCObject (Id NSPathCell) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPathCell"

class IsNSActionCell a => IsNSPathCell a where
  toNSPathCell :: a -> Id NSPathCell

instance IsNSPathCell (Id NSPathCell) where
  toNSPathCell = unsafeCastId

instance IsNSActionCell (Id NSPathCell) where
  toNSActionCell = unsafeCastId

instance IsNSCell (Id NSPathCell) where
  toNSCell = unsafeCastId

instance IsNSObject (Id NSPathCell) where
  toNSObject = unsafeCastId

-- ---------- NSSegmentedCell ----------

-- | Phantom type for @NSSegmentedCell@.
data NSSegmentedCell

instance IsObjCObject (Id NSSegmentedCell) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSegmentedCell"

class IsNSActionCell a => IsNSSegmentedCell a where
  toNSSegmentedCell :: a -> Id NSSegmentedCell

instance IsNSSegmentedCell (Id NSSegmentedCell) where
  toNSSegmentedCell = unsafeCastId

instance IsNSActionCell (Id NSSegmentedCell) where
  toNSActionCell = unsafeCastId

instance IsNSCell (Id NSSegmentedCell) where
  toNSCell = unsafeCastId

instance IsNSObject (Id NSSegmentedCell) where
  toNSObject = unsafeCastId

-- ---------- NSSliderCell ----------

-- | Phantom type for @NSSliderCell@.
data NSSliderCell

instance IsObjCObject (Id NSSliderCell) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSliderCell"

class IsNSActionCell a => IsNSSliderCell a where
  toNSSliderCell :: a -> Id NSSliderCell

instance IsNSSliderCell (Id NSSliderCell) where
  toNSSliderCell = unsafeCastId

instance IsNSActionCell (Id NSSliderCell) where
  toNSActionCell = unsafeCastId

instance IsNSCell (Id NSSliderCell) where
  toNSCell = unsafeCastId

instance IsNSObject (Id NSSliderCell) where
  toNSObject = unsafeCastId

-- ---------- NSStepperCell ----------

-- | Phantom type for @NSStepperCell@.
data NSStepperCell

instance IsObjCObject (Id NSStepperCell) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSStepperCell"

class IsNSActionCell a => IsNSStepperCell a where
  toNSStepperCell :: a -> Id NSStepperCell

instance IsNSStepperCell (Id NSStepperCell) where
  toNSStepperCell = unsafeCastId

instance IsNSActionCell (Id NSStepperCell) where
  toNSActionCell = unsafeCastId

instance IsNSCell (Id NSStepperCell) where
  toNSCell = unsafeCastId

instance IsNSObject (Id NSStepperCell) where
  toNSObject = unsafeCastId

-- ---------- NSTextFieldCell ----------

-- | Phantom type for @NSTextFieldCell@.
data NSTextFieldCell

instance IsObjCObject (Id NSTextFieldCell) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTextFieldCell"

class IsNSActionCell a => IsNSTextFieldCell a where
  toNSTextFieldCell :: a -> Id NSTextFieldCell

instance IsNSTextFieldCell (Id NSTextFieldCell) where
  toNSTextFieldCell = unsafeCastId

instance IsNSActionCell (Id NSTextFieldCell) where
  toNSActionCell = unsafeCastId

instance IsNSCell (Id NSTextFieldCell) where
  toNSCell = unsafeCastId

instance IsNSObject (Id NSTextFieldCell) where
  toNSObject = unsafeCastId

-- ---------- NSCollectionLayoutBoundarySupplementaryItem ----------

-- | Phantom type for @NSCollectionLayoutBoundarySupplementaryItem@.
data NSCollectionLayoutBoundarySupplementaryItem

instance IsObjCObject (Id NSCollectionLayoutBoundarySupplementaryItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSCollectionLayoutBoundarySupplementaryItem"

class IsNSCollectionLayoutSupplementaryItem a => IsNSCollectionLayoutBoundarySupplementaryItem a where
  toNSCollectionLayoutBoundarySupplementaryItem :: a -> Id NSCollectionLayoutBoundarySupplementaryItem

instance IsNSCollectionLayoutBoundarySupplementaryItem (Id NSCollectionLayoutBoundarySupplementaryItem) where
  toNSCollectionLayoutBoundarySupplementaryItem = unsafeCastId

instance IsNSCollectionLayoutItem (Id NSCollectionLayoutBoundarySupplementaryItem) where
  toNSCollectionLayoutItem = unsafeCastId

instance IsNSCollectionLayoutSupplementaryItem (Id NSCollectionLayoutBoundarySupplementaryItem) where
  toNSCollectionLayoutSupplementaryItem = unsafeCastId

instance IsNSObject (Id NSCollectionLayoutBoundarySupplementaryItem) where
  toNSObject = unsafeCastId

-- ---------- NSArrayController ----------

-- | Phantom type for @NSArrayController@.
data NSArrayController

instance IsObjCObject (Id NSArrayController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSArrayController"

class IsNSObjectController a => IsNSArrayController a where
  toNSArrayController :: a -> Id NSArrayController

instance IsNSArrayController (Id NSArrayController) where
  toNSArrayController = unsafeCastId

instance IsNSController (Id NSArrayController) where
  toNSController = unsafeCastId

instance IsNSObject (Id NSArrayController) where
  toNSObject = unsafeCastId

instance IsNSObjectController (Id NSArrayController) where
  toNSObjectController = unsafeCastId

-- ---------- NSTreeController ----------

-- | Phantom type for @NSTreeController@.
data NSTreeController

instance IsObjCObject (Id NSTreeController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTreeController"

class IsNSObjectController a => IsNSTreeController a where
  toNSTreeController :: a -> Id NSTreeController

instance IsNSTreeController (Id NSTreeController) where
  toNSTreeController = unsafeCastId

instance IsNSController (Id NSTreeController) where
  toNSController = unsafeCastId

instance IsNSObject (Id NSTreeController) where
  toNSObject = unsafeCastId

instance IsNSObjectController (Id NSTreeController) where
  toNSObjectController = unsafeCastId

-- ---------- NSBackgroundExtensionView ----------

-- | A view that extends content to fill its own bounds.
--
-- A background extension view can be laid out to extend outside the safe area, such as under the titlebar, sidebar, or inspector. By default it lays out its content to stay within the safe area, and uses modifications of the content along the edges to fill the container view.
-- 
-- Phantom type for @NSBackgroundExtensionView@.
data NSBackgroundExtensionView

instance IsObjCObject (Id NSBackgroundExtensionView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSBackgroundExtensionView"

class IsNSView a => IsNSBackgroundExtensionView a where
  toNSBackgroundExtensionView :: a -> Id NSBackgroundExtensionView

instance IsNSBackgroundExtensionView (Id NSBackgroundExtensionView) where
  toNSBackgroundExtensionView = unsafeCastId

instance IsNSObject (Id NSBackgroundExtensionView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSBackgroundExtensionView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id NSBackgroundExtensionView) where
  toNSView = unsafeCastId

-- ---------- NSBox ----------

-- | Phantom type for @NSBox@.
data NSBox

instance IsObjCObject (Id NSBox) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSBox"

class IsNSView a => IsNSBox a where
  toNSBox :: a -> Id NSBox

instance IsNSBox (Id NSBox) where
  toNSBox = unsafeCastId

instance IsNSObject (Id NSBox) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSBox) where
  toNSResponder = unsafeCastId

instance IsNSView (Id NSBox) where
  toNSView = unsafeCastId

-- ---------- NSClipView ----------

-- | Phantom type for @NSClipView@.
data NSClipView

instance IsObjCObject (Id NSClipView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSClipView"

class IsNSView a => IsNSClipView a where
  toNSClipView :: a -> Id NSClipView

instance IsNSClipView (Id NSClipView) where
  toNSClipView = unsafeCastId

instance IsNSObject (Id NSClipView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSClipView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id NSClipView) where
  toNSView = unsafeCastId

-- ---------- NSCollectionView ----------

-- | Phantom type for @NSCollectionView@.
data NSCollectionView

instance IsObjCObject (Id NSCollectionView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSCollectionView"

class IsNSView a => IsNSCollectionView a where
  toNSCollectionView :: a -> Id NSCollectionView

instance IsNSCollectionView (Id NSCollectionView) where
  toNSCollectionView = unsafeCastId

instance IsNSObject (Id NSCollectionView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSCollectionView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id NSCollectionView) where
  toNSView = unsafeCastId

-- ---------- NSControl ----------

-- | Phantom type for @NSControl@.
data NSControl

instance IsObjCObject (Id NSControl) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSControl"

class IsNSView a => IsNSControl a where
  toNSControl :: a -> Id NSControl

instance IsNSControl (Id NSControl) where
  toNSControl = unsafeCastId

instance IsNSObject (Id NSControl) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSControl) where
  toNSResponder = unsafeCastId

instance IsNSView (Id NSControl) where
  toNSView = unsafeCastId

-- ---------- NSGlassEffectContainerView ----------

-- | A view that efficiently merges descendant glass effect views together when they are within a specified proximity to each other.
--
-- - Tip: Using a glass effect container view can improve performance by reducing the number of passes required to render similar glass effect views.
-- 
-- Phantom type for @NSGlassEffectContainerView@.
data NSGlassEffectContainerView

instance IsObjCObject (Id NSGlassEffectContainerView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSGlassEffectContainerView"

class IsNSView a => IsNSGlassEffectContainerView a where
  toNSGlassEffectContainerView :: a -> Id NSGlassEffectContainerView

instance IsNSGlassEffectContainerView (Id NSGlassEffectContainerView) where
  toNSGlassEffectContainerView = unsafeCastId

instance IsNSObject (Id NSGlassEffectContainerView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSGlassEffectContainerView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id NSGlassEffectContainerView) where
  toNSView = unsafeCastId

-- ---------- NSGlassEffectView ----------

-- | A view that embeds its content view in a dynamic glass effect.
-- 
-- Phantom type for @NSGlassEffectView@.
data NSGlassEffectView

instance IsObjCObject (Id NSGlassEffectView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSGlassEffectView"

class IsNSView a => IsNSGlassEffectView a where
  toNSGlassEffectView :: a -> Id NSGlassEffectView

instance IsNSGlassEffectView (Id NSGlassEffectView) where
  toNSGlassEffectView = unsafeCastId

instance IsNSObject (Id NSGlassEffectView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSGlassEffectView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id NSGlassEffectView) where
  toNSView = unsafeCastId

-- ---------- NSGridView ----------

-- | Phantom type for @NSGridView@.
data NSGridView

instance IsObjCObject (Id NSGridView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSGridView"

class IsNSView a => IsNSGridView a where
  toNSGridView :: a -> Id NSGridView

instance IsNSGridView (Id NSGridView) where
  toNSGridView = unsafeCastId

instance IsNSObject (Id NSGridView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSGridView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id NSGridView) where
  toNSView = unsafeCastId

-- ---------- NSOpenGLView ----------

-- | Phantom type for @NSOpenGLView@.
data NSOpenGLView

instance IsObjCObject (Id NSOpenGLView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSOpenGLView"

class IsNSView a => IsNSOpenGLView a where
  toNSOpenGLView :: a -> Id NSOpenGLView

instance IsNSOpenGLView (Id NSOpenGLView) where
  toNSOpenGLView = unsafeCastId

instance IsNSObject (Id NSOpenGLView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSOpenGLView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id NSOpenGLView) where
  toNSView = unsafeCastId

-- ---------- NSProgressIndicator ----------

-- | Phantom type for @NSProgressIndicator@.
data NSProgressIndicator

instance IsObjCObject (Id NSProgressIndicator) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSProgressIndicator"

class IsNSView a => IsNSProgressIndicator a where
  toNSProgressIndicator :: a -> Id NSProgressIndicator

instance IsNSProgressIndicator (Id NSProgressIndicator) where
  toNSProgressIndicator = unsafeCastId

instance IsNSObject (Id NSProgressIndicator) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSProgressIndicator) where
  toNSResponder = unsafeCastId

instance IsNSView (Id NSProgressIndicator) where
  toNSView = unsafeCastId

-- ---------- NSRulerView ----------

-- | Phantom type for @NSRulerView@.
data NSRulerView

instance IsObjCObject (Id NSRulerView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSRulerView"

class IsNSView a => IsNSRulerView a where
  toNSRulerView :: a -> Id NSRulerView

instance IsNSRulerView (Id NSRulerView) where
  toNSRulerView = unsafeCastId

instance IsNSObject (Id NSRulerView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSRulerView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id NSRulerView) where
  toNSView = unsafeCastId

-- ---------- NSScrollView ----------

-- | Phantom type for @NSScrollView@.
data NSScrollView

instance IsObjCObject (Id NSScrollView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSScrollView"

class IsNSView a => IsNSScrollView a where
  toNSScrollView :: a -> Id NSScrollView

instance IsNSScrollView (Id NSScrollView) where
  toNSScrollView = unsafeCastId

instance IsNSObject (Id NSScrollView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSScrollView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id NSScrollView) where
  toNSView = unsafeCastId

-- ---------- NSScrubber ----------

-- | NSScrubber
--
-- @NSScrubber@ is a control designed for the NSTouchBar environment.
--
-- @NSScrubber@ arranges a finite number of "items" (represented by views of type @NSScrubberItemView@ ) according to a layout object (see @NSScrubberLayout@ ), and provides several methods for navigating and selecting those items.
--
-- Clients provide data to @NSScrubber@ via a data source object (see the @NSScrubberDataSource@ protocol) and react to user interaction via a delegate object (see the @NSScrubberDelegate@ protocol).
-- 
-- Phantom type for @NSScrubber@.
data NSScrubber

instance IsObjCObject (Id NSScrubber) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSScrubber"

class IsNSView a => IsNSScrubber a where
  toNSScrubber :: a -> Id NSScrubber

instance IsNSScrubber (Id NSScrubber) where
  toNSScrubber = unsafeCastId

instance IsNSObject (Id NSScrubber) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSScrubber) where
  toNSResponder = unsafeCastId

instance IsNSView (Id NSScrubber) where
  toNSView = unsafeCastId

-- ---------- NSScrubberArrangedView ----------

-- | Phantom type for @NSScrubberArrangedView@.
data NSScrubberArrangedView

instance IsObjCObject (Id NSScrubberArrangedView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSScrubberArrangedView"

class IsNSView a => IsNSScrubberArrangedView a where
  toNSScrubberArrangedView :: a -> Id NSScrubberArrangedView

instance IsNSScrubberArrangedView (Id NSScrubberArrangedView) where
  toNSScrubberArrangedView = unsafeCastId

instance IsNSObject (Id NSScrubberArrangedView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSScrubberArrangedView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id NSScrubberArrangedView) where
  toNSView = unsafeCastId

-- ---------- NSSplitView ----------

-- | Phantom type for @NSSplitView@.
data NSSplitView

instance IsObjCObject (Id NSSplitView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSplitView"

class IsNSView a => IsNSSplitView a where
  toNSSplitView :: a -> Id NSSplitView

instance IsNSSplitView (Id NSSplitView) where
  toNSSplitView = unsafeCastId

instance IsNSObject (Id NSSplitView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSSplitView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id NSSplitView) where
  toNSView = unsafeCastId

-- ---------- NSStackView ----------

-- | Phantom type for @NSStackView@.
data NSStackView

instance IsObjCObject (Id NSStackView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSStackView"

class IsNSView a => IsNSStackView a where
  toNSStackView :: a -> Id NSStackView

instance IsNSStackView (Id NSStackView) where
  toNSStackView = unsafeCastId

instance IsNSObject (Id NSStackView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSStackView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id NSStackView) where
  toNSView = unsafeCastId

-- ---------- NSTabView ----------

-- | Phantom type for @NSTabView@.
data NSTabView

instance IsObjCObject (Id NSTabView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTabView"

class IsNSView a => IsNSTabView a where
  toNSTabView :: a -> Id NSTabView

instance IsNSTabView (Id NSTabView) where
  toNSTabView = unsafeCastId

instance IsNSObject (Id NSTabView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSTabView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id NSTabView) where
  toNSView = unsafeCastId

-- ---------- NSTableCellView ----------

-- | Phantom type for @NSTableCellView@.
data NSTableCellView

instance IsObjCObject (Id NSTableCellView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTableCellView"

class IsNSView a => IsNSTableCellView a where
  toNSTableCellView :: a -> Id NSTableCellView

instance IsNSTableCellView (Id NSTableCellView) where
  toNSTableCellView = unsafeCastId

instance IsNSObject (Id NSTableCellView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSTableCellView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id NSTableCellView) where
  toNSView = unsafeCastId

-- ---------- NSTableHeaderView ----------

-- | Phantom type for @NSTableHeaderView@.
data NSTableHeaderView

instance IsObjCObject (Id NSTableHeaderView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTableHeaderView"

class IsNSView a => IsNSTableHeaderView a where
  toNSTableHeaderView :: a -> Id NSTableHeaderView

instance IsNSTableHeaderView (Id NSTableHeaderView) where
  toNSTableHeaderView = unsafeCastId

instance IsNSObject (Id NSTableHeaderView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSTableHeaderView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id NSTableHeaderView) where
  toNSView = unsafeCastId

-- ---------- NSTableRowView ----------

-- | Phantom type for @NSTableRowView@.
data NSTableRowView

instance IsObjCObject (Id NSTableRowView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTableRowView"

class IsNSView a => IsNSTableRowView a where
  toNSTableRowView :: a -> Id NSTableRowView

instance IsNSTableRowView (Id NSTableRowView) where
  toNSTableRowView = unsafeCastId

instance IsNSObject (Id NSTableRowView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSTableRowView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id NSTableRowView) where
  toNSView = unsafeCastId

-- ---------- NSText ----------

-- | Phantom type for @NSText@.
data NSText

instance IsObjCObject (Id NSText) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSText"

class IsNSView a => IsNSText a where
  toNSText :: a -> Id NSText

instance IsNSText (Id NSText) where
  toNSText = unsafeCastId

instance IsNSObject (Id NSText) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSText) where
  toNSResponder = unsafeCastId

instance IsNSView (Id NSText) where
  toNSView = unsafeCastId

-- ---------- NSTextInsertionIndicator ----------

-- | Phantom type for @NSTextInsertionIndicator@.
data NSTextInsertionIndicator

instance IsObjCObject (Id NSTextInsertionIndicator) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTextInsertionIndicator"

class IsNSView a => IsNSTextInsertionIndicator a where
  toNSTextInsertionIndicator :: a -> Id NSTextInsertionIndicator

instance IsNSTextInsertionIndicator (Id NSTextInsertionIndicator) where
  toNSTextInsertionIndicator = unsafeCastId

instance IsNSObject (Id NSTextInsertionIndicator) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSTextInsertionIndicator) where
  toNSResponder = unsafeCastId

instance IsNSView (Id NSTextInsertionIndicator) where
  toNSView = unsafeCastId

-- ---------- NSVisualEffectView ----------

-- | Phantom type for @NSVisualEffectView@.
data NSVisualEffectView

instance IsObjCObject (Id NSVisualEffectView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSVisualEffectView"

class IsNSView a => IsNSVisualEffectView a where
  toNSVisualEffectView :: a -> Id NSVisualEffectView

instance IsNSVisualEffectView (Id NSVisualEffectView) where
  toNSVisualEffectView = unsafeCastId

instance IsNSObject (Id NSVisualEffectView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSVisualEffectView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id NSVisualEffectView) where
  toNSView = unsafeCastId

-- ---------- NSCollectionViewItem ----------

-- | Phantom type for @NSCollectionViewItem@.
data NSCollectionViewItem

instance IsObjCObject (Id NSCollectionViewItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSCollectionViewItem"

class IsNSViewController a => IsNSCollectionViewItem a where
  toNSCollectionViewItem :: a -> Id NSCollectionViewItem

instance IsNSCollectionViewItem (Id NSCollectionViewItem) where
  toNSCollectionViewItem = unsafeCastId

instance IsNSObject (Id NSCollectionViewItem) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSCollectionViewItem) where
  toNSResponder = unsafeCastId

instance IsNSViewController (Id NSCollectionViewItem) where
  toNSViewController = unsafeCastId

-- ---------- NSPageController ----------

-- | Phantom type for @NSPageController@.
data NSPageController

instance IsObjCObject (Id NSPageController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPageController"

class IsNSViewController a => IsNSPageController a where
  toNSPageController :: a -> Id NSPageController

instance IsNSPageController (Id NSPageController) where
  toNSPageController = unsafeCastId

instance IsNSObject (Id NSPageController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSPageController) where
  toNSResponder = unsafeCastId

instance IsNSViewController (Id NSPageController) where
  toNSViewController = unsafeCastId

-- ---------- NSSplitViewController ----------

-- | NSSplitViewController is a container view controller that manages side-by-side (horizontal or vertical) children view controllers. Views are lazily loaded. For instance, adding a collapsed SplitViewItem will not load the associated ViewController's view until it is uncollapsed. The NSSplitViewController is set as the delegate of its managed NSSplitView. Any overrides of NSSplitViewDelegate methods must call super. Only the @-vertical,@ @-autosaveName,@ and divider properties should be manipulated on the managed NSSplitView. Changing other properties (such as delegate, manipulating subviews, holding priorities) will cause an exception to be thrown. Autolayout must be used with NSSplitViewController to properly control the layout of the child views and the animations of collapses and reveals. e.g., Constraints can be used to setup whether a window should grow/shrink or stay the same size when showing and hiding a sidebar. NSViewController's methods @-addChildViewController:,@ @-insertViewController:atIndex:,@ and @-removeChildViewControllerAtIndex:@ can all be used as convience methods to add children; default SplitViewItems will be appropriately created or destroyed.
-- 
-- Phantom type for @NSSplitViewController@.
data NSSplitViewController

instance IsObjCObject (Id NSSplitViewController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSplitViewController"

class IsNSViewController a => IsNSSplitViewController a where
  toNSSplitViewController :: a -> Id NSSplitViewController

instance IsNSSplitViewController (Id NSSplitViewController) where
  toNSSplitViewController = unsafeCastId

instance IsNSObject (Id NSSplitViewController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSSplitViewController) where
  toNSResponder = unsafeCastId

instance IsNSViewController (Id NSSplitViewController) where
  toNSViewController = unsafeCastId

-- ---------- NSSplitViewItemAccessoryViewController ----------

-- | Phantom type for @NSSplitViewItemAccessoryViewController@.
data NSSplitViewItemAccessoryViewController

instance IsObjCObject (Id NSSplitViewItemAccessoryViewController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSplitViewItemAccessoryViewController"

class IsNSViewController a => IsNSSplitViewItemAccessoryViewController a where
  toNSSplitViewItemAccessoryViewController :: a -> Id NSSplitViewItemAccessoryViewController

instance IsNSSplitViewItemAccessoryViewController (Id NSSplitViewItemAccessoryViewController) where
  toNSSplitViewItemAccessoryViewController = unsafeCastId

instance IsNSObject (Id NSSplitViewItemAccessoryViewController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSSplitViewItemAccessoryViewController) where
  toNSResponder = unsafeCastId

instance IsNSViewController (Id NSSplitViewItemAccessoryViewController) where
  toNSViewController = unsafeCastId

-- ---------- NSTabViewController ----------

-- | NSTabViewController is a container view controller that displays a single child view controller at a time from its @childViewControllers.@ It provides standard tab-style UI for user selection of tabs, or allows custom UI to be easily created by providing targets for bindings. ChildViewControllers’ views are lazily loaded; they are only loaded once their tab is selected and visible. The NSTabViewController is set as the delegate of its managed NSTabView. Any overrides of NSTabViewDelegate methods must call super. Properties of the TabView such as the tabStyle can be directly manipulated, but calling methods that add and remove tabViewItems or changing the delegate is not allowed. NSViewController's methods @-addChildViewController:,@ @-insertViewController:atIndex:,@ and @-removeChildViewControllerAtIndex:@ can all be used as convience methods to add children; default TabViewItems will be appropriately created or destroyed. The default NSTabViewItem created with with +[NSTabViewItem tabViewItemForViewController:].
-- 
-- Phantom type for @NSTabViewController@.
data NSTabViewController

instance IsObjCObject (Id NSTabViewController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTabViewController"

class IsNSViewController a => IsNSTabViewController a where
  toNSTabViewController :: a -> Id NSTabViewController

instance IsNSTabViewController (Id NSTabViewController) where
  toNSTabViewController = unsafeCastId

instance IsNSObject (Id NSTabViewController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSTabViewController) where
  toNSResponder = unsafeCastId

instance IsNSViewController (Id NSTabViewController) where
  toNSViewController = unsafeCastId

-- ---------- NSTitlebarAccessoryViewController ----------

-- | Phantom type for @NSTitlebarAccessoryViewController@.
data NSTitlebarAccessoryViewController

instance IsObjCObject (Id NSTitlebarAccessoryViewController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTitlebarAccessoryViewController"

class IsNSViewController a => IsNSTitlebarAccessoryViewController a where
  toNSTitlebarAccessoryViewController :: a -> Id NSTitlebarAccessoryViewController

instance IsNSTitlebarAccessoryViewController (Id NSTitlebarAccessoryViewController) where
  toNSTitlebarAccessoryViewController = unsafeCastId

instance IsNSObject (Id NSTitlebarAccessoryViewController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSTitlebarAccessoryViewController) where
  toNSResponder = unsafeCastId

instance IsNSViewController (Id NSTitlebarAccessoryViewController) where
  toNSViewController = unsafeCastId

-- ---------- NSPanel ----------

-- | Phantom type for @NSPanel@.
data NSPanel

instance IsObjCObject (Id NSPanel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPanel"

class IsNSWindow a => IsNSPanel a where
  toNSPanel :: a -> Id NSPanel

instance IsNSPanel (Id NSPanel) where
  toNSPanel = unsafeCastId

instance IsNSObject (Id NSPanel) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSPanel) where
  toNSResponder = unsafeCastId

instance IsNSWindow (Id NSPanel) where
  toNSWindow = unsafeCastId

-- ---------- NSTextListElement ----------

-- | Phantom type for @NSTextListElement@.
data NSTextListElement

instance IsObjCObject (Id NSTextListElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTextListElement"

class IsNSTextParagraph a => IsNSTextListElement a where
  toNSTextListElement :: a -> Id NSTextListElement

instance IsNSTextListElement (Id NSTextListElement) where
  toNSTextListElement = unsafeCastId

instance IsNSObject (Id NSTextListElement) where
  toNSObject = unsafeCastId

instance IsNSTextElement (Id NSTextListElement) where
  toNSTextElement = unsafeCastId

instance IsNSTextParagraph (Id NSTextListElement) where
  toNSTextParagraph = unsafeCastId

-- ---------- NSMenuItemCell ----------

-- | Phantom type for @NSMenuItemCell@.
data NSMenuItemCell

instance IsObjCObject (Id NSMenuItemCell) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSMenuItemCell"

class IsNSButtonCell a => IsNSMenuItemCell a where
  toNSMenuItemCell :: a -> Id NSMenuItemCell

instance IsNSMenuItemCell (Id NSMenuItemCell) where
  toNSMenuItemCell = unsafeCastId

instance IsNSActionCell (Id NSMenuItemCell) where
  toNSActionCell = unsafeCastId

instance IsNSButtonCell (Id NSMenuItemCell) where
  toNSButtonCell = unsafeCastId

instance IsNSCell (Id NSMenuItemCell) where
  toNSCell = unsafeCastId

instance IsNSObject (Id NSMenuItemCell) where
  toNSObject = unsafeCastId

-- ---------- NSComboBoxCell ----------

-- | Phantom type for @NSComboBoxCell@.
data NSComboBoxCell

instance IsObjCObject (Id NSComboBoxCell) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSComboBoxCell"

class IsNSTextFieldCell a => IsNSComboBoxCell a where
  toNSComboBoxCell :: a -> Id NSComboBoxCell

instance IsNSComboBoxCell (Id NSComboBoxCell) where
  toNSComboBoxCell = unsafeCastId

instance IsNSActionCell (Id NSComboBoxCell) where
  toNSActionCell = unsafeCastId

instance IsNSCell (Id NSComboBoxCell) where
  toNSCell = unsafeCastId

instance IsNSObject (Id NSComboBoxCell) where
  toNSObject = unsafeCastId

instance IsNSTextFieldCell (Id NSComboBoxCell) where
  toNSTextFieldCell = unsafeCastId

-- ---------- NSPathComponentCell ----------

-- | Phantom type for @NSPathComponentCell@.
data NSPathComponentCell

instance IsObjCObject (Id NSPathComponentCell) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPathComponentCell"

class IsNSTextFieldCell a => IsNSPathComponentCell a where
  toNSPathComponentCell :: a -> Id NSPathComponentCell

instance IsNSPathComponentCell (Id NSPathComponentCell) where
  toNSPathComponentCell = unsafeCastId

instance IsNSActionCell (Id NSPathComponentCell) where
  toNSActionCell = unsafeCastId

instance IsNSCell (Id NSPathComponentCell) where
  toNSCell = unsafeCastId

instance IsNSObject (Id NSPathComponentCell) where
  toNSObject = unsafeCastId

instance IsNSTextFieldCell (Id NSPathComponentCell) where
  toNSTextFieldCell = unsafeCastId

-- ---------- NSSearchFieldCell ----------

-- | Phantom type for @NSSearchFieldCell@.
data NSSearchFieldCell

instance IsObjCObject (Id NSSearchFieldCell) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSearchFieldCell"

class IsNSTextFieldCell a => IsNSSearchFieldCell a where
  toNSSearchFieldCell :: a -> Id NSSearchFieldCell

instance IsNSSearchFieldCell (Id NSSearchFieldCell) where
  toNSSearchFieldCell = unsafeCastId

instance IsNSActionCell (Id NSSearchFieldCell) where
  toNSActionCell = unsafeCastId

instance IsNSCell (Id NSSearchFieldCell) where
  toNSCell = unsafeCastId

instance IsNSObject (Id NSSearchFieldCell) where
  toNSObject = unsafeCastId

instance IsNSTextFieldCell (Id NSSearchFieldCell) where
  toNSTextFieldCell = unsafeCastId

-- ---------- NSSecureTextFieldCell ----------

-- | Phantom type for @NSSecureTextFieldCell@.
data NSSecureTextFieldCell

instance IsObjCObject (Id NSSecureTextFieldCell) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSecureTextFieldCell"

class IsNSTextFieldCell a => IsNSSecureTextFieldCell a where
  toNSSecureTextFieldCell :: a -> Id NSSecureTextFieldCell

instance IsNSSecureTextFieldCell (Id NSSecureTextFieldCell) where
  toNSSecureTextFieldCell = unsafeCastId

instance IsNSActionCell (Id NSSecureTextFieldCell) where
  toNSActionCell = unsafeCastId

instance IsNSCell (Id NSSecureTextFieldCell) where
  toNSCell = unsafeCastId

instance IsNSObject (Id NSSecureTextFieldCell) where
  toNSObject = unsafeCastId

instance IsNSTextFieldCell (Id NSSecureTextFieldCell) where
  toNSTextFieldCell = unsafeCastId

-- ---------- NSTableHeaderCell ----------

-- | Phantom type for @NSTableHeaderCell@.
data NSTableHeaderCell

instance IsObjCObject (Id NSTableHeaderCell) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTableHeaderCell"

class IsNSTextFieldCell a => IsNSTableHeaderCell a where
  toNSTableHeaderCell :: a -> Id NSTableHeaderCell

instance IsNSTableHeaderCell (Id NSTableHeaderCell) where
  toNSTableHeaderCell = unsafeCastId

instance IsNSActionCell (Id NSTableHeaderCell) where
  toNSActionCell = unsafeCastId

instance IsNSCell (Id NSTableHeaderCell) where
  toNSCell = unsafeCastId

instance IsNSObject (Id NSTableHeaderCell) where
  toNSObject = unsafeCastId

instance IsNSTextFieldCell (Id NSTableHeaderCell) where
  toNSTextFieldCell = unsafeCastId

-- ---------- NSTokenFieldCell ----------

-- | Phantom type for @NSTokenFieldCell@.
data NSTokenFieldCell

instance IsObjCObject (Id NSTokenFieldCell) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTokenFieldCell"

class IsNSTextFieldCell a => IsNSTokenFieldCell a where
  toNSTokenFieldCell :: a -> Id NSTokenFieldCell

instance IsNSTokenFieldCell (Id NSTokenFieldCell) where
  toNSTokenFieldCell = unsafeCastId

instance IsNSActionCell (Id NSTokenFieldCell) where
  toNSActionCell = unsafeCastId

instance IsNSCell (Id NSTokenFieldCell) where
  toNSCell = unsafeCastId

instance IsNSObject (Id NSTokenFieldCell) where
  toNSObject = unsafeCastId

instance IsNSTextFieldCell (Id NSTokenFieldCell) where
  toNSTextFieldCell = unsafeCastId

-- ---------- NSDictionaryController ----------

-- | Phantom type for @NSDictionaryController@.
data NSDictionaryController

instance IsObjCObject (Id NSDictionaryController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSDictionaryController"

class IsNSArrayController a => IsNSDictionaryController a where
  toNSDictionaryController :: a -> Id NSDictionaryController

instance IsNSDictionaryController (Id NSDictionaryController) where
  toNSDictionaryController = unsafeCastId

instance IsNSArrayController (Id NSDictionaryController) where
  toNSArrayController = unsafeCastId

instance IsNSController (Id NSDictionaryController) where
  toNSController = unsafeCastId

instance IsNSObject (Id NSDictionaryController) where
  toNSObject = unsafeCastId

instance IsNSObjectController (Id NSDictionaryController) where
  toNSObjectController = unsafeCastId

-- ---------- NSBrowser ----------

-- | Phantom type for @NSBrowser@.
data NSBrowser

instance IsObjCObject (Id NSBrowser) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSBrowser"

class IsNSControl a => IsNSBrowser a where
  toNSBrowser :: a -> Id NSBrowser

instance IsNSBrowser (Id NSBrowser) where
  toNSBrowser = unsafeCastId

instance IsNSControl (Id NSBrowser) where
  toNSControl = unsafeCastId

instance IsNSObject (Id NSBrowser) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSBrowser) where
  toNSResponder = unsafeCastId

instance IsNSView (Id NSBrowser) where
  toNSView = unsafeCastId

-- ---------- NSButton ----------

-- | Phantom type for @NSButton@.
data NSButton

instance IsObjCObject (Id NSButton) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSButton"

class IsNSControl a => IsNSButton a where
  toNSButton :: a -> Id NSButton

instance IsNSButton (Id NSButton) where
  toNSButton = unsafeCastId

instance IsNSControl (Id NSButton) where
  toNSControl = unsafeCastId

instance IsNSObject (Id NSButton) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSButton) where
  toNSResponder = unsafeCastId

instance IsNSView (Id NSButton) where
  toNSView = unsafeCastId

-- ---------- NSColorWell ----------

-- | Phantom type for @NSColorWell@.
data NSColorWell

instance IsObjCObject (Id NSColorWell) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSColorWell"

class IsNSControl a => IsNSColorWell a where
  toNSColorWell :: a -> Id NSColorWell

instance IsNSColorWell (Id NSColorWell) where
  toNSColorWell = unsafeCastId

instance IsNSControl (Id NSColorWell) where
  toNSControl = unsafeCastId

instance IsNSObject (Id NSColorWell) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSColorWell) where
  toNSResponder = unsafeCastId

instance IsNSView (Id NSColorWell) where
  toNSView = unsafeCastId

-- ---------- NSComboButton ----------

-- | Phantom type for @NSComboButton@.
data NSComboButton

instance IsObjCObject (Id NSComboButton) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSComboButton"

class IsNSControl a => IsNSComboButton a where
  toNSComboButton :: a -> Id NSComboButton

instance IsNSComboButton (Id NSComboButton) where
  toNSComboButton = unsafeCastId

instance IsNSControl (Id NSComboButton) where
  toNSControl = unsafeCastId

instance IsNSObject (Id NSComboButton) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSComboButton) where
  toNSResponder = unsafeCastId

instance IsNSView (Id NSComboButton) where
  toNSView = unsafeCastId

-- ---------- NSDatePicker ----------

-- | Phantom type for @NSDatePicker@.
data NSDatePicker

instance IsObjCObject (Id NSDatePicker) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSDatePicker"

class IsNSControl a => IsNSDatePicker a where
  toNSDatePicker :: a -> Id NSDatePicker

instance IsNSDatePicker (Id NSDatePicker) where
  toNSDatePicker = unsafeCastId

instance IsNSControl (Id NSDatePicker) where
  toNSControl = unsafeCastId

instance IsNSObject (Id NSDatePicker) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSDatePicker) where
  toNSResponder = unsafeCastId

instance IsNSView (Id NSDatePicker) where
  toNSView = unsafeCastId

-- ---------- NSImageView ----------

-- | Phantom type for @NSImageView@.
data NSImageView

instance IsObjCObject (Id NSImageView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSImageView"

class IsNSControl a => IsNSImageView a where
  toNSImageView :: a -> Id NSImageView

instance IsNSImageView (Id NSImageView) where
  toNSImageView = unsafeCastId

instance IsNSControl (Id NSImageView) where
  toNSControl = unsafeCastId

instance IsNSObject (Id NSImageView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSImageView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id NSImageView) where
  toNSView = unsafeCastId

-- ---------- NSLevelIndicator ----------

-- | Phantom type for @NSLevelIndicator@.
data NSLevelIndicator

instance IsObjCObject (Id NSLevelIndicator) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSLevelIndicator"

class IsNSControl a => IsNSLevelIndicator a where
  toNSLevelIndicator :: a -> Id NSLevelIndicator

instance IsNSLevelIndicator (Id NSLevelIndicator) where
  toNSLevelIndicator = unsafeCastId

instance IsNSControl (Id NSLevelIndicator) where
  toNSControl = unsafeCastId

instance IsNSObject (Id NSLevelIndicator) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSLevelIndicator) where
  toNSResponder = unsafeCastId

instance IsNSView (Id NSLevelIndicator) where
  toNSView = unsafeCastId

-- ---------- NSMatrix ----------

-- | Phantom type for @NSMatrix@.
data NSMatrix

instance IsObjCObject (Id NSMatrix) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSMatrix"

class IsNSControl a => IsNSMatrix a where
  toNSMatrix :: a -> Id NSMatrix

instance IsNSMatrix (Id NSMatrix) where
  toNSMatrix = unsafeCastId

instance IsNSControl (Id NSMatrix) where
  toNSControl = unsafeCastId

instance IsNSObject (Id NSMatrix) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSMatrix) where
  toNSResponder = unsafeCastId

instance IsNSView (Id NSMatrix) where
  toNSView = unsafeCastId

-- ---------- NSPathControl ----------

-- | Phantom type for @NSPathControl@.
data NSPathControl

instance IsObjCObject (Id NSPathControl) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPathControl"

class IsNSControl a => IsNSPathControl a where
  toNSPathControl :: a -> Id NSPathControl

instance IsNSPathControl (Id NSPathControl) where
  toNSPathControl = unsafeCastId

instance IsNSControl (Id NSPathControl) where
  toNSControl = unsafeCastId

instance IsNSObject (Id NSPathControl) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSPathControl) where
  toNSResponder = unsafeCastId

instance IsNSView (Id NSPathControl) where
  toNSView = unsafeCastId

-- ---------- NSRuleEditor ----------

-- | Phantom type for @NSRuleEditor@.
data NSRuleEditor

instance IsObjCObject (Id NSRuleEditor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSRuleEditor"

class IsNSControl a => IsNSRuleEditor a where
  toNSRuleEditor :: a -> Id NSRuleEditor

instance IsNSRuleEditor (Id NSRuleEditor) where
  toNSRuleEditor = unsafeCastId

instance IsNSControl (Id NSRuleEditor) where
  toNSControl = unsafeCastId

instance IsNSObject (Id NSRuleEditor) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSRuleEditor) where
  toNSResponder = unsafeCastId

instance IsNSView (Id NSRuleEditor) where
  toNSView = unsafeCastId

-- ---------- NSScroller ----------

-- | Phantom type for @NSScroller@.
data NSScroller

instance IsObjCObject (Id NSScroller) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSScroller"

class IsNSControl a => IsNSScroller a where
  toNSScroller :: a -> Id NSScroller

instance IsNSScroller (Id NSScroller) where
  toNSScroller = unsafeCastId

instance IsNSControl (Id NSScroller) where
  toNSControl = unsafeCastId

instance IsNSObject (Id NSScroller) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSScroller) where
  toNSResponder = unsafeCastId

instance IsNSView (Id NSScroller) where
  toNSView = unsafeCastId

-- ---------- NSSegmentedControl ----------

-- | Phantom type for @NSSegmentedControl@.
data NSSegmentedControl

instance IsObjCObject (Id NSSegmentedControl) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSegmentedControl"

class IsNSControl a => IsNSSegmentedControl a where
  toNSSegmentedControl :: a -> Id NSSegmentedControl

instance IsNSSegmentedControl (Id NSSegmentedControl) where
  toNSSegmentedControl = unsafeCastId

instance IsNSControl (Id NSSegmentedControl) where
  toNSControl = unsafeCastId

instance IsNSObject (Id NSSegmentedControl) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSSegmentedControl) where
  toNSResponder = unsafeCastId

instance IsNSView (Id NSSegmentedControl) where
  toNSView = unsafeCastId

-- ---------- NSSlider ----------

-- | Phantom type for @NSSlider@.
data NSSlider

instance IsObjCObject (Id NSSlider) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSlider"

class IsNSControl a => IsNSSlider a where
  toNSSlider :: a -> Id NSSlider

instance IsNSSlider (Id NSSlider) where
  toNSSlider = unsafeCastId

instance IsNSControl (Id NSSlider) where
  toNSControl = unsafeCastId

instance IsNSObject (Id NSSlider) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSSlider) where
  toNSResponder = unsafeCastId

instance IsNSView (Id NSSlider) where
  toNSView = unsafeCastId

-- ---------- NSStepper ----------

-- | Phantom type for @NSStepper@.
data NSStepper

instance IsObjCObject (Id NSStepper) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSStepper"

class IsNSControl a => IsNSStepper a where
  toNSStepper :: a -> Id NSStepper

instance IsNSStepper (Id NSStepper) where
  toNSStepper = unsafeCastId

instance IsNSControl (Id NSStepper) where
  toNSControl = unsafeCastId

instance IsNSObject (Id NSStepper) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSStepper) where
  toNSResponder = unsafeCastId

instance IsNSView (Id NSStepper) where
  toNSView = unsafeCastId

-- ---------- NSSwitch ----------

-- | Phantom type for @NSSwitch@.
data NSSwitch

instance IsObjCObject (Id NSSwitch) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSwitch"

class IsNSControl a => IsNSSwitch a where
  toNSSwitch :: a -> Id NSSwitch

instance IsNSSwitch (Id NSSwitch) where
  toNSSwitch = unsafeCastId

instance IsNSControl (Id NSSwitch) where
  toNSControl = unsafeCastId

instance IsNSObject (Id NSSwitch) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSSwitch) where
  toNSResponder = unsafeCastId

instance IsNSView (Id NSSwitch) where
  toNSView = unsafeCastId

-- ---------- NSTableView ----------

-- | Phantom type for @NSTableView@.
data NSTableView

instance IsObjCObject (Id NSTableView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTableView"

class IsNSControl a => IsNSTableView a where
  toNSTableView :: a -> Id NSTableView

instance IsNSTableView (Id NSTableView) where
  toNSTableView = unsafeCastId

instance IsNSControl (Id NSTableView) where
  toNSControl = unsafeCastId

instance IsNSObject (Id NSTableView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSTableView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id NSTableView) where
  toNSView = unsafeCastId

-- ---------- NSTextField ----------

-- | Phantom type for @NSTextField@.
data NSTextField

instance IsObjCObject (Id NSTextField) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTextField"

class IsNSControl a => IsNSTextField a where
  toNSTextField :: a -> Id NSTextField

instance IsNSTextField (Id NSTextField) where
  toNSTextField = unsafeCastId

instance IsNSControl (Id NSTextField) where
  toNSControl = unsafeCastId

instance IsNSObject (Id NSTextField) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSTextField) where
  toNSResponder = unsafeCastId

instance IsNSView (Id NSTextField) where
  toNSView = unsafeCastId

-- ---------- NSScrubberItemView ----------

-- | NSScrubberItemView
--
-- The base view class that is arranged by a @NSScrubber@ control.
-- 
-- Phantom type for @NSScrubberItemView@.
data NSScrubberItemView

instance IsObjCObject (Id NSScrubberItemView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSScrubberItemView"

class IsNSScrubberArrangedView a => IsNSScrubberItemView a where
  toNSScrubberItemView :: a -> Id NSScrubberItemView

instance IsNSScrubberItemView (Id NSScrubberItemView) where
  toNSScrubberItemView = unsafeCastId

instance IsNSObject (Id NSScrubberItemView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSScrubberItemView) where
  toNSResponder = unsafeCastId

instance IsNSScrubberArrangedView (Id NSScrubberItemView) where
  toNSScrubberArrangedView = unsafeCastId

instance IsNSView (Id NSScrubberItemView) where
  toNSView = unsafeCastId

-- ---------- NSScrubberSelectionView ----------

-- | NSScrubberSelectionView
--
-- The base view class for all selection decorations used by the @NSScrubber@ control.
-- 
-- Phantom type for @NSScrubberSelectionView@.
data NSScrubberSelectionView

instance IsObjCObject (Id NSScrubberSelectionView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSScrubberSelectionView"

class IsNSScrubberArrangedView a => IsNSScrubberSelectionView a where
  toNSScrubberSelectionView :: a -> Id NSScrubberSelectionView

instance IsNSScrubberSelectionView (Id NSScrubberSelectionView) where
  toNSScrubberSelectionView = unsafeCastId

instance IsNSObject (Id NSScrubberSelectionView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSScrubberSelectionView) where
  toNSResponder = unsafeCastId

instance IsNSScrubberArrangedView (Id NSScrubberSelectionView) where
  toNSScrubberArrangedView = unsafeCastId

instance IsNSView (Id NSScrubberSelectionView) where
  toNSView = unsafeCastId

-- ---------- NSTextView ----------

-- | Phantom type for @NSTextView@.
data NSTextView

instance IsObjCObject (Id NSTextView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTextView"

class IsNSText a => IsNSTextView a where
  toNSTextView :: a -> Id NSTextView

instance IsNSTextView (Id NSTextView) where
  toNSTextView = unsafeCastId

instance IsNSObject (Id NSTextView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSTextView) where
  toNSResponder = unsafeCastId

instance IsNSText (Id NSTextView) where
  toNSText = unsafeCastId

instance IsNSView (Id NSTextView) where
  toNSView = unsafeCastId

-- ---------- NSColorPanel ----------

-- | Phantom type for @NSColorPanel@.
data NSColorPanel

instance IsObjCObject (Id NSColorPanel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSColorPanel"

class IsNSPanel a => IsNSColorPanel a where
  toNSColorPanel :: a -> Id NSColorPanel

instance IsNSColorPanel (Id NSColorPanel) where
  toNSColorPanel = unsafeCastId

instance IsNSObject (Id NSColorPanel) where
  toNSObject = unsafeCastId

instance IsNSPanel (Id NSColorPanel) where
  toNSPanel = unsafeCastId

instance IsNSResponder (Id NSColorPanel) where
  toNSResponder = unsafeCastId

instance IsNSWindow (Id NSColorPanel) where
  toNSWindow = unsafeCastId

-- ---------- NSFontPanel ----------

-- | Phantom type for @NSFontPanel@.
data NSFontPanel

instance IsObjCObject (Id NSFontPanel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSFontPanel"

class IsNSPanel a => IsNSFontPanel a where
  toNSFontPanel :: a -> Id NSFontPanel

instance IsNSFontPanel (Id NSFontPanel) where
  toNSFontPanel = unsafeCastId

instance IsNSObject (Id NSFontPanel) where
  toNSObject = unsafeCastId

instance IsNSPanel (Id NSFontPanel) where
  toNSPanel = unsafeCastId

instance IsNSResponder (Id NSFontPanel) where
  toNSResponder = unsafeCastId

instance IsNSWindow (Id NSFontPanel) where
  toNSWindow = unsafeCastId

-- ---------- NSSavePanel ----------

-- | Phantom type for @NSSavePanel@.
data NSSavePanel

instance IsObjCObject (Id NSSavePanel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSavePanel"

class IsNSPanel a => IsNSSavePanel a where
  toNSSavePanel :: a -> Id NSSavePanel

instance IsNSSavePanel (Id NSSavePanel) where
  toNSSavePanel = unsafeCastId

instance IsNSObject (Id NSSavePanel) where
  toNSObject = unsafeCastId

instance IsNSPanel (Id NSSavePanel) where
  toNSPanel = unsafeCastId

instance IsNSResponder (Id NSSavePanel) where
  toNSResponder = unsafeCastId

instance IsNSWindow (Id NSSavePanel) where
  toNSWindow = unsafeCastId

-- ---------- NSPopUpButtonCell ----------

-- | Phantom type for @NSPopUpButtonCell@.
data NSPopUpButtonCell

instance IsObjCObject (Id NSPopUpButtonCell) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPopUpButtonCell"

class IsNSMenuItemCell a => IsNSPopUpButtonCell a where
  toNSPopUpButtonCell :: a -> Id NSPopUpButtonCell

instance IsNSPopUpButtonCell (Id NSPopUpButtonCell) where
  toNSPopUpButtonCell = unsafeCastId

instance IsNSActionCell (Id NSPopUpButtonCell) where
  toNSActionCell = unsafeCastId

instance IsNSButtonCell (Id NSPopUpButtonCell) where
  toNSButtonCell = unsafeCastId

instance IsNSCell (Id NSPopUpButtonCell) where
  toNSCell = unsafeCastId

instance IsNSMenuItemCell (Id NSPopUpButtonCell) where
  toNSMenuItemCell = unsafeCastId

instance IsNSObject (Id NSPopUpButtonCell) where
  toNSObject = unsafeCastId

-- ---------- NSPopUpButton ----------

-- | Phantom type for @NSPopUpButton@.
data NSPopUpButton

instance IsObjCObject (Id NSPopUpButton) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPopUpButton"

class IsNSButton a => IsNSPopUpButton a where
  toNSPopUpButton :: a -> Id NSPopUpButton

instance IsNSPopUpButton (Id NSPopUpButton) where
  toNSPopUpButton = unsafeCastId

instance IsNSButton (Id NSPopUpButton) where
  toNSButton = unsafeCastId

instance IsNSControl (Id NSPopUpButton) where
  toNSControl = unsafeCastId

instance IsNSObject (Id NSPopUpButton) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSPopUpButton) where
  toNSResponder = unsafeCastId

instance IsNSView (Id NSPopUpButton) where
  toNSView = unsafeCastId

-- ---------- NSStatusBarButton ----------

-- | Status bar buttons are the visual representation of @NSStatusItem@s, and are primarily displayed on the right side of the menu bar. When a template image is set as the @image@ property of the status bar button, it is rendered with the correct menu bar style. This guarantees that the button will look correct in various button states and appearances (such as dark menu bar).
-- 
-- Phantom type for @NSStatusBarButton@.
data NSStatusBarButton

instance IsObjCObject (Id NSStatusBarButton) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSStatusBarButton"

class IsNSButton a => IsNSStatusBarButton a where
  toNSStatusBarButton :: a -> Id NSStatusBarButton

instance IsNSStatusBarButton (Id NSStatusBarButton) where
  toNSStatusBarButton = unsafeCastId

instance IsNSButton (Id NSStatusBarButton) where
  toNSButton = unsafeCastId

instance IsNSControl (Id NSStatusBarButton) where
  toNSControl = unsafeCastId

instance IsNSObject (Id NSStatusBarButton) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSStatusBarButton) where
  toNSResponder = unsafeCastId

instance IsNSView (Id NSStatusBarButton) where
  toNSView = unsafeCastId

-- ---------- NSForm ----------

-- | Phantom type for @NSForm@.
data NSForm

instance IsObjCObject (Id NSForm) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSForm"

class IsNSMatrix a => IsNSForm a where
  toNSForm :: a -> Id NSForm

instance IsNSForm (Id NSForm) where
  toNSForm = unsafeCastId

instance IsNSControl (Id NSForm) where
  toNSControl = unsafeCastId

instance IsNSMatrix (Id NSForm) where
  toNSMatrix = unsafeCastId

instance IsNSObject (Id NSForm) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSForm) where
  toNSResponder = unsafeCastId

instance IsNSView (Id NSForm) where
  toNSView = unsafeCastId

-- ---------- NSPredicateEditor ----------

-- | Phantom type for @NSPredicateEditor@.
data NSPredicateEditor

instance IsObjCObject (Id NSPredicateEditor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPredicateEditor"

class IsNSRuleEditor a => IsNSPredicateEditor a where
  toNSPredicateEditor :: a -> Id NSPredicateEditor

instance IsNSPredicateEditor (Id NSPredicateEditor) where
  toNSPredicateEditor = unsafeCastId

instance IsNSControl (Id NSPredicateEditor) where
  toNSControl = unsafeCastId

instance IsNSObject (Id NSPredicateEditor) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSPredicateEditor) where
  toNSResponder = unsafeCastId

instance IsNSRuleEditor (Id NSPredicateEditor) where
  toNSRuleEditor = unsafeCastId

instance IsNSView (Id NSPredicateEditor) where
  toNSView = unsafeCastId

-- ---------- NSOutlineView ----------

-- | Phantom type for @NSOutlineView@.
data NSOutlineView

instance IsObjCObject (Id NSOutlineView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSOutlineView"

class IsNSTableView a => IsNSOutlineView a where
  toNSOutlineView :: a -> Id NSOutlineView

instance IsNSOutlineView (Id NSOutlineView) where
  toNSOutlineView = unsafeCastId

instance IsNSControl (Id NSOutlineView) where
  toNSControl = unsafeCastId

instance IsNSObject (Id NSOutlineView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSOutlineView) where
  toNSResponder = unsafeCastId

instance IsNSTableView (Id NSOutlineView) where
  toNSTableView = unsafeCastId

instance IsNSView (Id NSOutlineView) where
  toNSView = unsafeCastId

-- ---------- NSComboBox ----------

-- | Phantom type for @NSComboBox@.
data NSComboBox

instance IsObjCObject (Id NSComboBox) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSComboBox"

class IsNSTextField a => IsNSComboBox a where
  toNSComboBox :: a -> Id NSComboBox

instance IsNSComboBox (Id NSComboBox) where
  toNSComboBox = unsafeCastId

instance IsNSControl (Id NSComboBox) where
  toNSControl = unsafeCastId

instance IsNSObject (Id NSComboBox) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSComboBox) where
  toNSResponder = unsafeCastId

instance IsNSTextField (Id NSComboBox) where
  toNSTextField = unsafeCastId

instance IsNSView (Id NSComboBox) where
  toNSView = unsafeCastId

-- ---------- NSSearchField ----------

-- | Phantom type for @NSSearchField@.
data NSSearchField

instance IsObjCObject (Id NSSearchField) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSearchField"

class IsNSTextField a => IsNSSearchField a where
  toNSSearchField :: a -> Id NSSearchField

instance IsNSSearchField (Id NSSearchField) where
  toNSSearchField = unsafeCastId

instance IsNSControl (Id NSSearchField) where
  toNSControl = unsafeCastId

instance IsNSObject (Id NSSearchField) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSSearchField) where
  toNSResponder = unsafeCastId

instance IsNSTextField (Id NSSearchField) where
  toNSTextField = unsafeCastId

instance IsNSView (Id NSSearchField) where
  toNSView = unsafeCastId

-- ---------- NSSecureTextField ----------

-- | Phantom type for @NSSecureTextField@.
data NSSecureTextField

instance IsObjCObject (Id NSSecureTextField) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSecureTextField"

class IsNSTextField a => IsNSSecureTextField a where
  toNSSecureTextField :: a -> Id NSSecureTextField

instance IsNSSecureTextField (Id NSSecureTextField) where
  toNSSecureTextField = unsafeCastId

instance IsNSControl (Id NSSecureTextField) where
  toNSControl = unsafeCastId

instance IsNSObject (Id NSSecureTextField) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSSecureTextField) where
  toNSResponder = unsafeCastId

instance IsNSTextField (Id NSSecureTextField) where
  toNSTextField = unsafeCastId

instance IsNSView (Id NSSecureTextField) where
  toNSView = unsafeCastId

-- ---------- NSTokenField ----------

-- | Phantom type for @NSTokenField@.
data NSTokenField

instance IsObjCObject (Id NSTokenField) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSTokenField"

class IsNSTextField a => IsNSTokenField a where
  toNSTokenField :: a -> Id NSTokenField

instance IsNSTokenField (Id NSTokenField) where
  toNSTokenField = unsafeCastId

instance IsNSControl (Id NSTokenField) where
  toNSControl = unsafeCastId

instance IsNSObject (Id NSTokenField) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSTokenField) where
  toNSResponder = unsafeCastId

instance IsNSTextField (Id NSTokenField) where
  toNSTextField = unsafeCastId

instance IsNSView (Id NSTokenField) where
  toNSView = unsafeCastId

-- ---------- NSScrubberImageItemView ----------

-- | NSScrubberTextItemView
--
-- A simple @NSScrubberItemView@ for displaying an image.
--
-- If the provided image is larger than the view's frame, it is scaled proportionally to fill the entire frame. The cropped portion of the image is determined by the @imageAlignment@ property.
-- 
-- Phantom type for @NSScrubberImageItemView@.
data NSScrubberImageItemView

instance IsObjCObject (Id NSScrubberImageItemView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSScrubberImageItemView"

class IsNSScrubberItemView a => IsNSScrubberImageItemView a where
  toNSScrubberImageItemView :: a -> Id NSScrubberImageItemView

instance IsNSScrubberImageItemView (Id NSScrubberImageItemView) where
  toNSScrubberImageItemView = unsafeCastId

instance IsNSObject (Id NSScrubberImageItemView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSScrubberImageItemView) where
  toNSResponder = unsafeCastId

instance IsNSScrubberArrangedView (Id NSScrubberImageItemView) where
  toNSScrubberArrangedView = unsafeCastId

instance IsNSScrubberItemView (Id NSScrubberImageItemView) where
  toNSScrubberItemView = unsafeCastId

instance IsNSView (Id NSScrubberImageItemView) where
  toNSView = unsafeCastId

-- ---------- NSScrubberTextItemView ----------

-- | NSScrubberTextItemView
--
-- A simple @NSScrubberItemView@ for displaying text. The -fittingSize method can be used to measure the smallest size for the view which fits the title without truncating.
-- 
-- Phantom type for @NSScrubberTextItemView@.
data NSScrubberTextItemView

instance IsObjCObject (Id NSScrubberTextItemView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSScrubberTextItemView"

class IsNSScrubberItemView a => IsNSScrubberTextItemView a where
  toNSScrubberTextItemView :: a -> Id NSScrubberTextItemView

instance IsNSScrubberTextItemView (Id NSScrubberTextItemView) where
  toNSScrubberTextItemView = unsafeCastId

instance IsNSObject (Id NSScrubberTextItemView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NSScrubberTextItemView) where
  toNSResponder = unsafeCastId

instance IsNSScrubberArrangedView (Id NSScrubberTextItemView) where
  toNSScrubberArrangedView = unsafeCastId

instance IsNSScrubberItemView (Id NSScrubberTextItemView) where
  toNSScrubberItemView = unsafeCastId

instance IsNSView (Id NSScrubberTextItemView) where
  toNSView = unsafeCastId

-- ---------- NSOpenPanel ----------

-- | Phantom type for @NSOpenPanel@.
data NSOpenPanel

instance IsObjCObject (Id NSOpenPanel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSOpenPanel"

class IsNSSavePanel a => IsNSOpenPanel a where
  toNSOpenPanel :: a -> Id NSOpenPanel

instance IsNSOpenPanel (Id NSOpenPanel) where
  toNSOpenPanel = unsafeCastId

instance IsNSObject (Id NSOpenPanel) where
  toNSObject = unsafeCastId

instance IsNSPanel (Id NSOpenPanel) where
  toNSPanel = unsafeCastId

instance IsNSResponder (Id NSOpenPanel) where
  toNSResponder = unsafeCastId

instance IsNSSavePanel (Id NSOpenPanel) where
  toNSSavePanel = unsafeCastId

instance IsNSWindow (Id NSOpenPanel) where
  toNSWindow = unsafeCastId
