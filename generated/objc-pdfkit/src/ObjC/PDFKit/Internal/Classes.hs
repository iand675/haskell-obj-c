{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.PDFKit.Internal.Classes (
    module ObjC.PDFKit.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- PDFAction ----------

-- | Phantom type for @PDFAction@.
data PDFAction

instance IsObjCObject (Id PDFAction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PDFAction"

class IsNSObject a => IsPDFAction a where
  toPDFAction :: a -> Id PDFAction

instance IsPDFAction (Id PDFAction) where
  toPDFAction = unsafeCastId

instance IsNSObject (Id PDFAction) where
  toNSObject = unsafeCastId

-- ---------- PDFAnnotation ----------

-- | Phantom type for @PDFAnnotation@.
data PDFAnnotation

instance IsObjCObject (Id PDFAnnotation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PDFAnnotation"

class IsNSObject a => IsPDFAnnotation a where
  toPDFAnnotation :: a -> Id PDFAnnotation

instance IsPDFAnnotation (Id PDFAnnotation) where
  toPDFAnnotation = unsafeCastId

instance IsNSObject (Id PDFAnnotation) where
  toNSObject = unsafeCastId

-- ---------- PDFAppearanceCharacteristics ----------

-- | Phantom type for @PDFAppearanceCharacteristics@.
data PDFAppearanceCharacteristics

instance IsObjCObject (Id PDFAppearanceCharacteristics) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PDFAppearanceCharacteristics"

class IsNSObject a => IsPDFAppearanceCharacteristics a where
  toPDFAppearanceCharacteristics :: a -> Id PDFAppearanceCharacteristics

instance IsPDFAppearanceCharacteristics (Id PDFAppearanceCharacteristics) where
  toPDFAppearanceCharacteristics = unsafeCastId

instance IsNSObject (Id PDFAppearanceCharacteristics) where
  toNSObject = unsafeCastId

-- ---------- PDFBorder ----------

-- | Phantom type for @PDFBorder@.
data PDFBorder

instance IsObjCObject (Id PDFBorder) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PDFBorder"

class IsNSObject a => IsPDFBorder a where
  toPDFBorder :: a -> Id PDFBorder

instance IsPDFBorder (Id PDFBorder) where
  toPDFBorder = unsafeCastId

instance IsNSObject (Id PDFBorder) where
  toNSObject = unsafeCastId

-- ---------- PDFDestination ----------

-- | Phantom type for @PDFDestination@.
data PDFDestination

instance IsObjCObject (Id PDFDestination) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PDFDestination"

class IsNSObject a => IsPDFDestination a where
  toPDFDestination :: a -> Id PDFDestination

instance IsPDFDestination (Id PDFDestination) where
  toPDFDestination = unsafeCastId

instance IsNSObject (Id PDFDestination) where
  toNSObject = unsafeCastId

-- ---------- PDFDocument ----------

-- | Phantom type for @PDFDocument@.
data PDFDocument

instance IsObjCObject (Id PDFDocument) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PDFDocument"

class IsNSObject a => IsPDFDocument a where
  toPDFDocument :: a -> Id PDFDocument

instance IsPDFDocument (Id PDFDocument) where
  toPDFDocument = unsafeCastId

instance IsNSObject (Id PDFDocument) where
  toNSObject = unsafeCastId

-- ---------- PDFOutline ----------

-- | Phantom type for @PDFOutline@.
data PDFOutline

instance IsObjCObject (Id PDFOutline) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PDFOutline"

class IsNSObject a => IsPDFOutline a where
  toPDFOutline :: a -> Id PDFOutline

instance IsPDFOutline (Id PDFOutline) where
  toPDFOutline = unsafeCastId

instance IsNSObject (Id PDFOutline) where
  toNSObject = unsafeCastId

-- ---------- PDFPage ----------

-- | Phantom type for @PDFPage@.
data PDFPage

instance IsObjCObject (Id PDFPage) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PDFPage"

class IsNSObject a => IsPDFPage a where
  toPDFPage :: a -> Id PDFPage

instance IsPDFPage (Id PDFPage) where
  toPDFPage = unsafeCastId

instance IsNSObject (Id PDFPage) where
  toNSObject = unsafeCastId

-- ---------- PDFSelection ----------

-- | Phantom type for @PDFSelection@.
data PDFSelection

instance IsObjCObject (Id PDFSelection) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PDFSelection"

class IsNSObject a => IsPDFSelection a where
  toPDFSelection :: a -> Id PDFSelection

instance IsPDFSelection (Id PDFSelection) where
  toPDFSelection = unsafeCastId

instance IsNSObject (Id PDFSelection) where
  toNSObject = unsafeCastId

-- ---------- PDFActionGoTo ----------

-- | Phantom type for @PDFActionGoTo@.
data PDFActionGoTo

instance IsObjCObject (Id PDFActionGoTo) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PDFActionGoTo"

class IsPDFAction a => IsPDFActionGoTo a where
  toPDFActionGoTo :: a -> Id PDFActionGoTo

instance IsPDFActionGoTo (Id PDFActionGoTo) where
  toPDFActionGoTo = unsafeCastId

instance IsNSObject (Id PDFActionGoTo) where
  toNSObject = unsafeCastId

instance IsPDFAction (Id PDFActionGoTo) where
  toPDFAction = unsafeCastId

-- ---------- PDFActionNamed ----------

-- | Phantom type for @PDFActionNamed@.
data PDFActionNamed

instance IsObjCObject (Id PDFActionNamed) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PDFActionNamed"

class IsPDFAction a => IsPDFActionNamed a where
  toPDFActionNamed :: a -> Id PDFActionNamed

instance IsPDFActionNamed (Id PDFActionNamed) where
  toPDFActionNamed = unsafeCastId

instance IsNSObject (Id PDFActionNamed) where
  toNSObject = unsafeCastId

instance IsPDFAction (Id PDFActionNamed) where
  toPDFAction = unsafeCastId

-- ---------- PDFActionRemoteGoTo ----------

-- | Phantom type for @PDFActionRemoteGoTo@.
data PDFActionRemoteGoTo

instance IsObjCObject (Id PDFActionRemoteGoTo) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PDFActionRemoteGoTo"

class IsPDFAction a => IsPDFActionRemoteGoTo a where
  toPDFActionRemoteGoTo :: a -> Id PDFActionRemoteGoTo

instance IsPDFActionRemoteGoTo (Id PDFActionRemoteGoTo) where
  toPDFActionRemoteGoTo = unsafeCastId

instance IsNSObject (Id PDFActionRemoteGoTo) where
  toNSObject = unsafeCastId

instance IsPDFAction (Id PDFActionRemoteGoTo) where
  toPDFAction = unsafeCastId

-- ---------- PDFActionResetForm ----------

-- | Phantom type for @PDFActionResetForm@.
data PDFActionResetForm

instance IsObjCObject (Id PDFActionResetForm) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PDFActionResetForm"

class IsPDFAction a => IsPDFActionResetForm a where
  toPDFActionResetForm :: a -> Id PDFActionResetForm

instance IsPDFActionResetForm (Id PDFActionResetForm) where
  toPDFActionResetForm = unsafeCastId

instance IsNSObject (Id PDFActionResetForm) where
  toNSObject = unsafeCastId

instance IsPDFAction (Id PDFActionResetForm) where
  toPDFAction = unsafeCastId

-- ---------- PDFActionURL ----------

-- | Phantom type for @PDFActionURL@.
data PDFActionURL

instance IsObjCObject (Id PDFActionURL) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PDFActionURL"

class IsPDFAction a => IsPDFActionURL a where
  toPDFActionURL :: a -> Id PDFActionURL

instance IsPDFActionURL (Id PDFActionURL) where
  toPDFActionURL = unsafeCastId

instance IsNSObject (Id PDFActionURL) where
  toNSObject = unsafeCastId

instance IsPDFAction (Id PDFActionURL) where
  toPDFAction = unsafeCastId

-- ---------- PDFAnnotationButtonWidget ----------

-- | Phantom type for @PDFAnnotationButtonWidget@.
data PDFAnnotationButtonWidget

instance IsObjCObject (Id PDFAnnotationButtonWidget) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PDFAnnotationButtonWidget"

class IsPDFAnnotation a => IsPDFAnnotationButtonWidget a where
  toPDFAnnotationButtonWidget :: a -> Id PDFAnnotationButtonWidget

instance IsPDFAnnotationButtonWidget (Id PDFAnnotationButtonWidget) where
  toPDFAnnotationButtonWidget = unsafeCastId

instance IsNSObject (Id PDFAnnotationButtonWidget) where
  toNSObject = unsafeCastId

instance IsPDFAnnotation (Id PDFAnnotationButtonWidget) where
  toPDFAnnotation = unsafeCastId

-- ---------- PDFAnnotationChoiceWidget ----------

-- | Phantom type for @PDFAnnotationChoiceWidget@.
data PDFAnnotationChoiceWidget

instance IsObjCObject (Id PDFAnnotationChoiceWidget) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PDFAnnotationChoiceWidget"

class IsPDFAnnotation a => IsPDFAnnotationChoiceWidget a where
  toPDFAnnotationChoiceWidget :: a -> Id PDFAnnotationChoiceWidget

instance IsPDFAnnotationChoiceWidget (Id PDFAnnotationChoiceWidget) where
  toPDFAnnotationChoiceWidget = unsafeCastId

instance IsNSObject (Id PDFAnnotationChoiceWidget) where
  toNSObject = unsafeCastId

instance IsPDFAnnotation (Id PDFAnnotationChoiceWidget) where
  toPDFAnnotation = unsafeCastId

-- ---------- PDFAnnotationCircle ----------

-- | Phantom type for @PDFAnnotationCircle@.
data PDFAnnotationCircle

instance IsObjCObject (Id PDFAnnotationCircle) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PDFAnnotationCircle"

class IsPDFAnnotation a => IsPDFAnnotationCircle a where
  toPDFAnnotationCircle :: a -> Id PDFAnnotationCircle

instance IsPDFAnnotationCircle (Id PDFAnnotationCircle) where
  toPDFAnnotationCircle = unsafeCastId

instance IsNSObject (Id PDFAnnotationCircle) where
  toNSObject = unsafeCastId

instance IsPDFAnnotation (Id PDFAnnotationCircle) where
  toPDFAnnotation = unsafeCastId

-- ---------- PDFAnnotationFreeText ----------

-- | Phantom type for @PDFAnnotationFreeText@.
data PDFAnnotationFreeText

instance IsObjCObject (Id PDFAnnotationFreeText) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PDFAnnotationFreeText"

class IsPDFAnnotation a => IsPDFAnnotationFreeText a where
  toPDFAnnotationFreeText :: a -> Id PDFAnnotationFreeText

instance IsPDFAnnotationFreeText (Id PDFAnnotationFreeText) where
  toPDFAnnotationFreeText = unsafeCastId

instance IsNSObject (Id PDFAnnotationFreeText) where
  toNSObject = unsafeCastId

instance IsPDFAnnotation (Id PDFAnnotationFreeText) where
  toPDFAnnotation = unsafeCastId

-- ---------- PDFAnnotationInk ----------

-- | Phantom type for @PDFAnnotationInk@.
data PDFAnnotationInk

instance IsObjCObject (Id PDFAnnotationInk) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PDFAnnotationInk"

class IsPDFAnnotation a => IsPDFAnnotationInk a where
  toPDFAnnotationInk :: a -> Id PDFAnnotationInk

instance IsPDFAnnotationInk (Id PDFAnnotationInk) where
  toPDFAnnotationInk = unsafeCastId

instance IsNSObject (Id PDFAnnotationInk) where
  toNSObject = unsafeCastId

instance IsPDFAnnotation (Id PDFAnnotationInk) where
  toPDFAnnotation = unsafeCastId

-- ---------- PDFAnnotationLine ----------

-- | Phantom type for @PDFAnnotationLine@.
data PDFAnnotationLine

instance IsObjCObject (Id PDFAnnotationLine) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PDFAnnotationLine"

class IsPDFAnnotation a => IsPDFAnnotationLine a where
  toPDFAnnotationLine :: a -> Id PDFAnnotationLine

instance IsPDFAnnotationLine (Id PDFAnnotationLine) where
  toPDFAnnotationLine = unsafeCastId

instance IsNSObject (Id PDFAnnotationLine) where
  toNSObject = unsafeCastId

instance IsPDFAnnotation (Id PDFAnnotationLine) where
  toPDFAnnotation = unsafeCastId

-- ---------- PDFAnnotationLink ----------

-- | Phantom type for @PDFAnnotationLink@.
data PDFAnnotationLink

instance IsObjCObject (Id PDFAnnotationLink) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PDFAnnotationLink"

class IsPDFAnnotation a => IsPDFAnnotationLink a where
  toPDFAnnotationLink :: a -> Id PDFAnnotationLink

instance IsPDFAnnotationLink (Id PDFAnnotationLink) where
  toPDFAnnotationLink = unsafeCastId

instance IsNSObject (Id PDFAnnotationLink) where
  toNSObject = unsafeCastId

instance IsPDFAnnotation (Id PDFAnnotationLink) where
  toPDFAnnotation = unsafeCastId

-- ---------- PDFAnnotationMarkup ----------

-- | Phantom type for @PDFAnnotationMarkup@.
data PDFAnnotationMarkup

instance IsObjCObject (Id PDFAnnotationMarkup) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PDFAnnotationMarkup"

class IsPDFAnnotation a => IsPDFAnnotationMarkup a where
  toPDFAnnotationMarkup :: a -> Id PDFAnnotationMarkup

instance IsPDFAnnotationMarkup (Id PDFAnnotationMarkup) where
  toPDFAnnotationMarkup = unsafeCastId

instance IsNSObject (Id PDFAnnotationMarkup) where
  toNSObject = unsafeCastId

instance IsPDFAnnotation (Id PDFAnnotationMarkup) where
  toPDFAnnotation = unsafeCastId

-- ---------- PDFAnnotationPopup ----------

-- | Phantom type for @PDFAnnotationPopup@.
data PDFAnnotationPopup

instance IsObjCObject (Id PDFAnnotationPopup) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PDFAnnotationPopup"

class IsPDFAnnotation a => IsPDFAnnotationPopup a where
  toPDFAnnotationPopup :: a -> Id PDFAnnotationPopup

instance IsPDFAnnotationPopup (Id PDFAnnotationPopup) where
  toPDFAnnotationPopup = unsafeCastId

instance IsNSObject (Id PDFAnnotationPopup) where
  toNSObject = unsafeCastId

instance IsPDFAnnotation (Id PDFAnnotationPopup) where
  toPDFAnnotation = unsafeCastId

-- ---------- PDFAnnotationSquare ----------

-- | Phantom type for @PDFAnnotationSquare@.
data PDFAnnotationSquare

instance IsObjCObject (Id PDFAnnotationSquare) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PDFAnnotationSquare"

class IsPDFAnnotation a => IsPDFAnnotationSquare a where
  toPDFAnnotationSquare :: a -> Id PDFAnnotationSquare

instance IsPDFAnnotationSquare (Id PDFAnnotationSquare) where
  toPDFAnnotationSquare = unsafeCastId

instance IsNSObject (Id PDFAnnotationSquare) where
  toNSObject = unsafeCastId

instance IsPDFAnnotation (Id PDFAnnotationSquare) where
  toPDFAnnotation = unsafeCastId

-- ---------- PDFAnnotationStamp ----------

-- | Phantom type for @PDFAnnotationStamp@.
data PDFAnnotationStamp

instance IsObjCObject (Id PDFAnnotationStamp) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PDFAnnotationStamp"

class IsPDFAnnotation a => IsPDFAnnotationStamp a where
  toPDFAnnotationStamp :: a -> Id PDFAnnotationStamp

instance IsPDFAnnotationStamp (Id PDFAnnotationStamp) where
  toPDFAnnotationStamp = unsafeCastId

instance IsNSObject (Id PDFAnnotationStamp) where
  toNSObject = unsafeCastId

instance IsPDFAnnotation (Id PDFAnnotationStamp) where
  toPDFAnnotation = unsafeCastId

-- ---------- PDFAnnotationText ----------

-- | Phantom type for @PDFAnnotationText@.
data PDFAnnotationText

instance IsObjCObject (Id PDFAnnotationText) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PDFAnnotationText"

class IsPDFAnnotation a => IsPDFAnnotationText a where
  toPDFAnnotationText :: a -> Id PDFAnnotationText

instance IsPDFAnnotationText (Id PDFAnnotationText) where
  toPDFAnnotationText = unsafeCastId

instance IsNSObject (Id PDFAnnotationText) where
  toNSObject = unsafeCastId

instance IsPDFAnnotation (Id PDFAnnotationText) where
  toPDFAnnotation = unsafeCastId

-- ---------- PDFAnnotationTextWidget ----------

-- | Phantom type for @PDFAnnotationTextWidget@.
data PDFAnnotationTextWidget

instance IsObjCObject (Id PDFAnnotationTextWidget) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PDFAnnotationTextWidget"

class IsPDFAnnotation a => IsPDFAnnotationTextWidget a where
  toPDFAnnotationTextWidget :: a -> Id PDFAnnotationTextWidget

instance IsPDFAnnotationTextWidget (Id PDFAnnotationTextWidget) where
  toPDFAnnotationTextWidget = unsafeCastId

instance IsNSObject (Id PDFAnnotationTextWidget) where
  toNSObject = unsafeCastId

instance IsPDFAnnotation (Id PDFAnnotationTextWidget) where
  toPDFAnnotation = unsafeCastId

-- ---------- PDFThumbnailView ----------

-- | Phantom type for @PDFThumbnailView@.
data PDFThumbnailView

instance IsObjCObject (Id PDFThumbnailView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PDFThumbnailView"

class IsNSView a => IsPDFThumbnailView a where
  toPDFThumbnailView :: a -> Id PDFThumbnailView

instance IsPDFThumbnailView (Id PDFThumbnailView) where
  toPDFThumbnailView = unsafeCastId

instance IsNSObject (Id PDFThumbnailView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id PDFThumbnailView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id PDFThumbnailView) where
  toNSView = unsafeCastId

-- ---------- PDFView ----------

-- | Phantom type for @PDFView@.
data PDFView

instance IsObjCObject (Id PDFView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PDFView"

class IsNSView a => IsPDFView a where
  toPDFView :: a -> Id PDFView

instance IsPDFView (Id PDFView) where
  toPDFView = unsafeCastId

instance IsNSObject (Id PDFView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id PDFView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id PDFView) where
  toNSView = unsafeCastId
