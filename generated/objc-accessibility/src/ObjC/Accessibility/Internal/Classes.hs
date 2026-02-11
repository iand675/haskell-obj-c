{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.Accessibility.Internal.Classes (
    module ObjC.Accessibility.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- AXBrailleMap ----------

-- | Phantom type for @AXBrailleMap@.
data AXBrailleMap

instance IsObjCObject (Id AXBrailleMap) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AXBrailleMap"

class IsNSObject a => IsAXBrailleMap a where
  toAXBrailleMap :: a -> Id AXBrailleMap

instance IsAXBrailleMap (Id AXBrailleMap) where
  toAXBrailleMap = unsafeCastId

instance IsNSObject (Id AXBrailleMap) where
  toNSObject = unsafeCastId

-- ---------- AXBrailleTable ----------

-- | A rule for translating print text to Braille, and back-translating Braille to print text.
-- 
-- Phantom type for @AXBrailleTable@.
data AXBrailleTable

instance IsObjCObject (Id AXBrailleTable) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AXBrailleTable"

class IsNSObject a => IsAXBrailleTable a where
  toAXBrailleTable :: a -> Id AXBrailleTable

instance IsAXBrailleTable (Id AXBrailleTable) where
  toAXBrailleTable = unsafeCastId

instance IsNSObject (Id AXBrailleTable) where
  toNSObject = unsafeCastId

-- ---------- AXBrailleTranslationResult ----------

-- | The result of translation or back-translation.
-- 
-- Phantom type for @AXBrailleTranslationResult@.
data AXBrailleTranslationResult

instance IsObjCObject (Id AXBrailleTranslationResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AXBrailleTranslationResult"

class IsNSObject a => IsAXBrailleTranslationResult a where
  toAXBrailleTranslationResult :: a -> Id AXBrailleTranslationResult

instance IsAXBrailleTranslationResult (Id AXBrailleTranslationResult) where
  toAXBrailleTranslationResult = unsafeCastId

instance IsNSObject (Id AXBrailleTranslationResult) where
  toNSObject = unsafeCastId

-- ---------- AXBrailleTranslator ----------

-- | Translates print text to Braille and Braille to print text according to the given Braille table.
-- 
-- Phantom type for @AXBrailleTranslator@.
data AXBrailleTranslator

instance IsObjCObject (Id AXBrailleTranslator) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AXBrailleTranslator"

class IsNSObject a => IsAXBrailleTranslator a where
  toAXBrailleTranslator :: a -> Id AXBrailleTranslator

instance IsAXBrailleTranslator (Id AXBrailleTranslator) where
  toAXBrailleTranslator = unsafeCastId

instance IsNSObject (Id AXBrailleTranslator) where
  toNSObject = unsafeCastId

-- ---------- AXCategoricalDataAxisDescriptor ----------

-- | Phantom type for @AXCategoricalDataAxisDescriptor@.
data AXCategoricalDataAxisDescriptor

instance IsObjCObject (Id AXCategoricalDataAxisDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AXCategoricalDataAxisDescriptor"

class IsNSObject a => IsAXCategoricalDataAxisDescriptor a where
  toAXCategoricalDataAxisDescriptor :: a -> Id AXCategoricalDataAxisDescriptor

instance IsAXCategoricalDataAxisDescriptor (Id AXCategoricalDataAxisDescriptor) where
  toAXCategoricalDataAxisDescriptor = unsafeCastId

instance IsNSObject (Id AXCategoricalDataAxisDescriptor) where
  toNSObject = unsafeCastId

-- ---------- AXChartDescriptor ----------

-- | The top-level descriptor object for an accessible chart.
-- 
-- Phantom type for @AXChartDescriptor@.
data AXChartDescriptor

instance IsObjCObject (Id AXChartDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AXChartDescriptor"

class IsNSObject a => IsAXChartDescriptor a where
  toAXChartDescriptor :: a -> Id AXChartDescriptor

instance IsAXChartDescriptor (Id AXChartDescriptor) where
  toAXChartDescriptor = unsafeCastId

instance IsNSObject (Id AXChartDescriptor) where
  toNSObject = unsafeCastId

-- ---------- AXCustomContent ----------

-- | Phantom type for @AXCustomContent@.
data AXCustomContent

instance IsObjCObject (Id AXCustomContent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AXCustomContent"

class IsNSObject a => IsAXCustomContent a where
  toAXCustomContent :: a -> Id AXCustomContent

instance IsAXCustomContent (Id AXCustomContent) where
  toAXCustomContent = unsafeCastId

instance IsNSObject (Id AXCustomContent) where
  toNSObject = unsafeCastId

-- ---------- AXDataPoint ----------

-- | Provides axis values for a single data point within a series.
-- 
-- Phantom type for @AXDataPoint@.
data AXDataPoint

instance IsObjCObject (Id AXDataPoint) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AXDataPoint"

class IsNSObject a => IsAXDataPoint a where
  toAXDataPoint :: a -> Id AXDataPoint

instance IsAXDataPoint (Id AXDataPoint) where
  toAXDataPoint = unsafeCastId

instance IsNSObject (Id AXDataPoint) where
  toNSObject = unsafeCastId

-- ---------- AXDataPointValue ----------

-- | Describes a single data value, either numeric or categorical. Only the @number@ property will be used for data points in a numeric axis, and only the @category@ property will be used for data points in a categorical axis.
-- 
-- Phantom type for @AXDataPointValue@.
data AXDataPointValue

instance IsObjCObject (Id AXDataPointValue) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AXDataPointValue"

class IsNSObject a => IsAXDataPointValue a where
  toAXDataPointValue :: a -> Id AXDataPointValue

instance IsAXDataPointValue (Id AXDataPointValue) where
  toAXDataPointValue = unsafeCastId

instance IsNSObject (Id AXDataPointValue) where
  toNSObject = unsafeCastId

-- ---------- AXDataSeriesDescriptor ----------

-- | Provides information about a data series. A chart may have one or many data series.
-- 
-- Phantom type for @AXDataSeriesDescriptor@.
data AXDataSeriesDescriptor

instance IsObjCObject (Id AXDataSeriesDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AXDataSeriesDescriptor"

class IsNSObject a => IsAXDataSeriesDescriptor a where
  toAXDataSeriesDescriptor :: a -> Id AXDataSeriesDescriptor

instance IsAXDataSeriesDescriptor (Id AXDataSeriesDescriptor) where
  toAXDataSeriesDescriptor = unsafeCastId

instance IsNSObject (Id AXDataSeriesDescriptor) where
  toNSObject = unsafeCastId

-- ---------- AXLiveAudioGraph ----------

-- | Phantom type for @AXLiveAudioGraph@.
data AXLiveAudioGraph

instance IsObjCObject (Id AXLiveAudioGraph) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AXLiveAudioGraph"

class IsNSObject a => IsAXLiveAudioGraph a where
  toAXLiveAudioGraph :: a -> Id AXLiveAudioGraph

instance IsAXLiveAudioGraph (Id AXLiveAudioGraph) where
  toAXLiveAudioGraph = unsafeCastId

instance IsNSObject (Id AXLiveAudioGraph) where
  toNSObject = unsafeCastId

-- ---------- AXMathExpression ----------

-- | Phantom type for @AXMathExpression@.
data AXMathExpression

instance IsObjCObject (Id AXMathExpression) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AXMathExpression"

class IsNSObject a => IsAXMathExpression a where
  toAXMathExpression :: a -> Id AXMathExpression

instance IsAXMathExpression (Id AXMathExpression) where
  toAXMathExpression = unsafeCastId

instance IsNSObject (Id AXMathExpression) where
  toNSObject = unsafeCastId

-- ---------- AXNumericDataAxisDescriptor ----------

-- | Phantom type for @AXNumericDataAxisDescriptor@.
data AXNumericDataAxisDescriptor

instance IsObjCObject (Id AXNumericDataAxisDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AXNumericDataAxisDescriptor"

class IsNSObject a => IsAXNumericDataAxisDescriptor a where
  toAXNumericDataAxisDescriptor :: a -> Id AXNumericDataAxisDescriptor

instance IsAXNumericDataAxisDescriptor (Id AXNumericDataAxisDescriptor) where
  toAXNumericDataAxisDescriptor = unsafeCastId

instance IsNSObject (Id AXNumericDataAxisDescriptor) where
  toNSObject = unsafeCastId

-- ---------- AXRequest ----------

-- | Phantom type for @AXRequest@.
data AXRequest

instance IsObjCObject (Id AXRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AXRequest"

class IsNSObject a => IsAXRequest a where
  toAXRequest :: a -> Id AXRequest

instance IsAXRequest (Id AXRequest) where
  toAXRequest = unsafeCastId

instance IsNSObject (Id AXRequest) where
  toNSObject = unsafeCastId

-- ---------- AXMathExpressionFenced ----------

-- | Phantom type for @AXMathExpressionFenced@.
data AXMathExpressionFenced

instance IsObjCObject (Id AXMathExpressionFenced) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AXMathExpressionFenced"

class IsAXMathExpression a => IsAXMathExpressionFenced a where
  toAXMathExpressionFenced :: a -> Id AXMathExpressionFenced

instance IsAXMathExpressionFenced (Id AXMathExpressionFenced) where
  toAXMathExpressionFenced = unsafeCastId

instance IsAXMathExpression (Id AXMathExpressionFenced) where
  toAXMathExpression = unsafeCastId

instance IsNSObject (Id AXMathExpressionFenced) where
  toNSObject = unsafeCastId

-- ---------- AXMathExpressionFraction ----------

-- | Phantom type for @AXMathExpressionFraction@.
data AXMathExpressionFraction

instance IsObjCObject (Id AXMathExpressionFraction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AXMathExpressionFraction"

class IsAXMathExpression a => IsAXMathExpressionFraction a where
  toAXMathExpressionFraction :: a -> Id AXMathExpressionFraction

instance IsAXMathExpressionFraction (Id AXMathExpressionFraction) where
  toAXMathExpressionFraction = unsafeCastId

instance IsAXMathExpression (Id AXMathExpressionFraction) where
  toAXMathExpression = unsafeCastId

instance IsNSObject (Id AXMathExpressionFraction) where
  toNSObject = unsafeCastId

-- ---------- AXMathExpressionIdentifier ----------

-- | Phantom type for @AXMathExpressionIdentifier@.
data AXMathExpressionIdentifier

instance IsObjCObject (Id AXMathExpressionIdentifier) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AXMathExpressionIdentifier"

class IsAXMathExpression a => IsAXMathExpressionIdentifier a where
  toAXMathExpressionIdentifier :: a -> Id AXMathExpressionIdentifier

instance IsAXMathExpressionIdentifier (Id AXMathExpressionIdentifier) where
  toAXMathExpressionIdentifier = unsafeCastId

instance IsAXMathExpression (Id AXMathExpressionIdentifier) where
  toAXMathExpression = unsafeCastId

instance IsNSObject (Id AXMathExpressionIdentifier) where
  toNSObject = unsafeCastId

-- ---------- AXMathExpressionMultiscript ----------

-- | Phantom type for @AXMathExpressionMultiscript@.
data AXMathExpressionMultiscript

instance IsObjCObject (Id AXMathExpressionMultiscript) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AXMathExpressionMultiscript"

class IsAXMathExpression a => IsAXMathExpressionMultiscript a where
  toAXMathExpressionMultiscript :: a -> Id AXMathExpressionMultiscript

instance IsAXMathExpressionMultiscript (Id AXMathExpressionMultiscript) where
  toAXMathExpressionMultiscript = unsafeCastId

instance IsAXMathExpression (Id AXMathExpressionMultiscript) where
  toAXMathExpression = unsafeCastId

instance IsNSObject (Id AXMathExpressionMultiscript) where
  toNSObject = unsafeCastId

-- ---------- AXMathExpressionNumber ----------

-- | Phantom type for @AXMathExpressionNumber@.
data AXMathExpressionNumber

instance IsObjCObject (Id AXMathExpressionNumber) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AXMathExpressionNumber"

class IsAXMathExpression a => IsAXMathExpressionNumber a where
  toAXMathExpressionNumber :: a -> Id AXMathExpressionNumber

instance IsAXMathExpressionNumber (Id AXMathExpressionNumber) where
  toAXMathExpressionNumber = unsafeCastId

instance IsAXMathExpression (Id AXMathExpressionNumber) where
  toAXMathExpression = unsafeCastId

instance IsNSObject (Id AXMathExpressionNumber) where
  toNSObject = unsafeCastId

-- ---------- AXMathExpressionOperator ----------

-- | Phantom type for @AXMathExpressionOperator@.
data AXMathExpressionOperator

instance IsObjCObject (Id AXMathExpressionOperator) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AXMathExpressionOperator"

class IsAXMathExpression a => IsAXMathExpressionOperator a where
  toAXMathExpressionOperator :: a -> Id AXMathExpressionOperator

instance IsAXMathExpressionOperator (Id AXMathExpressionOperator) where
  toAXMathExpressionOperator = unsafeCastId

instance IsAXMathExpression (Id AXMathExpressionOperator) where
  toAXMathExpression = unsafeCastId

instance IsNSObject (Id AXMathExpressionOperator) where
  toNSObject = unsafeCastId

-- ---------- AXMathExpressionRoot ----------

-- | Phantom type for @AXMathExpressionRoot@.
data AXMathExpressionRoot

instance IsObjCObject (Id AXMathExpressionRoot) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AXMathExpressionRoot"

class IsAXMathExpression a => IsAXMathExpressionRoot a where
  toAXMathExpressionRoot :: a -> Id AXMathExpressionRoot

instance IsAXMathExpressionRoot (Id AXMathExpressionRoot) where
  toAXMathExpressionRoot = unsafeCastId

instance IsAXMathExpression (Id AXMathExpressionRoot) where
  toAXMathExpression = unsafeCastId

instance IsNSObject (Id AXMathExpressionRoot) where
  toNSObject = unsafeCastId

-- ---------- AXMathExpressionRow ----------

-- | Phantom type for @AXMathExpressionRow@.
data AXMathExpressionRow

instance IsObjCObject (Id AXMathExpressionRow) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AXMathExpressionRow"

class IsAXMathExpression a => IsAXMathExpressionRow a where
  toAXMathExpressionRow :: a -> Id AXMathExpressionRow

instance IsAXMathExpressionRow (Id AXMathExpressionRow) where
  toAXMathExpressionRow = unsafeCastId

instance IsAXMathExpression (Id AXMathExpressionRow) where
  toAXMathExpression = unsafeCastId

instance IsNSObject (Id AXMathExpressionRow) where
  toNSObject = unsafeCastId

-- ---------- AXMathExpressionSubSuperscript ----------

-- | Phantom type for @AXMathExpressionSubSuperscript@.
data AXMathExpressionSubSuperscript

instance IsObjCObject (Id AXMathExpressionSubSuperscript) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AXMathExpressionSubSuperscript"

class IsAXMathExpression a => IsAXMathExpressionSubSuperscript a where
  toAXMathExpressionSubSuperscript :: a -> Id AXMathExpressionSubSuperscript

instance IsAXMathExpressionSubSuperscript (Id AXMathExpressionSubSuperscript) where
  toAXMathExpressionSubSuperscript = unsafeCastId

instance IsAXMathExpression (Id AXMathExpressionSubSuperscript) where
  toAXMathExpression = unsafeCastId

instance IsNSObject (Id AXMathExpressionSubSuperscript) where
  toNSObject = unsafeCastId

-- ---------- AXMathExpressionTable ----------

-- | Phantom type for @AXMathExpressionTable@.
data AXMathExpressionTable

instance IsObjCObject (Id AXMathExpressionTable) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AXMathExpressionTable"

class IsAXMathExpression a => IsAXMathExpressionTable a where
  toAXMathExpressionTable :: a -> Id AXMathExpressionTable

instance IsAXMathExpressionTable (Id AXMathExpressionTable) where
  toAXMathExpressionTable = unsafeCastId

instance IsAXMathExpression (Id AXMathExpressionTable) where
  toAXMathExpression = unsafeCastId

instance IsNSObject (Id AXMathExpressionTable) where
  toNSObject = unsafeCastId

-- ---------- AXMathExpressionTableCell ----------

-- | Phantom type for @AXMathExpressionTableCell@.
data AXMathExpressionTableCell

instance IsObjCObject (Id AXMathExpressionTableCell) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AXMathExpressionTableCell"

class IsAXMathExpression a => IsAXMathExpressionTableCell a where
  toAXMathExpressionTableCell :: a -> Id AXMathExpressionTableCell

instance IsAXMathExpressionTableCell (Id AXMathExpressionTableCell) where
  toAXMathExpressionTableCell = unsafeCastId

instance IsAXMathExpression (Id AXMathExpressionTableCell) where
  toAXMathExpression = unsafeCastId

instance IsNSObject (Id AXMathExpressionTableCell) where
  toNSObject = unsafeCastId

-- ---------- AXMathExpressionTableRow ----------

-- | Phantom type for @AXMathExpressionTableRow@.
data AXMathExpressionTableRow

instance IsObjCObject (Id AXMathExpressionTableRow) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AXMathExpressionTableRow"

class IsAXMathExpression a => IsAXMathExpressionTableRow a where
  toAXMathExpressionTableRow :: a -> Id AXMathExpressionTableRow

instance IsAXMathExpressionTableRow (Id AXMathExpressionTableRow) where
  toAXMathExpressionTableRow = unsafeCastId

instance IsAXMathExpression (Id AXMathExpressionTableRow) where
  toAXMathExpression = unsafeCastId

instance IsNSObject (Id AXMathExpressionTableRow) where
  toNSObject = unsafeCastId

-- ---------- AXMathExpressionText ----------

-- | Phantom type for @AXMathExpressionText@.
data AXMathExpressionText

instance IsObjCObject (Id AXMathExpressionText) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AXMathExpressionText"

class IsAXMathExpression a => IsAXMathExpressionText a where
  toAXMathExpressionText :: a -> Id AXMathExpressionText

instance IsAXMathExpressionText (Id AXMathExpressionText) where
  toAXMathExpressionText = unsafeCastId

instance IsAXMathExpression (Id AXMathExpressionText) where
  toAXMathExpression = unsafeCastId

instance IsNSObject (Id AXMathExpressionText) where
  toNSObject = unsafeCastId

-- ---------- AXMathExpressionUnderOver ----------

-- | Phantom type for @AXMathExpressionUnderOver@.
data AXMathExpressionUnderOver

instance IsObjCObject (Id AXMathExpressionUnderOver) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AXMathExpressionUnderOver"

class IsAXMathExpression a => IsAXMathExpressionUnderOver a where
  toAXMathExpressionUnderOver :: a -> Id AXMathExpressionUnderOver

instance IsAXMathExpressionUnderOver (Id AXMathExpressionUnderOver) where
  toAXMathExpressionUnderOver = unsafeCastId

instance IsAXMathExpression (Id AXMathExpressionUnderOver) where
  toAXMathExpression = unsafeCastId

instance IsNSObject (Id AXMathExpressionUnderOver) where
  toNSObject = unsafeCastId
