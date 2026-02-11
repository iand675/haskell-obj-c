{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPresentationIntent@.
module ObjC.Foundation.NSPresentationIntent
  ( NSPresentationIntent
  , IsNSPresentationIntent(..)
  , init_
  , paragraphIntentWithIdentity_nestedInsideIntent
  , headerIntentWithIdentity_level_nestedInsideIntent
  , codeBlockIntentWithIdentity_languageHint_nestedInsideIntent
  , thematicBreakIntentWithIdentity_nestedInsideIntent
  , orderedListIntentWithIdentity_nestedInsideIntent
  , unorderedListIntentWithIdentity_nestedInsideIntent
  , listItemIntentWithIdentity_ordinal_nestedInsideIntent
  , blockQuoteIntentWithIdentity_nestedInsideIntent
  , tableIntentWithIdentity_columnCount_alignments_nestedInsideIntent
  , tableHeaderRowIntentWithIdentity_nestedInsideIntent
  , tableRowIntentWithIdentity_row_nestedInsideIntent
  , tableCellIntentWithIdentity_column_nestedInsideIntent
  , isEquivalentToPresentationIntent
  , intentKind
  , parentIntent
  , identity
  , ordinal
  , columnAlignments
  , columnCount
  , headerLevel
  , languageHint
  , column
  , row
  , indentationLevel
  , initSelector
  , paragraphIntentWithIdentity_nestedInsideIntentSelector
  , headerIntentWithIdentity_level_nestedInsideIntentSelector
  , codeBlockIntentWithIdentity_languageHint_nestedInsideIntentSelector
  , thematicBreakIntentWithIdentity_nestedInsideIntentSelector
  , orderedListIntentWithIdentity_nestedInsideIntentSelector
  , unorderedListIntentWithIdentity_nestedInsideIntentSelector
  , listItemIntentWithIdentity_ordinal_nestedInsideIntentSelector
  , blockQuoteIntentWithIdentity_nestedInsideIntentSelector
  , tableIntentWithIdentity_columnCount_alignments_nestedInsideIntentSelector
  , tableHeaderRowIntentWithIdentity_nestedInsideIntentSelector
  , tableRowIntentWithIdentity_row_nestedInsideIntentSelector
  , tableCellIntentWithIdentity_column_nestedInsideIntentSelector
  , isEquivalentToPresentationIntentSelector
  , intentKindSelector
  , parentIntentSelector
  , identitySelector
  , ordinalSelector
  , columnAlignmentsSelector
  , columnCountSelector
  , headerLevelSelector
  , languageHintSelector
  , columnSelector
  , rowSelector
  , indentationLevelSelector

  -- * Enum types
  , NSPresentationIntentKind(NSPresentationIntentKind)
  , pattern NSPresentationIntentKindParagraph
  , pattern NSPresentationIntentKindHeader
  , pattern NSPresentationIntentKindOrderedList
  , pattern NSPresentationIntentKindUnorderedList
  , pattern NSPresentationIntentKindListItem
  , pattern NSPresentationIntentKindCodeBlock
  , pattern NSPresentationIntentKindBlockQuote
  , pattern NSPresentationIntentKindThematicBreak
  , pattern NSPresentationIntentKindTable
  , pattern NSPresentationIntentKindTableHeaderRow
  , pattern NSPresentationIntentKindTableRow
  , pattern NSPresentationIntentKindTableCell

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

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- init@
init_ :: IsNSPresentationIntent nsPresentationIntent => nsPresentationIntent -> IO (Id NSPresentationIntent)
init_ nsPresentationIntent  =
  sendMsg nsPresentationIntent (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ paragraphIntentWithIdentity:nestedInsideIntent:@
paragraphIntentWithIdentity_nestedInsideIntent :: IsNSPresentationIntent parent => CLong -> parent -> IO (Id NSPresentationIntent)
paragraphIntentWithIdentity_nestedInsideIntent identity parent =
  do
    cls' <- getRequiredClass "NSPresentationIntent"
    withObjCPtr parent $ \raw_parent ->
      sendClassMsg cls' (mkSelector "paragraphIntentWithIdentity:nestedInsideIntent:") (retPtr retVoid) [argCLong (fromIntegral identity), argPtr (castPtr raw_parent :: Ptr ())] >>= retainedObject . castPtr

-- | @+ headerIntentWithIdentity:level:nestedInsideIntent:@
headerIntentWithIdentity_level_nestedInsideIntent :: IsNSPresentationIntent parent => CLong -> CLong -> parent -> IO (Id NSPresentationIntent)
headerIntentWithIdentity_level_nestedInsideIntent identity level parent =
  do
    cls' <- getRequiredClass "NSPresentationIntent"
    withObjCPtr parent $ \raw_parent ->
      sendClassMsg cls' (mkSelector "headerIntentWithIdentity:level:nestedInsideIntent:") (retPtr retVoid) [argCLong (fromIntegral identity), argCLong (fromIntegral level), argPtr (castPtr raw_parent :: Ptr ())] >>= retainedObject . castPtr

-- | @+ codeBlockIntentWithIdentity:languageHint:nestedInsideIntent:@
codeBlockIntentWithIdentity_languageHint_nestedInsideIntent :: (IsNSString languageHint, IsNSPresentationIntent parent) => CLong -> languageHint -> parent -> IO (Id NSPresentationIntent)
codeBlockIntentWithIdentity_languageHint_nestedInsideIntent identity languageHint parent =
  do
    cls' <- getRequiredClass "NSPresentationIntent"
    withObjCPtr languageHint $ \raw_languageHint ->
      withObjCPtr parent $ \raw_parent ->
        sendClassMsg cls' (mkSelector "codeBlockIntentWithIdentity:languageHint:nestedInsideIntent:") (retPtr retVoid) [argCLong (fromIntegral identity), argPtr (castPtr raw_languageHint :: Ptr ()), argPtr (castPtr raw_parent :: Ptr ())] >>= retainedObject . castPtr

-- | @+ thematicBreakIntentWithIdentity:nestedInsideIntent:@
thematicBreakIntentWithIdentity_nestedInsideIntent :: IsNSPresentationIntent parent => CLong -> parent -> IO (Id NSPresentationIntent)
thematicBreakIntentWithIdentity_nestedInsideIntent identity parent =
  do
    cls' <- getRequiredClass "NSPresentationIntent"
    withObjCPtr parent $ \raw_parent ->
      sendClassMsg cls' (mkSelector "thematicBreakIntentWithIdentity:nestedInsideIntent:") (retPtr retVoid) [argCLong (fromIntegral identity), argPtr (castPtr raw_parent :: Ptr ())] >>= retainedObject . castPtr

-- | @+ orderedListIntentWithIdentity:nestedInsideIntent:@
orderedListIntentWithIdentity_nestedInsideIntent :: IsNSPresentationIntent parent => CLong -> parent -> IO (Id NSPresentationIntent)
orderedListIntentWithIdentity_nestedInsideIntent identity parent =
  do
    cls' <- getRequiredClass "NSPresentationIntent"
    withObjCPtr parent $ \raw_parent ->
      sendClassMsg cls' (mkSelector "orderedListIntentWithIdentity:nestedInsideIntent:") (retPtr retVoid) [argCLong (fromIntegral identity), argPtr (castPtr raw_parent :: Ptr ())] >>= retainedObject . castPtr

-- | @+ unorderedListIntentWithIdentity:nestedInsideIntent:@
unorderedListIntentWithIdentity_nestedInsideIntent :: IsNSPresentationIntent parent => CLong -> parent -> IO (Id NSPresentationIntent)
unorderedListIntentWithIdentity_nestedInsideIntent identity parent =
  do
    cls' <- getRequiredClass "NSPresentationIntent"
    withObjCPtr parent $ \raw_parent ->
      sendClassMsg cls' (mkSelector "unorderedListIntentWithIdentity:nestedInsideIntent:") (retPtr retVoid) [argCLong (fromIntegral identity), argPtr (castPtr raw_parent :: Ptr ())] >>= retainedObject . castPtr

-- | @+ listItemIntentWithIdentity:ordinal:nestedInsideIntent:@
listItemIntentWithIdentity_ordinal_nestedInsideIntent :: IsNSPresentationIntent parent => CLong -> CLong -> parent -> IO (Id NSPresentationIntent)
listItemIntentWithIdentity_ordinal_nestedInsideIntent identity ordinal parent =
  do
    cls' <- getRequiredClass "NSPresentationIntent"
    withObjCPtr parent $ \raw_parent ->
      sendClassMsg cls' (mkSelector "listItemIntentWithIdentity:ordinal:nestedInsideIntent:") (retPtr retVoid) [argCLong (fromIntegral identity), argCLong (fromIntegral ordinal), argPtr (castPtr raw_parent :: Ptr ())] >>= retainedObject . castPtr

-- | @+ blockQuoteIntentWithIdentity:nestedInsideIntent:@
blockQuoteIntentWithIdentity_nestedInsideIntent :: IsNSPresentationIntent parent => CLong -> parent -> IO (Id NSPresentationIntent)
blockQuoteIntentWithIdentity_nestedInsideIntent identity parent =
  do
    cls' <- getRequiredClass "NSPresentationIntent"
    withObjCPtr parent $ \raw_parent ->
      sendClassMsg cls' (mkSelector "blockQuoteIntentWithIdentity:nestedInsideIntent:") (retPtr retVoid) [argCLong (fromIntegral identity), argPtr (castPtr raw_parent :: Ptr ())] >>= retainedObject . castPtr

-- | @+ tableIntentWithIdentity:columnCount:alignments:nestedInsideIntent:@
tableIntentWithIdentity_columnCount_alignments_nestedInsideIntent :: (IsNSArray alignments, IsNSPresentationIntent parent) => CLong -> CLong -> alignments -> parent -> IO (Id NSPresentationIntent)
tableIntentWithIdentity_columnCount_alignments_nestedInsideIntent identity columnCount alignments parent =
  do
    cls' <- getRequiredClass "NSPresentationIntent"
    withObjCPtr alignments $ \raw_alignments ->
      withObjCPtr parent $ \raw_parent ->
        sendClassMsg cls' (mkSelector "tableIntentWithIdentity:columnCount:alignments:nestedInsideIntent:") (retPtr retVoid) [argCLong (fromIntegral identity), argCLong (fromIntegral columnCount), argPtr (castPtr raw_alignments :: Ptr ()), argPtr (castPtr raw_parent :: Ptr ())] >>= retainedObject . castPtr

-- | @+ tableHeaderRowIntentWithIdentity:nestedInsideIntent:@
tableHeaderRowIntentWithIdentity_nestedInsideIntent :: IsNSPresentationIntent parent => CLong -> parent -> IO (Id NSPresentationIntent)
tableHeaderRowIntentWithIdentity_nestedInsideIntent identity parent =
  do
    cls' <- getRequiredClass "NSPresentationIntent"
    withObjCPtr parent $ \raw_parent ->
      sendClassMsg cls' (mkSelector "tableHeaderRowIntentWithIdentity:nestedInsideIntent:") (retPtr retVoid) [argCLong (fromIntegral identity), argPtr (castPtr raw_parent :: Ptr ())] >>= retainedObject . castPtr

-- | @+ tableRowIntentWithIdentity:row:nestedInsideIntent:@
tableRowIntentWithIdentity_row_nestedInsideIntent :: IsNSPresentationIntent parent => CLong -> CLong -> parent -> IO (Id NSPresentationIntent)
tableRowIntentWithIdentity_row_nestedInsideIntent identity row parent =
  do
    cls' <- getRequiredClass "NSPresentationIntent"
    withObjCPtr parent $ \raw_parent ->
      sendClassMsg cls' (mkSelector "tableRowIntentWithIdentity:row:nestedInsideIntent:") (retPtr retVoid) [argCLong (fromIntegral identity), argCLong (fromIntegral row), argPtr (castPtr raw_parent :: Ptr ())] >>= retainedObject . castPtr

-- | @+ tableCellIntentWithIdentity:column:nestedInsideIntent:@
tableCellIntentWithIdentity_column_nestedInsideIntent :: IsNSPresentationIntent parent => CLong -> CLong -> parent -> IO (Id NSPresentationIntent)
tableCellIntentWithIdentity_column_nestedInsideIntent identity column parent =
  do
    cls' <- getRequiredClass "NSPresentationIntent"
    withObjCPtr parent $ \raw_parent ->
      sendClassMsg cls' (mkSelector "tableCellIntentWithIdentity:column:nestedInsideIntent:") (retPtr retVoid) [argCLong (fromIntegral identity), argCLong (fromIntegral column), argPtr (castPtr raw_parent :: Ptr ())] >>= retainedObject . castPtr

-- | Returns @YES@ if this intent is equivalent to the other presentation intent. Equivalence is the same as equality except that identity is not taken into account.
--
-- ObjC selector: @- isEquivalentToPresentationIntent:@
isEquivalentToPresentationIntent :: (IsNSPresentationIntent nsPresentationIntent, IsNSPresentationIntent other) => nsPresentationIntent -> other -> IO Bool
isEquivalentToPresentationIntent nsPresentationIntent  other =
withObjCPtr other $ \raw_other ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPresentationIntent (mkSelector "isEquivalentToPresentationIntent:") retCULong [argPtr (castPtr raw_other :: Ptr ())]

-- | @- intentKind@
intentKind :: IsNSPresentationIntent nsPresentationIntent => nsPresentationIntent -> IO NSPresentationIntentKind
intentKind nsPresentationIntent  =
  fmap (coerce :: CLong -> NSPresentationIntentKind) $ sendMsg nsPresentationIntent (mkSelector "intentKind") retCLong []

-- | @- parentIntent@
parentIntent :: IsNSPresentationIntent nsPresentationIntent => nsPresentationIntent -> IO (Id NSPresentationIntent)
parentIntent nsPresentationIntent  =
  sendMsg nsPresentationIntent (mkSelector "parentIntent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An integer value which uniquely identifies this intent in the document. Identity disambiguates attributes which apply to contiguous text -- for example, two headers in a row with the same level. It can also be used to track the location in an attributed string of a particular part of a document, even after mutation.
--
-- ObjC selector: @- identity@
identity :: IsNSPresentationIntent nsPresentationIntent => nsPresentationIntent -> IO CLong
identity nsPresentationIntent  =
  sendMsg nsPresentationIntent (mkSelector "identity") retCLong []

-- | If the intent is not a list, this value is 0.
--
-- ObjC selector: @- ordinal@
ordinal :: IsNSPresentationIntent nsPresentationIntent => nsPresentationIntent -> IO CLong
ordinal nsPresentationIntent  =
  sendMsg nsPresentationIntent (mkSelector "ordinal") retCLong []

-- | If the intent is not a table, this value is @nil@.
--
-- ObjC selector: @- columnAlignments@
columnAlignments :: IsNSPresentationIntent nsPresentationIntent => nsPresentationIntent -> IO (Id NSArray)
columnAlignments nsPresentationIntent  =
  sendMsg nsPresentationIntent (mkSelector "columnAlignments") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | If the intent is not a table, this value is 0.
--
-- ObjC selector: @- columnCount@
columnCount :: IsNSPresentationIntent nsPresentationIntent => nsPresentationIntent -> IO CLong
columnCount nsPresentationIntent  =
  sendMsg nsPresentationIntent (mkSelector "columnCount") retCLong []

-- | If the intent is not a header, this value is 0.
--
-- ObjC selector: @- headerLevel@
headerLevel :: IsNSPresentationIntent nsPresentationIntent => nsPresentationIntent -> IO CLong
headerLevel nsPresentationIntent  =
  sendMsg nsPresentationIntent (mkSelector "headerLevel") retCLong []

-- | If the intent is not a code block, this value is @nil@.
--
-- ObjC selector: @- languageHint@
languageHint :: IsNSPresentationIntent nsPresentationIntent => nsPresentationIntent -> IO (Id NSString)
languageHint nsPresentationIntent  =
  sendMsg nsPresentationIntent (mkSelector "languageHint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The column to which this cell belongs (0-based). If the intent is not a cell, this value is 0.
--
-- ObjC selector: @- column@
column :: IsNSPresentationIntent nsPresentationIntent => nsPresentationIntent -> IO CLong
column nsPresentationIntent  =
  sendMsg nsPresentationIntent (mkSelector "column") retCLong []

-- | The row to which this cell belongs (0-based). If the intent is not a row, this value is 0. Header rows are always row 0. If the table has more rows, those start at row 1.
--
-- ObjC selector: @- row@
row :: IsNSPresentationIntent nsPresentationIntent => nsPresentationIntent -> IO CLong
row nsPresentationIntent  =
  sendMsg nsPresentationIntent (mkSelector "row") retCLong []

-- | The indentation level of this intent. Each nested list increases the indentation level by one; all elements within the same list (and not then nested into a child list intent) have the same indentation level. Text outside list intents has an indentation level of 0.
--
-- ObjC selector: @- indentationLevel@
indentationLevel :: IsNSPresentationIntent nsPresentationIntent => nsPresentationIntent -> IO CLong
indentationLevel nsPresentationIntent  =
  sendMsg nsPresentationIntent (mkSelector "indentationLevel") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @paragraphIntentWithIdentity:nestedInsideIntent:@
paragraphIntentWithIdentity_nestedInsideIntentSelector :: Selector
paragraphIntentWithIdentity_nestedInsideIntentSelector = mkSelector "paragraphIntentWithIdentity:nestedInsideIntent:"

-- | @Selector@ for @headerIntentWithIdentity:level:nestedInsideIntent:@
headerIntentWithIdentity_level_nestedInsideIntentSelector :: Selector
headerIntentWithIdentity_level_nestedInsideIntentSelector = mkSelector "headerIntentWithIdentity:level:nestedInsideIntent:"

-- | @Selector@ for @codeBlockIntentWithIdentity:languageHint:nestedInsideIntent:@
codeBlockIntentWithIdentity_languageHint_nestedInsideIntentSelector :: Selector
codeBlockIntentWithIdentity_languageHint_nestedInsideIntentSelector = mkSelector "codeBlockIntentWithIdentity:languageHint:nestedInsideIntent:"

-- | @Selector@ for @thematicBreakIntentWithIdentity:nestedInsideIntent:@
thematicBreakIntentWithIdentity_nestedInsideIntentSelector :: Selector
thematicBreakIntentWithIdentity_nestedInsideIntentSelector = mkSelector "thematicBreakIntentWithIdentity:nestedInsideIntent:"

-- | @Selector@ for @orderedListIntentWithIdentity:nestedInsideIntent:@
orderedListIntentWithIdentity_nestedInsideIntentSelector :: Selector
orderedListIntentWithIdentity_nestedInsideIntentSelector = mkSelector "orderedListIntentWithIdentity:nestedInsideIntent:"

-- | @Selector@ for @unorderedListIntentWithIdentity:nestedInsideIntent:@
unorderedListIntentWithIdentity_nestedInsideIntentSelector :: Selector
unorderedListIntentWithIdentity_nestedInsideIntentSelector = mkSelector "unorderedListIntentWithIdentity:nestedInsideIntent:"

-- | @Selector@ for @listItemIntentWithIdentity:ordinal:nestedInsideIntent:@
listItemIntentWithIdentity_ordinal_nestedInsideIntentSelector :: Selector
listItemIntentWithIdentity_ordinal_nestedInsideIntentSelector = mkSelector "listItemIntentWithIdentity:ordinal:nestedInsideIntent:"

-- | @Selector@ for @blockQuoteIntentWithIdentity:nestedInsideIntent:@
blockQuoteIntentWithIdentity_nestedInsideIntentSelector :: Selector
blockQuoteIntentWithIdentity_nestedInsideIntentSelector = mkSelector "blockQuoteIntentWithIdentity:nestedInsideIntent:"

-- | @Selector@ for @tableIntentWithIdentity:columnCount:alignments:nestedInsideIntent:@
tableIntentWithIdentity_columnCount_alignments_nestedInsideIntentSelector :: Selector
tableIntentWithIdentity_columnCount_alignments_nestedInsideIntentSelector = mkSelector "tableIntentWithIdentity:columnCount:alignments:nestedInsideIntent:"

-- | @Selector@ for @tableHeaderRowIntentWithIdentity:nestedInsideIntent:@
tableHeaderRowIntentWithIdentity_nestedInsideIntentSelector :: Selector
tableHeaderRowIntentWithIdentity_nestedInsideIntentSelector = mkSelector "tableHeaderRowIntentWithIdentity:nestedInsideIntent:"

-- | @Selector@ for @tableRowIntentWithIdentity:row:nestedInsideIntent:@
tableRowIntentWithIdentity_row_nestedInsideIntentSelector :: Selector
tableRowIntentWithIdentity_row_nestedInsideIntentSelector = mkSelector "tableRowIntentWithIdentity:row:nestedInsideIntent:"

-- | @Selector@ for @tableCellIntentWithIdentity:column:nestedInsideIntent:@
tableCellIntentWithIdentity_column_nestedInsideIntentSelector :: Selector
tableCellIntentWithIdentity_column_nestedInsideIntentSelector = mkSelector "tableCellIntentWithIdentity:column:nestedInsideIntent:"

-- | @Selector@ for @isEquivalentToPresentationIntent:@
isEquivalentToPresentationIntentSelector :: Selector
isEquivalentToPresentationIntentSelector = mkSelector "isEquivalentToPresentationIntent:"

-- | @Selector@ for @intentKind@
intentKindSelector :: Selector
intentKindSelector = mkSelector "intentKind"

-- | @Selector@ for @parentIntent@
parentIntentSelector :: Selector
parentIntentSelector = mkSelector "parentIntent"

-- | @Selector@ for @identity@
identitySelector :: Selector
identitySelector = mkSelector "identity"

-- | @Selector@ for @ordinal@
ordinalSelector :: Selector
ordinalSelector = mkSelector "ordinal"

-- | @Selector@ for @columnAlignments@
columnAlignmentsSelector :: Selector
columnAlignmentsSelector = mkSelector "columnAlignments"

-- | @Selector@ for @columnCount@
columnCountSelector :: Selector
columnCountSelector = mkSelector "columnCount"

-- | @Selector@ for @headerLevel@
headerLevelSelector :: Selector
headerLevelSelector = mkSelector "headerLevel"

-- | @Selector@ for @languageHint@
languageHintSelector :: Selector
languageHintSelector = mkSelector "languageHint"

-- | @Selector@ for @column@
columnSelector :: Selector
columnSelector = mkSelector "column"

-- | @Selector@ for @row@
rowSelector :: Selector
rowSelector = mkSelector "row"

-- | @Selector@ for @indentationLevel@
indentationLevelSelector :: Selector
indentationLevelSelector = mkSelector "indentationLevel"

