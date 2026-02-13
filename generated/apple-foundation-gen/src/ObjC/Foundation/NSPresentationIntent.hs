{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , blockQuoteIntentWithIdentity_nestedInsideIntentSelector
  , codeBlockIntentWithIdentity_languageHint_nestedInsideIntentSelector
  , columnAlignmentsSelector
  , columnCountSelector
  , columnSelector
  , headerIntentWithIdentity_level_nestedInsideIntentSelector
  , headerLevelSelector
  , identitySelector
  , indentationLevelSelector
  , initSelector
  , intentKindSelector
  , isEquivalentToPresentationIntentSelector
  , languageHintSelector
  , listItemIntentWithIdentity_ordinal_nestedInsideIntentSelector
  , orderedListIntentWithIdentity_nestedInsideIntentSelector
  , ordinalSelector
  , paragraphIntentWithIdentity_nestedInsideIntentSelector
  , parentIntentSelector
  , rowSelector
  , tableCellIntentWithIdentity_column_nestedInsideIntentSelector
  , tableHeaderRowIntentWithIdentity_nestedInsideIntentSelector
  , tableIntentWithIdentity_columnCount_alignments_nestedInsideIntentSelector
  , tableRowIntentWithIdentity_row_nestedInsideIntentSelector
  , thematicBreakIntentWithIdentity_nestedInsideIntentSelector
  , unorderedListIntentWithIdentity_nestedInsideIntentSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- init@
init_ :: IsNSPresentationIntent nsPresentationIntent => nsPresentationIntent -> IO (Id NSPresentationIntent)
init_ nsPresentationIntent =
  sendOwnedMessage nsPresentationIntent initSelector

-- | @+ paragraphIntentWithIdentity:nestedInsideIntent:@
paragraphIntentWithIdentity_nestedInsideIntent :: IsNSPresentationIntent parent => CLong -> parent -> IO (Id NSPresentationIntent)
paragraphIntentWithIdentity_nestedInsideIntent identity parent =
  do
    cls' <- getRequiredClass "NSPresentationIntent"
    sendClassMessage cls' paragraphIntentWithIdentity_nestedInsideIntentSelector identity (toNSPresentationIntent parent)

-- | @+ headerIntentWithIdentity:level:nestedInsideIntent:@
headerIntentWithIdentity_level_nestedInsideIntent :: IsNSPresentationIntent parent => CLong -> CLong -> parent -> IO (Id NSPresentationIntent)
headerIntentWithIdentity_level_nestedInsideIntent identity level parent =
  do
    cls' <- getRequiredClass "NSPresentationIntent"
    sendClassMessage cls' headerIntentWithIdentity_level_nestedInsideIntentSelector identity level (toNSPresentationIntent parent)

-- | @+ codeBlockIntentWithIdentity:languageHint:nestedInsideIntent:@
codeBlockIntentWithIdentity_languageHint_nestedInsideIntent :: (IsNSString languageHint, IsNSPresentationIntent parent) => CLong -> languageHint -> parent -> IO (Id NSPresentationIntent)
codeBlockIntentWithIdentity_languageHint_nestedInsideIntent identity languageHint parent =
  do
    cls' <- getRequiredClass "NSPresentationIntent"
    sendClassMessage cls' codeBlockIntentWithIdentity_languageHint_nestedInsideIntentSelector identity (toNSString languageHint) (toNSPresentationIntent parent)

-- | @+ thematicBreakIntentWithIdentity:nestedInsideIntent:@
thematicBreakIntentWithIdentity_nestedInsideIntent :: IsNSPresentationIntent parent => CLong -> parent -> IO (Id NSPresentationIntent)
thematicBreakIntentWithIdentity_nestedInsideIntent identity parent =
  do
    cls' <- getRequiredClass "NSPresentationIntent"
    sendClassMessage cls' thematicBreakIntentWithIdentity_nestedInsideIntentSelector identity (toNSPresentationIntent parent)

-- | @+ orderedListIntentWithIdentity:nestedInsideIntent:@
orderedListIntentWithIdentity_nestedInsideIntent :: IsNSPresentationIntent parent => CLong -> parent -> IO (Id NSPresentationIntent)
orderedListIntentWithIdentity_nestedInsideIntent identity parent =
  do
    cls' <- getRequiredClass "NSPresentationIntent"
    sendClassMessage cls' orderedListIntentWithIdentity_nestedInsideIntentSelector identity (toNSPresentationIntent parent)

-- | @+ unorderedListIntentWithIdentity:nestedInsideIntent:@
unorderedListIntentWithIdentity_nestedInsideIntent :: IsNSPresentationIntent parent => CLong -> parent -> IO (Id NSPresentationIntent)
unorderedListIntentWithIdentity_nestedInsideIntent identity parent =
  do
    cls' <- getRequiredClass "NSPresentationIntent"
    sendClassMessage cls' unorderedListIntentWithIdentity_nestedInsideIntentSelector identity (toNSPresentationIntent parent)

-- | @+ listItemIntentWithIdentity:ordinal:nestedInsideIntent:@
listItemIntentWithIdentity_ordinal_nestedInsideIntent :: IsNSPresentationIntent parent => CLong -> CLong -> parent -> IO (Id NSPresentationIntent)
listItemIntentWithIdentity_ordinal_nestedInsideIntent identity ordinal parent =
  do
    cls' <- getRequiredClass "NSPresentationIntent"
    sendClassMessage cls' listItemIntentWithIdentity_ordinal_nestedInsideIntentSelector identity ordinal (toNSPresentationIntent parent)

-- | @+ blockQuoteIntentWithIdentity:nestedInsideIntent:@
blockQuoteIntentWithIdentity_nestedInsideIntent :: IsNSPresentationIntent parent => CLong -> parent -> IO (Id NSPresentationIntent)
blockQuoteIntentWithIdentity_nestedInsideIntent identity parent =
  do
    cls' <- getRequiredClass "NSPresentationIntent"
    sendClassMessage cls' blockQuoteIntentWithIdentity_nestedInsideIntentSelector identity (toNSPresentationIntent parent)

-- | @+ tableIntentWithIdentity:columnCount:alignments:nestedInsideIntent:@
tableIntentWithIdentity_columnCount_alignments_nestedInsideIntent :: (IsNSArray alignments, IsNSPresentationIntent parent) => CLong -> CLong -> alignments -> parent -> IO (Id NSPresentationIntent)
tableIntentWithIdentity_columnCount_alignments_nestedInsideIntent identity columnCount alignments parent =
  do
    cls' <- getRequiredClass "NSPresentationIntent"
    sendClassMessage cls' tableIntentWithIdentity_columnCount_alignments_nestedInsideIntentSelector identity columnCount (toNSArray alignments) (toNSPresentationIntent parent)

-- | @+ tableHeaderRowIntentWithIdentity:nestedInsideIntent:@
tableHeaderRowIntentWithIdentity_nestedInsideIntent :: IsNSPresentationIntent parent => CLong -> parent -> IO (Id NSPresentationIntent)
tableHeaderRowIntentWithIdentity_nestedInsideIntent identity parent =
  do
    cls' <- getRequiredClass "NSPresentationIntent"
    sendClassMessage cls' tableHeaderRowIntentWithIdentity_nestedInsideIntentSelector identity (toNSPresentationIntent parent)

-- | @+ tableRowIntentWithIdentity:row:nestedInsideIntent:@
tableRowIntentWithIdentity_row_nestedInsideIntent :: IsNSPresentationIntent parent => CLong -> CLong -> parent -> IO (Id NSPresentationIntent)
tableRowIntentWithIdentity_row_nestedInsideIntent identity row parent =
  do
    cls' <- getRequiredClass "NSPresentationIntent"
    sendClassMessage cls' tableRowIntentWithIdentity_row_nestedInsideIntentSelector identity row (toNSPresentationIntent parent)

-- | @+ tableCellIntentWithIdentity:column:nestedInsideIntent:@
tableCellIntentWithIdentity_column_nestedInsideIntent :: IsNSPresentationIntent parent => CLong -> CLong -> parent -> IO (Id NSPresentationIntent)
tableCellIntentWithIdentity_column_nestedInsideIntent identity column parent =
  do
    cls' <- getRequiredClass "NSPresentationIntent"
    sendClassMessage cls' tableCellIntentWithIdentity_column_nestedInsideIntentSelector identity column (toNSPresentationIntent parent)

-- | Returns @YES@ if this intent is equivalent to the other presentation intent. Equivalence is the same as equality except that identity is not taken into account.
--
-- ObjC selector: @- isEquivalentToPresentationIntent:@
isEquivalentToPresentationIntent :: (IsNSPresentationIntent nsPresentationIntent, IsNSPresentationIntent other) => nsPresentationIntent -> other -> IO Bool
isEquivalentToPresentationIntent nsPresentationIntent other =
  sendMessage nsPresentationIntent isEquivalentToPresentationIntentSelector (toNSPresentationIntent other)

-- | @- intentKind@
intentKind :: IsNSPresentationIntent nsPresentationIntent => nsPresentationIntent -> IO NSPresentationIntentKind
intentKind nsPresentationIntent =
  sendMessage nsPresentationIntent intentKindSelector

-- | @- parentIntent@
parentIntent :: IsNSPresentationIntent nsPresentationIntent => nsPresentationIntent -> IO (Id NSPresentationIntent)
parentIntent nsPresentationIntent =
  sendMessage nsPresentationIntent parentIntentSelector

-- | An integer value which uniquely identifies this intent in the document. Identity disambiguates attributes which apply to contiguous text -- for example, two headers in a row with the same level. It can also be used to track the location in an attributed string of a particular part of a document, even after mutation.
--
-- ObjC selector: @- identity@
identity :: IsNSPresentationIntent nsPresentationIntent => nsPresentationIntent -> IO CLong
identity nsPresentationIntent =
  sendMessage nsPresentationIntent identitySelector

-- | If the intent is not a list, this value is 0.
--
-- ObjC selector: @- ordinal@
ordinal :: IsNSPresentationIntent nsPresentationIntent => nsPresentationIntent -> IO CLong
ordinal nsPresentationIntent =
  sendMessage nsPresentationIntent ordinalSelector

-- | If the intent is not a table, this value is @nil@.
--
-- ObjC selector: @- columnAlignments@
columnAlignments :: IsNSPresentationIntent nsPresentationIntent => nsPresentationIntent -> IO (Id NSArray)
columnAlignments nsPresentationIntent =
  sendMessage nsPresentationIntent columnAlignmentsSelector

-- | If the intent is not a table, this value is 0.
--
-- ObjC selector: @- columnCount@
columnCount :: IsNSPresentationIntent nsPresentationIntent => nsPresentationIntent -> IO CLong
columnCount nsPresentationIntent =
  sendMessage nsPresentationIntent columnCountSelector

-- | If the intent is not a header, this value is 0.
--
-- ObjC selector: @- headerLevel@
headerLevel :: IsNSPresentationIntent nsPresentationIntent => nsPresentationIntent -> IO CLong
headerLevel nsPresentationIntent =
  sendMessage nsPresentationIntent headerLevelSelector

-- | If the intent is not a code block, this value is @nil@.
--
-- ObjC selector: @- languageHint@
languageHint :: IsNSPresentationIntent nsPresentationIntent => nsPresentationIntent -> IO (Id NSString)
languageHint nsPresentationIntent =
  sendMessage nsPresentationIntent languageHintSelector

-- | The column to which this cell belongs (0-based). If the intent is not a cell, this value is 0.
--
-- ObjC selector: @- column@
column :: IsNSPresentationIntent nsPresentationIntent => nsPresentationIntent -> IO CLong
column nsPresentationIntent =
  sendMessage nsPresentationIntent columnSelector

-- | The row to which this cell belongs (0-based). If the intent is not a row, this value is 0. Header rows are always row 0. If the table has more rows, those start at row 1.
--
-- ObjC selector: @- row@
row :: IsNSPresentationIntent nsPresentationIntent => nsPresentationIntent -> IO CLong
row nsPresentationIntent =
  sendMessage nsPresentationIntent rowSelector

-- | The indentation level of this intent. Each nested list increases the indentation level by one; all elements within the same list (and not then nested into a child list intent) have the same indentation level. Text outside list intents has an indentation level of 0.
--
-- ObjC selector: @- indentationLevel@
indentationLevel :: IsNSPresentationIntent nsPresentationIntent => nsPresentationIntent -> IO CLong
indentationLevel nsPresentationIntent =
  sendMessage nsPresentationIntent indentationLevelSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSPresentationIntent)
initSelector = mkSelector "init"

-- | @Selector@ for @paragraphIntentWithIdentity:nestedInsideIntent:@
paragraphIntentWithIdentity_nestedInsideIntentSelector :: Selector '[CLong, Id NSPresentationIntent] (Id NSPresentationIntent)
paragraphIntentWithIdentity_nestedInsideIntentSelector = mkSelector "paragraphIntentWithIdentity:nestedInsideIntent:"

-- | @Selector@ for @headerIntentWithIdentity:level:nestedInsideIntent:@
headerIntentWithIdentity_level_nestedInsideIntentSelector :: Selector '[CLong, CLong, Id NSPresentationIntent] (Id NSPresentationIntent)
headerIntentWithIdentity_level_nestedInsideIntentSelector = mkSelector "headerIntentWithIdentity:level:nestedInsideIntent:"

-- | @Selector@ for @codeBlockIntentWithIdentity:languageHint:nestedInsideIntent:@
codeBlockIntentWithIdentity_languageHint_nestedInsideIntentSelector :: Selector '[CLong, Id NSString, Id NSPresentationIntent] (Id NSPresentationIntent)
codeBlockIntentWithIdentity_languageHint_nestedInsideIntentSelector = mkSelector "codeBlockIntentWithIdentity:languageHint:nestedInsideIntent:"

-- | @Selector@ for @thematicBreakIntentWithIdentity:nestedInsideIntent:@
thematicBreakIntentWithIdentity_nestedInsideIntentSelector :: Selector '[CLong, Id NSPresentationIntent] (Id NSPresentationIntent)
thematicBreakIntentWithIdentity_nestedInsideIntentSelector = mkSelector "thematicBreakIntentWithIdentity:nestedInsideIntent:"

-- | @Selector@ for @orderedListIntentWithIdentity:nestedInsideIntent:@
orderedListIntentWithIdentity_nestedInsideIntentSelector :: Selector '[CLong, Id NSPresentationIntent] (Id NSPresentationIntent)
orderedListIntentWithIdentity_nestedInsideIntentSelector = mkSelector "orderedListIntentWithIdentity:nestedInsideIntent:"

-- | @Selector@ for @unorderedListIntentWithIdentity:nestedInsideIntent:@
unorderedListIntentWithIdentity_nestedInsideIntentSelector :: Selector '[CLong, Id NSPresentationIntent] (Id NSPresentationIntent)
unorderedListIntentWithIdentity_nestedInsideIntentSelector = mkSelector "unorderedListIntentWithIdentity:nestedInsideIntent:"

-- | @Selector@ for @listItemIntentWithIdentity:ordinal:nestedInsideIntent:@
listItemIntentWithIdentity_ordinal_nestedInsideIntentSelector :: Selector '[CLong, CLong, Id NSPresentationIntent] (Id NSPresentationIntent)
listItemIntentWithIdentity_ordinal_nestedInsideIntentSelector = mkSelector "listItemIntentWithIdentity:ordinal:nestedInsideIntent:"

-- | @Selector@ for @blockQuoteIntentWithIdentity:nestedInsideIntent:@
blockQuoteIntentWithIdentity_nestedInsideIntentSelector :: Selector '[CLong, Id NSPresentationIntent] (Id NSPresentationIntent)
blockQuoteIntentWithIdentity_nestedInsideIntentSelector = mkSelector "blockQuoteIntentWithIdentity:nestedInsideIntent:"

-- | @Selector@ for @tableIntentWithIdentity:columnCount:alignments:nestedInsideIntent:@
tableIntentWithIdentity_columnCount_alignments_nestedInsideIntentSelector :: Selector '[CLong, CLong, Id NSArray, Id NSPresentationIntent] (Id NSPresentationIntent)
tableIntentWithIdentity_columnCount_alignments_nestedInsideIntentSelector = mkSelector "tableIntentWithIdentity:columnCount:alignments:nestedInsideIntent:"

-- | @Selector@ for @tableHeaderRowIntentWithIdentity:nestedInsideIntent:@
tableHeaderRowIntentWithIdentity_nestedInsideIntentSelector :: Selector '[CLong, Id NSPresentationIntent] (Id NSPresentationIntent)
tableHeaderRowIntentWithIdentity_nestedInsideIntentSelector = mkSelector "tableHeaderRowIntentWithIdentity:nestedInsideIntent:"

-- | @Selector@ for @tableRowIntentWithIdentity:row:nestedInsideIntent:@
tableRowIntentWithIdentity_row_nestedInsideIntentSelector :: Selector '[CLong, CLong, Id NSPresentationIntent] (Id NSPresentationIntent)
tableRowIntentWithIdentity_row_nestedInsideIntentSelector = mkSelector "tableRowIntentWithIdentity:row:nestedInsideIntent:"

-- | @Selector@ for @tableCellIntentWithIdentity:column:nestedInsideIntent:@
tableCellIntentWithIdentity_column_nestedInsideIntentSelector :: Selector '[CLong, CLong, Id NSPresentationIntent] (Id NSPresentationIntent)
tableCellIntentWithIdentity_column_nestedInsideIntentSelector = mkSelector "tableCellIntentWithIdentity:column:nestedInsideIntent:"

-- | @Selector@ for @isEquivalentToPresentationIntent:@
isEquivalentToPresentationIntentSelector :: Selector '[Id NSPresentationIntent] Bool
isEquivalentToPresentationIntentSelector = mkSelector "isEquivalentToPresentationIntent:"

-- | @Selector@ for @intentKind@
intentKindSelector :: Selector '[] NSPresentationIntentKind
intentKindSelector = mkSelector "intentKind"

-- | @Selector@ for @parentIntent@
parentIntentSelector :: Selector '[] (Id NSPresentationIntent)
parentIntentSelector = mkSelector "parentIntent"

-- | @Selector@ for @identity@
identitySelector :: Selector '[] CLong
identitySelector = mkSelector "identity"

-- | @Selector@ for @ordinal@
ordinalSelector :: Selector '[] CLong
ordinalSelector = mkSelector "ordinal"

-- | @Selector@ for @columnAlignments@
columnAlignmentsSelector :: Selector '[] (Id NSArray)
columnAlignmentsSelector = mkSelector "columnAlignments"

-- | @Selector@ for @columnCount@
columnCountSelector :: Selector '[] CLong
columnCountSelector = mkSelector "columnCount"

-- | @Selector@ for @headerLevel@
headerLevelSelector :: Selector '[] CLong
headerLevelSelector = mkSelector "headerLevel"

-- | @Selector@ for @languageHint@
languageHintSelector :: Selector '[] (Id NSString)
languageHintSelector = mkSelector "languageHint"

-- | @Selector@ for @column@
columnSelector :: Selector '[] CLong
columnSelector = mkSelector "column"

-- | @Selector@ for @row@
rowSelector :: Selector '[] CLong
rowSelector = mkSelector "row"

-- | @Selector@ for @indentationLevel@
indentationLevelSelector :: Selector '[] CLong
indentationLevelSelector = mkSelector "indentationLevel"

