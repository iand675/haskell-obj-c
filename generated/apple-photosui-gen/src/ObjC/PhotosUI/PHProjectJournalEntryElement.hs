{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A PHProjectJournalEntryElement object represents auxilary, date specific information that may be interesting to include in a project. For example, callouts for specific birthdays or holidays. In general, these will only be included for projects created from existing Apple Calendar projects.
--
-- Generated bindings for @PHProjectJournalEntryElement@.
module ObjC.PhotosUI.PHProjectJournalEntryElement
  ( PHProjectJournalEntryElement
  , IsPHProjectJournalEntryElement(..)
  , date
  , assetElement
  , textElement
  , assetElementSelector
  , dateSelector
  , textElementSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PhotosUI.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Date to which the provided asset and/or text pertain
--
-- ObjC selector: @- date@
date :: IsPHProjectJournalEntryElement phProjectJournalEntryElement => phProjectJournalEntryElement -> IO (Id NSDate)
date phProjectJournalEntryElement =
  sendMessage phProjectJournalEntryElement dateSelector

-- | Representative asset, if any, for that date.
--
-- ObjC selector: @- assetElement@
assetElement :: IsPHProjectJournalEntryElement phProjectJournalEntryElement => phProjectJournalEntryElement -> IO (Id PHProjectAssetElement)
assetElement phProjectJournalEntryElement =
  sendMessage phProjectJournalEntryElement assetElementSelector

-- | Descriptive text (e.g., "Mom's Birthday") for that date.
--
-- ObjC selector: @- textElement@
textElement :: IsPHProjectJournalEntryElement phProjectJournalEntryElement => phProjectJournalEntryElement -> IO (Id PHProjectTextElement)
textElement phProjectJournalEntryElement =
  sendMessage phProjectJournalEntryElement textElementSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @date@
dateSelector :: Selector '[] (Id NSDate)
dateSelector = mkSelector "date"

-- | @Selector@ for @assetElement@
assetElementSelector :: Selector '[] (Id PHProjectAssetElement)
assetElementSelector = mkSelector "assetElement"

-- | @Selector@ for @textElement@
textElementSelector :: Selector '[] (Id PHProjectTextElement)
textElementSelector = mkSelector "textElement"

