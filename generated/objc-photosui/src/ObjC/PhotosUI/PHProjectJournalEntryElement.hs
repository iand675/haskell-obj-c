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
  , dateSelector
  , assetElementSelector
  , textElementSelector


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
import ObjC.Foundation.Internal.Classes

-- | Date to which the provided asset and/or text pertain
--
-- ObjC selector: @- date@
date :: IsPHProjectJournalEntryElement phProjectJournalEntryElement => phProjectJournalEntryElement -> IO (Id NSDate)
date phProjectJournalEntryElement  =
  sendMsg phProjectJournalEntryElement (mkSelector "date") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Representative asset, if any, for that date.
--
-- ObjC selector: @- assetElement@
assetElement :: IsPHProjectJournalEntryElement phProjectJournalEntryElement => phProjectJournalEntryElement -> IO (Id PHProjectAssetElement)
assetElement phProjectJournalEntryElement  =
  sendMsg phProjectJournalEntryElement (mkSelector "assetElement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Descriptive text (e.g., "Mom's Birthday") for that date.
--
-- ObjC selector: @- textElement@
textElement :: IsPHProjectJournalEntryElement phProjectJournalEntryElement => phProjectJournalEntryElement -> IO (Id PHProjectTextElement)
textElement phProjectJournalEntryElement  =
  sendMsg phProjectJournalEntryElement (mkSelector "textElement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @date@
dateSelector :: Selector
dateSelector = mkSelector "date"

-- | @Selector@ for @assetElement@
assetElementSelector :: Selector
assetElementSelector = mkSelector "assetElement"

-- | @Selector@ for @textElement@
textElementSelector :: Selector
textElementSelector = mkSelector "textElement"

